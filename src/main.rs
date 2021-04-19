//! # git-coprs
//!
//! Checkout a Github PR with fuzzy selection
//! Requires the following binaries:
//!   - https://github.com/cli/cli
//!   - https://github.com/junegunn/fzf
//!
//! # Install
//!
//! ```
//! make install
//! ```
//!
//! On macOS, you might need to install a new make, e.g. via homebrew:
//!
//! ```
//! gmake install
//! ```
//!
//! # Usage
//!
//! Basic usage:
//!
//! ```
//! git coprs
//! ```
//!
//! Can optionally provide a query that will be filtering the result
//!
//! ```
//! git coprs 1337
//! ```
//!
//! The order depends on your account, which can be provided.
//!
//! ```
//! git coprs --me=superuser
//! ```
//!
//! The default can be baked into the binary when building
//!
//! ```
//! GH_USER=superuser make install
//! ```
//!
//! Otherwise, the gh api is queried for the user that is authenticated.

use humantime;
use serde_json::Value;
use std::{
    array::IntoIter,
    cmp::Ordering,
    ffi::OsStr,
    fmt::Display,
    io::Write,
    process::{Command, Stdio},
    time::SystemTime,
};

fn num(pr: &Value) -> u64 {
    pr.as_u64().expect("number")
}

fn string<'a>(pr: &'a Value) -> &'a str {
    pr.as_str().expect("string")
}

fn time(pr: &Value) -> SystemTime {
    let time = pr.as_str().expect("string");
    humantime::parse_rfc3339(time).expect("RFC3339 Timestamp")
}

fn string_opt<'a>(pr: &'a Value, path: &str) -> Option<&'a str> {
    pr.pointer(path).and_then(|v| v.as_str())
}

struct Selection<'a>(&'a Value);
struct DraftSelection<'a>(&'a Value);
struct Preview<'a>(&'a Value);
struct Label<'a>(&'a Value);
struct FullLine(Value);
struct PreviewBold<D>(D);

impl Display for Selection<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "#{number}: {title} (\x1B[1m{head}\x1B[0m)",
            number = num(&self.0["number"]),
            title = string(&self.0["title"]),
            head = string(&self.0["head"]["label"]),
        ))
    }
}

impl Display for DraftSelection<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "\x1B[2m#{number}: {title} ({head})\x1B[0m",
            number = num(&self.0["number"]),
            title = string(&self.0["title"]),
            head = string(&self.0["head"]["label"]),
        ))
    }
}

impl Display for Label<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let color = string(&self.0["color"]);
        let r = u8::from_str_radix(&color[0..2], 16).expect("non hex color");
        let g = u8::from_str_radix(&color[2..4], 16).expect("non hex color");
        let b = u8::from_str_radix(&color[4..6], 16).expect("non hex color");
        let name = string(&self.0["name"]);
        f.write_fmt(format_args!("\\e[38;2;{};{};{}m{}\\e[0m", r, g, b, name))
    }
}

impl<D> Display for PreviewBold<D>
where
    D: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("\\e[1m{}\\e[0m", self.0))
    }
}

impl Display for Preview<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::time::Duration;

        const PREVIEW_NEWLINE: &str = "\\n";

        f.write_fmt(format_args!(
            "{author} wants to merge {head} into {base}{nl}",
            author = PreviewBold(string(&self.0["user"]["login"])),
            head = PreviewBold(string(&self.0["head"]["label"])),
            base = PreviewBold(string(&self.0["base"]["label"])),
            nl = PREVIEW_NEWLINE
        ))?;

        match self.0["labels"].as_array() {
            Some(labels) => match labels.split_first() {
                None => {}
                Some((label, &[])) => {
                    write!(f, "Label: {}", Label(label))?;
                }
                Some((first, rest)) => {
                    write!(f, "Labels: {}", Label(first))?;
                    for label in rest {
                        write!(f, ", {}", Label(label))?;
                    }
                }
            },
            None => {}
        };

        if matches!(self.0["draft"].as_bool(), Some(true)) {
            f.write_str(" 📜 Draft 📜")?;
        }

        if !self.0["auto_merge"].is_null() {
            f.write_str(" 🚗 Auto Merge Enabled 🚗")?;
        }

        if string(&self.0["state"]) == "closed" {
            f.write_str(" 🔒 Closed 🔒")?;
        }

        f.write_str(PREVIEW_NEWLINE)?;

        let now = SystemTime::now();
        let created_at = time(&self.0["created_at"]);
        let updated_at = time(&self.0["updated_at"]);
        let created = now
            .duration_since(created_at)
            .map(|d| Duration::from_secs(d.as_secs()))
            .unwrap_or_else(|_| Default::default());
        let updated = now
            .duration_since(updated_at)
            .map(|d| Duration::from_secs(d.as_secs()))
            .unwrap_or_else(|_| Default::default());

        f.write_fmt(format_args!(
            "Created: {relative} ago ({absolute}){nl}",
            relative = PreviewBold(humantime::format_duration(created)),
            absolute = PreviewBold(humantime::format_rfc3339(created_at)),
            nl = PREVIEW_NEWLINE,
        ))?;

        f.write_fmt(format_args!(
            "Updated: {relative} ago ({absolute}){nl}",
            relative = PreviewBold(humantime::format_duration(updated)),
            absolute = PreviewBold(humantime::format_rfc3339(updated_at)),
            nl = PREVIEW_NEWLINE,
        ))?;

        if let Some(reviewer) = string_opt(&self.0, "/assignee/login") {
            f.write_fmt(format_args!(
                "Reviewer: {reviewer}{nl}",
                reviewer = PreviewBold(reviewer),
                nl = PREVIEW_NEWLINE
            ))?;
        }

        if let Some(body) = string_opt(self.0, "/body") {
            for body_line in body.lines() {
                f.write_fmt(format_args!(
                    "{line}{nl}",
                    line = body_line,
                    nl = PREVIEW_NEWLINE
                ))?;
            }
        }

        Ok(())
    }
}

impl Display for FullLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if matches!(self.0["draft"].as_bool(), Some(true)) {
            DraftSelection(&self.0).fmt(f)?;
        } else {
            Selection(&self.0).fmt(f)?;
        };
        f.write_str("\t")?;
        Preview(&self.0).fmt(f)
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
struct SortKey {
    is_assigned_to_me: bool,
    is_opened_by_me: bool,
    is_draft: bool,
    updated_at: u64,
    pr_number: u64,
}

impl SortKey {
    fn new(pr: &Value, me: Option<&str>) -> Self {
        let assignee = string_opt(pr, "/assignee/login");
        let is_assigned_to_me = assignee.is_some() && assignee == me;
        let is_opened_by_me = pr["user"]["login"].as_str() == me;
        let is_draft = pr["draft"].as_bool().expect(".draft := bool");
        let updated_at = pr.get("updated_at").map_or(0, |time| {
            self::time(time)
                .duration_since(SystemTime::UNIX_EPOCH)
                .map_or(0, |v| v.as_secs())
        });
        let pr_number = pr["number"].as_u64().expect(".number := 'number'");

        Self {
            is_assigned_to_me,
            is_opened_by_me,
            is_draft,
            updated_at,
            pr_number,
        }
    }
}

impl PartialOrd for SortKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SortKey {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.is_draft, other.is_draft) {
            (true, false) => Ordering::Less,
            (false, true) => Ordering::Greater,
            _ => match (self.is_assigned_to_me, other.is_assigned_to_me) {
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less,
                _ => match (self.is_opened_by_me, other.is_opened_by_me) {
                    (true, false) => Ordering::Greater,
                    (false, true) => Ordering::Less,
                    _ => match self.updated_at.cmp(&other.updated_at) {
                        Ordering::Equal => self.pr_number.cmp(&other.pr_number),
                        cmp => cmp,
                    },
                },
            },
        }
    }
}

fn get_me() -> Option<String> {
    let user = crate::gh_api("user").ok()?;
    let mut user = serde_json::from_slice::<Value>(&user).ok()?;
    let user = user.get_mut("login")?.take();
    match user {
        Value::String(user) => Some(user),
        _ => None,
    }
}

fn exec<I, S>(args: I) -> std::io::Result<Vec<u8>>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut args = args.into_iter();
    let mut command = Command::new(args.next().expect("missing command"));

    let output = command.args(args).output()?;

    if !output.status.success() {
        let exit_code = output.status.code().unwrap_or(1);
        let err = String::from_utf8_lossy(&output.stderr);
        eprintln!("{}", err);
        std::process::exit(exit_code)
    }

    Ok(output.stdout)
}

fn gh_api(api: &str) -> std::io::Result<Vec<u8>> {
    exec(IntoIter::new(["gh", "api"]).chain(std::iter::once(api)))
}

fn fzf_select<II>(items: II, query: Option<&str>) -> Option<u64>
where
    II: IntoIterator,
    <II as IntoIterator>::Item: Display,
{
    let mut fzf_process = Command::new("fzf")
        .arg("--exact")
        .args(&["--with-nth", "1"])
        .args(&["--delimiter", "\t"])
        .arg("--no-sort")
        .arg("--tac")
        .args(&["--bind", "?:toggle-preview"])
        .args(&["--bind", "esc:cancel"])
        .args(&["--layout", "reverse"])
        .args(&["--info", "inline"])
        .args(&["--prompt", " "])
        .arg("--ansi")
        .args(&["--preview", "echo -e {2}"])
        .args(&["--preview-window", "up"])
        .args(
            query
                .into_iter()
                .flat_map(|q| IntoIter::new(["--query", q])),
        )
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .expect("Could not invoke `fzf`, is it installed?");

    let stdin = fzf_process.stdin.as_mut().expect("Failed to open stdin");
    for item in items {
        stdin
            .write_fmt(format_args!("{}\n", item))
            .expect("Failed to write to stdin");
    }

    let fzf_result = fzf_process.wait_with_output().expect("Calling fzf failed");
    if fzf_result.status.success() {
        parse_number_from_fzf(fzf_result.stdout)
    } else {
        match fzf_result.status.code() {
            None | Some(1) | Some(130) => None,
            Some(code) => {
                let err = String::from_utf8_lossy(&fzf_result.stderr);
                eprintln!("{}", err);
                std::process::exit(code)
            }
        }
    }
}

fn parse_number_from_fzf(output: Vec<u8>) -> Option<u64> {
    let start = output.iter().position(u8::is_ascii_digit)?;
    atoi::atoi(&output[start..])
}

fn run() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let mut args = pico_args::Arguments::from_env();

    let me = args
        .opt_value_from_str("--me")?
        .or_else(|| option_env!("GH_USER").map(String::from))
        .or_else(|| get_me());
    let me = me.as_deref();

    let query: Option<String> = args.opt_free_from_str()?;
    let query = query.as_deref();

    let pulls = crate::gh_api("repos/:owner/:repo/pulls")?;
    let mut pulls = serde_json::from_slice::<Vec<Value>>(&pulls)?;
    pulls.sort_by_cached_key(|pr| SortKey::new(pr, me));

    if let Some(number) = fzf_select(pulls.into_iter().map(|p| FullLine(p)), query) {
        let cmd = format!("gh pr checkout {}", number);
        let _ = exec(cmd.split_ascii_whitespace())?;
    }

    Ok(())
}

pub fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
        std::process::exit(1)
    }
}
