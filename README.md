# git-coprs

Checkout a Github PR with fuzzy selection
Requires the following binaries:
  - https://github.com/cli/cli
  - https://github.com/junegunn/fzf

# Install

```
make install
```

On macOS, you might need to install a new make, e.g. via homebrew:

```
gmake install
```


Alternatively, install via cargo

```
cargo install --locked --git https://github.com/knutwalker/git-coprs
```

# Usage

Basic usage:

```
git coprs
```

Can optionally provide a query that will be filtering the result

```
git coprs 1337
```

The order depends on your account, which can be provided.

```
git coprs --me=superuser
```

The default can be baked into the binary when building

```
GH_USER=superuser make install
```

Otherwise, the gh api is queried for the user that is authenticated.

