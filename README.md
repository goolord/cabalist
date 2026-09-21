# cabalist

A GUI for making Hackage releases of Haskell packages, in the careful,
stepped style of [hkgr](https://github.com/juhp/hkgr), built on
[nano-ui](https://github.com/goolord/nano-ui). It works with a repository
holding one package at its root or many in subdirectories of a monorepo.

![cabalist with a two-package monorepo, one package's candidate up on Hackage](docs/screenshot.png)

Version, tag, upload candidates, upload documentation, and publish. Supports monorepos and cabal subliraries.

## Logging in to Hackage

cabal uploads with whatever login its config file holds (a token, a password
command, or a username and password). Without one, set a username and
password or an API token with *Log in to Hackage*. *Remember it* saves the
login in the operating system's keyring (Windows Credential Manager, the
macOS keychain, or the Secret Service through `secret-tool` on Linux) for the
next time cabalist starts; otherwise it stays in memory until cabalist
closes. Neither kind reaches cabal on its command line, where other programs
could read it: a password goes on cabal's standard input, and a token in a
temporary copy of cabal's config file, deleted after the upload.

## Trying it safely

*Dry run*, in *Settings* (or `cabalist --dry-run`), runs every step but logs
the uploads and pushes instead of doing them.

## Running

```sh
cabal run cabalist -- [--dry-run] [DIRECTORY]
```

cabalist opens the repository containing `DIRECTORY`, or the current
directory's repository, or the one opened last.

It needs `git`, `cabal`, `tar` and `curl` on the `PATH`, and uses `hlint` if
it is there. When several cabals are on the `PATH`, cabalist uses the newest.

## Building

You need GHC 9.14, and SDL3 and SDL3_ttf for nano-ui's SDL backend.
`cabal.project` pins nano-ui to a commit on GitHub.

```sh
cabal build
cabal test                                # the release steps, on a throwaway monorepo
cabal run cabalist -- --selftest shots    # the window, driven headlessly; screenshots in shots/
cabal run cabalist -- --screenshot out.bmp DIRECTORY
```

The self-test builds a two-package monorepo in the temporary directory, then
tags and builds one package through the window, opens each dialog, and
uploads candidates of both in dependency order, all in dry-run mode.

### Release builds

```sh
runghc --ghc-arg=-package-env=- tools/Release.hs            # dist/cabalist-VERSION-OS-ARCH/
runghc --ghc-arg=-package-env=- tools/Release.hs --archive  # the same, as a .zip (Windows) or .tar.gz
```
