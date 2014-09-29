This is a thin wrapper for testing SCORM 1.2 and 2004 package with a sandboxed Javascript API.

## Installation

```
cabal install cabalg
cabalg https://github.com/wereHamster/scorm-sandbox
```

This uses [cabalg](https://hackage.haskell.org/package/cabalg).

## Usage

Assuming the installation succeeded, to use the package:

```
scorm-sandbox <SCORM-archive.zip>
```

Then leave the terminal, open a browser and go to `http://localhost:8000`.

This should load the page you had selected in the options list.

**Note:** If there is only one accessible resource, it will be automatically chosen.
