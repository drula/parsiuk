# Parsiuk

**The project has not been finished yet.**

## Installation prerequisites

1. Install Stack: [see instructions.](https://docs.haskellstack.org/en/stable/README/)
2. Install `alex` and `happy`:
```sh
parsiuk$ stack install alex
parsiuk$ stack install happy
```
3. Set `$HOME/.local/bin` directory to `PATH` environment variable, if it has not been set yet:
```sh
export PATH=$PATH:$HOME/.local/bin
```

## Build
```sh
parsiuk$ stack build
```

## Run unit tests
```sh
parsiuk$ stack test
```

## Installation
```sh
parsiuk$ stack install
```

## Usage (after intallation)
```sh
parsiuk-exe <prl file>
```

Example:
```sh
parsiuk$ parsiuk-exe implemented.prl
Files implemented.h and implemented.c are successfully created!
```