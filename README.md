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
parsiuk-exe <prl file> <prefix>
```

Example:
```sh
parsiuk$ parsiuk-exe implemented.prl my
Files implemented.h and implemented.c are successfully created!
```

`implemented.prl`:
```c++
struct main_struct {
}
```

`implemented.h`:
```c++
typedef struct my_main_struct {
} my_main_struct_t;
```

`implemented.c`:
```c++
prs_result_t my_main_struct_parse(uint8_t const *data, size_t size, my_main_struct_t **out) {
}


void my_main_struct_free(my_main_struct_t *p) {
}
```
