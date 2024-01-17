[![GitHub](https://img.shields.io/badge/GitHub-ForMatmul-blue.svg?style=social&logo=github)](https://github.com/gha3mi/formatmul)
[![Version](https://img.shields.io/github/release/gha3mi/formatmul.svg)](https://github.com/gha3mi/formatmul/releases/latest)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/formatmul/)
[![License](https://img.shields.io/github/license/gha3mi/formatmul?color=green)](https://github.com/gha3mi/formatmul/blob/main/LICENSE)
[![Build](https://github.com/gha3mi/formatmul/actions/workflows/CI_test.yml/badge.svg)](https://github.com/gha3mi/formatmul/actions/workflows/CI_test.yml)

<img alt="ForMatmul" src="https://github.com/gha3mi/formatmul/raw/main/media/logo.png" width="750">

**ForMatmul**: A Fortran library that overloads the `matmul` function to enable efficient matrix multiplication with/without coarray.

## Usage

```fortran
use formatmul

c = matmul(a,b, option='m1', transA=.true., transB=.true., coarray=.true.)
```

## fpm dependency

If you want to use `ForMatmul` as a dependency in your own fpm project,
you can easily include it by adding the following line to your `fpm.toml` file:

```toml
[dependencies]
formatmul = {git="https://github.com/gha3mi/formatmul.git"}
```

## How to run tests

**Clone the repository:**

You can clone the `ForMatmul` repository from GitHub using the following command:

```shell
git clone https://github.com/gha3mi/formatmul.git
```

```shell
cd formatmul
```

Tested with Intel compiler: ifort (IFORT) 2021.10.0 20230609

```shell
fpm @ifort-test
```

```shell
fpm @ifort-test-coarray
```

Tested with Intel compiler: ifx (IFX) 2023.2.0 20230622

```shell
fpm @ifx-test
```

```shell
fpm @ifx-test-coarray
```

Tested with NVIDIA compiler: nvfortran 23.11-0 64-bit target on x86-64 Linux

```shell
fpm @nvfortran-test
```

## Benchmarks
You can find benchmark results on [ForBenchmark](https://github.com/gha3mi/forbenchmark).

## API documentation

The most up-to-date API documentation for the master branch is available
[here](https://gha3mi.github.io/formatmul/).
To generate the API documentation for `ForMatmul` using
[ford](https://github.com/Fortran-FOSS-Programmers/ford) run the following
command:

```shell
ford ford.yml
```

## Contributing

Contributions to `ForMatmul` are welcome!
If you find any issues or would like to suggest improvements, please open an issue.
