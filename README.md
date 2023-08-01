[![GitHub](https://img.shields.io/badge/GitHub-ForMatmul-blue.svg?style=social&logo=github)](https://github.com/gha3mi/formatmul)
[![Version](https://img.shields.io/github/v/tag/gha3mi/formatmul?color=blue&logo=github&style=flat)](https://github.com/gha3mi/formatmul/releases)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/formatmul/)
[![License](https://img.shields.io/github/license/gha3mi/formatmul?color=green)](https://github.com/gha3mi/formatmul/blob/main/LICENSE)
[![Build](https://github.com/gha3mi/formatmul/actions/workflows/ci.yml/badge.svg)](https://github.com/gha3mi/formatmul/actions/workflows/ci.yml)

<img alt="ForMatmul" src="https://github.com/gha3mi/formatmul/raw/main/media/logo.png" width="750">

**ForMatmul**: A Fortran library that overloads the `matmul` function to enable efficient matrix multiplication with coarray.

## Usage
```Fortran
use formatmul

c = matmul(a,b,method='coarray')
```

## fpm dependency

If you want to use `ForMatmul` as a dependency in your own fpm project,
you can easily include it by adding the following line to your `fpm.toml` file:

```toml
[dependencies]
formatmul = {git="https://github.com/gha3mi/formatmul.git"}
```

## How to run tests and examples

**Clone the repository:**

You can clone the `ForMatmul` repository from GitHub using the following command:

```shell
git clone https://github.com/gha3mi/formatmul.git
```

```shell
cd formatmul
```

**Run tests:**

Tested with Intel compiler: ifort (IFORT) 2021.10.0 20230609

```shell
fpm @ifort-test
```

Tested with Intel compiler: ifx (IFX) 2023.2.0 20230622

```shell
fpm @ifx-test
```

**Run examples:**

```shell
fpm @ifort-example
```
```shell
fpm @ifx-example
```
Results:

```shell
Elapsed time (example1: mat_mat):  1.616 [s]
Elapsed time (example2: mat_mat, coarray):  0.444 [s]
Elapsed time (example2: mat_mat, coarray):  0.444 [s]
Elapsed time (example2: mat_mat, coarray):  0.444 [s]
Elapsed time (example2: mat_mat, coarray):  0.467 [s]
Elapsed time (example3: mat_vec):  0.047 [s]
Elapsed time (example4: mat_vec, coarray):  0.012 [s]
Elapsed time (example4: mat_vec, coarray):  0.012 [s]
Elapsed time (example4: mat_vec, coarray):  0.012 [s]
Elapsed time (example4: mat_vec, coarray):  0.012 [s]
```

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
Contributions to `ForMatmul` are welcome! If you find any issues or would like to suggest improvements, please open an issue.
