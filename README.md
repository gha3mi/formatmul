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

c = matmul(a,b,method='coarray',option='m1')
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

**Run examples:**

To set the stack size to unlimited, use the following command: `ulimit -s unlimited`.

You have the option to modify the number of images in the `fpm.rsp` file by using the flag `-coarray-num-images=N`.

```shell
fpm @ifort-example
```

```shell
fpm @ifort-example-coarray
```

```shell
fpm @ifx-example
```

```shell
fpm @ifx-example-coarray
```


```shell
fpm @nvfortran-example
```

Results with `-coarray-num-images=4`:

```shell
Elapsed time (example1: mat_mat):  1.175 [s]
Elapsed time (example2: mat_mat, coarray):  0.269 [s]
Elapsed time (example2: mat_mat, coarray):  0.272 [s]
Elapsed time (example2: mat_mat, coarray):  0.274 [s]
Elapsed time (example2: mat_mat, coarray):  0.275 [s]

Elapsed time (example3: mat_vec):  0.047 [s]
Elapsed time (example4: mat_vec, coarray):  0.012 [s]
Elapsed time (example4: mat_vec, coarray):  0.012 [s]
Elapsed time (example4: mat_vec, coarray):  0.012 [s]
Elapsed time (example4: mat_vec, coarray):  0.012 [s]
```

**Benchmark:**

To set the stack size to unlimited, use the following command: `ulimit -s unlimited`.

**Intel Fortran Compiler (ifort)**

```shell
fpm run --example benchmark3 --compiler ifort --flag "-Ofast -mtune=native -xHost -qmkl -qopenmp -ipo -coarray -coarray-num-images=4 -DUSE_COARRAY"
```

**Intel Fortran Compiler (ifx)**

```shell
fpm run --example --all --compiler ifx --flag "-Ofast -mtune=native -xHost -qmkl -qopenmp -coarray -coarray-num-images=4 -DUSE_COARRAY"
```

You can then use the provided Python script to generate visual plots for the benchmark3 data:

```shell
python benchmark/benchmark3_co.py
```

Results obtained on an `Intel(R) Core(TM) i9-9980HK CPU @ 2.40GHz` using `ifort (IFORT) 2021.11.0 20231010` are as follows:

- with `-coarray-num-images=4`, `MKL_NUM_THREADS=1` and `OMP_NUM_THREADS=1`:

<img alt="ForMatmul" src="https://github.com/gha3mi/formatmul/raw/main/benchmark/singlethread/benchmark3t_nim4.png" width="350"><img alt="ForMatmul" src="https://github.com/gha3mi/formatmul/raw/main/benchmark/singlethread/benchmark3p_nim4.png" width="350">

- with `-coarray-num-images=4` and Multithread:

<img alt="ForMatmul" src="https://github.com/gha3mi/formatmul/raw/main/benchmark/multithread/benchmark3t_nim4.png" width="350"><img alt="ForMatmul" src="https://github.com/gha3mi/formatmul/raw/main/benchmark/multithread/benchmark3p_nim4.png" width="350">

- with `-coarray-num-images=5`, `MKL_NUM_THREADS=1` and `OMP_NUM_THREADS=1`:

<img alt="ForMatmul" src="https://github.com/gha3mi/formatmul/raw/main/benchmark/singlethread/benchmark3t_nim5.png" width="350"><img alt="ForMatmul" src="https://github.com/gha3mi/formatmul/raw/main/benchmark/singlethread/benchmark3p_nim5.png" width="350">

- with `-coarray-num-images=5` and Multithread:

<img alt="ForMatmul" src="https://github.com/gha3mi/formatmul/raw/main/benchmark/multithread/benchmark3t_nim5.png" width="350"><img alt="ForMatmul" src="https://github.com/gha3mi/formatmul/raw/main/benchmark/multithread/benchmark3p_nim5.png" width="350">

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
