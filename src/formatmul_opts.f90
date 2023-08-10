module formatmul_opts

   use kinds, only: rk

   implicit none

   private
   public :: matmul_opts, matmul_blas

   interface matmul_opts
      procedure :: matmul_mat_mat_rel_opts
      procedure :: matmul_mat_vec_rel_opts
   end interface

   !> Interface for BLAS-based matrix multiplication functions.
   interface matmul_blas
      procedure :: gemm_mat_mat_rel
      procedure :: gemv_mat_vec_rel
   end interface

contains

   pure function matmul_mat_mat_rel_opts(A, B, option) result(C)
      real(rk),     intent(in), contiguous :: A(:,:), B(:,:)
      character(*), intent(in) :: option
      real(rk)                 :: C(size(A,1),size(B,2))

      select case (option)
       case ('m1')
         C = matmul(A, B)
       case ('m2')
         C = matmul_blas(A, B)
       case default
         C = matmul(A, B)
      end select
   end function matmul_mat_mat_rel_opts

   pure function matmul_mat_vec_rel_opts(A, v, option) result(w)
      real(rk),     intent(in), contiguous :: A(:,:), v(:)
      character(*), intent(in) :: option
      real(rk)                 :: w(size(A,1))

      select case (option)
       case ('m1')
         w = matmul(A, v)
       case ('m2')
         w = matmul_blas(A, v)
       case default
         w = matmul(A, v)
      end select
   end function matmul_mat_vec_rel_opts

   !> Matrix-matrix multiplication using BLAS.
   !> author: Seyed Ali Ghasemi
   pure function gemm_mat_mat_rel(A, B) result(C)
      !> Input matrices A and B.
      real(rk), dimension(:, :), contiguous, intent(in) :: A
      real(rk), dimension(:, :), contiguous, intent(in) :: B
      !> Result matrix C.
      real(rk), dimension(size(A,1), size(B,2))         :: C
      ! real(rk), dimension(:,:), allocatable             :: C
      integer                                           :: m, n, k

      interface
         !> BLAS subroutine for matrix-matrix multiplication.
         pure subroutine dgemm(f_transa, f_transb, f_m, f_n, f_k, f_alpha, f_a, f_lda, f_b, f_ldb, f_beta, f_c, f_ldc)
            import rk
            integer,   intent(in)    :: f_ldc
            integer,   intent(in)    :: f_ldb
            integer,   intent(in)    :: f_lda
            character, intent(in)    :: f_transa
            character, intent(in)    :: f_transb
            integer,   intent(in)    :: f_m
            integer,   intent(in)    :: f_n
            integer,   intent(in)    :: f_k
            real(rk),  intent(in)    :: f_alpha
            real(rk),  intent(in)    :: f_a(f_lda, *)
            real(rk),  intent(in)    :: f_b(f_ldb, *)
            real(rk),  intent(in)    :: f_beta
            real(rk),  intent(inout) :: f_c(f_ldc, *)
         end subroutine dgemm
      end interface

      m = size(A, 1)
      n = size(B, 2)
      k = size(A, 2)

      ! Call BLAS dgemm subroutine for matrix-matrix multiplication.
      call dgemm('N', 'N', m, n, k, 1.0_rk, A, m, B, k, 0.0_rk, C, m)

   end function gemm_mat_mat_rel

   !> Matrix-vector multiplication using BLAS.
   !> author: Seyed Ali Ghasemi
   pure function gemv_mat_vec_rel(A, x) result(y)
      !> Input matrix A and vector x.
      real(rk), dimension(:, :), contiguous, intent(in) :: A
      real(rk), dimension(:), contiguous, intent(in)    :: x
      !> Result vector y.
      real(rk), dimension(size(A,1))                    :: y
      ! real(rk), dimension(:), allocatable               :: y
      integer                                           :: m, n

      interface
         !> BLAS subroutine for matrix-vector multiplication.
         pure subroutine dgemv(f_trans, f_m, f_n, f_alpha, f_a, f_lda, f_x, f_incx, f_beta, f_y, f_incy)
            import rk
            integer,   intent(in)    :: f_m
            integer,   intent(in)    :: f_n
            integer,   intent(in)    :: f_lda
            character, intent(in)    :: f_trans
            real(rk),  intent(in)    :: f_alpha
            real(rk),  intent(in)    :: f_a(f_lda, *)
            real(rk),  intent(in)    :: f_x(*)
            integer,   intent(in)    :: f_incx
            real(rk),  intent(in)    :: f_beta
            real(rk),  intent(inout) :: f_y(*)
            integer,   intent(in)    :: f_incy
         end subroutine dgemv
      end interface

      m = size(A, 1)
      n = size(A, 2)

      ! Call BLAS dgemv subroutine for matrix-vector multiplication.
      call dgemv('N', m, n, 1.0_rk, A, m, x, 1, 0.0_rk, y , 1)

   end function gemv_mat_vec_rel

end module formatmul_opts

