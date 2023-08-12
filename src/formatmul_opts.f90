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
      integer                  :: m,n,k

      select case (option)
       case ('m1')
         C = matmul(A, B)
       case ('m2')
         C = matmul_blas(A, B)
       case('m3')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_mnp(m, k, n, a, b, c)
       case('m4')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_mpn(m, k, n, a, b, c)
       case('m5')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_nmp(m, k, n, a, b, c)
       case('m6')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_npm(m, k, n, a, b, c)
       case('m7')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_pmn(m, k, n, a, b, c)
       case('m8')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_pnm(m, k, n, a, b, c)
       case('m9')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_9(m, k, n, a, b, c)
       case('m10')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_10(m, k, n, a, b, c)
       case('m11')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_11(m, k, n, a, b, c)
       case('m12')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_12(m, k, n, a, b, c)
       case('m13')
         m = size(A,1)
         n = size(B,2)
         k = size(B,1)
         call mm_13(m, k, n, a, b, c)
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
      C = 0.0_rk
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
      y = 0.0_rk
      ! Call BLAS dgemv subroutine for matrix-vector multiplication.
      call dgemv('N', m, n, 1.0_rk, A, m, x, 1, 0.0_rk, y , 1)
   end function gemv_mat_vec_rel

   !> author: @tyrandis
   pure subroutine mm_mnp(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, j, k
      c = 0.0_rk
      do i=1,m
         do j=1,n
            do k=1,p
               c(i,k) = c(i,k) + a(i,j)*b(j,k)
            end do
         end do
      end do
   end subroutine mm_mnp

   !> author: @tyrandis
   pure subroutine mm_mpn(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, j, k
      c = 0.0_rk
      do i=1,m
         do j=1,p
            do k=1,n
               c(i,j) = c(i,j) + a(i,k)*b(k,j)
            end do
         end do
      end do
   end subroutine mm_mpn

   !> author: @tyrandis
   pure subroutine mm_nmp(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, j, k
      c = 0.0_rk
      do i=1,n
         do j=1,m
            do k=1,p
               c(j,k) = c(j,k) + a(j,i)*b(i,k)
            end do
         end do
      end do
   end subroutine mm_nmp

   !> author: @tyrandis
   pure subroutine mm_npm(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, j, k
      c = 0.0_rk
      do i=1,n
         do j=1,p
            do k=1,m
               c(k,j) = c(k,j) + a(k,i)*b(i,j)
            end do
         end do
      end do
   end subroutine mm_npm

   !> author: @tyrandis
   pure subroutine mm_pmn(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, j, k
      c = 0.0_rk
      do i=1,p
         do j=1,m
            do k=1,n
               c(j,i) = c(j,i) + a(j,k)*b(k,i)
            end do
         end do
      end do
   end subroutine mm_pmn

   !> author: @tyrandis
   pure subroutine mm_pnm(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, j, k
      c = 0.0_rk
      do i=1,p
         do j=1,n
            do k=1,m
               c(k,i) = c(k,i) + a(k,j)*b(j,i)
            end do
         end do
      end do
   end subroutine mm_pnm

   !> author: Seyed Ali Ghasemi
   pure subroutine mm_9(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, k
      c = 0.0_rk
      do i = 1, p
         do k = 1, m
            c(k,i) = dot_product(a(k,:), b(:,i))
         end do
      end do
   end subroutine mm_9

   !> author: Seyed Ali Ghasemi
   pure subroutine mm_10(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, k
      c = 0.0_rk
      do concurrent (i = 1: p)
         do k = 1, m
            c(k,i) = dot_product(a(k,:), b(:,i))
         end do
      end do
   end subroutine mm_10

   !> author: Seyed Ali Ghasemi
   pure subroutine mm_11(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, j, k
      c = 0.0_rk
      do concurrent (i = 1: p)
         do j=1,n
            do k=1,m
               c(k,i) = c(k,i) + a(k,j)*b(j,i)
            end do
         end do
      end do
   end subroutine mm_11

   !> author: Seyed Ali Ghasemi
   pure subroutine mm_12(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, k
      c = 0.0_rk
      block
         !$OMP PARALLEL DO PRIVATE(i, k)
         do i = 1, p
            do k = 1, m
               c(k,i) = dot_product(a(k,:), b(:,i))
            end do
         end do
         !$OMP END PARALLEL DO
      end block
   end subroutine mm_12

   !> author: Seyed Ali Ghasemi
   pure subroutine mm_13(m, n, p, a, b, c)
      integer, intent(in) :: m, n, p
      real(rk), intent(in) :: a(m,n), b(n,p)
      real(rk), intent(out) :: c(m,p)
      integer :: i, k
      c = 0.0_rk
      block
         !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(i, k) SHARED(m, p, a, b, c)
         do i = 1, p
            do k = 1, m
               c(k,i) = dot_product(a(k,:), b(:,i))
            end do
         end do
         !$OMP END PARALLEL DO
      end block
   end subroutine mm_13

end module formatmul_opts
