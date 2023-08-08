!> Module: formatmul
!! This module provides matrix and vector multiplication functions using different methods.

module formatmul

   !> Use the kinds module for real(kind) type.
   use kinds

   implicit none

   private
   public matmul, matmul_blas

   !> Interface for matrix multiplication functions.
   interface matmul
      procedure :: mat_mat
      procedure :: mat_vec
   end interface

   !> Interface for BLAS-based matrix multiplication functions.
   interface matmul_blas
      procedure :: gemm_mat_mat_rel
      procedure :: gemv_mat_vec_rel
   end interface


contains

   !> Matrix-matrix multiplication using coarray parallelism.
   !> author: Seyed Ali Ghasemi
   pure function mat_mat(A, B, method, option) result(C)
      !> Input matrices A and B.
      real(rk),     intent(in), contiguous :: A(:,:), B(:,:)
      !> Multiplication method ('coarray').
      character(*), intent(in)             :: method
      !> Optional method-specific option.
      character(*), intent(in), optional   :: option
      !> Result matrix C.
      ! real(rk), allocatable                :: C(:,:)
      real(rk)                             :: C(size(A,1),size(B,2))

      if (method == 'coarray') then
         ! Coarray-based parallel multiplication.

         if (size(A,1) >= size(B,2)) then
            ! Handle A's columns > B's rows.

            block
               integer               :: i, block_size, n, im, nimg
               real(rk), allocatable :: C_block(:,:)[:]
               integer               :: m, o, remainder_m
               real(rk), allocatable :: A_block(:,:)

               im          = this_image()
               nimg        = num_images()
               m           = size(A,1)
               o           = size(B,2)
               n           = size(A,2)
               block_size  = m/nimg

               if ( mod(m, nimg) == 0 ) then
                  if (.not. allocated(C_block)) allocate(C_block(block_size, o)[*])
                  A_block = A((im-1)*block_size + 1 : im*block_size, :)
               else
                  remainder_m = m - block_size * (nimg - 1)
                  if (.not. allocated(C_block)) allocate(C_block(remainder_m, m)[*])
                  A_block = A((im-1)*block_size + 1 : im*block_size + remainder_m, :)
               end if

               select case (option)
                  case ('matmul')
                  C_block(:, :)[im] = matmul(A_block, B)
                  case ('blas')
                  C_block(:, :)[im] = matmul_blas(A_block, B)
                  case default
                  C_block(:, :)[im] = matmul(A_block, B)
               end select

               sync all

               ! critical
               if (im == 1) then
                  ! if (.not. allocated(C)) allocate(C(m, o))
                  do i = 1, nimg - 1
                     C((i-1)*block_size + 1 : i*block_size, :) = C_block(:,:)[i]
                  end do
                  C((nimg-1)*block_size + 1 : m, :) = C_block(:,:)[nimg]
               end if
               ! end critical

            end block

         else
            ! Handle B's columns > A's rows.

            block
               integer               :: i, block_size, n, im, nimg
               real(rk), allocatable :: C_block(:,:)[:]
               integer               :: m, o, remainder_m
               real(rk), allocatable :: B_block(:,:)

               im          = this_image()
               nimg        = num_images()
               m           = size(A,1)
               o           = size(B,2)
               n           = size(A,2)
               block_size  = o/nimg

               if ( mod(o, nimg) == 0 ) then
                  if (.not. allocated(C_block)) allocate(C_block(m, block_size)[*])
                  B_block = B(:, (im-1)*block_size + 1 : im*block_size)
               else
                  remainder_m = o - block_size * (nimg - 1)
                  if (.not. allocated(C_block)) allocate(C_block(m, remainder_m)[*])
                  B_block = B(:, (im-1)*block_size + 1 : im*block_size + remainder_m)
               end if

               select case (option)
                  case ('matmul')
                  C_block(:, :)[im] = matmul(A, B_block)
                  case ('blas')
                  C_block(:, :)[im] = matmul_blas(A, B_block)
                  case default
                  C_block(:, :)[im] = matmul(A, B_block)
               end select

               sync all

               ! critical
               if (im == 1) then
                  ! if (.not. allocated(C)) allocate(C(m, o))
                  do i = 1, nimg - 1
                     C(:, (i-1)*block_size + 1 : i*block_size) = C_block(:,:)[i]
                  end do
                  C(:, (nimg-1)*block_size + 1 : o) = C_block(:,:)[nimg]
               end if
               ! end critical

            end block

         end if

      else
         ! Unsupported multiplication method.

         error stop 'Error: The specified method is not available for matrix-matrix multiplication!'
         ! if (this_image() == 1) then
         !    ! if (.not. allocated(C)) allocate(C(m, o))
         !    C = matmul(A, B)
         ! end if

      end if
   end function mat_mat



   !> Matrix-vector multiplication using coarray parallelism.
   !> author: Seyed Ali Ghasemi
   pure function mat_vec(A, v, method, option) result(w)
      !> Input matrix A and vector v.
      real(rk),     intent(in), contiguous :: A(:,:), v(:)
      !> Multiplication method ('coarray').
      character(*), intent(in)             :: method
      !> Optional method-specific option.
      character(*), intent(in), optional   :: option
      !> Result vector w.
      real(rk), allocatable                :: w(:)

      if (method == 'coarray') then
         ! Coarray-based parallel multiplication.
         block
            integer               :: i, block_size, n, im, nimg
            real(rk), allocatable :: w_block(:)[:]
            integer               :: m, remainder_m

            im          = this_image()
            nimg        = num_images()
            m           = size(A, 1)
            n           = size(A, 2)
            block_size  = m/nimg
            remainder_m = m - block_size * (nimg - 1)

            if (mod(m, nimg) == 0) then
               if (.not. allocated(w_block)) allocate(w_block(block_size)[*])

               select case (option)
                  case ('matmul')
                  w_block(:) = matmul(A((im-1)*block_size + 1 : im*block_size, 1:n), v)
                  case ('blas')
                  w_block(:) = matmul_blas(A((im-1)*block_size + 1 : im*block_size, 1:n), v)
                  case default
                  w_block(:) = matmul(A((im-1)*block_size + 1 : im*block_size, 1:n), v)
               end select

            else
               if (.not. allocated(w_block)) allocate(w_block(remainder_m)[*])

               select case (option)
                  case ('matmul')
                  w_block(:) = matmul(A((im-1)*block_size + 1 : im*block_size + remainder_m, 1:n), v)
                  case ('blas')
                  w_block(:) = matmul_blas(A((im-1)*block_size + 1 : im*block_size + remainder_m, 1:n), v)
                  case default
                  w_block(:) = matmul(A((im-1)*block_size + 1 : im*block_size + remainder_m, 1:n), v)
               end select

            end if

            sync all

            ! critical
            if (im == 1) then
               if (.not. allocated(w)) allocate(w(m))
               do i = 1, nimg - 1
                  w((i-1)*block_size + 1 : i*block_size) = w_block(:)[i]
               end do
               w((nimg-1)*block_size + 1 : m) = w_block(:)[nimg]
            end if
            ! end critical
         end block

      else
         ! Unsupported multiplication method.

         error stop 'Error: The specified method is not available for matrix-vector multiplication!'
         ! if (this_image() == 1) then
         !    ! if (.not. allocated(w)) allocate(w(m))
         !    w = matmul(A, v)
         ! end if

      end if
   end function mat_vec



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

end module formatmul
