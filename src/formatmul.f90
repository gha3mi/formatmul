!> Module: formatmul
!! This module provides matrix and vector multiplication functions using different methods.

module formatmul

   !> Use the kinds module for real(kind) type.
   use kinds
   use formatmul_opts

   implicit none

   private
   public matmul

   !> Interface for matrix multiplication functions.
   interface matmul
      procedure :: mat_mat
      procedure :: mat_vec
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
                  if (.not. allocated(C_block)) allocate(C_block(remainder_m, o)[*])
                  A_block = A((im-1)*block_size + 1 : im*block_size + remainder_m, :)
               end if

               C_block(:, :)[im] = matmul_opts(A_block, B, option)

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

               C_block(:, :)[im] = matmul_opts(A, B_block, option)

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
               w_block(:) = matmul_opts(A((im-1)*block_size + 1 : im*block_size, 1:n), v, option)
            else
               if (.not. allocated(w_block)) allocate(w_block(remainder_m)[*])
               w_block(:) = matmul_opts(A((im-1)*block_size + 1 : im*block_size + remainder_m, 1:n), v, option)
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

end module formatmul
