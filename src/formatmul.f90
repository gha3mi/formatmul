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
               integer               :: i, m, n, o, im, nimg, base_value, remainder
               integer               :: block_size(num_images())
               integer               :: offsets(num_images())
               real(rk), allocatable :: C_block(:,:)[:], A_block(:,:)[:]

               im   = this_image()
               nimg = num_images()
               m    = size(A,1)
               o    = size(B,2)
               n    = size(A,2)

               call calc_block_offsets(m, nimg, block_size, offsets)

               allocate(A_block(block_size(im), n)[*])
               allocate(C_block(block_size(im), o)[*])

               A_block(:,:)[im] = A(offsets(im) : offsets(im) + block_size(im) - 1, :)
               C_block(:,:)[im] = matmul_opts(A_block(:,:)[im], B, option)

               sync all
               if (im == 1) then
                  do i = 1, nimg
                     C(offsets(i) : offsets(i) + block_size(i) - 1, :) = C_block(:,:)[i]
                  end do
               end if

            end block

         else
            ! Handle B's columns > A's rows.

            block
               integer               :: i, m, n, o, im, nimg, base_value, remainder
               integer               :: block_size(num_images())
               integer               :: offsets(num_images())
               real(rk), allocatable :: C_block(:,:)[:], B_block(:,:)[:]

               im   = this_image()
               nimg = num_images()
               m    = size(A,1)
               o    = size(B,2)
               n    = size(A,2)

               call calc_block_offsets(o, nimg, block_size, offsets)

               allocate(B_block(n, block_size(im))[*])
               allocate(C_block(m, block_size(im))[*])

               B_block(:,:)[im] = B(:, offsets(im) : offsets(im) + block_size(im) - 1)
               C_block(:,:)[im] = matmul_opts(A, B_block(:,:)[im], option)

               sync all
               if (im == 1) then
                  do i = 1, nimg
                     C(:,offsets(i) : offsets(i) + block_size(i) - 1) = C_block(:,:)[i]
                  end do
               end if

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

   !> Calculate block sizes and offsets.
   !> author: Seyed Ali Ghasemi
   pure subroutine calc_block_offsets(d,nimg, block_size, offsets)
      integer, intent(in)  :: d, nimg
      integer, intent(out) :: block_size(nimg), offsets(nimg)
      integer              :: i, base_value, remainder
      base_value = d / nimg
      remainder = d - base_value * nimg
      block_size(1:nimg) = base_value
      if (remainder > 0) block_size(1:remainder) = base_value + 1
      offsets = [(sum(block_size(1:i-1)) + 1, i = 1, nimg)]
   end subroutine calc_block_offsets

end module formatmul
