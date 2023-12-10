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
      real(rk)                             :: C(size(A,1),size(B,2))


      select case (method)
#if defined(COARRAY)
      case ('coarray')
         ! Coarray-based parallel multiplication.

         if (size(A,1) >= size(B,2)) then
            ! Handle A's columns > B's rows.

            block
               integer               :: i, im, nimg, n, o
               integer               :: block_size(num_images()), start_elem(num_images()), end_elem(num_images())
               real(rk), allocatable :: C_block(:,:)[:], A_block(:,:)[:]
               im   = this_image()
               nimg = num_images()
               n    = size(A,2)
               o    = size(B,2)
               call compute_block_ranges(size(A,1), nimg, block_size, start_elem, end_elem)
               allocate(A_block(block_size(im), n)[*], C_block(block_size(im), o)[*])
               A_block(:,:)[im] = A(start_elem(im):end_elem(im), :)
               C_block(:,:)[im] = matmul_opts(A_block(:,:)[im], B, option)
               sync all
               if (im == 1) then
                  do i = 1, nimg
                     C(start_elem(i):end_elem(i), :) = C_block(:,:)[i]
                  end do
               end if
            end block

         else
            ! Handle B's columns > A's rows.

            block
               integer               :: i, im, nimg, n, m
               integer               :: block_size(num_images()), start_elem(num_images()), end_elem(num_images())
               real(rk), allocatable :: C_block(:,:)[:], B_block(:,:)[:]
               im   = this_image()
               nimg = num_images()
               n    = size(A,2)
               m    = size(A,1)
               call compute_block_ranges(size(B,2), nimg, block_size, start_elem, end_elem)
               allocate(B_block(n, block_size(im))[*], C_block(m, block_size(im))[*])
               B_block(:,:)[im] = B(:, start_elem(im):end_elem(im))
               C_block(:,:)[im] = matmul_opts(A, B_block(:,:)[im], option)
               sync all
               if (im == 1) then
                  do i = 1, nimg
                     C(:,start_elem(i):end_elem(i)) = C_block(:,:)[i]
                  end do
               end if
            end block

         end if
#endif
      case default
         C = matmul_opts(A, B, option)
      end select

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
      real(rk)                             :: w(size(A,1))


      select case (method)
#if defined(COARRAY)
      case ('coarray')
         ! Coarray-based parallel multiplication.

         block
            integer               :: i, im, nimg
            integer               :: block_size(num_images()), start_elem(num_images()), end_elem(num_images())
            real(rk), allocatable :: w_block(:)[:]
            im   = this_image()
            nimg = num_images()
            call compute_block_ranges(size(A,1), nimg, block_size, start_elem, end_elem)
            allocate(w_block(block_size(im))[*])
            w_block(:)[im] = matmul_opts(A(start_elem(im):end_elem(im), :), v, option)
            sync all
            if (im == 1) then
               do i = 1, nimg
                  w(start_elem(i):end_elem(i)) = w_block(:)[i]
               end do
            end if
         end block
#endif
      case default
         w = matmul_opts(A, v, option)
      end select

   end function mat_vec

   !> Calculate block sizes and ranges.
   !> author: Seyed Ali Ghasemi
   pure subroutine compute_block_ranges(d, nimg, block_size, start_elem, end_elem)
      integer, intent(in)  :: d, nimg
      integer, intent(out) :: block_size(nimg), start_elem(nimg), end_elem(nimg)
      integer              :: i, remainder
      block_size = d / nimg
      remainder = mod(d, nimg)
      block_size(1:remainder) = block_size(1:remainder) + 1
      start_elem(1) = 1
      end_elem(1) = block_size(1)
      do i = 2, nimg
         start_elem(i) = start_elem(i - 1) + block_size(i - 1)
         end_elem(i) = start_elem(i) + block_size(i) - 1
      end do
   end subroutine compute_block_ranges

end module formatmul
