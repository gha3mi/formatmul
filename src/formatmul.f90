module formatmul

   use kinds

   implicit none

   private
   public matmul

   !===============================================================================
   interface matmul
      procedure :: mat_mat
      procedure :: mat_vec
   end interface
   !===============================================================================

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure function mat_mat(A, B, method) result(C)
      real(rk),     intent(in), contiguous :: A(:,:), B(:,:)
      character(*), intent(in)             :: method
      real(rk), allocatable                :: C(:,:)
      integer                              :: m, o, remainder_m

      m = size(A,1)
      o = size(B,2)

      if (method == 'coarray') then

         block

            integer               :: i, block_size, n, im, nimg
            real(rk), allocatable :: C_block(:,:)[:]

            im          = this_image()
            nimg        = num_images()
            n           = size(A,2)
            block_size  = m/nimg
            remainder_m = m - block_size * (nimg - 1)

            if ( mod(m, nimg) == 0 ) then
               if (.not. allocated(C_block)) allocate(C_block(block_size, o)[*])
               C_block(1:block_size, 1:o)[im] = matmul(A((im-1)*block_size + 1 : im*block_size, 1:n), B(1:n, 1:o))
            else
               if (.not. allocated(C_block)) allocate(C_block(remainder_m, o)[*])
               C_block(1:remainder_m, 1:o)[im] = matmul(A((im-1)*block_size + 1 : im*block_size + remainder_m, 1:n), B(1:n, 1:o))
            end if

            sync all

            ! critical
            if (im == 1) then
               if (.not. allocated(C)) allocate(C(m, o))
               do i = 1, nimg - 1
                  C((i-1)*block_size + 1 : i*block_size, :) = C_block(1:block_size, 1:o)[i]
               end do
               C((nimg-1)*block_size + 1 : m, :) = C_block(1:remainder_m, 1:o)[nimg]
            end if
            ! end critical

         end block

      else

         error stop 'Error: The specified method is not available for matrix-matrix multiplication!'
         ! if (this_image() == 1) then
         !    ! if (.not. allocated(C)) allocate(C(m, o))
         !    C = matmul(A, B)
         ! end if

      end if
   end function mat_mat
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure function mat_vec(A, v, method) result(w)
      real(rk),     intent(in), contiguous :: A(:,:), v(:)
      character(*), intent(in)             :: method
      real(rk), allocatable                :: w(:)
      integer                              :: m, remainder_m

      m = size(A, 1)

      if (method == 'coarray') then
         block
            integer               :: i, block_size, n, im, nimg
            real(rk), allocatable :: w_block(:)[:]

            im          = this_image()
            nimg        = num_images()
            m           = size(A, 1)
            n           = size(A, 2)
            block_size  = m/nimg
            remainder_m = m - block_size * (nimg - 1)

            if (mod(m, nimg) == 0) then
               if (.not. allocated(w_block)) allocate(w_block(block_size)[*])
               w_block(:) = matmul(A((im-1)*block_size + 1 : im*block_size, 1:n), v)
            else
               if (.not. allocated(w_block)) allocate(w_block(remainder_m)[*])
               w_block(:) = matmul(A((im-1)*block_size + 1 : im*block_size + remainder_m, 1:n), v)
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

         error stop 'Error: The specified method is not available for matrix-vector multiplication!'
         ! if (this_image() == 1) then
         !    ! if (.not. allocated(w)) allocate(w(m))
         !    w = matmul(A, v)
         ! end if

      end if
   end function mat_vec
   !===============================================================================

end module formatmul
