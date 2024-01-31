module formatmul

   use kinds
   use formatmul_opts

   implicit none

   private

   public matmul

   interface matmul
      procedure :: mat_mat_rel
      procedure :: mat_mat_block_rel
      procedure :: mat_mat_coarray_rel

      procedure :: mat_vec_rel
      procedure :: mat_vec_block_rel
      procedure :: mat_vec_coarray_rel

      !   procedure :: vec_mat_rel ! Ambiguous interface
   end interface matmul

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   impure function mat_mat_coarray_rel(a, b, transA, transB, option, coarray) result(c)
      real(rk), intent(in), contiguous :: a(:,:), b(:,:)
      character(*), intent(in), optional :: option
      logical, intent(in), optional :: transA, transB
      real(rk), allocatable :: c(:,:)
      logical, intent(in)   :: coarray
#if defined (USE_COARRAY)
      integer               :: i, im, nimg, n, m
      integer               :: block_size(num_images()), start_elem(num_images()), end_elem(num_images())
      real(rk), allocatable :: C_block(:,:)[:], B_block(:,:)[:], A_block(:,:)[:]

      if (present(transA) .and. present(transB)) then
         if (.not.transA .and. .not.transB) then
            ! AB
            allocate(C(size(A,1), size(B,2)), source=0.0_rk)
            im   = this_image()
            nimg = num_images()
            m    = size(A,1)
            n    = size(A,2)
            call compute_block_ranges(size(B,2), nimg, block_size, start_elem, end_elem)
            allocate(B_block(n, block_size(im))[*], C_block(m, block_size(im))[*])
            B_block(:,:)[im] = B(:, start_elem(im):end_elem(im))
            C_block(:,:)[im] = matmul(A, B_block(:,:)[im], transA=.false., transB=.false., option=option)
            sync all
            if (im == 1) then
               do i = 1, nimg
                  C(:,start_elem(i):end_elem(i)) = C_block(:,:)[i]
               end do
            end if
         else if (transA .and. transB) then
            ! ATBT
            allocate(C(size(A,2), size(B,1)), source=0.0_rk)
            im   = this_image()
            nimg = num_images()
            m    = size(A,1)
            n    = size(A,2)
            call compute_block_ranges(size(A,2), nimg, block_size, start_elem, end_elem)
            allocate(A_block(m, block_size(im))[*], C_block(block_size(im), size(B,1))[*])
            A_block(:,:)[im] = A(:, start_elem(im):end_elem(im))
            C_block(:,:)[im] = matmul(A_block(:,:)[im], B, transA=.true., transB=.true., option=option)
            sync all
            if (im == 1) then
               do i = 1, nimg
                  C(start_elem(i):end_elem(i), :) = C_block(:,:)[i]
               end do
            end if
         else if (transA .and. .not.transB) then
            ! ATB
            allocate(C(size(A,2), size(B,2)), source=0.0_rk)
            im   = this_image()
            nimg = num_images()
            m    = size(A,1)
            n    = size(A,2)
            call compute_block_ranges(size(A,2), nimg, block_size, start_elem, end_elem)
            allocate(A_block(m, block_size(im))[*], C_block(block_size(im), size(B,2))[*])
            A_block(:,:)[im] = A(:, start_elem(im):end_elem(im))
            C_block(:,:)[im] = matmul(A_block(:, :)[im], B, transA=.true., transB=.false., option=option)
            sync all
            if (im == 1) then
               do i = 1, nimg
                  C(start_elem(i):end_elem(i), :) = C_block(:,:)[i]
               end do
            end if
         else if (.not.transA .and. transB) then
            ! ABT
            allocate(C(size(A,1), size(B,1)), source=0.0_rk)
            im   = this_image()
            nimg = num_images()
            m    = size(A,1)
            n    = size(A,2)
            call compute_block_ranges(size(A,2), nimg, block_size, start_elem, end_elem)
            allocate(A_block(m, block_size(im))[*], B_block(size(B,1), block_size(im))[*])
            allocate(C_block(m, size(B,1))[*])
            A_block(:,:)[im] = A(:, start_elem(im):end_elem(im))
            B_block(:,:)[im] = B(:, start_elem(im):end_elem(im))
            C_block(:,:)[im] = matmul(A_block(:,:)[im], B_block(:,:)[im], transA=.false., transB=.true., option=option)
            sync all
            if (im == 1) then
               do i = 1, nimg
                  C(:, :) = C(:, :) + C_block(:,:)[i]
               end do
            end if
         end if
      else if (present(transA) .or. present(transB)) then
         if (present(transA)) then
            if (transA) then
               ! ATB
               allocate(C(size(A,2), size(B,2)), source=0.0_rk)
               im   = this_image()
               nimg = num_images()
               m    = size(A,1)
               n    = size(A,2)
               call compute_block_ranges(size(A,2), nimg, block_size, start_elem, end_elem)
               allocate(A_block(m, block_size(im))[*], C_block(block_size(im), size(B,2))[*])
               A_block(:,:)[im] = A(:, start_elem(im):end_elem(im))
               C_block(:,:)[im] = matmul(A_block(:, :)[im], B, transA=.true., transB=.false., option=option)
               sync all
               if (im == 1) then
                  do i = 1, nimg
                     C(start_elem(i):end_elem(i), :) = C_block(:,:)[i]
                  end do
               end if
            else if (.not.transA) then
               ! ABT
               allocate(C(size(A,1), size(B,1)), source=0.0_rk)
               im   = this_image()
               nimg = num_images()
               m    = size(A,1)
               n    = size(A,2)
               call compute_block_ranges(size(A,2), nimg, block_size, start_elem, end_elem)
               allocate(A_block(m, block_size(im))[*], B_block(size(B,1), block_size(im))[*])
               allocate(C_block(m, size(B,1))[*])
               A_block(:,:)[im] = A(:, start_elem(im):end_elem(im))
               B_block(:,:)[im] = B(:, start_elem(im):end_elem(im))
               C_block(:,:)[im] = matmul(A_block(:,:)[im], B_block(:,:)[im], transA=.false., transB=.true., option=option)
               sync all
               if (im == 1) then
                  do i = 1, nimg
                     C(:, :) = C(:, :) + C_block(:,:)[i]
                  end do
               end if
            end if
         else if (present(transB)) then
            if (transB) then
               ! ABT
               allocate(C(size(A,1), size(B,1)), source=0.0_rk)
               im   = this_image()
               nimg = num_images()
               m    = size(A,1)
               n    = size(A,2)
               call compute_block_ranges(size(A,2), nimg, block_size, start_elem, end_elem)
               allocate(A_block(m, block_size(im))[*], B_block(size(B,1), block_size(im))[*])
               allocate(C_block(m, size(B,1))[*])
               A_block(:,:)[im] = A(:, start_elem(im):end_elem(im))
               B_block(:,:)[im] = B(:, start_elem(im):end_elem(im))
               C_block(:,:)[im] = matmul(A_block(:,:)[im], B_block(:,:)[im], transA=.false., transB=.true., option=option)
               sync all
               if (im == 1) then
                  do i = 1, nimg
                     C(:, :) = C(:, :) + C_block(:,:)[i]
                  end do
               end if
            else if (.not.transB) then
               ! ATB
               allocate(C(size(A,2), size(B,2)), source=0.0_rk)
               im   = this_image()
               nimg = num_images()
               m    = size(A,1)
               n    = size(A,2)
               call compute_block_ranges(size(A,2), nimg, block_size, start_elem, end_elem)
               allocate(A_block(m, block_size(im))[*], C_block(block_size(im), size(B,2))[*])
               A_block(:,:)[im] = A(:, start_elem(im):end_elem(im))
               C_block(:,:)[im] = matmul(A_block(:, :)[im], B, transA=.true., transB=.false., option=option)
               sync all
               if (im == 1) then
                  do i = 1, nimg
                     C(start_elem(i):end_elem(i), :) = C_block(:,:)[i]
                  end do
               end if
            end if
         end if
      else if (.not.present(transA) .and. .not.present(transB)) then
         ! AB
         allocate(C(size(A,1), size(B,2)), source=0.0_rk)
         im   = this_image()
         nimg = num_images()
         m    = size(A,1)
         n    = size(A,2)
         call compute_block_ranges(size(B,2), nimg, block_size, start_elem, end_elem)
         allocate(B_block(n, block_size(im))[*], C_block(m, block_size(im))[*])
         B_block(:,:)[im] = B(:, start_elem(im):end_elem(im))
         C_block(:,:)[im] = matmul(A, B_block(:,:)[im], transA=.false., transB=.false., option=option)
         sync all
         if (im == 1) then
            do i = 1, nimg
               C(:,start_elem(i):end_elem(i)) = C_block(:,:)[i]
            end do
         end if
      end if

#else
      C = matmul(A, B, transA=transA, transB=transB, option=option)
#endif

   end function mat_mat_coarray_rel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   impure function mat_vec_coarray_rel(A, v, transA, option, coarray) result(w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      character(*), intent(in), optional :: option
      logical, intent(in), optional :: transA
      real(rk), allocatable :: w(:)
      logical, intent(in)   :: coarray
#if defined (USE_COARRAY)
      integer               :: i, im, nimg, n, m
      integer               :: block_size(num_images()), start_elem(num_images()), end_elem(num_images())
      real(rk), allocatable :: w_block(:)[:], v_block(:)[:], A_block(:,:)[:]

      if (present(transA)) then
         if (transA) then
            ! ATv
            allocate(w(size(A,2)), source=0.0_rk)
            im   = this_image()
            nimg = num_images()
            call compute_block_ranges(size(A,2), nimg, block_size, start_elem, end_elem)
            allocate(w_block(block_size(im))[*], A_block(size(A,1), block_size(im))[*])
            A_block(:,:)[im] = A(:, start_elem(im):end_elem(im))
            w_block(:)[im] = matmul(A_block(:, :)[im], v, transA=.true., option=option)
            sync all
            if (im == 1) then
               do i = 1, nimg
                  w(start_elem(i):end_elem(i)) = w_block(:)[i]
               end do
            end if
         else if (.not. transA) then
            ! Av
            allocate(w(size(A,1)), source=0.0_rk)
            im   = this_image()
            nimg = num_images()
            call compute_block_ranges(size(A,2), nimg, block_size, start_elem, end_elem)
            allocate(w_block(size(A,1))[*], v_block(block_size(im))[*], A_block(size(A,1), block_size(im))[*])
            A_block(:,:)[im] = A(:, start_elem(im):end_elem(im))
            v_block(:)[im]   = v(start_elem(im):end_elem(im))
            w_block(:)[im]   = matmul(A_block(:,:)[im], v_block(:)[im], transA=.false., option=option)
            sync all
            if (im == 1) then
               do i = 1, nimg
                  w(:) = w(:) + w_block(:)[i]
               end do
            end if
         end if
      else if (.not. present(transA)) then
         ! Av
         allocate(w(size(A,1)), source=0.0_rk)
         im   = this_image()
         nimg = num_images()
         call compute_block_ranges(size(A,2), nimg, block_size, start_elem, end_elem)
         allocate(w_block(size(A,1))[*], v_block(block_size(im))[*], A_block(size(A,1), block_size(im))[*])
         A_block(:,:)[im] = A(:, start_elem(im):end_elem(im))
         v_block(:)[im]   = v(start_elem(im):end_elem(im))
         w_block(:)[im]   = matmul(A_block(:,:)[im], v_block(:)[im], transA=.false., option=option)
         sync all
         if (im == 1) then
            do i = 1, nimg
               w(:) = w(:) + w_block(:)[i]
            end do
         end if
      end if

#else
      w = matmul(A, v, transA=transA, option=option)
#endif

   end function mat_vec_coarray_rel
   !===============================================================================


   !===============================================================================
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
      do i = 2, nimg
         start_elem(i) = start_elem(i - 1) + block_size(i - 1)
      end do
      end_elem(1) = block_size(1)
      end_elem(2:) = start_elem(2:) + block_size(2:) - 1
      ! Check if the block sizes are valid.
      if (minval(block_size) <= 0) error stop 'ForDot: reduce the number of images of coarray.'
   end subroutine compute_block_ranges
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure function mat_mat_block_rel(a, b, transA, transB, option, nblock) result(c)
      real(rk), intent(in), contiguous :: a(:,:), b(:,:)
      logical, intent(in), optional :: transA, transB
      character(*), intent(in), optional :: option
      integer, intent(in)   :: nblock
      real(rk), allocatable :: c(:,:)
      integer               :: ib, se, ee
      integer :: block_size(nblock), start_elem(nblock), end_elem(nblock)

      if (present(transA) .and. present(transB)) then
         if (.not.transA .and. .not.transB) then
            ! AB
            allocate(C(size(A,1), size(B,2)), source=0.0_rk)
            call compute_block_ranges(size(B,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)            
            do concurrent (ib = 1: nblock)
               se = start_elem(ib)
               ee = end_elem(ib)
               C(:, se:ee) = &
                  C(:, se:ee) + matmul(A, B(:,se:ee), transA=.false., transB=.false., option=option)
            end do
#else
            do ib = 1, nblock
               se = start_elem(ib)
               ee = end_elem(ib)
               C(:, se:ee) = &
                  C(:, se:ee) + matmul(A, B(:,se:ee), transA=.false., transB=.false., option=option)
            end do
#endif
         else if (transA .and. transB) then
            ! ATBT
            allocate(C(size(A,2), size(B,1)), source=0.0_rk)
            call compute_block_ranges(size(A,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)            
            do concurrent (ib = 1: nblock)
               se = start_elem(ib)
               ee = end_elem(ib)
               C(se:ee, :) = &
                  C(se:ee, :) + matmul(A(:, se:ee), B, transA=.true., transB=.true., option=option)
            end do
#else
            do ib = 1, nblock
               se = start_elem(ib)
               ee = end_elem(ib)
               C(se:ee, :) = &
                  C(se:ee, :) + matmul(A(:, se:ee), B, transA=.true., transB=.true., option=option)
            end do
#endif
         else if (transA .and. .not.transB) then
            ! ATB
            allocate(C(size(A,2), size(B,2)), source=0.0_rk)
            call compute_block_ranges(size(A,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)            
            do concurrent (ib = 1: nblock)
               se = start_elem(ib)
               ee = end_elem(ib)
               C(se:ee, :) = &
                  C(se:ee, :) + matmul(A(:, se:ee), B, transA=.true., transB=.false., option=option)
            end do
#else
            do ib = 1, nblock
               se = start_elem(ib)
               ee = end_elem(ib)
               C(se:ee, :) = &
                  C(se:ee, :) + matmul(A(:, se:ee), B, transA=.true., transB=.false., option=option)
            end do
#endif
         else if (.not.transA .and. transB) then
            ! ABT
            allocate(C(size(A,1), size(B,1)), source=0.0_rk)
            call compute_block_ranges(size(A,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)            
            do concurrent (ib = 1: nblock)
               se = start_elem(ib)
               ee = end_elem(ib)
               C(:, :) = C(:, :) + &
                  matmul(A(:, se:ee), B(:,se:ee), transA=.false., transB=.true., option=option)
            end do
#else
            do ib = 1, nblock
               se = start_elem(ib)
               ee = end_elem(ib)
               C(:, :) = C(:, :) + &
                  matmul(A(:, se:ee), B(:,se:ee), transA=.false., transB=.true., option=option)
            end do
#endif
         end if
      else if (present(transA) .or. present(transB)) then
         if (present(transA)) then
            if (transA) then
               ! ATB
               allocate(C(size(A,2), size(B,2)), source=0.0_rk)
               call compute_block_ranges(size(A,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)               
               do concurrent (ib = 1: nblock)
                  se = start_elem(ib)
                  ee = end_elem(ib)
                  C(se:ee, :) = &
                     C(se:ee, :) + matmul(A(:, se:ee), B, transA=.true., transB=.false., option=option)
               end do
#else
               do ib = 1, nblock
                  se = start_elem(ib)
                  ee = end_elem(ib)
                  C(se:ee, :) = &
                     C(se:ee, :) + matmul(A(:, se:ee), B, transA=.true., transB=.false., option=option)
               end do
#endif
            else if (.not.transA) then
               ! ABT
               allocate(C(size(A,1), size(B,1)), source=0.0_rk)
               call compute_block_ranges(size(A,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)               
               do concurrent (ib = 1: nblock)
                  se = start_elem(ib)
                  ee = end_elem(ib)
                  C(:, :) = C(:, :) + &
                     matmul(A(:, se:ee), B(:,se:ee), transA=.false., transB=.true., option=option)
               end do
#else
               do ib = 1, nblock
                  se = start_elem(ib)
                  ee = end_elem(ib)
                  C(:, :) = C(:, :) + &
                     matmul(A(:, se:ee), B(:,se:ee), transA=.false., transB=.true., option=option)
               end do
#endif
            end if
         else if (present(transB)) then
            if (transB) then
               ! ABT
               allocate(C(size(A,1), size(B,1)), source=0.0_rk)
               call compute_block_ranges(size(A,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)               
               do concurrent (ib = 1: nblock)
                  se = start_elem(ib)
                  ee = end_elem(ib)
                  C(:, :) = C(:, :) + &
                     matmul(A(:, se:ee), B(:,se:ee), transA=.false., transB=.true., option=option)
               end do
#else
               do ib = 1, nblock
                  se = start_elem(ib)
                  ee = end_elem(ib)
                  C(:, :) = C(:, :) + &
                     matmul(A(:, se:ee), B(:,se:ee), transA=.false., transB=.true., option=option)
               end do
#endif
            else if (.not.transB) then
               ! ATB
               allocate(C(size(A,2), size(B,2)), source=0.0_rk)
               call compute_block_ranges(size(A,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)               
               do concurrent (ib = 1: nblock)
                  se = start_elem(ib)
                  ee = end_elem(ib)
                  C(se:ee, :) = &
                     C(se:ee, :) + matmul(A(:, se:ee), B, transA=.true., transB=.false., option=option)
               end do
#else
               do ib = 1, nblock
                  se = start_elem(ib)
                  ee = end_elem(ib)
                  C(se:ee, :) = &
                     C(se:ee, :) + matmul(A(:, se:ee), B, transA=.true., transB=.false., option=option)
               end do
#endif
            end if
         end if
      else if (.not.present(transA) .and. .not.present(transB)) then
         ! AB
         allocate(C(size(A,1), size(B,2)), source=0.0_rk)
         call compute_block_ranges(size(B,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)         
         do concurrent (ib = 1: nblock)
            se = start_elem(ib)
            ee = end_elem(ib)
            C(:, se:ee) = &
               C(:, se:ee) + matmul(A, B(:,se:ee), transA=.false., transB=.false., option=option)
         end do
#else
         do ib = 1, nblock
            se = start_elem(ib)
            ee = end_elem(ib)
            C(:, se:ee) = &
               C(:, se:ee) + matmul(A, B(:,se:ee), transA=.false., transB=.false., option=option)
         end do
#endif
      end if

   end function mat_mat_block_rel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure function mat_vec_block_rel(A, v, transA, option, nblock) result(w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      logical, intent(in), optional :: transA
      character(*), intent(in), optional :: option
      integer, intent(in)   :: nblock
      real(rk), allocatable :: w(:)
      integer               :: ib, se, ee
      integer :: block_size(nblock), start_elem(nblock), end_elem(nblock)


      if (present(transA)) then
         if (transA) then
            ! ATv
            allocate(w(size(A,2)), source=0.0_rk)
            call compute_block_ranges(size(A,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)
            do concurrent (ib = 1: nblock)
               se = start_elem(ib)
               ee = end_elem(ib)
               w(se:ee) = &
                  w(se:ee) + matmul(A(:,se:ee), v, transA=.true., option=option)
            end do
#else
            do ib = 1, nblock
               se = start_elem(ib)
               ee = end_elem(ib)
               w(se:ee) = &
                  w(se:ee) + matmul(A(:,se:ee), v, transA=.true., option=option)
            end do
#endif
         else if (.not. transA) then
            ! Av
            allocate(w(size(A,1)), source=0.0_rk)
            call compute_block_ranges(size(A,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)
            do concurrent (ib = 1: nblock)
               se = start_elem(ib)
               ee = end_elem(ib)
               w(:) = &
                  w(:) + matmul(A(:,se:ee), v(se:ee), transA=.false., option=option)
            end do
#else
            do ib = 1, nblock
               se = start_elem(ib)
               ee = end_elem(ib)
               w(:) = &
                  w(:) + matmul(A(:,se:ee), v(se:ee), transA=.false., option=option)
            end do
#endif
         end if
      else if (.not. present(transA)) then
         ! Av
         allocate(w(size(A,1)), source=0.0_rk)
         call compute_block_ranges(size(A,2), nblock, block_size, start_elem, end_elem)
#if defined(USE_DO_CONCURRENT)
         do concurrent (ib = 1: nblock)
            se = start_elem(ib)
            ee = end_elem(ib)
            w(:) = &
               w(:) + matmul(A(:,se:ee), v(se:ee), transA=.false., option=option)
         end do
#else
         do ib = 1, nblock
            se = start_elem(ib)
            ee = end_elem(ib)
            w(:) = &
               w(:) + matmul(A(:,se:ee), v(se:ee), transA=.false., option=option)
         end do
#endif
      end if

   end function mat_vec_block_rel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure function mat_mat_rel(A, B, transA, transB, option) result(C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), allocatable :: C(:,:)
      character(*), intent(in), optional :: option
      logical, intent(in), optional :: transA, transB

      if (present(transA) .and. present(transB)) then
         if (.not.transA .and. .not.transB) then
            ! AB
            allocate(C(size(A,1), size(B,2)), source=0.0_rk)
            call mat_mat_rel_AB(A, B, C, option)
         else if (transA .and. transB) then
            ! ATBT
            allocate(C(size(A,2), size(B,1)), source=0.0_rk)
            call mat_mat_rel_ATBT(A, B, C, option)
         else if (transA .and. .not.transB) then
            ! ATB
            allocate(C(size(A,2), size(B,2)), source=0.0_rk)
            call mat_mat_rel_ATB(A, B, C, option)
         else if (.not.transA .and. transB) then
            ! ABT
            allocate(C(size(A,1), size(B,1)), source=0.0_rk)
            call mat_mat_rel_ABT(A, B, C, option)
         end if
      else if (present(transA) .or. present(transB)) then
         if (present(transA)) then
            if (transA) then
               ! ATB
               allocate(C(size(A,2), size(B,2)), source=0.0_rk)
               call mat_mat_rel_ATB(A, B, C, option)
            else if (.not.transA) then
               ! ABT
               allocate(C(size(A,1), size(B,1)), source=0.0_rk)
               call mat_mat_rel_ABT(A, B, C, option)
            end if
         else if (present(transB)) then
            if (transB) then
               ! ABT
               allocate(C(size(A,1), size(B,1)), source=0.0_rk)
               call mat_mat_rel_ABT(A, B, C, option)
            else if (.not.transB) then
               ! ATB
               allocate(C(size(A,2), size(B,2)), source=0.0_rk)
               call mat_mat_rel_ATB(A, B, C, option)
            end if
         end if
      else if (.not.present(transA) .and. .not.present(transB)) then
         ! AB
         allocate(C(size(A,1), size(B,2)), source=0.0_rk)
         call mat_mat_rel_AB(A, B, C, option)
      end if
   end function mat_mat_rel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure function mat_vec_rel(A, v, transA, option) result(w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), allocatable :: w(:)
      character(*), intent(in), optional :: option
      logical, intent(in), optional :: transA

      if (present(transA)) then
         if (transA) then
            ! ATv
            allocate(w(size(A,2)), source=0.0_rk)
            call mat_vec_rel_ATv(A, v , w, option)
         else if (.not. transA) then
            ! Av
            allocate(w(size(A,1)), source=0.0_rk)
            call mat_vec_rel_Av(A, v, w, option)
         end if
      else if (.not. present(transA)) then
         ! Av
         allocate(w(size(A,1)), source=0.0_rk)
         call mat_vec_rel_Av(A, v, w, option)
      end if

   end function mat_vec_rel
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_mat_rel_AB(A, B, C, option)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      character(*), intent(in), optional :: option

      if (present(option)) then
         call mat_mat_rel_AB_opt(A, B, C, option)
      else
         call mat_mat_rel_AB_opt(A, B, C, 'm2')
      end if
   end subroutine mat_mat_rel_AB
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_mat_rel_ATB(A, B, C, option)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      character(*), intent(in), optional :: option

      if (present(option)) then
         call mat_mat_rel_ATB_opt(A, B, C, option)
      else
         call mat_mat_rel_ATB_opt(A, B, C, 'm2')
      end if
   end subroutine mat_mat_rel_ATB
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_mat_rel_ABT(A, B, C, option)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      character(*), intent(in), optional :: option

      if (present(option)) then
         call mat_mat_rel_ABT_opt(A, B, C, option)
      else
         call mat_mat_rel_ABT_opt(A, B, C, 'm2')
      end if
   end subroutine mat_mat_rel_ABT
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_mat_rel_ATBT(A, B, C, option)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      character(*), intent(in), optional :: option

      if (present(option)) then
         call mat_mat_rel_ATBT_opt(A, B, C, option)
      else
         call mat_mat_rel_ATBT_opt(A, B, C, 'm2')
      end if
   end subroutine mat_mat_rel_ATBT
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_vec_rel_Av(A, v, w, option)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      character(*), intent(in), optional :: option

      if (present(option)) then
         call mat_vec_rel_Av_opt(A, v, w, option)
      else
         call mat_vec_rel_Av_opt(A, v, w, 'm2')
      end if
   end subroutine mat_vec_rel_Av
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_vec_rel_ATv(A, v, w, option)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      character(*), intent(in), optional :: option

      if (present(option)) then
         call mat_vec_rel_ATv_opt(A, v, w, option)
      else
         call mat_vec_rel_ATv_opt(A, v, w, 'm2')
      end if
   end subroutine mat_vec_rel_ATv
   !===============================================================================


   !    ! Ambiguous interface
   !    !===============================================================================
   !    !> author: Seyed Ali Ghasemi
   !    pure function vec_mat_rel(v, A, transA, option) result(w)
   !       real(rk), intent(in), contiguous :: v(:), A(:,:)
   !       real(rk), allocatable :: w(:)
   !       character(*), intent(in), optional :: option
   !       logical, intent(in), optional :: transA

   !       if (present(transA)) then
   !          if (transA) then
   !             ! ATv
   !             allocate(w(size(A,1)), source=0.0_rk)
   !             call vec_mat_rel_ATv(v, A, w, option)
   !          else if (.not. transA) then
   !             ! Av
   !             allocate(w(size(A,2)), source=0.0_rk)
   !             call vec_mat_rel_Av(v, A, w, option)
   !          end if
   !       else if (.not. present(transA)) then
   !          ! Av
   !          allocate(w(size(A,2)), source=0.0_rk)
   !          call vec_mat_rel_Av(v, A, w, option)
   !       end if

   !    end function vec_mat_rel
   !    !===============================================================================


   !    !===============================================================================
   !    !> author: Seyed Ali Ghasemi
   !    pure subroutine vec_mat_rel_Av(v, A, w, option)
   !       real(rk), intent(in), contiguous :: A(:,:), v(:)
   !       real(rk), intent(inout), contiguous :: w(:)
   !       character(*), intent(in), optional :: option

   !       if (present(option)) then
   !          call vec_mat_rel_Av_opt(v, A, w, option)
   !       else
   !          call vec_mat_rel_Av_opt(v, A, w, 'm2')
   !       end if
   !    end subroutine vec_mat_rel_Av
   !    !===============================================================================


   !    !===============================================================================
   !    !> author: Seyed Ali Ghasemi
   !    pure subroutine vec_mat_rel_ATv(v, A, w, option)
   !       real(rk), intent(in), contiguous :: A(:,:), v(:)
   !       real(rk), intent(inout), contiguous :: w(:)
   !       character(*), intent(in), optional :: option

   !       if (present(option)) then
   !          call vec_mat_rel_ATv_opt(v, A, w, option)
   !       else
   !          call vec_mat_rel_ATv_opt(v, A, w, 'm2')
   !       end if
   !    end subroutine vec_mat_rel_ATv
   !    !===============================================================================


   !    !===============================================================================
   !    !> author: Seyed Ali Ghasemi
   !    pure subroutine vec_mat_rel_Av_opt(v, A, w, option)
   !       real(rk), intent(in), contiguous :: A(:,:), v(:)
   !       real(rk), intent(inout), contiguous :: w(:)
   !       character(*), intent(in) :: option

   !       !   select case (option)
   !       !    case ('m1')
   !       !      call vm_vA_1(v, A, w)
   !       !    case ('m2')
   !       !      call vm_vA_2(v, A, w)
   !       !    case ('m3')
   !       !      call vm_vA_3(v, A, w)
   !       !    case ('m4')
   !       !      call vm_vA_4(v, A, w)
   !       !    case ('m5')
   !       !      call vm_vA_5(v, A, w)
   !       !    case ('m6')
   !       !      call vm_vA_6(v, A, w)
   !       !    case ('m7')
   !       !      call vm_vA_7(v, A, w)
   !       !    case ('m8')
   !       !      call vm_vA_8(v, A, w)
   !       !   end select

   !    end subroutine vec_mat_rel_Av_opt
   !    !===============================================================================


   !    !===============================================================================
   !    !> author: Seyed Ali Ghasemi
   !    pure subroutine vec_mat_rel_ATv_opt(v, A, w, option)
   !       real(rk), intent(in), contiguous :: A(:,:), v(:)
   !       real(rk), intent(inout), contiguous :: w(:)
   !       character(*), intent(in) :: option

   !       !   select case (option)
   !       !    case ('m1')
   !       !      call vm_vAT_1(v, A, w)
   !       !    case ('m2')
   !       !      call vm_vAT_2(v, A, w)
   !       !    case ('m3')
   !       !      call vm_vAT_3(v, A, w)
   !       !    case ('m4')
   !       !      call vm_vAT_4(v, A, w)
   !       !    case ('m5')
   !       !      call vm_vAT_5(v, A, w)
   !       !    case ('m6')
   !       !      call vm_vAT_6(v, A, w)
   !       !    case ('m7')
   !       !      call vm_vAT_7(v, A, w)
   !       !    case ('m8')
   !       !      call vm_vAT_8(v, A, w)
   !       !   end select

   !    end subroutine vec_mat_rel_ATv_opt
   !    !===============================================================================

end module formatmul