module formatmul_opts

   use kinds

   implicit none

   private

   public &
      mat_mat_rel_AB_opt, &
      mat_mat_rel_ATB_opt, &
      mat_mat_rel_ABT_opt, &
      mat_mat_rel_ATBT_opt, &
      mat_vec_rel_Av_opt, &
      mat_vec_rel_Atv_opt

contains

   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_mat_rel_AB_opt(A, B, C, option)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      character(*), intent(in) :: option

      select case (option)
       case ('m1')
         call mm_AB_1(A, B, C)
       case ('m2')
         call mm_AB_2(A, B, C)
       case ('m3')
         call mm_AB_3(A, B, C)
       case ('m4')
         call mm_AB_4(A, B, C)
       case ('m5')
         call mm_AB_5(A, B, C)
       case ('m6')
         call mm_AB_6(A, B, C)
       case ('m7')
         call mm_AB_7(A, B, C)
       case ('m8')
         call mm_AB_8(A, B, C)
       case ('m9')
         call mm_AB_9(A, B, C)
       case ('m10')
         call mm_AB_10(A, B, C)
       case ('m11')
         call mm_AB_11(A, B, C)
       case ('m12')
         call mm_AB_12(A, B, C)
       case ('m13')
         call mm_AB_13(A, B, C)
      end select

   end subroutine mat_mat_rel_AB_opt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_mat_rel_ATB_opt(A, B, C, option)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      character(*), intent(in) :: option

      select case (option)
       case ('m1')
         call mm_ATB_1(A, B, C)
       case ('m2')
         call mm_ATB_2(A, B, C)
       case ('m3')
         call mm_ATB_3(A, B, C)
       case ('m4')
         call mm_ATB_4(A, B, C)
       case ('m5')
         call mm_ATB_5(A, B, C)
       case ('m6')
         call mm_ATB_6(A, B, C)
       case ('m7')
         call mm_ATB_7(A, B, C)
       case ('m8')
         call mm_ATB_8(A, B, C)
       case ('m9')
         call mm_ATB_9(A, B, C)
       case ('m10')
         call mm_ATB_10(A, B, C)
       case ('m11')
         call mm_ATB_11(A, B, C)
       case ('m12')
         call mm_ATB_12(A, B, C)
       case ('m13')
         call mm_ATB_13(A, B, C)
      end select

   end subroutine mat_mat_rel_ATB_opt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_mat_rel_ABT_opt(A, B, C, option)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      character(*), intent(in) :: option

      select case (option)
       case ('m1')
         call mm_ABT_1(A, B, C)
       case ('m2')
         call mm_ABT_2(A, B, C)
       case ('m3')
         call mm_ABT_3(A, B, C)
       case ('m4')
         call mm_ABT_4(A, B, C)
       case ('m5')
         call mm_ABT_5(A, B, C)
       case ('m6')
         call mm_ABT_6(A, B, C)
       case ('m7')
         call mm_ABT_7(A, B, C)
       case ('m8')
         call mm_ABT_8(A, B, C)
       case ('m9')
         call mm_ABT_9(A, B, C)
       case ('m10')
         call mm_ABT_10(A, B, C)
       case ('m11')
         call mm_ABT_11(A, B, C)
       case ('m12')
         call mm_ABT_12(A, B, C)
       case ('m13')
         call mm_ABT_13(A, B, C)
      end select

   end subroutine mat_mat_rel_ABT_opt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_mat_rel_ATBT_opt(A, B, C, option)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      character(*), intent(in) :: option

      select case (option)
       case ('m1')
         call mm_ATBT_1(A, B, C)
       case ('m2')
         call mm_ATBT_2(A, B, C)
       case ('m3')
         call mm_ATBT_3(A, B, C)
       case ('m4')
         call mm_ATBT_4(A, B, C)
       case ('m5')
         call mm_ATBT_5(A, B, C)
       case ('m6')
         call mm_ATBT_6(A, B, C)
       case ('m7')
         call mm_ATBT_7(A, B, C)
       case ('m8')
         call mm_ATBT_8(A, B, C)
       case ('m9')
         call mm_ATBT_9(A, B, C)
       case ('m10')
         call mm_ATBT_10(A, B, C)
       case ('m11')
         call mm_ATBT_11(A, B, C)
       case ('m12')
         call mm_ATBT_12(A, B, C)
       case ('m13')
         call mm_ATBT_13(A, B, C)
      end select

   end subroutine mat_mat_rel_ATBT_opt
   !===============================================================================

      !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_vec_rel_Av_opt(A, v, w, option)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      character(*), intent(in) :: option

      select case (option)
       case ('m1')
         call mv_Av_1(A, v, w)
       case ('m2')
         call mv_Av_2(A, v, w)
       case ('m3')
         call mv_Av_3(A, v, w)
       case ('m4')
         call mv_Av_4(A, v, w)
       case ('m5')
         call mv_Av_5(A, v, w)
       case ('m6')
         call mv_Av_6(A, v, w)
       case ('m7')
         call mv_Av_7(A, v, w)
       case ('m8')
         call mv_Av_8(A, v, w)
      end select

   end subroutine mat_vec_rel_Av_opt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mat_vec_rel_ATv_opt(A, v, w, option)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      character(*), intent(in) :: option

      select case (option)
       case ('m1')
         call mv_ATv_1(A, v, w)
       case ('m2')
         call mv_ATv_2(A, v, w)
       case ('m3')
         call mv_ATv_3(A, v, w)
       case ('m4')
         call mv_ATv_4(A, v, w)
       case ('m5')
         call mv_ATv_5(A, v, w)
       case ('m6')
         call mv_ATv_6(A, v, w)
       case ('m7')
         call mv_ATv_7(A, v, w)
       case ('m8')
         call mv_ATv_8(A, v, w)
      end select

   end subroutine mat_vec_rel_ATv_opt
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_1(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      C = matmul(A, B)

   end subroutine mm_AB_1
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_2(A, B, C)
      use external_interfaces

      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer                 :: m, n

      m = size(A, 1)
      n = size(A, 2)
      ! Call BLAS gemm subroutine for matrix-matrix multiplication.
      call gemm('N', 'N', m, size(B, 2), n, 1.0_rk, A, m, B, n, 0.0_rk, C, m)

   end subroutine mm_AB_2
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_3(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,2)

      do i=1,m
         do j=1,n
            do k=1,p
               c(i,k) = c(i,k) + a(i,j)*b(j,k)
            end do
         end do
      end do

   end subroutine mm_AB_3
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_4(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,2)

      do i=1,m
         do j=1,p
            do k=1,n
               c(i,j) = c(i,j) + a(i,k)*b(k,j)
            end do
         end do
      end do

   end subroutine mm_AB_4
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_5(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,2)

      do i=1,n
         do j=1,m
            do k=1,p
               c(j,k) = c(j,k) + a(j,i)*b(i,k)
            end do
         end do
      end do

   end subroutine mm_AB_5
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_6(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,2)

      do i=1,n
         do j=1,p
            do k=1,m
               c(k,j) = c(k,j) + a(k,i)*b(i,j)
            end do
         end do
      end do

   end subroutine mm_AB_6
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_7(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,2)

      do i=1,p
         do j=1,m
            do k=1,n
               c(j,i) = c(j,i) + a(j,k)*b(k,i)
            end do
         end do
      end do

   end subroutine mm_AB_7
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_8(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,2)

      do i=1,p
         do j=1,n
            do k=1,m
               c(k,i) = c(k,i) + a(k,j)*b(j,i)
            end do
         end do
      end do

   end subroutine mm_AB_8
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_9(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, p
      integer :: i, k

      m = size(A,1)
      p = size(B,2)

      do i = 1, p
         do k = 1, m
            c(k,i) = dot_product(a(k,:), b(:,i))
         end do
      end do

   end subroutine mm_AB_9
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_10(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, p
      integer :: i, k

      m = size(A,1)
      p = size(B,2)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !   do concurrent (i = 1: p) shared(m, p) ! check shared variables
      !     do k = 1, m
      !         c(k,i) = dot_product(a(k,:), b(:,i))
      !     end do
      !   end do

   end subroutine mm_AB_10
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_11(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,2)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !    do concurrent (i = 1: p) shared(m, n, p) ! check shared variables
      !         do j=1,n
      !             do k=1,m
      !                 c(k,i) = c(k,i) + a(k,j)*b(j,i)
      !             end do
      !         end do
      !     end do

   end subroutine mm_AB_11
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_12(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      interface
         pure subroutine impure_mm_AB_12(f_a, f_b, f_c)
            import rk
            real(rk), intent(in) :: f_a(:,:), f_b(:,:)
            real(rk), intent(inout) :: f_c(:,:)
         end subroutine impure_mm_AB_12
      end interface

      call impure_mm_AB_12(a, b, c)

   end subroutine mm_AB_12
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_AB_13(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      interface
         pure subroutine impure_mm_AB_13(f_a, f_b, f_c)
            import rk
            real(rk), intent(in) :: f_a(:,:), f_b(:,:)
            real(rk), intent(inout) :: f_c(:,:)
         end subroutine impure_mm_AB_13
      end interface

      call impure_mm_AB_13(a, b, c)

   end subroutine mm_AB_13
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_1(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      C = matmul(transpose(A), B)

   end subroutine mm_ATB_1
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_2(A, B, C)
      use external_interfaces

      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer                 :: m, n

      m = size(A, 2)
      n = size(A, 1)
      ! Call BLAS gemm subroutine for matrix-matrix multiplication.
      call gemm('T', 'N', m, size(B, 2), n, 1.0_rk, A, n, B, n, 0.0_rk, C, m)

   end subroutine mm_ATB_2
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_3(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,2)

      do i=1,m
         do j=1,n
            do k=1,p
               c(i,k) = c(i,k) + a(j,i)*b(j,k)
            end do
         end do
      end do

   end subroutine mm_ATB_3
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_4(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,2)

      do i=1,m
         do j=1,p
            do k=1,n
               c(i,j) = c(i,j) + a(k,i)*b(k,j)
            end do
         end do
      end do

   end subroutine mm_ATB_4
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_5(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,2)

      do i=1,n
         do j=1,m
            do k=1,p
               c(j,k) = c(j,k) + a(i,j)*b(i,k)
            end do
         end do
      end do

   end subroutine mm_ATB_5
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_6(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,2)

      do i=1,n
         do j=1,p
            do k=1,m
               c(k,j) = c(k,j) + a(i,k)*b(i,j)
            end do
         end do
      end do

   end subroutine mm_ATB_6
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_7(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,2)

      do i=1,p
         do j=1,m
            do k=1,n
               c(j,i) = c(j,i) + a(k,j)*b(k,i)
            end do
         end do
      end do

   end subroutine mm_ATB_7
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_8(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,2)

      do i=1,p
         do j=1,n
            do k=1,m
               c(k,i) = c(k,i) + a(j,k)*b(j,i)
            end do
         end do
      end do

   end subroutine mm_ATB_8
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_9(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, p
      integer :: i, k

      m = size(A,2)
      p = size(B,2)

      do i = 1, p
         do k = 1, m
            c(k,i) = dot_product(a(:,k), b(:,i))
         end do
      end do

   end subroutine mm_ATB_9
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_10(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, p
      integer :: i, k

      m = size(A,2)
      p = size(B,2)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !   do concurrent (i = 1: p) shared(m, p) ! check shared variables
      !     do k = 1, m
      !         c(k,i) = dot_product(a(:,k), b(:,i))
      !     end do
      !   end do

   end subroutine mm_ATB_10
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_11(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,2)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !    do concurrent (i = 1: p) shared(m, n, p) ! check shared variables
      !         do j=1,n
      !             do k=1,m
      !                 c(k,i) = c(k,i) + a(j,k)*b(j,i)
      !             end do
      !         end do
      !     end do

   end subroutine mm_ATB_11
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_12(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      interface
         pure subroutine impure_mm_ATB_12(f_a, f_b, f_c)
            import rk
            real(rk), intent(in) :: f_a(:,:), f_b(:,:)
            real(rk), intent(inout) :: f_c(:,:)
         end subroutine impure_mm_ATB_12
      end interface

      call impure_mm_ATB_12(a, b, c)

   end subroutine mm_ATB_12
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATB_13(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      interface
         pure subroutine impure_mm_ATB_13(f_a, f_b, f_c)
            import rk
            real(rk), intent(in) :: f_a(:,:), f_b(:,:)
            real(rk), intent(inout) :: f_c(:,:)
         end subroutine impure_mm_ATB_13
      end interface

      call impure_mm_ATB_13(a, b, c)

   end subroutine mm_ATB_13
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_1(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      C = matmul(A, transpose(B))

   end subroutine mm_ABT_1
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_2(A, B, C)
      use external_interfaces

      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer                 :: m, p

      m = size(A, 1)
      p = size(B, 1)
      ! Call BLAS gemm subroutine for matrix-matrix multiplication.
      call gemm('N', 'T', m, p, size(A, 2), 1.0_rk, A, m, B, p, 0.0_rk, C, m)

   end subroutine mm_ABT_2
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_3(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,1)

      do i=1,m
         do j=1,n
            do k=1,p
               c(i,k) = c(i,k) + a(i,j)*b(k,j)
            end do
         end do
      end do

   end subroutine mm_ABT_3
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_4(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,1)

      do i=1,m
         do j=1,p
            do k=1,n
               c(i,j) = c(i,j) + a(i,k)*b(j,k)
            end do
         end do
      end do
   end subroutine mm_ABT_4
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_5(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,1)

      do i=1,n
         do j=1,m
            do k=1,p
               c(j,k) = c(j,k) + a(j,i)*b(k,i)
            end do
         end do
      end do

   end subroutine mm_ABT_5
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_6(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,1)

      do i=1,n
         do j=1,p
            do k=1,m
               c(k,j) = c(k,j) + a(k,i)*b(j,i)
            end do
         end do
      end do

   end subroutine mm_ABT_6
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_7(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,1)

      do i=1,p
         do j=1,m
            do k=1,n
               c(j,i) = c(j,i) + a(j,k)*b(i,k)
            end do
         end do
      end do

   end subroutine mm_ABT_7
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_8(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,1)

      do i=1,p
         do j=1,n
            do k=1,m
               c(k,i) = c(k,i) + a(k,j)*b(i,j)
            end do
         end do
      end do

   end subroutine mm_ABT_8
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_9(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, p
      integer :: i, k

      m = size(A,1)
      p = size(B,1)

      do i = 1, p
         do k = 1, m
            c(k,i) = dot_product(a(k,:), b(i,:))
         end do
      end do

   end subroutine mm_ABT_9
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_10(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, p
      integer :: i, k

      m = size(A,1)
      p = size(B,1)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !   do concurrent (i = 1: p) shared(m, p) ! check shared variables
      !     do k = 1, m
      !         c(k,i) = dot_product(a(k,:), b(i,:))
      !     end do
      !   end do

   end subroutine mm_ABT_10
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_11(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,1)
      n = size(A,2)
      p = size(B,1)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !    do concurrent (i = 1: p) shared(m, n, p) ! check shared variables
      !         do j=1,n
      !             do k=1,m
      !                 c(k,i) = c(k,i) + a(k,j)*b(i,j)
      !             end do
      !         end do
      !     end do

   end subroutine mm_ABT_11
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_12(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      interface
         pure subroutine impure_mm_ABT_12(f_a, f_b, f_c)
            import rk
            real(rk), intent(in) :: f_a(:,:), f_b(:,:)
            real(rk), intent(inout) :: f_c(:,:)
         end subroutine impure_mm_ABT_12
      end interface

      call impure_mm_ABT_12(a, b, c)

   end subroutine mm_ABT_12
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ABT_13(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      interface
         pure subroutine impure_mm_ABT_13(f_a, f_b, f_c)
            import rk
            real(rk), intent(in) :: f_a(:,:), f_b(:,:)
            real(rk), intent(inout) :: f_c(:,:)
         end subroutine impure_mm_ABT_13
      end interface

      call impure_mm_ABT_13(a, b, c)

   end subroutine mm_ABT_13
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_1(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      C = matmul(transpose(A), transpose(B))

   end subroutine mm_ATBT_1
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_2(A, B, C)
      use external_interfaces

      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer                 :: m, n, p

      m = size(A, 2)
      n = size(A, 1)
      p = size(B, 1)
      ! Call BLAS gemm subroutine for matrix-matrix multiplication.
      call gemm('T', 'T', m, p, n, 1.0_rk, A, n, B, p, 0.0_rk, C, m)

   end subroutine mm_ATBT_2
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_3(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,1)

      do i=1,m
         do j=1,n
            do k=1,p
               c(i,k) = c(i,k) + a(j,i)*b(k,j)
            end do
         end do
      end do

   end subroutine mm_ATBT_3
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_4(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,1)

      do i=1,m
         do j=1,p
            do k=1,n
               c(i,j) = c(i,j) + a(k,i)*b(j,k)
            end do
         end do
      end do

   end subroutine mm_ATBT_4
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_5(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,1)

      do i=1,n
         do j=1,m
            do k=1,p
               c(j,k) = c(j,k) + a(i,j)*b(k,i)
            end do
         end do
      end do

   end subroutine mm_ATBT_5
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_6(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,1)

      do i=1,n
         do j=1,p
            do k=1,m
               c(k,j) = c(k,j) + a(i,k)*b(j,i)
            end do
         end do
      end do

   end subroutine mm_ATBT_6
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_7(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,1)

      do i=1,p
         do j=1,m
            do k=1,n
               c(j,i) = c(j,i) + a(k,j)*b(i,k)
            end do
         end do
      end do

   end subroutine mm_ATBT_7
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_8(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,1)

      do i=1,p
         do j=1,n
            do k=1,m
               c(k,i) = c(k,i) + a(j,k)*b(i,j)
            end do
         end do
      end do

   end subroutine mm_ATBT_8
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_9(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, p
      integer :: i, k

      m = size(A,2)
      p = size(B,1)

      do i = 1, p
         do k = 1, m
            c(k,i) = dot_product(a(:,k), b(i,:))
         end do
      end do

   end subroutine mm_ATBT_9
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_10(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, p
      integer :: i, k

      m = size(A,2)
      p = size(B,1)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !   do concurrent (i = 1: p) shared(m, p) ! check shared variables
      !     do k = 1, m
      !         c(k,i) = dot_product(a(:,k), b(i,:))
      !     end do
      !   end do

   end subroutine mm_ATBT_10
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_11(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)
      integer :: m, n, p
      integer :: i, j, k

      m = size(A,2)
      n = size(A,1)
      p = size(B,1)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !    do concurrent (i = 1: p) shared(m, n, p) ! check shared variables
      !         do j=1,n
      !             do k=1,m
      !                 c(k,i) = c(k,i) + a(j,k)*b(i,j)
      !             end do
      !         end do
      !     end do


   end subroutine mm_ATBT_11
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_12(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      interface
         pure subroutine impure_mm_ATBT_12(f_a, f_b, f_c)
            import rk
            real(rk), intent(in) :: f_a(:,:), f_b(:,:)
            real(rk), intent(inout) :: f_c(:,:)
         end subroutine impure_mm_ATBT_12
      end interface

      call impure_mm_ATBT_12(a, b, c)

   end subroutine mm_ATBT_12
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mm_ATBT_13(A, B, C)
      real(rk), intent(in), contiguous :: A(:,:), B(:,:)
      real(rk), intent(inout), contiguous :: C(:,:)

      interface
         pure subroutine impure_mm_ATBT_13(f_a, f_b, f_c)
            import rk
            real(rk), intent(in) :: f_a(:,:), f_b(:,:)
            real(rk), intent(inout) :: f_c(:,:)
         end subroutine impure_mm_ATBT_13
      end interface

      call impure_mm_ATBT_13(a, b, c)

   end subroutine mm_ATBT_13
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_Av_1(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)

      w = matmul(A, v)

   end subroutine mv_Av_1
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_Av_2(A, v, w)
      use external_interfaces

      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer                             :: m

      m = size(A, 1)
      ! Call BLAS gemv subroutine for matrix-vector multiplication.
      call gemv('N', m, size(A, 2), 1.0_rk, A, m, v, 1, 0.0_rk, w , 1)

   end subroutine mv_Av_2
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_Av_3(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: m, n
      integer :: i, j

      m = size(A,1)
      n = size(A,2)

      do i=1,m
         do j=1,n
            w(i) = w(i) + a(i,j)*v(j)
         end do
      end do

   end subroutine mv_Av_3
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_Av_4(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: m, n
      integer :: i, j

      m = size(A,1)
      n = size(A,2)

      do j=1,n
         do i=1,m
            w(i) = w(i) + a(i,j)*v(j)
         end do
      end do

   end subroutine mv_Av_4
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_Av_5(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: m
      integer :: k

      m = size(A,1)

      do k = 1, m
         w(k) = dot_product(a(k,:), v(:))
      end do

   end subroutine mv_Av_5
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_Av_6(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: n
      integer :: k

      n = size(A,2)

      do k = 1, n
         w(:) = w(:) + a(:,k)*v(k)
      end do

   end subroutine mv_Av_6
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_Av_7(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: m, n
      integer :: k

      m = size(A,1)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !   do concurrent (k = 1: m) shared(m,a,v) ! check shared variables
      !      w(k) = dot_product(a(k,:), v(:))
      !   end do

   end subroutine mv_Av_7
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_Av_8(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: m, n
      integer :: i, j

      m = size(A,1)
      n = size(A,2)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !   do concurrent(i=1:m) shared(m, n, a, v) ! check shared variables
      !      do j=1,n
      !         w(i) = w(i) + a(i,j)*v(j)
      !      end do
      !   end do

   end subroutine mv_Av_8
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_ATv_1(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)

      w = matmul(transpose(A), v)

   end subroutine mv_ATv_1
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_ATv_2(A, v, w)
      use external_interfaces

      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer                             :: m

      m = size(A, 1)

      ! Call BLAS gemv subroutine for matrix-vector multiplication.
      call gemv('T', m, size(A, 2), 1.0_rk, A, m, v, 1, 0.0_rk, w , 1)

   end subroutine mv_ATv_2
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_ATv_3(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: m, n
      integer :: i, j

      m = size(A,2)
      n = size(A,1)

      do i=1,m
         do j=1,n
            w(i) = w(i) + a(j,i)*v(j)
         end do
      end do

   end subroutine mv_ATv_3
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_ATv_4(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: m, n
      integer :: i, j

      m = size(A,2)
      n = size(A,1)

      do j=1,n
         do i=1,m
            w(i) = w(i) + a(j,i)*v(j)
         end do
      end do

   end subroutine mv_ATv_4
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_ATv_5(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: m
      integer :: k

      m = size(A,2)

      do k = 1, m
         w(k) = dot_product(a(:,k), v(:))
      end do

   end subroutine mv_ATv_5
   !===============================================================================


   !===============================================================================
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_ATv_6(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: n
      integer :: k

      n = size(A,1)

      do k = 1, n
         w(:) = w(:) + a(k,:)*v(k)
      end do

   end subroutine mv_ATv_6
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_ATv_7(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: m, n
      integer :: k

      m = size(A,2)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !   do concurrent (k = 1: m) shared(m,a,v) ! check shared variables
      !      w(k) = dot_product(a(:,k), v(:))
      !   end do

   end subroutine mv_ATv_7
   !===============================================================================


   !===============================================================================
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! GFORTRAN DOESNT SUPPORT SHARED !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> author: Seyed Ali Ghasemi
   pure subroutine mv_ATv_8(A, v, w)
      real(rk), intent(in), contiguous :: A(:,:), v(:)
      real(rk), intent(inout), contiguous :: w(:)
      integer :: m, n
      integer :: i, j

      m = size(A,2)
      n = size(A,1)

      error stop 'ForMatMul: shared is not supported in gfortran'

      !   do concurrent(i=1:m) shared(m, n, a, v) ! check shared variables
      !      do j=1,n
      !         w(i) = w(i) + a(j,i)*v(j)
      !      end do
      !   end do

   end subroutine mv_ATv_8
   !===============================================================================

end module formatmul_opts







 !> author: Seyed Ali Ghasemi
impure subroutine impure_mm_AB_12(a, b, c)
   use kinds
   implicit none
   real(rk), intent(in) :: a(:,:), b(:,:)
   real(rk), intent(inout) :: c(:,:)
   integer :: m, p
   integer :: i, k

   m = size(A,1)
   p = size(B,2)

   !$OMP PARALLEL DO PRIVATE(i, k)
   do i = 1, p
      do k = 1, m
         c(k,i) = dot_product(a(k,:), b(:,i))
      end do
   end do
   !$OMP END PARALLEL DO
end subroutine impure_mm_AB_12

 !> author: Seyed Ali Ghasemi
impure subroutine impure_mm_AB_13(a, b, c)
   use kinds
   implicit none
   real(rk), intent(in) :: a(:,:), b(:,:)
   real(rk), intent(inout) :: c(:,:)
   integer :: m, p
   integer :: i, k

   m = size(A,1)
   p = size(B,2)

   !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(i, k) SHARED(m, p, a, b, c)
   do i = 1, p
      do k = 1, m
         c(k,i) = dot_product(a(k,:), b(:,i))
      end do
   end do
   !$OMP END PARALLEL DO
end subroutine impure_mm_AB_13



 !> author: Seyed Ali Ghasemi
impure subroutine impure_mm_ATB_12(a, b, c)
   use kinds
   implicit none
   real(rk), intent(in) :: a(:,:), b(:,:)
   real(rk), intent(inout) :: c(:,:)
   integer :: m, p
   integer :: i, k

   m = size(A,2)
   p = size(B,2)

   !$OMP PARALLEL DO PRIVATE(i, k)
   do i = 1, p
      do k = 1, m
         c(k,i) = dot_product(a(:,k), b(:,i))
      end do
   end do
   !$OMP END PARALLEL DO
end subroutine impure_mm_ATB_12

 !> author: Seyed Ali Ghasemi
impure subroutine impure_mm_ATB_13(a, b, c)
   use kinds
   implicit none
   real(rk), intent(in) :: a(:,:), b(:,:)
   real(rk), intent(inout) :: c(:,:)
   integer :: m, p
   integer :: i, k

   m = size(A,2)
   p = size(B,2)

   !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(i, k) SHARED(m, p, a, b, c)
   do i = 1, p
      do k = 1, m
         c(k,i) = dot_product(a(:,k), b(:,i))
      end do
   end do
   !$OMP END PARALLEL DO
end subroutine impure_mm_ATB_13



 !> author: Seyed Ali Ghasemi
impure subroutine impure_mm_ABT_12(a, b, c)
   use kinds
   implicit none
   real(rk), intent(in) :: a(:,:), b(:,:)
   real(rk), intent(inout) :: c(:,:)
   integer :: m, p
   integer :: i, k

   m = size(A,1)
   p = size(B,1)

   !$OMP PARALLEL DO PRIVATE(i, k)
   do i = 1, p
      do k = 1, m
         c(k,i) = dot_product(a(k,:), b(i,:))
      end do
   end do
   !$OMP END PARALLEL DO
end subroutine impure_mm_ABT_12

 !> author: Seyed Ali Ghasemi
impure subroutine impure_mm_ABT_13(a, b, c)
   use kinds
   implicit none
   real(rk), intent(in) :: a(:,:), b(:,:)
   real(rk), intent(inout) :: c(:,:)
   integer :: m, p
   integer :: i, k

   m = size(A,1)
   p = size(B,1)

   !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(i, k) SHARED(m, p, a, b, c)
   do i = 1, p
      do k = 1, m
         c(k,i) = dot_product(a(k,:), b(i,:))
      end do
   end do
   !$OMP END PARALLEL DO
end subroutine impure_mm_ABT_13



 !> author: Seyed Ali Ghasemi
impure subroutine impure_mm_ATBT_12(a, b, c)
   use kinds
   implicit none
   real(rk), intent(in) :: a(:,:), b(:,:)
   real(rk), intent(inout) :: c(:,:)
   integer :: m, p
   integer :: i, k

   m = size(A,2)
   p = size(B,1)

   !$OMP PARALLEL DO PRIVATE(i, k)
   do i = 1, p
      do k = 1, m
         c(k,i) = dot_product(a(:,k), b(i,:))
      end do
   end do
   !$OMP END PARALLEL DO
end subroutine impure_mm_ATBT_12

 !> author: Seyed Ali Ghasemi
impure subroutine impure_mm_ATBT_13(a, b, c)
   use kinds
   implicit none
   real(rk), intent(in) :: a(:,:), b(:,:)
   real(rk), intent(inout) :: c(:,:)
   integer :: m, p
   integer :: i, k

   m = size(A,2)
   p = size(B,1)

   !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(i, k) SHARED(m, p, a, b, c)
   do i = 1, p
      do k = 1, m
         c(k,i) = dot_product(a(:,k), b(i,:))
      end do
   end do
   !$OMP END PARALLEL DO
end subroutine impure_mm_ATBT_13
