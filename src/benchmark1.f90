program benchmark1

   use kinds,     only: rk
   use fortime,   only: timer
   use formatmul, only: matmul, matmul_blas

   implicit none

   real(rk), allocatable :: A(:,:), B(:,:)
   real(rk), allocatable :: C(:,:)
   type(timer)           :: t
   integer               :: m, n, o, i ,l
   character(2) :: im

   ! C(m,o) = A(m,n).B(n,o)
   m = 3000
   n = 2000
   o = 1000

   l = 5

   allocate(A(m,n),B(n,o))
   call random_number(A)
   call random_number(B)

   sync all

   if (this_image() == 1) call t%timer_start()
   do i = 1,l
      C = matmul(A,B)
   end do
   if (this_image() == 1) call t%timer_stop(message=' Elapsed time (matmul):',nloops=l)

   sync all

   if (this_image() == 1) call t%timer_start()
   do i = 1,l
      C = matmul_blas(A,B)
   end do
   if (this_image() == 1) call t%timer_stop(message=' Elapsed time (dgemm):',nloops=l)

   sync all

   call t%timer_start()
   do i = 1,l
      C = matmul(A,B,'coarray','matmul')
   end do
   write (im, "(I2)") this_image()
   call t%timer_stop(message=' Elapsed time (coarray with matmul) image='//trim(im)//':',nloops=l)

   sync all

   call t%timer_start()
   do i = 1,l
      C = matmul(A,B,'coarray','blas')
   end do
   call t%timer_stop(message=' Elapsed time (coarray with dgemm) image='//trim(im)//':',nloops=l)

end program benchmark1
