program benchmark2

   use kinds,     only: rk
   use fortime,   only: timer
   use formatmul, only: matmul
   use formatmul_opts, only: matmul_blas

   implicit none

   real(rk), allocatable :: A(:,:), v(:)
   real(rk), allocatable :: w(:)
   type(timer)           :: t
   integer               :: m, n, i ,l
   character(2) :: im

   ! w(m) = A(m,n).v(n)
   m = 64*2*100
   n = 16*2*100

   l = 5

   allocate(A(m,n),v(n))
   call random_number(A)
   call random_number(v)

#if defined(USE_COARRAY)
   sync all

   if (this_image() == 1) call t%timer_start()
   do i = 1,l
      w = matmul(A,v)
   end do
   if (this_image() == 1) call t%timer_stop(message=' Elapsed time (matmul):',nloops=l)
#else
   call t%timer_start()
   do i = 1,l
      w = matmul(A,v)
   end do
   call t%timer_stop(message=' Elapsed time (matmul):',nloops=l)
#endif

#if defined(USE_COARRAY)
   sync all

   if (this_image() == 1) call t%timer_start()
   do i = 1,l
      w = matmul_blas(A,v)
   end do
   if (this_image() == 1) call t%timer_stop(message=' Elapsed time (dgemv):',nloops=l)
#else
   call t%timer_start()
   do i = 1,l
      w = matmul_blas(A,v)
   end do
   call t%timer_stop(message=' Elapsed time (dgemv):',nloops=l)
#endif

#if defined(USE_COARRAY)
   sync all

   call t%timer_start()
   do i = 1,l
      w = matmul(A,v,'coarray','m1')
   end do
   write (im, "(I2)") this_image()
   call t%timer_stop(message=' Elapsed time (coarray with matmul) image='//trim(im)//':',nloops=l)
#else
   call t%timer_start()
   do i = 1,l
      w = matmul(A,v,'default','m1')
   end do
   call t%timer_stop(message=' Elapsed time (default with matmul) image='//trim(im)//':',nloops=l)
#endif

#if defined(USE_COARRAY)
   sync all

   call t%timer_start()
   do i = 1,l
      w = matmul(A,v,'coarray','m2')
   end do
   write (im, "(I2)") this_image()
   call t%timer_stop(message=' Elapsed time (coarray with dgemv) image='//trim(im)//':',nloops=l)
#else
   call t%timer_start()
   do i = 1,l
      w = matmul(A,v,'default','m2')
   end do
   call t%timer_stop(message=' Elapsed time (default with dgemv) image='//trim(im)//':',nloops=l)
#endif

end program benchmark2
