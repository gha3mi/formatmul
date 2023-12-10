program example8

   use kinds,     only: rk
   use fortime,   only: timer
   use formatmul, only: matmul

   implicit none

   real(rk), allocatable :: A(:,:), v(:)
   real(rk), allocatable :: w(:)
   type(timer)           :: t
   integer               :: m, n

   ! w(m) = A(m,n).v(n)
   m = 64*2*100
   n = 16*2*100

   allocate(A(m,n),v(n))
   call random_number(A)
   call random_number(v)

#if defined(COARRAY)
   sync all

   call t%timer_start()
      w = matmul(A,v,'coarray',option='m2')
   call t%timer_stop(message=' Elapsed time (example8: mat_vec, coarray with blas):')
#else
   call t%timer_start()
      w = matmul(A,v,'default',option='m2')
   call t%timer_stop(message=' Elapsed time (example8: mat_vec, blas):')
#endif

end program example8

