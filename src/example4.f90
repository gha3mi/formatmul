program example4

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

#if defined(USE_COARRAY)
   sync all

   call t%timer_start()
      w = matmul(A,v,'coarray')
   call t%timer_stop(message=' Elapsed time (example4: mat_vec, coarray):')
#else
   call t%timer_start()
      w = matmul(A,v)
   call t%timer_stop(message=' Elapsed time (example4: mat_vec):')
#endif

end program example4

