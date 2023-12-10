program mat_vec

   use kinds,     only: rk
   use fortime,   only: timer
   use formatmul, only: matmul

   implicit none

   real(rk), allocatable :: A(:,:), v(:)
   real(rk), allocatable :: w(:), w_co(:)
   type(timer)           :: t
   integer               :: m, n

   ! w(m) = A(m,n).v(n)
   m = 200
   n = 100

   allocate(A(m,n),v(n))
   call random_number(A)
   call random_number(v)

   w = matmul(A,v)

#if defined(USE_COARRAY)
   sync all

   w_co = matmul(A,v,'coarray')

   sync all

   if (this_image() == 1) print*,'test2: relative error:', norm2(w-w_co)/norm2(w)
#endif

end program mat_vec

