program example5

   use kinds,     only: rk
   use fortime,   only: timer
   use formatmul, only: matmul_blas

   implicit none

   real(rk), allocatable :: A(:,:), B(:,:)
   real(rk), allocatable :: C(:,:)
   type(timer)           :: t
   integer               :: m, n, o

   ! C(m,o) = A(m,n).B(n,o)
   m = 64*2*100
   n = 16*100
   o = 64*10

   allocate(A(m,n),B(n,o))
   call random_number(A)
   call random_number(B)

   sync all

   if (this_image() == 1) call t%timer_start()
      C = matmul_blas(A,B)
   if (this_image() == 1) call t%timer_stop(message=' Elapsed time (example5: mat_mat with blas):')

end program example5

