program mat_mat

   use kinds,     only: rk
   use fortime,   only: timer
   use formatmul, only: matmul

   implicit none

   real(rk), allocatable :: A(:,:), B(:,:)
   real(rk), allocatable :: C(:,:), C_co(:,:)
   type(timer)           :: t
   integer               :: m, n, o

   ! C(m,o) = A(m,n).B(n,o)
   m = 200
   n = 100
   o = 10

   allocate(A(m,n),B(n,o))
   call random_number(A)
   call random_number(B)

#if defined(USE_COARRAY)
   sync all
#endif

   C = matmul(A,B)

#if defined(USE_COARRAY)
   sync all

   C_co = matmul(A,B,'coarray')

   sync all

   if (this_image() == 1) print*,'test1: relative error:', norm2(C-C_co)/norm2(C)
#endif

end program mat_mat

