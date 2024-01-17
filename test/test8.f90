program test_matmul10

   use kinds
   use formatmul
   use forunittest

   implicit none

   real(rk), allocatable :: A(:,:), B(:,:)
   real(rk), allocatable :: C_ref(:,:), C(:,:)
   integer               :: m, n, o, im
   type(unit_test)       :: ut

#if defined(USE_COARRAY)
   im = this_image()
#else
   im = 1
#endif

   ! C(m,o) = A(m,n).B(n,o)
   m = 500
   n = 400
   o = 300

   allocate(A(m,n),B(n,o))
   call random_number(A)
   call random_number(B)
   A = A*100.0_rk
   B = B*100.0_rk

   C_ref = matmul(A,B)

   C = matmul(A,B, coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.1')

   C = matmul(A,B, option='m1', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.2')

   C = matmul(A,B, option='m2', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.3')

   C = matmul(A,B, option='m3', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.4')

   C = matmul(A,B, option='m4', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.5')

   C = matmul(A,B, option='m5', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.6')

   C = matmul(A,B, option='m6', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.7')

   C = matmul(A,B, option='m7', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.8')

   C = matmul(A,B, option='m8', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.9')

   C = matmul(A,B, option='m9', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.10')

!    C = matmul(A,B, option='m10', coarray=.true.)
!    if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.11')

!    C = matmul(A,B, option='m11', coarray=.true.)
!    if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.12')

   C = matmul(A,B, option='m12', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.13')

   C = matmul(A,B, option='m13', coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul10.14')

end program test_matmul10

