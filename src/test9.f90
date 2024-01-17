program test_matmul11

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

   ! C(m,o) = A(n,m).B(n,o)
   m = 500
   n = 400
   o = 300

   allocate(A(n,m),B(n,o))
   call random_number(A)
   call random_number(B)
   A = A*100.0_rk
   B = B*100.0_rk

   C_ref = matmul(transpose(A),B)

   C = matmul(A,B, transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.1')

   C = matmul(A,B, option='m1', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.2')

   C = matmul(A,B, option='m2', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.3')

   C = matmul(A,B, option='m3', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.4')

   C = matmul(A,B, option='m4', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.5')

   C = matmul(A,B, option='m5', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.6')

   C = matmul(A,B, option='m6', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.7')

   C = matmul(A,B, option='m7', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.8')

   C = matmul(A,B, option='m8', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.9')

   C = matmul(A,B, option='m9', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.10')

   ! C = matmul(A,B, option='m10', transA=.true., coarray=.true.)
   ! if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.11')

   ! C = matmul(A,B, option='m11', transA=.true., coarray=.true.)
   ! if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.12')

   C = matmul(A,B, option='m12', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.13')

   C = matmul(A,B, option='m13', transA=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul11.14')

end program test_matmul11

