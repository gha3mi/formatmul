program test_matmul12

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

   ! C(m,o) = A(m,n).B(o,n)
   m = 500
   n = 400
   o = 300

   allocate(A(m,n),B(o,n))
   call random_number(A)
   call random_number(B)
   A = A*100.0_rk
   B = B*100.0_rk

   C_ref = matmul(A,transpose(B))

   C = matmul(A,B, transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.1')

   C = matmul(A,B, option='m1', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.2')

   C = matmul(A,B, option='m2', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.3')

   C = matmul(A,B, option='m3', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.4')

   C = matmul(A,B, option='m4', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.5')

   C = matmul(A,B, option='m5', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.6')

   C = matmul(A,B, option='m6', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.7')

   C = matmul(A,B, option='m7', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.8')

   C = matmul(A,B, option='m8', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.9')

   C = matmul(A,B, option='m9', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.10')

   ! C = matmul(A,B, option='m10', transB=.true., coarray=.true.)
   ! if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.11')

   ! C = matmul(A,B, option='m11', transB=.true., coarray=.true.)
   ! if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.12')

   C = matmul(A,B, option='m12', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.13')

   C = matmul(A,B, option='m13', transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul12.14')

end program test_matmul12

