program test_matmul13

   use kinds
   use formatmul
   use forunittest, only: unit_test

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

   ! C(m,o) = A(n,m).B(o,n)
   m = 500
   n = 400
   o = 300

   allocate(A(n,m),B(o,n))
   call random_number(A)
   call random_number(B)
   A = A*100.0_rk
   B = B*100.0_rk

   C_ref = matmul(transpose(A),transpose(B))

   C = matmul(A,B, transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.1')

   C = matmul(A,B, option='m1', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.2')

   C = matmul(A,B, option='m2', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.3')

   C = matmul(A,B, option='m3', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.4')

   C = matmul(A,B, option='m4', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.5')

   C = matmul(A,B, option='m5', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.6')

   C = matmul(A,B, option='m6', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.7')

   C = matmul(A,B, option='m7', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.8')

   C = matmul(A,B, option='m8', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.9')

   C = matmul(A,B, option='m9', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.10')

   ! C = matmul(A,B, option='m10', transA=.true., transB=.true., coarray=.true.)
   ! if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.11')

   ! C = matmul(A,B, option='m11', transA=.true., transB=.true., coarray=.true.)
   ! if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.12')

   C = matmul(A,B, option='m12', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.13')

   C = matmul(A,B, option='m13', transA=.true., transB=.true., coarray=.true.)
   if (im==1) call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul13.14')

end program test_matmul13

