program test_matmul15

   use kinds
   use formatmul
   use forunittest

   implicit none

   real(rk), allocatable :: A(:,:), v(:)
   real(rk), allocatable :: w_ref(:), w(:)
   integer               :: m, n, im
   type(unit_test)       :: ut

#if defined(USE_COARRAY)
   im = this_image()
#else
   im = 1
#endif

   ! w(m) = A(n,m).v(n)
   m = 100
   n = 300

   allocate(A(n,m),v(n))
   call random_number(A)
   call random_number(v)
   A = A*100.0_rk
   v = v*100.0_rk


   w_ref = matmul(transpose(A),v)

   w = matmul(A,v, transA=.true., coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul15.1')

   w = matmul(A,v, option='m1', transA=.true., coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul15.2')

   w = matmul(A,v, option='m2', transA=.true., coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul15.3')

   w = matmul(A,v, option='m3', transA=.true., coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul15.4')

   w = matmul(A,v, option='m4', transA=.true., coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul15.5')

   w = matmul(A,v, option='m5', transA=.true., coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul15.6')

   w = matmul(A,v, option='m6', transA=.true., coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul15.7')

   ! w = matmul(A,v, option='m7', transA=.true., coarray=.true.)
   ! if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul15.8')

   ! w = matmul(A,v, option='m8', transA=.true., coarray=.true.)
   ! if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul15.9')

end program test_matmul15

