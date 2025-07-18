program test_matmul14

   use kinds
   use formatmul
   use forunittest, only: unit_test

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

   ! w(m) = A(m,n).v(n)
   m = 500
   n = 300

   allocate(A(m,n),v(n))
   call random_number(A)
   call random_number(v)
   A = A*100.0_rk
   v = v*100.0_rk


   w_ref = matmul(A,v)

   w = matmul(A,v, coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul14.1')

   w = matmul(A,v, option='m1', coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul14.2')

   w = matmul(A,v, option='m2', coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul14.3')

   w = matmul(A,v, option='m3', coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul14.4')

   w = matmul(A,v, option='m4', coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul14.5')

   w = matmul(A,v, option='m5', coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul14.6')

   w = matmul(A,v, option='m6', coarray=.true.)
   if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul14.7')

   ! w = matmul(A,v, option='m7', coarray=.true.)
   ! if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul14.8')

   ! w = matmul(A,v, option='m8', coarray=.true.)
   ! if (im==1) call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul14.9')

end program test_matmul14

