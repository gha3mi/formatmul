program test_matmul8

   use kinds
   use formatmul
   use forunittest, only: unit_test

   implicit none

   real(rk), allocatable :: A(:,:), v(:)
   real(rk), allocatable :: w_ref(:), w(:)
   integer               :: m, n
   type(unit_test)       :: ut

   ! w(m) = A(n,m).v(n)
   m = 100
   n = 300

   allocate(A(n,m),v(n))
   call random_number(A)
   call random_number(v)
   A = A*100.0_rk
   v = v*100.0_rk


   w_ref = matmul(transpose(A),v)

   w = matmul(A,v, transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.1')

   w = matmul(A,v, option='m1', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.2')

   w = matmul(A,v, option='m2', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.3')

   w = matmul(A,v, option='m3', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.4')

   w = matmul(A,v, option='m4', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.5')

   w = matmul(A,v, option='m5', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.6')

   w = matmul(A,v, option='m6', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.7')

   ! w = matmul(A,v, option='m7', transA=.true.)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.8')

   ! w = matmul(A,v, option='m8', transA=.true.)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.9')




   w = matmul(A,v, transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.10')

   w = matmul(A,v, option='m1', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.11')

   w = matmul(A,v, option='m2', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.12')

   w = matmul(A,v, option='m3', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.13')

   w = matmul(A,v, option='m4', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.14')

   w = matmul(A,v, option='m5', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.15')

   w = matmul(A,v, option='m6', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.16')

   ! w = matmul(A,v, option='m7', transA=.true., nblock=4)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.17')

   ! w = matmul(A,v, option='m8', transA=.true., nblock=4)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul8.18')

end program test_matmul8

