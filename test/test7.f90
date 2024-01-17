program test_matmul9

   use kinds
   use formatmul
   use forunittest

   implicit none

   real(rk), allocatable :: A(:,:), v(:)
   real(rk), allocatable :: w_ref(:), w(:)
   integer               :: m, n
   type(unit_test)       :: ut

   ! w(n) = v(m).A(m,n)
   m = 100
   n = 300

   allocate(A(m,n),v(m))
   call random_number(A)
   call random_number(v)
   A = A*100.0_rk
   v = v*100.0_rk


   w_ref = matmul(v,A)

   w = matmul(A,v, transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.1')

   w = matmul(A,v, option='m1', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.2')

   w = matmul(A,v, option='m2', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.3')

   w = matmul(A,v, option='m3', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.4')

   w = matmul(A,v, option='m4', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.5')

   w = matmul(A,v, option='m5', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.6')

   w = matmul(A,v, option='m6', transA=.true.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.7')

   ! w = matmul(A,v, option='m7', transA=.true.)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.8')

   ! w = matmul(A,v, option='m8', transA=.true.)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.9')




   w = matmul(A,v, transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.10')

   w = matmul(A,v, option='m1', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.11')

   w = matmul(A,v, option='m2', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.12')

   w = matmul(A,v, option='m3', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.13')

   w = matmul(A,v, option='m4', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.14')

   w = matmul(A,v, option='m5', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.15')

   w = matmul(A,v, option='m6', transA=.true., nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.16')

   ! w = matmul(A,v, option='m7', transA=.true., nblock=4)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.17')

   ! w = matmul(A,v, option='m8', transA=.true., nblock=4)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul9.18')

end program test_matmul9

