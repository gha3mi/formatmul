program test_matmul7

   use kinds
   use formatmul
   use forunittest

   implicit none

   real(rk), allocatable :: A(:,:), v(:)
   real(rk), allocatable :: w_ref(:), w(:)
   integer               :: m, n
   type(unit_test)       :: ut

   ! w(m) = A(m,n).v(n)
   m = 100
   n = 300

   allocate(A(m,n),v(n))
   call random_number(A)
   call random_number(v)
   A = A*100.0_rk
   v = v*100.0_rk


   w_ref = matmul(A,v)

   w = matmul(A,v)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.1')

   w = matmul(A,v, option='m1')
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.2')

   w = matmul(A,v, option='m2')
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.3')

   w = matmul(A,v, option='m3')
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.4')

   w = matmul(A,v, option='m4')
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.5')

   w = matmul(A,v, option='m5')
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.6')

   w = matmul(A,v, option='m6')
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.7')

   ! w = matmul(A,v, option='m7')
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.8')

   ! w = matmul(A,v, option='m8')
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.9')




   w = matmul(A,v, nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.10')

   w = matmul(A,v, option='m1', nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.11')

   w = matmul(A,v, option='m2', nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.12')

   w = matmul(A,v, option='m3', nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.13')

   w = matmul(A,v, option='m4', nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.14')

   w = matmul(A,v, option='m5', nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.15')

   w = matmul(A,v, option='m6', nblock=4)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.16')

   ! w = matmul(A,v, option='m7', nblock=4)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.17')

   ! w = matmul(A,v, option='m8', nblock=4)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.18')



   w = matmul(A,v, option='m1', transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.19')

   w = matmul(A,v, option='m2', transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.20')

   w = matmul(A,v, option='m3', transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.21')

   w = matmul(A,v, option='m4', transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.22')

   w = matmul(A,v, option='m5', transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.23')

   w = matmul(A,v, option='m6', transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.24')

   ! w = matmul(A,v, option='m7', transA=.false.)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.25')

   ! w = matmul(A,v, option='m8', transA=.false.)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.26')




   w = matmul(A,v, nblock=4, transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.27')

   w = matmul(A,v, option='m1', nblock=4, transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.28')

   w = matmul(A,v, option='m2', nblock=4, transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.29')

   w = matmul(A,v, option='m3', nblock=4, transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.30')

   w = matmul(A,v, option='m4', nblock=4, transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.31')

   w = matmul(A,v, option='m5', nblock=4, transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.32')

   w = matmul(A,v, option='m6', nblock=4, transA=.false.)
   call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.33')

   ! w = matmul(A,v, option='m7', nblock=4, transA=.false.)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.34')

   ! w = matmul(A,v, option='m8', nblock=4, transA=.false.)
   ! call ut%check(w, w_ref, tol=1e-5_rk, msg='test_matmul7.35')

end program test_matmul7

