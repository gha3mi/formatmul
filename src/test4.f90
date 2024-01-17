program test_matmul6

   use kinds
   use formatmul
   use forunittest

   implicit none

   real(rk), allocatable :: A(:,:), B(:,:)
   real(rk), allocatable :: C_ref(:,:), C(:,:)
   integer               :: m, n, o
   type(unit_test)       :: ut

   ! C(m,o) = A(n,m).B(o,n)
   m = 300
   n = 200
   o = 100

   allocate(A(n,m),B(o,n))
   call random_number(A)
   call random_number(B)
   A = A*100.0_rk
   B = B*100.0_rk

   C_ref = matmul(transpose(A),transpose(B))

   C = matmul(A,B, transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.1')

   C = matmul(A,B, option='m1', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.2')

   C = matmul(A,B, option='m2', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.3')

   C = matmul(A,B, option='m3', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.4')

   C = matmul(A,B, option='m4', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.5')

   C = matmul(A,B, option='m5', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.6')

   C = matmul(A,B, option='m6', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.7')

   C = matmul(A,B, option='m7', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.8')

   C = matmul(A,B, option='m8', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.9')

   C = matmul(A,B, option='m9', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.10')

   ! C = matmul(A,B, option='m10', transA=.true., transB=.true.)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.11')

   ! C = matmul(A,B, option='m11', transA=.true., transB=.true.)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.12')

   C = matmul(A,B, option='m12', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.13')

   C = matmul(A,B, option='m13', transA=.true., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.14')




   C = matmul(A,B, transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.15')

   C = matmul(A,B, option='m1', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.16')

   C = matmul(A,B, option='m2', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.17')

   C = matmul(A,B, option='m3', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.18')

   C = matmul(A,B, option='m4', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.19')

   C = matmul(A,B, option='m5', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.20')

   C = matmul(A,B, option='m6', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.21')

   C = matmul(A,B, option='m7', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.22')

   C = matmul(A,B, option='m8', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.23')

   C = matmul(A,B, option='m9', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.24')

   ! C = matmul(A,B, option='m10', transA=.true., transB=.true., nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.25')

   ! C = matmul(A,B, option='m11', transA=.true., transB=.true., nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.26')

   C = matmul(A,B, option='m12', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.27')

   C = matmul(A,B, option='m13', transA=.true., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul6.28')

end program test_matmul6

