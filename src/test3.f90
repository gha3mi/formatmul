program test_matmul5

   use kinds
   use formatmul
   use forunittest, only: unit_test

   implicit none

   real(rk), allocatable :: A(:,:), B(:,:)
   real(rk), allocatable :: C_ref(:,:), C(:,:)
   integer               :: m, n, o
   type(unit_test)       :: ut

   ! C(m,o) = A(m,n).B(o,n)
   m = 300
   n = 200
   o = 100

   allocate(A(m,n),B(o,n))
   call random_number(A)
   call random_number(B)
   A = A*100.0_rk
   B = B*100.0_rk

   C_ref = matmul(A,transpose(B))

   C = matmul(A,B, transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.1')

   C = matmul(A,B, option='m1', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.2')

   C = matmul(A,B, option='m2', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.3')

   C = matmul(A,B, option='m3', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.4')

   C = matmul(A,B, option='m4', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.5')

   C = matmul(A,B, option='m5', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.6')

   C = matmul(A,B, option='m6', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.7')

   C = matmul(A,B, option='m7', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.8')

   C = matmul(A,B, option='m8', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.9')

   C = matmul(A,B, option='m9', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.10')

   ! C = matmul(A,B, option='m10', transB=.true.)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.11')

   ! C = matmul(A,B, option='m11', transB=.true.)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.12')

   C = matmul(A,B, option='m12', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.13')

   C = matmul(A,B, option='m13', transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.14')




   C = matmul(A,B, transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.15')

   C = matmul(A,B, option='m1', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.16')

   C = matmul(A,B, option='m2', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.17')

   C = matmul(A,B, option='m3', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.18')

   C = matmul(A,B, option='m4', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.19')

   C = matmul(A,B, option='m5', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.20')

   C = matmul(A,B, option='m6', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.21')

   C = matmul(A,B, option='m7', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.22')

   C = matmul(A,B, option='m8', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.23')

   C = matmul(A,B, option='m9', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.24')

   ! C = matmul(A,B, option='m10', transB=.true., nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.25')

   ! C = matmul(A,B, option='m11', transB=.true., nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.26')

   C = matmul(A,B, option='m12', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.27')

   C = matmul(A,B, option='m13', transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.28')




   C = matmul(A,B, transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.29')

   C = matmul(A,B, option='m1', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.30')

   C = matmul(A,B, option='m2', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.31')

   C = matmul(A,B, option='m3', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.32')

   C = matmul(A,B, option='m4', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.33')

   C = matmul(A,B, option='m5', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.34')

   C = matmul(A,B, option='m6', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.35')

   C = matmul(A,B, option='m7', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.36')

   C = matmul(A,B, option='m8', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.37')

   C = matmul(A,B, option='m9', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.38')

   ! C = matmul(A,B, option='m10', transA=.false., transB=.true.)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.39')

   ! C = matmul(A,B, option='m11', transA=.false., transB=.true.)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.40')

   C = matmul(A,B, option='m12', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.41')

   C = matmul(A,B, option='m13', transA=.false., transB=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.42')




   C = matmul(A,B, transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.43')

   C = matmul(A,B, option='m1', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.44')

   C = matmul(A,B, option='m2', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.45')

   C = matmul(A,B, option='m3', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.46')

   C = matmul(A,B, option='m4', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.47')

   C = matmul(A,B, option='m5', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.48')

   C = matmul(A,B, option='m6', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.49')

   C = matmul(A,B, option='m7', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.50')

   C = matmul(A,B, option='m8', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.51')

   C = matmul(A,B, option='m9', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.52')

   ! C = matmul(A,B, option='m10', transA=.false., transB=.true., nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.53')

   ! C = matmul(A,B, option='m11', transA=.false., transB=.true., nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.54')

   C = matmul(A,B, option='m12', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.55')

   C = matmul(A,B, option='m13', transA=.false., transB=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul5.56')

end program test_matmul5

