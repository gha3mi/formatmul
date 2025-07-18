program test_matmul4

   use kinds
   use formatmul
   use forunittest, only: unit_test

   implicit none

   real(rk), allocatable :: A(:,:), B(:,:)
   real(rk), allocatable :: C_ref(:,:), C(:,:)
   integer               :: m, n, o
   type(unit_test)       :: ut

   ! C(m,o) = A(n,m).B(n,o)
   m = 300
   n = 200
   o = 100

   allocate(A(n,m),B(n,o))
   call random_number(A)
   call random_number(B)
   A = A*100.0_rk
   B = B*100.0_rk

   C_ref = matmul(transpose(A),B)

   C = matmul(A,B, transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.1')

   C = matmul(A,B, option='m1', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.2')

   C = matmul(A,B, option='m2', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.3')

   C = matmul(A,B, option='m3', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.4')

   C = matmul(A,B, option='m4', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.5')

   C = matmul(A,B, option='m5', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.6')

   C = matmul(A,B, option='m6', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.7')

   C = matmul(A,B, option='m7', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.8')

   C = matmul(A,B, option='m8', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.9')

   C = matmul(A,B, option='m9', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.10')

   ! C = matmul(A,B, option='m10', transA=.true.)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.11')

   ! C = matmul(A,B, option='m11', transA=.true.)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.12')

   C = matmul(A,B, option='m12', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.13')

   C = matmul(A,B, option='m13', transA=.true.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.14')





   C = matmul(A,B, transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.15')

   C = matmul(A,B, option='m1', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.16')

   C = matmul(A,B, option='m2', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.17')

   C = matmul(A,B, option='m3', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.18')

   C = matmul(A,B, option='m4', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.19')

   C = matmul(A,B, option='m5', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.20')

   C = matmul(A,B, option='m6', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.21')

   C = matmul(A,B, option='m7', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.22')

   C = matmul(A,B, option='m8', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.23')

   C = matmul(A,B, option='m9', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.24')

   ! C = matmul(A,B, option='m10', transA=.true., nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.25')

   ! C = matmul(A,B, option='m11', transA=.true., nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.26')

   C = matmul(A,B, option='m12', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.27')

   C = matmul(A,B, option='m13', transA=.true., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.28')




   C = matmul(A,B, transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.29')

   C = matmul(A,B, option='m1', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.30')

   C = matmul(A,B, option='m2', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.31')

   C = matmul(A,B, option='m3', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.32')

   C = matmul(A,B, option='m4', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.33')

   C = matmul(A,B, option='m5', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.34')

   C = matmul(A,B, option='m6', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.35')

   C = matmul(A,B, option='m7', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.36')

   C = matmul(A,B, option='m8', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.37')

   C = matmul(A,B, option='m9', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.38')

   ! C = matmul(A,B, option='m10', transA=.true., transB=.false.)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.39')

   ! C = matmul(A,B, option='m11', transA=.true., transB=.false.)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.40')

   C = matmul(A,B, option='m12', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.41')

   C = matmul(A,B, option='m13', transA=.true., transB=.false.)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.42')





   C = matmul(A,B, transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.43')

   C = matmul(A,B, option='m1', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.44')

   C = matmul(A,B, option='m2', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.45')

   C = matmul(A,B, option='m3', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.46')

   C = matmul(A,B, option='m4', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.47')

   C = matmul(A,B, option='m5', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.48')

   C = matmul(A,B, option='m6', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.49')

   C = matmul(A,B, option='m7', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.50')

   C = matmul(A,B, option='m8', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.51')

   C = matmul(A,B, option='m9', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.52')

   ! C = matmul(A,B, option='m10', transA=.true., transB=.false., nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.53')

   ! C = matmul(A,B, option='m11', transA=.true., transB=.false., nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.54')

   C = matmul(A,B, option='m12', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.55')

   C = matmul(A,B, option='m13', transA=.true., transB=.false., nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul4.56')
   
end program test_matmul4

