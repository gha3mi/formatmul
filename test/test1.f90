program test_matmul3

   use kinds
   use formatmul
   use forunittest

   implicit none

   real(rk), allocatable :: A(:,:), B(:,:)
   real(rk), allocatable :: C_ref(:,:), C(:,:)
   integer               :: m, n, o
   type(unit_test)       :: ut

   ! C(m,o) = A(m,n).B(n,o)
   m = 300
   n = 200
   o = 100

   allocate(A(m,n),B(n,o))
   call random_number(A)
   call random_number(B)
   A = A*100.0_rk
   B = B*100.0_rk

   C_ref = matmul(A,B)

   C = matmul(A,B)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.1')

   C = matmul(A,B, option='m1')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.2')

   C = matmul(A,B, option='m2')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.3')

   C = matmul(A,B, option='m3')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.4')

   C = matmul(A,B, option='m4')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.5')

   C = matmul(A,B, option='m5')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.6')

   C = matmul(A,B, option='m6')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.7')

   C = matmul(A,B, option='m7')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.8')

   C = matmul(A,B, option='m8')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.9')

   C = matmul(A,B, option='m9')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.10')

   ! C = matmul(A,B, option='m10')
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.11')

   ! C = matmul(A,B, option='m11')
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.12')

   C = matmul(A,B, option='m12')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.13')

   C = matmul(A,B, option='m13')
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.14')


   C = matmul(A,B, nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.15')

   C = matmul(A,B, option='m1', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.16')

   C = matmul(A,B, option='m2', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.17')

   C = matmul(A,B, option='m3', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.18')

   C = matmul(A,B, option='m4', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.19')

   C = matmul(A,B, option='m5', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.20')

   C = matmul(A,B, option='m6', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.21')

   C = matmul(A,B, option='m7', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.22')

   C = matmul(A,B, option='m8', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.23')

   C = matmul(A,B, option='m9', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.24')

   ! C = matmul(A,B, option='m10', nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.25')

   ! C = matmul(A,B, option='m11', nblock=4)
   ! call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.26')

   C = matmul(A,B, option='m12', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.27')

   C = matmul(A,B, option='m13', nblock=4)
   call ut%check(C, C_ref, tol=1e-5_rk, msg='test_matmul3.28')

end program test_matmul3

