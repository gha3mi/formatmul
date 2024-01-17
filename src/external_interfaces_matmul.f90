module external_interfaces

   use kinds

   implicit none

   interface gemm
#if defined(REAL64)
      pure subroutine dgemm(f_transa, f_transb, f_m, f_n, f_k, f_alpha, f_a, f_lda, f_b, f_ldb, f_beta, f_c, f_ldc)
         import rk
         integer,   intent(in)    :: f_ldc
         integer,   intent(in)    :: f_ldb
         integer,   intent(in)    :: f_lda
         character, intent(in)    :: f_transa
         character, intent(in)    :: f_transb
         integer,   intent(in)    :: f_m
         integer,   intent(in)    :: f_n
         integer,   intent(in)    :: f_k
         real(rk),  intent(in)    :: f_alpha
         real(rk),  intent(in)    :: f_a(f_lda, *)
         real(rk),  intent(in)    :: f_b(f_ldb, *)
         real(rk),  intent(in)    :: f_beta
         real(rk),  intent(inout) :: f_c(f_ldc, *)
      end subroutine dgemm
#elif defined(REAL32)
      pure subroutine sgemm(f_transa, f_transb, f_m, f_n, f_k, f_alpha, f_a, f_lda, f_b, f_ldb, f_beta, f_c, f_ldc)
         import rk
         integer,   intent(in)    :: f_ldc
         integer,   intent(in)    :: f_ldb
         integer,   intent(in)    :: f_lda
         character, intent(in)    :: f_transa
         character, intent(in)    :: f_transb
         integer,   intent(in)    :: f_m
         integer,   intent(in)    :: f_n
         integer,   intent(in)    :: f_k
         real(rk),  intent(in)    :: f_alpha
         real(rk),  intent(in)    :: f_a(f_lda, *)
         real(rk),  intent(in)    :: f_b(f_ldb, *)
         real(rk),  intent(in)    :: f_beta
         real(rk),  intent(inout) :: f_c(f_ldc, *)
      end subroutine sgemm
#else
      pure subroutine dgemm(f_transa, f_transb, f_m, f_n, f_k, f_alpha, f_a, f_lda, f_b, f_ldb, f_beta, f_c, f_ldc)
         import rk
         integer,   intent(in)    :: f_ldc
         integer,   intent(in)    :: f_ldb
         integer,   intent(in)    :: f_lda
         character, intent(in)    :: f_transa
         character, intent(in)    :: f_transb
         integer,   intent(in)    :: f_m
         integer,   intent(in)    :: f_n
         integer,   intent(in)    :: f_k
         real(rk),  intent(in)    :: f_alpha
         real(rk),  intent(in)    :: f_a(f_lda, *)
         real(rk),  intent(in)    :: f_b(f_ldb, *)
         real(rk),  intent(in)    :: f_beta
         real(rk),  intent(inout) :: f_c(f_ldc, *)
      end subroutine dgemm
#endif
   end interface

   interface gemv
#if defined(REAL64)
      pure subroutine dgemv(f_trans, f_m, f_n, f_alpha, f_a, f_lda, f_x, f_incx, f_beta, f_y, f_incy)
         import rk
         integer,   intent(in)    :: f_m
         integer,   intent(in)    :: f_n
         integer,   intent(in)    :: f_lda
         character, intent(in)    :: f_trans
         real(rk),  intent(in)    :: f_alpha
         real(rk),  intent(in)    :: f_a(f_lda, *)
         real(rk),  intent(in)    :: f_x(*)
         integer,   intent(in)    :: f_incx
         real(rk),  intent(in)    :: f_beta
         real(rk),  intent(inout) :: f_y(*)
         integer,   intent(in)    :: f_incy
      end subroutine dgemv
#elif defined(REAL32)
      pure subroutine sgemv(f_trans, f_m, f_n, f_alpha, f_a, f_lda, f_x, f_incx, f_beta, f_y, f_incy)
         import rk
         integer,   intent(in)    :: f_m
         integer,   intent(in)    :: f_n
         integer,   intent(in)    :: f_lda
         character, intent(in)    :: f_trans
         real(rk),  intent(in)    :: f_alpha
         real(rk),  intent(in)    :: f_a(f_lda, *)
         real(rk),  intent(in)    :: f_x(*)
         integer,   intent(in)    :: f_incx
         real(rk),  intent(in)    :: f_beta
         real(rk),  intent(inout) :: f_y(*)
         integer,   intent(in)    :: f_incy
      end subroutine sgemv
#else
      pure subroutine dgemv(f_trans, f_m, f_n, f_alpha, f_a, f_lda, f_x, f_incx, f_beta, f_y, f_incy)
         import rk
         integer,   intent(in)    :: f_m
         integer,   intent(in)    :: f_n
         integer,   intent(in)    :: f_lda
         character, intent(in)    :: f_trans
         real(rk),  intent(in)    :: f_alpha
         real(rk),  intent(in)    :: f_a(f_lda, *)
         real(rk),  intent(in)    :: f_x(*)
         integer,   intent(in)    :: f_incx
         real(rk),  intent(in)    :: f_beta
         real(rk),  intent(inout) :: f_y(*)
         integer,   intent(in)    :: f_incy
      end subroutine dgemv
#endif
   end interface

end module external_interfaces
