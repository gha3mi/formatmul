program benchmark3

   use kinds,     only: rk
   use fortime,   only: timer
   use formatmul, only: matmul

   implicit none

   real(rk), allocatable :: A(:,:), B(:,:)
   real(rk), allocatable :: C_ref(:,:), C(:,:)
   type(timer)           :: t
   integer               :: m, n, o, i ,l

   ! C(m,o) = A(m,n).B(n,o)
   m = 2**12
   n = 2**12
   o = 2**12

   ! number of loops
   l = 10

   allocate(A(m,n),B(n,o))
   call random_number(A)
   call random_number(B)

   call start_benchmark2(t,1,"C_ref = matmul(A,B)")
   do i = 1,l
      C_ref = matmul(A,B)
   end do
   call stop_benchmark2(t,1,m,n,o,l,C)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m1')")
   do i = 1,l
      C = matmul(A,B,'coarray','m1')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m2')")
   do i = 1,l
      C = matmul(A,B,'coarray','m2')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m3')")
   do i = 1,l
      C = matmul(A,B,'coarray','m3')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m4')")
   do i = 1,l
      C = matmul(A,B,'coarray','m4')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m5')")
   do i = 1,l
      C = matmul(A,B,'coarray','m5')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m6')")
   do i = 1,l
      C = matmul(A,B,'coarray','m6')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m7')")
   do i = 1,l
      C = matmul(A,B,'coarray','m7')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m8')")
   do i = 1,l
      C = matmul(A,B,'coarray','m8')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m9')")
   do i = 1,l
      C = matmul(A,B,'coarray','m9')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m10')")
   do i = 1,l
      C = matmul(A,B,'coarray','m10')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

   call start_benchmark(t,1,"C = matmul(A,B,'coarray','m11')")
   do i = 1,l
      C = matmul(A,B,'coarray','m11')
   end do
   call stop_benchmark(t,1,m,n,o,l,C,C_ref)

contains

   subroutine start_benchmark(ti,im,msg)
      type(timer), intent(out) :: ti
      integer,     intent(in)    :: im
      character(*),intent(in)    :: msg
      sync all
      if (this_image() == im) then
         print'(a)', msg
         print'(a,i2)', ' Image        :',im
         call ti%timer_start()
      end if
   end subroutine start_benchmark

   subroutine stop_benchmark(ti,im,s1,s2,s3,nloops,Mat,Mat_ref)
      type(timer), intent(inout) :: ti
      integer,     intent(in)    :: im
      integer,     intent(in)    :: s1,s2,s3,nloops
      real(rk),    intent(in)    :: Mat(s1,s3), Mat_ref(s1,s3)
      if (this_image() == im) then
         call ti%timer_stop(message=' Elapsed time :',nloops=l)
         print'(a,f6.2,a)', ' Performance  : ', real(nloops, rk)*real(s1,rk)*real(s2,rk)*real(s3,rk)*1e-9_rk/ti%elapsed_time,' [GFLOPS]'
         print'(a,3e13.6)', ' Relative err.: ', norm2(Mat_ref-Mat)/norm2(Mat_ref)
         print'(a)', ''
      end if
   end subroutine stop_benchmark

   subroutine start_benchmark2(ti,im,msg)
      type(timer), intent(out) :: ti
      integer,     intent(in)    :: im
      character(*),intent(in)    :: msg
      sync all
      if (this_image() == im) then
         print'(a)', msg
         call ti%timer_start()
      end if
   end subroutine start_benchmark2

   subroutine stop_benchmark2(ti,im,s1,s2,s3,nloops,Mat)
      type(timer), intent(inout) :: ti
      integer,     intent(in)    :: im
      integer,     intent(in)    :: s1,s2,s3,nloops
      real(rk),    intent(in)    :: Mat(s1,s3)
      if (this_image() == im) then
         call ti%timer_stop(message=' Elapsed time :',nloops=l)
         print'(a,f6.2,a)', ' Performance  : ', real(nloops, rk)*real(s1,rk)*real(s2,rk)*real(s3,rk)*1e-9_rk/ti%elapsed_time,' [GFLOPS]'
         print'(a)', ''
      end if
   end subroutine stop_benchmark2

end program benchmark3
