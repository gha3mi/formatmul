program benchmark3

   use kinds,     only: rk
   use fortime,   only: timer
   use formatmul, only: matmul

   implicit none

   real(rk), allocatable         :: A(:,:), B(:,:)
   real(rk), allocatable         :: C_ref(:,:), C(:,:)
   type(timer)                   :: t
   integer                       :: m, n, o, i ,l, p, unit_num
   character(len=:), allocatable :: file_name

   ! number of loops
   l = 10

   file_name = "benchmark3"
   open (newunit = unit_num, file = file_name)
   write(unit_num,'(a)') 'Coarray Matmul Benchmark'
   close(unit_num)

   do p = 250,4000,250

      ! C(m,o) = A(m,n).B(n,o)
      m = p
      n = p
      o = p

      if (this_image() == 1) print*,'----------------------------------------'
      if (this_image() == 1) print'(a,g0,a,g0,a,g0,a,g0,a,g0,a,g0,a)', 'C(',m,',',o,') = A(',m,',',n,').B(',n,',',o,')'
      if (this_image() == 1) print*,''

      if (allocated(A))     deallocate(A)
      if (allocated(B))     deallocate(B)
      if (allocated(C))     deallocate(C)
      if (allocated(C_ref)) deallocate(C_ref)
      allocate(A(m,n))
      allocate(B(n,o))
      allocate(C(m,o))
      allocate(C_ref(m,o))
      call random_number(A)
      call random_number(B)

      call start_benchmark2(t,1,"C_ref = matmul(A,B)")
      do i = 1,l
         C_ref = matmul(A,B)
      end do
      call stop_benchmark2(t,1,m,n,o,l,C,'Matmul',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m1')")
      do i = 1,l
         C = matmul(A,B,'coarray','m1')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_Matmul',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m2')")
      do i = 1,l
         C = matmul(A,B,'coarray','m2')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_dgemm',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m3')")
      do i = 1,l
         C = matmul(A,B,'coarray','m3')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m3',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m4')")
      do i = 1,l
         C = matmul(A,B,'coarray','m4')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m4',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m5')")
      do i = 1,l
         C = matmul(A,B,'coarray','m5')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m5',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m6')")
      do i = 1,l
         C = matmul(A,B,'coarray','m6')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m6',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m7')")
      do i = 1,l
         C = matmul(A,B,'coarray','m7')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m7',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m8')")
      do i = 1,l
         C = matmul(A,B,'coarray','m8')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m8',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m9')")
      do i = 1,l
         C = matmul(A,B,'coarray','m9')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m9',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m10')")
      do i = 1,l
         C = matmul(A,B,'coarray','m10')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m10',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m11')")
      do i = 1,l
         C = matmul(A,B,'coarray','m11')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m11',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m12')")
      do i = 1,l
         C = matmul(A,B,'coarray','m12')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m12',file_name)

      call start_benchmark(t,1,"C = matmul(A,B,'coarray','m13')")
      do i = 1,l
         C = matmul(A,B,'coarray','m13')
      end do
      call stop_benchmark(t,1,m,n,o,l,C,C_ref,'coarray_m13',file_name)

   end do

contains

   subroutine start_benchmark(ti,im,msg)
      type(timer), intent(out) :: ti
      integer,     intent(in)  :: im
      character(*),intent(in)  :: msg
      sync all
      if (this_image() == im) then
         print'(a)', msg
         print'(a,i2)', ' Image        :',im
         call ti%timer_start()
      end if
   end subroutine start_benchmark

   subroutine stop_benchmark(ti,im,s1,s2,s3,nloops,Mat,Mat_ref,method,filename)
      type(timer), intent(inout) :: ti
      integer,     intent(in)    :: im
      integer,     intent(in)    :: s1,s2,s3,nloops
      real(rk),    intent(in)    :: Mat(s1,s3), Mat_ref(s1,s3)
      character(*),intent(in)    :: method
      character(*),intent(in)    :: filename
      integer                    :: nunit
      if (this_image() == im) then
         call ti%timer_stop(message=' Elapsed time :',nloops=nloops)
         print'(a,f6.2,a)', ' Performance  : ', real(s1,rk)*real(s2,rk)*real(s3,rk)*1e-9_rk/ti%elapsed_time,' [GFLOPS]'
         print'(a,3e13.6)', ' Relative err.: ', norm2(Mat_ref-Mat)/norm2(Mat_ref)
         print'(a)', ''
         call write_benchmark(method,s1,s2,s3,nloops,ti,num_images(),filename)
      end if
   end subroutine stop_benchmark

   subroutine start_benchmark2(ti,im,msg)
      type(timer), intent(out) :: ti
      integer,     intent(in)  :: im
      character(*),intent(in)  :: msg
      sync all
      if (this_image() == im) then
         print'(a)', msg
         call ti%timer_start()
      end if
   end subroutine start_benchmark2

   subroutine stop_benchmark2(ti,im,s1,s2,s3,nloops,Mat,method,filename)
      type(timer), intent(inout) :: ti
      integer,     intent(in)    :: im
      integer,     intent(in)    :: s1,s2,s3,nloops
      real(rk),    intent(in)    :: Mat(s1,s3)
      character(*),intent(in)    :: method
      character(*),intent(in)    :: filename
      if (this_image() == im) then
         call ti%timer_stop(message=' Elapsed time :',nloops=nloops)
         print'(a,f6.2,a)', ' Performance  : ', real(s1,rk)*real(s2,rk)*real(s3,rk)*1e-9_rk/ti%elapsed_time,' [GFLOPS]'
         print'(a)', ''
         call write_benchmark(method,s1,s2,s3,nloops,ti,num_images(),filename)
      end if
   end subroutine stop_benchmark2

   subroutine write_benchmark(method,s1,s2,s3,nloops,ti,nimg,filename)
      character(*),intent(in) :: method
      integer,     intent(in) :: s1,s2,s3,nloops
      type(timer), intent(in) :: ti
      integer,     intent(in) :: nimg
      character(*),intent(in) :: filename
      integer                 :: nunit
      open (newunit = nunit, file = filename, access = 'append')
      write(nunit,'(a," ",g0," ",g0," ",g0," ",g0," ",g0," ",g0," ",g0)') method, nimg, s1,s2,s3,nloops, ti%elapsed_time, real(s1,rk)*real(s2,rk)*real(s3,rk)*1e-9_rk/ti%elapsed_time
      close(nunit)
   end subroutine write_benchmark
   
end program benchmark3
