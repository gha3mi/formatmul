module formatmul_benchmark

   use kinds,   only: rk
   use fortime, only: timer

   implicit none

   private
   public start_benchmark, stop_benchmark, write_benchmark

contains

   !> author: Seyed Ali Ghasemi
   subroutine start_benchmark(t,m,n,o,msg)
      type(timer), intent(out) :: t
      character(*),intent(in)  :: msg
      integer,     intent(in)  :: m,n,o

#if defined(USE_COARRAY)
      sync all
      if (this_image() == 1) then
         print"(a,', m=',g0,', n=',g0,', o=',g0)", msg, m, n, o
      end if
      call t%timer_start()
#else
      print"(a,', m=',g0,', n=',g0,', o=',g0)", msg, m, n, o
      call t%timer_start()
#endif
   end subroutine start_benchmark

   !> author: Seyed Ali Ghasemi
   subroutine stop_benchmark(t,m,n,o,nloops,Mat,Mat_ref,method,filename)

#if defined(USE_COARRAY)
      type(timer), intent(inout) :: t[*]
#else
      type(timer), intent(inout) :: t
#endif
      integer,     intent(in)    :: m,n,o,nloops
      real(rk),    intent(in)    :: Mat(m,o), Mat_ref(m,o)
      character(*),intent(in)    :: method
      character(*),intent(in)    :: filename
      integer                    :: nunit, i
      real(rk)                   :: elapsed_time_average
#if defined(USE_COARRAY)
      real(rk), allocatable      :: gflops[:]
#else
      real(rk)                   :: gflops
#endif
      real(rk)                   :: gflops_total

#if defined(USE_COARRAY)
      allocate(gflops[*])
      call t[this_image()]%timer_stop(message=' Elapsed time :',nloops=nloops)
      gflops[this_image()] = real(m,rk)*real(n,rk)*real(o,rk)*1e-9_rk/t[this_image()]%elapsed_time
      print'(a,f6.2,a)', ' Performance  : ', gflops[this_image()],' [GFLOPS/image]'
      sync all
      if (this_image() == 1) then
         elapsed_time_average = 0.0_rk
         gflops_total = 0.0_rk
         do i = 1,num_images()
            elapsed_time_average = elapsed_time_average + t[i]%elapsed_time
            gflops_total = gflops_total + gflops[i]
         end do
         elapsed_time_average = elapsed_time_average/num_images()
         print'(a,3e13.6)', ' Relative err.: ', norm2(Mat_ref-Mat)/norm2(Mat_ref)
         print'(a,f7.3,a)', ' Elapsed time (average) :', elapsed_time_average,' [s]'
         print'(a,f6.2,a)', ' Performance  (total)   : ', gflops_total, ' [GFLOPS]'
         print'(a)', ''
      end if
      call co_broadcast(elapsed_time_average, 1)
      call co_broadcast(gflops_total, 1)
      call write_benchmark(method,m,n,o,nloops,t,elapsed_time_average,gflops,gflops_total,filename)
#else
      call t%timer_stop(message=' Elapsed time :',nloops=nloops)
      gflops = real(m,rk)*real(n,rk)*real(o,rk)*1e-9_rk/t%elapsed_time
      print'(a,f6.2,a)', ' Performance  : ', gflops,' [GFLOPS]'
      print'(a,3e13.6)', ' Relative err.: ', norm2(Mat_ref-Mat)/norm2(Mat_ref)
      print'(a,f7.3,a)', ' Elapsed time :', t%elapsed_time,' [s]'
      print'(a,f6.2,a)', ' Performance  : ', gflops, ' [GFLOPS]'
      print'(a)', ''
      call write_benchmark(method,m,n,o,nloops,t,gflops,filename)
#endif
   end subroutine stop_benchmark

#if defined(USE_COARRAY)
   !> author: Seyed Ali Ghasemi
   subroutine write_benchmark(method,m,n,o,nloops,t,elapsed_time_average,gflops,gflops_total,filename)
      character(*),intent(in) :: method
      integer,     intent(in) :: m,n,o,nloops
      type(timer), intent(in) :: t[*]
      character(*),intent(in) :: filename
      real(rk),    intent(in) :: elapsed_time_average
      real(rk),    intent(in) :: gflops_total
      integer                 :: nunit
      real(rk)                :: gflops[*]

      open (newunit = nunit, file = filename, access = 'append')
      write(nunit,'(a," ",g0," ",g0," ",g0," ",g0," ",g0," ",g0," ",g0," ",g0)') &
         method, m,n,o,nloops, t[this_image()]%elapsed_time, gflops[this_image()], elapsed_time_average, gflops_total
      close(nunit)
   end subroutine write_benchmark
#else
   !> author: Seyed Ali Ghasemi
   subroutine write_benchmark(method,m,n,o,nloops,t,gflops,filename)
      character(*),intent(in) :: method
      integer,     intent(in) :: m,n,o,nloops
      type(timer), intent(in) :: t
      character(*),intent(in) :: filename
      integer                 :: nunit
      real(rk)                :: gflops

      open (newunit = nunit, file = filename, access = 'append')
      write(nunit,'(a," ",g0," ",g0," ",g0," ",g0," ",g0," ",g0)') &
         method, m,n,o,nloops, t%elapsed_time, gflops
      close(nunit)
   end subroutine write_benchmark
#endif

end module formatmul_benchmark
