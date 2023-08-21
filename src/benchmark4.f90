program benchmark4

   use kinds,                         only: rk
   use fortime,                       only: timer
   use formatmul,                     only: matmul
   use formatmul_benchmark,           only: start_benchmark, stop_benchmark, write_benchmark
   use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options

   implicit none

   real(rk), allocatable         :: A(:,:), v(:)
   real(rk), allocatable         :: w_ref(:), w(:)
   type(timer)                   :: t[*]
   integer                       :: m, n, i ,nloops, p, unit_num, im, nim
   character(len=:), allocatable :: file_name
   character(len=1000)           :: im_chr

   nloops = 10

   im  = this_image()
   nim = num_images()

   write (im_chr, '(i0)') im

   file_name = "benchmark/benchmark4_im"//trim(im_chr)//".data"

   open (newunit = unit_num, file = file_name)
   write(unit_num,'(a)') 'ForMatmul'
   write(unit_num,'(a)') compiler_version()
   write(unit_num,'(a)') compiler_options()
   write(unit_num,"(g0,' ',g0)") im, nim
   close(unit_num)

   do p = 250,30000,250

      ! w(m) = A(m,n).v(n)
      m = p
      n = p

      if (allocated(A))     deallocate(A)
      if (allocated(v))     deallocate(v)
      if (allocated(w))     deallocate(w)
      if (allocated(w_ref)) deallocate(w_ref)
      allocate(A(m,n))
      allocate(v(n))
      allocate(w(m))
      allocate(w_ref(m))
      call random_number(A)
      call random_number(v)

      call start_benchmark(t[im],m,n,1,"w_ref = matmul(A,v)")
      do i = 1,nloops
         w_ref = matmul(A,v)
      end do
      call stop_benchmark(t[im],m,n,1,nloops,w_ref,w_ref,'Matmul',file_name)

      call start_benchmark(t[im],m,n,1,"w = matmul(A,v,'coarray','m1')")
      do i = 1,nloops
         w = matmul(A,v,'coarray','m1')
      end do
      call stop_benchmark(t[im],m,n,1,nloops,w,w_ref,'coarray_Matmul',file_name)
      call start_benchmark(t[im],m,n,1,"w = matmul(A,v,'coarray','m2')")
      do i = 1,nloops
         w = matmul(A,v,'coarray','m2')
      end do
      call stop_benchmark(t[im],m,n,1,nloops,w,w_ref,'coarray_dgemm',file_name)

      call start_benchmark(t[im],m,n,1,"w = matmul(A,v,'coarray','m3')")
      do i = 1,nloops
         w = matmul(A,v,'coarray','m3')
      end do
      call stop_benchmark(t[im],m,n,1,nloops,w,w_ref,'coarray_m3',file_name)

      call start_benchmark(t[im],m,n,1,"w = matmul(A,v,'coarray','m4')")
      do i = 1,nloops
         w = matmul(A,v,'coarray','m4')
      end do
      call stop_benchmark(t[im],m,n,1,nloops,w,w_ref,'coarray_m4',file_name)

      call start_benchmark(t[im],m,n,1,"w = matmul(A,v,'coarray','m5')")
      do i = 1,nloops
         w = matmul(A,v,'coarray','m5')
      end do
      call stop_benchmark(t[im],m,n,1,nloops,w,w_ref,'coarray_m5',file_name)

      call start_benchmark(t[im],m,n,1,"w = matmul(A,v,'coarray','m6')")
      do i = 1,nloops
         w = matmul(A,v,'coarray','m6')
      end do
      call stop_benchmark(t[im],m,n,1,nloops,w,w_ref,'coarray_m6',file_name)

   end do

end program benchmark4
