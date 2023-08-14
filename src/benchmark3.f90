program benchmark3

   use kinds,                         only: rk
   use fortime,                       only: timer
   use formatmul,                     only: matmul
   use formatmul_benchmark,           only: start_benchmark, stop_benchmark, write_benchmark
   use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options

   implicit none

   real(rk), allocatable         :: A(:,:), B(:,:)
   real(rk), allocatable         :: C_ref(:,:), C(:,:)
   type(timer)                   :: t[*]
   integer                       :: m, n, o, i ,nloops, p, unit_num, im, nim
   character(len=:), allocatable :: file_name
   character(len=1000)           :: im_chr

   nloops = 10

   im  = this_image()
   nim = num_images()

   write (im_chr, '(i0)') im

   file_name = "benchmark/benchmark3_im"//trim(im_chr)//".data"

   open (newunit = unit_num, file = file_name)
   write(unit_num,'(a)') 'ForMatmul'
   write(unit_num,'(a)') compiler_version()
   write(unit_num,'(a)') compiler_options()
   write(unit_num,"(g0,' ',g0)") im, nim
   close(unit_num)

   do p = 250,4000,250

      ! C(m,o) = A(m,n).B(n,o)
      m = p
      n = p
      o = p

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

      call start_benchmark(t[im],m,n,o,"C_ref = matmul(A,B)")
      do i = 1,nloops
         C_ref = matmul(A,B)
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C_ref,C_ref,'Matmul',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m1')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m1')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_Matmul',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m2')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m2')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_dgemm',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m3')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m3')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m3',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m4')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m4')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m4',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m5')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m5')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m5',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m6')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m6')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m6',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m7')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m7')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m7',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m8')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m8')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m8',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m9')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m9')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m9',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m10')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m10')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m10',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m11')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m11')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m11',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m12')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m12')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m12',file_name)

      call start_benchmark(t[im],m,n,o,"C = matmul(A,B,'coarray','m13')")
      do i = 1,nloops
         C = matmul(A,B,'coarray','m13')
      end do
      call stop_benchmark(t[im],m,n,o,nloops,C,C_ref,'coarray_m13',file_name)

   end do

end program benchmark3
