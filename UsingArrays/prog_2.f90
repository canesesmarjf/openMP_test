PROGRAM prog_2

! Declare modules:
! ===================================================
use OMP_LIB

implicit none
! Local variables:
! =====================================================
integer m, n, i, j
real, dimension(:,:), ALLOCATABLE :: a,s

! openMP variables:
! =====================================================
INTEGER:: nn, tt
INTEGER :: id, num_threads
DOUBLE PRECISION :: t_end, t_start

! Initialize variables:
! ===================================================
m = 10000000
n = 1
allocate(a(m,n), s(32,m))
a = 3.0

! Check how many threads:
! ==================================================
!call OMP_SET_NUM_THREADS(4)

!$OMP PARALLEL PRIVATE(id)
num_threads = OMP_GET_NUM_THREADS()
id = OMP_GET_THREAD_NUM()
if (id .EQ. 0) write(*,*) "number of threads given = ", num_threads
!$OMP END PARALLEL

! Start computation:
! ==================================================
t_start = OMP_GET_WTIME()

!$omp parallel do private(i,j), shared(s,a)
do i = 1, m
   s(1,i) = 0.0
   do j = 1, n
      s(1,i) = s(1,i) + a(i,j)
   enddo
enddo
!$omp end parallel do

t_end = OMP_GET_WTIME()

write(*,*) "Time taken: ", (t_end-t_start)*1000.0, '[ms]'

end PROGRAM
