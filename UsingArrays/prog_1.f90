Program prog_1
implicit none

! OBJECTIVE:

! Declare openMP functions to use:
! ===========================================
INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
DOUBLE PRECISION, EXTERNAL :: OMP_GET_WTIME

! Declare local variables:
! ==========================================
! Program variables:
INTEGER(kind = 8) :: N, T
DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: A
DOUBLE PRECISION :: b

! openMP variables
INTEGER(kind = 8) :: nn, tt
INTEGER :: id, num_threads
DOUBLE PRECISION :: t_end, t_start

! Initialize variables:
! ==========================================
N = 1000000
T = 500
ALLOCATE(A(16,N))
A = 0.0
b = 0.0

WRITE(*,*) "A(22,22)", A(22,22)

! Check how many threads:
! =========================================
!$OMP PARALLEL PRIVATE(id)
num_threads = OMP_GET_NUM_THREADS()
id = OMP_GET_THREAD_NUM()
if (id .EQ. 0) write(*,*) "number of threads given = ", num_threads
!$OMP END PARALLEL

! Main part of PROGRAM
! =========================================
t_start = OMP_GET_WTIME()

!$OMP PARALLEL DO PRIVATE(nn), SHARED(A)
do tt = 1,T
  do nn = 1,N
      A(1,nn) = nn + A(1,nn)
  end do
end do
!$OMP END PARALLEL DO


t_end = OMP_GET_WTIME()

write(*,*) "Time taken: ", (t_end-t_start)*1000.0, '[ms]'


end program
