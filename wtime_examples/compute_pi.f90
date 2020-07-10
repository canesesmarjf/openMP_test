PROGRAM Compute_Pi
IMPLICIT NONE

! OBJECTIVE:
! Demonstrate the use of openMP directives to solve the classical compute_pi PROGRAM
! and illusrate the use of OMP_GET_TIME to record computation time

! The number of threads needs to be set with the following CLI statement:
! export OMP_NUM_THREADS=16

! OMP_GET_TIME needs to be declared as a DOUBLE PRECISION type in order for it
! Declaring it as REAL or INTEGER makes the function not work even when the program compiles


! Declare openMP functions to use:
! ===========================================
INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
!INTEGER, EXTERNAL :: OMP_SET_NUM_THREADS
DOUBLE PRECISION, EXTERNAL :: OMP_GET_WTIME

! Declare variables:
INTEGER(kind = 8) :: N
DOUBLE PRECISION :: w, pi

! Declare openMP variables
INTEGER(kind = 8) :: i
INTEGER :: id, num_threads
DOUBLE PRECISION :: x, sm, final_sum
DOUBLE PRECISION :: oend, ostart
REAL :: fend, fstart

! Initialize variables:
! ==========================================
N = 2000000000  				! Number of intervals
w = 1.0/N						! Width of each interval
pi = 0.0

! Start timer:
! =========================================
! OMP function:
ostart = OMP_GET_WTIME()
! Fortran function:
call cpu_time(fstart)

sm = 0.0

! Start of parallel section
! ========================================
!$OMP PARALLEL PRIVATE(id)
num_threads = OMP_GET_NUM_THREADS()
id = OMP_GET_THREAD_NUM()
if (id .EQ. 0) write(*,*) "number of threads given = ", num_threads
!$OMP END PARALLEL
! END of parallel section
! =========================================


! Solve via two different methods:
! ============================================================

! ============================================================
! ------------------------REDUCTION---------------------------
! ============================================================
if (.false.) then
write(*,*) "Reduction method"
!$OMP PARALLEL DO PRIVATE(x,i)  &
!$OMP REDUCTION(+:sm)
DO i = 0, N-1
	x = w*(i+0.5)
	sm = sm + (4.0/(1.0 + x*x))
END DO
!$OMP END PARALLEL DO
! End of parallel DO section
! =========================================
pi = pi + sm*w
end if
! ============================================================
! ------------------------CRITICAL---------------------------
! ============================================================
write(*,*) "Critical barrier method"
final_sum = 0.0
!$OMP PARALLEL PRIVATE(x,i,sm)
sm = 0.0
!$OMP DO
DO i = 0, N-1
	x = w*(i+0.5)
	sm = sm + (4.0/(1.0 + x*x))
END DO
!$OMP END DO

!$OMP CRITICAL
final_sum = final_sum + sm
!$OMP END CRITICAL

!$OMP END PARALLEL
pi = pi + final_sum*w
! ============================================================

oend = OMP_GET_WTIME()
call cpu_time(fend)

write(*,*) "pi = ", pi
write(*,*) "OMP elapsed time [s] = ", (oend-ostart)
write(*,*) "Fortran elapsed time [s] = ", (fend-fstart)/num_threads

END PROGRAM
