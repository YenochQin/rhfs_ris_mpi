!***********************************************************************
      SUBROUTINE gisummpi (ix, n)
!     Sum x over all nodes and in iy, then copy iy to  ix
!***********************************************************************
      IMPLICIT NONE
      INCLUDE 'mpif.h'

      INTEGER, INTENT(IN)                         :: n
      INTEGER, DIMENSION(1:n), INTENT(INOUT) ::  ix

      INTEGER                                     :: ierr, i
      INTEGER, DIMENSION(1:n)                ::  iy

      iy = 0

      CALL MPI_Allreduce (ix(1), iy(1), n, MPI_INTEGER, MPI_SUM, &
                                     MPI_COMM_WORLD, ierr)
      CALL icopy (n, iy, 1, ix, 1)

      RETURN
      END 