SUBROUTINE mpi_bcast_dp_a(buffer, count, root)
  USE vast_kind_param, ONLY: DOUBLE
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, INTENT(IN) :: count, root
  REAL(DOUBLE), DIMENSION(*), INTENT(INOUT) :: buffer
  INTEGER :: ierr
  CALL MPI_Bcast(buffer, count, MPI_DOUBLE_PRECISION, root, MPI_COMM_WORLD, ierr)
END SUBROUTINE mpi_bcast_dp_a 