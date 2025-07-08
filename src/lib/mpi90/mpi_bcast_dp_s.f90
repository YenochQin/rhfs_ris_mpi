SUBROUTINE mpi_bcast_dp_s(buffer, root)
  USE vast_kind_param, ONLY: DOUBLE
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  REAL(DOUBLE), INTENT(INOUT) :: buffer
  INTEGER, INTENT(IN) :: root
  INTEGER :: ierr
  CALL MPI_Bcast(buffer, 1, MPI_DOUBLE_PRECISION, root, MPI_COMM_WORLD, ierr)
END SUBROUTINE mpi_bcast_dp_s 