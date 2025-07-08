SUBROUTINE mpi_send_int(val, dest, tag)
  INCLUDE 'mpif.h'
  INTEGER, INTENT(IN) :: val, dest, tag
  INTEGER :: ierr
  CALL MPI_Send(val, 1, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
END SUBROUTINE mpi_send_int 