SUBROUTINE mpi_send_char(buf, count, dest, tag)
  INCLUDE 'mpif.h'
  INTEGER, INTENT(IN) :: count, dest, tag
  CHARACTER(LEN=*), INTENT(IN) :: buf
  INTEGER :: ierr
  CALL MPI_Send(buf, count, MPI_CHARACTER, dest, tag, MPI_COMM_WORLD, ierr)
END SUBROUTINE mpi_send_char 