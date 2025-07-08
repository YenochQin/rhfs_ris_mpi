SUBROUTINE mpi_recv_char(buf, count, source, tag, istat)
  INCLUDE 'mpif.h'
  INTEGER, INTENT(IN) :: count, source, tag
  CHARACTER(LEN=*), INTENT(OUT) :: buf
  INTEGER, INTENT(OUT) :: istat(MPI_STATUS_SIZE)
  INTEGER :: ierr
  CALL MPI_Recv(buf, count, MPI_CHARACTER, source, tag, MPI_COMM_WORLD, istat, ierr)
END SUBROUTINE mpi_recv_char 