SUBROUTINE mpi_recv_int(val, source, tag, istat)
  INCLUDE 'mpif.h'
  INTEGER, INTENT(IN) :: source, tag
  INTEGER, INTENT(OUT) :: val
  INTEGER, INTENT(OUT) :: istat(MPI_STATUS_SIZE)
  INTEGER :: ierr
  CALL MPI_Recv(val, 1, MPI_INTEGER, source, tag, MPI_COMM_WORLD, istat, ierr)
END SUBROUTINE mpi_recv_int 