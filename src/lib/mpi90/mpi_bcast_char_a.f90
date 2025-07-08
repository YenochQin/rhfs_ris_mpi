SUBROUTINE mpi_bcast_char_a(buffer, count, root)
  INCLUDE 'mpif.h'
  INTEGER, INTENT(IN) :: count, root
  CHARACTER(LEN=*), DIMENSION(*), INTENT(INOUT) :: buffer
  INTEGER :: ierr
  CALL MPI_Bcast(buffer, count, MPI_BYTE, root, MPI_COMM_WORLD, ierr)
END SUBROUTINE mpi_bcast_char_a 