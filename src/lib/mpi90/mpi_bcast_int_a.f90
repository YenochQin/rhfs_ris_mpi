SUBROUTINE mpi_bcast_int_a(buffer, count, root)
  INCLUDE 'mpif.h'
  INTEGER, INTENT(IN) :: count, root
  INTEGER, DIMENSION(*), INTENT(INOUT) :: buffer
  INTEGER :: ierr
  CALL MPI_Bcast(buffer, count, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
END SUBROUTINE mpi_bcast_int_a 