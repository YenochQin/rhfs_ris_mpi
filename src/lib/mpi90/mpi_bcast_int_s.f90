SUBROUTINE mpi_bcast_int_s(buffer, root)
  INCLUDE 'mpif.h'
  INTEGER, INTENT(INOUT) :: buffer
  INTEGER, INTENT(IN) :: root
  INTEGER :: ierr
  CALL MPI_Bcast(buffer, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
END SUBROUTINE mpi_bcast_int_s 