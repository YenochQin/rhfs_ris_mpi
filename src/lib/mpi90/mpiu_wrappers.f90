MODULE mpiu_wrappers
  IMPLICIT NONE
CONTAINS

  SUBROUTINE mpi_send_int(val, dest, tag)
    INCLUDE 'mpif.h'
    INTEGER, INTENT(IN) :: val, dest, tag
    INTEGER :: ierr
    CALL MPI_Send(val, 1, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
  END SUBROUTINE mpi_send_int

  SUBROUTINE mpi_send_char(buf, count, dest, tag)
    INCLUDE 'mpif.h'
    INTEGER, INTENT(IN) :: count, dest, tag
    CHARACTER(LEN=*), INTENT(IN) :: buf
    INTEGER :: ierr
    CALL MPI_Send(buf, count, MPI_CHARACTER, dest, tag, MPI_COMM_WORLD, ierr)
  END SUBROUTINE mpi_send_char

  SUBROUTINE mpi_recv_int(val, source, tag, istat)
    INCLUDE 'mpif.h'
    INTEGER, INTENT(IN) :: source, tag
    INTEGER, INTENT(OUT) :: val
    INTEGER, INTENT(OUT) :: istat(MPI_STATUS_SIZE)
    INTEGER :: ierr
    CALL MPI_Recv(val, 1, MPI_INTEGER, source, tag, MPI_COMM_WORLD, istat, ierr)
  END SUBROUTINE mpi_recv_int

  SUBROUTINE mpi_recv_char(buf, count, source, tag, istat)
    INCLUDE 'mpif.h'
    INTEGER, INTENT(IN) :: count, source, tag
    CHARACTER(LEN=*), INTENT(OUT) :: buf
    INTEGER, INTENT(OUT) :: istat(MPI_STATUS_SIZE)
    INTEGER :: ierr
    CALL MPI_Recv(buf, count, MPI_CHARACTER, source, tag, MPI_COMM_WORLD, istat, ierr)
  END SUBROUTINE mpi_recv_char

END MODULE mpiu_wrappers 