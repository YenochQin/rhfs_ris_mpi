!***********************************************************************
!                                                                      *
      MODULE ris_cal_mpi_I
!                                                                      *
!   Interface for RIS_CAL_MPI subroutine                               *
!                                                                      *
!***********************************************************************
      INTERFACE
         SUBROUTINE RIS_CAL_MPI(NAME, MYID, NPROCS)
         CHARACTER(LEN=*), INTENT(IN) :: NAME
         INTEGER, INTENT(IN) :: MYID, NPROCS
         END SUBROUTINE RIS_CAL_MPI
      END INTERFACE
      END MODULE ris_cal_mpi_I 