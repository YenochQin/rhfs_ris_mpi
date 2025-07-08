!***********************************************************************
!                                                                      *
      MODULE hfsggmpi_I
!                                                                      *
!   Interface for HFSGGMPI subroutine                                  *
!                                                                      *
!***********************************************************************
      INTERFACE
         SUBROUTINE HFSGGMPI(MYID, NPROCS)
         INTEGER, INTENT(IN) :: MYID, NPROCS
         END SUBROUTINE HFSGGMPI
      END INTERFACE
      END MODULE hfsggmpi_I 