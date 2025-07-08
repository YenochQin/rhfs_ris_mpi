!***********************************************************************
!                                                                      *
      MODULE hfszeemanmpi_I
!                                                                      *
!   Interface for HFSZEEMANMPI subroutine                              *
!                                                                      *
!***********************************************************************
      INTERFACE
         SUBROUTINE HFSZEEMANMPI(NOFFD, MYID, NPROCS)
         INTEGER, INTENT(IN) :: NOFFD, MYID, NPROCS
         END SUBROUTINE HFSZEEMANMPI
      END INTERFACE
      END MODULE hfszeemanmpi_I 