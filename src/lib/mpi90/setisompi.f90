!***********************************************************************
      SUBROUTINE setisompi (isofile)

! An MPI container for setiso which opens, checks, and loads isofile
! to get isotope data. Data loaded are:
!     EMN,Z,/NPAR/,/NSMDAT/
! where /.../ means whole common block.
!
! Xinghong He 98-08-06
!
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:04:58   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE MPI_C
      USE DEF_C
      USE NPAR_C
      USE NSMDAT_C, ONLY: SQN, DMOMNM, QMOMB
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      USE setiso_I
      USE orthsc_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      CHARACTER (LEN = *) :: isofile
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: J, I, K, NWIN, IOS, NPY, NAKY, MY
      REAL(DOUBLE) :: CON, FKK, EY, PZY, DNORM
      real(double), dimension(:), pointer :: PA, QA, RA
      REAL(DOUBLE) :: Z, EMN, PARM(2)
      INTEGER NPARM
      INTEGER ierr
      EXTERNAL mpi_bcast_dp_s, mpi_bcast_dp_a, mpi_bcast_int_s
!-----------------------------------------------
      IF (myid .EQ. 0) THEN
         OPEN (UNIT=22, FILE='isodata', STATUS='OLD')
         CALL SETISO (isofile)
         CLOSE (22)
      ENDIF

      CALL mpi_bcast_dp_s (Z, 0)
      CALL mpi_bcast_dp_s (EMN, 0)
      CALL mpi_bcast_dp_a (PARM, 2, 0)
      CALL mpi_bcast_int_s (NPARM, 0)
      CALL MPI_Bcast (SQN,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
      CALL MPI_Bcast (DMOMNM,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD, &
        ierr)
      CALL MPI_Bcast (QMOMB,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD, &
        ierr)

      RETURN
      END
