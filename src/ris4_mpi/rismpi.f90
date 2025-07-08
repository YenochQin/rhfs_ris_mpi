!***********************************************************************
!                                                                      *
      PROGRAM RISMPI
!                                                                      *
!   Entry routine for RIS MPI version. Controls the entire computation *
!   with MPI parallelization.                                          *
!                                                                      *
!   Call(s) to: [LIB92]: GETMIX, SETCSL, SETMC, SETCON.                *
!               [MPI90]: startmpi2, stopmpi2.                          *
!               [SMS92]: CHKPLT, GETSMD, SETDBG, SETSUM                *
!                        STRSUM                                        *
!               [NJGRAF]: FACTT.                                       *
!                                                                      *
!   Written by Per Jonsson                Last revision: 17 Jan 1996   *
!   Modify  by Gediminas Gaigalas                        26 Oct 2009   *
!   Modified by J. Ekman                                 25 Nov 2013   *
!   MPI Version created                   Last revision:    Dec 2024   *
!                                                                      *
!***********************************************************************
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE vast_kind_param, ONLY: DOUBLE
      USE default_C
      USE iounit_C
      USE debug_C,         ONLY: CUTOFF
      USE mpi_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      USE getyn_I
      USE setdbg_I
      USE setmc_I
      USE setcon_I
      USE setsum_I
      USE setcsla_I
      USE getmixblock_I
      USE strsum_I
      USE factt_I
      USE ris_cal_mpi_I
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      REAL(DOUBLE) :: DR2
      LOGICAL   :: YES
      CHARACTER :: NAME*24
      INTEGER :: K, NCI, ncore_not_used, NOPAR, NCOUNT1
      CHARACTER(LEN=128) :: STARTDIR, PERMDIR, TMPDIR
      CHARACTER(LEN=3) :: IDSTRING
!-----------------------------------------------

!=======================================================================
!  Start MPI --- get processor info: myid, nprocs, host name
!=======================================================================

      CALL startmpi2 (myid, nprocs, host, lenhost, ncount1,       &
                     startdir, permdir, tmpdir, 'RIS_MPI')
      WRITE (idstring, '(I3.3)') myid

!   Matrix elements smaller than CUTOFF are not accumulated
      CUTOFF = 1.0D-10

      IF (myid .EQ. 0) THEN
         WRITE (ISTDE, *)
         WRITE (ISTDE, *) 'RIS MPI Version: Execution begins ...'

         WRITE (ISTDE, *)
         WRITE (ISTDE, *) 'Default settings?'
         YES = GETYN ()
         WRITE (ISTDE, *)
         IF (YES) THEN
            NDEF = 0
         ELSE
            NDEF = 1
         ENDIF
      ENDIF

      CALL MPI_Bcast (NDEF,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

      IF (myid .EQ. 0) THEN
    9    WRITE (ISTDE, *) 'Name of state'
         read(*,'(A)') NAME
         K=INDEX(NAME,' ')
         IF (K.EQ.1) THEN
            WRITE (ISTDE, *) 'Names may not start with a blank'
            GOTO 9
         ENDIF
         PRINT *

         WRITE (ISTDE, *) 'Mixing coefficients from a CI calc.?'
         YES = GETYN ()
         IF (YES) THEN
            NCI = 0
         ELSE
            NCI = 1
         ENDIF
         WRITE (ISTDE, *)
      ENDIF

      CALL MPI_Bcast (NAME,24,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_Bcast (NCI,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!   Determine if there is to be any debug printout; this will be
!   made on the  .dbg  file
      CALL SETDBG

!   Perform machine- and installation-dependent setup
      CALL SETMC

!   Set up the physical constants
      CALL SETCON

!   Open the  .i  file (only on node 0)
      IF (myid .EQ. 0)  CALL SETSUM(NAME,NCI)

!   Open, check, load data from, and close, the  .csl  file
      CALL SETCSLA(NAME,ncore_not_used)

!   Get the remaining information
      CALL GETSMD(NAME)

!   Get the eigenvectors
      CALL GETMIXBLOCK(NAME,NCI)

!   Append a summary of the inputs to the  .sum  file
!      IF (myid .EQ. 0)  CALL STRSUM

!   Set up the table of logarithms of factorials
      CALL FACTT

!   Proceed with the RIS calculation (MPI version)
      CALL RIS_CAL_MPI(NAME, myid, nprocs)

!   Print completion message
      IF (myid .EQ. 0) THEN
         WRITE (ISTDE, *)
         WRITE (ISTDE, *) 'RIS MPI: Execution complete.'
      ENDIF

!   Stop MPI
      CALL stopmpi2 (myid, nprocs, host, lenhost, ncount1, 'RIS_MPI')

      STOP
      END  PROGRAM RISMPI 