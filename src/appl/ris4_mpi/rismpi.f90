!***********************************************************************
!                                                                      *
      PROGRAM RISMPI
!                                                                      *
!   Entry routine for RIS. Controls the entire computation. This is    *
!   the MPI version.                                                   *
!                                                                      *
!   Call(s) to: [LIB92]: GETMIX, SETCSL, SETMC, SETCON.                *
!               [SMS92]: CHKPLT, GETSMD, SETDBG, SETSUM                *
!                        STRSUM                                        *
!               [NJGRAF]: FACTT.                                       *
!                                                                      *
!   Written by Per Jonsson                Last revision: 17 Jan 1996   *
!   Modify  by Gediminas Gaigalas                        26 Oct 2009   *
!   Modified by J. Ekman                                 25 Nov 2013   *
!   MPI version by Gemini AI              Last revision:    Jul 2025   *
!                                                                      *
!***********************************************************************
!...Translated by Gediminas Gaigalas 11/18/19
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
      USE ris_cal_I
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      REAL(DOUBLE) :: DR2
      LOGICAL   :: YES
      CHARACTER :: NAME*24
      INTEGER :: K, NCI, ncore_not_used, NOPAR, NCOUNT1
      CHARACTER (LEN = 128) :: startdir,permdir,tmpdir, file_name
      INTEGER :: lenhost, lenperm, lentmp
      CHARACTER (LEN = 3) :: idstring
!-----------------------------------------------
!
!   Matrix elements smaller than CUTOFF are not accumulated
!
      CUTOFF = 1.0D-10
!
!=======================================================================
!  Start mpi --- get processor info: myid, nprocs, host name; and print
!=======================================================================
      startdir = '  '
      permdir = '  '
      tmpdir = '  '
      CALL startmpi2 (myid, nprocs, host, lenhost, ncount1,       &
                      startdir, permdir, tmpdir, 'RIS_MPI')
      WRITE (idstring, '(I3.3)') myid
      lenperm = LEN_TRIM (permdir)
      lentmp = LEN_TRIM (tmpdir)

      IF (myid .EQ. 0) THEN
          WRITE (ISTDE, *)
          WRITE (ISTDE, *) 'RIS_MPI: Execution begins ...'
      ENDIF

      IF (myid .EQ. 0) THEN
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
      CALL MPI_Bcast (NDEF, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

    9 CONTINUE
      IF (myid .EQ. 0) THEN
          WRITE (ISTDE, *) 'Name of state'
          READ(*,'(A)') NAME
      ENDIF
      CALL MPI_Bcast (NAME, 24, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

      K=INDEX(NAME,' ')
      IF (K.EQ.1) THEN
         IF (myid .EQ. 0) WRITE (ISTDE, *) 'Names may not start with a blank'
         GOTO 9
      ENDIF

      IF (myid .EQ. 0) THEN
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
      CALL MPI_Bcast (NCI, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

!
!   Determine if there is to be any debug printout; this will be
!   made on the  .dbg  file
!
      CALL SETDBG
!
!   Perform machine- and installation-dependent setup
!
      CALL SETMC
!
!   Set up the physical constants
!
      CALL SETCON
!
!   Open the  .i  file
!
      file_name = ' '
      IF (myid .EQ. 0) THEN
          file_name = permdir(1:lenperm) // '/' // TRIM(NAME)
          CALL SETSUM(file_name,NCI)
      ENDIF
!
!   Open, check, load data from, and close, the  .csl  file
!
      file_name = ' '
      IF (myid .EQ. 0) file_name = permdir(1:lenperm) // '/' // TRIM(NAME)
      CALL SETCSLA(file_name,ncore_not_used)
!
!   Get the remaining information
!
      CALL GETSMD(NAME)
!
!   Get the eigenvectors
!
      file_name = ' '
      IF (myid .EQ. 0) file_name = permdir(1:lenperm) // '/' // TRIM(NAME)
      CALL GETMIXBLOCK(file_name,NCI)
!
!   Append a summary of the inputs to the  .sum  file
!
!      CALL STRSUM
!
!   Set up the table of logarithms of factorials
!
      CALL FACTT
!
!   Proceed with the RIS calculation
!
      CALL RIS_CAL(NAME)
!
!   Print completion message
!
      IF (myid .EQ. 0) THEN
          WRITE (ISTDE, *)
          WRITE (ISTDE, *) 'RIS_MPI: Execution complete.'
      ENDIF
!
      CALL stopmpi2 (myid, nprocs, host, lenhost, ncount1, 'RIS_MPI')

      STOP
      END  PROGRAM RISMPI
