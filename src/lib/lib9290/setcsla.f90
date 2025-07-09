!***********************************************************************
!                                                                      *
      SUBROUTINE SETCSLA(NAME, NCORE)
!                                                                      *
!   Open, check, load data from and close the  .csl  file. This file   *
!   is always attached to stream 21.                                   *
!                                                                      *
!   Call(s) to: [RCI92]: LODCSL, OPENFL.                               *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 23 Dec 1992   *
!   Modified by G. Gaigalas,                                May 2011   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  10:50:33   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE IOUNIT_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      USE openfl_I
      USE lodcsl_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER  :: NCORE
      CHARACTER (LEN = 24), INTENT(IN) :: NAME
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER   :: K, IERR, IOS, FILE_SIZE
      LOGICAL   :: FILE_EXISTS
      CHARACTER(LEN=3)   :: STATUS
      CHARACTER(LEN=9)   :: FORM
      CHARACTER(LEN=15)  :: RECORD
      CHARACTER(LEN=256) :: FILNAM, PWD
!-----------------------------------------------
!
!   The  .csl  file is FORMATTED; it must exist
!
      K = INDEX(NAME,' ')
      IF (K == 0) THEN
         ! No space found, use the entire trimmed string
         FILNAM = TRIM(NAME)//'.c'
      ELSE
         ! Space found, use up to the first space
         FILNAM = NAME(1:K-1)//'.c'
      ENDIF
      
      ! Debug: Show current working directory
      CALL GETCWD(PWD)
      WRITE (ISTDE, *) 'Current working directory: ', TRIM(PWD)
      
      ! For MPI programs, need to look in the parent directory (Serial I/O dir)
      ! Check if we're in a mpi_tmp subdirectory
      IF (INDEX(PWD, 'mpi_tmp') > 0) THEN
         ! We're in MPI work directory, go back to parent for input files
         FILNAM = '../../' // TRIM(FILNAM)
         WRITE (ISTDE, *) 'Adjusted path for MPI: ', TRIM(FILNAM)
      ENDIF
      
      FORM = 'FORMATTED'
      STATUS = 'OLD'

!
      WRITE (ISTDE, *) 'Looking for file: ', TRIM(FILNAM)
      
      ! Check if file exists first using INQUIRE (like setcsll does)
      INQUIRE(FILE=TRIM(FILNAM), EXIST=FILE_EXISTS, SIZE=FILE_SIZE)
      IF (.NOT.FILE_EXISTS) THEN
         WRITE (ISTDE, *) TRIM(FILNAM), ' does not exist'
         STOP
      ELSE
         WRITE (ISTDE, *) 'File exists, size: ', FILE_SIZE, ' bytes'
      ENDIF
      
      ! Use standard OPEN instead of OPENFL to avoid potential issues
      OPEN(21, FILE=TRIM(FILNAM), FORM='FORMATTED', STATUS='OLD', IOSTAT=IOS)
      IF (IOS /= 0) THEN
         WRITE (ISTDE, *) 'Error when opening', TRIM(FILNAM), ' IOS=', IOS
         STOP
      ENDIF
      
      ! Check the first record of the file; if not as expected, stop (like setcsll)
      READ (21, '(1A15)', IOSTAT=IOS) RECORD
      IF (IOS/=0 .OR. RECORD(1:15)/='Core subshells:') THEN
         WRITE (ISTDE, *) 'Not a Configuration Symmetry List File;'
         WRITE (ISTDE, *) 'IOS=', IOS, ' Record="', RECORD(1:15), '"'
         WRITE (ISTDE, *) 'File: ', TRIM(FILNAM)
         CLOSE(21)
         STOP
      ENDIF
!
!   Load data from the  .csl  file directly without checking header
!   This bypasses the header check problem and lets LODCSL handle the file
!
      CALL LODCSL (NCORE)
!
!   Close the  .csl  file
!
      CLOSE(21)
!
      RETURN
      END SUBROUTINE SETCSLA
