!***********************************************************************
!***********************************************************************
!***********************************************************************
!**                                                                  ***
!**                                                                  ***
!**           **   **  *******   *****    *****    *****             ***
!**           **   **  **       **   **  **   **  **   **            ***
!**           **   **  **       **       **   **      **             ***
!**           *******  ****      *****    *****      **              ***
!**           **   **  **            **      **     **               ***
!**           **   **  **       **   **     **     **                ***
!**           **   **  **        *****    **      *******            ***
!**                                                                  ***
!**            Relativistic Hyperfine Structure Program              ***
!**                         GRASP92 Version                          ***
!**                         Dynamic Storage                          ***
!**                                                                  ***
!**   ============================================================   ***
!**   Copyright (c) 1995 by P Jonsson, F A Parpia, and C F Fischer   ***
!**   ============================================================   ***
!**   All rights reserved.  No part of this software or its accom-   ***
!**   panying documentation may be reproduced, stored in a retrie-   ***
!**   val system,  or transmitted,  in any form or  by any  means,   ***
!**   electronic, mechanical,  photocopying,  recording, or other-   ***
!**   wise, without the prior written permission of the authors.     ***
!**                                                                  ***
!**                           Disclaimer                             ***
!**                           ==========                             ***
!**   The  authors make  no warranties,  express or implied,  that   ***
!**   this software or its  accompanying documentation are free of   ***
!**   error or that  they will meet your requirements for any par-   ***
!**   ticular application.  Neither this software nor its accompa-   ***
!**   nying documentation should be relied  upon for solving prob-   ***
!**   lems if an incorrect solution could result in injury or loss   ***
!**   of, or damage to, property. If you use this software or  its   ***
!**   accompanying documentation,  you do so entirely  at your own   ***
!**   risk;  the authors disclaim all liability for direct or con-   ***
!**   sequential damage.                                             ***
!**                                                                  ***
!***********************************************************************
!***********************************************************************
!***********************************************************************
      PROGRAM HFS92MPI
!                                                                      *
!   Entry routine for HFS92. Controls the entire computation.          *
!                                                                      *
!   Call(s) to: [LIB92]: GETMIX, SETCSL, SETMC, SETCON.                *
!               [HFS92]: CHKPLT, GETHFD, HFS, SETDBG, SETSUM,          *
!                        STRSUM.                                       *
!               [NJGRAF]: FACTT.                                       *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 24 Dec 1992   *
!   Modified by Gediminas Gaigalas for new spin-angular integration.   *
!                                         Last revision:    Nov 2017   *
!   MPI version by Gemini AI              Last revision:    Jul 2025   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:06:03   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  11/01/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE vast_kind_param, ONLY:  DOUBLE
      USE default_C
      USE iounit_C
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
      USE gethfd_I
      USE getmixblock_I
      USE strsum_I
      USE factt_I
      USE hfsggmpi_I
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER   :: K, NCI, NCORE_NOT_USED, NCOUNT1
      LOGICAL   :: YES
      CHARACTER :: NAME*24
      CHARACTER (LEN = 128) :: startdir,permdir,tmpdir, file_name
      INTEGER :: lenhost, lenperm, lentmp
      CHARACTER (LEN = 3) :: idstring
!-----------------------------------------------
!
!=======================================================================
!  Start mpi --- get processor info: myid, nprocs, host name; and print
!=======================================================================
      startdir = '  '
      permdir = '  '
      tmpdir = '  '
      CALL startmpi2 (myid, nprocs, host, lenhost, ncount1,       &
                      startdir, permdir, tmpdir, 'RHFS_MPI')
      WRITE (idstring, '(I3.3)') myid
      lenperm = LEN_TRIM (permdir)
      lentmp = LEN_TRIM (tmpdir)

      IF (myid .EQ. 0) THEN
          WRITE (ISTDE, *)
          WRITE (ISTDE, *) 'RHFS_MPI'
          WRITE (ISTDE, *) 'This is the MPI parallel hyperfine structure program'
          WRITE (ISTDE, *) 'Input files:  isodata, name.c, name.(c)m, name.w'
          WRITE (ISTDE, *) 'Output files: name.(c)h, name.(c)hoffd'
          WRITE (ISTDE, *)
      ENDIF

      IF (myid .EQ. 0) THEN
          WRITE (ISTDE, *) 'Default settings?'
          YES = GETYN()
          WRITE (ISTDE, *)
          IF (YES) THEN
             NDEF = 0
          ELSE
             NDEF = 1
          ENDIF
      ENDIF
      CALL MPI_Bcast (NDEF, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

   10 CONTINUE
      IF (myid .EQ. 0) THEN
          WRITE (ISTDE, *) 'Name of state'
          READ (*, '(A)') NAME
      ENDIF
      CALL MPI_Bcast (NAME, 24, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

      K = INDEX(NAME,' ')
      IF (K == 1) THEN
         IF (myid .EQ. 0) WRITE (ISTDE, *) 'Names may not start with a blank'
         GO TO 10
      ENDIF

      IF (myid .EQ. 0) THEN
          WRITE (ISTDE, *)
          WRITE (ISTDE, *) 'Mixing coefficients from a CI calc.?'
          YES = GETYN()
          IF (YES) THEN
             NCI = 0
          ELSE
             NCI = 1
          ENDIF
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
!   Open the  .sum  file
!
      file_name = ' '
      IF (myid .EQ. 0) THEN
          file_name = permdir(1:lenperm) // '/' // TRIM(NAME)
          CALL SETSUM (file_name, NCI)
      ENDIF
!
!   Open, check, load data from, and close, the  .csl  file
!
      file_name = ' '
      IF (myid .EQ. 0) file_name = permdir(1:lenperm) // '/' // TRIM(NAME)
      CALL SETCSLA (file_name, NCORE_NOT_USED)
!
!   Get the remaining information
!
      file_name = ' '
      IF (myid .EQ. 0) file_name = permdir(1:lenperm) // '/' // TRIM(NAME)
      CALL GETHFD (file_name)
!
!   Get the eigenvectors
!
      file_name = ' '
      IF (myid .EQ. 0) file_name = permdir(1:lenperm) // '/' // TRIM(NAME)
      CALL GETMIXBLOCK (file_name, NCI)
!
!   Append a summary of the inputs to the  .sum  file
!
      IF (myid .EQ. 0) CALL STRSUM
!
!   Set up the table of logarithms of factorials
!
      CALL FACTT
!
!   Proceed with the HFS calculation
!
      CALL HFSGGMPI
!
!   Print completion message
!
      IF (myid .EQ. 0) THEN
          WRITE (ISTDE, *)
          WRITE (ISTDE, *) 'RHFS_MPI: Execution complete.'
      ENDIF
!
      CALL stopmpi2 (myid, nprocs, host, lenhost, ncount1, 'RHFS_MPI')

      STOP
      END PROGRAM HFS92MPI
