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
!**                         GRASP92 MPI Version                      ***
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
!   Entry routine for HFS92 MPI version. Controls the entire           *
!   computation with MPI parallelization.                              *
!                                                                      *
!   Call(s) to: [LIB92]: GETMIX, SETCSL, SETMC, SETCON.                *
!               [MPI90]: startmpi2, stopmpi2.                          *
!               [HFS92]: CHKPLT, GETHFD, HFS, SETDBG, SETSUM,          *
!                        STRSUM.                                       *
!               [NJGRAF]: FACTT.                                       *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 24 Dec 1992   *
!   Modified by Gediminas Gaigalas for new spin-angular integration.   *
!                                         Last revision:    Nov 2017   *
!   MPI Version created                   Last revision:    Dec 2024   *
!                                                                      *
!***********************************************************************
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
      CHARACTER(LEN=128) :: STARTDIR, PERMDIR, TMPDIR
      CHARACTER(LEN=3) :: IDSTRING
!-----------------------------------------------

!=======================================================================
!  Start MPI --- get processor info: myid, nprocs, host name
!=======================================================================

      CALL startmpi2 (myid, nprocs, host, lenhost, ncount1,       &
                     startdir, permdir, tmpdir, 'RHFS_MPI')
      WRITE (idstring, '(I3.3)') myid

!=======================================================================
!  Main computation done on node 0, others wait
!=======================================================================

      IF (myid .EQ. 0) THEN
         WRITE (ISTDE, *)
         WRITE (ISTDE, *) 'RHFS MPI Version'
         WRITE (ISTDE, *) 'This is the hyperfine structure program'
         WRITE (ISTDE, *) 'Input files:  isodata, name.c, name.(c)m, name.w'
         WRITE (ISTDE, *) 'Output files: name.(c)h, name.(c)hoffd'

         WRITE (ISTDE, *)
         WRITE (ISTDE, *) 'Default settings?'
         YES = GETYN()
         WRITE (ISTDE, *)
         IF (YES) THEN
            NDEF = 0
         ELSE
            NDEF = 1
         ENDIF
      ENDIF

      CALL MPI_Bcast (NDEF,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

      IF (myid .EQ. 0) THEN
   10    CONTINUE
         WRITE (ISTDE, *) 'Name of state'
         read (*, '(A)') NAME
         K = INDEX(NAME,' ')
         IF (K == 1) THEN
            WRITE (ISTDE, *) 'Names may not start with a blank'
            GO TO 10
         ENDIF
         WRITE (ISTDE, *)
         WRITE (ISTDE, *) 'Mixing coefficients from a CI calc.?'
         YES = GETYN()
         IF (YES) THEN
            NCI = 0
         ELSE
            NCI = 1
         ENDIF
      ENDIF

      CALL MPI_Bcast (NAME,24,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_Bcast (NCI,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

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
!   Open the  .sum  file (only on node 0)
!
      IF (myid .EQ. 0)  CALL SETSUM (NAME, NCI)
!
!   Open, check, load data from, and close, the  .csl  file
!
      CALL SETCSLA (NAME, NCORE_NOT_USED)
!
!   Get the remaining information
!
      CALL GETHFD (NAME)
!
!   Get the eigenvectors
!
      CALL GETMIXBLOCK (NAME, NCI)
!
!   Append a summary of the inputs to the  .sum  file
!
      IF (myid .EQ. 0)  CALL STRSUM
!
!   Set up the table of logarithms of factorials
!
      CALL FACTT
!
!   Proceed with the HFS calculation (MPI version)
!
      CALL HFSGGMPI (myid, nprocs)
!
!   Print completion message
!
      IF (myid .EQ. 0) THEN
         WRITE (ISTDE, *)
         WRITE (ISTDE, *) 'RHFS MPI: Execution complete.'
      ENDIF
!
!   Stop MPI
!
      CALL stopmpi2 (myid, nprocs, host, lenhost, ncount1, 'RHFS_MPI')

      STOP
      END PROGRAM HFS92MPI 