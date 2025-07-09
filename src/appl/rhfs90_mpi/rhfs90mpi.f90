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
!**      Relativistic Hyperfine Structure Program - MPI Version      ***
!**                         GRASP92 Version                          ***
!**                         Dynamic Storage                          ***
!**                                                                  ***
!***********************************************************************
!***********************************************************************
!***********************************************************************
      PROGRAM RHFS90MPI
!                                                                      *
!   Entry routine for HFS92-MPI. Controls the entire computation.      *
!                                                                      *
!   This is an MPI parallel version of the original RHFS90 program.   *
!   The main parallelization is in the calculation of matrix elements *
!   in the double loop over configuration state functions.             *
!                                                                      *
!   Call(s) to: [LIB92]: GETMIX, SETCSL, SETMC, SETCON.               *
!               [HFS92]: CHKPLT, GETHFD, HFSMPI, SETDBG, SETSUM,       *
!                        STRSUM.                                       *
!               [NJGRAF]: FACTT.                                       *
!               [MPI90]: STARTMPI2, STOPMPI2                           *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 24 Dec 1992   *
!   Modified by Gediminas Gaigalas for new spin-angular integration.   *
!                                         Last revision:    Nov 2017   *
!   MPI version created                   Last revision:    Dec 2024   *
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
      USE orb_C
      USE def_C, ONLY: NELEC
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
      INTEGER   :: K, NCI, NCORE_NOT_USED, LENNAME
      INTEGER   :: NCOUNT1
      LOGICAL   :: YES
      CHARACTER :: NAME*24, NAMESAVE*24
      CHARACTER(LEN=128) :: STARTDIR, TMPDIR, PERMDIR
      CHARACTER(LEN=80)  :: MSG
      CHARACTER(LEN=3)   :: IDSTRING
!-----------------------------------------------
!=======================================================================
!  Start mpi --- get processor info: myid, nprocs, host name; and print
!=======================================================================

      CALL startmpi2 (myid, nprocs, host, lenhost, ncount1,       &
                           startdir, permdir, tmpdir, 'RHFS_MPI')
      WRITE (idstring, '(I3.3)') myid

!=======================================================================
!  Get default settings on node-0 and then send to all nodes
!=======================================================================

      IF (myid .EQ. 0) THEN
         WRITE (ISTDE, *)
         WRITE (ISTDE, *) 'RHFS - MPI Version'
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

!=======================================================================
!  Get name of the state (used in files like <n>.c, <n>.s)
!  info used only on node-0
!=======================================================================

      lenname = 0     ! Otherwise it might cause trouble for myid!=0
      IF (myid .EQ. 0) THEN
   10    CONTINUE
         WRITE (ISTDE, *) 'Name of state'
         read (*, '(A)') NAME
         K = INDEX(NAME,' ')
         IF (K == 1) THEN
            WRITE (ISTDE, *) 'Names may not start with a blank'
            GO TO 10
         ENDIF
         NAMESAVE = NAME
         lenname = LEN_TRIM (NAME)
         
         WRITE (ISTDE, *)
         WRITE (ISTDE, *) 'Mixing coefficients from a CI calc.?'
         YES = GETYN()
         IF (YES) THEN
            NCI = 0
         ELSE
            NCI = 1
         ENDIF
      ENDIF
      
      CALL MPI_Bcast (lenname,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_Bcast (NAME,lenname,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_Bcast (NCI,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

!=======================================================================
!   Check compatibility of plant substitutions (commented out)
!=======================================================================
!GG      CALL CHKPLT

!=======================================================================
!   Determine if there is to be any debug printout; this will be
!   made on the  .dbg  file
!=======================================================================
      CALL SETDBG

!=======================================================================
!   Perform machine- and installation-dependent setup
!=======================================================================
      CALL SETMC

!=======================================================================
!   Set up the physical constants
!=======================================================================
      CALL SETCON

!=======================================================================
!   Open the  .sum  file (only on node 0)
!=======================================================================
      IF (myid .EQ. 0) CALL SETSUM (NAME, NCI)

!=======================================================================
!   Open, check, load data from, and close, the  .csl  file
!   Only master process reads the file, then broadcasts data to all processes
!=======================================================================
      IF (myid .EQ. 0) THEN
         CALL SETCSLA (NAME, NCORE_NOT_USED)
      ENDIF
      
      ! Broadcast the orbital configuration data to all processes
      ! Need to broadcast: NCORE_NOT_USED, NCF, NW, NP, NAK, NKL, NKJ, NH, NELEC
      CALL MPI_Bcast (NCORE_NOT_USED, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (NCF, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (NW, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (NELEC, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      
      IF (myid .NE. 0) THEN
         ! Non-master processes need to allocate arrays based on received dimensions
         ! This ensures all processes have the same orbital data structures
      ENDIF
      
      ! Broadcast orbital data arrays (if NW > 0)
      IF (NW > 0) THEN
         CALL MPI_Bcast (NP, NW, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
         CALL MPI_Bcast (NAK, NW, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
         CALL MPI_Bcast (NKL, NW, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
         CALL MPI_Bcast (NKJ, NW, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
         CALL MPI_Bcast (NH, 2*NW, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
      ENDIF

!=======================================================================
!   Get the remaining information (only master process)
!=======================================================================
      IF (myid .EQ. 0) THEN
         CALL GETHFD (NAME)
      ENDIF

!=======================================================================
!   Get the eigenvectors (only master process) 
!=======================================================================
      IF (myid .EQ. 0) THEN
         CALL GETMIXBLOCK (NAME, NCI)
      ENDIF

!=======================================================================
!   Append a summary of the inputs to the  .sum  file
!=======================================================================
      IF (myid .EQ. 0) CALL STRSUM

!=======================================================================
!   Set up the table of logarithms of factorials
!=======================================================================
      CALL FACTT

!=======================================================================
!   Proceed with the HFS calculation (MPI parallel version)
!=======================================================================
      CALL HFSGGMPI

!=======================================================================
!  Execution finished; Statistics output
!=======================================================================

      CALL stopmpi2 (myid, nprocs, host, lenhost,          &
                           ncount1, 'RHFS_MPI')

!=======================================================================
!   Print completion message (only on node 0)
!=======================================================================
      IF (myid .EQ. 0) THEN
         WRITE (ISTDE, *)
         WRITE (ISTDE, *) 'RHFS: Execution complete.'
      ENDIF

      STOP
      END PROGRAM RHFS90MPI
