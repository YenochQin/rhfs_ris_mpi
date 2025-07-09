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
      USE def_C, ONLY: NELEC, ACCY, C, Z, CVAC, EMN, EMPAM, RBCM, AUCM, CCMS, B1
      USE EIGV_C
      USE memory_man
      USE decide_C
      USE foparm_C
      USE nsmdat_C
      USE prnt_C
      USE syma_C
      USE grid_C
      USE wave_C
      USE npar_C
      USE wfac_C
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
      USE cslhmpi_I
      USE lodrwfmpi_I
      USE setrwfmpi_I
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER   :: K, NCI, NCORE_NOT_USED, LENNAME
      INTEGER   :: NCOUNT1
      LOGICAL   :: YES
      CHARACTER :: NAME*24, NAMESAVE*24
      CHARACTER(LEN=8), DIMENSION(50) :: IDBLK
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
      CALL MPI_Bcast (NAME,24,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
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
!   Load configuration data using cslhmpi which handles MPI broadcasting
!   This function properly allocates arrays on all processes before broadcasting
!=======================================================================
      ! cslhmpi expects the base name and adds .c internally
      ! Build the filename with .c extension following RCI90 pattern
      IF (lenname >= 2 .AND. NAME(lenname-1:lenname) == '.c') THEN
         CALL cslhmpi (NAME(1:lenname-2) // '.c', NCORE_NOT_USED, 50, IDBLK)
      ELSE
         CALL cslhmpi (NAME(1:lenname) // '.c', NCORE_NOT_USED, 50, IDBLK)
      ENDIF
      
      ! Print summary information on master process
      IF (myid .EQ. 0) THEN
         WRITE (ISTDE, *) 'There are', NW, 'relativistic subshells;'
         WRITE (ISTDE, *) 'There are', NCF, 'relativistic CSFs;'
         WRITE (ISTDE, *) '... load complete;'
      ENDIF

!=======================================================================
!   Get the remaining information (only master process)
!=======================================================================
      IF (myid .EQ. 0) THEN
         CALL GETHFD (NAME)
      ENDIF
      
!=======================================================================
!   Broadcast data from GETHFD to all processes
!=======================================================================
      ! Broadcast control variables
      CALL MPI_Bcast (LFORDR, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (ICCUT, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      
      ! Broadcast nuclear data
      CALL MPI_Bcast (SQN, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (DMOMNM, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (QMOMB, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (HFSI, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (HFSD, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (HFSQ, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      
      ! Broadcast nuclear parameter data
      CALL MPI_Bcast (NPARM, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      IF (NPARM > 0) THEN
         CALL MPI_Bcast (PARM(1), NPARM, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      ENDIF
      
      ! Broadcast wave factor data
      CALL MPI_Bcast (WFACT, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      
      ! Note: Physical constants are now available through def_C module import
      
      ! Broadcast grid data (from RADGRD called in GETHFD)
      CALL MPI_Bcast (N, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (RNT, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (H, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (HP, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      IF (N > 0) THEN
         ! Grid arrays are statically allocated in grid_C module, just broadcast data
         CALL MPI_Bcast (R(1), N, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
         CALL MPI_Bcast (RP(1), N, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
         CALL MPI_Bcast (RPOR(1), N, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      ENDIF
      
!=======================================================================
!   Load wavefunction data using MPI-aware function
!=======================================================================
      ! setrwfmpi opens .w file and calls lodrwfmpi internally
      ! Check if filename ends with .c extension and build .w filename
      IF (lenname >= 2 .AND. NAME(lenname-1:lenname) == '.c') THEN
         CALL setrwfmpi (NAME(1:lenname-2) // '.w')
      ELSE
         CALL setrwfmpi (NAME(1:lenname) // '.w')
      ENDIF
      
      IF (myid .EQ. 0) WRITE (ISTDE, *) 'Wavefunction data loaded successfully'

!=======================================================================
!   Get the eigenvectors (only master process) 
!=======================================================================
      IF (myid .EQ. 0) THEN
         CALL GETMIXBLOCK (NAME, NCI)
      ENDIF

!=======================================================================
!   Broadcast eigenvector data from master process to all other processes
!=======================================================================
      ! Broadcast basic dimensions first
      CALL MPI_Bcast (NVEC, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      IF (myid .EQ. 0) WRITE (6, *) 'DEBUG: NVEC = ', NVEC
      
      ! Check for valid NVEC across all processes
      IF (NVEC <= 0) THEN
         IF (myid .EQ. 0) WRITE (ISTDE, *) 'Error: Invalid NVEC = ', NVEC
         CALL MPI_FINALIZE(ierr)
         STOP
      ENDIF
      
      ! Allocate arrays on non-master processes
      IF (myid .NE. 0) THEN
         CALL ALLOC (EVAL, NVEC, 'EVAL', 'RHFS90MPI')
         CALL ALLOC (EVEC, NCF*NVEC, 'EVEC', 'RHFS90MPI')
         CALL ALLOC (IVEC, NVEC, 'IVEC', 'RHFS90MPI')
         CALL ALLOC (IATJPO, NVEC, 'IATJPO', 'RHFS90MPI')
         CALL ALLOC (IASPAR, NVEC, 'IASPAR', 'RHFS90MPI')
      ENDIF
      
      ! Broadcast the arrays with proper error checking
      CALL MPI_Bcast (EVAL(1), NVEC, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      IF (ierr .NE. 0) THEN
         WRITE (ISTDE, *) 'MPI_Bcast failed for EVAL, ierr =', ierr
         CALL MPI_FINALIZE(ierr)
         STOP
      ENDIF
      
      CALL MPI_Bcast (EVEC(1), NCF*NVEC, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
      IF (ierr .NE. 0) THEN
         WRITE (ISTDE, *) 'MPI_Bcast failed for EVEC, ierr =', ierr
         CALL MPI_FINALIZE(ierr)
         STOP
      ENDIF
      
      CALL MPI_Bcast (IVEC(1), NVEC, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (IATJPO(1), NVEC, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      CALL MPI_Bcast (IASPAR(1), NVEC, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      
      ! Synchronize all processes after data distribution
      CALL MPI_Barrier (MPI_COMM_WORLD, ierr)

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
      IF (NVEC <= 0) THEN
         IF (myid .EQ. 0) WRITE (ISTDE, *) 'Error: NVEC = ', NVEC
         CALL MPI_FINALIZE(ierr)
         STOP
      ENDIF
      
      IF (myid .EQ. 0) WRITE (ISTDE, *) 'Starting parallel HFS calculation with', nprocs, 'processes'
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
