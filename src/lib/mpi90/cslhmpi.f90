!***********************************************************************
!
      SUBROUTINE CSLHMPI(NAME, NCORE, NBLKIN, IDBLK)
!
!  A container which calls setcsll to open, read <name> file to get
!     nblock, ncfblk(), idblk(), ncftot.
!  It then calls lib92/lodcsh to get
!     ncore, nelec, nw, np(), nak(), nkl(), nkj(), nh()
!  The file pointer points to the first CSL record after this routine.
!
!  Called by rcimpivu, mcpmpi
!
!  Xinghong He 98-06-23
!
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:04:58   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE memory_man
      use mpi_C
      USE HBLOCK_C
      USE def_C, ONLY: EMN,IONCTY,NELEC,Z
      USE ORB_C, NCFTOT=>NCF
!
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      USE setcsll_I
      USE lodcsh_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER  :: NCORE
      INTEGER, INTENT(IN)  :: NBLKIN
      CHARACTER  :: NAME*(*)
      CHARACTER  :: IDBLK(*)*8
      CHARACTER*2 NH (*)
      EXTERNAL mpi_bcast_int_s, mpi_bcast_int_a, mpi_bcast_char_a
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: IQADUM
!-----------------------------------------------
!

!     node-0 does exactly the same as the serial code does
      IF (myid .EQ. 0) THEN
        CALL ALLOC   (ncfblk, nblkin+1, 'ncfblk', 'cslhmpi')
        CALL SETCSLL (21, name, nblkin, nblock, ncfblk, ncftot, &
                       idblk)
        CALL RALLOC  (ncfblk, nblock+1, 'ncfblk', 'cslhmpi')
         REWIND (21)
         READ   (21,*)
         !..Load header of <name> file
         CALL LODCSH (21, NCORE)
      ENDIF

! Broadcast results to other nodes. ncfblk should be allocated
! on these nodes (with myid .ne. 0)

      CALL mpi_bcast_int_s (NBLOCK, 0)

      IF (myid .NE. 0) THEN
         CALL ALLOC (NCFBLK, 1+NBLOCK, 'NCFBLK', 'CSLHMPI')
         CALL ALLOC (IDBLK, NBLOCK, 'IDBLK', 'CSLHMPI')
      ENDIF

      CALL mpi_bcast_int_a (NCFBLK, 1+NBLOCK, 0)
      CALL mpi_bcast_char_a (IDBLK, 8*NBLOCK, 0)

      NW = NCFBLK(NBLOCK+1)

      IF (myid .NE. 0) THEN
         CALL ALLOC (NH, NW, 'NH', 'CSLHMPI')
      ENDIF

      CALL mpi_bcast_int_a (NP, NW, 0)
      CALL mpi_bcast_int_a (NAK, NW, 0)
      CALL mpi_bcast_int_a (NKL, NW, 0)
      CALL mpi_bcast_int_a (NKJ, NW, 0)
      CALL mpi_bcast_char_a (NH, 2*NW, 0)

      RETURN
      END SUBROUTINE CSLHMPI
