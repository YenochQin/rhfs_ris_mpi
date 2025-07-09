!***********************************************************************
      SUBROUTINE LODRWFmpi (ierror)
!     IMPLICIT REAL*8          (A-H, O-Z)

!   This subroutine loads radial wavefunctions from the .rwf  file
!   and performs some related setup. It does not handle any error
!   so the caller has to do it.
!       ierror=0, normal
!       ierror=1, error
!
!   Used by rcimpivu, rscfmpi, rcimpi
!
!   Call(s) to: [LIB92]: ALLOC, DALLOC, INTRPQ, ORTHSC.                *
!                                                                      *
!   Written by Farid A. Parpia            Last revision: 05 Oct 1992   *
!   MPI version by Xinghong He            Last revision: 27 May 1997   *
!
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:04:58   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE vast_kind_param, ONLY:  DOUBLE
      USE parameter_def,   ONLY:  NNNP
      USE memory_man
      USE DEBUG_C
      USE DEF_C, ONLY: C, Z
      USE GRID_C
      USE NPAR_C
      USE ORB_C
      USE WAVE_C, ONLY: PZ, PF, QF
      USE MPI_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      USE intrpq_I
      USE orthsc_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER, INTENT(OUT) :: ierror
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: J, I, K, NWIN, IOS, NPY, NAKY, MY
!     integer :: ierr
      REAL(DOUBLE) :: CON, FKK, EY, PZY, DNORM
      real(double), dimension(:), pointer :: PA, QA, RA
!-----------------------------------------------
!
!-----------------------------------------------------------------------
!
!   Allocate storage to orbital arrays
!
      IF (myid .EQ. 0) WRITE (6, *) 'LODRWFMPI: Function entered'
      CALL ALLOC(PF, NNNP, NW, 'PF', 'LODRWFMPI')
      CALL ALLOC(QF, NNNP, NW, 'QF', 'LODRWFMPI')

!   Setup: (1) Orbital arrays to zero
!          (2) Array E to -1 (no orbitals estimated)
!          (3) Parameters GAMMA for each orbital
!
      CON = Z / C
      CON = CON * CON

      DO J = 1, NW

         DO I = 1, N
            PF(I,J) = 0.D0
            QF(I,J) = 0.D0
         ENDDO

         E(J) = -1.D0
         K = ABS (NAK(J))
         IF (NPARM .GT. 0) THEN
            GAMA(J) = DBLE (K)
         ELSEIF (NPARM .EQ. 0) THEN
            FKK = DBLE (K*K)
            IF (FKK .GE. CON) THEN
               GAMA(J) = SQRT (FKK-CON)
            ELSE
               !WRITE (istde,*) 'LODRWF: Imaginary gamma parameter'
               !WRITE (istde,*) ' for ',NP(J),NH(J),' orbital; the'
               !WRITE (istde,*) ' point model for the nucleus'
               !WRITE (istde,*) ' is inappropriate for Z > ',C,'.'
               CALL stopmpi ('lodrwfmpi: Inappropriate gamma', myid)
            ENDIF
         ENDIF
      ENDDO
!
!   Read orbital information from Read Orbitals File; write summary
!   to  .dbg  file if option set
!
      IF (LDBPR(3) .AND. myid .EQ. 0) WRITE (99,300)
      NWIN = 0
      
      ! Process 0 reads entire .w file first
      IF (myid .EQ. 0) THEN
         DO WHILE (.TRUE.)
            READ (23,IOSTAT = IOS) NPY,NAKY,EY,MY
            IF (IOS .NE. 0) EXIT
            
            ! Allocate temporary arrays
            CALL ALLOC (PA,MY, 'PA', 'LODRWFMPI')
            CALL ALLOC (QA,MY, 'QA', 'LODRWFMPI')
            CALL ALLOC (RA,MY, 'RA', 'LODRWFMPI')
            
            ! Read orbital data
            READ (23) PZY,(PA(I),I = 1,MY),(QA(I),I =1 ,MY)
            read (23) (RA(I),I = 1,MY)
            
            ! Process orbital data
            DO J = 1, NW
               IF (.NOT.(E(J)<0.0D00 .AND. NPY==NP(J) .AND. NAKY==NAK(J))) CYCLE
                  PZ(J) = PZY
                  E(J) = EY
                  CALL INTRPQ (PA, QA, MY, RA, J, DNORM)
                  IF (LDBPR(3)) WRITE (99,301) NP(J), NH(J), E(J), DNORM
                  NWIN = NWIN + 1
                  WRITE (6, *) 'Match found J=', J, 'NWIN=', NWIN
            ENDDO
            
            ! Deallocate temporary arrays
            CALL DALLOC (PA, 'PA', 'LODRWFMPI')
            CALL DALLOC (QA, 'QA', 'LODRWFMPI')
            CALL DALLOC (RA, 'RA', 'LODRWFMPI')
         ENDDO
         WRITE (6, *) 'Process 0 finished reading .w file, NWIN=', NWIN
      ENDIF
      
      ! Broadcast the results to all processes
      CALL MPI_Bcast (NWIN,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
      CALL MPI_Bcast (PZ,NW,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
      CALL MPI_Bcast (E,NW,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
      CALL MPI_Bcast (PF,N*NW,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
      CALL MPI_Bcast (QF,N*NW,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
      
      IF (myid .NE. 0) THEN
         WRITE (6, *) 'Process', myid, 'received NWIN=', NWIN
      ENDIF
      IF (LDBPR(3) .AND. myid .EQ. 0) &
            WRITE (99,*) ' orbitals renormalised;'
!
!   Return with an error code if all orbitals are not known
!
      IF (NWIN .LT. NW) THEN
         IF (myid .EQ. 0) THEN
            WRITE (6, *) 'LODRWFMPI: Not all orbitals loaded'
            WRITE (6, *) 'NWIN =', NWIN, 'NW =', NW
         ENDIF
         ierror = 1
         RETURN
      ENDIF
!
!   Schmidt orthogonalise the orbitals
!
      CALL ORTHSC

      IF (LDBPR(3) .AND. myid .EQ. 0) &
            WRITE (99,*) 'orbitals orthogonalised and renormalised;'
      ierror = 0
      IF (myid .EQ. 0) WRITE (6, *) 'LODRWFMPI: Function completed successfully'

  300 FORMAT (/'From SUBROUTINE LODRWFmpi:' &
              /' Orbital',8X,'Eigenvalue',19X,'Norm')
  301 FORMAT (2X,I2,A2,4X,1P,1D22.15,4X,1D22.15)

      RETURN
      END SUBROUTINE LODRWFmpi
