      SUBROUTINE MTXMAR(A,B,ALAM,X1,X2,M,N,FRAC,OK)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=1000)
      DIMENSION A(M,N),B(M,1),X1(N),X2(N),D(NMAX),E(NMAX)
      LOGICAL OK
      COMMON /DASV01/ D,E
      PARAMETER(NB=1)
C STEP 1: Bidiagonalisation of A
      CALL MTXSV1(A,B,D,E,M,N,NB)
C STEP 2: Diagonalisation of bidiagonal matrix
      CALL MTXSV2(A,B,D,E,M,N,NB,OK)
C STEP 3: Order singular values and perform  permutations
      CALL MTXSV3(A,B,D,M,N,NB)
C STEP 4: Singular value analysis and appl. of Marquardt method
      CALL MTXSVM(A,B,D,ALAM,X1,X2,M,N,FRAC)
      END
C-----------------------------------------------------------------
      SUBROUTINE MTXSVM(A,B,D,ALAM,X1,X2,M,N,FRAC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=1000)
      DIMENSION A(M,N),B(M),D(N),X1(N),X2(N),P1(NMAX),P2(NMAX)
      COMMON /DASV00/ P1,P2
      PARAMETER (EPSILN=1.D-15)
      FRACT=ABS(FRAC)
      IF(FRACT.LT.EPSILN) FRACT=EPSILN
      SINMAX=DBLE(0.)
      ALAM2=ALAM**2
      ALAMP=ALAM*DBLE(0.1)
      ALAMP2=ALAMP**2
      DO 10 I =  1, N
        SINMAX=MAX(SINMAX,D(I))
   10 CONTINUE
      SINMIN=SINMAX*FRACT
      KK=N
      DO 20 I=1,N
        IF(D(I).LE.SINMIN) THEN
          KK=I-1
          GO TO 30
        END IF
   20 CONTINUE
   30 CONTINUE
      DO 40 I =  1, M
        G=B(I)
        IF(I.LE.KK) THEN
          DEN1=1./(D(I)**2+ALAM2)
          DEN2=1./(D(I)**2+ALAMP2)
          P1(I)=G*D(I)*DEN1
          P2(I)=G*D(I)*DEN2
        ELSE
          IF(I.LE.N) THEN
            P1(I)=0.
            P2(I)=0.
          END IF
        END IF
   40 CONTINUE
      DO 60 I=1,N
        X1(I)=0.
        X2(I)=0.
        DO 50 K=1,N
          X1(I)=X1(I)+A(I,K)*P1(K)
          X2(I)=X2(I)+A(I,K)*P2(K)
   50   CONTINUE
   60 CONTINUE
      N=KK
      END
