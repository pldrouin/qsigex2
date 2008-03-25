      SUBROUTINE MTXHSD(V,UP,B,N,LP,L)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION V(N)
      C=ABS(V(LP))
      DO 10 I=L,N
        C=MAX(ABS(V(I)),C)
   10 CONTINUE
      IF(C.LE.0.D0) GO TO 30
      C1=1./C
      SD=(V(LP)*C1)**2
      DO 20 I=L,N
        SD=SD+(V(I)*C1)**2
   20 CONTINUE
      VPPRIM=SD
      VPPRIM=C*SQRT(ABS(VPPRIM))
      IF(V(LP).GT.0.D0) VPPRIM=-VPPRIM
      UP=V(LP)-VPPRIM
      B=1./(VPPRIM*UP)
   30 CONTINUE
      END
