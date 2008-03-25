      SUBROUTINE SMMNVR(DATA,N,XMEAN,DELXM,S2,DELS2,S,DELS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DATA(*)
      PARAMETER(ZERO=0.D0, ONE=1.D0, TWO=2.D0)
      XMEAN=ZERO
      XN=DBLE(N)
      XN1=XN-ONE
      DO 10 I =  1, N
        XMEAN=XMEAN+DATA(I)
   10 CONTINUE
      XMEAN=XMEAN/XN
      IF(N.GT.1) THEN
        Q=ZERO
        DO 20 I =  1, N
          Q=Q+(DATA(I)-XMEAN)**2
   20   CONTINUE
        S2=Q/XN1
        S=SQRT(S2)
        DELXM=S/SQRT(XN)
        DELS2=S2*SQRT(TWO/XN1)
        DELS=S/SQRT(TWO*XN1)
      ELSE
        DELXM=ZERO
        S2=ZERO
        S=ZERO
        DELS2=ZERO
        DELS=ZERO
      END IF
      END
