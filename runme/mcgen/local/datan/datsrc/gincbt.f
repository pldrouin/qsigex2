      DOUBLE PRECISION FUNCTION GINCBT(AA,BB,XX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL REFLEC
      PARAMETER(EPSILN=1.D-8, ONE=1.D0, ZERO=0.D0, TWO=2.D0)
      XLIM=(AA+ONE)/(AA+BB+ONE)
      IF (XX.LT.XLIM) THEN
        REFLEC=.FALSE.
        A=AA
        B=BB
        X=XX
      ELSE
        REFLEC=.TRUE.
        A=BB
        B=AA
        X=ONE-XX
      END IF
      IF(X.LT.EPSILN) THEN
C function known at end of range
        CF=0.
      ELSE
C continued fraction
        A1=ONE
        B1=ONE
        A2=ONE
        B2=ONE-(A+B)*X/(A+ONE)
        FNORM=ONE/B2
        CF=A2*FNORM
        DO 10 M=1,100
          RM=DBLE(M)
          APL2M=A+TWO*RM
          D2M=RM*(B-RM)*X/((APL2M-ONE)*APL2M)
          D2M1=-(A+RM)*(A+B+RM)*X/(APL2M*(APL2M+1))
          A1=(A2+D2M*A1)*FNORM
          B1=(B2+D2M*B1)*FNORM
          A2=A1+D2M1*A2*FNORM
          B2=B1+D2M1*B2*FNORM
          IF(B2.NE.0.) THEN
C renormalize and test for convergence
            FNORM=ONE/B2
            CFNEW=A2*FNORM
            IF(ABS(CF-CFNEW)/CF .LT. EPSILN) GO TO 20
            CF=CFNEW
          END IF
   10   CONTINUE
   20   CF=CF*(X**A)*((ONE-X)**B)/(A*GBETAF(A,B))
      END IF
      IF(REFLEC) THEN
        GINCBT=ONE-CF
      ELSE
        GINCBT=CF
      END IF
      END
