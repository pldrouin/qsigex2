      DOUBLE PRECISION FUNCTION GINCGM(A,X)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(BIG=500.D0, EPSILN=1.D-6, ONE=1.D0, ZERO=0.D0)
      ALOGGM=GLNGAM(A)
      IF(X.LE.A+ONE) THEN
C series development
        F=ONE/A
        S=F
        ANUM=A
        DO 10 I=1,100
          ANUM=ANUM+ONE
          F=X*F/ANUM
          S=S+F
          IF(F.LT.EPSILN) GO TO 20
   10   CONTINUE
   20   IF(X.LT.EPSILN) THEN
          GINCGM=ZERO
        ELSE
          HELP=A*LOG(X)-X-ALOGGM
          IF(ABS(HELP).GE.BIG) THEN
            GINCGM=ZERO
          ELSE
            GINCGM=S*EXP(HELP)
          END IF
        END IF
      ELSE
C continued fraction
        A0=ZERO
        B0=ONE
        A1=ONE
        B1=X
        CF=ONE
        FNORM=ONE
        DO 30 J=1,100
          A2J=DBLE(J)-A
          A2J1=DBLE(J)
          B2J=ONE
          B2J1=X
          A0=(B2J*A1+A2J*A0)*FNORM
          B0=(B2J*B1+A2J*B0)*FNORM
          A1=B2J1*A0+A2J1*A1*FNORM
          B1=B2J1*B0+A2J1*B1*FNORM
          IF(B1.NE.ZERO) THEN
C renormalize and test for convergence
            FNORM=ONE/B1
            CFNEW=A1*FNORM
            IF(ABS(CF-CFNEW)/CF .LT. EPSILN) GO TO 40
            CF=CFNEW
          END IF
   30   CONTINUE
   40   CONTINUE
        HELP=A*LOG(X)-X-ALOGGM
        IF(ABS(HELP).GE.BIG) THEN
          GINCGM=ONE
        ELSE
          GINCGM=ONE-EXP(HELP)*CFNEW
        END IF
      END IF
      END
