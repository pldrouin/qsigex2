      DOUBLE PRECISION FUNCTION GGAMMA(X)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION C(6)
      LOGICAL REFLEC
      PARAMETER(RTWOPI=2.506628275D0, PI=3.141592654D0,ONE=1.D0)
      PARAMETER(HALF=0.5D0)
      DATA C/76.18009173D0,-86.50532033D0,24.01409822D0,
     +-1.231739516D0,0.120858003D-2,-0.536382D-5/
      IF(X.GE.ONE) THEN
        REFLEC=.FALSE.
        XX=X-ONE
      ELSE
        REFLEC=.TRUE.
        XX=ONE-X
      END IF
      XH=XX+HALF
      XGH=XX+DBLE(5.5)
      S=ONE
      ANUM=XX
      DO 10 I=1,6
        ANUM=ANUM+ONE
        S=S+C(I)/ANUM
   10 CONTINUE
      S=S*RTWOPI
      G=(XGH**XH)*S/EXP(XGH)
      IF (REFLEC) THEN
        GGAMMA=PI*XX/(G*SIN(PI*XX))
      ELSE
        GGAMMA=G
      END IF
      END
