      DOUBLE PRECISION FUNCTION SDBINM(K,N,P)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(ZERO=0.D0,ONE=1.D0,SMALL=.0001D0,BIG=100D0)
      AK=DBLE(K)
      AN=DBLE(N)
      IF(N.LT.1 .OR. K.LT.0 .OR. K.GT.N) THEN
        SDBINM=ZERO
      ELSE
        ALBINC=GLNGAM(AN+ONE)-GLNGAM(AK+ONE)-GLNGAM(AN-AK+ONE)
        IF(P.LE.ZERO) THEN
          IF(K.EQ.0) THEN
            SDBINM=ONE
          ELSE
            SDBINM=ZERO
          END IF
        ELSE IF(P.GE.ONE) THEN
          IF(K.EQ.N) THEN
            SDBINM=ONE
          ELSE
            SDBINM=ZERO
          END IF
        ELSE
          ALFACT=LOG(P)*AK+LOG(ONE-P)*(AN-AK)
          ARG=ALFACT+ALBINC
          ABSARG=ABS(ARG)
          IF(ABSARG.LT.SMALL) THEN
            SDBINM=ONE
          ELSE IF(ABSARG.GT.BIG) THEN
            SDBINM=ZERO
          ELSE
            SDBINM=EXP(ARG)
          END IF
        END IF
      END IF
      END
