      SUBROUTINE AUXHES(X,HESSE,N,NRED,LIST,FUNC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(DELTA=1.D-5,CUT=1.D-9)
      DIMENSION X(*),HESSE(NRED,*),LIST(*)
      EXTERNAL FUNC
      IF(NRED.GT.0) THEN
        IL=0
        DO 20 I=1,N
          KL=0
          IF(LIST(I).EQ. 1.OR. N.EQ.NRED) THEN
            IL=IL+1
            DO 10 K=1,N
              IF(LIST(K).EQ. 1.OR. NRED.EQ.N) THEN
                KL=KL+1
                IF(KL.LT.IL) THEN
                  HESSE(IL,KL)=HESSE(KL,IL)
                ELSE
                  E=ABS(X(I))
                  IF(E.LT.CUT) E=CUT
                  DELI=DELTA*E
                  SAVI=X(I)
                  IF (K.EQ.I) THEN
                    F=FUNC(X,N)
                    X(I)=X(I)+DELI
                    FP=FUNC(X,N)
                    X(I)=SAVI-DELI
                    FM=FUNC(X,N)
                    X(I)=SAVI
                    HESSE(IL,IL)=((FP-F)/DELI-(F-FM)/DELI)/DELI
                  ELSE
                    E=ABS(X(K))
                    IF(E.LT.CUT) E=CUT
                    DELK=DELTA*E
                    SAVK=X(K)
                    X(K)=SAVK+DELK
                    X(I)=SAVI+DELI
                    FP=FUNC(X,N)
                    X(I)=SAVI-DELI
                    FM=FUNC(X,N)
                    DFDXIP=(FP-FM)/(DELI+DELI)
                    X(K)=SAVK-DELK
                    X(I)=SAVI+DELI
                    FP=FUNC(X,N)
                    X(I)=SAVI-DELI
                    FM=FUNC(X,N)
                    X(I)=SAVI
                    X(K)=SAVK
                    DFDXIM=(FP-FM)/(DELI+DELI)
                    HESSE(IL,KL)=(DFDXIP-DFDXIM)/(DELK+DELK)
                  END IF
                END IF
              END IF
   10       CONTINUE
          END IF
   20   CONTINUE
      END IF
      END
