      SUBROUTINE AUXGRD(X,GRD,N,NRED,LIST,FUNC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(DELTA=1.D-11,CUT=1.D-9)
      DIMENSION X(*),GRD(*),LIST(*)
      EXTERNAL FUNC
      IL=0
      IF(NRED.GT.0) THEN
        DO 10 I=1,N
          IF(LIST(I).EQ. 1.OR. NRED.EQ.N) THEN
            IL=IL+1
            ARG=ABS(X(I))
            IF(ARG.LT.CUT) ARG=CUT
            DEL=DELTA*ARG
            SAV=X(I)
            X(I)=SAV+DEL
            FP=FUNC(X,N)
            X(I)=SAV-DEL
            FM=FUNC(X,N)
            X(I)=SAV
            GRD(IL)=(FP-FM)/(DEL+DEL)
          END IF
   10   CONTINUE
      END IF
      END
