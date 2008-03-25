      PROGRAM S1ML
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C Likelihood estimate for mean life in radioactive decay
      DIMENSION T(1000),HIST(100)
      CHARACTER*10 TX,TY,CAPT
      PARAMETER(FACT=.39894228D0)
      DATA TX/'t^^"-'/,TY/'N(t^^"-#)'/,CAPT/' '/
C identify program to user
      WRITE(*,*)' Program S1ML plots likelihood estimate TBAR'
      WRITE(*,*)' for mean life in radioactive decay'
      WRITE(*,*)' determined in several experiments'
      WRITE(*,'(A,/)')' with few decays each'
C ask for input
      WRITE(*,*)' Enter number NEXP of experiments (>>1)'
      WRITE(*,*)' >'
      READ(*,*)NEXP
      WRITE(*,*)' Enter number N of decays (1 <= N <= 1000)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - Histogram of TBAR'
      WRITE(*,*)' 2 - Cumulative frequency distribution of TBAR'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C initialize histogram
      X0=0.D0
      DELX=0.05D0
      NX=100
      CALL SMHSIN(HIST,X0,DELX,NX)
C loop over all experiments
      DO 100 IEXP=1,NEXP
C generate sample and draw 1D scatter diagram
        CALL RNECUY(T,N)
        TBAR=0.
        DO 2 I=1,N
          T(I)=-LOG(T(I))
          TBAR=TBAR+T(I)
2       CONTINUE
        TBAR=TBAR/DBLE(N)
        CALL SMHSFL(HIST,X0,DELX,NX,TBAR,1.D0)
100   CONTINUE
      IF(ISWIT.EQ.1) THEN
        TY='N(t^^"-#)'
        CALL SMHSGR(HIST,X0,DELX,NX,TX,5,TY,9,CAPT,1,NWS)
      ELSE
        F=1.D0/DBLE(NEXP)
        HLAST=0.D0
        DO 200 I=1,NX
          HLAST=HLAST+HIST(I)*F
          HIST(I)=HLAST
200     CONTINUE
        TY='F(t^^"-#)'
        CALL SMHSGR(HIST,X0,DELX,NX,TX,5,TY,9,CAPT,1,NWS)
      END IF
      END
