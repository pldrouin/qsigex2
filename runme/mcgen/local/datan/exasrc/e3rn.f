      PROGRAM E3RN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(NX=100)
      DIMENSION HIST(NX)
      CHARACTER*1 TX,TY,CAPT
      PARAMETER(TX='t',TY='N',CAPT=' ')
C identify program to user
      WRITE(*,*)' Program E3RN demonstrates use of RNRADI'
      WRITE(*,*)' '
C ask for numerical input
      WRITE(*,*)' Enter number of decays N (>>1)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter fraction A (0. < A < 1.)'
      WRITE(*,*)' >'
      READ(*,*)A
      WRITE(*,*)' Enter TAU1 (>0.)'
      WRITE(*,*)' >'
      READ(*,*)TAU1
      WRITE(*,*)' Enter TAU2 (>0.)'
      WRITE(*,*)' >'
      READ(*,*)TAU2
C ask for type of output
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - Numerical Output'
      WRITE(*,*)' 2 - Graphical Output'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      IF(ISWIT.EQ.1)THEN
C numerical output
        WRITE(*,'(3(A,F6.2))')' A = ',A,', TAU1=',TAU1,
     +  ', TAU2 = ',TAU2
        WRITE(*,*)' Decay time values produced by RNRADI:'
        DO 10 I=1,N
C simulate decay times
          CALL RNRADI(A,TAU1,TAU2,T)
          WRITE(*,'(F12.7)')T
10      CONTINUE
      ELSE IF(ISWIT.EQ.2)THEN
C graphical output
        CALL GRNBWS()
        WRITE(*,*)' Please, enter number of workstation:'
        WRITE(*,*)' >'
        READ(*,*)NWS
C initiate and fill histogram
        X0=0.D0
        DELX=0.03D0*MAX(TAU1,TAU2)
        CALL SMHSIN(HIST,X0,DELX,NX)
        DO 20 I=1,N
C simulate decay times
          CALL RNRADI(A,TAU1,TAU2,T)
          CALL SMHSFL(HIST,X0,DELX,NX,T,1.D0)
20      CONTINUE
C present histogram
        CALL SMHSGR(HIST,X0,DELX,NX,TX,1,TY,1,CAPT,1,NWS)
      END IF
      END
