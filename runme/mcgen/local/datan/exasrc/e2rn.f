      PROGRAM E2RN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(NMAX=100)
      DIMENSION T(NMAX),Y(NMAX)
      CHARACTER*1 TX,TY,CAPT
      PARAMETER(TX='t',TY='y',CAPT=' ')
C identify program to user
      WRITE(*,*)' Program E2RN demonstrates use of RNLINE'
      WRITE(*,*)' '
C ask for numerical input
      WRITE(*,*)' Enter number of points N (2 <= N <= 100)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter A'
      WRITE(*,*)' >'
      READ(*,*)A
      WRITE(*,*)' Enter B'
      WRITE(*,*)' >'
      READ(*,*)B
      WRITE(*,*)' Enter T0'
      WRITE(*,*)' >'
      READ(*,*)T0
      WRITE(*,*)' Enter DT'
      WRITE(*,*)' >'
      READ(*,*)DT
      WRITE(*,*)' Enter SIGMAY (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMAY
C simulate data points
      CALL RNLINE(A,B,T0,DT,N,SIGMAY,T,Y)
C ask for type of output
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - Numerical Output'
      WRITE(*,*)' 2 - Graphical Output'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      IF(ISWIT.EQ.1)THEN
C numerical output
        WRITE(*,'(5(A,F7.2))')' A = ',A,', B = ',B,', T0 = ', T0,
     +  ', DT = ', DT,', SIGMAY = ',SIGMAY
        WRITE(*,*)' Pairs T(I), Y(I) produced by RNLINE:'
        DO 10 I=1,N
          WRITE(*,'(2G15.5)')T(I),Y(I)
10      CONTINUE
      ELSE IF(ISWIT.EQ.2)THEN
C graphical output
        CALL GRNBWS()
        WRITE(*,*)' Please, enter number of workstation:'
        WRITE(*,*)' >'
        READ(*,*)NWS
        XA=T0-1.D0
        XB=T0+N*DT
        YA=A*T0+B-SIGN(5.D0,A)*SIGMAY
        YB=A*(T0+(N-1)*DT)+B+SIGN(5.D0,A)*SIGMAY
        CALL SMSDGR(T,Y,N,XA,XB,YA,YB,TX,1,TY,1,CAPT,1,NWS)
      END IF
      END
