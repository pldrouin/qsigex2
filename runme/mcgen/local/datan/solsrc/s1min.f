      PROGRAM S1MIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(N=3)
      COMMON /FUNTYP/ ITYPE
      DIMENSION X(N),XIN(N),SCRAT(N,N+1),LIST(N)
      EXTERNAL FUNCT
      PARAMETER(ZERO=0.D0,BIG=1.D30)
C identify program to user
      WRITE(*,*)' Program S1MIN determines first approximation'
      WRITE(*,*)' of minimum position by Monte Carlo trials'
      WRITE(*,*)' '
C ask for minimization method
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - minimization by MINSIM'
      WRITE(*,*)' 2 - minimization by MINPOW'
      WRITE(*,*)' 3 - minimization by MINCJG'
      WRITE(*,*)' 4 - minimization by MINQDR'
      WRITE(*,*)' 5 - minimization by MINMAR'
      WRITE(*,*)' >'
      READ(*,*)METHOD
C ask for number of trial points for first approximation
      WRITE(*,*)' Enter number NPT of trial points'
      WRITE(*,*)' for first approximation'
      WRITE(*,*)' >'
      READ(*,*)NPT
C look for point with lowest function value among trial points
      FMIN=BIG
      DO 5 I=1,NPT
        CALL RNECUY(X,N)
        DO 3 K=1,N
          X(K)=-10.D0+X(K)*20.D0
3       CONTINUE
        F=FUNCT(X,N)
        IF(F.LT.FMIN) THEN
          FMIN=F
          CALL MTXCPV(X,XIN,N)
        END IF
5     CONTINUE
C set initial values of variables and set LIST
      CALL MTXCPV(XIN,X,N)
      NRED=N
      NSTEP=1000
      FMIN=0.D0
      DO 10 I=1,N
        IF(I.LE.NRED) THEN
          LIST(I)=1
        ELSE
          LIST(I)=0
        END IF
10    CONTINUE
C write out initial conditions
      WRITE(*,1000)N,NRED,LIST
      WRITE(*,'(A,3F10.5)')' first approx. at X = ',X
C perform minimization
      IF(METHOD.EQ.1) THEN
        WRITE(*,*)' minimization by MINSIM'
        CALL MINSIM(X,N,NRED,LIST,FUNCT,FMIN,ZERO,NSTEP,SCRAT)
      ELSE IF(METHOD.EQ.2) THEN
        WRITE(*,*)' minimization by MINPOW'
        CALL MINPOW(X,N,NRED,LIST,FUNCT,FMIN,ZERO,NSTEP,SCRAT)
      ELSE IF(METHOD.EQ.3) THEN
        WRITE(*,*)' minimization by MINCJG'
        CALL MINCJG(X,N,NRED,LIST,FUNCT,FMIN,ZERO,NSTEP)
      ELSE IF(METHOD.EQ.4) THEN
        WRITE(*,*)' minimization by MINQDR'
        CALL MINQDR(X,N,NRED,LIST,FUNCT,FMIN,ZERO,NSTEP,SCRAT)
      ELSE IF(METHOD.EQ.5) THEN
        WRITE(*,*)' minimization by MINMAR'
        CALL MINMAR(X,N,NRED,LIST,FUNCT,FMIN,ZERO,NSTEP,SCRAT)
      END IF
C write out results
      WRITE(*,'(A,I5)')' NSTEP = ',NSTEP
      WRITE(*,'(A,3F10.5)')' minimum found at X = ',X
      WRITE(*,'(A,G15.5)')' minimum of function is FMIN = ',FMIN
1000  FORMAT( ' N = ',I2,' , NRED = ',I2,' , LIST = ',3I2)
      END
C----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FUNCT(X,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FUNTYP/ ITYPE
      PARAMETER(ZERO=0.D0,BIG=30.D0,FACT=10.D0)
      DIMENSION X(N),A(3)
      DATA A /3.D0,3.D0,3.D0/
C function to be minimized
      R2=X(1)**2+X(2)**2+X(3)**2
      IF(R2.GT.BIG) THEN
        FUNCT=ZERO
      ELSE
        RA2=(X(1)-A(1))**2+(X(2)-A(2))**2+(X(3)-A(3))**2
        FUNCT=-EXP(-R2)-FACT*EXP(-RA2)
      END IF
      END
