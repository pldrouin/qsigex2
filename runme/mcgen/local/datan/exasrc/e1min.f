      PROGRAM E1MIN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(N=3)
      COMMON /FUNTYP/ ITYPE
      DIMENSION X(N),XIN(N),SCRAT(N,N+1),LIST(N)
      EXTERNAL FUNCT
      DOUBLE PRECISION FUNCT
      PARAMETER(ZERO=0.D0)
C identify program to user
      WRITE(*,*)' Program E1MIN demonstrates use of'
      WRITE(*,*)' MINSIM,MINPOW,MINCJG,MINQDR,MINMAR'
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
C ask for type of function
      WRITE(*,*)' Choose function to be minimized'
      WRITE(*,*)' 1 - f=r**2=x1**2+x2**2+x3**2'
      WRITE(*,*)' 2 - f=r**10'
      WRITE(*,*)' 3 - f=r'
      WRITE(*,*)' 4 - f=-exp(-r**2)'
      WRITE(*,*)' 5 - f=r**6-2*r**4+r**2'
      WRITE(*,*)' 6 - f=r**2*exp(-r**2)'
      WRITE(*,*)' 7 - f=-exp(-r**2)-10*exp(-ra**2)'
      WRITE(*,*)'       ra**2=(x1-3)**2+(x2-3)**2+(x3-3)**2'
      WRITE(*,*)' >'
      READ(*,*)ITYPE
C ask for initial values
      DO 1 I=1,N
        WRITE(*,'(A,I2,A)')' Enter initial value of X(',I,')'
        WRITE(*,*)' >'
        READ(*,*)XIN(I)
1     CONTINUE
C loop over number of unfixed variables
      DO 20 NVAR=N,0,-1
C set initial values of variables and set LIST
        CALL MTXCPV(XIN,X,N)
        NRED=NVAR
        NSTEP=1000
        FMIN=0.D0
        DO 10 I=1,N
          IF(I.LE.NRED) THEN
            LIST(I)=1
          ELSE
            LIST(I)=0
          END IF
10      CONTINUE
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
20    CONTINUE
1000  FORMAT(' N = ',I2,' , NRED = ',I2,' , LIST = ',3I2)
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
      IF(ITYPE.EQ.1) THEN
        FUNCT=R2
      ELSE IF(ITYPE.EQ.2) THEN
        FUNCT=R2**5
      ELSE IF(ITYPE.EQ.3) THEN
        FUNCT=SQRT(ABS(R2))
      ELSE IF(ITYPE.EQ.4) THEN
        IF(R2.GT.BIG) THEN
          FUNCT=ZERO
        ELSE
          FUNCT=-EXP(-R2)
        END IF
      ELSE IF(ITYPE.EQ.5) THEN
        FUNCT=R2**3-2.D0*R2**2+R2
      ELSE IF(ITYPE.EQ.6) THEN
        IF(R2.GT.BIG) THEN
          FUNCT=ZERO
        ELSE
          FUNCT=R2*EXP(-R2)
        END IF
      ELSE IF(ITYPE.EQ.7) THEN
        IF(R2.GT.BIG) THEN
          FUNCT=ZERO
        ELSE
          RA2=(X(1)-A(1))**2+(X(2)-A(2))**2+(X(3)-A(3))**2
          FUNCT=-EXP(-R2)-FACT*EXP(-RA2)
        END IF
      END IF
      END
