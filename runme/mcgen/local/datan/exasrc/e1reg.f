      PROGRAM E1REG
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(N=10,NR=10)
      DIMENSION T(N),Y(N),DELTAY(N),X(NR),CHI2(NR)
      DIMENSION B(NR,NR),A(N,NR)
      PARAMETER (ZERO=0.D0,ONE=1.D0)
      DATA T
     +/-.9D0,-.7D0,-.5D0,-.3D0,-.1D0,.1D0,.3D0,.5D0,.7D0,.9D0/
      DATA Y /81.D0,50.D0,35.D0,27.D0,26.D0,
     +60.D0,106.D0,189.D0,318.D0,520.D0/
C identify program to user
      WRITE(*,*)' Program E1REG demonstrates use of REGPOL'
      WRITE(*,*)' '
C set errors
      DO 10 I=1,N
          DELTAY(I)=SQRT(Y(I))
10    CONTINUE
C perform polynomial regression
      CALL REGPOL(T,Y,DELTAY,N,NR,X,B,A,CHI2)
C output
      WRITE(*,*)' T '
      WRITE(*,'(10F7.2)')T
      WRITE(*,*)' Y'
      WRITE(*,'(10F7.2)')Y
      WRITE(*,*)' DELTAY'
      WRITE(*,'(10F7.2)')DELTAY
      WRITE(*,*)' X'
      WRITE(*,'(10F7.2)')X
      WRITE(*,*)' CHI2'
      WRITE(*,'(10F7.2)')CHI2
      END
