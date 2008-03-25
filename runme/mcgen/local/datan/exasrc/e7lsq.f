      PROGRAM E7LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXN=8,MAXNR=2,MAXM=4)
      DIMENSION X(MAXNR),Y(MAXN),CY(MAXN,MAXN)
      DIMENSION GY(MAXN,MAXN),CX(MAXNR,MAXNR)
      DIMENSION F(MAXN+MAXNR,MAXN+MAXNR)
      DIMENSION E(MAXM,MAXN+MAXNR)
      DIMENSION A2(MAXN+MAXNR,MAXN+MAXNR),LIST(2)
      DIMENSION T(4),S(4),DT(4),DS(4),RHO(4)
      DATA T /0.2D0,0.5D0,0.8D0,1.3D0/
      DATA S /0.15D0,0.4D0,0.7D0,0.8D0/
      DATA DT/0.1D0,0.1D0,0.1D0,0.05D0/
      DATA DS/0.05D0,0.1D0,0.1D0,0.1D0/
      DATA RHO/0.D0,0.D0,0.5D0,0.D0/
C identify program to user
      WRITE(*,*)' Program E7LSQ demonstrates use of LSQGEN'
      WRITE(*,*)' '
C write table of data
      WRITE(*,*)'    T         S         DT        DS        RHO'
      DO 5 I=1,4
        WRITE(*,'(5F10.5)')T(I),S(I),DT(I),DS(I),RHO(I)
5     CONTINUE
      WRITE(*,'(/)')
C set up data for input to LSQGEN
      N=8
      M=4
      NR=2
      DO 20 NRED=2,0,-1
        CALL MTXUNT(CY,8)
        DO 10 I=1,4
          Y((I-1)*2+1)=T(I)
          Y((I-1)*2+2)=S(I)
          CY((I-1)*2+1,(I-1)*2+1)=DT(I)**2
          CY((I-1)*2+2,(I-1)*2+2)=DS(I)**2
          CY((I-1)*2+1,(I-1)*2+2)=RHO(I)*DS(I)*DT(I)
          CY((I-1)*2+2,(I-1)*2+1)=RHO(I)*DS(I)*DT(I)
10      CONTINUE
C determine first approximation
        IF(NRED.EQ.2) THEN
          LIST(1)=1
          LIST(2)=1
          X(2)=(S(4)-S(1))/(T(4)-T(1))
          X(1)=S(1)-X(2)*T(1)
        ELSE IF(NRED.EQ.1) THEN
          LIST(1)=0
          LIST(2)=1
          X(1)=.2D0
          X(2)=(S(4)-S(1))/(T(4)-T(1))
        ELSE IF(NRED.EQ.0) THEN
          LIST(1)=0
          LIST(2)=0
          X(1)=0.D0
          X(2)=0.5D0
        END IF
C header for output of results
        WRITE(*,*)' performing fit with LSQGEN'
        WRITE(*,'(3(A,I2),A,2I2)')' N = ',N,', NR = ',NR,
     +  ', NRED = ',NRED,', LIST = ',LIST
        WRITE(*,'(A,2F10.5)')' first approx.: X = ',X
        NSTEP=100
        CALL LSQGEN(Y,CY,GY,F,E,M,N,NR,NRED,LIST,X,CX,R,A2,NSTEP)
C output of results
        WRITE(*,'(A,F6.2,A,I3,/,A,(2F10.5))')
     +  ' result of fit: R = ',R,', NSTEP =',NSTEP,' X =',X
        IF(NRED.GT.0) THEN
          WRITE(*,*)' covariance matrix CX = '
          CALL MTXWRT(CX,NRED,NRED)
        END IF
        WRITE(*,'(/)')
20    CONTINUE
      END
