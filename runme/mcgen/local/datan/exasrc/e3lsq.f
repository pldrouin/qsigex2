      PROGRAM E3LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MAXN=20,MAXNR=3,N=20,NR=3)
      DIMENSION T(MAXN),Y(MAXN),DELTAY(MAXN),LIST(MAXNR),
     +X(MAXNR),CX(MAXNR,MAXNR),GX(MAXNR,MAXNR),A(MAXN,MAXNR)
      DOUBLE PRECISION LSQGSS
      EXTERNAL LSQGSS
C identify program to user
      WRITE(*,*)' Program E3LSQ demonstrates use of LSQNON'
      WRITE(*,*)' '
C create simulated data
      X(1)=1.0D0
      X(2)=1.2D0
      X(3)=0.4D0
      NSEED1=15
      NSEED2=215
      CALL RNE2IN(NSEED1,NSEED2)
      CALL RNSTNR(DELTAY,N)
      CALL RNECUY(Y,N)
      DELT=DBLE(2./21.)
      SIGMA=.1D0
      WRITE(*,*)'   T         Y       DELTAY'
      DO 10 I=1,N
        T(I)=DBLE(I)*DELT
        SIG=SIGMA*(DBLE(0.5)+Y(I))
        Y(I)=LSQGSS(X,NR,T(I))+DELTAY(I)*SIG
        DELTAY(I)=SIG
        WRITE(*,'(3F10.5)')T(I),Y(I),DELTAY(I)
10    CONTINUE
      WRITE(*,*)' '
C loop over different numbers of unfixed variables
      DO 50 NLOOP = 3,0,-1
        NRED=NLOOP
C set first approximation of unknowns and perform fit with LSQNON
        IF(NRED.EQ.3) THEN
          X(1)=.8D0
          X(2)=.8D0
          X(3)=.8D0
          LIST(1)=1
          LIST(2)=1
          LIST(3)=1
        ELSE IF(NRED.EQ.2) THEN
          X(1)=.8D0
          X(2)=1.2D0
          X(3)=.8D0
          LIST(1)=1
          LIST(2)=0
          LIST(3)=1
        ELSE IF(NRED.EQ.1) THEN
          X(1)=.8D0
          X(2)=1.2D0
          X(3)=.4D0
          LIST(1)=1
          LIST(2)=0
          LIST(3)=0
        ELSE IF(NRED.EQ.0) THEN
          X(1)=1.0D0
          X(2)=1.2D0
          X(3)=.4D0
          LIST(1)=0
          LIST(2)=0
          LIST(3)=0
        END IF
        WRITE(*,'(A,I2,A,I2,A,3I2,A,3F5.1)')' NR =',NR,
     +' , NRED = ',NRED,' , LIST = ',LIST,', first appr. X = ',X
        NSTEP=100
        CALL LSQNON(LSQGSS,T,Y,DELTAY,N,NR,NRED,LIST,X,CX,R,A,GX,
     +  NSTEP)
C output results
        WRITE(*,*)' Result of fit with LSQNON:'
        WRITE(*,'(A,F10.5,A,I5)')' Fit to Gaussian, M = ',R,
     +  ' , NSTEP = ',NSTEP
        WRITE(*,'(A,(3F10.5))')' X = ',(X(K),K=1,NR)
        IF(NRED.GT.0) THEN
          WRITE(*,*)' Covariance matrix CX ='
          CALL MTXWRT(CX,NRED,NRED)
          WRITE(*,*)' '
        END IF
50    CONTINUE
      END
