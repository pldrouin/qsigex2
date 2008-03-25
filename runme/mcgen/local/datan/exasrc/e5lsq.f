      PROGRAM E5LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MAXN=20,MAXNR=3,N=20,NR=2)
      DIMENSION T(MAXN),Y(MAXN),DELTAY(MAXN),LIST(MAXNR),
     +X(MAXNR),CX(MAXNR,MAXNR),GX(MAXNR,MAXNR),A(MAXN,MAXNR),
     +DXPLUS(MAXNR),DXMINS(MAXNR)
      DOUBLE PRECISION LSQEXP
      EXTERNAL LSQEXP
C identify program to user
      WRITE(*,*)' Program E5LSQ demonstrates use of LSQASN'
      WRITE(*,*)' '
C create simulated data
      X(1)=10.D0
      X(2)=1.D0
      NSEED1=15
      NSEED2=211
      CALL RNE2IN(NSEED1,NSEED2)
      CALL RNSTNR(DELTAY,N)
      CALL RNECUY(Y,N)
      DELT=DBLE(2./21.)
      SIGMA=2.D0
      WRITE(*,*)'   T         Y       DELTAY'
      DO 10 I=1,N
        T(I)=DBLE(I)*DELT
        SIG=SIGMA*(DBLE(0.5)+Y(I))
        Y(I)=LSQEXP(X,NR,T(I))+DELTAY(I)*SIG
        DELTAY(I)=SIG
        WRITE(*,'(3F10.5)')T(I),Y(I),DELTAY(I)
10    CONTINUE
      WRITE(*,*)' '
C set first approximation of unknowns and perform fit with LSQNON
      NRED=NR
      X(1)=.1D0
      X(2)=.1D0
      LIST(1)=1
      LIST(2)=1
      WRITE(*,'(A,I2,A,I2,A,3I2,A,3F5.1)')' NR =',NR,' , NRED = ',
     *NRED,' , LIST = ',LIST,', first appr. X = ',X
      NSTEP=100
      CALL LSQNON(LSQEXP,T,Y,DELTAY,N,NR,NRED,
     +LIST,X,CX,R,A,GX,NSTEP)
      WRITE(*,*)' Result of fit with LSQNON:'
      WRITE(*,'(A,F10.5,A,I5)')' Fit to Exponential, M = ',R,
     +' , NSTEP = ',NSTEP
      WRITE(*,'(A,(2F10.5))')' X = ',(X(K),K=1,NR)
      WRITE(*,*)' Covariance matrix CX ='
      CALL MTXWRT(CX,NRED,NRED)
      WRITE(*,*)' '
C compute asymmetric errors with LSQASN
      NSTEP=100
      CALL LSQASN(LSQEXP,T,Y,DELTAY,N,NR,NRED,LIST,X,CX,R,0.D0,
     +DXPLUS,DXMINS,A,GX,NSTEP)
      WRITE(*,'(A,(3F10.5))')' DXPLUS = ',(DXPLUS(K),K=1,NRED)
      WRITE(*,'(A,(3F10.5))')' DXMINS = ',(DXMINS(K),K=1,NRED)
      END
