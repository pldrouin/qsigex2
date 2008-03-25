      PROGRAM E2LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXNR=1,MAXN=10,N=10)
      DIMENSION T(MAXN),DELTAY(MAXN),CX(MAXNR,MAXNR),
     +A(MAXN,MAXNR),Y(MAXN),X(MAXNR),SCRAT(MAXNR,MAXNR),C(MAXN)
      LOGICAL OK
      DATA T
     +/-.9D0,-.7D0,-.5D0,-.3D0,-.1D0,.1D0,.3D0,.5D0,.7D0,.9D0/
      DATA Y
     +/-1.6D0,-1.5D0,-.8D0,-.9D0,-.4D0,0.D0,.5D0,1.D0,1.2D0,1.4D0/
      DATA DELTAY
     +/.2D0,.3D0,.1D0,.2D0,.1D0,.2D0,.2D0,.3D0,.2D0,.15D0/
C identify program to user
      WRITE(*,*)' Program E2LSQ demonstrates use of LSQLIN'
      WRITE(*,*)' '
C set up matrix A and vector C
      DO 10 I=1,N
        A(I,1)=-T(I)
        C(I)=-Y(I)
10    CONTINUE
C perform fit to proportionality
      NR=1
      CALL LSQLIN(T,C,DELTAY,N,NR,X,CX,R,A,SCRAT,OK)
C output results
      WRITE(*,*)'   T  ,      Y       DELTAY'
      DO 20 I=1,N
        WRITE(*,'(3F10.5)')T(I),Y(I),DELTAY(I)
20    CONTINUE
      WRITE(*,*)' '
      WRITE(*,'(A,F10.5)')' Fit to proportionality, M = ',R
      WRITE(*,'(A,F10.5)')' X = ',X
      WRITE(*,*)' Covariance  CX ='
      CALL MTXWRT(CX,NR,NR)
      END
