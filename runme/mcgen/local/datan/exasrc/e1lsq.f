      PROGRAM E1LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXNR=4,MAXN=10,N=10)
      DIMENSION T(MAXN),DELTAY(MAXN),CX(MAXNR,MAXNR),A(MAXN,MAXNR)
      DIMENSION Y(MAXN),X(MAXNR),SCRAT(MAXNR,MAXNR)
      LOGICAL OK
      DATA T /-.9D0,-.7D0,-.5D0,-.3D0,-.1D0,
     +.1D0,.3D0,.5D0,.7D0,.9D0/
      DATA Y /81.D0,50.D0,35.D0,27.D0,26.D0,
     +60.D0,106.D0,189.D0,318.D0,520.D0/
C identify program to user
      WRITE(*,*)' Program E1LSQ demonstrates use of LSQPOL'
      WRITE(*,*)' '
C list data
      WRITE(*,*)'   T  ,      Y       DELTAY'
      DO 10 I=1,MAXN
          DELTAY(I)=SQRT(Y(I))
        WRITE(*,'(3F10.5)')T(I),Y(I),DELTAY(I)
10    CONTINUE
      WRITE(*,*)' '
C perform fits with polynomials of different degree
      DO 20 NR=1,MAXNR
        CALL LSQPOL(T,Y,DELTAY,N,NR,X,CX,R,A,SCRAT,OK)
        WRITE(*,'(A,I1,A,F10.5)')' Polynomial fit with NR = ',NR,
     +  ', M = ',R
        WRITE(*,'(A))')' X = '
        CALL MTXWRT(X,1,NR)
        WRITE(*,*)' Covariance matrix CX ='
        CALL MTXWRT(CX,NR,NR)
        WRITE(*,*)' '
20    CONTINUE
      END
