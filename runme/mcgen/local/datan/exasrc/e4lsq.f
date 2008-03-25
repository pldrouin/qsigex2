      PROGRAM E4LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MAXN=50,MAXNR=9,N=50,NR=9)
      DIMENSION T(MAXN),Y(MAXN),DELTAY(MAXN),LIST(MAXNR),
     +X(MAXNR),CX(MAXNR,MAXNR),GX(MAXNR,MAXNR),A(MAXN,MAXNR)
      DOUBLE PRECISION LSQP2G
      EXTERNAL LSQP2G
C identify program to user
      WRITE(*,*)' Program E4LSQ demonstrates use of LSQMAR'
      WRITE(*,*)' '
C create simulated data
      X(1)=5.D0
      X(2)=-0.005D0
      X(3)=-0.001D0
      X(4)=10.D0
      X(5)=10.D0
      X(6)=2.D0
      X(7)=10.D0
      X(8)=30.D0
      X(9)=2.D0
      NSEED1=15
      NSEED2=135
      CALL RNE2IN(NSEED1,NSEED2)
      CALL RNSTNR(DELTAY,N)
      CALL RNECUY(Y,N)
      DELT=1.D0
      SIGMA=.4D0
      WRITE(*,*)'   T         Y       DELTAY'
      DO 10 I=1,N
        T(I)=DBLE(I)*DELT
        SIG=SIGMA*(DBLE(0.5)+Y(I))
        Y(I)=LSQP2G(X,NR,T(I))+DELTAY(I)*SIG
        DELTAY(I)=SIG
        WRITE(*,'(3F10.5)')T(I),Y(I),DELTAY(I)
10    CONTINUE
      WRITE(*,*)' '
C set first approximation of unknowns and perform fit with LSQMAR
      NRED=NR
      X(1)=0.D0
      X(2)=0.D0
      X(3)=0.D0
      X(4)=8.D0
      X(5)=12.D0
      X(6)=3.D0
      X(7)=12.D0
      X(8)=28.D0
      X(9)=1.5D0
      DO 20 I=1,NR
        LIST(I)=1
20    CONTINUE
      WRITE(*,'(A,I2,A,I2,A,9I2,A,/,A,9F8.3)')' NR =',NR,
     +' , NRED = ', NRED,' , LIST = ',LIST,', first appr.:',
     +' X = ',X
      NSTEP=100
      CALL LSQMAR(LSQP2G,T,Y,DELTAY,N,NR,NRED,
     +LIST,X,CX,R,A,GX,NSTEP)
C output results
      WRITE(*,*)' Result of fit with LSQMAR:'
      WRITE(*,'(A,F10.5,A,I5)')' Fit to Gaussian, M = ',R,
     +' , NSTEP = ',NSTEP
      WRITE(*,'(A,(9F8.3))')' X = ',(X(K),K=1,NR)
      END
C----------------------------------------------------------
      SUBROUTINE AUXDRI(DUMMY,X,T,N,NR,NRED,LIST,A,OK)
C analytical derivatives of 2 Gaussians on polynomial background
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(NR),T(N),A(N,NRED),LIST(NR)
      PARAMETER(BIG=100.D0,TWO=2.D0,ZERO=0.D0)
      LOGICAL OK
      OK=.TRUE.
      DO 100 I=1,N
        ARG1=(T(I)-X(5))**2/(TWO*X(6)**2)
        IF(ARG1.GT.BIG) THEN
          G1=ZERO
        ELSE
          G1=EXP(-ARG1)
        END IF
        ARG2=(T(I)-X(8))**2/(TWO*X(9)**2)
        IF(ARG2.GT.BIG) THEN
          G2=ZERO
        ELSE
          G2=EXP(-ARG2)
        END IF
        IX=0
        DO 50 J=1,NR
          IF(LIST(J).NE.0) THEN
            IX=IX+1
            IF(J.EQ.1) THEN
              A(I,IX)=1.D0
            ELSE IF(J.EQ.2) THEN
              A(I,IX)=T(I)
            ELSE IF(J.EQ.3) THEN
              A(I,IX)=T(I)**2
            ELSE IF(J.EQ.4) THEN
              A(I,IX)=G1
            ELSE IF(J.EQ.5) THEN
              A(I,IX)=X(4)*G1*(T(I)-X(5))/X(6)**2
            ELSE IF(J.EQ.6) THEN
              A(I,IX)=X(4)*G1*(T(I)-X(5))**2/X(6)**3
            ELSE IF(J.EQ.7) THEN
              A(I,IX)=G2
            ELSE IF(J.EQ.8) THEN
              A(I,IX)=X(7)*G2*(T(I)-X(8))/X(9)**2
            ELSE IF(J.EQ.9) THEN
              A(I,IX)=X(7)*G2*(T(I)-X(8))**2/X(9)**3
            END IF
          END IF
50      CONTINUE
100   CONTINUE
      END
