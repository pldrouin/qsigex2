      PROGRAM E4REG
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=100,NRMAX=10,NR=10)
      DIMENSION T(NMAX),Y(NMAX),DELTAY(NMAX),X(NRMAX),CHI2(NRMAX)
      DIMENSION B(NRMAX,NRMAX),A(NMAX,NRMAX)
C identify program to user
      WRITE(*,*)' Program E4REG simulates data points with errors'
      WRITE(*,*)' and performs polynomial regression on them'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number N of data points (10 <= N <= 100)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter SIGMA (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
      WRITE(*,*)
     +' Enter number M (>0) of terms in polynomial for data'
      WRITE(*,*)' >'
      READ(*,*)M
      DO 2 I=1,M
        WRITE(*,'(A,I3,A)')' Enter X(',I,' )'
        WRITE(*,*)' >'
        READ(*,*)X(I)
2     CONTINUE
      T0=-1.D0
      DT=2.D0/DBLE(N-1)
C store random numbers following standardized normal in Y
      CALL RNSTNR(Y,N)
C loop over N data points
      DO 10 I=1,N
        T(I)=T0+(I-1)*DT
        DAT=X(1)
        IF(M.GT.1) THEN
          DO 5 J=2,M
            DAT=DAT+X(J)*T(I)**(J-1)
5         CONTINUE
        END IF
C now DAT is exactly given by polynomial.
C add normal error of width SIGMA and store result in Y
        Y(I)=DAT+Y(I)*SIGMA
C set error of data point equal to SIGMA
        DELTAY(I)=SIGMA
10    CONTINUE
C perform polynomial regression
      CALL REGPOL(T,Y,DELTAY,N,NR,X,B,A,CHI2)
C output results
      WRITE(*,'(A)')' T '
      CALL MTXWRT(T,1,N)
      WRITE(*,'(A)')' Y'
      CALL MTXWRT(Y,1,N)
      WRITE(*,'(A)')' DELTAY'
      CALL MTXWRT(DELTAY,1,N)
      WRITE(*,'(A)')' X'
      CALL MTXWRT(X,1,NR)
      WRITE(*,'(A)')' CHI2'
      CALL MTXWRT(CHI2,1,NR)
      END
