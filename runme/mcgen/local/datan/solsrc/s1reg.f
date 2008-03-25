      PROGRAM S1REG
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=100,NRMAX=10,NR=10,NPL=200)
      DIMENSION T(NMAX),Y(NMAX),DELTAY(NMAX),X(NRMAX),CHI2(NRMAX)
      DIMENSION B(NRMAX,NRMAX),A(NMAX,NRMAX)
      DIMENSION XPL(NPL,NR),YPL(NPL,NR),NCOL(NR)
      DIMENSION DATSX(NMAX),DATCOV(NMAX)
C COMMON /CS1REG/ is needed for Microsoft FORTRAN compiler
      COMMON /CS1REG/ T,Y,DELTAY,X,CHI2,B,A,XPL,YPL,DATSX,DATCOV
      CHARACTER*75 TX,TY,CAPT
      PARAMETER (ZERO=0.D0,ONE=1.D0)
C identify program to user
      WRITE(*,*)' Program S1REG simulates data points with errors,'
      WRITE(*,*)' performs polynomial regression on them'
      WRITE(*,*)' and presents data and results graphically'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number N of data points (1 <= N <= 100)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter SIGMA (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
      WRITE(*,*)
     +' Enter number M of terms in polynomial for data (1<=M<=N)'
      WRITE(*,*)' >'
      READ(*,*)M
      DO 2 I=1,M
        WRITE(*,'(A,I3,A)')' Enter X(',I,' ) for simulation'
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
C Now DAT is exactly given by polynomial.
C Add normal error of width SIGMA and store result in Y
        Y(I)=DAT+Y(I)*SIGMA
C Set error of data point equal to SIGMA
        DELTAY(I)=SIGMA
10    CONTINUE
C Perform polynomial regression
      CALL REGPOL(T,Y,DELTAY,N,NR,X,B,A,CHI2)
C Output results
      WRITE(*,'(A)')' T '
      WRITE(*,'(1X,10F7.2)')(T(I),I=1,N)
      WRITE(*,'(A)')' Y'
      WRITE(*,'(1X,10F7.2)')(Y(I),I=1,N)
      WRITE(*,'(A)')' DELTAY'
      WRITE(*,'(1X,10F7.2)')(DELTAY(I),I=1,N)
      WRITE(*,'(A)')' X'
      WRITE(*,'(1X,10F7.2)')(X(I),I=1,NR)
      WRITE(*,'(A)')' CHI2'
      WRITE(*,'(1X,10F7.2)')(CHI2(I),I=1,NR)
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      WRITE(*,*)' Enter maximum number of terms in polynomials'
      WRITE(*,*)' to be plotted (<= 10)'
      WRITE(*,*)' >'
      READ(*,*)NPOL
C set errors along abscissa and covariances to zero for graphics
      DO 15 I=1,N
        DATSX(I)=0.D0
        DATCOV(I)=0.D0
15    CONTINUE
C compute polylines for polynomials of degrees 0,1,2,....
      T0=-1.5D0
      DT=3.D0/DBLE(NPL-1)
      DO 40 I=1,NPL
        DO 30 J=1,NPOL
          XPL(I,J)=T0+(I-1)*DT
          YPL(I,J)=ZERO
          DO 27 L=1,J
            D=B(L,1)
            IF(L.GT.1) THEN
              DO 25 K=2,L
                D=D+B(L,K)*XPL(I,J)**(K-1)
25            CONTINUE
            END IF
            YPL(I,J)=YPL(I,J)+X(L)*D
27        CONTINUE
30      CONTINUE
40    CONTINUE
      DO 50 J=1,10
        NCOL(J)=4
50    CONTINUE
C prepare texts for graphics
      TX='t'
      LTX=1
      TY='y'
      LTY=1
      CAPT=' '
      LCAPT=1
      NMARK=5
      SCALEF=.5D0
C call graphics routine for data points and multiple polylines
      CALL GRDTMC(XPL,YPL,NPL,NPOL,NCOL,NMARK,SCALEF, T,Y,DATSX,
     +DELTAY,DATCOV,N,TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
      END
