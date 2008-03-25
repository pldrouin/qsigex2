      PROGRAM S2REG
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NMAX=100,NRMAX=10,NR=10,NPL=200,NCURVE=3)
      DIMENSION T(NMAX),Y(NMAX),DELTAY(NMAX),X(NRMAX),CHI2(NRMAX)
      DIMENSION B(NRMAX,NRMAX),A(NMAX,NRMAX)
      DIMENSION XPL(NPL,NCURVE),YPL(NPL,NCURVE),NCOL(NCURVE)
      DIMENSION CONETA(NPL)
      DIMENSION DATSX(NMAX),DATCOV(NMAX)
      CHARACTER*75 TX,TY,CAPT
      PARAMETER (ZERO=0.D0,ONE=1.D0)
C identify program to user
      WRITE(*,*)' Program S2REG simulates data points with errors,'
      WRITE(*,*)' performs polynomial regression on them'
      WRITE(*,*)' and presents data and results graphically'
      WRITE(*,*)' including confidence limits'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number N of data points (10 <= N <= 100)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter SIGMA (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - SIGMA known for regression'
      WRITE(*,*)' 2 - SIGMA unknown'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
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
C Set error of data point equal to SIGMA or equal to one
        IF(ISWIT.EQ.1) THEN
            DELTAY(I)=SIGMA
        ELSE
            DELTAY(I)=ONE
        END IF
10    CONTINUE
C Perform polynomial regression
      CALL REGPOL(T,Y,DELTAY,N,NR,X,B,A,CHI2)
C Output results
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
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      WRITE(*,*)' Enter number n_pol (<= 10)of terms in polynomial'
      WRITE(*,*)' to be plotted'
      WRITE(*,*)' >'
      READ(*,*)NPOL
      WRITE(*,*)' Enter confidence level ( < 1.0 )'
      WRITE(*,*)' >'
      READ(*,*)CONFID
C perform polynomial regression using NPOL
      CALL REGPOL(T,Y,DELTAY,N,NPOL,X,B,A,CHI2)
C set errors along abscissa and covariances to zero for graphics
C in case that errors are unknown,
C set also errors along ordinate to zero
      DO 15 I=1,N
        DATSX(I)=0.D0
        DATCOV(I)=0.D0
        IF(ISWIT.EQ.2) DELTAY(I)=ZERO
15    CONTINUE
C compute polylines for fitted polynomial (I=1) and confidence
C limits (I=2,3) using REGCON
      IF(ISWIT.EQ.1) CHI2(NPOL)=ZERO
      T0=-1.5D0
      DT=3.D0/DBLE(NPL-1)
      DO 40 I=1,NPL
        XPL(I,1)=T0+(I-1)*DT
        CALL REGCON(X,B,XPL(I,1),CHI2(NPOL),CONFID,NPOL,N-NPOL,
     +  YPL(I,1),CONETA(I))
        XPL(I,2)=XPL(I,1)
        XPL(I,3)=XPL(I,1)
        YPL(I,2)=YPL(I,1)-CONETA(I)
        YPL(I,3)=YPL(I,1)+CONETA(I)
40    CONTINUE
      NCOL(1)=4
      NCOL(2)=-5
      NCOL(3)=-5
      TX='t'
      LTX=1
      TY='y'
      LTY=1
      CAPT=' n_pol# = ***,   C.L. = ******'
      WRITE(CAPT(11:13),'(I3)')NPOL
      WRITE(CAPT(25:30),'(F6.4)')CONFID
      LCAPT=30
      NMARK=5
      SCALEF=.5D0
      CALL GRDTMC(XPL,YPL,NPL,NCURVE,NCOL,NMARK,SCALEF,T,Y,DATSX,
     +DELTAY,DATCOV,N,TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
      END
