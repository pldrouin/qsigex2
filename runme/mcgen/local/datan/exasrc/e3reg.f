      PROGRAM E3REG
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(N=10,NR=10,NPL=200,NCURVE=3)
      DIMENSION T(N),Y(N),DELTAY(N),X(NR),CHI2(NR),B(NR,NR),
     +A(N,NR),XPL(NPL,NCURVE),YPL(NPL,NCURVE),NCOL(NCURVE),
     +CONETA(NPL),DATSX(N),DATCOV(N)
      CHARACTER*75 TX,TY,CAPT
      PARAMETER (ZERO=0.D0,ONE=1.D0)
      DATA T
     +/-.9D0,-.7D0,-.5D0,-.3D0,-.1D0,.1D0,.3D0,.5D0,.7D0,.9D0/
      DATA Y /81.D0,50.D0,35.D0,27.D0,26.D0,
     +60.D0,106.D0,189.D0,318.D0,520.D0/
C identify program to user
      WRITE(*,*)' Program E3REG demonstrates use of REGCON'
      WRITE(*,*)' '
C set errors
      DO 10 I=1,N
        DELTAY(I)=SQRT(Y(I))
10    CONTINUE
C perform polynomial regression
      CALL REGPOL(T,Y,DELTAY,N,NR,X,B,A,CHI2)
C output results in alphanumeric form
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
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      WRITE(*,*)' Enter number n_pol of terms in polynomial'
      WRITE(*,*)' to be plotted (>0, <10)'
      WRITE(*,*)' >'
      READ(*,*)NPOL
      WRITE(*,*)' Enter confidence level (>0., < 1.0 )'
      WRITE(*,*)' >'
      READ(*,*)CONFID
C perform polynomial regression
      CALL REGPOL(T,Y,DELTAY,N,NPOL,X,B,A,CHI2)
C set errors along abscissa and covariances to zero for graphics
      DO 15 I=1,N
        DATSX(I)=0.D0
        DATCOV(I)=0.D0
15    CONTINUE
C compute polylines for fitted polynomial (I=1) and confidence
C limits (I=2,3) using REGCON
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
C set colors for different curves
      NCOL(1)=4
      NCOL(2)=-5
      NCOL(3)=-5
C prepare texts and caption
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
C draw data points and curves
      CALL GRDTMC(XPL,YPL,NPL,NCURVE,NCOL,NMARK,SCALEF,
     +T,Y,DATSX,DELTAY,DATCOV,N,TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
      END
