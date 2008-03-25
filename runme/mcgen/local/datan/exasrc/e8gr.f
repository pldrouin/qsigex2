      PROGRAM E8GR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NPL=1000,NC=3)
      DIMENSION XPL(1000,NC),YPL(1000,NC),DATX(21),DATY(21)
      DIMENSION DATSX(21),DATSY(21),DATCOV(21),NCOL(NC)
C COMMON /CME8GR/ is needed for Microsoft FORTRAN compiler
      COMMON /CME8GR/ XPL,YPL
      CHARACTER*75 TX,TY,CAPT
C identify program to user
      WRITE(*,*)' Program E8GR demonstrates use of GRDTMC'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C ask for type of display
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - display data points only'
      WRITE(*,*)' 2 - display curves only'
      WRITE(*,*)' 3 - display both data points and curves'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
C set number NDAT of data points and number NCURVE of polylines
      IF(ISWIT.EQ.1) THEN
        NDAT=21
        NCURVE=0
      ELSE IF(ISWIT.EQ.2) THEN
        NDAT=0
        NCURVE=3
      ELSE IF(ISWIT.EQ.3) THEN
        NDAT=21
        NCURVE=3
      END IF
C generate simulated data points
      IF(NDAT.GT.0) THEN
        CALL RNSTNR(DATY,NDAT)
        DO 10 I=1,NDAT
          DATX(I)=-3.D0+(I-1)*0.3D0
          DATY(I)=0.05D0*DATY(I)+SDSTNR(DATX(I))
          DATSX(I)=0.D0
          DATSY(I)=0.05D0
          DATCOV(I)=0.D0
10      CONTINUE
      END IF
C compute points on polylines
      IF(NCURVE.GT.0) THEN
        X0=-10.D0
        DELX=2.D0*ABS(X0)/(DBLE(NPL-1))
        DO 30 K=1,NCURVE
          SIGMA=DBLE(K)*0.5D0
          NCOL(K)=2+K
          IF(K.NE.2) NCOL(K)=-NCOL(K)
          DO 20 I=1,NPL
            XPL(I,K)=X0+(I-1)*DELX
            YPL(I,K)=SDNORM(XPL(I,K),0.D0,SIGMA)
20        CONTINUE
30      CONTINUE
      END IF
C prepare texts and captions
      TX='x'
      LTX=1
      TY='f(x)'
      LTY=4
      IF(ISWIT.EQ.1) THEN
        CAPT='Data points'
        LCAPT=11
      ELSE IF(ISWIT.EQ.2) THEN
        CAPT='Gaussians of different widths'
        LCAPT=29
      ELSE IF(ISWIT.EQ.3) THEN
        CAPT='Data points and Gaussians of different widths'
        LCAPT=45
      END IF
      NMARK=5
      SCALEF=.5D0
C generate graphics by call of GRDTMC
      CALL GRDTMC(XPL,YPL,NPL,NCURVE,NCOL,NMARK,
     +SCALEF,DATX,DATY,DATSX,DATSY,DATCOV,NDAT,
     +TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
      END
