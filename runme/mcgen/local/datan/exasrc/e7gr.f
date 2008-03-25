      PROGRAM E7GR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XPL(100),YPL(100),DATX(21),DATY(21)
      DIMENSION DATSX(21),DATSY(21),DATCOV(21)
      CHARACTER*75 TX,TY,CAPT
C identify program to user
      WRITE(*,*)' Program E7GR demonstrates use of GRDTCV'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C generate data points
      NDAT=21
      A=1.D0
      B=1.D0
      CALL RNLINE(A,B,-1.D0,.1D0,NDAT,.1D0,DATX,DATY)
      DO 10 I=1,NDAT
        DATSX(I)=0.D0
        DATSY(I)=0.1D0
        DATCOV(I)=0.D0
10    CONTINUE
C compute points which define polyline
      NPL=2
      XPL(1)=-2.D0
      YPL(1)=A*XPL(1)+B
      XPL(2)=2.D0
      YPL(2)=A*XPL(2)+B
C prepare texts and caption
      TX='t'
      LTX=1
      TY='y'
      LTY=4
      CAPT='y = at + b'
      LCAPT=11
      NMARK=5
      SCALEF=.5D0
C produce graphics by call of GRDTCV
      CALL GRDTCV(XPL,YPL,NPL,NMARK,SCALEF,DATX,DATY,
     +DATSX,DATSY,DATCOV,NDAT,TX,LTX,TY,LTY,CAPT,LCAPT,NWS)
      END
