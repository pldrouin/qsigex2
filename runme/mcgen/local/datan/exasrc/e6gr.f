      PROGRAM E6GR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION HIST(100),XPL(1000),YPL(1000)
      CHARACTER*75 TX,TY,CAPT
C identify program to user
      WRITE(*,*)' Program E6GR demonstrates use of GRHSCV'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      ALAMBD=10.D0
C prepare histogram
      X0=-0.5D0
      NX=30
      DELX=1.D0
C fill histogram with Poisson probabilities
      DO 10 I=1,NX
        HIST(I)=SDPOIS(I-1,ALAMBD)
10    CONTINUE
C prepare polyline
      NPL=1000
      DX=DBLE(NX)*DELX/DBLE(NPL-1)
      SIGMA=SQRT(ALAMBD)
C compute points on polyline with normal probability density
      DO 20 I=1,NPL
        XPL(I)=X0+DBLE(I-1)*DX
        YPL(I)=SDNORM(XPL(I),ALAMBD,SIGMA)
20    CONTINUE
C prepare texts and caption
      TX='k'
      LTX=1
      TY='P(k)'
      LTY=4
      CAPT='Poisson (histogram) and Gaussian (cont. line)'
      LCAPT=45
C produce graphics by call of GRHSCV
      CALL GRHSCV(XPL,YPL,NPL,HIST,X0,DELX,NX,TX,LTX,
     +TY,LTY,CAPT,LCAPT,NWS)
      END
