      PROGRAM E5GR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TX(201),TY(201)
      CHARACTER*75 TEXTX,TEXTY
      PARAMETER(ZERO=0.D0,ONE=1.D0,ROOT2=1.414D0)
C identify program to user
      WRITE(*,*)' Program E5GR demonstrates use of'
      WRITE(*,*)' GRSCDF and GRCCRS'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C prepare texts for scales
      TEXTX='x'
      TEXTY='f(x)'
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(-10.D0,10.D0,-0.1D0,.5D0)
      CALL GRWNWC(0.D0,1.414D0,0.D0,1.D0)
      CALL GRFRAM
C loop over 4 plots
      DO 30 IPLOT=1,4
C set view port in WC for different plots
        XA=0.25
        IF(IPLOT.EQ. 2.OR. IPLOT.EQ.4) XA=0.25+.5*ROOT2
        XB=XA+0.4
        YA=0.65
        IF(IPLOT.EQ. 3.OR. IPLOT.EQ.4) YA=0.15
        YB=YA+0.3
        CALL GRVWWC(XA,XB,YA,YB)
        CALL GRSTCL(1)
C draw boundary (around window in CC)
        CALL GRBOUN
C draw coordinate cross
        CALL GRCCRS
        IF(IPLOT.EQ.1) THEN
C plot 1 (upper left): take defaults for designing scales
          CALL GRSCLX(TEXTX,75)
          CALL GRSCLY(TEXTY,75)
        ELSE IF(IPLOT.EQ.2) THEN
C plot 2 (upper right):
C change design of scales
          CALL GRSCDF(ZERO,5.D0,0,ONE,ONE,.TRUE.)
          CALL GRSCLX(TEXTX,75)
          CALL GRSCDF(ZERO,.2D0,2,ZERO,ZERO,.TRUE.)
          CALL GRSCLY(TEXTY,75)
        ELSE IF(IPLOT.EQ.3) THEN
C plot 2 (lower left):
C change design of scales
          CALL GRSCDF(ZERO,5.D0,0,0.8D0,0.8D0,.TRUE.)
          CALL GRSCLX(TEXTX,75)
          CALL GRSCDF(ZERO,.2D0,2,0.8D0,0.8D0,.TRUE.)
          CALL GRSCLY(TEXTY,75)
        ELSE IF(IPLOT.EQ.4) THEN
C plot 2 (lower right right):
C change design of scales
          CALL GRSCDF(ZERO,5.D0,5,0.8D0,0.8D0,.FALSE.)
          CALL GRSCLX(TEXTX,75)
          CALL GRSCDF(ZERO,.2D0,1,0.8D0,0.8D0,.FALSE.)
          CALL GRSCLY(TEXTY,75)
        END IF
C prepare and draw polyline
        DELX=0.1D0
        X0=-10.D0
        DO 10 I=1,201
          TX(I)=X0+(I-1)*DELX
          TY(I)=SDSTNR(TX(I))
10      CONTINUE
        CALL GRSTCL(6)
        CALL GRPLIN(201,TX,TY)
30    CONTINUE
      CALL GRCLSE
      END
