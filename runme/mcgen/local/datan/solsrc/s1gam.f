      PROGRAM S1GAM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION XPL(1000),YPL(1000)
      CHARACTER*75 TX,TY
      PARAMETER(EPS=1.D-5)
C identify program to user
      WRITE(*,*)' Program S1GAM provides interactively graphs'
      WRITE(*,*)' of the gamma function and its inverse'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - gamma function'
      WRITE(*,*)' 2 - 1/gamma'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      XMIN=-5.D0
      XMAX=5.D0
      YMIN=-5.D0
      YMAX=5.D0
      TX='x'
      LTX=1
      IF(ISWIT.EQ.1) THEN
        TY='&G@(x)'
        LTY=6
      ELSE IF(ISWIT.EQ.2) THEN
        TY='1/&G@(x)'
        LTY=8
      END IF
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(XMIN,XMAX,YMIN,YMAX)
      CALL GRVWWC(-0.15D0,0.9D0,0.15D0,0.85D0)
      CALL GRWNWC(-.414D0,1.D0,0.D0,1.D0)
      CALL GRFRAM
      CALL GRBOUN
      CALL GRCCRS
      CALL GRSCLX(TX,LTX)
      CALL GRSCLY(TY,LTY)
      CALL GRSTCL(2)
      IF(ISWIT.EQ.1) THEN
C gamma
        XA=NINT(XMIN-.5D0)
5       XB=XA+1.D0
        IF(XA.LT.0.D0)THEN
C draw separate polylines between adjacent poles for x<0
          X1=XA+EPS
          X2=XB-EPS
          NPL=100
          DEL=(X2-X1)/DBLE(NPL-1)
          DO 10 I=1,NPL
            XPL(I)=X1+(I-1)*DEL
            YPL(I)=GGAMMA(XPL(I))
10        CONTINUE
          CALL GRPLIN(NPL,XPL,YPL)
          XA=XA+1.D0
          GO TO 5
        ELSE
C draw one polyline for x>0
          XA=XA+EPS
          NPL=1000
          DEL=(XMAX-XA)/DBLE(NPL-1)
          DO 20 I=1,NPL
            XPL(I)=XA+(I-1)*DEL
            YPL(I)=GGAMMA(XPL(I))
20        CONTINUE
          CALL GRPLIN(NPL,XPL,YPL)
        END IF
      ELSE IF(ISWIT.EQ.2) THEN
C 1/gamma
        NPL=1000
        DEL=(XMAX-XMIN)/DBLE(NPL-1)
        DO 30 I=1,NPL
          XPL(I)=XMIN+(I-1)*DEL
          YPL(I)=1.D0/GGAMMA(XPL(I))
30      CONTINUE
        CALL GRPLIN(NPL,XPL,YPL)
      END IF
      CALL GRCLSE
      END
