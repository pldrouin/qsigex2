      PROGRAM E1GR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION TX(201),TY(201)
      CHARACTER*75 STRING
C identify program to user
      WRITE(*,*)' Program E1GR demonstrates use of'
      WRITE(*,*)' GROPEN,GRCLSE,GROPWS,GRCLWS,GRWNCC,GRVWWC,'
      WRITE(*,*)' GRWNWC,GRSTFR,GRFRAM,GRBOUN,GRSTCL,GRPLIN,'
      WRITE(*,*)' GRBRPL,GRSCLX,GRSCLY,GRTXTF'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C open GRPACK
      CALL GROPEN
C open workstation
      CALL GROPWS(NWS)
C define window in computing coordinates (CC)
      CALL GRWNCC(-10.D0,10.D0,0.D0,.5D0)
C define view port in world coordinates (WC)
      CALL GRVWWC(0.25D0,1.35D0,0.2D0,.8D0)
C define window in WC
      CALL GRWNWC(0.D0,1.414D0,0.D0,1.D0)
C define format in device coordinates
      CALL GRSTFR(NWS,5.D0,0.D0)
C draw frame (given by window in WC)
      CALL GRFRAM
C draw boundary (around window in CC)
      CALL GRBOUN
C scale on abscissa
      STRING='x'
      CALL GRSCLX(STRING,1)
C scale on ordinate
      STRING='f(x)'
      CALL GRSCLY(STRING,4)
C caption
      STRING='f(x)=(2&ps^2#@)^-1/2#exp(-(x-a)^2#/2&s^2#@)'
      CALL GRTXTC(1.5D0,STRING,75)
C compute points for polyline of standardized normal
      DELX=0.1D0
      X0=-10.D0
      DO 10 I=1,201
        TX(I)=X0+(I-1)*DELX
        TY(I)=SDSTNR(TX(I))
10    CONTINUE
C set color index
      CALL GRSTCL(6)
C draw continuous polyline
      CALL GRPLIN(201,TX,TY)
C compute points for polyline of Gaussian with mean 2
C and standard deviation 3
      DO 20 I=1,201
        TX(I)=X0+(I-1)*DELX
        TY(I)=SDNORM(TX(I),2.D0,3.D0)
20    CONTINUE
C draw broken polyline
      CALL GRBRPL(1,0.D0,201,TX,TY)
C compute and draw short straight continuous polyline
      TX(1)=-8.D0
      TX(2)=-5.D0
      TY(1)=.4D0
      TY(2)=.4D0
      CALL GRPLIN(2,TX,TY)
C place text to the right of short polyline
      STRING='a=0, &s=1'
      CALL GRTXTF(-4.5D0,.4D0,1.D0,STRING,75)
C compute and draw short straight broken polyline
      TY(1)=.3
      TY(2)=.3
      CALL GRBRPL(1,0.D0,2,TX,TY)
C place text to its right
      STRING='a=2, &s=3'
      CALL GRTXTF(-4.5D0,.3D0,1.D0,STRING,75)
C close workstation
      CALL GRCLWS(NWS)
C close GRPACK
      CALL GRCLSE
      END
