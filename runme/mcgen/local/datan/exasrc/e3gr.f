      PROGRAM E3GR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C identify program to user
      WRITE(*,*)' Program E3GR demonstrates use of GRDATP'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C initialization
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(-1.2D0,1.D0,-1.2D0,1.D0)
      CALL GRVWWC(0.D0,1.4D0,0.D0,1.D0)
      CALL GRWNWC(-.0D0,1.414D0,0.D0,1.D0)
      CALL GRFRAM
C draw different data points
      S=1.D0
      CALL GRDATP(1,S,-.8D0,.5D0,.3D0,0.D0,0.D0)
      CALL GRDATP(2,S,-.0D0,.5D0,.0D0,0.2D0,0.D0)
      CALL GRDATP(3,S,.7D0,.5D0,.2D0,0.3D0,0.D0)
      CALL GRDATP(4,S,-.6D0,-.5D0,.4D0,.4D0,-.4D0)
      CALL GRDATP(5,S,.5D0,-.5D0,.45D0,.3D0,0.6D0)
      CALL GRCLSE
      END
