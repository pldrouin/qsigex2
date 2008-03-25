      PROGRAM E4GR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*75 STRING
      PARAMETER(PI=3.14159D0,NSTEP=100,NCONT=9)
      EXTERNAL CONTFN
      DOUBLE PRECISION CONTFN
C identify program to user
      WRITE(*,*)' Program E4GR demonstrates use of GRPLCT'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C initialization
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(-PI,PI,-PI,PI)
      CALL GRVWWC(0.D0,1.D0,0.D0,1.D0)
      CALL GRWNWC(-.621D0,1.5D0,-0.3D0,1.2D0)
      CALL GRSTCL(1)
      CALL GRFRAM
      CALL GRBOUN
C scales
      STRING='x'
      CALL GRSCLX(STRING,75)
      STRING='y'
      CALL GRSCLY(STRING,75)
      STRING='sin(x+y)cos((x-y)/2)'
      CALL GRTXTC(1.2D0,STRING,75)
C define pixels
      D=2.D0*3.2D0/DBLE(NSTEP)
      X0=-3.2D0
      Y0=-3.2D0
      DX=D
      DY=D
      NX=NSTEP
      NY=NSTEP
      CALL GRSTCL(3)
C plot contour lines
      DELCNT=2.D0/DBLE(NCONT+1)
      DO 10 I=1,NCONT
        CONT=1.D0-DBLE(I)*DELCNT
        CALL GRPLCT(X0,Y0,DX,DY,NX,NY,CONT,CONTFN)
10    CONTINUE
      CALL GRCLSE
      END
C**********************************************************
      DOUBLE PRECISION FUNCTION CONTFN(X,Y)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(HALF=0.5D0)
C function for which contour lines are to be drawn
      CONTFN=SIN(X+Y)*COS(HALF*(X-Y))
      END
