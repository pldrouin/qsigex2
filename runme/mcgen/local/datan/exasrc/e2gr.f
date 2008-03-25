      PROGRAM E2GR
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*1 STRING
C identify program to user
      WRITE(*,*)' Program E2GR demonstrates use of GRMARK'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C initialization
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(DBLE(-5.),DBLE(5.),DBLE(0.),DBLE(.5))
      CALL GRVWWC(DBLE(0.),DBLE(1.4),DBLE(0.),DBLE(1.))
      CALL GRWNWC(DBLE(0.),DBLE(1.414),DBLE(0.),DBLE(1.0))
      CALL GRFRAM
      N=1
      S=1.D0
      X=-5.D0
      DX=1.D0
C loop over different types of mark
      DO 10 N=1,9
        X=X+DX
        IF(MOD(N,2).NE.0) THEN
          Y=.35D0
          YY=.4D0
        ELSE
          Y=.15D0
          YY=.1D0
        END IF
C prepare text string to identify marks
        WRITE(STRING,'(I1)') N
C draw mark
        CALL GRMARK(N,S,X,Y)
C place text near mark
        CALL GRTXTF(X,YY,S,STRING,1)
10    CONTINUE
      CALL GRCLSE
      END
