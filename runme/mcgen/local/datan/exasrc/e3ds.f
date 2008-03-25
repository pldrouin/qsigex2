      PROGRAM E3DS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R(1000),HIST(1001)
      CHARACTER*5 TX,TY
      CHARACTER*40 CAPT
C identify program to user
      WRITE(*,*)' Program E3DS simulates Galton''s board'
      WRITE(*,*)' and demonstrates statistical fluctuations'
      WRITE(*,*)' '
C ask for numerical input
      WRITE(*,*)' Enter number NEXP of trials'
      WRITE(*,*)' >'
      READ(*,*)NEXP
      WRITE(*,*)' Enter number n of steps per trial (n<1001)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter probability p'
      WRITE(*,*)' >'
      READ(*,*)P
C initialization
      CAPT='p = *****, n = ****, N_exp# = *******'
      WRITE(CAPT(5:9),'(F5.3)')P
      WRITE(CAPT(16:19),'(I4)')N
      WRITE(CAPT(31:37),'(I7)')NEXP
      X0=-0.5D0
      DELX=1.D0
      NX=N+1
      CALL SMHSIN(HIST,X0,DELX,NX)
C simulate trials and fill histogram
      DO 20 I=1,NEXP
        CALL RNECUY(R,N)
        K=0
        IF(N.GT.0) THEN
          DO 10 J=1,N
            IF(R(J).LT.P) K=K+1
10        CONTINUE
        END IF
        CALL SMHSFL(HIST,X0,DELX,NX,DBLE(K),1.D0)
20    CONTINUE
C graphical output
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C present histogram
      TX='k'
      TY='N(k)'
      CALL SMHSGR(HIST,X0,DELX,NX,TX,5,TY,5,CAPT,40,NWS)
      END
