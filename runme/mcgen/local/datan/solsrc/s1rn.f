      PROGRAM S1RN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R(10000),HIST(50)
      CHARACTER*5 TX,TY,CAPT
      DATA TX/'x'/,TY/'N(x)'/,CAPT/' '/
C identify program to user
      WRITE(*,*)' Program S1RN demonstrates use of RNBW'
      WRITE(*,*)' which is a subroutine generating random numbers'
      WRITE(*,*)' following a Breit-Wigner distribution'
      WRITE(*,*)' '
C let user choose
      WRITE(*,*)' Enter number N (<10001)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter mean value A'
      WRITE(*,*)' >'
      READ(*,*)A
      WRITE(*,*)' Enter full width at half maximum GAMMA (>0.)'
      WRITE(*,*)' >'
      READ(*,*)GAMMA
      WRITE(*,*)' Do you want'
      WRITE(*,*)' 1 - numerical output'
      WRITE(*,*)' 2 - graphical output'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      CALL RNBW(A,GAMMA,R,N)
      IF(ISWIT.EQ.1) THEN
C numerical output
        WRITE(*,'(1X,5F12.6)')(R(I),I=1,N)
      ELSE IF(ISWIT.EQ.2) THEN
C graphical output
C initialize histogram
        X0=A-5.D0*GAMMA
        DELX=.2D0*GAMMA
        NX=50
        CALL SMHSIN(HIST,X0,DELX,NX)
C fill histogram
        DO 10 I=1,N
          CALL SMHSFL(HIST,X0,DELX,NX,R(I),1.D0)
10      CONTINUE
C ask for number of workstation
        CALL GRNBWS()
        WRITE(*,*)' Please, enter number of workstation:'
        WRITE(*,*)' >'
        READ(*,*)NWS
C display of histogram
        CALL SMHSGR(HIST,X0,DELX,NX,TX,5,TY,5,CAPT,5,NWS)
      END IF
      END
C ------------------------------------------------------------------
      SUBROUTINE RNBW(A,GAMMA,R,N)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(PI=3.14159D0,HALF=0.5D0)
C a CALL of this subroutine generates in Array R a total of N
C random numbers following a Breit-Wigner distribution
C with mean A and full width at half maximum GAMMA
      DIMENSION R(N)
      CALL RNECUY(R,N)
      DO 10 I=1,N
        R(I)=A+HALF*GAMMA*TAN(PI*(R(I)-HALF))
10    CONTINUE
      END
