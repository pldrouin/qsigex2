      PROGRAM E2DS
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION HIST(1000),F(1000)
      CHARACTER*5 TX,TY
      CHARACTER*27 CAPT
C identify program to user
      WRITE(*,*)' Program E2DS simulates experiment'
      WRITE(*,*)' of Rutherford and Geiger'
      WRITE(*,*)' on statistical nature of radioactive decays'
      WRITE(*,*)' '
C ask for numerical input
      WRITE(*,*)' Enter number N of observed decays'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)
     +' Enter number NINT of time intervals (1 <= NINT <= 1000)'
      WRITE(*,*)' >'
      READ(*,*)NINT
C prepare histogram
      X0=0.D0
      DELX=1.D0
      NX=NINT
      NF=INT(FLOAT(N)/FLOAT(NINT))
      IDELNF=INT(SQRT(FLOAT(NF)))+1
      NF0=MAX(0,NF-5*IDELNF)
      NF=NF+5*IDELNF
      F0=-0.5D0
      DELF=1.D0
      NF=MIN(NF,1000)
C prepare texts
      CAPT='N = ******, n_int# = ******'
      WRITE(CAPT(5:10),'(I6)')N
      WRITE(CAPT(22:27),'(I6)')NINT
      CALL SMHSIN(HIST,X0,DELX,NX)
C simulate decay times and fill histogram
      DO 10 I=1,N
        CALL RNECUY(R,1)
        R=R*DBLE(NINT)
        CALL SMHSFL(HIST,X0,DELX,NX,R,1.D0)
10    CONTINUE
C graphical output
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C present histogram
      TX='t'
      TY='N(t)'
      CALL SMHSGR(HIST,X0,DELX,NX,TX,5,TY,5,CAPT,27,NWS)
C analyze histogram of decay times
      DO 30 I=1,NF+1
        K=I-1
        F(I)=0.D0
        DO 20 J=1,NX
          IF(INT(HIST(J)).EQ.K) F(I)=F(I)+1.D0
20      CONTINUE
30    CONTINUE
      TX='k'
      TY='N(k)'
      CALL SMHSGR(F,F0,DELF,NF,TX,5,TY,5,CAPT,27,NWS)
      END
