      PROGRAM E2TIM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(36),T(50),ETA(50),ETADEL(50),CONETA(50),A(21,6)
      DIMENSION ATA1(6,6),ATA1AT(6,21),SCRAT(6,6)
      CHARACTER*1 STRING
      CHARACTER*75 CAPT
      DATA Y /38.7D0,50.3D0,45.6D0,46.4D0,43.7D0,42.0D0,
     +21.8D0,21.8D0,51.3D0,39.5D0,26.9D0,23.2D0,
     +19.8D0,24.4D0,17.1D0,29.3D0,43.0D0,35.9D0,
     +19.6D0,33.2D0,38.8D0,35.3D0,23.4D0,14.9D0,
     +15.3D0,17.7D0,16.5D0, 6.6D0, 9.5D0, 9.1D0,
     + 3.1D0, 9.3D0, 4.7D0, 6.1D0, 7.4D0,15.1D0/
C identify program to user
      WRITE(*,*)' Program E2TIM performs time series analysis'
      WRITE(*,*)' and presents results graphically'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter k ( 0 < k < 5 )'
      WRITE(*,*)' >'
      READ(*,*)K
      WRITE(*,*)' Enter l ( 0 < l < k )'
      WRITE(*,*)' >'
      READ(*,*)L
      WRITE(*,*)' Enter P ( 0. < P < 1.0 )'
      WRITE(*,*)' >'
      READ(*,*)P
      N=36
      CALL TIMSER(Y,N,K,L,P,ETA,CONETA,A,ATA1,ATA1AT,SCRAT)
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(-10.D0,50.D0,-20.D0,70.D0)
      CALL GRVWWC(0.D0,1.D0,.2D0,.8D0)
      CALL GRWNWC(-.314D0,1.1D0,0.D0,1.D0)
      CALL GRBOUN
      CALL GRFRAM
C write caption
      CAPT='N = ***, k = ***, l = ***, P = *****'
      WRITE(CAPT(5:7),'(I3)')N
      WRITE(CAPT(14:16),'(I3)')K
      WRITE(CAPT(23:25),'(I3)')L
      WRITE(CAPT(32:36),'(F5.3)')P
      CALL GRTXTC(1.D0,CAPT,75)
      STRING='t'
      CALL GRSCLX(STRING,1)
      STRING='y'
      CALL GRSCLY(STRING,1)
C draw data points
      CALL GRSTCL(2)
      DO 10 I=1,N
        CALL GRDATP(1,.2D0,DBLE(I),Y(I),0.D0,0.D0,0.D0)
10    CONTINUE
C draw moving averages
      CALL GRSTCL(3)
      DO 20 I=1,N+2*K
        T(I)=DBLE(I-K)
20    CONTINUE
      CALL GRPLIN(N+2*K,T,ETA)
      CALL GRSTCL(4)
C draw confidence limits
      DO 30 I=1,N+2*K
        ETADEL(I)=ETA(I)+CONETA(I)
30    CONTINUE
      CALL GRPLIN(N+2*K,T,ETADEL)
      DO 40 I=1,N+2*K
        ETADEL(I)=ETA(I)-CONETA(I)
40    CONTINUE
      CALL GRPLIN(N+2*K,T,ETADEL)
      CALL GRCLSE
      END
