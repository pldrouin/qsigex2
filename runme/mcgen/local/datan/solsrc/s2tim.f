      PROGRAM S2TIM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXN=400,MAXL=10,MAXK=40)
      DIMENSION Y(MAXN),T(MAXN+2*MAXK),ETA(MAXN+2*MAXK)
      DIMENSION ETADEL(MAXN+2*MAXK),CONETA(MAXN+2*MAXK)
      DIMENSION ATA1(MAXL+1,MAXL+1),ATA1AT(MAXL+1,2*MAXK+1)
      DIMENSION SCRAT(MAXL+1,MAXL+1),A(2*MAXK+1,MAXL+1)
C COMMON /CS2TIM/ is needed for Microsoft FORTRAN compiler
      COMMON /CS2TIM/ Y,T,ETA,ETADEL,CONETA,ATA1,ATA1AT,SCRAT,A
      CHARACTER*1 STRING
      CHARACTER*75 CAPT
C identify program to user
      WRITE(*,*)' Program S2TIM simulates data with errors corres-'
      WRITE(*,*)' ponding to continuous or discontinuous functions,'
      WRITE(*,*)' performs time series analysis on them'
      WRITE(*,*)' and presents results graphically'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)'  Enter N(<= 400)'
      WRITE(*,*)' >'
      READ(*,*)N
      IF (N.GT.400) N=400
      WRITE(*,*)' Enter SIGMA (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
      WRITE(*,*)' Enter k ( 0 < k < min(40,N) )'
      WRITE(*,*)' >'
      READ(*,*)K
      IF (K.GT.40) K=40
      WRITE(*,*)' Enter l ( 0 < l < k  and l < 10)'
      WRITE(*,*)' >'
      READ(*,*)L
      IF (L.GT.MIN(K,9)) L=MIN(K,9)
      WRITE(*,*)' Enter P ( 0 < P < 1.0 )'
      WRITE(*,*)' >'
      READ(*,*)P
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - sine function'
      WRITE(*,*)' 2 - step function'
      WRITE(*,*)' 3 - saw tooth function'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C generate time series
      CALL RNSTNR(Y,N)
      DO 1 I=1,N
        Y(I)=Y(I)*SIGMA
        TIME=DBLE(I)
        IF(ISWIT.EQ.1) THEN
          Y(I)=SIN(.0174532*TIME)+Y(I)
        ELSE IF(ISWIT.EQ.2) THEN
          TT=MOD(TIME,200.D0)
          IF(TT.LE.100.D0) THEN
            Y(I)=-1.D0+Y(I)
          ELSE
            Y(I)=1.D0+Y(I)
          END IF
        ELSE IF(ISWIT.EQ.3) THEN
          TT=MOD(TIME,100.D0)
          Y(I)=0.02*(TT-50.D0)+Y(I)
        END IF
1     CONTINUE
      CALL TIMSER(Y,N,K,L,P,ETA,CONETA,A,ATA1,ATA1AT,SCRAT)
      CALL GROPEN
      CALL GROPWS(NWS)
      YMIN=Y(1)
      YMAX=Y(1)
      DO 7 I=2,N
        YMIN=MIN(Y(I),YMIN)
        YMAX=MAX(Y(I),YMAX)
7     CONTINUE
      CALL GRWNCC(-20.D0,DBLE(N+20),-1.5D0,1.5D0)
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
