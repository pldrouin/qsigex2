      PROGRAM S1TIM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXN=400,MAXL=20,MAXK=20)
      DIMENSION Y(MAXN),T(MAXN+2*MAXK),ETA(MAXN+2*MAXK)
      DIMENSION ETADEL(MAXN+2*MAXK),CONETA(MAXN+2*MAXK)
      DIMENSION ATA1(MAXL+1,MAXL+1),ATA1AT(MAXL+1,2*MAXK+1)
      DIMENSION SCRAT(MAXL+1,MAXL+1),A(2*MAXK+1,MAXL+1)
      COMMON /CS1TIM/ ATA1,AT1AT,A,SCRAT
      CHARACTER*1 STRING
      CHARACTER*75 CAPT
C identify program to user
      WRITE(*,*)' Program S1TIM simulates data with errors'
      WRITE(*,*)' performs time series analysis on them'
      WRITE(*,*)' and presents results graphically'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)'  Enter power m (>=0)'
      WRITE(*,*)' >'
      READ(*,*)M
      WRITE(*,*)' Enter SIGMA (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
      WRITE(*,*)' Enter k ( 0 < k < 21 )'
      WRITE(*,*)' >'
      READ(*,*)K
      WRITE(*,*)' Enter l ( 0 < l < k )'
      WRITE(*,*)' >'
      READ(*,*)L
      WRITE(*,*)' Enter P ( 0 < P < 1.0 )'
      WRITE(*,*)' >'
      READ(*,*)P
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C generate time series
      N=200
      CALL RNSTNR(Y,N)
      DO 1 I=1,N
        Y(I)=Y(I)*SIGMA
        TIME=DBLE(I)
        X=0.02D0*(TIME-100.D0)
        F=X**M
        Y(I)=Y(I)+F
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
      D=YMAX-YMIN
      YMIN=YMIN-0.2D0*D
      YMAX=YMAX+0.2D0*D
      CALL GRWNCC(-20.D0,DBLE(N+20),YMIN,YMAX)
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
