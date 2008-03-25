      PROGRAM S1SD
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION HIST(1000)
      CHARACTER*75 TX,TY,CAPT
C identify program to user
      WRITE(*,*)' Program S1SD provides interactively graphs of'
      WRITE(*,*)' statistical functions of a discrete variable'
      WRITE(*,*)' '
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - binomial distribution'
      WRITE(*,*)' 2 - hypergeometric distribution'
      WRITE(*,*)' 3 - Poisson distribution'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - probability'
      WRITE(*,*)' 2 - distribution function (cumulative prob.)'
      WRITE(*,*)' >'
      READ(*,*)ISWIT1
      IF(ISWIT.EQ.1) THEN
C binomial
        WRITE(*,*)' Enter p (0. <= p <= 1.)'
        WRITE(*,*)' >'
        READ(*,*)P
        WRITE(*,*)' Enter n (0 <=n <= 998)'
        WRITE(*,*)' >'
        READ(*,*)N
        DELX=1.D0
          CAPT='n = '
        IF(ISWIT1.EQ.1) THEN
          TX='k'
          TY='P(k)'
          X0=-0.5D0
          NX=N+1
        ELSE IF(ISWIT1.EQ.2) THEN
          TX='K'
          TY='P(k<K)'
          X0=-1.D0
          NX=N+2
        END IF
        CALL SMHSIN(HIST,X0,DELX,NX)
        WRITE(CAPT(5:8),'(I4)')N
        DO 10 K=0,N+1
          IF(ISWIT1.EQ.1) THEN
            HIST(K+1)=SDBINM(K,N,P)
          ELSE IF(ISWIT1.EQ.2) THEN
            HIST(K+1)=SCBINM(K,N,P)
          END IF
10      CONTINUE
        CALL SMHSGR(HIST,X0,DELX,NX,TX,1,TY,5,CAPT,8,NWS)
      ELSE IF(ISWIT.EQ.2) THEN
C hypergeometric
        WRITE(*,*)' Enter K (0 <= K <= 998)'
        WRITE(*,*)' >'
        READ(*,*)KK
        WRITE(*,*)' Enter N (>= K)'
        WRITE(*,*)' >'
        READ(*,*)NN
        WRITE(*,*)' Enter n (0 <= n <= N)'
        WRITE(*,*)' >'
        READ(*,*)N
        KMAX=MIN(N,KK)
        DELX=1.D0
        NX=KMAX+2
        CAPT='K = ****, N = ****, n = ****'
        WRITE(CAPT(5:8),'(I4)')KK
        WRITE(CAPT(15:18),'(I4)')NN
        WRITE(CAPT(25:28),'(I4)')N
        IF(ISWIT1.EQ.1) THEN
          TX='k'
          TY='P(k)'
          X0=-0.5D0
        ELSE IF(ISWIT1.EQ.2) THEN
          TX='k'''
          TY='P(k<k'')'
          X0=-1.D0
        END IF
        CALL SMHSIN(HIST,X0,DELX,NX)
        DO 20 K=0,KMAX+1
          IF(ISWIT1.EQ.1) THEN
            HIST(K+1)=SDHYPG(K,N,KK,NN)
          ELSE IF(ISWIT1.EQ.2) THEN
            HIST(K+1)=SCHYPG(K,N,KK,NN)
          END IF
20      CONTINUE
        CALL SMHSGR(HIST,X0,DELX,NX,TX,2,TY,8,CAPT,28,NWS)
      ELSE IF(ISWIT.EQ.3) THEN
C Poisson
        WRITE(*,*)' Enter lambda (0. < lambda < 10)'
        WRITE(*,*)' >'
        READ(*,*)ALAMBD
        F=SDPOIS(K,ALAMBD)
        KMAX=NINT(10.D0*ALAMBD)
        DELX=1.D0
        NX=KMAX+1
        CAPT='&l@ = ******'
        WRITE(CAPT(7:12),'(F6.2)')ALAMBD
        IF(ISWIT1.EQ.1) THEN
          TX='k'
          TY='P(k)'
          X0=-0.5D0
        ELSE IF(ISWIT1.EQ.2) THEN
          TX='K'
          TY='P(k<K)'
          X0=-1.D0
        END IF
        CALL SMHSIN(HIST,X0,DELX,NX)
        DO 30 K=0,KMAX
          IF(ISWIT1.EQ.1) THEN
            HIST(K+1)=SDPOIS(K,ALAMBD)
          ELSE IF(ISWIT1.EQ.2) THEN
            HIST(K+1)=SCPOIS(K,ALAMBD)
          END IF
30      CONTINUE
        CALL SMHSGR(HIST,X0,DELX,NX,TX,1,TY,6,CAPT,12,NWS)
      END IF
      END
