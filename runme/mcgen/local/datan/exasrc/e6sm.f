      PROGRAM E6SM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C identify program to user
      WRITE(*,*)
     +' Program E6SM simulates experiments with few events'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number NEPX of experiments (>0)'
      WRITE(*,*)' >'
      READ(*,*)NEXP
      WRITE(*,*)' Enter number N of studied objects (>>0)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)
     +' Enter Poisson parameter ALSIGN for signal events (>0.)'
      WRITE(*,*)' >'
      READ(*,*)ALSIGN
      WRITE(*,*)
     +' Enter Poisson parameter ALBACK for background evts (>=0.)'
      WRITE(*,*)' >'
      READ(*,*)ALBACK
      WRITE(*,*)' Enter confidence limit CONFID (0.<CONFID<1.)'
      WRITE(*,*)' >'
      READ(*,*)CONFID
      PSIGN=ALSIGN/DBLE(N)
      PBACK=ALBACK/DBLE(N)
      P=PSIGN+PBACK
C write table header
      WRITE(*,'(/,I5,A)')NEXP,' experiments will be simulated with'
      WRITE(*,1000)' ALSIGN = ',ALSIGN,' i.e. PSIGN = ',PSIGN
      WRITE(*,1000)' ALBACK = ',ALBACK,' i.e. PBACK = ',PBACK
1000  FORMAT(A,F10.5,A,F10.8)
      WRITE(*,'(A,A,F5.3)')' Limits for ALSIGN are obtained at a',
     +' confidence level of ',CONFID
      WRITE(*,'(/,A)')
     +'    I   KS   KB    K    ALSMIN    ALSPLS    ALSUPR'
C loop over simulated experiments
      DO 20 I=1,NEXP
        K=0
        KS=0
        KB=0
        DO 10 J=1,N
          CALL RNECUY(R,1)
          IF(R.LT.P)THEN
            K=K+1
            IF(R.LT.PSIGN)THEN
              KS=KS+1
            ELSE
              KB=KB+1
            END IF
          END IF
10      CONTINUE
C compute confidence limits
        CALL SMERSS(K,CONFID,ALBACK,ALSMIN,ALSPLS,ALSUPR)
C output
        WRITE(*,'(4I5,3F10.5)')I,KS,KB,K,ALSMIN,ALSPLS,ALSUPR
20    CONTINUE
      END
