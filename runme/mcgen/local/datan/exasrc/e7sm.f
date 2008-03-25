      PROGRAM E7SM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C identify program to user
      WRITE(*,*)
     +' Program E7SM simulates experiments with few events'
      WRITE(*,*)' and reference events'
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
      WRITE(*,*)
     +' Enter Poisson parameter ALREF for reference evts (>0.)'
      WRITE(*,*)' >'
      READ(*,*)ALREF
      WRITE(*,*)' Enter confidence limit CONFID (0.<CONFID<1.)'
      WRITE(*,*)' >'
      READ(*,*)CONFID
      PSIGN=ALSIGN/DBLE(N)
      PBACK=ALBACK/DBLE(N)
      PREF=ALREF/DBLE(N)
      RSIGN=PSIGN/PREF
      RBACK=PBACK/PREF
      P=PSIGN+PBACK+PREF
C write table header
      WRITE(*,'(/,I5,A)')NEXP,' experiments will be simulated with'
      WRITE(*,1000)' ALSIGN = ',ALSIGN,' i.e. PSIGN = ',PSIGN,
     +' and RSIGN = ',RSIGN
      WRITE(*,1000)' ALBACK = ',ALBACK,' i.e. PBACK = ',PBACK,
     +' and RBACK = ',RBACK
      WRITE(*,1000)' ALREF  = ',ALREF,' i.e. PREF  = ',PREF
1000  FORMAT(A,F10.5,A,F10.8,A,F10.8)
      WRITE(*,'(A,A,F5.3)')' Limits for RSIGN are obtained at a',
     +' confidence level of ',CONFID
      WRITE(*,'(/,A)')
     +'    I   KS   KB    K   ID     RSMIN     RSPLS     RSUPR'
C loop over simulated experiments
      DO 20 I=1,NEXP
        KS=0
        KB=0
        ID=0
        DO 10 J=1,N
          CALL RNECUY(R,1)
          IF(R.LT.P)THEN
            IF(R.LT.PSIGN)THEN
              KS=KS+1
            ELSE IF(R.GE.PSIGN .AND. R.LT.PSIGN+PBACK)THEN
              KB=KB+1
            ELSE
              ID=ID+1
            END IF
          END IF
10      CONTINUE
        K=KS+KB
C compute confidence limits
        CALL SMERQS(K,ID,CONFID,RBACK,RSMIN,RSPLS,RSUPR)
C output
        WRITE(*,'(5I5,3F10.5)')I,KS,KB,K,ID,RSMIN,RSPLS,RSUPR
20    CONTINUE
      END
