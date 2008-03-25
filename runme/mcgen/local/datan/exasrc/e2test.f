      PROGRAM E2TEST
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R(1000)
      CHARACTER*15 TEXT
C identify program to user
      WRITE(*,*)' Program E2TEST simulates Gaussian sample'
      WRITE(*,*)' and performs t-test on mean'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number NEPX of experiments (>0)'
      WRITE(*,*)' >'
      READ(*,*)NEXP
      WRITE(*,*)' Enter number N of elements in sample (>0)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter mean X0 for sample'
      WRITE(*,*)' >'
      READ(*,*)X0
      WRITE(*,*)' Enter standard deviation SIGMA for sample (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
      WRITE(*,*)' Enter hypothesis AL0 for mean'
      WRITE(*,*)' >'
      READ(*,*)AL0
      WRITE(*,*)' Enter confidence limit CONFID (0.<CONFID<1.)'
      WRITE(*,*)' >'
      READ(*,*)CONFID
      C=0.5D0*(1.D0+CONFID)
      NF=N-1
      Q=SQSTUD(C,NF)
C write heading for table
      WRITE(*,*)' IEXP    N       XMEAN       t          Q'
C loop over all experiments
      DO 30 IEXP=1,NEXP
C simulate sample R
        CALL RNSTNR(R,N)
        DO 10 I=1,N
          R(I)=X0+R(I)*SIGMA
10      CONTINUE
C compute sample mean and variance and t
        CALL SMMNVR(R,N,XMEAN,DELXM,SSQ,DELS2,S,DELS)
        T=(XMEAN-AL0)*SQRT(DBLE(N)/SSQ)
        IF(ABS(T).GT.Q) THEN
          TEXT=' t-test failed'
        ELSE
          TEXT=' '
        END IF
C output
        WRITE(*,'(1X,2I5,3F12.5,A)')IEXP,N,XMEAN,T,Q,TEXT
30    CONTINUE
      END
