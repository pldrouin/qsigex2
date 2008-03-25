      PROGRAM E1TEST
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R1(1000),R2(1000)
      CHARACTER*15 TEXT
C identify program to user
      WRITE(*,*)' Program E1TEST simulates 2 Gaussian samples'
      WRITE(*,*)' and performs F-test for equality of variances'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter number NEPX of experiments (>0)'
      WRITE(*,*)' >'
      READ(*,*)NEXP
      WRITE(*,*)
     +' Enter number N1 of elements in 1st sample (1 <= N1 <= 1000)'
      WRITE(*,*)' >'
      READ(*,*)N1
      WRITE(*,*)
     +' Enter number N2 of elements in 2nd sample (1 <= N2 <= 1000)'
      WRITE(*,*)' >'
      READ(*,*)N2
      WRITE(*,*)
     +' Enter standard dev. SIGMA1 for in 1st sample (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA1
      WRITE(*,*)
     +' Enter standard dev. SIGMA2 for in 2nd sample (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA2
      WRITE(*,*)' Enter confidence limit CONFID (0.<CONFID<1.)'
      WRITE(*,*)' >'
      READ(*,*)CONFID
      C=0.5D0*(1.D0+CONFID)
C write heading for table
      WRITE(*,*)
     +' IEXP   NG   NK     SGSQ        SKSQ        F           Q'
C loop over all experiments
      DO 30 IEXP=1,NEXP
C simulate samples R1 and R2
        CALL RNSTNR(R1,N1)
        DO 10 I=1,N1
          R1(I)=R1(I)*SIGMA1
10      CONTINUE
        CALL RNSTNR(R2,N2)
        DO 20 I=1,N2
          R2(I)=R2(I)*SIGMA2
20      CONTINUE
C compute sample variances and F quotient
        CALL SMMNVR(R1,N1,XMEAN,DELXM,S1SQ,DELS2,S,DELS)
        CALL SMMNVR(R2,N2,XMEAN,DELXM,S2SQ,DELS2,S,DELS)
        IF(S1SQ.GT.S2SQ)THEN
          SGSQ=S1SQ
          SKSQ=S2SQ
          NG=N1
          NK=N2
        ELSE
          SKSQ=S1SQ
          SGSQ=S2SQ
          NK=N1
          NG=N2
        END IF
        F=SGSQ/SKSQ
        Q=SQFTST(C,NG-1,NK-1)
        IF(F.GT.Q) THEN
          TEXT=' F-test failed'
        ELSE
          TEXT=' '
        END IF
C output
        WRITE(*,'(1X,3I5,4F12.5,A)') IEXP,NG,NK,SGSQ,SKSQ,F,Q,TEXT
30    CONTINUE
      END
