      SUBROUTINE SMERQS(K,ID,CONFID,RBACK,RSMIN,RSPLS,RSUPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(ZERO=0.D0,ONE=1.D0,HALF=0.5D0,EPSILN=1.D-6)
      COMMON /CMSMER/ CALPHA
      EXTERNAL SMZRQS
      ALPHA=ONE-CONFID
C compute lower limit RSMIN of confidence region
      IF(K.EQ.0) THEN
        RSMIN=ZERO
      ELSE
        KK=K
        CALPHA=ONE-HALF*ALPHA
        RS0=ZERO
        RS1=ONE
        IF(K.GT.0)RS1=DBLE(K)
        CALL AUXZBR(RS0,RS1,SMZRQS,RBACK,KK,ID)
        CALL AUXZFN(RS0,RS1,RSMIN,SMZRQS,RBACK,KK,ID,EPSILN)
      END IF
      CALPHA=HALF*ALPHA
C compute upper limit RSPLS of confidence region
      KK=K+1
      RS0=ZERO
      RS1=DBLE(K)
      CALL AUXZBR(RS0,RS1,SMZRQS,RBACK,KK,ID)
      CALL AUXZFN(RS0,RS1,RSPLS,SMZRQS,RBACK,KK,ID,EPSILN)
C compute upper limit RSUPR of of parameter
      CALPHA=ALPHA
      KK=K+1
      RS0=ZERO
      RS1=DBLE(K)
      CALL AUXZBR(RS0,RS1,SMZRQS,RBACK,KK,ID)
      CALL AUXZFN(RS0,RS1,RSUPR,SMZRQS,RBACK,KK,ID,EPSILN)
      END
C-----------------------------------------------------------------
      DOUBLE PRECISION FUNCTION SMZRQS(RS,RB,K,ID)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /CMSMER/ CALPHA
      PARAMETER(ZERO=0.D0,ONE=1.D0,EPSILN=1.D-6)
      N=K-1+ID
      DEN=ONE+RS+RB
      PS=RS/DEN
      PB=RB/DEN
      IF(PB.LT.EPSILN) THEN
        DEN=ONE
      ELSE
        DEN=SCBINM(K,N,PB)
      END IF
      G=GLNGAM(DBLE(N+1))
      F=ZERO
      DO 20 NB=0,K-1
        DO 10 NS=0,K-NB-1
          IF(NS.EQ.0) THEN
            ES=ONE
          ELSE
            ES=PS**NS
          END IF
          IF(NB.EQ.0) THEN
            EB=ONE
          ELSE
            IF(PB.LT.EPSILN) THEN
              EB=ZERO
            ELSE
              EB=PB**NB
            END IF
          END IF
          IF(N-NS-NB.EQ.0) THEN
            E=ONE
          ELSE
            E=(ONE-PS-PB)**(N-NS-NB)
          END IF
          F=F+EXP(G-GLNGAM(DBLE(NS+1))-GLNGAM(DBLE(NB+1))-
     +            GLNGAM(DBLE(N-NS-NB+1)))*ES*EB*E
   10   CONTINUE
   20 CONTINUE
      SMZRQS=CALPHA-F/DEN
      END
