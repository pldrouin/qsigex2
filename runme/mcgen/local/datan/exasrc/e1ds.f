      PROGRAM E1DS
C simulates Example 5.1
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION R(1000)
C identify program to user
      WRITE(*,*)' Program E1DS simulates empirical frequency'
      WRITE(*,*)' and demonstrates statistical fluctuations'
      WRITE(*,*)' '
C ask for parameters
      WRITE(*,*)' Enter number of flies NFLY (>0)'
      WRITE(*,*)' >'
      READ(*,*) NFLY
      WRITE(*,*)' Enter number of experiments NEXP (>0)'
      WRITE(*,*)' >'
      READ(*,*) NEXP
      WRITE(*,*)' Enter probability P(A), (0.<P(A)<1.)'
      WRITE(*,*)' >'
      READ(*,*) P
C loop over all experiments
      DO 30 IEXP=1,NEXP
        NNFLY=NFLY
        NA=0
C loop over all flies
10      NT=MIN(NNFLY,1000)
        CALL RNECUY(R,NT)
        DO 20 IT=1,NT
          IF(R(IT).LT.P) NA=NA+1
20      CONTINUE
        NNFLY=NNFLY-NT
        IF(NNFLY.GT.0) THEN
          GO TO 10
        ELSE
          HA=DBLE(NA)/DBLE(NFLY)
          WRITE(*,1000)
     *    ' IEXP = ',IEXP,' ,  NA = ',NA,' ,  HA = ',HA
1000      FORMAT(1X,A,I3,A,I5,A,F10.8)
        END IF
30    CONTINUE
      END
