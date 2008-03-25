      PROGRAM E1SM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION SAMPLE(100)
C identify program to user
      WRITE(*,*)' Program E1SM demonstrates the use of SMMNVR'
      WRITE(*,*)' on 10 samples of 100 elements each taken'
      WRITE(*,*)' from the standardized normal distribution'
      WRITE(*,*)' '
C write table caption
      WRITE(*,*)
     +'    XMEAN     DELXM     S2        DELS2     S          DELS'
C perform loop over 10 simulated samples of 100 elements each
      DO 10 I=1,10
        CALL RNSTNR(SAMPLE,100)
        CALL SMMNVR(SAMPLE,100,XMEAN,DELXM,S2,DELS2,S,DELS)
        WRITE(*,'(6F10.5)')XMEAN,DELXM,S2,DELS2,S,DELS
10    CONTINUE
      END
