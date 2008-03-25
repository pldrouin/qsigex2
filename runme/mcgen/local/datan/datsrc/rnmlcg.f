      SUBROUTINE RNMLCG(U,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(N)
      PARAMETER (M=2147483563, IA=40014, IQ=53668, IR=12211)
      PARAMETER (XMINV=4.656613D-10)
      DATA IX /123456/
C Compute N random numbers
      DO 10 I=1,N
        K=IX/IQ
        IX=IA*(IX-K*IQ)-K*IR
        IF (IX.LT.0) IX=IX+M
        U(I)=DBLE(IX)*XMINV
   10 CONTINUE
      RETURN
C Input of seed
      ENTRY RNMSIN(ISEED)
      IX=ISEED
      RETURN
C Output of seed
      ENTRY RNMSOT(ISEED)
      ISEED=IX
      RETURN
      END
