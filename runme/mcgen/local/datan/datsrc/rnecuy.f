      SUBROUTINE RNECUY(U,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(N)
      PARAMETER (M1=2147483563,M1MIN1=2147483562,IA1=40014,
     +           IQ1=53668,IR1=12211)
      PARAMETER (M2=2147483399,IA2=40692,IQ2=52774,IR2=2791)
      PARAMETER (XM1INV=4.656613D-10)
      DATA IX1,IX2 /123456, 654321/
      DO 10 I=1,N
C Produce integer random number X1 from first MLCG
        K=IX1/IQ1
        IX1=IA1*(IX1-K*IQ1)-K*IR1
        IF (IX1.LT.0) IX1=IX1+M1
C Produce integer random number X2 from second MLCG
        K=IX2/IQ2
        IX2=IA2*(IX2-K*IQ2)-K*IR2
        IF (IX2.LT.0) IX2=IX2+M2
C Combine
        IZ=IX1-IX2
        IF(IZ.LT.1) IZ=IZ+M1MIN1
C Normalize and transform to floating point
        U(I)=DBLE(IZ)*XM1INV
   10 CONTINUE
      RETURN
C Input of seed
      ENTRY RNE2IN(ISEED1,ISEED2)
      IX1=ISEED1
      IX2=ISEED2
      RETURN
C Output of seed
      ENTRY RNE2OT(ISEED1,ISEED2)
      ISEED1=IX1
      ISEED2=IX2
      RETURN
      END
