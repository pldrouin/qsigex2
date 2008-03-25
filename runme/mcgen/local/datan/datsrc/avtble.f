      SUBROUTINE AVTBLE(X,NI,NJ,NK,XB,XBI,XBJ,XBIJ,Q,S,F,NDF,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(NI,NJ,NK),XBI(NI),XBJ(NJ),XBIJ(NI,NJ),Q(6),DF(6)
      DIMENSION S(6),F(4),NDF(6),A(4)
      PARAMETER(ZERO=0.D0, ONE=1.D0)
      AI=DBLE(NI)
      AK=DBLE(NK)
      AJ=DBLE(NJ)
C compute means
      XB=ZERO
      DO 10 I=1,NI
        XBI(I)=ZERO
   10 CONTINUE
      DO 20 J=1,NJ
        XBJ(J)=ZERO
        DO 20 I=1,NI
          XBIJ(I,J)=ZERO
   20 CONTINUE
      DO 40 I=1,NI
        DO 40 J=1,NJ
          DO 30 K=1,NK
            XBIJ(I,J) =XBIJ(I,J)+X(I,J,K)
   30     CONTINUE
          XBIJ(I,J) =XBIJ(I,J)/AK
   40 CONTINUE
      DO 60 I=1,NI
        DO 50 J=1,NJ
          XBI(I)=XBI(I)+XBIJ(I,J)
   50   CONTINUE
        XBI(I)=XBI(I)/AJ
   60 CONTINUE
      DO 80 J=1,NJ
        DO 70 I=1,NI
          XBJ(J)=XBJ(J)+XBIJ(I,J)
   70   CONTINUE
        XBJ(J)=XBJ(J)/AI
        XB=XB+XBJ(J)
   80 CONTINUE
      XB=XB/AJ
C compute sums of squares
      DO 90 L=1,6
        Q(L)=ZERO
   90 CONTINUE
      DO 100 I=1,NI
        Q(1)=Q(1)+(XBI(I)-XB)**2
  100 CONTINUE
      Q(1)=AJ*AK*Q(1)
      DO 110 J=1,NJ
        Q(2)=Q(2)+(XBJ(J)-XB)**2
  110 CONTINUE
      Q(2)=AI*AK*Q(2)
      DO 120 I=1,NI
        DO 120 J=1,NJ
          Q(3)=Q(3)+(XBIJ(I,J)+XB-XBI(I)-XBJ(J))**2
          DO 120 K=1,NK
            Q(5)=Q(5)+(X(I,J,K)-XBIJ(I,J))**2
            Q(6)=Q(6)+(X(I,J,K)-XB)**2
  120 CONTINUE
      Q(3) =AK*Q(3)
      Q(4)=Q(2)+Q(3)
C determine degrees of freedom
      DF(1)=AI-ONE
      DF(2)=AJ-ONE
      DF(3)=(AI-ONE)*(AJ-ONE)
      DF(4)=DF(2)+DF(3)
      DF(5)=AI*AJ*(AK-ONE)
      DF(6)=AI*AJ*AK-ONE
C mean squares
      DO 130 L=1,6
        S(L)=Q(L)/DF(L)
  130 CONTINUE
C F quotients
      DO 140 L=1,4
        F(L)=S(L)/S(5)
  140 CONTINUE
C degrees of freedom as integers
      DO 150 L=1,6
        NDF(L)=NINT(DF(L))
  150 CONTINUE
C levels of significance
      DO 160 L=1,4
        A(L)=ONE-SCFTST(F(L),NDF(L),NDF(5))
  160 CONTINUE
      END
