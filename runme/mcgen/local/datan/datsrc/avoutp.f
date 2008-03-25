      SUBROUTINE AVOUTP(X,NI,NJ,NK,Q,S,F,NDF,ALPHA,NTYPE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(NI,NJ,NK),Q(6),S(6),F(4),NDF(6),SOURCE(6),
     +ALPHA(4)
      CHARACTER*4 SOURCE
      DATA SOURCE /'A   ','B   ','INT.','B(A)','W   ','TTL.'/
      WRITE (*,90)
      DO 10 I=1,NI
        DO 10 J=1,NJ
          WRITE (6,60)I,J,(X(I,J,K),K=1,NK)
   10 CONTINUE
      WRITE (6,70)
      WRITE (6,80) SOURCE(1),Q(1),NDF(1),S(1),F(1),ALPHA(1)
      IF(NTYPE.EQ.0) THEN
        DO 20 L=2,3
          WRITE (6,80) SOURCE(L),Q(L),NDF(L),S(L),F(L),ALPHA(L)
   20   CONTINUE
      ELSE
        WRITE (6,80) SOURCE(4),Q(4),NDF(4),S(4),F(4),ALPHA(4)
      END IF
      DO 30 L=5,6
        WRITE (6,80) SOURCE(L),Q(L),NDF(L),S(L)
   30 CONTINUE
C format statements
   40 FORMAT(4I10)
   50 FORMAT(10F10.5)
   60 FORMAT(' A(',I2,'), B(',I2,') ',(10F10.2))
   70 FORMAT(///10X,'Analysis of Variance Table' //
     +' Source      Sum of    Degrees of   Mean     F Ratio  ',
     +' Alpha'/'             Squares   Freedom      Square' //)
   80 FORMAT(3X,A4,2X,F10.2,5X,I3,5X,F10.2,F10.5,F15.12)
   90 FORMAT( '           DATA'/)
      END
