      SUBROUTINE MTXUNT(R,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(N,N)
      DO 20 J=1,N
        DO 10 I=1,N
          IF(I.EQ.J) THEN
            R(I,J)=1.D0
          ELSE
            R(I,J)=0.D0
          END IF
   10   CONTINUE
   20 CONTINUE
      END
