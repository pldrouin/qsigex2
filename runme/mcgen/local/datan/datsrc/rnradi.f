      SUBROUTINE RNRADI(A,TAU1,TAU2,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(3)
      CALL RNECUY (R,3)
      IF(R(1).LT.A) THEN
C Mean life is TAU1
        T=-TAU1*LOG(R(2))
      ELSE
C Mean life is TAU2
        T=-TAU2*LOG(R(3))
      END IF
      END
