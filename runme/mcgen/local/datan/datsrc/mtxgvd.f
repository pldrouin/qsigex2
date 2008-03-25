      SUBROUTINE MTXGVD(V1,V2,C,S)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      A1=ABS(V1)
      A2=ABS(V2)
      IF(A1.GT.A2) THEN
        W=V2/V1
        Q=SQRT(1.+W*W)
        C=1./Q
        IF(V1.LT.0.) C=-C
        S=C*W
      ELSE
        IF(V2.NE.0.) THEN
          W=V1/V2
          Q=SQRT(1.+W*W)
          S=1./Q
          IF(V2.LT.0.) S=-S
          C=S*W
        ELSE
          C=1.
          S=0.
        END IF
      END IF
      END
