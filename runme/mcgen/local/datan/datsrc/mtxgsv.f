      SUBROUTINE MTXGSV(U,V,N,NRED,LIST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION U(N),V(NRED),LIST(N)
      IRED=0
      DO 10 I=1,N
        IF(LIST(I).EQ.1) THEN
          IRED=IRED+1
          V(IRED)=U(I)
        END IF
   10 CONTINUE
      END
