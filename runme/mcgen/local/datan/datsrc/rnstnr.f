      SUBROUTINE RNSTNR(R,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION R(N), U(2)
      I=1
   10 CONTINUE
      CALL RNECUY(U,2)
      V1=2.*U(1)-1.
      V2=2.*U(2)-1.
      S=V1*V1+V2*V2
      IF(S.GE.1.) GO TO 10
      ROOT=SQRT(-2.*LOG(S)/S)
      R(I)=V1*ROOT
      IF(I.LT.N) R(I+1)=V2*ROOT
      I=I+2
      IF(I.LE.N) GO TO 10
      END
