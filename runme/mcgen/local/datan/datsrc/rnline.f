      SUBROUTINE RNLINE(A,B,T0,DT,N,SIGMAY,T,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION T(N), Y(N)
      DO 10 I =  1,N
        T(I)=T0+(I-1)*DT
        Y(I)=A*T(I)+B
        CALL RNSTNR(R,1)
        Y(I)=Y(I)+R*SIGMAY
   10 CONTINUE
      END
