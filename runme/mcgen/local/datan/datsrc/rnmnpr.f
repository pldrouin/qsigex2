        SUBROUTINE RNMNPR(C,DPLUS,N)
C prepares for generation of random numbers from multivaraiate normal
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        PARAMETER(NMAX=10)
        DIMENSION B(NMAX,NMAX),D(NMAX,NMAX),R(NMAX)
        DIMENSION C(N,N),DPLUS(N,N)
        LOGICAL OK
        CALL MTXTRA(C,B,N,N)
        CALL MTXCHI(B,D,N)
        CALL MTXCHL(B,D,N)
        CALL MTXUNT(B,N)
        CALL MTXSVD(D,B,DPLUS,R,N,N,N,0.D0,OK)
        END
