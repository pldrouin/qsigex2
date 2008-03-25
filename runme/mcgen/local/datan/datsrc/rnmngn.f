        SUBROUTINE RNMNGN(DPLUS,A,X,N)
C generation of random numbers from multivaraiate normal
        IMPLICIT DOUBLE PRECISION(A-H,O-Z)
        PARAMETER(NMAX=10)
        DIMENSION R(NMAX)
        DIMENSION DPLUS(N,N),A(N),X(N)
        CALL RNSTNR(R,N)
        CALL MTXMLT(DPLUS,R,X,N,N,1)
        CALL MTXADD(X,A,X,N,1)
        END
