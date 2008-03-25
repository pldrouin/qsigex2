      SUBROUTINE LSQGEN(Y,CY,FY,F,E,M,N,NR,NRED,LIST,X,CX,R,
     +                  A2,NSTEP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION LSQGFN
      PARAMETER(LMAX=1050)
      DIMENSION Y(N),CY(N,N),X(NR),CX(NRED,NRED),LIST(NR)
      DIMENSION E(M,N+NRED),FY(N,N),F(N+NRED,N+NRED),A2(N+NRED,*)
      DIMENSION D(LMAX),T(LMAX),B(LMAX),U(LMAX)
      COMMON /DASV03/ D,T,B,U
      PARAMETER(MAXSTP=100,EPSILN=1.D-8,TT=1.D-15,ZERO=0.D0)
      LOGICAL OK,COVMAT
C general case of least squares fitting
      OK=.TRUE.
      COVMAT=.TRUE.
      IF(NSTEP.LT.0) THEN
        COVMAT=.FALSE.
        NSTEP=ABS(NSTEP)
      END IF
      IF(NSTEP.LT.1) NSTEP=MAXSTP
      IF(NR.GT.0) THEN
C For NR=NRED :  set LIST
        IF(NR.EQ.NRED) THEN
          DO 10 I=1,NR
            LIST(I)=1
   10     CONTINUE
        END IF
      END IF
      L=N+NRED
      CALL MTXZRV(T,L)
      CALL MTXTRA(CY,F,N,N)
      CALL MTXCHI(CY,FY,N)
      CALL MTXCHL(CY,FY,N)
      CALL MTXTRA(F,CY,N,N)
C start iteration
      R=ZERO
      DO 50 ISTEP=1,NSTEP
        RLST=R
        CALL MTXZER(F,L,L)
        CALL MTXPSM(F,FY,L,L,N,N,NRED+1,NRED+1)
        DO 20 K=1,M
          D(K)=-LSQGFN(Y,X,N,NR,K)
   20   CONTINUE
C Numerical Derivatives
        CALL AUXDRG(X,Y,M,N,NR,NRED,LIST,E,OK)
        IF(.NOT.OK) THEN
          NSTEP=-3
          GO TO 60
        END IF
        CALL MTXCHM(F,T,B,L,1)
        CALL MTXMSV(B,B,DBLE(-1.),L)
        CALL MTXLSC(F,B,E,D,U,R,A2,L,L,M,DBLE(0.),OK)
        IF(.NOT.OK) THEN
          NSTEP=-1
          GO TO 60
        END IF
        IF(NRED.GT.0) THEN
          IRED=0
          DO 30 I=1,NR
            IF(LIST(I).NE.0) THEN
              IRED=IRED+1
              X(I)=X(I)+U(IRED)
            END IF
   30     CONTINUE
        END IF
        DO 40 I=1,N
          Y(I)=Y(I)+U(I+NRED)
          T(I+NRED)=T(I+NRED)+U(I+NRED)
   40   CONTINUE
C test for convergence
        IF(ISTEP.GT. 1.AND. ABS(R-RLST).LT.EPSILN*R+TT) THEN
          NSTEP=ISTEP
          IF(COVMAT) THEN
C compute matrix GB
            CALL AUXDRG(X,Y,M,N,NR,NRED,LIST,E,OK)
            IF(.NOT.OK) THEN
              NSTEP=-3
              GO TO 60
            END IF
            CALL MTXGSM(E,A2,M,L,M,N,1,NRED+1)
            CALL MTXMBT(CY,A2,F,N,N,M)
            CALL MTXMLT(A2,F,FY,M,N,M)
            CALL MTXCHI(FY,F,M)
C array FY now contains matrix GB
            IF(NRED.GT.0) THEN
              CALL MTXGSM(E,A2,M,L,M,NRED,1,1)
              CALL MTXMAT(A2,FY,F,NRED,M,M)
              CALL MTXMLT(F,A2,CX,NRED,M,NR)
              CALL MTXCHI(CX,F,NRED)
C array CX now contains covariance matrix of unknowns
            ELSE
              CALL MTXMLT(A2,CY,F,M,N,N)
              CALL MTXMLT(FY,F,A2,M,M,N)
              CALL MTXMAT(F,A2,FY,N,M,N)
              CALL MTXSUB(CY,FY,CY,N,N)
C array CY now contains covariance matrix of 'improved'
C measurements
            END IF
          END IF
          GO TO 60
        END IF
   50 CONTINUE
      NSTEP=-2
   60 RETURN
      END
