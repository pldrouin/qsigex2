      SUBROUTINE LSQCOG(IX,JX,XI0,XJ0,DXI,DXJ,NXI,NXJ,FCONT,Y,CY,
     +GY,F,E,M,N,NR,X0,A2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION Y(N),CY(N,N),X0(NR),E(M,N),GY(N,N),F(N,N),A2(N,*)
      DIMENSION XI(2),FUN(2,2)
      PARAMETER(NRMAX=100,NMAX=1000)
      DIMENSION YSAV(NMAX),LIST(NRMAX)
      COMMON /DASV04/ YSAV
      DO 5 I=1,NR
        LIST(I)=0
    5 CONTINUE
      CALL MTXCPV(Y,YSAV,N)
      DO 40 I=0,NXI-1
        XI(1)=XI0+I*DXI
        XI(2)=XI(1)+DXI
        DO 30 J=0,NXJ
          XJ=XJ0+J*DXJ
          DO 20 IS=1,2
            CALL MTXCPV(YSAV,Y,N)
            X0(IX)=XI(IS)
            X0(JX)=XJ
            NSTEP=-100
            NRED=0
            CALL LSQGEN(Y,CY,GY,F,E,M,N,NR,NRED,LIST,X0,CX,R,A2,
     +      NSTEP)
            FUN(IS,2)=R
   20     CONTINUE
          IF (J.GT.0)
     +      CALL GRPXCT(XI(1),XI(2),XJO,XJ,FUN(1,1),FUN(1,2),
     +                  FUN(2,1),FUN(2,2),FCONT)
          XJO=XJ
          FUN(1,1)=FUN(1,2)
          FUN(2,1)=FUN(2,2)
   30   CONTINUE
   40 CONTINUE
      CALL MTXCPV(YSAV,Y,N)
      END
