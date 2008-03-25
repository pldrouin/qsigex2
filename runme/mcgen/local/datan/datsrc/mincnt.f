      SUBROUTINE MINCNT(IX,JX,XI0,XJ0,DXI,DXJ,NXI,NXJ,FCONT,X0,N,
     +USERFN)
      PARAMETER(MAXN=50)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X0(N)
      DIMENSION XI(2),XJ(2),F(2,2),X0SAV(MAXN)
      EXTERNAL USERFN
      CALL MTXCPV(X0,X0SAV,N)
      DO 40 I=0,NXI-1
        XI(1)=XI0+I*DXI
        XI(2)=XI(1)+DXI
        DO 30 J=0,NXJ-1
          XJ(1)=XJ0+J*DXJ
          XJ(2)=XJ(1)+DXJ
          DO 20 IS=1,2
            DO 10 JS=1,2
              CALL MTXCPV(X0SAV,X0,N)
              X0(IX)=XI(IS)
              X0(JX)=XJ(JS)
              F(IS,JS)=USERFN(X0,N)
   10       CONTINUE
   20     CONTINUE
          CALL GRPXCT(XI(1),XI(2),XJ(1),XJ(2),F(1,1),F(1,2),F(2,1),
     +    F(2,2),FCONT)
   30   CONTINUE
   40 CONTINUE
      CALL MTXCPV(X0SAV,X0,N)
      END
