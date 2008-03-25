      SUBROUTINE LSQCON(IX,JX,XI0,XJ0,DXI,DXJ,NXI,NXJ,FCONT,X0,T,Y,
     +DELTAY,N,NR,USERFN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X0(NR),T(N),Y(N),DELTAY(N)
      DIMENSION XI(2),XJ(2),F(2,2),X0SAV(100)
      EXTERNAL USERFN
      CALL MTXCPV(X0,X0SAV,NR)
      DO 50 I=0,NXI-1
        XI(1)=XI0+I*DXI
        XI(2)=XI(1)+DXI
        DO 40 J=0,NXJ-1
          XJ(1)=XJ0+J*DXJ
          XJ(2)=XJ(1)+DXJ
          DO 30 IS=1,2
            DO 20 JS=1,2
              CALL MTXCPV(X0SAV,X0,NR)
              X0(IX)=XI(IS)
              X0(JX)=XJ(JS)
              R=0.D0
              DO 10 L=1,N
                R=R+((Y(L)-USERFN(X0,NR,T(L)))/DELTAY(L))**2
   10         CONTINUE
              F(IS,JS)=R
   20       CONTINUE
   30     CONTINUE
          CALL GRPXCT(XI(1),XI(2),XJ(1),XJ(2),F(1,1),F(1,2),F(2,1),
     +    F(2,2),FCONT)
   40   CONTINUE
   50 CONTINUE
      END
