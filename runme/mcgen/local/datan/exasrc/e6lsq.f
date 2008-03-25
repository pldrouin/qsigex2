      PROGRAM E6LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MAXN=20,MAXNR=2,N=20,NR=2)
      DIMENSION T(MAXN),Y(MAXN),DELTAY(MAXN),LIST(MAXNR),
     +X(MAXNR),CX(MAXNR,MAXNR),GX(MAXNR,MAXNR),A(MAXN,MAXNR),
     +DXPLUS(MAXNR),DXMINS(MAXNR),XPL(2),YPL(2)
      CHARACTER*75 STRING
      DOUBLE PRECISION LSQEXP
      EXTERNAL LSQEXP
C identify program to user
      WRITE(*,*)' Program E6LSQ demonstrates use of LSQASN'
      WRITE(*,*)' '
C create simulated data
      X(1)=10.D0
      X(2)=1.D0
      NSEED1=15
      NSEED2=211
      CALL RNE2IN(NSEED1,NSEED2)
      CALL RNSTNR(DELTAY,N)
      CALL RNECUY(Y,N)
      DELT=DBLE(2./21.)
      SIGMA=2.D0
      DO 10 I=1,N
        T(I)=DBLE(I)*DELT
        SIG=SIGMA*(DBLE(0.5)+Y(I))
        Y(I)=LSQEXP(X,NR,T(I))+DELTAY(I)*SIG
        DELTAY(I)=SIG
10    CONTINUE
C set first approximation of unknowns and perform fit with LSQNON
      NRED=NR
      X(1)=.1D0
      X(2)=.1D0
      LIST(1)=1
      LIST(2)=1
      NSTEP=100
      CALL LSQNON(LSQEXP,T,Y,DELTAY,N,NR,NRED,
     +LIST,X,CX,R,A,GX,NSTEP)
      NSTEP=100
C compute asymmetric errors
      CALL LSQASN(LSQEXP,T,Y,DELTAY,N,NR,NRED,LIST,X,CX,R,0.D0,
     +DXPLUS,DXMINS,A,GX,NSTEP)
C prepare graphics
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      CALL GROPEN
      CALL GROPWS(NWS)
      XMIN=5.D0
      YMIN=.5D0
      XMAX=15.D0
      YMAX=1.5D0
      CALL GRWNCC(XMIN,XMAX,YMIN,YMAX)
      CALL GRVWWC(0.2D0,0.9D0,0.2D0,0.9D0)
      CALL GRWNWC(-.414D0,1.D0,0.D0,1.D0)
      CALL GRFRAM
      CALL GRBOUN
      STRING='x_1'
      CALL GRSCLX(STRING,75)
      STRING='x_2'
      CALL GRSCLY(STRING,75)
      DX1=SQRT(CX(1,1))
      DX2=SQRT(CX(2,2))
      RHO=CX(1,2)/(DX1*DX2)
      CALL GRSTCL(2)
      CALL GRDATP(1,1.D0,X(1),X(2),DX1,DX2,RHO)
      CALL GRSTCL(3)
      DO 20 I=1,2
        IF(I.EQ.1) THEN
          XPL(1)=X(1)-DXMINS(1)
        ELSE
          XPL(1)=X(1)+DXPLUS(1)
        END IF
        XPL(2)=XPL(1)
        YPL(1)=YMIN
        YPL(2)=YMAX
        CALL GRPLIN(2,XPL,YPL)
20    CONTINUE
      DO 30 I=1,2
        IF(I.EQ.1) THEN
          YPL(1)=X(2)-DXMINS(2)
        ELSE
          YPL(1)=X(2)+DXPLUS(2)
        END IF
        YPL(2)=YPL(1)
        XPL(1)=XMIN
        XPL(2)=XMAX
        CALL GRPLIN(2,XPL,YPL)
30    CONTINUE
      FCONT=R+1.D0
      CALL GRSTCL(4)
      NX=100
      NY=100
      DX=(XMAX-XMIN)/NX
      DY=(YMAX-YMIN)/NY
C compute and draw contour with LSQCON
      CALL LSQCON(1,2,XMIN,YMIN,DX,DY,NX,NY,FCONT,
     +X,T,Y,DELTAY,N,NR,LSQEXP)
      CALL GRCLSE
      END
