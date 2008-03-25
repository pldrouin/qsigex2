      PROGRAM S5LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXNR=2,MAXN=21,N=21,T0=-3.D0,DELTAT=0.3D0)
      DIMENSION T(MAXN),DELTAY(MAXN),CX(MAXNR,MAXNR),A(MAXN,MAXNR)
      DIMENSION Y(MAXN),X(MAXNR),SCRAT(MAXNR,MAXNR)
      DIMENSION DXPLUS(MAXNR),DXMINS(MAXNR),XPL(2),YPL(2)
      CHARACTER*75 CAPT,STRING
      DIMENSION LIST(MAXNR)
      EXTERNAL BWFNCT,GSFNCT
      DOUBLE PRECISION BWFNCT,GSFNCT
      DATA NR /2/
C identify program to user
      WRITE(*,*)' Program S5LSQ generates data'
      WRITE(*,*)' corresponding to a Breit-Wigner law'
      WRITE(*,*)' and fits a Breit-Wigner  OR a Gaussian.'
      WRITE(*,*)' It shows fitted parameters with symmetric'
      WRITE(*,*)' and asymmetric errors and confidence region'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter size of measurement errors (> 0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMA
      WRITE(*,*)' Choose'
      WRITE(*,*)' 1 - Fit to Breit-Wigner function'
      WRITE(*,*)' 2 - Fit to Gaussian'
      WRITE(*,*)' >'
      READ(*,*)ISWIT
C ask for number of workstation
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C generate data points corresponding to Breit-Wigner
      X(1)=0.0D0
      X(2)=1.0D0
      CALL RNSTNR(DELTAY,N)
      DO 10 I=1,N
        T(I)=T0+DBLE(I-1)*DELTAT
        Y(I)=BWFNCT(X,NR,T(I))
        Y(I)=Y(I)+DELTAY(I)*SIGMA
        DELTAY(I)=SIGMA
10    CONTINUE
C set first approximation of unknowns
      X(1)=0.5D0
      X(2)=0.5D0
C perform fit
      NSTEP=100
      IF(ISWIT.EQ.1) THEN
        CALL LSQNON(BWFNCT,T,Y,DELTAY,N,NR,NR,LIST,X,CX,R,A,SCRAT,
     +  NSTEP)
      ELSE
        CALL LSQNON(GSFNCT,T,Y,DELTAY,N,NR,NR,LIST,X,CX,R,A,SCRAT,
     +  NSTEP)
      END IF
      P=1.D0-SCCHI2(R,N-NR)
      IF(NSTEP.LT.0) THEN
        WRITE(*,'(A,I5)')' LSQNON ended with NSTEP = ',NSTEP
      ELSE
C compute asymmetric errors
        NSTEP=100
        IF(ISWIT.EQ.1)THEN
          CALL LSQASN(BWFNCT,T,Y,DELTAY,N,NR,NR,LIST,X,CX,R,0.D0,
     +    DXPLUS,DXMINS,A,SCRAT,NSTEP)
        ELSE
          CALL LSQASN(GSFNCT,T,Y,DELTAY,N,NR,NR,LIST,X,CX,R,0.D0,
     +    DXPLUS,DXMINS,A,SCRAT,NSTEP)
        END IF
C prepare graphic
        CAPT='x_1#=******, x_2=#******, M=******, P=********'
        WRITE(CAPT(6:11),'(F6.2)')X(1)
        WRITE(CAPT(19:24),'(F6.2)')X(2)
        WRITE(CAPT(29:34),'(F6.2)')R
        WRITE(CAPT(39:46),'(F8.6)')P
        IF(ISWIT.EQ.1) THEN
          WRITE(CAPT(47:69),'(A)')' (fit to Breit-Wigner)'
          ELSE
          WRITE(CAPT(47:69),'(A)')' (fit to Gaussian)    '
        END IF
        LCAPT=69
        DX1=SQRT(CX(1,1))
        DX2=SQRT(CX(2,2))
        RHO=CX(1,2)/(DX1*DX2)
        XMIN=X(1)-2.D0*DX1
        YMIN=X(2)-2.D0*DX2
        XMAX=X(1)+2.D0*DX1
        YMAX=X(2)+2.D0*DX2
        CALL GROPEN
        CALL GROPWS(NWS)
        CALL GRWNCC(XMIN,XMAX,YMIN,YMAX)
        CALL GRVWWC(0.2D0,0.9D0,0.17D0,0.87D0)
        CALL GRWNWC(-.414D0,1.D0,0.D0,1.D0)
        CALL GRFRAM
        CALL GRBOUN
        CALL GRTXTC(1.D0,CAPT,LCAPT)
        STRING='x_1'
        CALL GRSCLX(STRING,3)
        STRING='x_2'
        CALL GRSCLY(STRING,3)
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
20      CONTINUE
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
30      CONTINUE
        FCONT=R+1.D0
        CALL GRSTCL(4)
        NX=100
        NY=100
        DX=(XMAX-XMIN)/NX
        DY=(YMAX-YMIN)/NY
        IF(ISWIT.EQ.1) THEN
          CALL LSQCON(1,2,XMIN,YMIN,DX,DY,NX,NY,FCONT,X,T,Y,DELTAY,
     +    N,NR,BWFNCT)
        ELSE
          CALL LSQCON(1,2,XMIN,YMIN,DX,DY,NX,NY,FCONT,X,T,Y,DELTAY,
     +    N,NR,GSFNCT)
        END IF
        CALL GRCLSE
      END IF
      END
C-------------------------------------------------
      DOUBLE PRECISION FUNCTION BWFNCT(X,NR,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*)
      BWFNCT=2.D0*X(2)**2/((3.14159*X(2))*(4.D0*(T-X(1))**2+X(2)**2))
      END
C-------------------------------------------------
      DOUBLE PRECISION FUNCTION GSFNCT(X,NR,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION X(*)
      GSFNCT=(.39894228D0/X(2))*EXP(-0.5D0*((T-X(1))/X(2))**2)
      END
