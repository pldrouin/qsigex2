      PROGRAM S7LSQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(MAXN=40,MAXNR=3,MAXM=20)
      COMMON /COM/ X(MAXNR),Y(MAXN),CY(MAXN,MAXN),
     +GY(MAXN,MAXN),CX(MAXNR,MAXNR),E(MAXM,MAXN+MAXNR),
     +F(MAXN+MAXNR,MAXN+MAXNR),A2(MAXN+MAXNR,MAXN+MAXNR),
     +T(MAXM),S(MAXM),DT(MAXM),DS(MAXM),RHO(MAXM),
     +A(2),C(2,2),DPLUS(2,2),RANDOM(2),LIST(3)
C COMMON needed for Microsoft FORTRAN compiler
      CHARACTER*75 STRING,CAPT
      DIMENSION XPL(100),YPL(100)
C identify program to user
      WRITE(*,*)' Program S7LSQ generates data'
      WRITE(*,*)' on unit circle with (correlated) errors'
      WRITE(*,*)' on both coordinates and performs fit'
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)
     +' Enter number n of points (3 <= n <= 20)to be generated'
      WRITE(*,*)' >'
      READ(*,*)M
      WRITE(*,*)' Enter measurement error in abscissa (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMAT
      WRITE(*,*)' Enter measurement error in ordinate (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMAS
      WRITE(*,*)
     +' Enter measurement correlation coefficient (>-1., <1.)'
      WRITE(*,*)' >'
      READ(*,*)CORREL
C simulate data points
      C(1,1)=SIGMAT**2
      C(2,2)=SIGMAS**2
      C(1,2)=CORREL*SIGMAS*SIGMAT
      C(2,1)=C(1,2)
      NVAR=2
      CALL RNMNPR(C,DPLUS,NVAR)
      DELPHI=6.283185D0/DBLE(M)
      DO 5 I=1,M
        PHI=DBLE(I-1)*DELPHI
        CALL RNMNGN(DPLUS,A,RANDOM,2)
        T(I)=COS(PHI)+RANDOM(1)
        S(I)=SIN(PHI)+RANDOM(2)
        DT(I)=SIGMAT
        DS(I)=SIGMAS
        RHO(I)=CORREL
5     CONTINUE
C set up data for input to LSQGEN
      N=2*M
      NR=3
      NRED=3
      CALL MTXUNT(CY,N)
      DO 10 I=1,M
        Y((I-1)*2+1)=T(I)
        Y((I-1)*2+2)=S(I)
        CY((I-1)*2+1,(I-1)*2+1)=DT(I)**2
        CY((I-1)*2+2,(I-1)*2+2)=DS(I)**2
        CY((I-1)*2+1,(I-1)*2+2)=RHO(I)*DS(I)*DT(I)
        CY((I-1)*2+2,(I-1)*2+1)=RHO(I)*DS(I)*DT(I)
10    CONTINUE
C determine first approximation
      TA=0.5D0*(Y(1)+Y(3))
      SA=0.5D0*(Y(2)+Y(4))
      TB=0.5D0*(Y(3)+Y(5))
      SB=0.5D0*(Y(4)+Y(6))
      SLOPEA=-(Y(4)-Y(2))/(Y(3)-Y(1))
      SLOPEB=-(Y(6)-Y(4))/(Y(5)-Y(3))
      S0=(TA-TB-SLOPEA*SA+SLOPEB*SB)/(SLOPEB-SLOPEA)
      T0=TA+SLOPEA*(S0-SA)
      R0=SQRT((T0-Y(1))**2+(S0-Y(2))**2)
      X(1)=T0
      X(2)=S0
      X(3)=R0
      NSTEP=100
      CALL LSQGEN(Y,CY,GY,F,E,M,N,NR,NRED,LIST,X,CX,R,A2,NSTEP)
      P=1.D0-SCCHI2(R,M-NRED)
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
C prepare graphics
      CAPT=
     *'x_1#=******, x_2#=******, x_3#=******, M=******, P=******'
      WRITE(CAPT(6:11),'(F6.2)')X(1)
      WRITE(CAPT(19:24),'(F6.2)')X(2)
      WRITE(CAPT(32:37),'(F6.2)')X(3)
      WRITE(CAPT(42:47),'(F6.2)')R
      WRITE(CAPT(52:57),'(F6.4)')P
      LCAPT=57
C graphical output
      CALL GROPEN
      CALL GROPWS(NWS)
      CALL GRWNCC(-1.5D0,1.5D0,-1.5D0,1.5D0)
      CALL GRVWWC(0.2D0,0.9D0,0.17D0,0.87D0)
      CALL GRWNWC(-.414D0,1.D0,0.D0,1.D0)
      CALL GRFRAM
      CALL GRBOUN
      CALL GRTXTC(1.D0,CAPT,LCAPT)
      STRING='t'
      CALL GRSCLX(STRING,1)
      STRING='s'
      CALL GRSCLY(STRING,1)
      CALL GRSTCL(2)
      DO 30 I=1,M
        CALL GRDATP(1,.25D0,T(I),S(I),DT(I),DS(I),RHO(I))
30    CONTINUE
      NPL=100
      CALL GRSTCL(4)
C circle corresponding to first approximation
      DELPHI=6.283185D0/99.D0
      DO 50 IPL=1,NPL
        PHI=DBLE(IPL-1)*DELPHI
        XPL(IPL)=T0+R0*COS(PHI)
        YPL(IPL)=S0+R0*SIN(PHI)
50    CONTINUE
      CALL GRBRPL(1,.5D0,NPL,XPL,YPL)
C circle corresponding to solution
      DO 20 IPL=1,NPL
        PHI=DBLE(IPL-1)*DELPHI
        XPL(IPL)=X(1)+X(3)*COS(PHI)
        YPL(IPL)=X(2)+X(3)*SIN(PHI)
20    CONTINUE
      CALL GRPLIN(NPL,XPL,YPL)
      CALL GRCLSE
      END
C----------------------------------------------
      DOUBLE PRECISION FUNCTION LSQGFN(ETA,X,N,NR,K)
C fit to circle
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ETA(*),X(*)
      LSQGFN=(ETA(2*K-1)-X(1))**2+(ETA(2*K)-X(2))**2-X(3)**2
      END

