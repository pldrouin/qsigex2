      SUBROUTINE MINENC(XA,XB,XC,YA,YB,YC,USERFN,NSTEP,X0,XDIR,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(ZERO=0.D0,GSPAR=1.618034D0,FACMAG=10.D0,
     +MAXSTP=1000)
      DIMENSION X0(N),XDIR(N)
      DOUBLE PRECISION MINFND
      EXTERNAL USERFN
      YA=MINFND(XA,X0,XDIR,N,USERFN)
      YB=MINFND(XB,X0,XDIR,N,USERFN)
      NST=NSTEP
      IF (NST.LE.0) NST=MAXSTP
      IF (YB.GT.YA) THEN
C exchange A and B
        BUF=XA
        XA=XB
        XB=BUF
        BUF=YA
        YA=YB
        YB=BUF
      END IF
      XC=XB+GSPAR*(XB-XA)
      YC=MINFND(XC,X0,XDIR,N,USERFN)
      ISTEP=0
   10 IF (YB.GE.YC .AND. ISTEP.LT.NST) THEN
        ISTEP=ISTEP+1
C step was still downwards
        XEND=XB+FACMAG*(XC-XB)
        CALL MINPRB(XM,XA,YA,XB,YB,XC,YC)
        YM=MINFND(XM,X0,XDIR,N,USERFN)
        IF ((XM-XB)*(XC-XM) .GT. ZERO) THEN
C XM is between XB and XC
          IF (YM.LT.YC) THEN
C minimum is between XB and XC
            XA=XB
            YA=YB
            XB=XM
            YB=YM
            GO TO 10
          ELSE IF (YM.GT.YB) THEN
C minimum is between XA and XM
            XC=XM
            YC=YM
            GO TO 10
          END IF
C there was no minimum, go on
          XM=XC+GSPAR*(XC-XB)
        ELSE IF ((XC-XM)*(XM-XEND) .GT. ZERO) THEN
C XM is between XC and XEND
          IF (YM.LT.YC) THEN
            XB=XC
            XC=XM
            XM=XC+GSPAR*(XC-XB)
            YB=YC
            YC=YM
          END IF
        ELSE IF ((XM-XEND)*(XEND-XC) .GE. ZERO) THEN
C XM is beyond XEND
          XM=XEND
        ELSE
          XM=XC+GSPAR*(XC-XB)
        END IF
        XA=XB
        XB=XC
        XC=XM
        YA=YB
        YB=YC
        YC=MINFND(XC,X0,XDIR,N,USERFN)
        GO TO 10
      END IF
      IF(ISTEP.EQ.NST) THEN
        NSTEP=-1
      ELSE
        NSTEP=ISTEP
      END IF
      END
