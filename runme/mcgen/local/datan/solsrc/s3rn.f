      PROGRAM S3RN
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(NMAX=100)
      DIMENSION T(NMAX),Y(NMAX),DATSX(NMAX),DATSY(NMAX)
      DIMENSION DATCOV(NMAX),XPL(2),YPL(2)
      CHARACTER*1 TX,TY,CAPT
      PARAMETER(TX='t',TY='y',CAPT=' ')
C identify program to user
      WRITE(*,*)' Program S3RN demonstrates use of RNLNDF'
      WRITE(*,*)' which is a subroutine generating data points'
      WRITE(*,*)' along a straight line'
      WRITE(*,*)' with errors of different size'
      WRITE(*,*)' '
C ask for numerical input
      WRITE(*,*)' Enter number of points N (N<101)'
      WRITE(*,*)' >'
      READ(*,*)N
      WRITE(*,*)' Enter A'
      WRITE(*,*)' >'
      READ(*,*)A
      WRITE(*,*)' Enter B'
      WRITE(*,*)' >'
      READ(*,*)B
      WRITE(*,*)' Enter T0'
      WRITE(*,*)' >'
      READ(*,*)T0
      WRITE(*,*)' Enter DT'
      WRITE(*,*)' >'
      READ(*,*)DT
      WRITE(*,*)' Enter SIGMIN (>0.)'
      WRITE(*,*)' >'
      READ(*,*)SIGMIN
      WRITE(*,*)' Enter SIGMAX (>SIGMIN)'
      WRITE(*,*)' >'
      READ(*,*)SIGMAX
C simulate data points
      CALL RNLNDF(A,B,T0,DT,N,SIGMIN,SIGMAX,T,Y,DATSY)
      DO 10 I=1,N
          DATSX(I)=0.D0
          DATCOV(I)=0.D0
10    CONTINUE
      XPL(1)=T0-DT
      XPL(2)=T0+N*DT
      YPL(1)=A*XPL(1)+B
      YPL(2)=A*XPL(2)+B
C graphics
      CALL GRNBWS()
      WRITE(*,*)' Please, enter number of workstation:'
      WRITE(*,*)' >'
      READ(*,*)NWS
      CALL GRDTCV(XPL,YPL,2,1,.2D0,T,Y,DATSX,DATSY,DATCOV,N,TX,1,TY,
     +1,CAPT,1,NWS)
      END
C -----------------------------------------------------------
      SUBROUTINE RNLNDF(A,B,T0,DT,N,SIGMIN,SIGMAX,T,Y,SIGMAY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION T(N),Y(N),SIGMAY(N)
C generates data points (T(I),Y(I)) with errors of different size
      DO 10 I = 1 ,N
        T(I)=T0+(I-1)*DT
        Y(I)=A*T(I)+B
        CALL RNECUY(R,1)
        SIGMAY(I)=SIGMIN+R*(SIGMAX-SIGMIN)
        CALL RNSTNR(R,1)
        Y(I)=Y(I)+R*SIGMAY(I)
   10 CONTINUE
      END
