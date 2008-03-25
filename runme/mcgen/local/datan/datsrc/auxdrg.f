      SUBROUTINE AUXDRG(X,ETA,MM,N,NR,NRED,LIST,E,OK)
C Computes the derivative f'(x) of f(x) at x = X. Based on
C H. Rutishauser, Ausdehnung des Rombergschen Prinzips
C (Extension of Romberg's Principle), Numer. Math. 5 (1963) 48-54
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (EPS = 5.D-8, EPSI=1.D-10, DELTA=10.D0, S=1.D-1)
      DIMENSION X(NR),ETA(N),E(MM,NRED+N),LIST(NR)
      DIMENSION DX(0:9),W(0:9,3),T(0:9,0:9),A(0:9)
      LOGICAL LEV(0:9),LMT
      LOGICAL OK,LX
      DOUBLE PRECISION LSQGFN
      DATA DX /0.0256D0, 0.0192D0, 0.0128D0, 0.0096D0, 0.0064D0,
     +0.0048D0, 0.0032D0, 0.0024D0, 0.0016D0, 0.0012D0/
      DATA (LEV(K),K=0,8,2) /5*.TRUE./
      DATA (LEV(K),K=1,9,2) /5*.FALSE./
      DATA W(1,1) /1.33333 33333 333333D+00/
      DATA W(3,1) /1.06666 66666 666667D+00/
      DATA W(5,1) /1.01587 30158 730159D+00/
      DATA W(7,1) /1.00392 15686 274510D+00/
      DATA W(2,1) /3.33333 33333 333333D-01/
      DATA W(4,1) /6.66666 66666 666667D-02/
      DATA W(6,1) /1.58730 15873 015873D-02/
      DATA W(8,1) /3.92156 86274 509804D-03/
      DATA W(0,2) /2.28571 42857 142857D+00/
      DATA W(2,2) /1.16363 63636 363636D+00/
      DATA W(4,2) /1.03643 72469 635628D+00/
      DATA W(6,2) /1.00886 69950 738916D+00/
      DATA W(8,2) /1.00220 21042 329337D+00/
      DATA W(1,2) /1.28571 42857 142857D+00/
      DATA W(3,2) /1.63636 36363 636364D-01/
      DATA W(5,2) /3.64372 46963 562753D-02/
      DATA W(7,2) /8.86699 50738 916256D-03/
      DATA W(9,2) /2.20210 42329 336922D-03/
      DATA W(0,3) /1.80000 00000 000000D+00/
      DATA W(2,3) /1.12500 00000 000000D+00/
      DATA W(4,3) /1.02857 14285 714286D+00/
      DATA W(6,3) /1.00699 30069 930070D+00/
      DATA W(8,3) /1.00173 91304 347826D+00/
      DATA W(1,3) /8.00000 00000 000000D-01/
      DATA W(3,3) /1.25000 00000 000000D-01/
      DATA W(5,3) /2.85714 28571 428571D-02/
      DATA W(7,3) /6.99300 69930 069930D-03/
      DATA W(9,3) /1.73913 04347 826087D-03/
      OK=.TRUE.
      L=NR+N
      DO 90 IM=1,MM
        I2=0
        DO 80 IL=1,L
          DEL=DELTA
          IF(IL.LE.NR) THEN
            IF(LIST(IL).NE.0) THEN
              I2=I2+1
              LX=.TRUE.
              XSAV=X(IL)
            ELSE
              GO TO 80
            END IF
          ELSE
            I2=I2+1
            LX=.FALSE.
            IY=IL-NR
            XSAV=ETA(IY)
          END IF
          DO 40 I=1,10
            DEL=S*DEL
            IF(I.EQ. 10.OR. ABS(XSAV+DEL*DX(9)-XSAV) .LT. EPS) THEN
              OK=.FALSE.
              RETURN
            END IF
            DO 10 K = 0,9
              H=DEL*DX(K)
              IF(LX) THEN
                X(IL)=XSAV+H
              ELSE
                ETA(IY)=XSAV+H
              END IF
              FPLUS=LSQGFN(ETA,X,N,NR,IM)
              IF(LX) THEN
                X(IL)=XSAV-H
              ELSE
                ETA(IY)=XSAV-H
              END IF
              FMINUS=LSQGFN(ETA,X,N,NR,IM)
              IF(LX) THEN
                X(IL)=XSAV
              ELSE
                ETA(IY)=XSAV
              END IF
              T(K,0)=(FPLUS-FMINUS)/(H+H)
              A(K)=T(K,0)
   10       CONTINUE
            IF(A(0) .GE. A(9)) THEN
              DO 20 K = 0,9
                A(K)=-A(K)
   20         CONTINUE
            END IF
            LMT=.TRUE.
            DO 30 K = 1,9
              H=A(K-1)-A(K)
              LMT=LMT .AND. (H.LE.EPSI .OR. ABS(H).LE.EPS*ABS(A(K))
     +        +EPSI)
   30       CONTINUE
            IF(LMT) GO TO 50
   40     CONTINUE
   50     CONTINUE
          DO 70 M = 1,9
            DO 60 K = 0,9-M
              IF(LEV(M)) THEN
                T(K,M)=W(M-1,1)*T(K+1,M-1)-W(M,1)*T(K,M-1)
              ELSE IF(LEV(K)) THEN
                T(K,M)=W(M-1,2)*T(K+1,M-1)-W(M,2)*T(K,M-1)
              ELSE
                T(K,M)=W(M-1,3)*T(K+1,M-1)-W(M,3)*T(K,M-1)
              END IF
   60       CONTINUE
   70     CONTINUE
          E(IM,I2)=T(0,9)
   80   CONTINUE
   90 CONTINUE
      END
