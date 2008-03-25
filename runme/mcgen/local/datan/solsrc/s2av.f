      PROGRAM S2AV
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER(NI=4,NJ=4,NK=6)
      DIMENSION BUFFER(NK)
      DIMENSION X(NI,NJ,NK),XBI(NI),XBJ(NJ),XBIJ(NI,NJ)
      DIMENSION Q(6),S(6),F(4),NDF(6),ALPHA(4)
C identify program to user
      WRITE(*,*)' program S2AV generates data of nested classification'
      WRITE(*,*)' and performs analysis of variance on them '
      WRITE(*,*)' '
C ask for input
      WRITE(*,*)' Enter A'
      WRITE(*,*)' > '
      READ(*,*)A
      WRITE(*,*)' Enter B'
      WRITE(*,*)' > '
      READ(*,*)B
      WRITE(*,*)' Enter MU'
      WRITE(*,*)' > '
      READ(*,*)AMU
      WRITE(*,*)' Enter SIGMA (>0.)'
      WRITE(*,*)' > '
      READ(*,*)SIGMA
      WRITE(*,*)' Enter ISEED1 (>0)'
      WRITE(*,*)' > '
      READ(*,*)ISEED1
      WRITE(*,*)' Enter ISEED2 (>0)'
      WRITE(*,*)' > '
      READ(*,*)ISEED2
C initialize random number generator
      CALL RNE2IN(ISEED1,ISEED2)
C generate data
      DO 30 I=1,NI
        AI=(DBLE(I)-0.5D0*DBLE(I+1))*A
        DO 20 J=1,NJ
          CALL RNSTNR(BUFFER,NK)
          DO 10 K=1,NK
            BIJ=(DBLE(I)-0.5D0*DBLE(I+1)+J-0.5D0*DBLE(J+1))*B
            X(I,J,K)=AMU+AI+BIJ+BUFFER(K)*SIGMA
10        CONTINUE
20      CONTINUE
30    CONTINUE
      CALL AVTBLE(X,NI,NJ,NK,XB,XBI,XBJ,XBIJ,Q,S,F,NDF,ALPHA)
      NTYPE=1
      CALL AVOUTP(X,NI,NJ,NK,Q,S,F,NDF,ALPHA,NTYPE)
      END
