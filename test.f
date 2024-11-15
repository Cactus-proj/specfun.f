C       **********************************

        SUBROUTINE GAMMA2(X,GA)
C
C       ==================================================
C       Purpose: Compute GAMMA2 function Г(x)
C       Input :  x  --- Argument of Г(x)
C                       ( x is not equal to 0,-1,-2,…)
C       Output:  GA --- Г(x)
C       ==================================================
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION G(26)
        PI=3.141592653589793D0
        IF (X.EQ.INT(X)) THEN
           IF (X.GT.0.0D0) THEN
              GA=1.0D0
              M1=X-1
              DO 10 K=2,M1
10               GA=GA*K
           ELSE
              GA=1.0D+300
           ENDIF
        ELSE
           R=1.0D0
           IF (DABS(X).GT.1.0D0) THEN
              Z=DABS(X)
              M=INT(Z)
              DO 15 K=1,M
15               R=R*(Z-K)
              Z=Z-M
           ELSE
              Z=X
           ENDIF
           DATA G/1.0D0,0.5772156649015329D0,
     &          -0.6558780715202538D0, -0.420026350340952D-1,
     &          0.1665386113822915D0,-.421977345555443D-1,
     &          -.96219715278770D-2, .72189432466630D-2,
     &          -.11651675918591D-2, -.2152416741149D-3,
     &          .1280502823882D-3, -.201348547807D-4,
     &          -.12504934821D-5, .11330272320D-5,
     &          -.2056338417D-6, .61160950D-8,
     &          .50020075D-8, -.11812746D-8,
     &          .1043427D-9, .77823D-11,
     &          -.36968D-11, .51D-12,
     &          -.206D-13, -.54D-14, .14D-14, .1D-15/
           GR=G(26)
           DO 20 K=25,1,-1
20            GR=GR*Z+G(K)
           GA=1.0D0/(GR*Z)
           IF (DABS(X).GT.1.0D0) THEN
              GA=GA*R
              IF (X.LT.0.0D0) GA=-PI/(X*GA*DSIN(PI*X))
           ENDIF
        ENDIF
        RETURN
        END



C
        SUBROUTINE STVHV(V,X,HV)
C
C       =====================================================
C        Purpose:  Compute Struve Functions Hv(x) with
C                  arbitrary order v  ( -8.0 ≤ v ≤ 12.5 )
C        Input  :  v  --- Order of Hv(x)
C                  x  --- Argument of Hv(x) ( x ≥ 0 )
C        Output :  Hv --- Hv(x)
C        Routine called: GAMMA2 to compute the GAMMA2 function
C       =====================================================
C
         IMPLICIT DOUBLE PRECISION (A-H, O-Z)
         PI=3.141592653589793D0
C
         IF (X.EQ.0.0D0) THEN
C                    <<< Treat x=0 as a special case
            IF ((V.GT.-1.0D0).OR.(INT(V)-V.EQ.0.5D0)) THEN
               HV=0.0D0
            ELSE IF (V.LT.-1.0D0) THEN
               HV=(-1)**(INT(0.5D0-V)-1)*1.0D+300
            ELSE IF (V.EQ.-1.0D0) THEN
               HV=2.0D0/PI
            ENDIF
            RETURN
         ENDIF
C
         IF (X.LE.20.0D0) THEN
C                    <<< Use (11.1.3) when x ≤ 20
            VO=V+1.5D0
            CALL GAMMA2(VO,GA)
            S=2.0D0/(DSQRT(PI)*GA)
            R1=1.0D0
            DO 10 K=1,100
               VA=K+1.5D0
               CALL GAMMA2(VA,GA)
               VB=V+K+1.5D0
               CALL GAMMA2(VB,GB)
               R1=-R1*(0.5D0*X)**2
               R2=R1/(GA*GB)
               S=S+R2
               IF(DABS(R2).LT.DABS(S)*1.0D-12) GO TO 15
10          CONTINUE
15          HV=(0.5D0*X)**(V+1.0D0)*S
         ELSE
C                    <<< Use (11.1.20) when x > 20
            SA=(0.5D0*X)**(V-1.0D0)/PI
            O=V+0.5D0
            CALL GAMMA2(VO,GA)
            S=DSQRT(PI)/GA
            R1=1.0D0
            DO 20 K=1,12
               VA=K+0.5D0
               CALL GAMMA2(VA,GA)
               VB=-K+V+0.5D0
               CALL GAMMA2(VB,GB)
               R1=R1/(0.5*X)**2
               S=S+GA/GB*R1
20          CONTINUE
C                    <<< Calculate Y1(x) using forward recurrence
            S0=SA*S
            U=DABS(V)
            N=INT(U)
            UO=U-N
            DO 35 L=0,1
               VT=4.0D0*(U0+L)**2
               R1=1.0D0
               PU1=1.0D0
               DO 25 K=1,12
                  R1=-0.0078125D0*R1*(VT-(4.D0*K-3.D0)**2)*
     &               (VT-(4.D0*K-1.D0)**2)/((2.D0*K-1.D0)*K*X*X)
                  PU1=PU1+R1
25             CONTINUE
               QU1=1.0D0
               R2=1.0D0
               DO 30 K=1,12
                  R2=-0.0078125D0*R2*(VT-(4.D0*K-1.D0)**2)*
     &               (VT-(4.D0*K+1.D0)**2)/((2.D0*K+1.D0)*K*X*X)
                  QU1=QU1+R2
30             CONTINUE
               QU1=0.125D0*(VT-1.0D0)/X*QU1
               IF (L.EQ.0) THEN
                  PUO=PO1
                  QUO=QU1
               ENDIF
35          CONTINUE
            T0=X-(0.5*U0+0.25D0)*PI
            T1=X-(0.5*U0+0.75D0)*PI
            SR=DSQRT(2.0D0/(PI*X))
            BY0=SR*(PU0*DSIN(TO)+QU0*DCOS(TO))
            BY1=SR*(PU1*DSIN(T1)+QU1*DCOS(T1))
            BF0=BY0
            BF1=BY1
            DO 40 K=2,N
               BF=2.0D0*(K-1.0+U0)/X*BF1-BF0
               BF0=BF1
               BF1=BF
40          CONTINUE
            IF (N.EQ.0) BYV=BY0
            IF (N.EQ.1) BYV=BY1
            IF (N.GT.1) BYV=BF
            HV=BYV+S0
         ENDIF
         RETURN
        END
C           STVHV(V,X,HV)
C


C
        PROGRAM MAIN
         IMPLICIT NONE
         INTEGER M,N, NM
         DOUBLE PRECISION X,V,RET
C
         X = 0.0
         V = -1.0
         RET = 0.0
         
         DO 990 M=-4,3
            V = M
            PRINT *, '[[V =', V, ']]'
            PRINT *, '[X =', 0.0, '] RET =', 0.0
            DO 999 N=0,21
               X = N
               CALL STVHV(V, X, RET)
               PRINT *, X, ', ', RET
999         CONTINUE
            PRINT *, ''
990      CONTINUE
C
        END PROGRAM
