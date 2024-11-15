C
        SUBROUTINE STVH0(X,SH0)
C
C       ==============================================
C        Purpose: Compute Struve Function H0(x)
C        Input  : x   --- Argument of H0(x) ( x ≥ 0 )
C        output : SH0 --- H0(x)
C       ==============================================
C
         IMPLICIT DOUBLE PRECISION (A-H, O-Z)
         PI=3.141592653589793D0
         EPS=1.0D-12
C
         S=1.0D0
         R=1.0D0
         IF (X.LE.20.0D0) THEN
C                             Use (11.1.4) when x ≤ 20
            A0=2.0D0*X/PI
            DO 10 K=1,60
               R=-R*(X/(2.0D0*K+1.0D0))**2
               S=S+R
               IF(DABS(R).LT.DABS(S)*EPS) GO TO 15
10          CONTINUE
15          SH0=A0*S
         ELSE
C                             Use (11.1.21) when x > 20
            KM=INT(0.5*(X+1.0))
            IF (X.GE.50.0D0) KM=25
            DO 20 K=1,KM
               R=-R*((2.0D0*K-1.0D0)/X)**2
               S=S+R
               IF(DABS(R).LT.DABS(S)*EPS) GO TO 25
20          CONTINUE
C                             Calculate Y0(x) using (5.2.18)
25          T=4.0D0/X
            T2=T*T
            P0=  ((((-.37043D-5*T2+.173565D-4)*T2-.487613D-4)
     &         *T2+.17343D-3)*T2-.1753062D-2)*T2+.3989422793D0
            Q0=T*(((((.32312D-5*T2-.142078D-4)*T2+.342468D-4)
     &         *T2-.869791D-4)*T2+.4564324D-3)*T2-.0124669441D0)
            TA0=X-0.25D0*PI
            BY0=2.0D0/DSQRT(X)*(P0*DSIN(TA0)+Q0*DCOS(TA0))
            SH0=2.0D0/(PI*X)*S+BY0
         ENDIF
         RETURN
        END
C           STVH0(X,SH0)
C


        PROGRAM MAIN
         IMPLICIT NONE
         INTEGER N, NM
         DOUBLE PRECISION X,SH0
C
         X = 0.0
         SH0 = 0.0
         DO 999 N=0,40
            X = N
            CALL STVH0(X, SH0)
            PRINT *, '[X =',X, '] NM =', SH0
999      CONTINUE
C
        END PROGRAM
