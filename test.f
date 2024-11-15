        SUBROUTINE STVH1(X,SH1)
C
C       =============================================
C        Purpose: Compute Struve Function H1(x)
C        Input  : x   --- Argument of H1(x) ( x ≥ 0 )
C        Output : SH1 --- H1(x)
C       =============================================
C
         IMPLICIT DOUBLE PRECISION (A-H,O-Z)
         PI=3.141592653589793D0
         EPS=1.0D-12
C
         R=1.0D0
         IF (X.LE.20.0D0) THEN
C                       <<< Use (11.1.5) when x ≤ 20
            S=0.0D0
            AO=-2.0D0/PI
            DO 10 K=1,60
               R=-R*X*X/(4.0D0*K*K-1.0D0)
               S=S+R
               IF(DABS(R).LT.DABS(S)*EPS) GO TO 15
10          CONTINUE
15          SH1=AO*S
         ELSE
C                       <<< Use (11.1.22) when x > 20
            S=1.0D0
            KM=INT(0.5*X)
            IF (X.GT.50.0D0) KM=25
            DO 20 K=1,KM
               R=-R*(4.0D0*K*K-1.0D0)/(X*X)
               S=S+R
               IF(DABS(R).LT.DABS(S)*EPS) GO TO 25
20          CONTINUE
25          T=4.0D0/x
C                       <<< Calcuiate Y1(x) using (5.2.20)
            T2=T*T
            P1=  ((((.42414D-5*T2-.20092D-4)*T2+.580759D-4)
     &         *T2-.223203D-3)*T2+.29218256D-2)*T2+.3989422819D0
            Q1=T*(((((-.36594D-5*T2+.1622D-4)*T2-.398708D-4)
     &         *T2+.1064741D-3)*T2-.63904D-3)*T2+.0374008364D0)
            TA1=X-0.75D0*PI
            BY1=2.0D0/DSQRT(X)*(P1*DSIN(TA1)+Q1*DCOS(TA1))
            SH1=2.0/PI*(1.0D0+S/(X*X))+BY1
         ENDIF
         RETURN
        END
C           STVH1(X,SH1)
C

        PROGRAM MAIN
         IMPLICIT NONE
         INTEGER N, NM
         DOUBLE PRECISION X,SH0
C
         X = 0.0
         SH0 = 0.0
         PRINT *, '[X =', 0.0, '] ERT =', 0.0
         DO 999 N=0,40
            X = N
            CALL STVH1(X, SH0)
            PRINT *, X, ', ', SH0
999      CONTINUE
C
        END PROGRAM
