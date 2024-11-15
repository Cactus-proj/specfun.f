

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
