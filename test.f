


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
