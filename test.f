
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
