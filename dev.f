C
C       dev and test new functions
C


C
        SUBROUTINE CSPHIK(N,Z, NM,CSI,CDI,CSK,CDK)
C
C       =============================================================
C        Purpose: Compute modified spherical Bessel functions
C                 and their derivatives with a complex argument
C        Input  : Z --- Complex argument
C                 n --- Order of in(z) & kn(z)  ( n = 0,1,2,... )
C        output : CSI(n) --- in(z)
C                 CDI(n) --- in'(z)
C                 CSK(n) --- kn(z)
C                 CDK(n) --- kn'(z)
C                 NM --- Highest order computed
C        Routines called:
C                 MSTA1 and MSTA2 to calculate the starting
C                 point for backward recurrence
C       =============================================================
C
         IMPLICIT COMPLEX*16 (C,Z)
         DOUBLE PRECISION A0,PI
         DIMENSION CSI(0:N),CDI(0:N),CSK(0:N),CDK(0:N)
         PI=3.141592653589793D0
C
         A0=CDABS(Z)
         NM=N
         IF (A0.LT.1.0D-60) THEN
C                                <<<  Treat z = 0 as a specal case
            DO 10 K=0,N
               CSI(K)=0.0D0
               CDI(K)=0.0D0
               CSK(K)=1.0D+300
               CDK(K)=-1.0D+300
10          CONTINUE
            CSI(0)=1.0D0
            CDI(0)=.33333333333333D0
            RETURN
         ENDIF
C

         CI=CMPLX(0.0D0,1.0D0)
         CSINH=CDSIN(CI*Z)/CI
         CCOSH=CDCOS(CI*Z)
         CSI0=CSINH/Z
         CSI1=-(CSINH/Z-CCOSH)/Z
         CSI(0)=CSI0
         CSI(1)=CSI1
         IF (N.GE.2) THEN
            M=MSTA1(A0,200)
C                                <<< Calculate In(z) by backward recurrence
            IF (M.LT.N) THEN
               NM=M
            ELSE
               M=MSTA2(A0,N,15)
            ENDIF
            CF0=0.0D0
            CF1=1.0D0-100
            DO 15 K=M,0,-1
               CF=(2.0D0*K+3.0D0)*CF1/Z+CF0
               IF(K.LE.NM) CSI(K)=CF
               CF0=CF1
               CF1=CF
15          CONTINUE
            IF(CDABS(CSI0).GT.CDABS(CSI1)) CS=CSI0/CF
            IF(CDABS(CSI0).LE.CDABS(CSI1)) CS=CSI1/CF0
            DO 20 K=0,NM
20             CSI(K)=CS*CSI(K)
         ENDIF

C                                <<< Caiculate the dervatve of In(z)
         CDI(0)=CSI(1)
         DO 25 K=1,NM
25          CDI(K)=CSI(K-1)-(K+1.0D0)/Z*CSI(K)
         CSK(0)=0.5D0*PI/Z*CDEXP(-Z)
         CSK(1)=CSK(0)*(1.0D0+1.0D0/Z)

C                                <<< Calculate k0(z) using (8.3.29) or (8.3.30)
         DO 30 K=2,NM
            IF(CDABS(CSI(K-1)).GT.CDABS(CSI(K-2))) THEN
               CSK(K)=(0.5D0*PI/(Z*Z)-CSI(K)*CSK(K-1))/CSI(K-1)
            ELSE
               CSK(K)=(CSI(K)*CSK(K-2)+(K-0.5D0)*PI/Z**3)/CSI(K-2)
            ENDIF
30       CONTINUE
         CDK(0)=-CSK(1)
C                                <<< Calculate the dervatve of kn(z)
         DO 35 K=1,NM
35          CDK(K)=-CSK(K-1)-(K+1.0D0)/Z*CSK(K)
         RETURN
        END
C           CSPHIK(N,Z, NM,CSI,CDI,CSK,CDK)
C



C
        SUBROUTINE TEST_CSPHIK()
         IMPLICIT NONE
         INTEGER I,J,K, M,N, NM
         DOUBLE PRECISION X,Y, RET
         DOUBLE PRECISION SI(0:100),DI(0:100),SK(0:100),DK(0:100)
         COMPLEX*16 Z, CSI(0:100),CDI(0:100),CSK(0:100),CDK(0:100)

         INTEGER     DATA_X(6), DATA_NN(15)
         COMPLEX*16  DATA_Z(2)
         DATA DATA_X / 1,5,10, 25,50,100 /
         DATA DATA_NN/ 0,1,2,3, 4,5,6,7, 8,9, 10,20,30,50, 100 /
         DATA DATA_Z / (4.0,2.0), (20.0,10.0) /
C
         PRINT *, '[TEST_CSPHIK]'
         PRINT *, '  Test Real X inpit:'
         PRINT *, '  ref: CoSF: Table 8.9, 8.10'
         DO 990 I=1,6
            X = DATA_X(I)
            Z = X
            PRINT *, '[X/Z =', X, ']'
            PRINT *, 'REF:    N, Z,     NM,      CSI,CDI,CSK,CDK;;'
            DO 991 J=1,15
               N = DATA_NN(J)
               CALL CSPHIK(N,Z, NM,CSI,CDI,CSK,CDK)
               ! REF
               CALL SPHI(N,X, NM,SI,DI)
               CALL SPHK(N,X, NM,SK,DK)
               PRINT *, '   ', N,DREAL(Z),NM
     &            ,DREAL(CSI(NM)),DREAL(CDI(NM))
     &            ,DREAL(CSK(NM)),DREAL(CDK(NM))
               PRINT *, 'REF', N,X, NM
     &            ,SI(NM),DI(NM),SK(NM),DK(NM), ' ;;'
991         CONTINUE
            PRINT *, ''
990      CONTINUE
         PRINT *, ''
C

         PRINT *, '  Test Complex Z inpit:'
         PRINT *, '  ref: CoSF: Table 8.11~8.14'
         DO 995 I=1,2
            Z = DATA_Z(I)
            PRINT *, '[Z =', Z, ']'
            PRINT *, 'REF:    N,     NM,      CSI, CDI, CSK, CDK;;'
            DO 996 J=1,15
               N = DATA_NN(J)
               CALL CSPHIK(N,Z, NM,CSI,CDI,CSK,CDK)
               PRINT *, N,NM
     &            ,CSI(NM),CDI(NM)
     &            ,CSK(NM),CDK(NM)
996         CONTINUE
            PRINT *, ''
995      CONTINUE
         PRINT *, '[END]'
        END SUBROUTINE
C



C
        PROGRAM MAIN
         IMPLICIT NONE
C
         CALL TEST_CSPHIK()
C
        END PROGRAM
