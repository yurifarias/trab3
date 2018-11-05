!----------------------------------------------------------------------
      SUBROUTINE DLSLRGa (NM,NR,H,FS,IAT,FA)
      INTEGER NE, N1, NM,IAT
      PARAMETER (TOL=1D-12)
      REAL*8 H(NR,NR), FS(NR),FA(NR), AUX, C, D

	REAL*8,DIMENSION(:,:),ALLOCATABLE :: HA

	ALLOCATE (HA(NR,NR))
!	IATH=1        
      DO I=1,NM
        FA(I)=FS(I)
        DO J=1,NM
          HA(I,J)=H(I,J)
        END DO
      END DO  
    
      N1=(NM)-1
      DO K=1,N1
		K1=K+1
		C=H(K,K)
		IF (DABS(C)-TOL) 1,1,2
1		    DO J=K1,(NM)
				IF(DABS((H(J,K)))-TOL) 3,3,4
4				DO L=K,(NM)
					C=H(K,L)
					H(K,L)=H(J,L)   
					H(J,L)=C
				END DO
				C=FS(K)
				FS(K)=FS(J)
				FS(J)=C
				C=H(K,K)
				GOTO 2   		   
			END DO
 3		CONTINUE
		GOTO 7
 2		C=H(K,K)
		DO J=K1,(NM)
			H(K,J)=H(K,J)/C
		END DO
		FS(K)=FS(K)/C
		DO I=K1,(NM)
			C=H(I,K)
			DO J=K1,(NM)
				H(I,J)=H(I,J)-C*H(K,J)
			END DO
			FS(I)=FS(I)-C*FS(K)
		END DO
      END DO

      IF (DABS((H((NM),(NM))))-TOL) 7,7,8
   8  FS((NM))=FS((NM))/H((NM),(NM))
      DO L=1,N1
		K=(NM)-L
		K1=K+1   
		DO J=K1,(NM)
			FS(K)=FS(K)-H(K,J)*FS(J)
		END DO
      END DO

      D=1.0d0
      DO I=1,(NM)
!		D=D*H(I,I)
	END DO
	GOTO 9
   7  WRITE(*,10) K
10    FORMAT(' Singularidade na Coluna : ',I3)
      D=0.0d0
 9    CONTINUE
       
      DO I=1,NM
        AUX=FS(I)
        FS(I)=FA(I)
        FA(I)=AUX
        DO J=1,NM
          H(I,J)=HA(I,J)
        END DO
      END DO  
      RETURN
      END
!----------------------------------------------------------------------