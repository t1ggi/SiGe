	MODULE MDDYNAMSTW
	

	USE MDTYPES
	USE MDCONSTS
	USE MDBOUNDS,R=>GETR
	USE MDSTRUCT


	PRIVATE::FIND_NEIBRS
	PRIVATE::R,U
	PRIVATE::COS_
	PRIVATE::DCOS_
	PRIVATE::DLT
	PRIVATE::DR,D2R
	PRIVATE::D2COS
	PRIVATE::D2_H2,D2_H3
	PRIVATE::STIL_WEB 
	PUBLIC::GET_DINAM_MATRIX



	!-------------TYPES--------------

	TYPE T_NBS
	PRIVATE
	INTEGER*4,POINTER,DIMENSION(:)::N
	END TYPE
	!--------------------------------
	TYPE T_INDEX
	PRIVATE
	REAL I,J

	END TYPE 
	!--------------------------------
	TYPE T_DIM2

	PRIVATE
	TYPE(T_INDEX) IJK
	TYPE(T_INDEX) JIK
	TYPE(T_INDEX) IKJ

	END TYPE

	!--------------------------------
	TYPE T_DIM

	PRIVATE
	REAL IJK
	REAL JIK
	REAL IKJ

	END TYPE
	!--------------------------------

	TYPE(T_DIM2),PRIVATE:: D_COS(3)
	TYPE(T_DIM),PRIVATE:: D2_COS(3,3),D2H3(3,3)
	TYPE(T_NBS),PRIVATE,POINTER,DIMENSION(:)::NBS
	TYPE(T_STRUCT),PRIVATE::DIN_STR
	REAL*8,PRIVATE::D2_R(3,3)
	REAL*8,PRIVATE::D2_F3(3,3)
	
      
	CONTAINS
	
     	!--------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION COS_(J,I,K)

	INTEGER J,I,K

	REAL*8 R1,R2,R3

	R1 = R(I,J,DIN_STR)
	R2 = R(I,K,DIN_STR)
	R3 = R(J,K,DIN_STR)

	COS_ = DBLE((R1**2+R2**2-R3**2)/(2.0*R1*R2))

	END FUNCTION	

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE DCOS_(J,I,K)

	INTEGER J,I,K
	
      REAL*8 RIJ,RIK,RJK
	REAL*8 UIJ,UIK,UJK
	REAL*8 COS_JIK,COS_IJK,COS_IKJ
	INTEGER*2 M
	REAL*8 DR_IJMJ,DR_JKMJ
	 

	RIJ = R(I,J,DIN_STR)
	RIK = R(I,K,DIN_STR)
	RJK = R(J,K,DIN_STR)
	
      COS_JIK=COS_(J,I,K)
	COS_IJK=COS_(I,J,K)
	COS_IKJ=COS_(I,K,J)

	
      DO M=1,3

	UIJ=U(M,I,J,DIN_STR)
	UIK=U(M,I,K,DIN_STR)
	UJK=U(M,J,K,DIN_STR)

	DR_IJMJ=DR(I,J,M,J)
	DR_JKMJ=DR(J,K,M,J)
	
	D_COS(M)%JIK%I=COS_JIK*(UIK/RIK**2+UIJ/RIJ**2)-(UIJ+UIK)/(RIJ*RIK)
     
      D_COS(M)%JIK%J=UIK/(RIJ*RIK)-COS_JIK*UIJ/RIJ**2	 

	D_COS(M)%IJK%I=-UJK/(RIJ*RJK)-COS_IJK*UIJ/RIJ**2 

      D_COS(M)%IJK%J=COS_IJK*(DR_IJMJ/RIJ-DR_JKMJ/RJK)+
	1DR_JKMJ/RIJ-DR_IJMJ/RJK

	D_COS(M)%IKJ%I=(UIK+UIJ)/(RIK*RJK)-COS_IKJ*UIK/RIK**2

	D_COS(M)%IKJ%J=(UJK-UIJ)/(RIK*RJK)-COS_IKJ*UJK/RJK**2
      
      END DO
	 
	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION DR(M,N,ALPHA,P)

	INTEGER*2 ALPHA
	INTEGER M,N,P

	IF(M.GT.0.AND.N.GT.0.AND.P.GT.0.AND.ALPHA.GT.0) THEN

	IF(ALPHA.LT.4) THEN

	DR=U(ALPHA,M,N,DIN_STR)/R(M,N,DIN_STR)*(DLT(N,P)-DLT(M,P))

	ELSE 

	DR = -9.61E+10

	END IF

	ELSE 

	DR = -9.61E+10

	END IF
	
	END FUNCTION

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE D2R(I,J)

	INTEGER I,J
	INTEGER*2 M,N

	REAL*8 UMIJ,UNIJ,RIJ

	IF(I.GT.0.AND.J.GT.0) THEN

	RIJ=R(I,J,DIN_STR)

      DO M=1,3
	DO N=1,3

	UMIJ=U(M,I,J,DIN_STR)
	UNIJ=U(N,I,J,DIN_STR)
	!///////////////////////////////////////////////////////////////
      IF(M.EQ.N) THEN

	D2_R(M,N)=UMIJ*UNIJ/RIJ**3-1.0/RIJ

	ELSE

	D2_R(M,N)=-UMIJ*UNIJ/RIJ**3

	END IF

	END DO
	END DO
	!///////////////////////////////////////////////////////////////	

	END IF

	END SUBROUTINE

	!-------------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE D2COS(J,I,K)

	INTEGER I,J,K
	

	REAL*8 RIJ,RIK,RJK
	REAL*8 SJIK1,SJIK2,SJIK3
	REAL*8 SIJK1,SIJK2,SIJK3,SIJK4
	REAL*8 SIKJ1,SIKJ2,SIKJ3
	INTEGER*2 M,N
	REAL*8 DR_IJMI,DR_IJNJ,DR_IKMI,DR_JKNJ 

	RIJ = R(I,J,DIN_STR)
	RIK = R(I,K,DIN_STR)
	RJK = R(J,K,DIN_STR)

	CALL DCOS_(J,I,K)
	CALL D2R(I,J)


	DO M=1,3

	DR_IJMI=DR(I,J,M,I)
	DR_IKMI=DR(I,K,M,I)

	DO N=1,3
	
      DR_IJNJ=DR(I,J,N,J)
	DR_JKNJ=DR(J,K,N,J)

	!_________________COS_JIK_________________________________________
	SJIK1=-COS_(J,I,K)*(D2_R(M,N)-DR_IJMI*DR_IJNJ/RIJ)/RIJ
	SJIK2=D2_R(M,N)/RIK-DR_IJNJ*DR_IKMI/RIJ**2
	SJIK3=-D_COS(N)%JIK%J*(DR_IJMI/RIJ+DR_IKMI/RIK)

	D2_COS(M,N)%JIK=SJIK1+SJIK2+SJIK3

	!_________________COS_IJK_________________________________________
      SIJK1=-(RIJ**2*DR_JKNJ-2.*RJK*RIJ*DR_IJNJ)/RJK/RIJ**2*
	1D_COS(M)%IJK%I
	SIJK2= D2_R(M,N)*(RIJ**2+RIK**2-RJK**2)
	SIJK3=-2.*DR_IJMI*(-RIJ*DR_IJNJ-RJK*DR_JKNJ)
	SIJK4=-2.*RIK*DR_IKMI*DR_IJNJ

!	D2_COS(M,N)%IJK=SIJK1+SIJK2+SIJK3+SIJK4
      D2_COS(M,N)%IJK=SIJK1+(SIJK2+SIJK3+SIJK4)/(2.0*RJK*RIJ**2)  
  

	!_________________COS_IKJ_________________________________________
      SIKJ1=1./RJK*DR_JKNJ*D_COS(M)%IKJ%I
	SIKJ2=-DR_IKMI*(RIJ*DR_IJNJ+RJK*DR_JKNJ)
	SIKJ3=RIK*(RIJ*D2_R(M,N)+DR_IJMI*DR_IJNJ)

	D2_COS(M,N)%IKJ=SIKJ1+(SIKJ2+SIKJ3)/(RJK*RIK**2)	 
	 
	END DO
	END DO

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------







	

	END MODULE