	MODULE MDDYNAM_TSF


	USE MDTYPES
	USE MDCONSTS
	USE MDBOUNDS,R=>GETR
	USE MDSTRUCT

	PRIVATE::FIND_NEIBRS
	PRIVATE::DR,DCOS_,COS_,D2R,D2COS
	PRIVATE::GET_DZT,GET_G,GET_FA,GET_FR,GET_FC	
	PRIVATE::GET_DDZT,GET_DB,GET_DG_COS
	PRIVATE::GET_D2DZT,GET_D2B,GET_D2G,GET_D2VIJ
	PUBLIC::GET_DINAM_MATRIX

	!--------------TYPES---------------
	!COCEдх юрнлю
	TYPE T_NBS
	PRIVATE
	INTEGER*4,POINTER,DIMENSION(:)::N
	END TYPE

	!опнхгбндмше он I х J
	!----------------------------------
	TYPE T_DIM
	PRIVATE
      REAL*8 I
	REAL*8 J
	END TYPE

	!-----------------------------------

	TYPE(T_NBS),PRIVATE,POINTER,DIMENSION(:)::NBS
	TYPE(T_DIM),PRIVATE::D_COS(3),D_R(3)
	TYPE(T_DIM),PRIVATE::D_FC(3),D_DZT(3),D_B(3),D_V(3)	
	REAL*8,PRIVATE::D2_COS(3,3),D2_R(3,3),D2_V(3,3)
	REAL*8,PRIVATE::D2_FC(3,3),D2_DZT(3,3),D2_B(3,3),D2_G(3,3) 	
	TYPE(T_STRUCT),PRIVATE::DIN_STR


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
	
      REAL*8 RIJ,RIK
	REAL*8 UIJ,UIK
	REAL*8 COS_JIK
	INTEGER*2 M		 

	RIJ = R(I,J,DIN_STR)
	RIK = R(I,K,DIN_STR)

      COS_JIK=COS_(J,I,K)
	
      DO M=1,3

	UIJ=U(M,I,J,DIN_STR)
	UIK=U(M,I,K,DIN_STR)
	
	D_COS(M)%I=COS_JIK*(UIK/RIK**2+UIJ/RIJ**2)-(UIJ+UIK)/(RIJ*RIK)     
      D_COS(M)%J=UIK/(RIJ*RIK)-COS_JIK*UIJ/RIJ**2	
      
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
      SUBROUTINE GET_DR(I,J)

	INTEGER*4 I,J

	INTEGER*2 A

	DO A=1,3

	D_R(A)%I=DR(I,J,A,I)
	D_R(A)%J=DR(I,J,A,J)

	END DO

	END SUBROUTINE

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
	REAL*8 S1,S2,S3	
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
	S1=-COS_(J,I,K)*(D2_R(M,N)-DR_IJMI*DR_IJNJ/RIJ)/RIJ
	S2=D2_R(M,N)/RIK-DR_IJNJ*DR_IKMI/RIJ**2
	S3=-D_COS(N)%J*(DR_IJMI/RIJ+DR_IKMI/RIK)

	D2_COS(M,N)=S1+S2+S3 		 
	 
	END DO
	END DO

	END SUBROUTINE

	!---------------------------------------------------------------------------------------------------------------------------
	!онхяй яняедеи дкъ йюфднцн юрнлю
	SUBROUTINE FIND_NEIBRS

	INTEGER I,J,IA,JA
	REAL*8 RR 
	INTEGER*2 BT
	INTEGER*4 NUM 	
	
	ALLOCATE(NBS(SIZE(DIN_STR%ATS)))
!	DO I=1,SIZE(DIN_STR%ATS)
!	ALLOCATE(NBS(I)%N(20))
!	END DO     
	

     	DO I=1,SIZE(DIN_STR%ATS)-1
	DO J=I+1,SIZE(DIN_STR%ATS)

	IA = DIN_STR%NUMS(I)%IN
	JA = DIN_STR%NUMS(J)%IN

	IF(IA.LE.0.OR.JA.LE.0) THEN
	CALL EXIT(10)
	END IF	

	IF(IA.EQ.37.OR.IA.EQ.76) THEN
	GOTO 66
	END IF	 

66	RR=R(IA,JA,DIN_STR)
	BT=GET_BOND_TYPE(IA,JA,DIN_STR%ATS)
!	RR=R(I,J,DIN_STR)
!	BT=GET_BOND_TYPE(I,J,DIN_STR%ATS)


	IF(ABS(RR-TSF(BT)%S).GT.1E-8.AND.RR.LT.TSF(BT)%S) THEN
!	WRITE(*,*) 'IA=',IA,'JA=',JA
 	NUM=ADD(NBS(I)%N,JA)
	NUM=ADD(NBS(J)%N,IA) 
	
!	NUM=ADD(NBS(I)%N,J)
!	NUM=ADD(NBS(J)%N,I)
	END IF

	END DO
	END DO

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякемхе Fc(rij)
	REAL*8 FUNCTION GET_FC(R,BT)

	REAL*8 R	   !пюдхся	  rij
	INTEGER*2 BT   !рхо ябъгх IJ

	IF(R.LE.TSF(BT)%R) 
	1GET_FC=1

	IF(R.GT.TSF(BT)%R.AND.R.LT.TSF(BT)%S)
	1GET_FC=0.5+0.5*COS(PI*(R-TSF(BT)%R)/(TSF(BT)%S-TSF(BT)%R))

	IF(R.GE.TSF(BT)%S) 
	1GET_FC=0 
	
      END FUNCTION
	
	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякемхе Fa(rij)
	REAL*8 FUNCTION GET_FA(R,BT)

	REAL*8 R	   !пюдхся	rij
	INTEGER*2 BT   !рхо ябъгх IJ

	GET_FA=-TSF(BT)%B*EXP(-TSF(BT)%MU*R)

	END FUNCTION

      !--------------------------------------------------------------------------------------------------------------------------
	!бшвхякемхе Fr(rij)	
	REAL*8 FUNCTION GET_FR(R,BT)

	REAL*8 R		!пюдхся	  rij
	INTEGER*2 BT	!рхо ябъгх	IJ

	GET_FR=TSF(BT)%A*EXP(-TSF(BT)%LM*R)

	END FUNCTION

	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякемхе G(Qjik)
	REAL*8 FUNCTION GET_G(TYP,COS_Q)

	INTEGER*2 TYP  !рхо I-нцн юрнлю
	REAL*8 COS_Q   !янS(Qjik) 


	GET_G=1.0+TSF(TYP)%C**2*(1.0/TSF(TYP)%D**2-
	11.0/(TSF(TYP)%D**2+(TSF(TYP)%H-COS_Q)**2))

	END FUNCTION


	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякемхе DZTij
	REAL*8 FUNCTION GET_DZT(I,J)

	INTEGER*4 I,J  !мнлепю юрнлнб

	REAL*8 COS_Q,RIK
	REAL*8 SUM,FC,G
	INTEGER*2 BT
	INTEGER*4 NI,NJ,NK

	SUM=0.0
	
     	NI=DIN_STR%NUMS(I)%IN
	NJ=DIN_STR%NUMS(J)%IN

	DO KK=1,SIZE(NBS(I)%N)

	NK=NBS(I)%N(KK)

	IF(NK.NE.NI.AND.NK.NE.NJ) THEN 
	
	RIK=R(NI,NK,DIN_STR)
	BT=GET_BOND_TYPE(NI,NK,DIN_STR%ATS)
	FC=GET_FC(RIK,BT)
	
      IF(ABS(FC).GT.1E-10) THEN	 
	COS_Q=COS_(NJ,NI,NK)
	G=GET_G(DIN_STR%ATS(NI)%TYP,COS_Q)
	SUM=SUM+FC*G
      END IF
	
      END IF	

	END DO

	GET_DZT=SUM

	END FUNCTION 	

	!-------------------------------------------------------------------------------------------------------------------------
	!бшвхякемхе опнхгбндмни dg(Qjik)/dCOS(Qjik)
	REAL*8 FUNCTION GET_DG_COS(TYP,COS_Q)

	INTEGER*2 TYP  !рхо I-нцн юрнлю
	REAL*8 COS_Q   !COS(Qjik)

	REAL*8 D

	D=COS_Q-TSF(TYP)%H

	GET_DG_COS=2.0*TSF(TYP)%C**2*D/(TSF(TYP)%D**2+D**2)**2

	END FUNCTION
	
	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякемхе опнхгбндмни dDZTij/dCOS(Qjik)
     	SUBROUTINE GET_DDZT(I,J)
	
	INTEGER*4 I,J !мнлепю юрнлнб

	REAL*8 FC,DG
	REAL*8 RIK,COS_Q
	INTEGER*2 BT,T
	INTEGER*2 A
	INTEGER*4 NI,NJ,NK		

	NI=DIN_STR%NUMS(I)%IN
	NJ=DIN_STR%NUMS(J)%IN

	DO A=1,3

	D_DZT(A)%I=0.0
	D_DZT(A)%J=0.0

	DO KK=1,SIZE(NBS(I)%N)

	NK=NBS(I)%N(KK)

	IF(NK.NE.NI.AND.NK.NE.NJ)  THEN

	RIK=R(NI,NK,DIN_STR)
	T=DIN_STR%ATS(NI)%TYP
	BT=GET_BOND_TYPE(NI,NK,DIN_STR%ATS)
	FC=GET_FC(RIK,BT)

	IF(ABS(FC).GT.1E-10) THEN 

	COS_Q=COS_(NJ,NI,NK)
	G=GET_G(T,COS_Q) 
	DG=GET_DG_COS(DIN_STR%ATS(NI)%TYP,COS_Q)
	CALL DCOS_(NJ,NI,NK) 
	
	D_DZT(A)%I=D_DZT(A)%I+FC*DG*D_COS(A)%I
	D_DZT(A)%J=D_DZT(A)%J+FC*DG*D_COS(A)%J

	END IF


	END IF

	END DO

	END DO
		

	END SUBROUTINE
	
	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякемхе  опнхгбндмни dbij/drij, я свернл рнцн, врн хгбеярмн bij,DZTij
	!опенапюгнбюмхе мнлепнб днкфмн ашрэ бшонкмемн гюпюмее
	SUBROUTINE	GET_DB(I,J,B,DZT)

	INTEGER*4 I,J 	
	REAL*8 B		!bij
	REAL*8 DZT		!DZTij
		
	INTEGER*2 T	    !рхо I-нцн юрнлю
	INTEGER*2 BT	!рхо ябъгх IJ
	INTEGER*2 A
	REAL*8 MULT	


	T=DIN_STR%ATS(I)%TYP
	BT=GET_BOND_TYPE(I,J,DIN_STR%ATS)

	DO A=1,3
	D_B(A)%I=0.0
	D_B(A)%J=0.0
	END DO

	IF(ABS(DZT).GT.1E-10) THEN

	MULT=0.5*B*((B/TSF(BT)%HI)**(2.0*TSF(T)%N)-1.0)/DZT

	IF(I.LE.0.OR.J.LE.0) THEN
	WRITE(*,*) 'NUM IS NULL:  I=',I,'J=',J
	CALL EXIT(12)
	END IF

!	CALL GET_DDZT(I,J)

	DO A=1,3
	D_B(A)%I=MULT*D_DZT(A)%I
	D_B(A)%J=MULT*D_DZT(A)%J
	END DO

	END IF
!	GET_DB=0.5*B*((B/TSF(BT)%HI)**(2.0*TSF(TYP)%N)-1.0)*DDZT/DZT

	END SUBROUTINE 
	
	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякемхе брнпни опнхгбндмни d2g(Qjik)/dUi(Alpha)dUj(Betta) я свернл рнцн,врн опнхгбндмше йняхмсянб мюидемш 
	!опенапюгнбюмхе мнлепнб днкфмн ашрэ бшонкмемн гюпюмее
 	SUBROUTINE GET_D2G(J,I,K)

	INTEGER*4 J,I,K

	REAL*8 COS_Q 	
	REAL*8 S1,S2,D,S3
	INTEGER*2 T
	INTEGER*2 A,B

	T=DIN_STR%ATS(I)%TYP
	COS_Q=COS_(J,I,K) 

	D=TSF(T)%D**2+(COS_Q-TSF(T)%H)**2
	S1=2.0*TSF(T)%C**2/D**2
	S2=(TSF(T)%D**2-3.0*(COS_Q-TSF(T)%H)**2)
	S3=2.0*TSF(T)%C**2/D**3

	DO A=1,3
	DO B=1,3

	D2_G(A,B)=S3*S2*D_COS(A)%I*D_COS(B)%J+
	1S1*(COS_Q-TSF(T)%H)*D2_COS(A,B)

	END DO
	END DO		


	END SUBROUTINE
     
      !--------------------------------------------------------------------------------------------------------------------------]
	!бшвхякемхе брнпни опнхгбндмни d2DZTij/dUi(Alpha)dUj(Betta)
 	SUBROUTINE GET_D2DZT(I,J)

	INTEGER*4 I,J

	REAL*8 FC,DG
	REAL*8 RIK,COS_Q
	INTEGER*2 BT,T
	INTEGER*2 A,B
	INTEGER*4 NI,NJ,NK

	NI=DIN_STR%NUMS(I)%IN
	NJ=DIN_STR%NUMS(J)%IN	
	
	DO A=1,3
	DO B=1,3

	D2_DZT(A,B)=0.0

	DO KK=1,SIZE(NBS(I)%N)

	NK=NBS(I)%N(KK)

	IF(NK.NE.NI.AND.NK.NE.NJ) THEN

	RIK=R(NI,NK,DIN_STR)
	T=DIN_STR%ATS(NI)%TYP
	BT=GET_BOND_TYPE(NI,NK,DIN_STR%ATS)
	FC=GET_FC(RIK,BT)	

	IF(ABS(FC).GT.1E-10)	THEN

	COS_Q=COS_(NJ,NI,NK)
	CALL DCOS_(NJ,NI,NK)
	CALL D2COS(NJ,NI,NK)
	
	DG=GET_DG_COS(T,COS_Q)
	CALL GET_D2G(NJ,NI,NK)
	CALL GET_DR(NI,NK)

	D2_DZT(A,B)=D2_DZT(A,B)+FC*D2_G(A,B)
	
	END IF

	END IF

	END DO

	END DO
	END DO

	END SUBROUTINE


	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякемхе d2bij/dUi(Alpha)dUj(Betta)	я свернл рнцн, врн хгбеярмш bij,DZTij, ю рюйфе опнхгбндмше пюдхсянб rij
	!опенапюгнбюмхе мнлепнб днкфмн ашрэ бшонкмемн гюпюмее
	SUBROUTINE  GET_D2B(I,J,B,DZT)

	INTEGER*4 I,J
	REAL*8 B        !bij
	REAL*8 DZT
	
	INTEGER*2 ALP,BET
	REAL*8 BH,BH2
	INTEGER*2 T,BT


	DO ALP=1,3
	DO BET=1,3
	D2_B(ALP,BET)=0.0
	END DO
	END DO

	IF(ABS(DZT).GT.1E-10) THEN

	T=DIN_STR%ATS(I)%TYP
	BT=GET_BOND_TYPE(I,J,DIN_STR%ATS)
	BH=(B/TSF(BT)%HI)**(2.0*TSF(T)%N)
	BH2=0.5*B*(BH-1.0)/DZT

	DO ALP=1,3
	DO BET=1,3 		

	D2_B(ALP,BET)=BH2*(0.5*D_DZT(ALP)%I*D_DZT(BET)%J*
	1(BH*(2.0*TSF(T)%N+1.0)-3.0)/DZT+D2_DZT(ALP,BET))

	END DO
	END DO
	
     	END IF      

	END SUBROUTINE
		
	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE GET_D2VIJ(I,J)

	INTEGER*4 I,J

	REAL*8 H1,H2
	REAL*8 FA,FR,FC,B,DZT
	INTEGER*2 T,BT
	REAL*8 RIJ
	INTEGER*2 ALP,BET
	REAL*8 AR
	INTEGER*4 NI,NJ

	NI=DIN_STR%NUMS(I)%IN
	NJ=DIN_STR%NUMS(J)%IN


	DO ALP=1,3
	DO BET=1,3
	D2_V(ALP,BET)=0.0
      END DO
      END DO 	


	T=DIN_STR%ATS(NI)%TYP
	BT=GET_BOND_TYPE(NI,NJ,DIN_STR%ATS)
	RIJ=R(NI,NJ,DIN_STR)	
     	FC=GET_FC(RIJ,BT)

	!бшвхякемхе FA,FR,FC,B,DB,D2B,DZT,DDZT,D2DZT
	IF(ABS(FC).GT.1E-10) THEN
	
	FA=GET_FA(RIJ,BT)
	FR=GET_FR(RIJ,BT)
      DZT=GET_DZT(I,J)	 !опенапюгнбюмхе мнлепнб - б яюлни т-хх
	B=TSF(BT)%HI*(1.0+(TSF(T)%BETTA*DZT)**TSF(T)%N)
	1**(-1.0/(2.0*TSF(T)%N))

	CALL GET_DR(NI,NJ)
	CALL D2R(NI,NJ)

	IF(I.LE.0.OR.J.LE.0) THEN
	WRITE(*,*) 'NUM IS NULL:  I=',I,'J=',J
	CALL EXIT(12)
	END IF

	CALL GET_DDZT(I,J)	  !опенапюгнбюмхе мнлепнб - б яюлни т-хх
	CALL GET_D2DZT(I,J)	  !опенапюгнбюмхе мнлепнб - б яюлни т-хх
      CALL GET_DB(NI,NJ,B,DZT)
	CALL GET_D2B(NI,NJ,B,DZT)	

	AR=-TSF(BT)%MU*FA*B-TSF(BT)%LM*FR

	CALL GET_DR(NI,NJ)
	CALL D2R(NI,NJ)

	!бшвхякемхе dVij/dUj(Betta)
	DO BET=1,3		
	D_V(BET)%J=FC*AR*D_R(BET)%J+FC*D_B(BET)%J*FA	
	END DO

	!бшвхякемхе d2Vij/dUi(Alpha)dUj(Betta)
	DO ALP=1,3
	DO BET=1,3 	

	H1=AR*D2_R(ALP,BET)+D_R(ALP)%I*D_R(BET)%J*(TSF(BT)%MU**2*FA*B
     1+TSF(BT)%LM**2*FR)
	H1=H1-D_R(ALP)%I*TSF(BT)%MU*FA*D_B(BET)%J
	
      H2=-D_B(ALP)%I*D_R(BET)%J*FA*TSF(BT)%MU+D2_B(ALP,BET)*FA	
	
      D2_V(ALP,BET)=(H1+H2)*FC/DIN_STR%ATS(NI)%M !SQRT(DIN_STR%ATS(NI)%M*DIN_STR%ATS(NJ)%M) 	 

	END DO
	END DO
	
     
      END IF

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE GET_TSF_DINAM_MATRIX(STRUCT,DIN_MATR)

      TYPE(T_STRUCT) STRUCT
	REAL*8,POINTER,DIMENSION(:,:)::DIN_MATR

	INTEGER*4 NATOMS
	INTEGER*4 AI,AJ
	REAL*8 HD(3,3)
	INTEGER*2 A,B
	INTEGER*4 FLAG

		
	DIN_STR=STRUCT 
	
	NATOMS=STRUCT%NATOMS
	ALLOCATE(DIN_MATR(3*NATOMS,3*NATOMS))	

	DO I=1,3*NATOMS
	DO J=1,3*NATOMS
	DIN_MATR(I,J)=0.0
	END DO
	END DO

	CALL FIND_NEIBRS 
	
	write(*,*) 'jki'
	 
	IATOM=1

	DO 100 I=1,SIZE(DIN_STR%ATS)

	AI=DIN_STR%NUMS(I)%IN 

	IF (AI.GT.NATOMS) GOTO 100

	DO 105 II=1,3
	DO 105 JJ=1,3
105	HD(II,JJ)=0.D0

	ISOSED=1
	DO 150 J=1,SIZE(DIN_STR%ATS)
	
      IF (I.EQ.J) GOTO 149
	AJ= DIN_STR%NUMS(J)%IN

c	ЕЯКХ СВХРШБЮРЭ БЙКЮД Б ДХЮЦ. ЩКЕЛЕМР НР ЦПЮМХВМШУ ЮРНЛНБ, 
c	ЙНРНПШЕ МЕ БУНДЪР Б ДХМ. ЛЮРПХЖС, РН 150 ГЮЛЕМЪЕЛ МЮ 145

	IF (AJ.GT.NATOMS) GOTO 150

	FLAG= 0

	DO 111 JJ=1,SIZE(NBS(I)%N)  !DIN_AT(I)%NUM_NEIB
	IF (AJ.EQ.NBS(I)%N(JJ)) THEN
	FLAG=JJ
	GOTO 112
	END IF
111	CONTINUE

 
112   IF (FLAG.GT.0) THEN
		 
  !   CALL SIL_STW_NEW(KBULD8, I,J)
   !   CALL STIL_WEB(I,J,HS4)
	IF(DIN_STR%NUMS(I)%IN.LE.0.OR.DIN_STR%NUMS(J)%IN.LE.0) THEN
	WRITE(*,*) 'NUM IS NULL:  I=',I,'J=',J
	CALL EXIT(12)
	END IF

	if((iATOM.eq.1.and.ISOSED.eq.10).or.
	1(iATOM.eq.10.and.ISOSED.eq.1)) then
	alp=3
	end if

!      CALL GET_D2VIJ(DIN_STR%NUMS(I)%IN,DIN_STR%NUMS(J)%IN)
	CALL GET_D2VIJ(I,J)
	
      DO 120 II=1,3
	DO 121 JJ=1,3
	I1=3*(IATOM-1)+II
	J1=3*(ISOSED-1)+JJ
      HD(II,JJ)= HD(II,JJ)-D2_V(II,JJ)
	DIN_MATR(I1,J1)=D2_V(II,JJ)
121	CONTINUE
120	CONTINUE


	END IF

149   ISOSED= ISOSED+1
	GOTO 150

c	СВХРШБЮЕР БЙКЮД Б ДХЮЦНМЮКЭМШИ ЩКЕЛЕМР НР ЦПЮМХВМШУ ЮРНЛНБ, 
c	МЕ БУНДЪЫХУ Б ДХМ. ЛЮРПХЖС
145	CONTINUE
C	WRITE(*,*) '145 CONTINUE'
	FLAG= 0
	DO 146 JJ=1,SIZE(NBS(I)%N) !DIN_AT(I)%NUM_NEIB
	IF (AJ.EQ.NBS(I)%N(JJ)) THEN
	FLAG=JJ
	GOTO 147
	END IF
146	CONTINUE

147   IF (FLAG.GT.0) THEN
    !  CALL SIL_STW_NEW(KBULD8, I,J)
    !  CALL STIL_WEB(I,J,HS4)
      IF(DIN_STR%NUMS(I)%IN.LE.0.OR.DIN_STR%NUMS(J)%IN.LE.0) THEN
	WRITE(*,*) 'NUM IS NULL:  I=',I,'J=',J
	CALL EXIT(12)
	END IF

	if((iATOM.eq.9.and.ISOSED.eq.15).or.
	1(iATOM.eq.15.and.ISOSED.eq.9)) then
	alp=3
	end if

!	CALL GET_D2VIJ(DIN_STR%NUMS(I)%IN,DIN_STR%NUMS(J)%IN)
      CALL GET_D2VIJ(I,J)

      DO 148 II=1,3
	DO 148 JJ=1,3
      HD(II,JJ)= HD(II,JJ)-D2_V(II,JJ)
148	CONTINUE
	END IF


150	CONTINUE

	DO 135 II=1,3
	DO 135 JJ=1,3
	I1=3*(IATOM-1)+II
	J1=3*(IATOM-1)+JJ
	DIN_MATR(I1,J1)= HD(II,JJ)

135	CONTINUE

	IATOM= IATOM+1
100	CONTINUE




	END SUBROUTINE

	END MODULE