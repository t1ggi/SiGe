	MODULE MDPOTENTS

	USE MDTYPES
	USE MDCONSTS
	USE MDSTRUCT
	

	PRIVATE::SET_NB
	PUBLIC::TERSF_I,TRESF_FULL
	PUBLIC::STW_I,STW_FULL
	PRIVATE::GET_FC	
	PUBLIC::BOND_ENERGY
	PRIVATE::ENERGY_FULL,ENERGY_I

	!------------------INTERFACES-----------------------	

	INTERFACE ENERGY
	MODULE PROCEDURE ENERGY_FULL,ENERGY_I
	END INTERFACE 
	

	CONTAINS 	
      

	!--------------------------------------------------------------------------------------------------------------------------
	!Õ¿’Œ∆ƒ≈Õ»≈ —Œ—≈ƒ≈… ƒÀﬂ I-Œ√Œ ¿“ŒÃ¿
	SUBROUTINE SET_NB(I,CRD,STRUCT,NB,POTENT)

	INTEGER*4 I
	TYPE(T_COORD) CRD
	TYPE(T_STRUCT) STRUCT
	INTEGER*4,POINTER,DIMENSION(:)::NB	
	INTEGER*2 POTENT
	

	REAL*8 R
	INTEGER*2 BT,NUM
	TYPE(T_COORD) CRDJ
	
      ALLOCATE(NB(0))
	NUM=0  	

	DO JJ=1,SIZE(STRUCT%ATS(I)%ENVRS) 

      J=STRUCT%ATS(I)%ENVRS(JJ)
		
	CRDJ=T_COORD(STRUCT%ATS(J)%X,STRUCT%ATS(J)%Y,STRUCT%ATS(J)%Z)

	R=GETR(CRD,CRDJ,STRUCT)
	
	BT=GET_BOND_TYPE(I,J,STRUCT%ATS)


	IF(POTENT.EQ.P_TSF) THEN

	IF(ABS(R-TSF(BT)%S).GT.1E-3.AND.R.LT.TSF(BT)%S) NUM=ADD(NB,J)

	ELSE

	R=R/STW(BT)%SIG
	IF(R.LT.STW(BT)%RA) NUM=ADD(NB,J) !.AND.ABS(R-STW(BT)%RA).GT.1E-6)

	END IF

	IF(NUM.GT.40) EXIT 

	END DO

	
      END SUBROUTINE 	 

      !--------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION GET_FC(R,BT)

	REAL*8 R
	INTEGER*2 BT

	IF(R.LE.TSF(BT)%R) 
	1GET_FC=1

	IF(R.GT.TSF(BT)%R.AND.R.LT.TSF(BT)%S)
	1GET_FC=0.5+0.5*COS(PI*(R-TSF(BT)%R)/(TSF(BT)%S-TSF(BT)%R))

	IF(R.GE.TSF(BT)%S) 
	1GET_FC=0 
	
      END FUNCTION 	

	!-------------------------------------------------------------------------------------------------------------------------
	!›Õ≈–√»ﬂ ¿“ŒÃ¿ (œŒ“≈Õ÷»¿À “≈–—Œ‘‘¿)
	REAL*8 FUNCTION TERSF_I(I,CRD,STRUCT)

	INTEGER*4 I
	TYPE(T_STRUCT) STRUCT
	TYPE(T_COORD) CRD	


	INTEGER*4,POINTER,DIMENSION(:)::NB
	INTEGER*4 J,K
      REAL*8 VIJ,VI
	REAL*8 FCIJ,FCIK
	REAL*8 BIJ
	REAL*8 FRIJ,FAIJ
	REAL*8 DZTIJ
	REAL*8 G,COS_Q,G1
	REAL*8 RIJ,RIK,RJK
	TYPE(T_COORD) CRDJ,CRDK
	INTEGER*2 BTIJ,BTIK !“»œ€ —¬ﬂ«»

	VI=0.0

	ALLOCATE(NB(0))

	CALL SET_NB(I,CRD,STRUCT,NB,P_TSF)
	
	DO JJ=1,SIZE(NB)

	J=NB(JJ)
		
	BTIJ=GET_BOND_TYPE(I,J,STRUCT%ATS)
	CRDJ=T_COORD(STRUCT%ATS(J)%X,STRUCT%ATS(J)%Y,STRUCT%ATS(J)%Z)

	RIJ=GETR(CRD,CRDJ,STRUCT) 	

	FCIJ=GET_FC(RIJ,BTIJ)

	IF2:IF(ABS(FCIJ).LT.1E-6) THEN

	VIJ=0.0

	ELSE

	FRIJ=TSF(BTIJ)%A*EXP(-TSF(BTIJ)%LM*RIJ)
	FAIJ=-TSF(BTIJ)%B*EXP(-TSF(BTIJ)%MU*RIJ)
	DZTIJ=0.0 


	DO KK=1,SIZE(NB)
	
	K=NB(KK)

	IF3:IF(K.NE.I.AND.K.NE.J) THEN

      BTIK=GET_BOND_TYPE(I,K,STRUCT%ATS) 
	CRDK=T_COORD(STRUCT%ATS(K)%X,STRUCT%ATS(K)%Y,STRUCT%ATS(K)%Z)

	RIK=GETR(CRD,CRDK,STRUCT)	

	FCIK=GET_FC(RIK,BTIK)

	IF4:IF(ABS(FCIK).GT.1E-10) THEN

	RJK=GETR(CRDJ,CRDK,STRUCT)

	COS_Q=(RIJ**2+RIK**2-RJK**2)/(2.0*RIJ*RIK)

	!-- ¿  ¬€◊»—Àﬂ“‹ G ?--------------------------------------
	G1=(TSF(STRUCT%ATS(I)%TYP)%H-COS_Q)**2+TSF(STRUCT%ATS(I)%TYP)%D**2
	if(abs(g1)<1e-6) write(*,*) 'dev by zero!'
      G=1.0+TSF(STRUCT%ATS(I)%TYP)%C**2*(1.0/TSF(STRUCT%ATS(I)%TYP)%D**2
	1-1.0/G1)
	!---------------------------------------------------------

	DZTIJ=DZTIJ+TSF(BTIK)%W*FCIK*G

	END IF IF4

	END IF IF3

	END DO

	BIJ=TSF(BTIJ)%HI*(1.0+(TSF(STRUCT%ATS(I)%TYP)%BETTA*DZTIJ)
	1**TSF(STRUCT%ATS(I)%TYP)%N)**(-0.5/TSF(STRUCT%ATS(I)%TYP)%N)

	VIJ=FCIJ*(FRIJ+BIJ*FAIJ)

	END IF IF2

	VI=VI+VIJ 

	END DO      	

	DEALLOCATE(NB)
	
	TERSF_I=VI

	END FUNCTION

	!------------------------------------------------------------------------------------------------------------------------
	!›Õ≈–√»ﬂ ƒÀﬂ N0 ¿“ŒÃŒ¬  –»—“¿ÀÀ¿(œŒ“-À “≈–—Œ‘‘¿)
	REAL*8 FUNCTION TERSF_FULL(STRUCT,N0)

	TYPE(T_STRUCT) STRUCT
	INTEGER*4 N0

	REAL*8 	H
	TYPE(T_COORD) CRD

	H=0.0
	DO I=1,N0 !SIZE(ATOMS)
	CRD=T_COORD(STRUCT%ATS(I)%X,STRUCT%ATS(I)%Y,STRUCT%ATS(I)%Z)
	H=H+0.5*TERSF_I(I,CRD,STRUCT)
	END DO

	TERSF_FULL=H

	END FUNCTION

	!-------------------------------------------------------------------------------------------------------------------------
	!›Õ≈–√»ﬂ ¿“ŒÃ¿ (œŒ“≈Õ÷»¿À —“»À.-¬≈¡≈–¿)
	REAL*8 FUNCTION STW_I(I,CRD,STRUCT)

	INTEGER*4 I
	TYPE(T_STRUCT) STRUCT
	TYPE(T_COORD) CRD 	


      INTEGER*4,POINTER,DIMENSION(:)::NB
	TYPE(T_COORD) CRDJ,CRDK
	REAL*8 RIJ,RIK,RJK
	REAL*8 H2,H3,H31
	REAL*8 COS_JIK
	INTEGER*2 BT,BT_IJ,BT_IK,BT_JK	 !“»œ —¬ﬂ«≈…
	INTEGER*2 CT                	 !“»œ ”√À¿

	ALLOCATE(NB(0))
	CALL SET_NB(I,CRD,STRUCT,NB,P_STW)

	!ƒ¬”’◊¿—“»◊ÕŒ≈ ¬«¿»ÃŒƒ≈…—“¬»≈
	H2=0.0   

	DO  JJ=1,SIZE(NB)

	J=NB(JJ)

	CRDJ=T_COORD(STRUCT%ATS(J)%X,STRUCT%ATS(J)%Y,STRUCT%ATS(J)%Z)

	RIJ=GETR(CRD,CRDJ,STRUCT)	
	   
	BT=GET_BOND_TYPE(I,J,STRUCT%ATS)
	RIJ=RIJ/STW(BT)%SIG

	if(abs(RIJ-STW(BT)%RA)<1e-6) then 
	write(*,*) 'dev by zero!'
	end if

	H2=H2+STW(BT)%EN*STW(BT)%A*(STW(BT)%B*RIJ**(-STW(BT)%P)
	1-RIJ**(-STW(BT)%Q))*EXP(1/(RIJ-STW(BT)%RA))

      END DO 

 
      !“–≈’◊¿—“»◊ÕŒ≈ ¬«¿»ÃŒƒ≈…—“¬»≈

      H3=0.0

	DO JJ=1,SIZE(NB)
	DO KK=JJ+1,SIZE(NB)

	IF(JJ.NE.KK) THEN

	J=NB(JJ)
	K=NB(KK)

	CRDJ=T_COORD(STRUCT%ATS(J)%X,STRUCT%ATS(J)%Y,STRUCT%ATS(J)%Z)
	CRDK=T_COORD(STRUCT%ATS(K)%X,STRUCT%ATS(K)%Y,STRUCT%ATS(K)%Z)

	RIJ=GETR(CRD,CRDJ,STRUCT)
	RIK=GETR(CRD,CRDK,STRUCT)
	RJK=GETR(CRDJ,CRDK,STRUCT)

	BT_IJ=GET_BOND_TYPE(I,J,STRUCT%ATS)
	BT_IK=GET_BOND_TYPE(I,K,STRUCT%ATS)
	BT_JK=GET_BOND_TYPE(K,J,STRUCT%ATS)	
 	
	COS_JIK=(RIJ**2+RIK**2-RJK**2)/(2.*RIJ*RIK)

	RIJ=RIJ/STW(BT_IJ)%SIG	
 	RIK=RIK/STW(BT_IK)%SIG	
	RJK=RJK/STW(BT_JK)%SIG

	CT=GET_ANGLE_TYPE(I,J,K,STRUCT%ATS)	

	H31=STW(CT)%LMD*EXP(STW(BT_IJ)%GAM/(RIJ-STW(BT_IJ)%RA)
	1+STW(BT_IK)%GAM/(RIK-STW(BT_IK)%RA))*(COS_JIK+1./3.)**2	

	H3=H3+STW(CT)%EN*H31  !+H32) !+H33)

	END IF

      END DO
	END DO
	
      DEALLOCATE(NB)    
      
	STW_I=H2+H3


	END FUNCTION
	
	

	!------------------------------------------------------------------------------------------------------------------------
	!›Õ≈–√»ﬂ ƒÀﬂ N0 ¿“ŒÃŒ¬  –»—“¿ÀÀ¿(œŒ“-À —“»À.-¬≈¡≈–¿)
	REAL*8 FUNCTION STW_FULL(STRUCT,N0)

	TYPE(T_STRUCT) STRUCT
	INTEGER*4 N0 
 

	INTEGER*4,POINTER,DIMENSION(:)::NB
	TYPE(T_COORD) CRD,CRDJ,CRDK
	REAL*8 RIJ,RIK,RJK
	REAL*8 H2,H3,H30
	REAL*8 COS_JIK,COS_IJK,COS_IKJ
	INTEGER*2 BT,BT_IJ,BT_IK,BT_JK	 !“»œ —¬ﬂ«≈…
	INTEGER*2 CT         

	H=0.0
	H2=0.0
	H3=0.0
	 
	ALLOCATE(NB(0))

	DO I=1,N0 !SIZE(ATOMS) 

	CRD=T_COORD(STRUCT%ATS(I)%X,STRUCT%ATS(I)%Y,STRUCT%ATS(I)%Z) 
	
	
	CALL SET_NB(I,CRD,STRUCT,NB,P_STW) 	


	!ƒ¬”’◊¿—“»◊Õ€… œŒ“≈Õ÷»¿À	

	DO  JJ=1,SIZE(NB)  

	J=NB(JJ)   

	CRDJ=T_COORD(STRUCT%ATS(J)%X,STRUCT%ATS(J)%Y,STRUCT%ATS(J)%Z)

	RIJ=GETR(CRD,CRDJ,STRUCT)	

	BT=GET_BOND_TYPE(I,J,STRUCT%ATS)
	RIJ=RIJ/STW(BT)%SIG

	if(abs(RIJ-STW(BT)%RA)<1e-6) then
	write(*,*) 'dev by zero!'
	end if

	H2=H2+STW(BT)%EN*STW(BT)%A*(STW(BT)%B*RIJ**(-STW(BT)%P)
	1-RIJ**(-STW(BT)%Q))*EXP(1/(RIJ-STW(BT)%RA))

      END DO 


	!“–≈’◊¿—“»◊Õ€… œŒ“≈Õ÷»¿À
	DO JJ=1,SIZE(NB)
	DO KK=JJ+1,SIZE(NB)

	IF(JJ.NE.KK) THEN

	IF(I.EQ.127.AND.JJ.EQ.3.AND.KK.EQ.5) THEN

	J=NB(JJ)
	K=NB(KK)

	END IF

	J=NB(JJ)
	K=NB(KK)

	CRDJ=T_COORD(STRUCT%ATS(J)%X,STRUCT%ATS(J)%Y,STRUCT%ATS(J)%Z)
	CRDK=T_COORD(STRUCT%ATS(K)%X,STRUCT%ATS(K)%Y,STRUCT%ATS(K)%Z) 

	RIJ=GETR(CRD,CRDJ,STRUCT)
	RIK=GETR(CRD,CRDK,STRUCT)
	RJK=GETR(CRDJ,CRDK,STRUCT)	
	
	BT_IJ=GET_BOND_TYPE(I,J,STRUCT%ATS)
	BT_IK=GET_BOND_TYPE(I,K,STRUCT%ATS)
	BT_JK=GET_BOND_TYPE(K,J,STRUCT%ATS)

	if(abs(RIJ)<1e-6) then
	WRITE(*,*)'I=',I,'JJ=',JJ,'KK=',KK
	write(*,*) 'dev by zero!'
	end if

	if(abs(RIK)<1e-6) then
	WRITE(*,*)'I=',I,'JJ=',JJ,'KK=',KK
	write(*,*) 'dev by zero!'
	end if

	if(abs(RJK)<1e-6) then
	WRITE(*,*)'I=',I,'JJ=',JJ,'KK=',KK
	write(*,*) 'dev by zero!'
	end if

	COS_JIK=(RIJ**2+RIK**2-RJK**2)/(2.*RIJ*RIK)
	COS_IJK=(RJK**2+RIJ**2-RIK**2)/(2.*RIJ*RJK)
	COS_IKJ=(RJK**2+RIK**2-RIJ**2)/(2.*RJK*RIK)
	 
     	RIJ=RIJ/STW(BT_IJ)%SIG	
 	RIK=RIK/STW(BT_IK)%SIG	
	RJK=RJK/STW(BT_JK)%SIG	

	CT=GET_ANGLE_TYPE(I,J,K,STRUCT%ATS)

	IF(RIJ.LT.STW(BT_IJ)%RA.AND.RIK.LT.STW(BT_IK)%RA)THEN 

	if(abs(RIJ-STW(BT_IJ)%RA)<1e-6) then
	WRITE(*,*)'I=',I,'JJ=',JJ,'KK=',KK
	write(*,*) 'dev by zero!'
	end if

	if(abs(RIK-STW(BT_IK)%RA)<1e-6) then
	WRITE(*,*)'I=',I,'JJ=',JJ,'KK=',KK
	write(*,*) 'dev by zero!'
	end if

	H30=STW(CT)%LMD*EXP(STW(BT_IJ)%GAM/(RIJ-STW(BT_IJ)%RA)
	1+STW(BT_IK)%GAM/(RIK-STW(BT_IK)%RA))*(COS_JIK+1./3.)**2	
	
      H3=h3+STW(CT)%EN*H30 
	
      END IF

	END IF

      END DO
	END DO

!	H=

	END DO

	STW_FULL=0.5*H2+1.0/3.0*H3  !H

	DEALLOCATE(NB)

	END FUNCTION


	!--------------------------------------------------------------------------------------------------------------------------
	!!›Õ≈–√»ﬂ ƒÀﬂ N0 ¿“ŒÃŒ¬  –»—“¿ÀÀ¿ (“»œ œŒ“-À¿ «¿ƒ¿≈“—ﬂ œ¿–-Ã POTENT_TYPE)
	REAL*8 FUNCTION ENERGY_FULL(STRUCT,POTENT_TYPE,N0)

	TYPE(T_STRUCT) STRUCT
	INTEGER*2 POTENT_TYPE
	INTEGER*4 N0

	SELECT CASE(POTENT_TYPE)

	CASE(P_TSF) 
		ENERGY_FULL=TERSF_FULL(STRUCT,N0) 
		
	CASE(P_STW) 
		ENERGY_FULL=STW_FULL(STRUCT,N0)
		
	CASE DEFAULT
		ENERGY_FULL=0.0	  

	END SELECT


 	END FUNCTION


	!--------------------------------------------------------------------------------------------------------------------------
	!›Õ≈–√»ﬂ ¿“ŒÃ¿ (“»œ œŒ“-À¿ «¿ƒ¿≈“—ﬂ œ¿–-Ã POTENT)
	REAL*8 FUNCTION ENERGY_I(I,CRD,STRUCT,POTENT_TYPE)

	INTEGER*4 I
	TYPE(T_STRUCT) STRUCT
	INTEGER*4 POTENT_TYPE
	TYPE(T_COORD) CRD 


	SELECT CASE(POTENT_TYPE)

	CASE(P_TSF) 
		ENERGY_I=TERSF_I(I,CRD,STRUCT) 
		
	CASE(P_STW) 
		ENERGY_I=STW_I(I,CRD,STRUCT)
		
	CASE DEFAULT
		ENERGY_I=0.0 

	END SELECT


	END FUNCTION 
	
     
	!--------------------------------------------------------------------------------------------------------------------------
	!–¿—œ–≈ƒ≈À≈Õ»≈ ›Õ≈–√»» —¬ﬂ«≈…
	SUBROUTINE BOND_ENERGY(STRUCT,N0,POTENT,BE_SS,BE_SG,BE_GG)

	TYPE(T_STRUCT) STRUCT
	REAL*8,POINTER,DIMENSION(:)::BE_SS,BE_SG,BE_GG
	INTEGER*4 N0 	

	INTERFACE
	   REAL*8 FUNCTION POTENT(I,CRD,STRUCT,BOUNDS)
	     USE MDTYPES
	     INTEGER*4 I
	     TYPE(T_COORD) CRD	     
		 TYPE(T_STRUCT) STRUCT
		 INTEGER*2 BOUNDS   
	  END FUNCTION
	END INTERFACE

	TYPE(T_COORD) CRD
	REAL*8 EI,EJ
	INTEGER*4 NUM		
	
      ALLOCATE(BE_SS(0))
	ALLOCATE(BE_SG(0))
	ALLOCATE(BE_GG(0))
	
      DO I=1,N0-1
	DO J=I+1,N0
	
	IF(J.EQ.STRUCT%ATS(I)%NEIBR(1).OR.J.EQ.STRUCT%ATS(I)%NEIBR(2).OR.
	1J.EQ.STRUCT%ATS(I)%NEIBR(3).OR.J.EQ.STRUCT%ATS(I)%NEIBR(4)) THEN

	CRD=T_COORD(STRUCT%ATS(I)%X,STRUCT%ATS(I)%Y,STRUCT%ATS(I)%Z)
	EI=POTENT(I,CRD,STRUCT,RGD)
	CRD=T_COORD(STRUCT%ATS(J)%X,STRUCT%ATS(J)%Y,STRUCT%ATS(J)%Z)
	EJ=POTENT(J,CRD,STRUCT,RGD)

	IF(STRUCT%ATS(I)%TYP.EQ.GE.AND.STRUCT%ATS(J)%TYP.EQ.GE)
	1 NUM=ADD(BE_GG,(EI+EJ)*0.25)
	IF(STRUCT%ATS(I)%TYP.EQ.SI.AND.STRUCT%ATS(J)%TYP.EQ.SI)
	1 NUM=ADD(BE_SS,(EI+EJ)*0.25)
	IF(STRUCT%ATS(I)%TYP.EQ.GE.AND.STRUCT%ATS(J)%TYP.EQ.SI) 
	1NUM=ADD(BE_SG,(EI+EJ)*0.25)
	IF(STRUCT%ATS(I)%TYP.EQ.SI.AND.STRUCT%ATS(J)%TYP.EQ.GE) 
	1NUM=ADD(BE_SG,(EI+EJ)*0.25)

	END IF 
     
      END DO	
	END DO


	END SUBROUTINE 



	END MODULE