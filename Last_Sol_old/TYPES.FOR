	MODULE MDTYPES


	PRIVATE::ADD_ATOM,ADD_REAL,ADD_INT
	PRIVATE::SET_RADDISTR_SIZE,SET_MATRIX_SIZE
	PRIVATE::REINIT_ATOM_ARRAY,REINIT_REAL_ARRAY
	PRIVATE::DELETE_STRUCT,DELETE_ATOM
	PRIVATE::GET_R_ATOMS1,GET_R_ATOMS2,GET_R_COORDS1,GET_R_COORDS2
	PRIVATE::CRD_ATOM4,CRD_ATOM8,GET_R_CRD_ATOM
	PUBLIC::DLT

	PRIVATE::ASSIGN_ATOM,ASSIGN_STRUCT


	!--------------------INTERFACES--------------------------------
		
	!«¿ƒ¿≈“  ŒŒ–ƒ»Õ¿“€ ¿“ŒÃ¿
	INTERFACE CRD_ATOM
	MODULE PROCEDURE  CRD_ATOM4,CRD_ATOM8
      END INTERFACE 

	!ƒŒ¡¿¬Àﬂ≈“ ›À≈Ã≈Õ“ ¬ Ã¿——»¬
	INTERFACE ADD 
	MODULE PROCEDURE ADD_ATOM,ADD_REAL,ADD_INT
	END INTERFACE

	!”ƒ¿Àﬂ≈“ ›À≈Ã≈Õ“ »« Ã¿——»¬¿ œŒ »Õƒ≈ —”
	INTERFACE REMOVE_IND
	MODULE PROCEDURE REMOVE_ATOM,REMOVE_REAL,REMOVE_INT_IND
	END INTERFACE 
	
	!”ƒ¿Àﬂ≈“ ›À≈Ã≈Õ“ »« Ã¿——»¬¿ œŒ «Õ¿◊≈Õ»ﬁ
      INTERFACE REMOVE_OBJ
	MODULE PROCEDURE REMOVE_INT_OBJ
      END INTERFACE      

	INTERFACE SET_SIZE
	MODULE PROCEDURE SET_RADDISTR_SIZE
	END INTERFACE

	!»«Ã≈Õ≈Õ»≈ √–¿Õ»÷ Ã¿——»¬¿
	INTERFACE REINIT
	MODULE PROCEDURE REINIT_ATOM_ARRAY,REINIT_REAL_ARRAY
	END INTERFACE

	!”ƒ¿À≈Õ»≈ Œ¡⁄≈ “Œ¬
	INTERFACE DELETE
	MODULE PROCEDURE DELETE_ATOM,DELETE_STRUCT
	END INTERFACE

	!–¿——“ŒﬂÕ»≈ Ã≈∆ƒ” ƒ¬”Ãﬂ “Œ◊ ¿Ã» »À» ¿“ŒÃ¿Ã»
	INTERFACE GETR
	MODULE PROCEDURE GET_R_ATOMS2,GET_R_COORDS2,
	1GET_R_CRD_ATOM,GET_R_ATOMS0
	END INTERFACE

	!---------------------------------------------------------------
	INTERFACE ASSIGNMENT(=)
	MODULE PROCEDURE ASSIGN_ATOM,ASSIGN_STRUCT
	END INTERFACE


	!------------------------TYPES---------------------------------

	!--------------------------------------------------------------
	!“–≈’Ã≈–Õ€≈  ŒŒ–ƒ»Õ¿“€
	TYPE T_COORD

	REAL*8 X,Y,Z

	END TYPE T_COORD

	!---------------------------------------------------------------
	TYPE T_ATOM

	! ŒŒ–ƒ»Õ¿“€
	REAL*8 X,Y,Z
	!¡À»∆¿…ÿ»≈ —Œ—≈ƒ»
	INTEGER*4 NEIBR(4)
	!¬“Œ–€≈ —Œ—≈ƒ» 
	INTEGER*4,POINTER,DIMENSION(:)::ENVRS
	!ÕŒÃ≈– ”–Œ¬Õﬂ 
	INTEGER*4 LEV
	!“»œ(SI »À» GE)   
	INTEGER*2 TYP
	!Ã¿——¿	
	REAL*8 M
	!Õ¿À»◊»≈		
	LOGICAL IS
	!“»œ —ÀŒﬂ		
	INTEGER*2 LAY

	END TYPE

	!--------------------------------------------------------------
	TYPE T_NUMBER

	INTEGER*4 IN,INN

	END TYPE

	!---------------------------------------------------------------
	TYPE T_STRUCT

	!Ã¿——»¬ ¿“ŒÃŒ¬
	TYPE(T_ATOM),POINTER,DIMENSION(:)::ATS
	!√–¿Õ»◊Õ€≈ ”—ÀŒ¬»ﬂ
	INTEGER*2 BNDS
	!–¿—◊≈“Õ¿ﬂ ﬂ◊≈… ¿        
	TYPE(T_COORD) CELL
	!ÕŒÃ≈–¿,—¬ﬂ«€¬¿ﬁŸ»≈ —““¿–€≈	 ŒŒ–ƒ»Õ¿“€ — ÕŒ¬€Ã»					 
	TYPE(T_NUMBER),POINTER,DIMENSION(:)::NUMS	  
	!‘Œ–Ã¿  –»—“¿ÀÀ¿	
	INTEGER*2 FORM
	!◊»—ÀŒ ¿“ŒÃŒ¬ ﬂƒ–¿								 
	INTEGER*4 NATOMS
	!ÕŒÃ≈–¿ ¿“ŒÃŒ¬,	—Œ—“¿¬ÀﬂﬁŸ»’ ¬Õ”“–≈ÕÕ»…, œ–»√–¿Õ»◊Õ€… » ¬Õ≈ÿÕ»… —ÀŒ» ——Œ“¬.						 
	INTEGER*4,POINTER,DIMENSION(:)::INS,FRONTS,OUTS
		
	END TYPE 

	!---------------------------------------------------------------
	TYPE T_AMORF_PARAMS

	!«¿“–¿¬ ¿ ƒÀﬂ √—À
	INTEGER*4 IX
	!Œ“ÕŒ—»“≈À‹Õ€… —ƒ¬»√ √–¿Õ»◊Õ€’ » ¬Õ”“–≈ÕÕ»’ —ÀŒ≈¬
	REAL*8 SDVIG, SDVCR
	!œ–Œ÷≈Õ“ ¬¿ ¿Õ—»… 
	REAL*8 PRVAC
	!◊»—ÀŒ —ƒ¬»Õ”“€’ ¬Õ”“–≈ÕÕ»’ —ÀŒ≈¬
	INTEGER*4 NALIC

	END TYPE T_AMORF_PARAMS	
	
	
      !-------------------------------------------------- 
	TYPE T_RADDISTR

	INTEGER*4,POINTER,DIMENSION(:)::N
	REAL*8,POINTER,DIMENSION(:)::R
	REAL*8,POINTER,DIMENSION(:)::NS

	END TYPE

	!----------------------------------------------------
	TYPE T_SPECTRE

	REAL*8,POINTER,DIMENSION(:)::W
	REAL*8,POINTER,DIMENSION(:)::SP
	REAL*8,POINTER,DIMENSION(:)::SPR 

	END TYPE


	CONTAINS

	!--------------------------------------------------------------------------------------------------------------------------
	REAL FUNCTION DLT(I,J) 

	INTEGER I,J

	IF(I.EQ.J)THEN
	DLT=1
	ELSE 
	DLT=0
	END IF

	END FUNCTION

	!----------------------------------------------------------------------------------------------------------------------------
	TYPE(T_ATOM) FUNCTION CRD_ATOM4(X,Y,Z)

	REAL X,Y,Z

	TYPE(T_ATOM) ATOM

	ATOM%X=X
	ATOM%Y=Y
	ATOM%Z=Z

	CRD_ATOM4=ATOM

	END FUNCTION

	!----------------------------------------------------------------------------------------------------------------------------
	TYPE(T_ATOM) FUNCTION CRD_ATOM8(X,Y,Z)

	REAL*8 X,Y,Z

	TYPE(T_ATOM) ATOM

	ATOM%X=X
	ATOM%Y=Y
	ATOM%Z=Z

	CRD_ATOM8=ATOM

	END FUNCTION


	!----------------------------------------------------------------------------------------------------------------------------
	INTEGER*4 FUNCTION ADD_ATOM(ATOMS,NEW_ATOM)
	
	TYPE(T_ATOM),POINTER,DIMENSION(:)::ATOMS
	TYPE(T_ATOM) NEW_ATOM

	TYPE(T_ATOM),POINTER,DIMENSION(:)::TEMP

	IF(SIZE(ATOMS).GT.0) THEN

	ALLOCATE(TEMP(SIZE(ATOMS)))
	TEMP=ATOMS

	ALLOCATE(ATOMS(SIZE(TEMP)+1))

	DO I=1,SIZE(TEMP)
	ATOMS(I)=TEMP(I)
	END DO

	ATOMS(SIZE(ATOMS))=NEW_ATOM

	DEALLOCATE(TEMP)
	ADD_ATOM=SIZE(ATOMS)

	ELSE

	ALLOCATE(ATOMS(1))
	ATOMS(1)=NEW_ATOM
	ADD_ATOM=1

	END IF

	END FUNCTION

	!------------------------------------------------------------------------------------------------------------------------------
	INTEGER*4 FUNCTION ADD_REAL(REALS,NEW_REAL)

	REAL*8,POINTER,DIMENSION(:)::REALS
	REAL*8 NEW_REAL

	REAL*8,POINTER,DIMENSION(:)::TEMP

	IF(SIZE(REALS).GT.0) THEN

	ALLOCATE(TEMP(SIZE(REALS)))
	TEMP=REALS

	ALLOCATE(REALS(SIZE(TEMP)+1))

	DO I=1,SIZE(TEMP)
	REALS(I)=TEMP(I)
	END DO

	REALS(SIZE(REALS))=NEW_REAL

	DEALLOCATE(TEMP)
	ADD_REAL=SIZE(REALS)

	ELSE

	ALLOCATE(REALS(1))
	REALS(1)=NEW_REAL
	ADD_REAL=1

	END IF

	END FUNCTION
	
      !--------------------------------------------------------------------------------------------------------------------------------
	INTEGER*4 FUNCTION ADD_INT(INTS,NEW_INT)

	INTEGER*4,POINTER,DIMENSION(:)::INTS
	INTEGER*4 NEW_INT

	INTEGER*4,POINTER,DIMENSION(:)::TEMP
!	INTEGER*4 J

!	J=SIZE(INTS) 
!	WRITE(*,*) 'J',J
 
	IF(SIZE(INTS).GT.0) THEN


	ALLOCATE(TEMP(SIZE(INTS)))
	TEMP=INTS

	ALLOCATE(INTS(SIZE(TEMP)+1))

	DO I=1,SIZE(TEMP)
	INTS(I)=TEMP(I)
	END DO

	INTS(SIZE(INTS))=NEW_INT

	DEALLOCATE(TEMP)
	ADD_INT=SIZE(INTS)

	ELSE 

	ALLOCATE(INTS(1))
	INTS(1)=NEW_INT	
	ADD_INT=1

	END IF

	END FUNCTION

	!-----------------------------------------------------------------------------------------------------------------------------------
	LOGICAL FUNCTION REMOVE_ATOM(ATOMS,I)

	TYPE(T_ATOM),POINTER,DIMENSION(:)::ATOMS
	INTEGER*4 I

	TYPE(T_ATOM),POINTER,DIMENSION(:)::TEMP

	IF(SIZE(ATOMS).EQ.0.OR.I.LE.0.OR.I.GT.SIZE(ATOMS)) THEN

	REMOVE_ATOM=.FALSE.

	ELSE

	ALLOCATE(TEMP(SIZE(ATOMS)))

	DO J=1,SIZE(TEMP)
	TEMP(J)=ATOMS(J)
	END DO

	ALLOCATE(ATOMS(SIZE(TEMP)-1))

	DO J=1,SIZE(TEMP)

	IF(J.LT.I) ATOMS(J)=TEMP(J)

	IF(J.GT.I) ATOMS(J-1)=TEMP(J)

	END DO

	REMOVE_ATOM=.TRUE.

	END IF

	END FUNCTION

	!-----------------------------------------------------------------------------------------------------------------------------------
	LOGICAL FUNCTION REMOVE_INT_IND(INTS,I)

	INTEGER*4,POINTER,DIMENSION(:)::INTS
	INTEGER*4 I

	INTEGER*4,POINTER,DIMENSION(:)::TEMP

	IF(SIZE(INTS).EQ.0.OR.I.LE.0.OR.I.GT.SIZE(INTS)) THEN

	REMOVE_INT_IND=.FALSE.

	ELSE

	ALLOCATE(TEMP(SIZE(INTS)))

	DO J=1,SIZE(TEMP)
	TEMP(J)=INTS(J)
	END DO

	ALLOCATE(INTS(SIZE(TEMP)-1))

	DO J=1,SIZE(TEMP)

	IF(J.LT.I) INTS(J)=TEMP(J)

	IF(J.GT.I) INTS(J-1)=TEMP(J)

	END DO

	REMOVE_INT_IND=.TRUE.

	END IF

	END FUNCTION

	!-----------------------------------------------------------------------------------------------------------------------------------
	LOGICAL FUNCTION REMOVE_REAL(REALS,I)

	REAL*8,POINTER,DIMENSION(:)::REALS
	INTEGER*4 I

	REAL*8,POINTER,DIMENSION(:)::TEMP

	IF(SIZE(REALS).EQ.0.OR.I.LE.0.OR.I.GT.SIZE(REALS)) THEN

	REMOVE_REAL=.FALSE.

	ELSE

	ALLOCATE(TEMP(SIZE(REALS)))

	DO J=1,SIZE(TEMP)
	TEMP(J)=REALS(J)
	END DO

	ALLOCATE(REALS(SIZE(TEMP)-1))

	DO J=1,SIZE(TEMP)

	IF(J.LT.I) REALS(J)=TEMP(J)

	IF(J.GT.I) REALS(J-1)=TEMP(J)

	END DO

	REMOVE_REAL=.TRUE.

	END IF

	END FUNCTION

	!-----------------------------------------------------------------------------------------------------------------------------------
	LOGICAL FUNCTION REMOVE_INT_OBJ(INTS,INT)

	INTEGER*4,POINTER,DIMENSION(:)::INTS
	INTEGER*4 INT

	INTEGER*4 I

	I=0
	DO J=1,SIZE(INTS)

	IF(INT.EQ.INTS(J)) THEN
	I=J
	EXIT
	END IF

	END DO	 	

	REMOVE_INT_OBJ=REMOVE_INT_IND(INTS,I)
			 
	
     	END FUNCTION 
	!--------------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE REINIT_ATOM_ARRAY(ATOMS,I)

	TYPE(T_ATOM),POINTER,DIMENSION(:)::ATOMS,TEMP
	INTEGER*4 I

	N=SIZE(ATOMS)

	IF(I.GT.N) THEN	

	ALLOCATE(TEMP(N))
	TEMP=ATOMS	

	ALLOCATE(ATOMS(I))
		
	DO  J=1,N
 	ATOMS(J)=TEMP(J) 
      END DO

	DO  J=N+1,SIZE(ATOMS)
	DO  K=1,4
      ATOMS(J)%NEIBR(K)=0
	END DO
	END DO

	DEALLOCATE(TEMP)

	END IF

	END SUBROUTINE

	!-------------------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE REINIT_REAL_ARRAY(REALS,I)

	REAL*8,POINTER,DIMENSION(:)::REALS,TEMP
	INTEGER*4 I

	N=SIZE(REALS)

	IF(I.GT.N) THEN	

	ALLOCATE(TEMP(N))
	TEMP=REALS	

	ALLOCATE(REALS(I))
		
	DO  J=1,N
 	REALS(J)=TEMP(J) 
      END DO 	

	DEALLOCATE(TEMP)

	END IF

	END SUBROUTINE

	!------------------------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE DELETE_ATOM(ATOM)

	TYPE(T_ATOM),INTENT(INOUT)::ATOM
	
	DEALLOCATE(ATOM%ENVRS) 

	END SUBROUTINE

	!------------------------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE DELETE_STRUCT(STRUCT)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	
	DEALLOCATE(STRUCT%ATS) 
	DEALLOCATE(STRUCT%NUMS) 
	DEALLOCATE(STRUCT%INS) 
	DEALLOCATE(STRUCT%FRONTS) 
	DEALLOCATE(STRUCT%OUTS) 

	END SUBROUTINE


	!------------------------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE SET_RADDISTR_SIZE(RADDISTR,N)

	TYPE(T_RADDISTR),INTENT(INOUT)::RADDISTR
	INTEGER*4 N

	ALLOCATE(RADDISTR%N(N))
	ALLOCATE(RADDISTR%R(N))
	ALLOCATE(RADDISTR%NS(N))

	END SUBROUTINE

	!------------------------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION GET_R_ATOMS0(I,J,ATOMS)

	INTEGER*4 I,J
	TYPE(T_ATOM),POINTER,DIMENSION(:)::ATOMS

	GET_R_ATOMS0=SQRT((ATOMS(I)%X-ATOMS(J)%X)**2+
	1(ATOMS(I)%Y-ATOMS(J)%Y)**2+
     1(ATOMS(I)%Z-ATOMS(J)%Z)**2)
	

	END FUNCTION

	!------------------------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION GET_R_ATOMS2(ATOM1,ATOM2)

	TYPE(T_ATOM) ATOM1,ATOM2

	GET_R_ATOMS2=SQRT((ATOM1%X-ATOM2%X)**2+(ATOM1%Y-ATOM2%Y)**2+
	1(ATOM1%Z-ATOM2%Z)**2)

	END FUNCTION

	!--------------------------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION GET_R_COORDS2(CRD1,CRD2)

	TYPE(T_COORD) CRD1,CRD2

	GET_R_COORDS2=SQRT((CRD1%X-CRD2%X)**2+(CRD1%Y-CRD2%Y)**2+
	1(CRD1%Z-CRD2%Z)**2)

	END FUNCTION


	!----------------------------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION GET_R_CRD_ATOM(ATOM,CRD)

      TYPE(T_ATOM) ATOM
	TYPE(T_COORD) CRD

	GET_R_CRD_ATOM=SQRT((CRD%X-ATOM%X)**2+(CRD%Y-ATOM%Y)**2+
	1 (CRD%Z-ATOM%Z)**2)	

	END FUNCTION 		  

	!----------------------------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE	ASSIGN_ATOM(OUT_ATOM,IN_ATOM)

	TYPE(T_ATOM),INTENT(IN)::IN_ATOM
	TYPE(T_ATOM),INTENT(OUT)::OUT_ATOM

	OUT_ATOM%TYP=IN_ATOM%TYP
	OUT_ATOM%M=IN_ATOM%M
	OUT_ATOM%IS=IN_ATOM%IS
	OUT_ATOM%LAY=IN_ATOM%LAY
	OUT_ATOM%LEV=IN_ATOM%LEV
	OUT_ATOM%X=IN_ATOM%X
	OUT_ATOM%Y=IN_ATOM%Y
	OUT_ATOM%Z=IN_ATOM%Z
	OUT_ATOM%NEIBR=IN_ATOM%NEIBR

	ALLOCATE(OUT_ATOM%ENVRS(0))

	IF(SIZE(IN_ATOM%ENVRS).GT.0) THEN

	ALLOCATE(OUT_ATOM%ENVRS(SIZE(IN_ATOM%ENVRS)))
	DO I=1,SIZE(OUT_ATOM%ENVRS)
	OUT_ATOM%ENVRS(I)=IN_ATOM%ENVRS(I)
	END DO
	
      END IF 	

	END SUBROUTINE

	
	!----------------------------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE	ASSIGN_STRUCT(OUT_STRUCT,IN_STRUCT)

	TYPE(T_STRUCT),INTENT(IN)::IN_STRUCT
	TYPE(T_STRUCT),INTENT(OUT)::OUT_STRUCT

	

	OUT_STRUCT%FORM=IN_STRUCT%FORM
	OUT_STRUCT%BNDS=IN_STRUCT%BNDS
	OUT_STRUCT%CELL=IN_STRUCT%CELL
	OUT_STRUCT%NATOMS=IN_STRUCT%NATOMS

	ALLOCATE(OUT_STRUCT%ATS(SIZE(IN_STRUCT%ATS))) 
	
	DO I=1,SIZE(OUT_STRUCT%ATS)
	CALL ASSIGN_ATOM(OUT_STRUCT%ATS(I),IN_STRUCT%ATS(I))
	END DO 

	ALLOCATE(OUT_STRUCT%NUMS(SIZE(IN_STRUCT%NUMS)))
	DO I=1,SIZE(OUT_STRUCT%NUMS)
	OUT_STRUCT%NUMS(I)=IN_STRUCT%NUMS(I)
	END DO

	ALLOCATE(OUT_STRUCT%INS(SIZE(IN_STRUCT%INS)))
	DO I=1,SIZE(OUT_STRUCT%INS)
	OUT_STRUCT%INS(I)=IN_STRUCT%INS(I)
	END DO

	ALLOCATE(OUT_STRUCT%OUTS(SIZE(IN_STRUCT%OUTS)))
	DO I=1,SIZE(OUT_STRUCT%OUTS)
	OUT_STRUCT%OUTS(I)=IN_STRUCT%OUTS(I)
	END DO

	ALLOCATE(OUT_STRUCT%FRONTS(SIZE(IN_STRUCT%FRONTS)))
	DO I=1,SIZE(OUT_STRUCT%FRONTS)
	OUT_STRUCT%FRONTS(I)=IN_STRUCT%FRONTS(I)
	END DO

	

	END SUBROUTINE

	END MODULE MDTYPES