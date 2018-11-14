	MODULE MDBOUNDS

	USE MDTYPES
	USE MDCONSTS

	PRIVATE::PERIODIC_BOUNDS_CRD,PERIODIC_BOUNDS_ATOM
	PRIVATE::GETR_PER_BOUNDS_CRD
	PRIVATE::GETR_PER_BOUNDS_AT1,GETR_PER_BOUNDS_AT2
	PRIVATE::SEPARATION
	PUBLIC::U,DCRD

	!---------------INTERFACES--------------------------------

	!–≈¿À»«”≈“ œ≈–»Œƒ»◊≈— »≈ ”—ÀŒ¬»ﬂ ƒÀﬂ ¿“ŒÃ¿
	INTERFACE PERIODIC_BOUNDS
	MODULE PROCEDURE PERIODIC_BOUNDS_CRD,PERIODIC_BOUNDS_ATOM 
	END INTERFACE

	!–¿——“ŒﬂÕ»≈ Ã≈∆ƒ” ¿“ŒÃ¿Ã» — ”◊≈“ŒÃ œ≈–»Œƒ»◊≈— »’ ”—ÀŒ¬»…
	INTERFACE GETR
	MODULE PROCEDURE GETR_PER_BOUNDS_CRD,GETR_PER_BOUNDS_AT1,
	1GETR_PER_BOUNDS_AT2,GETR_BOUNDS_CRD,GETR_BOUNDS_STR
	END INTERFACE

	CONTAINS

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE PERIODIC_BOUNDS_CRD(CRD,CELL)

	TYPE(T_COORD) CELL
	TYPE(T_COORD),INTENT(INOUT)::CRD

	IF(CRD%X.LT.0) CRD%X=CRD%X+CELL%X
	IF(CRD%Y.LT.0) CRD%Y=CRD%Y+CELL%Y
	IF(CRD%Z.LT.0) CRD%Z=CRD%Z+CELL%Z

	IF(CRD%X.GT.CELL%X) CRD%X=CRD%X-CELL%X
	IF(CRD%Y.GT.CELL%Y) CRD%Y=CRD%Y-CELL%Y
	IF(CRD%Z.GT.CELL%Z) CRD%Z=CRD%Z-CELL%Z

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE PERIODIC_BOUNDS_ATOM(ATOM,CELL)

	TYPE(T_COORD) CELL
	TYPE(T_ATOM),INTENT(INOUT)::ATOM

	IF(ATOM%X.LT.0) ATOM%X=ATOM%X+CELL%X
	IF(ATOM%Y.LT.0) ATOM%Y=ATOM%Y+CELL%Y
	IF(ATOM%Z.LT.0) ATOM%Z=ATOM%Z+CELL%Z

	IF(ATOM%X.GT.CELL%X) ATOM%X=ATOM%X-CELL%X
	IF(ATOM%Y.GT.CELL%Y) ATOM%Y=ATOM%Y-CELL%Y
	IF(ATOM%Z.GT.CELL%Z) ATOM%Z=ATOM%Z-CELL%Z

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE SEPARATION(DX,DY,DZ,CELL)

	REAL*8,INTENT(INOUT)::DX,DY,DZ
	TYPE(T_COORD) CELL

	IF(DX.GT.0.5*CELL%X) DX=DX-CELL%X
	IF(DY.GT.0.5*CELL%Y) DY=DY-CELL%Y
	IF(DZ.GT.0.5*CELL%Z) DZ=DZ-CELL%Z

	IF(DX.LT.-0.5*CELL%X) DX=DX+CELL%X
	IF(DY.LT.-0.5*CELL%Y) DY=DY+CELL%Y
	IF(DZ.LT.-0.5*CELL%Z) DZ=DZ+CELL%Z

	END SUBROUTINE

	!-----------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION GETR_PER_BOUNDS_CRD(CRD1,CRD2,CELL)

	TYPE(T_COORD) CRD1,CRD2,CELL

	REAL*8 R
	REAL*8 DX,DY,DZ

	DX=CRD1%X-CRD2%X
	DY=CRD1%Y-CRD2%Y
	DZ=CRD1%Z-CRD2%Z
	
	CALL SEPARATION(DX,DY,DZ,CELL)
	 
      R=SQRT(DX*DX+DY*DY+DZ*DZ)
	GETR_PER_BOUNDS_CRD=R 

	END FUNCTION

	!--------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION	GETR_PER_BOUNDS_AT1(I,J,STRUCT,CELL)

	INTEGER*4 I,J
	TYPE(T_STRUCT) STRUCT
	TYPE(T_COORD) CELL 
	
      REAL*8 DX,DY,DZ

	DX=STRUCT%ATS(I)%X-STRUCT%ATS(J)%X
	DY=STRUCT%ATS(I)%Y-STRUCT%ATS(J)%Y
	DZ=STRUCT%ATS(I)%Z-STRUCT%ATS(J)%Z
	
	CALL SEPARATION(DX,DY,DZ,CELL)	 
    
	GETR_PER_BOUNDS_AT1=SQRT(DX*DX+DY*DY+DZ*DZ)

	END FUNCTION

	!--------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION	GETR_PER_BOUNDS_AT2(ATOM1,ATOM2,CELL)

	TYPE(T_ATOM) ATOM1,ATOM2
	TYPE(T_COORD) CELL 

	REAL*8 DX,DY,DZ

	DX=ATOM1%X-ATOM2%X
	DY=ATOM1%Y-ATOM2%Y
	DZ=ATOM1%Z-ATOM2%Z
	
	CALL SEPARATION(DX,DY,DZ,CELL)	 
    
	GETR_PER_BOUNDS_AT2=SQRT(DX*DX+DY*DY+DZ*DZ)	

	END FUNCTION
	
      !--------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION GETR_BOUNDS_STR(I,J,STRUCT)
	
	INTEGER*4 I,J
	TYPE(T_STRUCT) STRUCT    

	IF(STRUCT%BNDS.EQ.RGD) GETR_BOUNDS_STR=GETR(I,J,STRUCT%ATS)
	IF(STRUCT%BNDS.EQ.PRDC)
	1GETR_BOUNDS_STR=GETR_PER_BOUNDS_AT1(I,J,STRUCT,STRUCT%CELL)
     
      END FUNCTION

	!--------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION GETR_BOUNDS_CRD(CRDI,CRDJ,STRUCT)
	
	TYPE(T_COORD) CRDI,CRDJ
	TYPE(T_STRUCT) STRUCT 

	IF(STRUCT%BNDS.EQ.RGD) GETR_BOUNDS_CRD=GETR(CRDI,CRDJ)
	IF(STRUCT%BNDS.EQ.PRDC)GETR_BOUNDS_CRD=GETR(CRDI,CRDJ,STRUCT%CELL) 
     
      END FUNCTION
			

	!-------------------------------------------------------------------------------------------------------------------------
	TYPE(T_COORD) FUNCTION DCRD(I,J,STRUCT)

	TYPE(T_STRUCT) STRUCT
	INTEGER I,J
	
     	TYPE(T_COORD) CRD  

	CRD%X=STRUCT%ATS(J)%X-STRUCT%ATS(I)%X
	CRD%Y=STRUCT%ATS(J)%Y-STRUCT%ATS(I)%Y
	CRD%Z=STRUCT%ATS(J)%Z-STRUCT%ATS(I)%Z

	IF(STRUCT%BNDS.EQ.PRDC) 
	1CALL SEPARATION(CRD%X,CRD%Y,CRD%Z,STRUCT%CELL)
!	1CALL PERIODIC_BOUNDS_CRD(CRD,STRUCT%CELL)

	DCRD=CRD

	END FUNCTION

	!--------------------------------------------------------------------------------------------------------------------------
	REAL*8 FUNCTION U(ALPHA,I,J,STRUCT)
	
	TYPE(T_STRUCT) STRUCT
	INTEGER I,J
	INTEGER*2 ALPHA
	
	TYPE(T_COORD) CRD

	CRD=DCRD(I,J,STRUCT)

	SELECT CASE(ALPHA)

	CASE(C_X)
	   U= CRD%X        
	CASE(C_Y)
	   U = CRD%Y
	CASE(C_Z)
	   U = CRD%Z
	CASE DEFAULT
	   U=-9.61E+10

	END SELECT

	END FUNCTION


	END MODULE