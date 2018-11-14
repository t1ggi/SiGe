	MODULE MDRELAX

	USE MDTYPES
	USE MDCONSTS
	USE MDPOTENTS 
	USE MDBOUNDS

	!яйнпнярх
	TYPE(T_COORD),POINTER,DIMENSION(:)::V,V0
	!сяйнпемхъ
	TYPE(T_COORD),POINTER,DIMENSION(:)::A,A0
	!юрнлш б пекюйяхпсчыеи ярпсйрспе
	TYPE(T_STRUCT) RSTR,RSTR0
	!онкмне вхякн х вхякн ондбхфмшу юрнлнб йпхярюккю
	INTEGER*4,PRIVATE::N,N_MOVES
	!бпелеммни ьюц
	REAL*8,PUBLIC::DT  	

	PUBLIC::CREATE_SYSTEM,DESTROY_SYSTEM 
	PUBLIC::GET_RESULT 
	PUBLIC::ITER
	PRIVATE::ACLR	
	PUBLIC::SAVE_COORDS,LOAD_COORDS
	PUBLIC::EKIN	

	INTEGER*4,PRIVATE::KMAX
	REAL*8,PRIVATE::TAU	
	TYPE(T_COORD),PRIVATE::CELL			

	CONTAINS

	!----------------------------------------------------------------------------------------------------------------------------
	!гюдюер нямнбмше оюпюлерпш пекюйяюжхх
	SUBROUTINE CREATE_SYSTEM(STRUCT,DTIME)

	TYPE(T_STRUCT) STRUCT
	REAL*8 DTIME 	
      
	N=SIZE(STRUCT%ATS)	

	N_MOVES=STRUCT%NATOMS


	DT=DTIME 	

	ALLOCATE(V(N_MOVES))
	ALLOCATE(A(N_MOVES))		
	ALLOCATE(V0(N_MOVES))
	ALLOCATE(A0(N_MOVES))
		
	RSTR=STRUCT
	RSTR0=STRUCT

	DO I=1,N_MOVES       	
      V(I)=T_COORD(0.0,0.0,0.0) 
	A(I)=T_COORD(0.0,0.0,0.0)
      END DO
	
	V0=V
	A0=A 		

      KMAX=10								   	
	
	 
	END  SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE DESTROY_SYSTEM 
	
	DEALLOCATE(V)
	DEALLOCATE(A)
	DEALLOCATE(V0)
	DEALLOCATE(A0)
	CALL DELETE(RSTR)
!	CALL DELETE(RSTR0)	

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE GET_RESULT(STRUCT)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
		
      STRUCT=RSTR	
      
	END SUBROUTINE 

	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякъер сяйнпемхе я свернл цпюмхвмшу сякнбхи BOUNDS х гюдюммнцн онр-кю (POTENT)
	TYPE(T_COORD) FUNCTION ACLR(I,CRD,STRUCT,POTENT)

	INTERFACE
	  REAL*8 FUNCTION POTENT(I,CRD,STRUCT)
	     USE MDTYPES
	     INTEGER*4 I
	     TYPE(T_COORD) CRD	     
		 TYPE(T_STRUCT) STRUCT		  
	  END FUNCTION
	END INTERFACE	

	INTEGER*4 I

	TYPE(T_COORD) CRD
	TYPE(T_STRUCT) STRUCT 	 
      	
	TYPE(T_COORD) ACS,UP1,UM1,UP2,UM2 	

	CRD%X=CRD%X+DELT
	UP1%X=POTENT(I,CRD,STRUCT)
	CRD%X=CRD%X-2.0*DELT
	UM1%X=POTENT(I,CRD,STRUCT)
	CRD%X=CRD%X+DELT 

 
	CRD%Y=CRD%Y+DELT
	UP1%Y=POTENT(I,CRD,STRUCT)
	CRD%Y=CRD%Y-2.0*DELT
	UM1%Y=POTENT(I,CRD,STRUCT)
	CRD%Y=CRD%Y+DELT


      CRD%Z=CRD%Z+DELT
	UP1%Z=POTENT(I,CRD,STRUCT)
	CRD%Z=CRD%Z-2.0*DELT
	UM1%Z=POTENT(I,CRD,STRUCT)
	CRD%Z=CRD%Z+DELT

	
      CRD%X=CRD%X+2.0*DELT
	UP2%X=POTENT(I,CRD,STRUCT)
	CRD%X=CRD%X-4.0*DELT
	UM2%X=POTENT(I,CRD,STRUCT)
	CRD%X=CRD%X+2.0*DELT

	ACS%X=-1./(12.*DELT)*(8.0*UP1%X-UP2%X-8.0*UM1%X+UM2%X)

	CRD%Y=CRD%Y+2.0*DELT
	UP2%Y=POTENT(I,CRD,STRUCT)
	CRD%Y=CRD%Y-4.0*DELT
	UM2%Y=POTENT(I,CRD,STRUCT)
	CRD%Y=CRD%Y+2.0*DELT

	ACS%Y=-1./(12.*DELT)*(8.0*UP1%Y-UP2%Y-8.0*UM1%Y+UM2%Y)

	CRD%Z=CRD%Z+2.0*DELT
	UP2%Z=POTENT(I,CRD,STRUCT)
	CRD%Z=CRD%Z-4.0*DELT
	UM2%Z=POTENT(I,CRD,STRUCT)
	CRD%Z=CRD%Z+2.0*DELT

	ACS%Z=-1./(12.*DELT)*(8.0*UP1%Z-UP2%Z-8.0*UM1%Z+UM2%Z)
 
	ACLR=ACS

	END FUNCTION 

	!-----------------------------------------------------------------------------------------------------------------------
	!хрепюжхъ юкцнпхрлю бепке
	SUBROUTINE ITER(POTENT)

	INTERFACE
	  REAL*8 FUNCTION POTENT(I,CRD,STRUCT)
           USE MDTYPES
	     INTEGER*4 I
	     TYPE(T_COORD) CRD
		 TYPE(T_STRUCT) STRUCT		 		  
	  END FUNCTION
	END INTERFACE


!	REAL*8,INTENT(OUT)::EK,EP


	REAL*8 DITMAX
	TYPE(T_COORD) CRD0,ACS0,SP0
	TYPE(T_COORD) SP,CRD,ACS,CORD
	REAL*8 DCRD
	REAL*8::EPS6=0.01 
	
	

	DITMAX = 0.0
	 
	DO I=1,N_MOVES

      IF(RSTR%ATS(I)%IS)THEN

      K=0
	CRD=T_COORD(RSTR%ATS(I)%X,RSTR%ATS(I)%Y,RSTR%ATS(I)%Z) 
	
      DO WHILE(1)
	     
      ACS=ACLR(I,CRD,RSTR,POTENT)

      IF(K.EQ.0) THEN
	
	CRD0=T_COORD(RSTR%ATS(I)%X,RSTR%ATS(I)%Y,RSTR%ATS(I)%Z)
	ACS0=ACS
	SP0=V(I)    

	END IF

      K=K+1

	!
      SP%X=SP0%X+DT*(ACS0%X+ACS%X)/(2.0*RSTR%ATS(I)%M)
      SP%Y=SP0%Y+DT*(ACS0%Y+ACS%Y)/(2.0*RSTR%ATS(I)%M)
      SP%Z=SP0%Z+DT*(ACS0%Z+ACS%Z)/(2.0*RSTR%ATS(I)%M)

      CORD%X=CRD0%X+0.5*DT*(SP0%X+SP%X) 
      CORD%Y=CRD0%Y+0.5*DT*(SP0%Y+SP%Y) 
      CORD%Z=CRD0%Z+0.5*DT*(SP0%Z+SP%Z)
   
      !юкцнпхрл бепке
!	CORD%X=CRD0%X+SP0%X*DT+1.0/(2.0*R_A(I)%M)*ACS0%X*DT*DT 
!	CORD%Y=CRD0%Y+SP0%Y*DT+1.0/(2.0*R_A(I)%M)*ACS0%Y*DT*DT
!	CORD%Z=CRD0%Z+SP0%Z*DT+1.0/(2.0*R_A(I)%M)*ACS0%Z*DT*DT

      DCRD=SQRT(((CORD%X-CRD%X)**2)+((CORD%Y-CRD%Y)**2)
	1+((CORD%Z-CRD%Z)**2))
	  

	IF(K.EQ.1) THEN
	IF(DCRD.GT.DITMAX) DITMAX=DCRD
	END IF

	IF(DCRD.LT.EPS6) EXIT
	IF(K.EQ.KMAX) EXIT

	IF(RSTR%BNDS.EQ.PRDC) CALL PERIODIC_BOUNDS(CORD,RSTR%CELL)

	CRD%X=CORD%X
      CRD%Y=CORD%Y
      CRD%Z=CORD%Z 

	END DO

      RSTR%ATS(I)%X=CORD%X
      RSTR%ATS(I)%Y=CORD%Y
      RSTR%ATS(I)%Z=CORD%Z

      V(I)=SP

	END IF 

      END DO 

	END SUBROUTINE

	!------------------------------------------------------------------------------------------------------------------------
	!йхмерхвеяйюъ щмепцхъ (б щб)
	FUNCTION EKIN(STRUCT)

	TYPE(T_STRUCT) STRUCT

	REAL*8 EK

	EK=0.0
	DO I=1,SIZE(V) 
	EK=EK+0.5*STRUCT%ATS(I)%M*(V(I)%X**2+V(I)%Y**2+V(I)%Z**2)
	END DO

	EKIN=EK

	END FUNCTION

	!-----------------------------------------------------------------------------------------------------------------------
	!янупюмемхе яйнпняреи,сяйнпемхи х йннпдхмюр (опнхгбндхряъ опх йюфдни сдювмни хрепюжхх бепке)
	SUBROUTINE SAVE_COORDS

	RSTR0=RSTR
	V0=V
	A0=A

	END SUBROUTINE

	!------------------------------------------------------------------------------------------------------------------------
	!гюцпсгйю хяундмши яйнпняреи,сяйнпемхи х йннпдхмюр (опнхгбндхряъ опх йюфдни месдювмни хрепюжхх бепке)
	SUBROUTINE LOAD_COORDS

	RSTR=RSTR0
	V=V0
	A=A0

	END SUBROUTINE
	
      !---------------------------------------------------------------------------------------------------------------------------
	!гюмскемхе яйнпняреи (опх днярхфемхх люйяхлслю йхмерхвеяйни щмепцхх)
	SUBROUTINE NULL_SPEEDS

	DO I=1,SIZE(V)

	V(I)=T_COORD(0.0,0.0,0.0)
	V0(I)=V(I)

	END DO
	
      END SUBROUTINE


	

	END MODULE	MDRELAX