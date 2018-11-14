	MODULE MDGLOBALVARS

	  USE MDTYPES

	  !вхякн якнеб йпхярюккю	
	  INTEGER NMAX

	  !днкъ цеплюмхъ 
	  REAL*8 XG
	   
	  !вхякн юрнлнб мюмнйпхярюккю                              
	  INTEGER NATOMS

	  !вхякн юрнлнб йпхярюккю                             
	  INTEGER NBUILD

	  INTEGER KFIN
	   
	  !гюдюммши люяяхб юрнлнб йпхярюккю	                         
	  TYPE(T_ATOM),POINTER,DIMENSION(:)::AT0

	  !онксвеммши оняке пекюйяюжхх люяяхб юрнлнб йпхярюккю        
	  TYPE(T_ATOM),POINTER,DIMENSION(:)::AT 
	  
	  !люяяхб юрнлнб б яхярелмшу йннпдхмюрюу     
	  TYPE(T_ATOM),POINTER,DIMENSION(:)::SYS_A
	  
	  !мнлепю, ябъгшбючыхе юрнлш б мнбшу х ярюпшу йннпдхмюрюу     
	  TYPE(T_NUMBER),POINTER,DIMENSION(:)::NUM

	  TYPE(T_COORD),POINTER,DIMENSION(:)::AN,ANN 	  
	  
	  !дхмюлхвеяйюъ люрпхжю  
	  REAL*8,POINTER,DIMENSION(:,:)::MATR

	  !янаярбеммше гмювемхъ х бейрнпю люрпхжш
	  TYPE(T_MATRIX) M_VN
	  
	  !дхмюлхвеяйюъ люрпхжю оняке дхюцнмюкхгюжхх
	  REAL*8,POINTER,DIMENSION(:,:)::DIAG_MATR 
	  
	  !оюпюлерпш юлнптхгюжхх
	  TYPE(T_AMORF_PARAMS) AMP         
	  
	  !пюдхюкэмне пюяопедекемхе дн х оняке пекюйяюжхх
	  TYPE(T_RADDISTR) INIT_FRR
	  TYPE(T_RADDISTR) FRR

	  !дпсцни бюпхюмр пюдхюкэмнцн пюяопедекемхъ
	  TYPE(T_RADDISTR) NEW_FRR
	  
	  !пюлюмнбяйне пюяопедекемхе
	  REAL*8,POINTER,DIMENSION(:)::ALPH	  

	  !пюлюмнбяйхи яоейрп
	  TYPE(T_SPECTRE) R_SP

	  !тнмнммши яоейрп
	  TYPE(T_SPECTRE) PH_SP


	END MODULE