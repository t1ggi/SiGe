	MODULE MDGLOBALVARS

	  USE MDTYPES

	  !����� ����� ���������	
	  INTEGER NMAX

	  !���� �������� 
	  REAL*8 XG
	   
	  !����� ������ �������������                              
	  INTEGER NATOMS

	  !����� ������ ���������                             
	  INTEGER NBUILD

	  INTEGER KFIN
	   
	  !�������� ������ ������ ���������	                         
	  TYPE(T_ATOM),POINTER,DIMENSION(:)::AT0

	  !���������� ����� ���������� ������ ������ ���������        
	  TYPE(T_ATOM),POINTER,DIMENSION(:)::AT 
	  
	  !������ ������ � ��������� �����������     
	  TYPE(T_ATOM),POINTER,DIMENSION(:)::SYS_A
	  
	  !������, ����������� ����� � ����� � ������ �����������     
	  TYPE(T_NUMBER),POINTER,DIMENSION(:)::NUM

	  TYPE(T_COORD),POINTER,DIMENSION(:)::AN,ANN 	  
	  
	  !������������ �������  
	  REAL*8,POINTER,DIMENSION(:,:)::MATR

	  !����������� �������� � ������� �������
	  TYPE(T_MATRIX) M_VN
	  
	  !������������ ������� ����� ��������������
	  REAL*8,POINTER,DIMENSION(:,:)::DIAG_MATR 
	  
	  !��������� �����������
	  TYPE(T_AMORF_PARAMS) AMP         
	  
	  !���������� ������������� �� � ����� ����������
	  TYPE(T_RADDISTR) INIT_FRR
	  TYPE(T_RADDISTR) FRR

	  !������ ������� ����������� �������������
	  TYPE(T_RADDISTR) NEW_FRR
	  
	  !����������� �������������
	  REAL*8,POINTER,DIMENSION(:)::ALPH	  

	  !����������� ������
	  TYPE(T_SPECTRE) R_SP

	  !�������� ������
	  TYPE(T_SPECTRE) PH_SP


	END MODULE