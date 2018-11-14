	MODULE MDCONSTS

	REAL*8,PARAMETER,PRIVATE::SQRT3=1.732050808

	!тнплю йпхярюккю
	INTEGER*2,PARAMETER::CUB=1,ELL=2

	!цпюмхвмше сякнбхъ (оепхндхвеяйхе х феярйхе)
	INTEGER*2,PARAMETER::PRDC=1,RGD=2


	!рхо якнеб б йпхярюкке(IN - бмсрп.,FRONT - опхцпюмхв.,OUT-бмеьмхи)
	INTEGER*2,PARAMETER::IN=1,FRONT=2,OUT=3


	!рхош юрнлнб
      INTEGER*2,PARAMETER::SI=2,GE=1 
	 

	!оюпюлерп пеьерйх дкъ SI х GE
	REAL*8,PARAMETER:: PLT_SI=0.543072,PLT_GE=0.565748

	REAL*8,PARAMETER::PLT0(2)=(/PLT_GE,PLT_SI/)


	!пюбмнбеямне лефюрнлмне пюяярнъмхе	  	   
	REAL*8,PARAMETER:: R0_SI=PLT_SI*SQRT3/4.0,R0_GE=PLT_GE*SQRT3/4.0

	!пюгмхжю лефдс оюпюл-лх пеь-йх SI х GE
	REAL*8,PARAMETER::FX=0.042
	
     	!вхякн ох
	REAL*8,PARAMETER::PI=3.1415926


	!йняхмся хдеюкэмнцн сцкю лефдс юрнлюлх	
	REAL*8,PARAMETER::COSC=-1.0/3.0	
		
	
     	!люяяю юрнлнб SI х GE		   		
	REAL*8,PARAMETER::AM_SI=28.0855,AM_GE=72.59

	REAL*8,PARAMETER::MAS(2)=(/AM_GE,AM_SI/) 	

	!рхош онремжхюкю
	INTEGER*2,PARAMETER::P_STW=2,P_TSF=1

	!рхош йннпдхмюр
	INTEGER*2,PARAMETER::C_X=1,C_Y=2,C_Z=3
	 	
	
      !ьюц он йннпдхмюре 	   	       
	REAL*8,PARAMETER::DELT=0.01

					   
	INTEGER,PARAMETER::NGLUB=2

	!янаярбеммше вюярнрш йнкеаюмхи SI х GE
	REAL*8,PARAMETER::WT0=521.0,WL0=521.0

	
!	REAL*8,PARAMETER::W0_GE=((WT0*WT0+WL0*WL0)*0.5)**(1.0/2.0) !,W0_SI
	
     	!ьхпхмю онкняш яоейрпю (TO-лндш)	   
	REAL*8,PARAMETER::GAM_TO=0.02

	!тсмйжхъ яцкюфхбюмхъ яоейрпю
	INTEGER*2,PARAMETER::GSN=1,LRN=2	

	!рхош юрнлмнцн пюяопедекемхъ - яксвюимне,сонпъднвеммне,йкюяреп
	INTEGER*2,PARAMETER::RND=1,ORD=2,CLR=3


	!рхош ябъгеи
	INTEGER*2,PARAMETER::B_SS=2,B_GG=1,B_SG=3
	

      !рхош сцкнб
	INTEGER*2,PARAMETER::A_SSS=2,A_GGG=1,A_GGS=3,A_GSS=4

	!люйяхлюкэмше янаярбеммше гмювемхъ дхмюлхвеяйни люрпхжш
	REAL*8,PRIVATE,PARAMETER::EIGW_SI=1.143232955,EIGW_GE=0.38

	REAL*8,PARAMETER::EIGW_MAX(2)=(/EIGW_GE,EIGW_SI/)

	!йнкеаюмхъ лнд
	REAL*8,PARAMETER::W_T0=521.0,W_L0=521.0
	
      !янаярбеммше вюярнрш йнкеаюмхи
	REAL*8,PARAMETER::W0_SI=521.0,W0_GE=300.0
	
      REAL*8,PUBLIC,PARAMETER::W0(2)=(/W0_GE,W0_SI/)
			

	PUBLIC::SET_TERSF
	PUBLIC::SET_STW	

	!---оюпюлерпш онремжхюкю репянттю--------------------------------
	TYPE T_TERSOFF

	REAL*8  A,B
	REAL*8 LM,MU
	REAL*8 BETTA
	REAL*8 N,C,D,H
	REAL*8 R,S
	REAL*8 HI
	REAL*8 W

	END TYPE


	
	!----оюпюлерпш онремжхюкю ярхккхмдфепю-беаепю---------------------
	TYPE T_STW

	REAL*8 A
	REAL*8 B
	REAL*8 EN
	INTEGER P
	INTEGER Q
	REAL*8 LMD
	REAL*8 RA
	REAL*8 SIG
	REAL*8 GAM

	END TYPE
	
    
 
	!1 - GE,2 - SI,3 - GE-SI
	TYPE(T_TERSOFF),PUBLIC::TSF(3)

	!1 - GE-GE-GE,2 - SI-SI-SI,3 - SI-SI-GE,4 - GE-GE-SI 
	TYPE(T_STW),PUBLIC::STW(4)	 	
 

	CONTAINS  
	
	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE SET_TERSF

	!оюпюлерпш о-кю репянттю дкъ SI
	TSF(2)%A=1830.8  !щб
	TSF(2)%B=471.18  !щб
	TSF(2)%LM=24.799 !1/мл
	TSF(2)%MU=17.322 !1/мл
	TSF(2)%BETTA=1.1E-6
	TSF(2)%N=0.78734
	TSF(2)%C=100390
	TSF(2)%D=16.217
	TSF(2)%H=-0.59825
	TSF(2)%R=0.27	  !мл
	TSF(2)%S=0.3	  !мл
	TSF(2)%HI=1.0
	TSF(2)%W=1.0


	!оюпюлерпш о-кю репянттю дкъ GE
	TSF(1)%A=1769         !щб
	TSF(1)%B=419.23       !щб
	TSF(1)%LM=24.451      !1/мл
	TSF(1)%MU=17.047      !1/мл
	TSF(1)%BETTA=9.0166E-7
	TSF(1)%N=0.75627
	TSF(1)%C=106430
	TSF(1)%D=15.652
	TSF(1)%H=-0.43884
	TSF(1)%R=0.28	      !мл
	TSF(1)%S=0.31 	      !мл
	TSF(1)%HI=1.0
	TSF(1)%W=1


	!оюпюлерпш о-кю репянттю дкъ SI-GE
	TSF(3)%A=SQRT(TSF(1)%A*TSF(2)%A)      !щб
	TSF(3)%B=SQRT(TSF(1)%B*TSF(2)%B)      !щб
	TSF(3)%LM=(TSF(1)%LM+TSF(2)%LM)*0.5   !1/мл
	TSF(3)%MU=(TSF(1)%MU+TSF(2)%MU)*0.5   !1/мл
	TSF(3)%R=SQRT(TSF(1)%R*TSF(2)%R)	  !мл
	TSF(3)%S=SQRT(TSF(1)%S*TSF(2)%S) 	  !мл
	TSF(3)%HI=1.00061
	TSF(3)%W=1.00


	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE SET_STW

	!оюпюлерпш онремжхюкю ярхккхмфепю-беаепю дкъ SI-SI-SI
	STW(SI)%EN= 2.1672381 !щб
	STW(2)%A= 7.049556277
	STW(2)%B= 0.6022245584
	STW(2)%P=4.0
	STW(2)%Q=0.0
	STW(2)%RA=1.80
	STW(2)%LMD=21.0
	STW(2)%GAM=1.20
	STW(2)%SIG= PLT_SI*SQRT(3.0)/4./(2.0**(1./6.))	!HM

	!оюпюлерпш онремжхюкю ярхккхмфепю-беаепю дкъ GE-GE-GE
	STW(1)=STW(2)
	STW(1)%LMD=31.0
	STW(1)%EN=1.93 
	STW(1)%SIG=0.2181 !PLT_GE*SQRT(3.0)/4./(2.0**(1./6.))	!HM

	!оюпюлерпш онремжхюкю ярхккхмфепю-беаепю дкъ GE-GE-SI
	STW(3)=STW(1)
	STW(3)%LMD=27.66667
	STW(3)%EN=2.050 
	STW(3)%SIG=0.2124	!HM


	!оюпюлерпш онремжхюкю ярхккхмфепю-беаепю дкъ GE-SI-SI
	STW(4)=STW(3)
	STW(4)%LMD=24.33333	


	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------
	INTEGER*4 FUNCTION GET_NUMS(FORM,NUM_UNITS,NATOMS)
	
      INTEGER*2 FORM
	INTEGER*4 NUM_UNITS
	INTEGER*4,INTENT(OUT)::NATOMS

	INTEGER*4 RES

	RES=0

	IF(FORM.EQ.ELL) THEN
	
	IF(NUM_UNITS.EQ.2) THEN
	NATOMS=5
	RES=17
	END IF
	 
	IF(NUM_UNITS.EQ.4) THEN
	NATOMS=17
	RES=83
	END IF

	IF(NUM_UNITS.EQ.6) THEN
	NATOMS=83
	RES=239
	END IF

	IF(NUM_UNITS.EQ.8) THEN
	NATOMS=239
	RES=525
	END IF

	IF(NUM_UNITS.EQ.10) THEN
	NATOMS=525
	RES=981
	END IF

	IF(NUM_UNITS.EQ.12) THEN
	NATOMS=981
	RES=1647
	END IF

	END IF

	IF(FORM.EQ.CUB) THEN

	NATOMS=8*(NUM_UNITS-1)**3
	RES=8*NUM_UNITS**3 

	END IF

	GET_NUMS=RES

	END FUNCTION


	


	END MODULE