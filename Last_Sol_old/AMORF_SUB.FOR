    !-------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE AMORF_SUB(STRUCT,POTENT_TYPE,DTIME,MAX_STEPS,PLT,TMAX,
	1TMIN)

	USE MDTYPES
	USE MDCONSTS
	USE MDBOUNDS
	USE MDSTRUCT 
	USE MDRELAX	

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT	!йпхярюкк
	REAL*8 DTIME							!ярюпрнбши ьюц он бпелемх
	INTEGER*4 MAX_STEPS						!люйяхлюкэмне йнкхвеярбн ьюцнб
	INTEGER*2 POTENT_TYPE					!рхо онремжхюкю
	REAL*8 PLT								!оюпюлерп пеьерйх йпхярюккю
	REAL*8 TMAX                             !люйяхлюкэмюъ релоепюрспю окюбкемхъ йпхярюккю
	REAL*8 TMIN								!релоепюрспю няршбюмхъ

	REAL*8 E0	                            !мювюкэмюъ щмепцхъ
	REAL*8 E_FULL                           !онкмюъ щмепцхъ
	REAL*8 EK1	                            !йхм. щм-хъ опед. ьюцю
	REAL*8 EP1	                            !онремж. щм-хъ опед. ьюцю
	REAL*8 EK2	                            !йхм. щм-хъ рейсы. ьюцю
	REAL*8 EP2	                            !онремж. щм-хъ рейсы. ьюцю
			
	REAL*8 DELTA                            !нрмняхрекэмюъ пюгмхжю щмепцхи
	REAL*8 TIME	                            !онкмне бпелъ пекюйяюжхх
	INTEGER*4 STEP   	                    !мнлеп бпелеммнцн ьюцю
	INTEGER*4 SK                            !вхякн сдювмшу ьюцнб 
	REAL*8,POINTER,DIMENSION(:)::EK,EP      !люяяхбш йхм. х онремж. щм-хи дн (гюмскемхъ яй-еи)
	REAL*8 VS_OLD,VS_NEW					!япедмейбюдпюрхвмше яйнпнярх  мю онякед. х опед. ьюцюу
	REAL*8::T0=100.0                        !релоепюрспю, я йнрнпни мювхмюел мюцпебюмхе
	REAL*8::T_STEP=100.0 					!ьюц сбекхвемхъ релоепюрспш
	INTEGER*4::N_TSTEP=25                   !йнкхвеярбн ьюцнб, вепег йнрнпне сбекхвхбюел релоепюрспс яхярелш
	REAL*8 T                                !рейсыюъ релоепюрспю яхярелш
	INTEGER*4 CONT							!нярюмнбйю хкх опнднкфемхе пекюйяюжхх

	
	!гюдюел оюпюлерпш пекюйяюжхх
	CALL CREATE_SYSTEM(STRUCT,DTIME)		
	
	!йхмерхвеяйюъ щмепцхъ, яннрберярбсчыюъ гюдюммни релоепюрспе
	EK1=3.0/2.0*SIZE(STRUCT%ATS)*KB*T0


	!гдеяэ гюдюел яйнпнярх, яннрберярбсчыхе гюдюммни релоепюрспе
	VS_OLD=SCALE_SPEEDS(T0)


	!бшвхякъел мювюкэмсч щмепцхч
	E0=ENERGY(STRUCT,POTENT_TYPE,SIZE(RSTR%ATS)) 	

	WRITE(*,*) 'E0=',E0
	!-------------------------------------------------------------------	
		
	EP1=E0	
      STEP=1
	TIME=0.0
	SK=0
	E_FULL=EK1+EP1
	T=T0

	ALLOCATE(EK(1))
	ALLOCATE(EP(1))

	EK(1)=0.0
	EP(1)=E0
	DELTA=1E-6		

	!----------------меоняпедярбеммн пекюйяюжхъ----------------------------
	DO WHILE(STEP<MAX_STEPS)

	!янупюмемхе йннпд-р, яй-еи х сяй-хи
	CALL SAVE_COORDS	

	!бшвхякемхе онремж. щм-хх
	IF(POTENT_TYPE.EQ.P_TSF) THEN	 
	CALL ITER(TERSF_I)
	EP2=ENERGY(RSTR,P_TSF,SIZE(RSTR%ATS))
	ELSE 
	CALL ITER(STW_I)
	EP2=ENERGY(RSTR,P_STW,SIZE(RSTR%ATS))
	END IF

	!бшв-хе йхм. щм-хх
	EK2=EKIN(RSTR)


	WRITE(*,*) 'STEP=',STEP
	WRITE(*,*) 'EK1=',EK1,'EK2=',EK2
	WRITE(*,*) 'EP1=',EP1,'EP2=',EP2

	!онкмше щм-хх - мю опедшдсы. х рейсы. ьюцюу
	E1=EK1+EP1
	E2=EK2+EP2

	WRITE(*,*) 'E1=',E1,'E2=',E2
	WRITE(*,*) 'DEP=',EP1-EP2,'DEK=',EK2-EK1
	WRITE(*,*) 'E0=',E0



	!-----еякх щм-хъ ме янупюм-яъ хкх ясллю хглемемхи йхм. х онремж. щм-хх ме мнкэ-----
	IF10:IF(ABS((E2-E_FULL)/E_FULL).GT.1E-4.OR.((EP2-EP1)*
	1(EK1-EK2).LT.0.AND.ABS((EP2-EP1)*(EK1-EK2)).GT.1E-6)) THEN

	!--------хглемемхъ щмепцхх нвемэ люкш - лхмхлсл онремжхюкю-------------------------
	IF2:IF(ABS(EK1-EK2).LT.1E-6.OR.ABS(EP1-EP2).LT.1E-6) THEN

	WRITE(*,*) '-> ENERGY MINIMUM'

!	IF(ABS((EP2-E0)/E0).LT.0.001) EXIT

      !--------хгл-хъ онремж. щм-хх нвемэ люкш--------------------------------------------
      IF3:IF(ABS((EP2-E0)/E0).LT.DELTA) THEN 
	
	WRITE(*,*) 'DELTA E <0.001. CONTINUE MELTING? 0-NO,1-YES'
	READ(*,*) CONT
	IF(CONT.EQ.0) EXIT      

	END IF IF3 

	IF(T.GE.TMAX) THEN
	WRITE(*,*) 'T=',T
	WRITE(*,*) 'MELTING WAS ENDED'
	EXIT
	END IF

	!сбекхвемхе релоепюрспш
	T=T+T_STEP
	WRITE(*,*) 'NEW TEMPR=',T
	VS_NEW=SCALE_SPEEDS(T)
	WRITE(*,*) 'SPEEDS WERE SCALED'

	!нвхярйю люяяхбнб щм-хи
	ALLOCATE(EK(1))
	ALLOCATE(EP(1))

	!гюдюмхе мювюкэмшу щм-хи
	EK1=EKIN(RSTR)
	E_FULL=EK1+EP2
	EP1=EP2
	EK(1)=EK1
	EP(1)=EP2
	
	IF(DT.LT.1E-3) DT=1E-3


	ELSE  

	!----------------ьюц месдювмши----------------------------------------
	WRITE(*,*) 'ENERGY IS NOT KEPT'
	DT=DT*0.5  
	SK=0	

	IF4:IF(DT.LT.1E-10) THEN

      WRITE(*,*) 'DT<1E-10'
	DT=0.001
	WRITE(*,*) 'CHANGE DELTA? 0-NO,1-YES'
	READ(*,*) REL
	 
	IF5:IF(REL.NE.0) THEN
	WRITE(*,*) 'INPUT DELTA'
	READ(*,*) DELTA
      END IF IF5 

	END IF	IF4

	WRITE(*,*) 'NEW DT=',DT	
	
	!гюцпсгйю ярюпшу йннпдхмюр, р. й. ьюц месдювмши
	CALL LOAD_COORDS

	END IF	IF2

	ELSE

	!-------------ьюц явхрюел сдювмшл--------------------------------------
	STEP=STEP+1

	IF(T.GE.TMAX) THEN
	WRITE(*,*) 'RELAX ENDED'
	EXIT
	END IF

	!вепег йюфдше 10 ьюцнб мюундхл брнпше яняедх юрнлнб
	IF(MOD(STEP,10).EQ.0) THEN
	CALL SET_ENVIRS(RSTR,2.0*PLT*SQRT(3.0)/4.0)
	WRITE(*,*) 'ENVIRONMENT WAS FOUNDED'
	END IF	  

	!бшвхякемхе нямнбмшу оюпюлерпнб
	TIME=TIME+DT
	WRITE(*,*) 'TIME=',TIME
	EK1=EK2
	EP1=EP2
	SK=SK+1
	
	IF(T.GE.TMAX) EXIT
	 

	!вепег йюфдше N_TSTEP ьюцнб сбекхвхбюел релоепюрспс
	IF(MOD(STEP,N_TSTEP).EQ.0) THEN
	T=T+T_STEP
	WRITE(*,*) 'NEW TEMPR=',T
	END IF

	!люяьрюахпнбюмхе яйнпняреи б яннрберярбхх я релоепюрспни
	VS_NEW=SCALE_SPEEDS(T)
	WRITE(*,*) 'VS_NEW=',VS_NEW
	EK1=EKIN(RSTR)
	E_FULL=EK1+EP1

	!еякх вхякн сдювмшу ьюцнб анкэье 10 - лнфмн сбекхвхрэ бпелеммни ьюц
	IF(SK.GT.10.AND.DT.LE.0.0051) THEN
	SK=0
	DT=2.0*DT
	WRITE(*,*) 'NEW DT=',DT	 
	END IF

	!днаюбкемхе мнбшу щкелемрнб б люяяхб щмепцхи
	NM=ADD(EP,EP2)
	NM=ADD(EK,EK2) 

	END IF IF10	

	END DO

	DEALLOCATE(EK)
	DEALLOCATE(EP) 		 

	CALL GET_RESULT(STRUCT)
	CALL DESTROY_SYSTEM

	END SUBROUTINE
