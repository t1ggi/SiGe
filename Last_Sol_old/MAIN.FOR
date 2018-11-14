	PROGRAM     MAIN
     	
	USE MDTYPES
	USE MDCONSTS
	USE MDSTRUCT	
	USE MDPOTENTS
	USE MDDYNAM_TSF
	USE MDDYNAM_STW
	USE MDMATRIX_TRANSFS
	USE MDRAMAN_SPECTRES
	
	
	INTEGER*2 I_FORM,I_DISTR
	INTEGER*4  NUM_LAYS,NUM_CELL
	REAL*8 PLT
      REAL*8 AX
	REAL*8 XG
	TYPE(T_STRUCT) STRUCT
	TYPE(T_AMORF_PARAMS) AMP 
	INTEGER*2 I_BNDS	
	REAL*8 DTIME
	INTEGER*2 POTENT 	
	REAL*8,POINTER,DIMENSION(:)::L_SS,L_SG,L_GG
	REAL*8 SS,SG,GG

	real*8 av_sss, av_ssg, av_sgs, av_gsg, av_sgg, av_ggg  !ЯПЕДМХЕ СЦКШ ЛЕФДС ЯННРБ. ЮРНЛЮЛХ
	real*8 D_sss, D_ssg, D_sgs, D_gsg, D_sgg, D_ggg  !ДХЯОЕПЯХЪ СЦКНБ


	TYPE(T_ANGLES) ANGLES
	REAL*8,POINTER,DIMENSION(:,:)::MATR,EIG_MATR
	REAL*8,POINTER,DIMENSION(:)::EIG_VALS
	TYPE(T_SPECTRE) R_SP,PH_SP
	REAL*8,POINTER,DIMENSION(:)::ALPH
	REAL*8 DW

	INTEGER*4 NUM_ATOMS,NUM_INS,NUM_OUTS,NUM_FRONTS	 

      OPEN(UNIT=2,FILE='_BONDS_LENGTHS.TXT',STATUS='OLD')
	OPEN(UNIT=3,FILE='_BOND_ENERGIES.TXT',STATUS='OLD')
	OPEN(UNIT=4,FILE='_ANGLES.TXT',STATUS='OLD')
	OPEN(UNIT=5,FILE='_DINAM_MATRIX.TXT',STATUS='OLD')
	OPEN(UNIT=6,FILE='_OWN_VECTS.TXT',STATUS='OLD')
	OPEN(UNIT=7,FILE='_PHONON_SPECTRE.TXT',STATUS='OLD')
      OPEN(UNIT=8,FILE='_RAMAN_SPECTRE.TXT',STATUS='OLD')	 
	

	!гюдюмхе оюпюлерпнб онремжхюкнб 
	CALL SET_TERSF
	CALL SET_STW	
	
	!-------------------------
	WRITE(*,*) 'READ STRUCTURE FROM FILES? 0-NO,1-YES'
	READ(*,*) READ_STRUCT
	
      IF_STRUCT:IF(READ_STRUCT.EQ.0) THEN
	!-------------------------------------------------------------------------------------------------------
	!-----------------бшвхякемхе ярпсйрспш-------------------------------------------------------
      !
	NUM_CLR=0

	!тнплю йпхярюккю
	WRITE(*,*) 'INPUT CRYSTALL FORM,2-ELLIPSE,1-CUBIC' 
	READ(*,*) I_FORM

	!рхо пюяопедекемхъ юрнлнб б йпхярюкке
	WRITE(*,*) 'ATOMS DISTRIB,1-RANDOM,ORDER-2,CLASTER-3' 
	READ(*,*) I_DISTR 

	
	!опх яксвюимнл пюяопедекемхх юрнлнб гюдюел опнжемр цеплюмхъ х оюпюлерп пеьерйх
	IF(I_DISTR.EQ.RND)	THEN

	WRITE(*,*) 'INPUT DOLJU GE' !у-днкъ юрнлнб цеплюмхъ
	READ(*,*) XG
	
	!днкъ цеплюмхъ б оюпюлерпе пеьерйх ярпсйрспш
	WRITE(*,*) 'INPUT DOLJU Ge in LATTICE PARAM'		  
	READ(*,*) AX

	!оюпюлерп пеьерйх - бшвхякъеряъ он гюйнмс бецюпдю
	PLT=PLT_SI*(1.0-AX)+PLT_GE*AX
	WRITE(*,*) 'PLT=',PLT
	 
	END IF	
	    

	!гюдюел цпюмхвмше сякнбхъ(с щккхоянхдю-бяецдю феярйхе)
	IF(I_FORM.EQ.ELL) THEN	
      
	I_BNDS=RGD

	ELSE

	WRITE(*,*) 'SET BOUNDS, 2 - RIGID,1 - PERIODIC'
	READ(*,*) I_BNDS

	END IF


	!ярпнхл йпхярюкк б яннрберярбхх я гюдюммшлх оюпюлерпюлх 
	IF(I_FORM.EQ.ELL) THEN

	!-------щккхоянхд-------------------------
	WRITE(*,*) 'INPUT NUM LAYS'	 !вхякн якнеб
	READ(*,*) NUM_LAYS

	!гюдюел вхякн юрнлнб б йкюярепе
	IF(I_DISTR.EQ.CLR) THEN

	WRITE(*,*) 'INPUT ATOMS IN CLASTER, NUMBER SHOULD BE LESS',
	1GET_NUMS(I_FORM,NUM_LAYS,NATOMS)  
	READ(*,*) NUM_CLR

	END IF

	STRUCT=CREATE_STRUCT(NUM_LAYS,ELL,I_DISTR,GE,SI,PLT,XG,I_BNDS,
	1NUM_CLR) ! тсмйжхъ (йнмярпсйрнп) CREATE_STRUCT дкъ онярп. йпхяр-ю
			 !  б STRUCT.FOR
      ELSE
      
	!---------йса----------------------------------
	WRITE(*,*) 'INPUT NUM CELLS'
	READ(*,*) NUM_CELL

	!гюдюел вхякн юрнлнб б йкюярепе
	IF(I_DISTR.EQ.CLR) THEN

	WRITE(*,*) 'INPUT ATOMS IN CLASTER, NUMBER SHOULD BE LESS',
	1GET_NUMS(I_FORM,NUM_CELL,NATOMS)  
	READ(*,*) NUM_CLR

	END IF

	STRUCT=CREATE_STRUCT(NUM_CELL,CUB,I_DISTR,GE,SI,PLT,XG,I_BNDS,
	1NUM_CLR)	

	END IF
	
	WRITE(*,*) 'STRUCT COUNTED'
	WRITE(*,*) 'NBUILD=',SIZE(STRUCT%ATS)
	WRITE(*,*) 'NATOMS=',STRUCT%NATOMS			
	!-------------------------------------------------------------------------------------------------------
		
		
	!--------------------------------------------------------------------------------------------------
	!---------------------юлнптхгюжхъ------------------------------------------------------------------
      WRITE(*,*) 'AMORF? 1-YES,0-NO'
	READ(*,*) DO_AMORF

	IF(DO_AMORF.NE.0) THEN

	WRITE(*,*) 'INPUT IX'
	READ(*,*) AMP%IX 

	WRITE(*,*) 'INPUT PRVAC'
	READ(*,*) AMP%PRVAC

	WRITE(*,*) 'INPUT SHIFT BOUND LAYS'
	READ(*,*) AMP%SDVIG

	WRITE(*,*) 'INPUT SHIFT CRYSTALL'
	READ(*,*) AMP%SDVCR

	WRITE(*,*) 'INPUT NUM SHIFT LAYS'
	READ(*,*) AMP%NALIC
	
     	CALL AMORF(STRUCT,AMP)	

	END IF
	!-------------------------------------------------------------------------------------------------


	!--------------------------------------------------------------------------------------------------
	!сярюмюбкхбюел пюявермсч ъвеийс, еякх йсахвеяйюъ ярпсйрспю мехдеюкэмю
	WRITE(*,*) 'SET CELL? 0-NO,1-YES'
	READ(*,*) SET_NEW_CELL

	IF(SET_NEW_CELL.NE.0) THEN
	IF(I_FORM.EQ.CUB.AND.STRUCT%BNDS.EQ.PRDC)  
	1CALL FIND_CELL(STRUCT,PLT,NUM_CELL,0.5)
	END IF 

	!сярюмюбкхбюел нйпсфючыхе юрнлш
	CALL SET_ENVIRS(STRUCT,2.0*PLT*SQRT(3.0)/4.0)
	!-----------------------------------------------------------------------------------------------


	!-----------------------------------------------------------------------------------------------
	!опнамне бшвхякемхе щмепцхх
	WRITE(*,*) 'COUNT ENERGY? 0-NO,1-YES'
	READ(*,*) COUNT_ENERGY
	
	IF(COUNT_ENERGY.NE.0) THEN 
	WRITE(*,*) 'STW ENERGY = ',
     1STW_FULL(STRUCT,SIZE(STRUCT%ATS)) !SIZE(AT))
	WRITE(*,*) 'TSF ENERGY = ',
	1TERSF_FULL(STRUCT,SIZE(STRUCT%ATS)) !SIZE(AT))
	END IF
	!------------------------------------------------------------------------------------------------
		
		
    	!------------------------------------------------------------------------------------------------
	!------------------------RELAX--------------------------------------------------------------------
	WRITE(*,*) 'DO RELAX ? 1 - YES, 0 - NO'
	READ(*,*) DO_RELAX 		

	IF1:IF(DO_RELAX.NE.0) THEN

	WRITE(*,*) 'INPUT DT'
	READ(*,*) DTIME

	WRITE(*,*) 'INPUT POTENTIAL. 2-STW,1-TSF'
	READ(*,*) POTENT

	CALL RELAX(STRUCT,POTENT,DTIME,100000,PLT)

	!_________________________________________________________________
	WRITE(*,*) STW_FULL(STRUCT,SIZE(STRUCT%ATS))
	
	!___________гюохяэ б тюик онксвеммни ярпсйрспш____________________
	WRITE(*,*) 'WRITE RESULT INTO FILES? 0-NO,1-YES'
	READ(*,*) WRITE_RESULT

	IF(WRITE_RESULT.NE.0) THEN

	IF(STRUCT%FORM.EQ.ELL) NUM_UNITS=NUM_LAYS
	IF(STRUCT%FORM.EQ.CUB) NUM_UNITS=NUM_CELL	
	

	WRITE(*,*) 'WRITE PARAMS OF STRUCT'
	OPEN(UNIT=117,FILE='_RELAX_PARAMS.TXT',STATUS='OLD')
	WRITE(117,*) XG,'      днкъ цеплюмхъ'
	WRITE(117,*) I_FORM,'   тнплю йпхярюккю'
	WRITE(117,*) PLT,'     оюпюлерп пеьерйх'
	WRITE(117,*) NUM_UNITS,'      вхякн якнеб(ъвеей)'		
	WRITE(117,*) STRUCT%NATOMS,'    вхякн ондбхфмшу юрнлнб'
	WRITE(117,*) STRUCT%BNDS,'     рхо цпюмхвмшу сякнбхи'
	WRITE(117,*) STRUCT%CELL
	WRITE(117,*) SIZE(STRUCT%ATS),'     онкмне вхякн юрнлнб'
	WRITE(117,*) SIZE(STRUCT%INS),'      вхякн бмсрпеммху юрнлнб'
	WRITE(117,*) SIZE(STRUCT%OUTS),'      вхякн бмеьмху юрнлнб'
	WRITE(117,*) SIZE(STRUCT%FRONTS),'      вхякн опхцпюмхвмшу юрнлнб'

	CLOSE(UNIT=117)	

	WRITE(*,*) 'WRITE TYPES OF STRUCT'
	OPEN(UNIT=114,FILE='_RELAX_TYPES.TXT',STATUS='OLD')
	DO I=1,SIZE(STRUCT%ATS)
	WRITE(114,*) STRUCT%ATS(I)%TYP,STRUCT%ATS(I)%M
	END DO
	CLOSE(UNIT=114)

	WRITE(*,*) 'WRITE COORDS OF STRUCT'
	OPEN(UNIT=111,FILE='_RELAX_COORDS.TXT',STATUS='OLD')	
	DO I=1,SIZE(STRUCT%ATS)
	WRITE(111,*)  STRUCT%ATS(I)%X,STRUCT%ATS(I)%Y,STRUCT%ATS(I)%Z
	END DO
	CLOSE(UNIT=111)

	OPEN(UNIT=112,FILE='_RELAX_NEIBRS.TXT',STATUS='OLD')
	WRITE(*,*) 'WRITE ATOMS NEIBRS '
	DO I=1,SIZE(STRUCT%ATS)
	WRITE(112,*) STRUCT%ATS(I)%NEIBR(1),STRUCT%ATS(I)%NEIBR(2),
	1STRUCT%ATS(I)%NEIBR(3),STRUCT%ATS(I)%NEIBR(4)
	END DO
	CLOSE(UNIT=112)

	WRITE(*,*) 'WRITE ATOMS LEVELS'
	OPEN(UNIT=115,FILE='_RELAX_LEVELS.TXT',STATUS='OLD')
	DO I=1,SIZE(STRUCT%ATS)
	WRITE(115,*) STRUCT%ATS(I)%LEV,STRUCT%ATS(I)%LAY,STRUCT%ATS(I)%IS
	END DO
	CLOSE(UNIT=115)

	WRITE(*,*) 'WRITE ATOMS LAYS'
	OPEN(UNIT=116,FILE='_RELAX_LAYS.TXT',STATUS='OLD')
	DO I=1,MAX(SIZE(STRUCT%INS),SIZE(STRUCT%OUTS),SIZE(STRUCT%FRONTS))
	WRITE(116,*) STRUCT%INS(I),STRUCT%OUTS(I),STRUCT%FRONTS(I)
	END DO
	CLOSE(UNIT=116)	

	WRITE(*,*) 'WRITE ATOMS NUMS'
	OPEN(UNIT=113,FILE='_RELAX_NUMS.TXT',STATUS='OLD')
	DO I=1,SIZE(STRUCT%NUMS)
	WRITE(113,*) STRUCT%NUMS(I)%IN,STRUCT%NUMS(I)%INN
	END DO
	CLOSE(UNIT=113)
	
      END IF 

	END IF	IF1

	!------------------------------------------------------------------------------------------------ 

	ELSE   !IF_STRUCT

	!____________времхе ярпсйрспш хг тюикнб_____________________________________

	OPEN(UNIT=117,FILE='_RELAX_PARAMS.TXT',STATUS='OLD')
	READ(117,*)  XG
	READ(117,*)  STRUCT%FORM
	READ(117,*) PLT	
	READ(117,*) NUM_UNITS		
	READ(117,*) STRUCT%NATOMS
	READ(117,*) STRUCT%BNDS
	READ(117,*) STRUCT%CELL
	READ(117,*) NUM_ATOMS
	READ(117,*) NUM_INS
	READ(117,*) NUM_OUTS
	READ(117,*) NUM_FRONTS
	CLOSE(UNIT=117)	

	ALLOCATE(STRUCT%ATS(NUM_ATOMS))
	ALLOCATE(STRUCT%NUMS(NUM_ATOMS))
	ALLOCATE(STRUCT%INS(NUM_INS))
	ALLOCATE(STRUCT%OUTS(NUM_OUTS))
	ALLOCATE(STRUCT%FRONTS(NUM_FRONTS))


	WRITE(*,*) 'READ TYPES OF STRUCT'
	OPEN(UNIT=114,FILE='_RELAX_TYPES.TXT',STATUS='OLD')
	DO I=1,SIZE(STRUCT%ATS)
	READ(114,*) STRUCT%ATS(I)%TYP,STRUCT%ATS(I)%M
	END DO
	CLOSE(UNIT=114)

	WRITE(*,*) 'READ COORDS OF STRUCT'
	OPEN(UNIT=111,FILE='_RELAX_COORDS.TXT',STATUS='OLD')	
	DO I=1,SIZE(STRUCT%ATS)
	READ(111,*)  STRUCT%ATS(I)%X,STRUCT%ATS(I)%Y,STRUCT%ATS(I)%Z
	END DO
	CLOSE(UNIT=111)

	OPEN(UNIT=112,FILE='_RELAX_NEIBRS.TXT',STATUS='OLD')
	WRITE(*,*) 'READ ATOMS NEIBRS '
	DO I=1,SIZE(STRUCT%ATS)
	READ(112,*) STRUCT%ATS(I)%NEIBR(1),STRUCT%ATS(I)%NEIBR(2),
	1STRUCT%ATS(I)%NEIBR(3),STRUCT%ATS(I)%NEIBR(4)
	END DO
	CLOSE(UNIT=112)

	WRITE(*,*) 'READ ATOMS LEVELS'
	OPEN(UNIT=115,FILE='_RELAX_LEVELS.TXT',STATUS='OLD')
	DO I=1,SIZE(STRUCT%ATS)
	READ(115,*) STRUCT%ATS(I)%LEV,STRUCT%ATS(I)%LAY,STRUCT%ATS(I)%IS
	END DO
	CLOSE(UNIT=115)

	WRITE(*,*) 'READ ATOMS LAYS'
	OPEN(UNIT=116,FILE='_RELAX_LAYS.TXT',STATUS='OLD')
	DO I=1,MAX(SIZE(STRUCT%INS),SIZE(STRUCT%OUTS),SIZE(STRUCT%FRONTS))
	READ(116,*) I_IN,I_OUT,I_FRONT
	IF(I.LE.NUM_INS)    STRUCT%INS(I)=I_IN
	IF(I.LE.NUM_OUTS)   STRUCT%OUTS(I)=I_OUT
	IF(I.LE.NUM_FRONTS) STRUCT%FRONTS(I)=I_FRONT
	END DO
	CLOSE(UNIT=116)	

	WRITE(*,*) 'READ ATOMS NUMS'
	OPEN(UNIT=113,FILE='_RELAX_NUMS.TXT',STATUS='OLD')
	DO I=1,SIZE(STRUCT%NUMS)
	READ(113,*) STRUCT%NUMS(I)%IN,STRUCT%NUMS(I)%INN
	END DO
	CLOSE(UNIT=113)		  

	END IF IF_STRUCT 
	! --------------------------------------------------------------------------------------------------
	!____________бшвхякемхе пюяопедекемхи сцкнб лефдс ябъгълх__________________________________
	WRITE(*,*) 'COUNT BOND ANGLES? 0-NO,1-YES'
	READ(*,*) COUNT_ANGLES

	IF(COUNT_ANGLES.NE.0) THEN
	
	av_sss = 0 
	av_ssg = 0 
	av_sgs = 0 
	av_gsg = 0 
	av_sgg = 0 
	av_ggg = 0

	D_sss = 0
	D_ssg = 0
	D_sgs = 0
	D_gsg = 0
	D_sgg = 0
	D_ggg = 0

	CALL BOND_ANGLES(STRUCT%ATS,ANGLES)

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if(SIZE(ANGLES%SSS).GT.0) then
		SS=0.0
		WRITE(4,*) 'SI-SI-SI		1'

		DO I=1,SIZE(ANGLES%SSS)
			ANGLES%SSS(I) = (ANGLES%SSS(I)) * 57.29577951308232087679822
			SS=SS+ANGLES%SSS(I)
			WRITE(4,*) ANGLES%SSS(I)
		END DO

		av_sss = SS	/ SIZE(ANGLES%SSS)

		DO I=1,SIZE(ANGLES%SSS)
			D_sss = D_sss + (ANGLES%SSS(I)-av_sss)*(ANGLES%SSS(I)-av_sss)
		END DO
		D_sss = D_sss / SIZE(ANGLES%SSS)

		WRITE(*,*)'SI-SI-SI',SS/SIZE(ANGLES%SSS)
	endif


	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if(SIZE(ANGLES%GSS)/=0) THEN
		SS=0.0
		WRITE(4,*) 'SI-SI-GE		2'

		DO I=1,SIZE(ANGLES%GSS)
			ANGLES%GSS(I) = (ANGLES%GSS(I)) * 57.29577951308232087679822
			SS=SS+ANGLES%GSS(I)
			WRITE(4,*) ANGLES%GSS(I)
		END DO

		av_ssg = SS / SIZE(ANGLES%GSS)


		DO I=1,SIZE(ANGLES%GSS)
			D_ssg = D_ssg + (ANGLES%GSS(I)-av_ssg)*(ANGLES%GSS(I)-av_ssg)
		END DO
		D_ssg = D_ssg / SIZE(ANGLES%GSS)


		WRITE(*,*)'SI-SI-GE',SS/SIZE(ANGLES%GSS) 
	endif

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if(SIZE(ANGLES%SGS).GT.0) then
		SS=0.0
		WRITE(4,*) 'SI-GE-SI		3'

		DO I=1,SIZE(ANGLES%SGS)
			ANGLES%SGS(I) = (ANGLES%SGS(I)) * 57.29577951308232087679822
			SS=SS+ANGLES%SGS(I)
			WRITE(4,*) ANGLES%SGS(I)
		END DO

		av_sgs = SS / SIZE(ANGLES%SGS)


		DO I=1,SIZE(ANGLES%SGS)
			D_sgs = D_sgs + (ANGLES%SGS(I)-av_sgs)*(ANGLES%SGS(I)-av_sgs)
		END DO
		D_sgs = D_sgs / SIZE(ANGLES%SGS)


		WRITE(*,*)'SI-GE-SI',SS/SIZE(ANGLES%SGS) 
	endif


	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if(SIZE(ANGLES%GSG).GT.0) then
     		SS=0.0
		WRITE(4,*) 'GE-SI-GE		4'

		DO I=1,SIZE(ANGLES%GSG)
			ANGLES%GSG(I) = (ANGLES%GSG(I)) * 57.29577951308232087679822
			SS=SS+ANGLES%GSG(I)
			WRITE(4,*) ANGLES%GSG(I)
		END DO

		av_gsg = SS / SIZE(ANGLES%GSG)

		DO I=1,SIZE(ANGLES%GSG)
			D_gsg = D_gsg + (ANGLES%GSG(I)-av_gsg)*(ANGLES%GSG(I)-av_gsg)
		END DO
		D_gsg = D_gsg / SIZE(ANGLES%GSG)


		WRITE(*,*)'GE-SI-GE',SS/SIZE(ANGLES%GSG)
	endif
 

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if(SIZE(ANGLES%SGG).GT.0) then
		SS=0.0
		WRITE(4,*) 'SI-GE-GE		5'

		DO I=1,SIZE(ANGLES%SGG)
			ANGLES%SGG(I) = (ANGLES%SGG(I)) * 57.29577951308232087679822
			SS=SS+ANGLES%SGG(I)
			WRITE(4,*) ANGLES%SGG(I)
		END DO

		av_sgg = SS / SIZE(ANGLES%SGG)

		DO I=1,SIZE(ANGLES%SGG)
			D_sgg = D_sgg + (ANGLES%SGG(I)-av_sgg)*(ANGLES%SGG(I)-av_sgg)
		END DO
		D_sgg = D_sgg / SIZE(ANGLES%SGG)


		WRITE(*,*)'SI-GE-GE',SS/SIZE(ANGLES%SGG)
	endif
 

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if(SIZE(ANGLES%GGG).GT.0) then
		SS=0.0
		WRITE(4,*) 'GE-GE-GE		6'

		DO I=1,SIZE(ANGLES%GGG)
			ANGLES%GGG(I) = (ANGLES%GGG(I)) * 57.29577951308232087679822
			SS=SS+ANGLES%GGG(I)
			WRITE(4,*) ANGLES%GGG(I)
		END DO

		av_ggg = SS / SIZE(ANGLES%GGG)

		DO I=1,SIZE(ANGLES%GGG)
			D_ggg = D_ggg + (ANGLES%GGG(I)-av_ggg)*(ANGLES%GGG(I)-av_ggg)
		END DO
		D_ggg = D_ggg / SIZE(ANGLES%GGG)


		WRITE(*,*)'GE-GE-GE',SS/SIZE(ANGLES%GGG) 
	endif
 
     
!	 ГЮОХЯЭ ЯПЕДМХУ ГМЮВЕМХИ СЦКНБ Б ТЮИК

	if(SIZE(ANGLES%SSS).GT.0) WRITE(4,*)'average SI-SI-SI ',av_sss

	if(SIZE(ANGLES%GSS)/=0) WRITE(4,*)'average SI-SI-GE ',av_ssg

	if(SIZE(ANGLES%SGS).GT.0) WRITE(4,*)'average SI-GE-SI ',av_sgs

	if(SIZE(ANGLES%GSG).GT.0) WRITE(4,*)'average GE-SI-GE ',av_gsg

     	if(SIZE(ANGLES%SGG).GT.0) WRITE(4,*)'average SI-GE-GE ',av_sgg

      if(SIZE(ANGLES%GGG).GT.0) WRITE(4,*)'average GE-GE-GE ',av_ggg

!	 ГЮОХЯЭ ДХЯОЕПЯХИ СЦКНБ Б ТЮИК
	if(SIZE(ANGLES%SSS).GT.0) WRITE(4,*)'Dispersion SI-SI-SI ',D_sss

	if(SIZE(ANGLES%GSS)/=0) WRITE(4,*)'Dispersion SI-SI-GE ',D_ssg
	
	if(SIZE(ANGLES%SGS).GT.0) WRITE(4,*)'Dispersion SI-GE-SI ',D_sgs

	if(SIZE(ANGLES%GSG).GT.0) WRITE(4,*)'Dispersion GE-SI-GE ',D_gsg

     	if(SIZE(ANGLES%SGG).GT.0) WRITE(4,*)'Dispersion SI-GE-GE ',D_sgg

      if(SIZE(ANGLES%GGG).GT.0) WRITE(4,*)'Dispersion GE-GE-GE ',D_ggg


	END IF





	!_______________бшвхякемхе пюяопедекемхи дкхм ябъгеи___________________
	WRITE(*,*) 'FIND BOND LENGTHS? 0-NO,1-YES'
	READ(*,*) COUNT_BONDS

	IF(COUNT_BONDS.NE.0) THEN

	CALL FIND_BONDS_LENGTHS(STRUCT%ATS,L_SS,L_SG,L_GG)	

	SS=0.0
	WRITE(2,*)'BOND SI-SI'
	DO I=1,SIZE(L_SS)
	SS=SS+L_SS(I)
	WRITE(2,*) L_SS(I) 
	END DO

	IF(SIZE(L_SS).GT.0) WRITE(*,*) 'BOND LENGTH SI-SI',SS/SIZE(L_SS)

	SG=0.0
	WRITE(2,*)'BOND SI-GE'
	DO I=1,SIZE(L_SG)
	SG=SG+L_SG(I)
	WRITE(2,*) L_SG(I) 
	END DO

	IF(SIZE(L_SG).GT.0) WRITE(*,*) 'BOND LENGTH SI-GE',SG/SIZE(L_SG)

	GG=0.0
	WRITE(2,*)'BOND GE-GE'
	DO I=1,SIZE(L_GG)
	GG=GG+L_GG(I)
	WRITE(2,*) L_GG(I) 
	END DO

	IF(SIZE(L_GG).GT.0) WRITE(*,*) 'BOND LENGTH GE-GE',GG/SIZE(L_GG)

	END IF
	
	!________бшвхякемхе пюяопедекемхъ щмепцхх ябъгеи_________________
	WRITE(*,*) 'COUNT ENERGY DISTRIB? 0-NO,1-YES'
	READ(*,*) COUNT_ENERGY_DISTR 
	
	IF(COUNT_ENERGY_DISTR.NE.0) THEN

	IF(POTENT.EQ.P_STW) THEN
	CALL BOND_ENERGY(STRUCT,STRUCT%NATOMS,STW_I,L_SS,L_SG,L_GG)
	ELSE
      CALL BOND_ENERGY(STRUCT,STRUCT%NATOMS,TERSF_I,L_SS,L_SG,L_GG)
	END IF		
	

	SS=0.0
	WRITE(3,*)'BOND SI-SI'
	DO I=1,SIZE(L_SS)
	SS=SS+L_SS(I)
	WRITE(3,*) L_SS(I)	 
	END DO

	IF(SIZE(L_SS).GT.0) WRITE(*,*) 'BOND ENERGY SI-SI',SS/SIZE(L_SS)

	SG=0.0
	WRITE(3,*)'BOND SI-GE'
	DO I=1,SIZE(L_SG)
	SG=SG+L_SG(I)
	WRITE(3,*) L_SG(I) 
	END DO

	IF(SIZE(L_SG).GT.0) WRITE(*,*) 'BOND ENERGY SI-GE',SG/SIZE(L_SG)

	GG=0.0
	WRITE(3,*)'BOND GE-GE'
	DO I=1,SIZE(L_GG)
	GG=GG+L_GG(I)
	WRITE(3,*) L_GG(I) 
	END DO

	IF(SIZE(L_GG).GT.0) WRITE(*,*) 'BOND ENERGY GE-GE',GG/SIZE(L_GG)

	END IF


     	
	!------------------------------------------------------------------------------------------------
	!--------------ондявер яоейрпнб-------------------------------------------------------------------
	WRITE(*,*) 'COUNT SPECTRES? 0-NO,1-YES'
	READ(*,*) COUNT_SPECTRES

	IF(COUNT_SPECTRES.NE.0) THEN

	CALL SET_ENVIRS(STRUCT,2.0*PLT*SQRT(3.0)/4.0)
	
      
      !сярюмюбкхбюел пюявермсч ъвеийс, еякх йсахвеяйюъ ярпсйрспю мехдеюкэмю
	WRITE(*,*) 'COUNT NEW  CELL? 0-NO,1-YES'
	READ(*,*) COUNT_NEW_CELL

	IF(COUNT_NEW_CELL.NE.0) THEN
 	IF(STRUCT%FORM.EQ.CUB.AND.STRUCT%BNDS.EQ.PRDC)	THEN

	IF(NUM_UNITS.GT.0) NUM_CELL=NUM_UNITS  
	CALL FIND_CELL(STRUCT,PLT,NUM_CELL,0.0)

	END IF
	END IF 

      CALL GET_TSF_DINAM_MATRIX(STRUCT,MATR)	 

	DO I=1,3*STRUCT%NATOMS
	DO J=1,3*STRUCT%NATOMS
	IF(ABS(MATR(I,J)).GT.1E-3) THEN
	WRITE(5,*) I,J,MATR(I,J),MATR(J,I)
	IF(ABS((MATR(I,J)-MATR(J,I))/MATR(I,J)).GT.5E-2) WRITE(*,*) I,J 
	END IF
	END DO
	END DO

	WRITE(*,*) 'DINAM COUNTED'

	CALL TRANSFORM(MATR,EIG_MATR,EIG_VALS)

	DO I=1,SIZE(EIG_VALS)
	WRITE(6,*) I,EIG_VALS(I)
	END DO 

	WRITE(*,*) 'TRANSFORM ENDED'

	DW=0.002
	CALL GET_SPECTRES(STRUCT,EIG_MATR,EIG_VALS,1000,DW,PH_SP,R_SP,
	1ALPH,LRN,GSN)


	DO I=1,SIZE(PH_SP%W)
	WRITE(7,*) PH_SP%W(I),PH_SP%SP(I),PH_SP%SPR(I)
	END DO

	DO I=1,SIZE(R_SP%W)
	WRITE(8,*) R_SP%W(I),R_SP%SP(I),R_SP%SPR(I)	!R_SP%SP(I) ,
	END DO

	WRITE(*,*) 'SPECTRES FOUNDED'

	END IF
	!------------------------------------------------------------------------------------------------
	
      CLOSE(UNIT=2)
	CLOSE(UNIT=3)
	CLOSE(UNIT=4)
	CLOSE(UNIT=5)
	CLOSE(UNIT=6)
	CLOSE(UNIT=7)
	CLOSE(UNIT=8)  

     
      END	 

