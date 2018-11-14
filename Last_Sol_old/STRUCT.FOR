	MODULE MDSTRUCT

	USE MDTYPES
	USE MDCONSTS 	
	USE MDBOUNDS


	PUBLIC::CREATE_STRUCT
	PRIVATE::SYSCCOR
	PRIVATE::SET_TYPES,ORD_STRUCT,CLASTER_STRUCT
	PUBLIC::AMORF
	PUBLIC::FIND_BONDS_LENGTHS
	PRIVATE::SET_NEIBRS
	PRIVATE::SET_NUMS
	PUBLIC::SET_ENVIRS
	PRIVATE::SET_LAYS,SET_LEVELS
	PUBLIC::GET_BOND_TYPE	
      PUBLIC::GET_ANGLE_TYPE
	PUBLIC::SET_CELL,FIND_CELL
	PUBLIC::BOND_ANGLES
	PRIVATE::ELLIPSE_STRUCT,CUBIC_STRUCT	  


	!----------------TYPES-------------------------------------------
	TYPE T_ANGLES
	REAL*8,POINTER,DIMENSION(:)::SSS,GGG,SGS,GSG,GSS,SGG
	END TYPE

	TYPE T_NUM
	PRIVATE
	INTEGER*4 I,J,K
	END TYPE            

	CONTAINS

	!--------------------------------------------------------------------------------------------------------------------------
	!йнмярпсйрнп дкъ йпхярюккю

	TYPE(T_STRUCT) FUNCTION CREATE_STRUCT
	1(NUM,FORM,DISTR,TYP1,TYP2,PLT,X,BOUNDS,NUM_CLR)

	INTEGER*4 NUM			!вхякн якнеб хкх ъвеей
	INTEGER*2 FORM			!тнплю(йса хкх щккхоянхд)
	REAL*8 PLT				!оюпюлерп пеьерйх
	REAL*8 X				!днкъ цеплюмхъ
	INTEGER*2 DISTR         !рхо пюяопедекемхъ юрнлнб б ярпсйрспе
	INTEGER*2 TYP1,TYP2     !рхош юрнлнб  
	INTEGER*2 BOUNDS 		!цпюмхвмше сякнбхъ
	INTEGER*4 NUM_CLR       !йнкхвеярбн юрнлнб б йкюярепе 	

	TYPE(T_STRUCT) STRUCT
	REAL*8 AX

	IF(DISTR.EQ.ORD) PLT=(PLT_SI+PLT_GE)*0.5

	IF(DISTR.EQ.CLR) THEN
	NBUILD=GET_NUMS(FORM,NUM,NATOMS)
	AX=DBLE(NUM_CLR)/DBLE(NBUILD)
	PLT=PLT0(TYP2)*(1.0-AX)+PLT0(TYP1)*AX
	WRITE(*,*) 'PLT=',PLT
	END IF

	!щккхоянхд
	IF(FORM.EQ.ELL) THEN

	STRUCT%FORM=FORM
	CALL ELLIPSE_STRUCT(STRUCT,NUM,PLT)	
	CALL SET_LAYS(STRUCT)
	STRUCT%BNDS=RGD
	CALL SYSCOOR(STRUCT,PLT)
	IF(DISTR.EQ.ORD) CALL ORD_STRUCT(STRUCT,TYP1,TYP2)

      END IF
		
	!йса
	IF(FORM.EQ.CUB) THEN

	STRUCT%FORM=FORM
	STRUCT%BNDS=BOUNDS
	CALL CUBIC_STRUCT(STRUCT,NUM,PLT)
	CALL SET_NEIBRS(STRUCT,PLT)
	CALL SET_LAYS(STRUCT)
	CALL SET_LEVELS(STRUCT)

	IF(DISTR.EQ.ORD)   THEN
	CALL SET_NEIBRS(STRUCT,PLT)
	CALL ORD_STRUCT(STRUCT,TYP1,TYP2)
	END IF

	CALL SET_CELL(STRUCT,PLT,NUM,0.0)
	CALL SET_NEIBRS(STRUCT,PLT) 

	END IF

	IF(DISTR.EQ.RND) CALL SET_TYPES(STRUCT,X,TYP1,TYP2)	
	IF(DISTR.EQ.CLR) CALL CLASTER_STRUCT(STRUCT,TYP1,TYP2,NUM_CLR)

	CALL SET_MASSES(STRUCT)	 

	DO I=1,SIZE(STRUCT%ATS)
	STRUCT%ATS(I)%IS=.TRUE.
	END DO
	
	CREATE_STRUCT=STRUCT

	CONTAINS

	!-----------------------------------------------------------------------------
	SUBROUTINE SET_MASSES(STRUCT)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT

	INTEGER*2 TYP

	DO I=1,SIZE(STRUCT%ATS)

	TYP=STRUCT%ATS(I)%TYP
	IF(TYP.EQ.0) THEN
	FTY=456.00
	END IF
	STRUCT%ATS(I)%M=MAS(TYP)

	END DO

	END SUBROUTINE


	END FUNCTION

	!-----------------------------------------------------------------------------------------------------------------------------
	!лндекхпнбюмхе щккхорхвеяйнцн йпхярюккю я гюдюммшл вхякнл якнеб х оюпюлерпнл пеьерйх 
	SUBROUTINE ELLIPSE_STRUCT(STRUCT,NUM_LAYS,PLT) 
	

	INTEGER*4 NUM_LAYS
	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	REAL*8 PLT

	TYPE(T_ATOM) TA,TT
	INTEGER K,M
	REAL COSDI(3),SINDI(3)
	DIMENSION NBOND(20),DIED(3)
	INTEGER NSOS(4)
      TYPE(T_COORD) CRDNEW(3)
	INTEGER*4 NUM
	REAL*8 RR0
	INTEGER*4 NCRITR
	REAL*8 CSF(3,3)
	TYPE(T_COORD) CRD(3),DOPR(3)

	
	RR0=PLT*SQRT(3.0)/4.0
     	
	NMAX = NUM_LAYS		
      
	N1=1
      NP=1
      NBUILD=1
      N=0	
	DZ=RR0*SQRT(1./3)
      DX=RR0*SQRT(2./3)
      DY=DX   
     	NCRITR=1
     	
      !--онярпнемхе 1-нцн якнъ--
	TA%X=0.0
	TA%Y=0.0
	TA%Z=0.0
	TA%LEV=0
	TA%LAY=IN
	NUM=ADD(STRUCT%ATS,TA)

	DO 21 IB=1,4
      
	IF(IB.EQ.4) THEN		
      TT%X=TA%X
      TT%Y=TA%Y-DY
      TT%Z=TA%Z+DZ
	TT%LEV=1
	TT%LAY=IN
	NUM=ADD(STRUCT%ATS,TT)
      END IF

	IF(IB.EQ.1) THEN
      TT%X=TA%X-DX
      TT%Y=TA%Y
      TT%Z=TA%Z-DZ
	TT%LEV=1
	TT%LAY=IN
	NUM=ADD(STRUCT%ATS,TT)
      END IF

	IF(IB.EQ.2) THEN
      TT%X=TA%X
      TT%Y=TA%Y+DY
      TT%Z=TA%Z+DZ
	TT%LEV=1
	TT%LAY=IN
	NUM=ADD(STRUCT%ATS,TT)
      END IF

	IF(IB.EQ.3) THEN 	
      TT%X=TA%X+DX
      TT%Y=TA%Y
      TT%Z=TA%Z-DZ
	TT%LEV=1
	TT%LAY=IN
	NUM=ADD(STRUCT%ATS,TT)
	END IF	

      M=NBUILD

      DO 19 K=1,M

	REY=GETR(STRUCT%ATS(NBUILD+1),STRUCT%ATS(K))**2 

    !  REY=(ATOMS(NBUILD+1)%X-ATOMS(K)%X)**2+(ATOMS(NBUILD+1)%Y
	!1-ATOMS(K)%Y)**2
      !REY=(ATOMS(NBUILD+1)%Z-ATOMS(K)%Z)**2+REY

      IF(REY.LT.3E-6) THEN 
	STRUCT%ATS(NBUILD+1)%NEIBR(IB)=K 
     	END IF 
	 
  19  CONTINUE 

      NBUILD=NBUILD+1
	STRUCT%ATS(1)%NEIBR(IB)=NBUILD 
	
  21  CONTINUE 

  
      !---------------
	N=N+1
      NBOND(N)=NBUILD

	STRUCT%ATS(2)%NEIBR(3)=1
	STRUCT%ATS(3)%NEIBR(4)=1
	STRUCT%ATS(4)%NEIBR(1)=1
	STRUCT%ATS(5)%NEIBR(2)=1	           
      
      DIED(1)=PI/3
      DIED(2)=PI
      DIED(3)=DIED(1)*5.0
      ZPR=RR0*(1-COSC)
      DLINA=RR0*SQRT(1.-COSC*COSC)

      DO 40 KK=1,3
      COSDI(KK)=COS(DIED(KK))
      SINDI(KK)=SIN(DIED(KK))
  40  CONTINUE

	!--онярпнемхе 2-нцн якнъ--	
	NACH=2

      DO 500 I=2,NMAX

        NPOSL=NBOND(I-1)
        !--наунд он якнч (I-1): ГЮ АЮГНБШЕ АЕПСРЯЪ ЮРНЛШ ХГ (I-1)ЯКНЪ --
        DO 490 J=NACH,NPOSL

          !-- оНКСВЕМХЕ ЙННПДХМЮР ЮРНЛЮ, ОЮПМНЦН J-НЛС ЮРНЛС --
          CRD(2)%X=STRUCT%ATS(J)%X
          CRD(2)%Y=STRUCT%ATS(J)%Y
          CRD(2)%Z=STRUCT%ATS(J)%Z

          !-- оНХЯЙ МНЛЕПЮ НОНПМНЦН ЮРНЛЮ  --
          NSOS(1)=STRUCT%ATS(J)%NEIBR(1)
          NSOS(2)=STRUCT%ATS(J)%NEIBR(2)
          NSOS(3)=STRUCT%ATS(J)%NEIBR(3)
          NSOS(4)=STRUCT%ATS(J)%NEIBR(4)

          DO 300 L=1,4
            IF(NSOS(L).NE.0) THEN
              NOPOR=NSOS(L)
	        EXIT !GOTO 305
	      END IF            
 300      CONTINUE

        IF(NOPOR.GE.NACH) THEN
           write(*,*)'NOPOR=',nopor
           STOP
	    END IF

          CRD(1)%X=STRUCT%ATS(NOPOR)%X
          CRD(1)%Y=STRUCT%ATS(NOPOR)%Y
          CRD(1)%Z=STRUCT%ATS(NOPOR)%Z

          !-- оНХЯЙ МНЛЕПЮ ОЕПБНЦН ЯНЯЕДЮ NOPOR Х ЕЦН ЙННПДХМЮР; МН ОЕПБЮЪ --
          !-- МЕ ЯНБОЮДЮЕР Я J МНЛЕПНЛ --
          NSOS(1)=STRUCT%ATS(NOPOR)%NEIBR(1)
          NSOS(2)=STRUCT%ATS(NOPOR)%NEIBR(2)
          NSOS(3)=STRUCT%ATS(NOPOR)%NEIBR(3)
          NSOS(4)=STRUCT%ATS(NOPOR)%NEIBR(4)

          M1=0

          DO 320 M=1,4
           IF(NSOS(M).NE.J) THEN
             M1=M1+1
             M2=NSOS(M)

	       if(m1.gt.3) then

	         m1=m1+1
	         m1=m1-1

	       end if

             DOPR(M1)%X=STRUCT%ATS(M2)%X
             DOPR(M1)%Y=STRUCT%ATS(M2)%Y
             DOPR(M1)%Z=STRUCT%ATS(M2)%Z
	      END IF
 320      CONTINUE

          !-- оНХЯЙ РПЕУ ЙННПДХМЮР ЯНЯЕДЕИ PAR --
          CRD(3)%X=DOPR(1)%X
          CRD(3)%Y=DOPR(1)%Y
          CRD(3)%Z=DOPR(1)%Z

          CALL UGLXYZ(CRD,DOPR,CSF,NCRITR)
			         
		!IF(I.GT.NMAX-2) THEN
		!  ZPR=R0SI*(1-COSKOV)
		!  DLINA=R0SI*SQRT(1.-COSKOV*COSKOV)
		!END IF

          DO 340 N=1,3
            XPR=DLINA*COSDI(N)
            YPR=DLINA*SINDI(N)
            CRDNEW(N)%X=XPR*CSF(1,1)+YPR*CSF(2,1)+ZPR*CSF(3,1)+CRD(1)%X
            CRDNEW(N)%Y=XPR*CSF(1,2)+YPR*CSF(2,2)+ZPR*CSF(3,2)+CRD(1)%Y
            CRDNEW(N)%Z=XPR*CSF(1,3)+YPR*CSF(2,3)+ZPR*CSF(3,3)+CRD(1)%Z
 340      CONTINUE

	    !--------------------------------------------------------
          DO 380 L=1,3

            ISITUA=0

            DO 360 NPOSTR=1,NBUILD

	        ! RASTKV=GETR(CRD_ATOM(CRDNEW(L)),STRUCT%ATS(NPOSTR))**2
			RASTKV=GETR(STRUCT%ATS(NPOSTR),CRDNEW(L))**2

              !RASTKV=(XNOV(L)-ATOMS(NPOSTR)%X)**2+(YNOV(L)
     P		!-ATOMS(NPOSTR)%Y)**2 + (ZNOV(L)-ATOMS(NPOSTR)%Z)**2

              IF(RASTKV.LE.1.0E-03) THEN !GOTO 360

               !-- оНЯРП. ЮРНЛ АШК ЯНГДЮМ ПЮМЕЕ ОНД МНЛЕПНЛ NPOSTR --
               ISITUA=ISITUA+1

               IF(STRUCT%ATS(J)%NEIBR(1).EQ.NPOSTR.OR.
     P		 STRUCT%ATS(J)%NEIBR(2).EQ.NPOSTR.OR.
     P         STRUCT%ATS(J)%NEIBR(3).EQ.NPOSTR.OR.
     P         STRUCT%ATS(J)%NEIBR(4).EQ.NPOSTR)
     P                                 GOTO 360
               !-- гЮЯШКЙЮ NPOSTR Б ОЕПБСЧ ОСЯРСЧ ЪВЕИЙС NEIBR.--
	         CALL REINIT(STRUCT%ATS,J)


               IF(STRUCT%ATS(J)%NEIBR(1).EQ.0) THEN
			 			  
			    STRUCT%ATS(J)%NEIBR(1)=NPOSTR
               ELSE
			 IF(STRUCT%ATS(J)%NEIBR(2).EQ.0) THEN

				  STRUCT%ATS(J)%NEIBR(2)=NPOSTR
               ELSE				 
			 IF(STRUCT%ATS(J)%NEIBR(3).EQ.0) THEN

				     STRUCT%ATS(J)%NEIBR(3)=NPOSTR 
               ELSE 
			 IF(STRUCT%ATS(J)%NEIBR(4).EQ.0) THEN
			 					 
					    STRUCT%ATS(J)%NEIBR(4)=NPOSTR
	         ELSE					   
					    EXIT
	         END IF
	         END IF 
	         END IF 
	         END IF
             
			   !-- гЮЯШКЙЮ NPAR(=J) Б ОЕПБШИ ОСЯРНИ NEIBR.(NROSTR) --
			   IF(STRUCT%ATS(NPOSTR)%NEIBR(1).EQ.0)  THEN
		    				  
				  STRUCT%ATS(NPOSTR)%NEIBR(1)=J
                
			   ELSE 
			   IF(STRUCT%ATS(NPOSTR)%NEIBR(2).EQ.0)  THEN
                
					STRUCT%ATS(NPOSTR)%NEIBR(2)=J
               
			   ELSE
			   IF(STRUCT%ATS(NPOSTR)%NEIBR(3).EQ.0)  THEN
  
						STRUCT%ATS(NPOSTR)%NEIBR(3)=J
                
			   ELSE
			   IF(STRUCT%ATS(NPOSTR)%NEIBR(4).EQ.0)  THEN
             
							STRUCT%ATS(NPOSTR)%NEIBR(4)=J
			   ELSE
								EXIT 
                
			   END IF
			   END IF
			   END IF
			   END IF


            END IF
		    
 360        CONTINUE


            IF(ISITUA.EQ.0) THEN

              NBUILD=NBUILD+1
	        CALL REINIT(STRUCT%ATS,NBUILD) 
              STRUCT%ATS(NBUILD)%NEIBR(1)=J

			!--гюонкмемхе нямнбмшу оюпюлерпнб юрнлю--
              STRUCT%ATS(NBUILD)%X=CRDNEW(L)%X
              STRUCT%ATS(NBUILD)%Y=CRDNEW(L)%Y
              STRUCT%ATS(NBUILD)%Z=CRDNEW(L)%Z
	        STRUCT%ATS(NBUILD)%LEV=I 			

	        IF(STRUCT%ATS(J)%NEIBR(1).EQ.0) THEN
						  
			    STRUCT%ATS(J)%NEIBR(1)=NBUILD

              ELSE
			IF(STRUCT%ATS(J)%NEIBR(2).EQ.0) THEN

				  STRUCT%ATS(J)%NEIBR(2)=NBUILD
              ELSE				 
			IF(STRUCT%ATS(J)%NEIBR(3).EQ.0) THEN

				     STRUCT%ATS(J)%NEIBR(3)=NBUILD 
              ELSE 
			IF(STRUCT%ATS(J)%NEIBR(4).EQ.0) THEN
								 
					    STRUCT%ATS(J)%NEIBR(4)=NBUILD
	        ELSE					   
					    EXIT
	        END IF
	        END IF 
	        END IF 
	        END IF
              

      	  END IF
 380      CONTINUE
 490    CONTINUE	
        !-- оНЯРПНЕМШ БЯЕ ЮРНЛШ МНБНЦН ЯКНЪ, ОЕПЕАПЮМШ БЯЕ ЮРНЛШ ОПЕДШДСЫЕЦН --
        NBOND(I)=NBUILD
        NACH=NBOND(I-1)+1

 500  CONTINUE  

     	STRUCT%NATOMS=NBOND(NMAX-2)	

	WRITE(*,*) 'ELLIPSE CRISTALL BUILT'

	CONTAINS

	!--------------------------------------------------------------
	RECURSIVE SUBROUTINE  UGLXYZ(CRD,DOPR,CSF,NCRITR)

	TYPE(T_COORD),INTENT(INOUT)::CRD(3),DOPR(3)
	REAL*8,INTENT(INOUT)::CSF(3,3)
	INTEGER*4,INTENT(INOUT)::NCRITR

	REAL R,A,B,C
	   
	R=(CRD(2)%X-CRD(1)%X)*(CRD(2)%X-CRD(1)%X)
	R=R+(CRD(2)%Y-CRD(1)%Y)*(CRD(2)%Y-CRD(1)%Y)
	R=R+(CRD(2)%Z-CRD(1)%Z)*(CRD(2)%Z-CRD(1)%Z)
	R=SQRT(R)

	CSF(3,1)=(CRD(2)%X-CRD(1)%X)/R;
	CSF(3,2)=(CRD(2)%Y-CRD(1)%Y)/R;
	CSF(3,3)=(CRD(2)%Z-CRD(1)%Z)/R;   

	A=(CRD(2)%Y-CRD(1)%Y)*(CRD(3)%Z-CRD(1)%Z)-(CRD(2)%Z-CRD(1)%Z)*
     1(CRD(3)%Y-CRD(1)%Y)
	B=(CRD(2)%Z-CRD(1)%Z)*(CRD(3)%X-CRD(1)%X)-(CRD(2)%X-CRD(1)%X)*
	2(CRD(3)%Z-CRD(1)%Z)
	C=(CRD(2)%X-CRD(1)%X)*(CRD(3)%Y-CRD(1)%Y)-(CRD(2)%Y-CRD(1)%Y)*
	3(CRD(3)%X-CRD(1)%X)

	R=SQRT(A*A+B*B+C*C)
	IF(R.GT.0.0001) THEN

	CSF(2,1)=A/R
      CSF(2,2)=B/R
      CSF(2,3)=C/R 

	CSF(1,1)=CSF(2,2)*CSF(3,3)-CSF(2,3)*CSF(3,2)
      CSF(1,2)=CSF(2,3)*CSF(3,1)-CSF(2,1)*CSF(3,3)
      CSF(1,3)=CSF(2,1)*CSF(3,2)-CSF(2,2)*CSF(3,1)

	ELSE 

	NCRITR=NCRITR+1
	CRD(3)%X=DOPR(NCRITR)%X
      CRD(3)%Y=DOPR(NCRITR)%Y
      CRD(3)%Z=DOPR(NCRITR)%Z

	CALL UGLXYZ(CRD,DOPR,CSF,NCRITR)

	END IF 
      
	END	 SUBROUTINE  UGLXYZ	


      END SUBROUTINE 	

	!--------------------------------------------------------------------------------------------------------------------------
	!лндекхпнбюмхе йсахвеяйнцн йпхярюккю я гюдюммшл оюпюлерпнл пеьерйх
	!йнкхвеярбн юрнлнб = 8*NUM^3

	SUBROUTINE CUBIC_STRUCT(STRUCT,NUM,PLT)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	REAL*8 PLT     
	INTEGER*4 NUM  	
	
	INTEGER*4 NA
	

	ALLOCATE(STRUCT%ATS(8*NUM**3))
	ALLOCATE(STRUCT%NUMS(8*NUM**3))		

	!мнлепю юрнлнб IN ярюбхл б яннрберярбхх я бнгпюярюмхел йннпдхмюр Y х Z
	
	DO K=1,NUM
	DO J=1,NUM
	DO I=1,NUM 

	IF(I.EQ.1.AND.J.EQ.1.AND.K.EQ.1) THEN

	!ярпнхл 1-сч ъвеийс

	!нямнбмюъ цжй-пеьерйю
	STRUCT%ATS(1)=CRD_ATOM(0.0,0.0,0.0)
	STRUCT%NUMS(1)%IN=1	
	STRUCT%ATS(2)=CRD_ATOM(0.0*PLT,0.5*PLT,0.5*PLT)
	STRUCT%NUMS(2)%IN=1+4*NUM**2+NUM
	STRUCT%ATS(3)=CRD_ATOM(0.5*PLT,0.0*PLT,0.5*PLT)
	STRUCT%NUMS(3)%IN=1+4*NUM**2
	STRUCT%ATS(4)=CRD_ATOM(0.5*PLT,0.5*PLT,0.0*PLT)
	STRUCT%NUMS(4)%IN=1+NUM
	
	!ядбхмсрюъ цжй-пеьерйю
	STRUCT%ATS(5)=CRD_ATOM(0.25*PLT,0.25*PLT,0.25*PLT)
	STRUCT%NUMS(5)%IN=1+2*NUM**2
	STRUCT%ATS(6)=CRD_ATOM(0.25*PLT,0.75*PLT,0.75*PLT)
	STRUCT%NUMS(6)%IN=1+6*NUM**2+NUM
	STRUCT%ATS(7)=CRD_ATOM(0.75*PLT,0.25*PLT,0.75*PLT)
	STRUCT%NUMS(7)%IN=1+6*NUM**2
      STRUCT%ATS(8)=CRD_ATOM(0.75*PLT,0.75*PLT,0.25*PLT)
	STRUCT%NUMS(8)%IN=1+2*NUM**2+NUM

	NA=8
		 
	ELSE

	NA=NA+8

	DO N=1,8

	STRUCT%ATS(NA-8+N)=CRD_ATOM(STRUCT%ATS(N)%X+PLT*(I-1),
	1STRUCT%ATS(N)%Y+PLT*(J-1),STRUCT%ATS(N)%Z+PLT*(K-1)) 
     
      STRUCT%NUMS(NA-8+N)%IN=	
	1STRUCT%NUMS(N)%IN+8*NUM**2*(K-1)+2*NUM*(J-1)+(I-1)       	

	END DO  
		 
	END IF			

	END DO
	END DO
	END DO	
    

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	!сярюмнбйю пюявермни ъвеийх
	SUBROUTINE SET_CELL(STRUCT,PLT,NUM_CELLS,SHIFT)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	REAL*8 PLT
	INTEGER*4 NUM_CELLS
	REAL SHIFT
	
      REAL*8 NS
	NS=(NUM_CELLS+SHIFT*2)	

	STRUCT%CELL=T_COORD(PLT*NS,PLT*NS,PLT*NS)

	DO I=1,SIZE(STRUCT%ATS)

	STRUCT%ATS(I)%X=STRUCT%ATS(I)%X+SHIFT*PLT
	STRUCT%ATS(I)%Y=STRUCT%ATS(I)%Y+SHIFT*PLT
	STRUCT%ATS(I)%Z=STRUCT%ATS(I)%Z+SHIFT*PLT

	END DO
	

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	!мюунфдемхе пюявермни ъвеий, менаундхлни дкъ бшвхякемхъ дхмюлхвеяйни люрпхжш 
	SUBROUTINE FIND_CELL(STRUCT,PLT,NUM_CELLS,SHIFT)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	REAL*8 PLT
	REAL SHIFT
	INTEGER*4 NUM_CELLS	
    
	INTEGER*4 NCX,NCY,NCZ      	
	
      TYPE(T_COORD) CRD_MIN,CRD_MAX,CRD	
	
      CRD_MIN=T_COORD(STRUCT%ATS(1)%X,STRUCT%ATS(1)%Y,STRUCT%ATS(1)%Z)     	
	CRD_MAX=T_COORD(STRUCT%ATS(1)%X,STRUCT%ATS(1)%Y,STRUCT%ATS(1)%Z)

	!мюундхл кебши мхфмхи х опюбши бепумхи сцнк ъвеийх
	DO I=2,SIZE(STRUCT%ATS)

	CRD=T_COORD(STRUCT%ATS(I)%X,STRUCT%ATS(I)%Y,STRUCT%ATS(I)%Z)
	
	CRD_MAX%X=MAX(CRD%X,CRD_MAX%X)
	CRD_MAX%Y=MAX(CRD%Y,CRD_MAX%Y)
	CRD_MAX%Z=MAX(CRD%X,CRD_MAX%Z) 

	CRD_MIN%X=MIN(CRD%X,CRD_MIN%X)
	CRD_MIN%Y=MIN(CRD%Y,CRD_MIN%Y)
	CRD_MIN%Z=MIN(CRD%X,CRD_MIN%Z)     

	END DO
	
	!сярюмюбкхбюел ъвеийс
	

	STRUCT%CELL%X=CRD_MAX%X-CRD_MIN%X+2.0*SHIFT*PLT
	STRUCT%CELL%Y=CRD_MAX%Y-CRD_MIN%Y+2.0*SHIFT*PLT
	STRUCT%CELL%Z=CRD_MAX%Z-CRD_MIN%Z+2.0*SHIFT*PLT
	
	IF(ABS(SHIFT).LT.1E-3) THEN

	NCX=INT(STRUCT%CELL%X/PLT)
	NCY=INT(STRUCT%CELL%Y/PLT)
	NCZ=INT(STRUCT%CELL%Z/PLT)

	STRUCT%CELL%X=PLT*(NCX+1)
	STRUCT%CELL%Y=PLT*(NCY+1)
	STRUCT%CELL%Z=PLT*(NCZ+1)


	END IF 

	!ялеыюел юрнлш мю CRD_MIN
	DO I=1,SIZE(STRUCT%ATS)

	STRUCT%ATS(I)%X=STRUCT%ATS(I)%X-CRD_MIN%X+SHIFT*PLT
	STRUCT%ATS(I)%Y=STRUCT%ATS(I)%Y-CRD_MIN%Y+SHIFT*PLT
	STRUCT%ATS(I)%Z=STRUCT%ATS(I)%Z-CRD_MIN%Z+SHIFT*PLT

      END DO


	END SUBROUTINE 

	!--------------------------------------------------------------------------------------------------------------------------
	!дюммюъ тсмйжхъ опнхгбндхр онхяй акхфюиьху дкъ йюфднцн юрнлю яняедеи 	
	!я свернл цпюмхвмшу сякнбхи
	SUBROUTINE SET_NEIBRS(STRUCT,PLT)
	
	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	REAL*8 PLT				!оюпюлерп пеьерйх

	
      REAL*8 D,R
	D=PLT*SQRT(3.0)/4.0


	DO_I:DO I=1,SIZE(STRUCT%ATS)	

	DO J=1,4
	STRUCT%ATS(I)%NEIBR(J)=0
	END DO

	N=0

	DO_J:DO J=1,SIZE(STRUCT%ATS)

	IF(I.NE.J) THEN

	R=GETR(I,J,STRUCT) 

	IF(ABS(R-D).LT.0.01.OR.R.LE.D) THEN
	N=N+1
	STRUCT%ATS(I)%NEIBR(N)=J	 
      END IF
	
      IF(N.EQ.4) EXIT
	 
	END IF 

	END DO  DO_J 

	END DO	DO_I 
	
	 
      END SUBROUTINE	  
		
	!-------------------------------------------------------------------------------------------------------------------------
	!мюундхр 50 юрнлнб хг нйпсфемхъ мю пюяярнъмхх R0
	SUBROUTINE SET_ENVIRS(STRUCT,R0)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	REAL*8 R0 

	REAL*8	R
	INTEGER*4 N
	

	DO I=1,SIZE(STRUCT%ATS)

	ALLOCATE(STRUCT%ATS(I)%ENVRS(0))

	DO J=1,SIZE(STRUCT%ATS)

	IF(I.NE.J) THEN

	R=GETR(I,J,STRUCT)	

	IF(R.LT.R0.OR.ABS(R-R0).LE.0.001) N=ADD(STRUCT%ATS(I)%ENVRS,J)

	END IF	

	IF(SIZE(STRUCT%ATS(I)%ENVRS).GE.50) EXIT

	END DO
	END DO

	END SUBROUTINE
	!-------------------------------------------------------------------------------------------------------------------------
	!гюдюер рхош юрнлнб б ярпсйрспе яксвюимшл напюгнл
	SUBROUTINE SET_TYPES(STRUCT,X,TYP1,TYP2)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	REAL*8 X	!днкъ цеплюмхъ
	INTEGER*2 TYP1,TYP2

	INTEGER*4,POINTER,DIMENSION(:)::NUMS
	INTEGER*4 N1,N2
	INTEGER*4 NUM

	N1=INT(X*SIZE(STRUCT%ATS))
	N2=SIZE(STRUCT%ATS)-N1
	ALLOCATE(NUMS(N1))
	
	WRITE(*,*) 'N1=',N1
	WRITE(*,*) 'N2=',N2		

	!гюонкмемхе юрнлнб GE

	I=0
	
	write(*,*) 'zatravka dlja GSCh'
	read(*,*) ztr 
      CALL SEED(ztr)
      CALL RANDOM(ran)

!	write(*,*) ran
	
	DO WHILE(I<N1)

	CALL RANDOM(ran)
!	NUM=INT(SIZE(STRUCT%ATS)*RAND())+1	   ! НДМЮ ЯКСВЮИМЮЪ ОНЯКЕД-РЭ
      NUM=INT(SIZE(STRUCT%ATS)*ran)+1		   ! ПЮГМШЕ ОНЯКЕДНБЮРЕКЭМНЯРХ ВХЯЕК

!	write(*,*) rand()
	write(*,*) ran

	IF(I.GT.0) THEN

	DO J=1,I
	IF(NUMS(J).EQ.NUM) EXIT
	END DO

	IF(J.EQ.I+1) THEN
	I=I+1
	NUMS(I)=NUM
	STRUCT%ATS(NUMS(I))%TYP=TYP1
	END IF

	ELSE

	I=I+1
	NUMS(1)=NUM
	STRUCT%ATS(NUMS(1))%TYP=TYP1

	END IF

	END DO

	!гюонкмемхе юрнлнб SI
	DO I=1,SIZE(STRUCT%ATS)

	IF(STRUCT%ATS(I)%TYP.NE.TYP1) THEN

	STRUCT%ATS(I)%TYP=TYP2
	

	END IF

	END DO 

	DEALLOCATE(NUMS)
	WRITE(*,*) 'TYPES SET'

	END SUBROUTINE

	!-------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE ORD_STRUCT(STRUCT,TYP1,TYP2)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	INTEGER*2 TYP1,TYP2

	INTEGER*4 I,J,N,NN
	INTEGER*2 TA,TN
	INTEGER*4,POINTER,DIMENSION(:)::NUMS,NBS

	ALLOCATE(NUMS(0))
	ALLOCATE(NBS(0))

	DO I=1,SIZE(STRUCT%ATS)
	STRUCT%ATS(I)%TYP=0
	END DO

	!1-нлс юрнлс ярюбхл б яннрберярбхе рхо TYP1
	STRUCT%ATS(1)%TYP=TYP1	
	N=ADD(NUMS,1)

	!ецн яняедъл - рхо TYP2
	DO I=1,4

	IF(STRUCT%ATS(1)%NEIBR(I).GT.0) THEN

	N=ADD(NBS,STRUCT%ATS(1)%NEIBR(I))
	STRUCT%ATS(NBS(N))%TYP=TYP2	
	NN=ADD(NUMS,NBS(N))

	END IF

	END DO

	!--------------------------------------------------------------

	I=2	

	!гюонкмъел рхош юрнлнб, мювхмюъ ян брнпнцн мнлепю
	DO WHILE(SIZE(NUMS)<SIZE(STRUCT%ATS))		 !!!!

	ALLOCATE(NBS(0))
	TA=0
	TN=0
	
	!гюонкмъел люяяхб яняедеи юрнлю
	DO J=1,4
	IF(STRUCT%ATS(I)%NEIBR(J).GT.0) THEN	
	N=ADD(NBS,STRUCT%ATS(I)%NEIBR(J))	
	END IF
	END DO

	!-------------------------------------------
	
	IF0:IF(STRUCT%ATS(I)%TYP.NE.0) THEN

      !еякх юрнлс сфе онярюбкем б яннрберярбхе рхо, рн гюонкмъел ецн
	!яняедеи  опнрхбнонкнфмшлх рхоюлх

	IF(STRUCT%ATS(I)%TYP.EQ.TYP1) TN=TYP2
	IF(STRUCT%ATS(I)%TYP.EQ.TYP2) TN=TYP1
	
	DO J=1,SIZE(NBS)
	IF(STRUCT%ATS(NBS(J))%TYP.EQ.0) THEN
	STRUCT%ATS(NBS(J))%TYP=TN	
	N=ADD(NUMS,NBS(J))
	END IF
	END DO


	ELSE	   !ELSE0

	!еякх рхо юрнлс ме онярюбкем, рн опнбепъел бяеу ецн яняедеи		
	DO J=1,SIZE(NBS)
	IF(STRUCT%ATS(NBS(J))%TYP.NE.0) THEN

	!еякх с йюйнцн-рн хг яняедеи рхо юрнлю ме 0,рн гюонлхмюел ецн рхо
	IF(STRUCT%ATS(NBS(J))%TYP.EQ.TYP1) THEN

	TA=TYP2
	TN=TYP1
	EXIT

	ELSE

	TA=TYP1
	TN=TYP2
	EXIT

	END IF
		
	END IF
	END DO

	
      IF(TA.NE.0) THEN

	!ярюбхл б яннрберярбхе юрнлс х ецн яняедъл мюидеммше рхош
	STRUCT%ATS(I)%TYP=TA	
	N=ADD(NUMS,I)
	
      DO J=1,SIZE(NBS)
	IF(STRUCT%ATS(NBS(J))%TYP.EQ.0) THEN
	STRUCT%ATS(NBS(J))%TYP=TN	
	N=ADD(NUMS,NBS(J))
	END IF
	END DO 
     
      END IF 


	END IF IF0

	I=I+1
	IF(I.GT.SIZE(STRUCT%ATS)) I=2

	END DO
	
      DEALLOCATE(NBS)
	DEALLOCATE(NUMS) 

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE CLASTER_STRUCT(STRUCT,C_TYP,S_TYP,NUM_CLR)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	INTEGER*2 C_TYP   !рхо жемрпюкэмшу юрнлнб
	INTEGER*2 S_TYP   !рхо юрнлнб ондкнфйх
	INTEGER*4 NUM_CLR !вхякн юрнлнб б йкюярепе 

	INTEGER*4 I

	DO I=1,SIZE(STRUCT%ATS)

	IF(I.LE.NUM_CLR) THEN 
	STRUCT%ATS(I)%TYP=C_TYP
	ELSE
	STRUCT%ATS(I)%TYP=S_TYP
	END IF
	
	END DO

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	!гюдюер рхош якнеб б йпхярюкке
	SUBROUTINE SET_LAYS(STRUCT)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT

	INTEGER*4 IRES
	LOGICAL BRES
	INTEGER*4,POINTER,DIMENSION(:)::TEMP
	INTEGER*4 N1,N2,N3,N4

	ALLOCATE(STRUCT%INS(SIZE(STRUCT%ATS)))

	DO I=1,SIZE(STRUCT%INS)
	STRUCT%INS(I)=I
	STRUCT%ATS(I)%LAY=IN
	END DO

	!FIND OUTER ATOMS
	DO I=1,SIZE(STRUCT%ATS)

	N1=STRUCT%ATS(I)%NEIBR(1)
	N2=STRUCT%ATS(I)%NEIBR(2)
	N3=STRUCT%ATS(I)%NEIBR(3)
	N4=STRUCT%ATS(I)%NEIBR(4)

	IF(N1.EQ.0.OR.N2.EQ.0.OR.N3.EQ.0.OR.N4.EQ.0) THEN
     
	IRES=ADD(STRUCT%OUTS,I)
	STRUCT%ATS(I)%LAY=OUT
	BRES=REMOVE_OBJ(STRUCT%INS,I)
     
      END IF

	END DO


	!FIND FRONT ATOMS
	ALLOCATE(TEMP(SIZE(STRUCT%INS)))
	TEMP=STRUCT%INS

	DO I=1,SIZE(TEMP)
	DO J=1,SIZE(STRUCT%OUTS)

	IF(STRUCT%ATS(TEMP(I))%NEIBR(1).EQ.STRUCT%OUTS(J).OR
	1.STRUCT%ATS(TEMP(I))%NEIBR(2).EQ.STRUCT%OUTS(J).OR
	1.STRUCT%ATS(TEMP(I))%NEIBR(3).EQ.STRUCT%OUTS(J).OR
	1.STRUCT%ATS(TEMP(I))%NEIBR(4).EQ.STRUCT%OUTS(J)) THEN

	IRES=ADD(STRUCT%FRONTS,TEMP(I))
	STRUCT%ATS(TEMP(I))%LAY=FRONT
	BRES=REMOVE_OBJ(STRUCT%INS,TEMP(I))
	EXIT

	END IF

	END DO
	END DO

	IF(STRUCT%BNDS.EQ.RGD) STRUCT%NATOMS=SIZE(STRUCT%INS)
	IF(STRUCT%BNDS.EQ.PRDC) STRUCT%NATOMS=SIZE(STRUCT%ATS)

	END SUBROUTINE

	!-------------------------------------------------------------------------------------------------------------------------
	!гюдюер спнбмх б йсахвеяйнл йпхярюкке, гюрел опнмслепнбшбюер ху он бнгпюярюмхч спнбмъ
	SUBROUTINE SET_LEVELS(STRUCT)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT	

	INTEGER*4 L,NUM,NUM_LEVS
	INTEGER*4 N1,N2,N3,N4,NN
	TYPE(T_ATOM),POINTER,DIMENSION(:)::TEMP	
	INTEGER*4,POINTER,DIMENSION(:)::NUMS_IN,NUMS_INN

	ALLOCATE(NUMS_IN(SIZE(STRUCT%NUMS)))	
	ALLOCATE(NUMS_INN(SIZE(STRUCT%NUMS)))

	DO I=1,SIZE(NUMS_IN)
	NUMS_IN(I)=STRUCT%NUMS(I)%IN
	END DO	

	DO I=1,SIZE(STRUCT%ATS)
	STRUCT%ATS(I)%LEV=0
	IF(STRUCT%ATS(I)%LAY.EQ.OUT) STRUCT%ATS(I)%LEV=1
	IF(STRUCT%ATS(I)%LAY.EQ.FRONT) STRUCT%ATS(I)%LEV=2
	STRUCT%NUMS(I)%INN=0	
	END DO


	L=2
	DOWHILE:DO WHILE(1) 
	
	NUM=0

	DO I=1,SIZE(STRUCT%ATS)

	N1=STRUCT%ATS(I)%NEIBR(1)
	N2=STRUCT%ATS(I)%NEIBR(2)
	N3=STRUCT%ATS(I)%NEIBR(3)
	N4=STRUCT%ATS(I)%NEIBR(4)

	IF(STRUCT%ATS(I)%LEV.EQ.0.AND.(STRUCT%ATS(N1)%LEV.EQ.L.OR
	1.STRUCT%ATS(N2)%LEV.EQ.L.OR.STRUCT%ATS(N3)%LEV.EQ.L.OR	
	1.STRUCT%ATS(N4)%LEV.EQ.L)) THEN

	STRUCT%ATS(I)%LEV=L+1
	NUM=NUM+1

	END IF 

	END DO

	IF(NUM.EQ.0) EXIT
	
     	L=L+1	

	END DO	DOWHILE

	NUM_LEVS=L

	!оепемслепнбшбюел спнбмх
	DO I=1,SIZE(STRUCT%ATS)
	STRUCT%ATS(I)%LEV=NUM_LEVS+1-STRUCT%ATS(I)%LEV
	END DO

	!оепемслепнбшбюел юрнлш б яннрберярбхх я спнбмълх
	!ALLOCATE(TEMP(SIZE(STRUCT%ATS)))
	NUM=0

	DOWHILE1:DO WHILE(1)

	DO J=1,NUM_LEVS

	DO I=1,SIZE(STRUCT%ATS)
	IF(STRUCT%ATS(I)%LEV.EQ.J) THEN
	 NUM=ADD(TEMP,STRUCT%ATS(I))
	 NN=NUMS_IN(I)
	 STRUCT%NUMS(NUM)%IN=NN
	 NUMS_INN(NN)=NUM
	END IF
	END DO

	END DO

	IF(NUM.EQ.SIZE(STRUCT%ATS)) EXIT

	END DO DOWHILE1

	J=0
	DO I=1,SIZE(NUMS_INN)	
	NUM=NUMS_INN(I)
	IF(NUM.LE.STRUCT%NATOMS) THEN
	J=J+1
	STRUCT%NUMS(NUM)%INN=J
	END IF	 
	END DO

	STRUCT%ATS=TEMP

	
	DEALLOCATE(TEMP)
	DEALLOCATE(NUMS_IN) 
	DEALLOCATE(NUMS_INN)

	!FIND NEIBRS
	!CALL SET_NEIBRS(STRUCT,PLT)

	END SUBROUTINE
	!-------------------------------------------------------------------------------------------------------------------------
	!оепеунд й мнбни яхяреле йннпдхмюр
	SUBROUTINE SYSCOOR(STRUCT,PLT)	

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	REAL*8 PLT

	REAL*8 AD,BD,AD0,BD0,A0,B0,XX,YY
	REAL*8,PARAMETER::EPS0=0.01
	INTEGER*4 NMAX,NV

	NMAX=STRUCT%ATS(SIZE(STRUCT%ATS))%LEV

	AD=PLT*SQRT(3.0)/4.0*SQRT(1./3)
	BD=PLT*SQRT(3.0)/4.0*SQRT(1./3)	

	AD0=PLT*SQRT(3.0)/4.0*SQRT(2./3)
	BD0=PLT*SQRT(3.0)/4.0*SQRT(1./3)

	!-------
	A0=FLOAT(NMAX)*AD0
	B0=FLOAT(NMAX)*BD0
	NV=2*NMAX+1	 



	DO I=1,SIZE(STRUCT%ATS)
	
	XX=STRUCT%ATS(I)%X
	YY=STRUCT%ATS(I)%Y+A0

	X=STRUCT%ATS(I)%X
	Y=STRUCT%ATS(I)%Y
	Z=STRUCT%ATS(I)%Z
	 
	STRUCT%ATS(I)%X=(XX+YY)/SQRT(2.0)
	STRUCT%ATS(I)%Y=(YY-XX)/SQRT(2.0)
	STRUCT%ATS(I)%Z=STRUCT%ATS(I)%Z+B0

	END DO

	K=0


	ALLOCATE(STRUCT%NUMS(SIZE(STRUCT%ATS)))
	WRITE(*,*) 'NUMS ALLOCATED'


	!FIND IN
	DO NZ=1,NV
	DO NY=1,NV
	DO NX=1,NV	

	DO I=1,SIZE(STRUCT%ATS)	

	IF(ABS(STRUCT%ATS(I)%Z-(NZ-1)*BD).LE.EPS0.AND.
	1ABS(STRUCT%ATS(I)%Y-(NY-1)*AD).LE.EPS0.AND.
	1ABS(STRUCT%ATS(I)%X-(NX-1)*AD).LE.EPS0)  THEN

	K=K+1
	STRUCT%NUMS(K)%IN=I

	END IF

	END DO

	END DO
	END DO
	END DO

	WRITE(*,*) 'IN WERE FOUNDED'

	!FIND INN
	K=0

	DO NZ=3,NV
	DO NY=3,NV
	DO NX=3,NV	

	DO I=1,SIZE(STRUCT%ATS)

	IF(I.LE.STRUCT%NATOMS)	THEN

	IF(ABS(STRUCT%ATS(I)%Z-(NZ-1)*BD).LT.EPS0.AND.
	1ABS(STRUCT%ATS(I)%Y-(NY-1)*AD).LT.EPS0.AND.
	1ABS(STRUCT%ATS(I)%X-(NX-1)*AD).LT.EPS0)  THEN

	K=K+1
	STRUCT%NUMS(K)%INN=I

	END IF

	END IF

	END DO

	END DO
	END DO
	END DO

	WRITE(*,*) 'INN WERE FOUNDED'

	WRITE(*,*) 'SYSCOOR ENDED'

	END SUBROUTINE SYSCOOR

	!-------------------------------------------------------------------------------------------------------------------------
	!юлнптхгюжхъ йпхярюккю я гюдюммшлх оюпюлерпюлх
	SUBROUTINE AMORF(STRUCT,AMP)

	TYPE(T_STRUCT),INTENT(INOUT)::STRUCT
	TYPE(T_AMORF_PARAMS) AMP


	REAL*8 YFL
	INTEGER*4 IX,IY	
	REAL*8,POINTER,DIMENSION(:)::MSDV, MSDVCR	  
	INTEGER I_GRAN, I_MAX, IVAK,INUM,I_GRANCR
	REAL*8 PLT(2)
	INTEGER*4 N_MAX	
	
      ALLOCATE(MSDV(0))					!
      ALLOCATE(MSDVCR(0))						 !

	N_MAX=STRUCT%ATS(SIZE(STRUCT%ATS))%LEV
	NOST=N_MAX-AMP%NALIC-2

	PLT(1)=PLT_GE
	PLT(2)=PLT_SI 	

	I_MAX=SIZE(STRUCT%ATS)
	I_GRAN=STRUCT%NATOMS 
	I_GRANCR=0
	DO I=1,STRUCT%NATOMS
	IF(STRUCT%ATS(I)%LEV.LE.NOST) I_GRANCR=I_GRANCR+1 
	END DO
	
	IX=AMP%IX

	DO  I=1,100
      CALL DRANDU(IX,IY,YFL)
	IX=IY
	END DO

	!------------------------юлнптхгюжхъ цпюмхж-------------------------------
	IF(AMP%SDVIG.GT.0.0) THEN

	ALLOCATE(MSDV(SIZE(STRUCT%ATS)))

	DO I=1,SIZE(STRUCT%ATS)	
	MSDV(I)=PLT(STRUCT%ATS(I)%TYP)*AMP%SDVIG*SQRT(3.0)/8.0  !ДКЪ МЮМНЙПХЯРЮККЮ	
	END DO

	CALL DRANDU(IX,IY,YFL)
	STRUCT%ATS(I)%X=STRUCT%ATS(I)%X+MSDV(I)*(2.0*SNGL(YFL)-1.0)
	IX=IY

      CALL DRANDU(IX,IY,YFL)
	STRUCT%ATS(I)%Y= STRUCT%ATS(I)%Y+MSDV(I)*(2.0*SNGL(YFL)-1.0)
	IX= IY

      CALL DRANDU(IX,IY,YFL)
 	STRUCT%ATS(I)%Z= STRUCT%ATS(I)%Z+MSDV(I)*(2.0*SNGL(YFL)-1.0)
	IX= IY

	END IF

	!-----------------------оняюдйю бюйюмяхи------------------------------------
	IF(INT(AMP%PRVAC*SIZE(STRUCT%ATS)).GT.0) THEN

	IVAK=AMP%PRVAC*(I_MAX-I_GRAN)/100.0

	DO  I=1,150
      CALL DRANDU(IX,IY,YFL)
      IX=IY
	END DO

	I=0

	DO WHILE (I.LT.IVAK)

	CALL DRANDU(IX,IY,YFL)
	IX=IY
	INUM=(I_MAX-I_GRAN)*YFL+I_GRAN
	
      IF (STRUCT%ATS(INUM)%IS) THEN
	STRUCT%ATS(INUM)%IS=.FALSE.
	I=I+1
	END IF

	END DO

	END IF

	!---------------юлнптхгюжхъ опхцпюмхвмшу якнеб--------------------------------
	IF(AMP%NALIC.GT.0.AND.AMP%SDVCR.GT.1E-3.AND.I_GRANCR.NE.I_GRAN)
	1THEN

	ALLOCATE(MSDVCR(SIZE(STRUCT%ATS)))

	DO I=1,SIZE(STRUCT%ATS)	
	MSDVCR(I)=PLT(STRUCT%ATS(I)%TYP)*AMP%SDVCR*SQRT(3.0)/8.0 ! ДКЪ ЙПХЯРЮККЮ 
	END DO


	DO  I=I_GRANCR+1,I_GRAN

	CALL DRANDU(IX,IY,YFL)
	STRUCT%ATS(I)%X= STRUCT%ATS(I)%X+MSDVCR(I)*(2.0*SNGL(YFL)-1.0)
	IX= IY

      CALL DRANDU(IX,IY,YFL)
	STRUCT%ATS(I)%Y=STRUCT%ATS(I)%Y+MSDVCR(I)*(2.0*SNGL(YFL)-1.0)
	IX=IY

      CALL DRANDU(IX,IY,YFL)
 	STRUCT%ATS(I)%Z=STRUCT%ATS(I)%Z+MSDVCR(I)*(2.0*SNGL(YFL)-1.0)
	IX=IY	
      
      
	END DO

	END IF

	DEALLOCATE(MSDV)				!
	DEALLOCATE(MSDVCR)					!

	CONTAINS
	!____________________________________________________________End AMORF
	SUBROUTINE DRANDU(IX,IY,YFL)

	REAL*8,INTENT(INOUT)::YFL
	INTEGER*4,INTENT(INOUT)::IX,IY

      IY=IX*16807
      IF(IY)5,6,6
   5  IY=IY+2147483647+1
   6  YFL=DFLOAT(IY)
      YFL=YFL*0.4656613d-9     

      END SUBROUTINE DRANDU


	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	!рхо ябъгх
	INTEGER*2 FUNCTION GET_BOND_TYPE(I,J,ATOMS)

	INTEGER*4 I,J
	TYPE(T_ATOM),POINTER,DIMENSION(:)::ATOMS

	INTEGER*2 TI,TJ
	TI=ATOMS(I)%TYP
	TJ=ATOMS(J)%TYP

	IF(ATOMS(I)%TYP.EQ.GE.AND.ATOMS(J)%TYP.EQ.GE) GET_BOND_TYPE=B_GG
	IF(ATOMS(I)%TYP.EQ.SI.AND.ATOMS(J)%TYP.EQ.SI) GET_BOND_TYPE=B_SS
	IF(ATOMS(I)%TYP.NE.ATOMS(J)%TYP)            GET_BOND_TYPE=B_SG 
	

      END FUNCTION

	!--------------------------------------------------------------------------------------------------------------------------
	!рхо сцкю
	INTEGER*2 FUNCTION GET_ANGLE_TYPE(I,J,K,ATOMS)

	INTEGER*4 I,J,K
	TYPE(T_ATOM),POINTER,DIMENSION(:)::ATOMS

	!GE-GE-GE
	IF(ATOMS(I)%TYP.EQ.GE.AND.ATOMS(J)%TYP.EQ.GE.AND.
	1ATOMS(K)%TYP.EQ.GE)  GET_ANGLE_TYPE=A_GGG
	

	!SI-SI-SI
	IF(ATOMS(I)%TYP.EQ.SI.AND.ATOMS(J)%TYP.EQ.SI.AND.
	1ATOMS(K)%TYP.EQ.SI) GET_ANGLE_TYPE=A_SSS

	!GE-GE-SI
	IF(ATOMS(I)%TYP.EQ.GE.AND.ATOMS(J)%TYP.EQ.GE.AND.
	1ATOMS(K)%TYP.EQ.SI) GET_ANGLE_TYPE=A_GGS

	IF(ATOMS(I)%TYP.EQ.GE.AND.ATOMS(J)%TYP.EQ.SI.AND.
	1ATOMS(K)%TYP.EQ.GE)  GET_ANGLE_TYPE=A_GGS
	
 
 	IF(ATOMS(I)%TYP.EQ.SI.AND.ATOMS(J)%TYP.EQ.GE.AND.
	1ATOMS(K)%TYP.EQ.GE) 	GET_ANGLE_TYPE=A_GGS 	

	!GE-SI-SI
	IF(ATOMS(I)%TYP.EQ.GE.AND.ATOMS(J)%TYP.EQ.SI.AND.
	1ATOMS(K)%TYP.EQ.SI) 	GET_ANGLE_TYPE=A_GSS  	

	IF(ATOMS(I)%TYP.EQ.SI.AND.ATOMS(J)%TYP.EQ.SI.AND.
	1ATOMS(K)%TYP.EQ.GE)   GET_ANGLE_TYPE=A_GSS
 
 	IF(ATOMS(I)%TYP.EQ.SI.AND.ATOMS(J)%TYP.EQ.GE.AND.
	1ATOMS(K)%TYP.EQ.SI) 	GET_ANGLE_TYPE=A_GSS	

      END FUNCTION 


	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякъер пюяопедекемхе дкхм ябъгеи
	SUBROUTINE FIND_BONDS_LENGTHS(ATOMS,L_SS,L_SG,L_GG)

	TYPE(T_ATOM),POINTER,DIMENSION(:)::ATOMS
	REAL*8,POINTER,DIMENSION(:)::L_SS,L_SG,L_GG
		
	REAL*8 R
	INTEGER*4 NUM
 
      ALLOCATE(L_SS(0))
	ALLOCATE(L_SG(0))
	ALLOCATE(L_GG(0))
	

	DO I=1,SIZE(ATOMS) 	
	
      IF(ATOMS(I)%LAY.NE.OUT)	 THEN
     	
	DO JJ=1,4

	J=ATOMS(I)%NEIBR(JJ)

	IF(ATOMS(J).LAY.NE.OUT)	 THEN

	R=GETR(I,J,ATOMS)

	IF(ATOMS(I)%TYP.EQ.GE.AND.ATOMS(J)%TYP.EQ.GE) NUM=ADD(L_GG,R)
	IF(ATOMS(I)%TYP.EQ.SI.AND.ATOMS(J)%TYP.EQ.SI) NUM=ADD(L_SS,R)
	IF(ATOMS(I)%TYP.EQ.GE.AND.ATOMS(J)%TYP.EQ.SI) NUM=ADD(L_SG,R)
	IF(ATOMS(I)%TYP.EQ.SI.AND.ATOMS(J)%TYP.EQ.GE) NUM=ADD(L_SG,R)
	
      END IF	

	END DO 	 

	END IF

	END DO

	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	!бшвхякъер пюяопедекемхе йняхмсянб сцкнб лефдс рпнийюлх акхфюиьху юрнлнб
	SUBROUTINE BOND_ANGLES(ATOMS,ANGLES)

	TYPE(T_ATOM),POINTER,DIMENSION(:)::ATOMS
	TYPE(T_ANGLES) ANGLES

	REAL*8 COS_JIK,RIJ,RIK,RJK
	INTEGER*4 NUM
	INTEGER*2 BT_IJ,BT_IK

	ALLOCATE(ANGLES%SSS(0))
	ALLOCATE(ANGLES%SGS(0))
	ALLOCATE(ANGLES%GSS(0))
	ALLOCATE(ANGLES%GGG(0))
	ALLOCATE(ANGLES%GSG(0))
	ALLOCATE(ANGLES%SGG(0))

	NUM=0

	DO I=1,SIZE(ATOMS)
	
	IF(ATOMS(I)%LAY.NE.OUT) THEN

	DO JJ=1,3

	J=ATOMS(I)%NEIBR(JJ)

	IF(ATOMS(J)%LAY.NE.OUT) THEN

	DO KK=JJ+1,4

	K=ATOMS(I)%NEIBR(KK)

	IF(ATOMS(KK)%LAY.NE.OUT) THEN

	RIJ=GETR(I,J,ATOMS)
	RIK=GETR(I,J,ATOMS)
	RJK=GETR(J,K,ATOMS)

	COS_JIK=(RIJ**2+RIK**2-RJK**2)/(2*RIJ*RIK)

	BT_IJ=GET_BOND_TYPE(I,J,ATOMS)
	BT_IK=GET_BOND_TYPE(I,K,ATOMS)


	IF(ATOMS(I)%TYP.EQ.GE) THEN
	
	IF(ATOMS(J)%TYP.EQ.GE.AND.ATOMS(K)%TYP.EQ.GE)
	1 NUM=ADD(ANGLES%GGG,ACOS(COS_JIK))
      
	IF(ATOMS(J)%TYP.EQ.SI.AND.ATOMS(K)%TYP.EQ.SI)
	1 NUM=ADD(ANGLES%SGS,ACOS(COS_JIK))
 
      IF(ATOMS(J)%TYP.NE.ATOMS(K)%TYP)
	1 NUM=ADD(ANGLES%SGG,ACOS(COS_JIK))
       
	ELSE

	IF(ATOMS(J)%TYP.EQ.SI.AND.ATOMS(K)%TYP.EQ.SI)
	1 NUM=ADD(ANGLES%SSS,ACOS(COS_JIK))
      
	IF(ATOMS(J)%TYP.EQ.GE.AND.ATOMS(K)%TYP.EQ.GE)
	1 NUM=ADD(ANGLES%GSG,ACOS(COS_JIK))
 
      IF(ATOMS(J)%TYP.NE.ATOMS(K)%TYP)
	1 NUM=ADD(ANGLES%GSS,ACOS(COS_JIK))

	END IF

	END IF

	END DO

	END IF

	END DO
	
	END IF

	END DO


	END SUBROUTINE
	

	END MODULE	MDSTRUCT