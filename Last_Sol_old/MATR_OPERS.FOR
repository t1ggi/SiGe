	MODULE MDMATRIX_TRANSFS


	USE MDCONSTS
	USE MDTYPES
	USE MDBOUNDS
	USE MDSTRUCT

	PRIVATE::DTRED
	PRIVATE::DTQLI
	PRIVATE::NORM
	PRIVATE::CHECK_ORTS
	PUBLIC::TRANSFORM


	CONTAINS

	!---------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE DTRED(DIN_MATR,EIG_VALS,EIG_VALS2)	 

	REAL*8 ,POINTER,DIMENSION(:,:)::DIN_MATR
	REAL*8 ,POINTER,DIMENSION(:)::EIG_VALS,EIG_VALS2 	
	
	REAL*8,POINTER,DIMENSION(:,:)::NEW_MATR 
	REAL*8  H,HH,F,G,TOL 
	INTEGER*4 NATOMS

	INTEGER*4 NTEST



	ALLOCATE(EIG_VALS2(SIZE(DIN_MATR,1)))	
	ALLOCATE(EIG_VALS(SIZE(DIN_MATR,1)))
      ALLOCATE(NEW_MATR(SIZE(DIN_MATR,1),SIZE(DIN_MATR,2)))
	NATOMS=SIZE(DIN_MATR,1)/3

	DO I=1,3*NATOMS
	DO J=1,3*NATOMS 
	DIN_MATR(I,J)=DIN_MATR(I,J)/100.0
!	IF(ABS(DIN_MATR(I,J)).LE.1E-3) DIN_MATR(I,J)=0.0
	END DO 
	END DO

	NTEST=0

	DO I=1,3*NATOMS
	DO J=1,I 
	NEW_MATR(I,J) = DIN_MATR(I,J)
	END DO 
	END DO

	TOL = 1.D-50

	 NTEST=0
	!_____________________________________
	do 2 i=3*NATOMS,2,-1
	l=i-2
	f=NEW_MATR(i,i-1)
	g=0.d0

	do 12 k=1,l
12	g=g+NEW_MATR(i,k)*NEW_MATR(i,k)
      
	h=g+f*f
c
	if(g.le.tol)then 
c	write(*,*)'l=',l ,'k=',k,'g=',g
	EIG_VALS2(i)=f
	h=0.d0
	goto 400
	end if
	l=l+1
	if(f.ge.0.d0)then
	EIG_VALS2(i)=-sqrt(h)
	g=EIG_VALS2(I)
	else
	EIG_VALS2(i)=sqrt(h)
	g=EIG_VALS2(i)
	end if
	h=h-f*g
	NEW_MATR(i,i-1)=f-g
	NTEST=NTEST+1
!	WRITE(818,*) NTEST,NEW_MATR(I,I-1)
	f=0.d0
c
	do 3 j=1,l
	NEW_MATR(j,i)=NEW_MATR(i,j)/h
	NTEST=NTEST+1
!	WRITE(818,*) NTEST,NEW_MATR(J,I)
	g=0.d0

	do 4 k=1,j
4	g=g+NEW_MATR(j,k)*NEW_MATR(i,k)
	do 5 k=j+1,l
5	g=g+NEW_MATR(k,j)*NEW_MATR(i,k)
c
	EIG_VALS2(j)=g/h
	f=f+g*NEW_MATR(j,i)
3	continue


	hh=f/(h+h)
c
	do 6 j=1,l
	f=NEW_MATR(i,j)
	EIG_VALS2(j)=EIG_VALS2(j)-hh*f
	g=EIG_VALS2(j)
	do 6 k=1,j
	NEW_MATR(j,k)=NEW_MATR(j,k)-f*EIG_VALS2(k)-g*NEW_MATR(i,k)
	NTEST=NTEST+1
!	WRITE(818,*) NTEST,NEW_MATR(J,K)
6	continue
400     continue
2	EIG_VALS(i)=h



	EIG_VALS(1)=0.d0
	EIG_VALS2(1)=0.d0
c
	do 7 i=1,3*NATOMS
	l=i-1
	if(EIG_VALS(i).ne.0.d0)then
	do 8 j=1,l
	g=0.d0
	do 9 k=1,l
9	g=g+NEW_MATR(i,k)*NEW_MATR(k,j)
	do 8 k=1,l

8	NEW_MATR(k,j)=NEW_MATR(k,j)-g*NEW_MATR(k,i)
        end if


	EIG_VALS(i)=NEW_MATR(i,i)
	NEW_MATR(i,i)=1.d0
	do 17 j=1,l
	NEW_MATR(i,j)=0.d0
17	NEW_MATR(j,i)=NEW_MATR(i,j)
7 	continue


 

!	DIAG_MATR = NEW_MATR

	DO I=1,3*NATOMS
	DO J=1,3*NATOMS 
	DIN_MATR(I,J)=NEW_MATR(I,J)
	END DO 
	END DO 

	END SUBROUTINE

	!-------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE DTQLI(DIN_MATR,EIG_VALS,EIG_VALS2)	  

	REAL*8 ,POINTER,DIMENSION(:,:)::DIN_MATR
	REAL*8 ,POINTER,DIMENSION(:)::EIG_VALS,EIG_VALS2 

	real*8 dd,r,g,s,c0,p,b,f
	INTEGER*4 N

	N=SIZE(DIN_MATR,1)	

	do 11 i=2,n
11    EIG_VALS2(i-1)=EIG_VALS2(i)

      EIG_VALS2(n)=0.d0

      do 15 l=1,n
c	WRITE(*,*)'L=',L
        iter=0

1       do 12 m=l,n-1
        dd=dabs(EIG_VALS(m))+dabs(EIG_VALS(m+1))
        if(dabs(EIG_VALS2(m))+dd.eq.dd) THEN
	    MK=25.3
		goto 2
	  END IF
12      continue


        m=n
2       if(m.ne.l) then

        !if(iter.eq.100) pause 'too many iteration'

        iter=iter+1
        g=(EIG_VALS(l+1)-EIG_VALS(l))/(2.*EIG_VALS(l))
        r=dsqrt(g**2+1.)
        g=EIG_VALS(m)-EIG_VALS(l)+EIG_VALS2(l)/(g+dsign(r,g))
        s=1.d0
        c0=1.d0
        p=0.d0

        do 14 i=m-1,l,-1
        f=s*EIG_VALS2(i)
        b=c0*EIG_VALS2(i)


        if(dabs(f).ge.dabs(g)) then

        c0=g/f
        r=dsqrt(c0**2+1.d0)
        EIG_VALS2(i+1)=f*r
        s=1.d0/r
        c0=c0*s

        else

        s=f/g
        r=dsqrt(s**2+1.d0)
        EIG_VALS2(i+1)=g*r
        c0=1.d0/r
        s=s*c0

        endif

        g=EIG_VALS(i+1)-p
        r=(EIG_VALS(i)-g)*s+2.*c0*b
        p=s*r
        EIG_VALS(i+1)=g+p
        g=c0*r-b
c

	do 16 is=1,n
	f=DIN_MATR(is,i+1)
	DIN_MATR(is,i+1)=s*DIN_MATR(is,i)+c0*f
16	DIN_MATR(is,i)=c0*DIN_MATR(is,i)-s*f
c
14      continue

        EIG_VALS(l)=EIG_VALS(l)-p
        EIG_VALS2(l)=g
        EIG_VALS2(m)=0.d0
        go to 1

        endif

15      continue	 

 !       endif !ÍÓÌÂˆ ÓÒÌÓ‚ÌÓ„Ó ˆËÍÎ‡


	do 30 i=1,n
	k=i
	cc=EIG_VALS(i)

	do 31 j=i+1,n

	if(EIG_VALS(j).lt.cc)then
	k=j
	cc=EIG_VALS(j)
	end if

31    continue

	if(k.ne.i)then
	EIG_VALS(k)=EIG_VALS(i)
	EIG_VALS(i)=cc
	do 32 j=1,n
	cc=DIN_MATR(j,i)
	DIN_MATR(j,i)=DIN_MATR(j,k)
32    DIN_MATR(j,k)=cc
      end if

30    continue

	END SUBROUTINE 

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE NORM(DIN_MATR)

 	REAL*8 ,POINTER,DIMENSION(:,:)::DIN_MATR 

	REAL*8 S,A1,A2
	INTEGER*4 N

	N=SIZE(DIN_MATR,1)

	do 110 i=1,N
	a1=dabs(DIN_MATR(1,i))
	do 220 j=2,N
	a2=dabs(DIN_MATR(j,i))
220	a1=dmax1(a1,a2)
	do 330 j=1,N
330	DIN_MATR(j,i)=DIN_MATR(j,i)/a1	
110	continue
	!ÌÓÏ‡ÎËÁÛÂÏ Ì‡ ÒÂ‰ÌÂÍ‚‡‰. 
	write(*,*)' Normalisation'
	do 201 i=1,N
	s=0.d00
	do 200 j=1,N
200	s=s+DIN_MATR(j,i)*DIN_MATR(j,i)
	s=dsqrt(s)
      if(s.eq.0.d0)goto 202
	do 310 j=1,N
310	DIN_MATR(j,i)=DIN_MATR(j,i)/s
201   continue
202   continue 


	END SUBROUTINE

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE CHECK_ORTS(DIN_MATR,EVALS)

	REAL*8 ,POINTER,DIMENSION(:,:)::DIN_MATR
	REAL*8 ,POINTER,DIMENSION(:)::EVALS


	REAL*8 SZ0,A1,A2
	INTEGER*4 N

	N=SIZE(DIN_MATR,1)

	j=1
172	continue
	sz0=0.d00
	j2=j+1
	if(j2.gt.N+1)goto 173
	if(j2.eq.N+1)j2=1

	do 171 k=1,N
171	sz0=sz0+DIN_MATR(k,j)*DIN_MATR(k,j2)

173	j=j+1
	if(j.lt.N+1)goto 172			
c	  
	a1=dabs(EVALS(1))
	do 22 j=2,N
	a2=dabs(EVALS(j))
22	a1=dmax1(a1,a2)

	!A1 ¬€¡»–¿≈Ã  ¿  —¿ÃŒ≈ (!!!) Ã¿ —»Ã¿À‹ÕŒ≈ —Œ¡—“¬≈ÕÕŒ≈ «Õ¿◊≈Õ»≈ ƒ»Õ¿Ã»◊≈— Œ… Ã¿“–»÷€,
	!œŒÀ”◊≈ÕÕŒ≈ œ–» Ã¿ —»Ã¿À‹ÕŒ ¬Œ«ÃŒ∆ÕŒÃ ◊»—À≈ ¿“ŒÃŒ¬  –»—“¿ÀÀ¿  !
	!ƒÀﬂ –¿«ÕŒ√Œ —Œ—“¿¬¿  –»—“¿ÀÀ¿ ¿1 ¡”ƒ≈“ –¿«À»◊Õ€Ã !!!

	!Õ≈ Œ“Œ–€≈ œŒÀ”◊≈ÕÕ€≈ «Õ¿◊≈Õ»ﬂ ¿1 œ–»¬≈ƒ≈Õ€ «ƒ≈—‹

!	A1=0.36                        !GE, TSF
!	A1=1.429041193961722E-001*8.d0 !SI,STW
!	A1=0.385                       !GE,STW  
!     A1=1.06                        !SI,TSF
!     A1=0.64                        !SI_GE,TSF
  	A1=1.06

	do 33 j=1,N
	IF(EVALS(J).GT.0.D0)THEN
	EVALS(j)=dsqrt(EVALS(j)/a1)
	ELSE
	EVALS(J)=0.D0
33	END IF


	END SUBROUTINE 

	!--------------------------------------------------------------------------------------------------------------------------
	SUBROUTINE TRANSFORM(DIN_MATR,EIG_MATR,EIG_VALS)

	USE MSIMSL

	REAL*8,POINTER,DIMENSION(:,:)::DIN_MATR
	REAL*8,POINTER,DIMENSION(:,:)::EIG_MATR
	REAL*8,POINTER,DIMENSION(:)::EIG_VALS	  

	REAL,POINTER,DIMENSION(:,:)::MATR
	COMPLEX,POINTER,DIMENSION(:)::EVAL
	COMPLEX,POINTER,DIMENSION(:,:)::EVEC
	REAL*8,POINTER,DIMENSION(:)::EIG_VALS2

	ALLOCATE(EIG_MATR(SIZE(DIN_MATR,1),SIZE(DIN_MATR,2)))
	ALLOCATE(EIG_VALS(SIZE(DIN_MATR,1)))

!	GOTO 2
	
	!-------------»—œŒÀ‹«Œ¬¿Õ»≈ ‘”Õ ÷»… »« ¡»¡À»Œ“≈ »------------

	ALLOCATE(MATR(SIZE(DIN_MATR,1),SIZE(DIN_MATR,1)))
	ALLOCATE(EVAL(SIZE(DIN_MATR,1)))
	ALLOCATE(EVEC(SIZE(DIN_MATR,1),SIZE(DIN_MATR,1)))

	DO I=1,SIZE(DIN_MATR,1)
	DO J=1,SIZE(DIN_MATR,2)
	MATR(I,J)=DIN_MATR(I,J)
	END DO
	END DO	

	
	CALL EVCRG(SIZE(DIN_MATR,1),MATR,SIZE(DIN_MATR,1),EVAL,EVEC,
	1SIZE(DIN_MATR,1))

	WRITE(*,*) 'EIGEN VECTS FOUNDED'
	 

	DO I=1,SIZE(EVAL)
	EVAL(I)=EVAL(I)/100.0
	EIG_VALS(I)=EVAL(I)
	END DO

	DEALLOCATE(EVAL)

	DO I=1,SIZE(DIN_MATR,1)
	DO J=1,SIZE(DIN_MATR,2)
	EIG_MATR(I,J)=EVEC(I,J)
	END DO
	END DO 

	DEALLOCATE(EVEC)
	DEALLOCATE(MATR)


	CALL NORM(EIG_MATR)
	CALL CHECK_ORTS(EIG_MATR,EIG_VALS)

	GOTO 1
	
!     --------»—œŒÀ‹«Œ¬¿Õ»≈ ‘”Õ ÷»… »« ÃŒƒ”Àﬂ--------------------------	

2	ALLOCATE(EIG_VALS2(SIZE(DIN_MATR,1)))

	DO I=1,SIZE(DIN_MATR,1)
	DO J=1,SIZE(DIN_MATR,2)
	EIG_MATR(I,J)=DIN_MATR(I,J)
	END DO
	END DO

	CALL DTRED(EIG_MATR,EIG_VALS,EIG_VALS2)
	CALL DTQLI(EIG_MATR,EIG_VALS,EIG_VALS2)
	CALL NORM(EIG_MATR)
	CALL CHECK_ORTS(EIG_MATR,EIG_VALS)	

1	END SUBROUTINE

	END MODULE