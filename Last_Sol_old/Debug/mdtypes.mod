MODULE MDTYPES,0
FILE 0,E:\4 ����\��������\Vasin\Last_Sol\TYPES.FOR
TYPE T_COORD,1,0,24
  X(0): 2,8,0,0,0,2097152,0
  Y(8): 2,8,0,0,0,2097152,0
  Z(16): 2,8,0,0,0,2097152,0
ENDTYPE
TYPE T_ATOM,1,0,96
  X(0): 2,8,0,0,0,2097152,0
  Y(8): 2,8,0,0,0,2097152,0
  Z(16): 2,8,0,0,0,2097152,0
  NEIBR(24): 1,4,0,106,0,2097152,0 (1,1,4: 1,4,1,1)
  ENVRS(40): 1,4,0,112,32,2097152,0 (1,8,0: 1,0,5,3)
  LEV(72): 1,4,0,0,0,2097152,0
  TYP(76): 1,2,0,0,0,2097152,0
  M(80): 2,8,0,0,0,2097152,0
  IS(88): 4,0,0,0,0,2097152,0
  LAY(92): 1,2,0,0,0,2097152,0
ENDTYPE
TYPE T_NUMBER,1,0,8
  IN(0): 1,4,0,0,0,2097152,0
  INN(4): 1,4,0,0,0,2097152,0
ENDTYPE
TYPE T_STRUCT,1,0,200
  ATS(0): 7,T_ATOM,0,147,32,2097152,0 (1,8,0: 1,0,5,3)
  BNDS(32): 1,2,0,0,0,2097152,0
  CELL(40): 7,T_COORD,0,0,0,2097152,0
  NUMS(64): 7,T_NUMBER,0,162,32,2097152,0 (1,8,0: 1,0,5,3)
  FORM(96): 1,2,0,0,0,2097152,0
  NATOMS(100): 1,4,0,0,0,2097152,0
  INS(104): 1,4,0,177,32,2097152,0 (1,8,0: 1,0,5,3)
  FRONTS(136): 1,4,0,177,32,2097152,0 (1,8,0: 1,0,5,3)
  OUTS(168): 1,4,0,177,32,2097152,0 (1,8,0: 1,0,5,3)
ENDTYPE
TYPE T_AMORF_PARAMS,1,0,40
  IX(0): 1,4,0,0,0,2097152,0
  SDVIG(8): 2,8,0,0,0,2097152,0
  SDVCR(16): 2,8,0,0,0,2097152,0
  PRVAC(24): 2,8,0,0,0,2097152,0
  NALIC(32): 1,4,0,0,0,2097152,0
ENDTYPE
TYPE T_RADDISTR,1,0,96
  N(0): 1,4,0,207,32,2097152,0 (1,8,0: 1,0,5,3)
  R(32): 2,8,0,214,32,2097152,0 (1,8,0: 1,0,5,3)
  NS(64): 2,8,0,221,32,2097152,0 (1,8,0: 1,0,5,3)
ENDTYPE
TYPE T_SPECTRE,1,0,96
  W(0): 2,8,0,230,32,2097152,0 (1,8,0: 1,0,5,3)
  SP(32): 2,8,0,237,32,2097152,0 (1,8,0: 1,0,5,3)
  SPR(64): 2,8,0,244,32,2097152,0 (1,8,0: 1,0,5,3)
ENDTYPE
PROC CRD_ATOM8,3,8,0: 7,T_ATOM,0,0,393353,2097152,2
VAR X,3,0,,: 2,8,0,0,259,2097152,0
VAR Y,3,0,,: 2,8,0,0,259,2097152,0
VAR Z,3,0,,: 2,8,0,0,259,2097152,0
ENDPROC
PROC CRD_ATOM4,3,8,0: 7,T_ATOM,0,0,393353,2097152,2
VAR X,3,0,,: 2,0,0,0,259,2097152,0
VAR Y,3,0,,: 2,0,0,0,259,2097152,0
VAR Z,3,0,,: 2,0,0,0,259,2097152,0
ENDPROC
PROC GET_R_COORDS2,2,8,0: 2,8,0,0,393345,2097152,2
VAR CRD1,3,0,,: 7,T_COORD,0,0,259,2097152,0
VAR CRD2,3,0,,: 7,T_COORD,0,0,259,2097152,0
ENDPROC
PROC GET_R_ATOMS2,2,8,0: 2,8,0,0,393345,2097152,2
VAR ATOM1,3,0,,: 7,T_ATOM,0,0,259,2097152,0
VAR ATOM2,3,0,,: 7,T_ATOM,0,0,259,2097152,0
ENDPROC
PROC DELETE_ATOM,1,8,0: 8,0,0,0,393216,2097152,2
VAR ATOM,3,3,,: 7,T_ATOM,0,0,259,2097152,0
ENDPROC
PROC GET_R_ATOMS0,3,8,0: 2,8,0,0,393345,2097152,1
VAR I,3,0,,: 1,4,0,0,259,2097152,0
VAR J,3,0,,: 1,4,0,0,259,2097152,0
VAR ATOMS,3,0,,: 7,T_ATOM,0,1267,8388899,2097152,0 (1,8,0: 1,0,5,3)
ENDPROC
PROC DELETE_STRUCT,1,8,0: 8,0,0,0,393216,2097152,2
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,259,2097152,0
ENDPROC
PROC ADD_REAL,2,8,0: 1,4,0,0,393345,2097152,2
VAR REALS,3,0,,: 2,8,0,454,-2139094613,2097152,0 (1,8,0: 1,0,5,3)
VAR NEW_REAL,3,0,,: 2,8,0,0,259,2097152,0
ENDPROC
PROC REINIT_REAL_ARRAY,2,8,0: 8,0,0,0,393216,2097152,2
VAR REALS,3,0,,: 2,8,0,1116,-2139094613,2097152,0 (1,8,0: 1,0,5,3)
VAR I,3,0,,: 1,4,0,0,259,2097152,0
ENDPROC
PROC REINIT_ATOM_ARRAY,2,8,0: 8,0,0,0,393216,2097152,2
VAR ATOMS,3,0,,: 7,T_ATOM,0,1034,-2139094613,2097152,0 (1,8,0: 1,0,5,3)
VAR I,3,0,,: 1,4,0,0,259,2097152,0
ENDPROC
PROC REMOVE_INT_OBJ,2,8,0: 4,0,0,0,393345,2097152,1
VAR INTS,3,0,,: 1,4,0,984,8388907,2097152,0 (1,8,0: 1,0,5,3)
VAR INT,3,0,,: 1,4,0,0,259,2097152,0
ENDPROC
PROC REMOVE_INT_IND,2,8,0: 4,0,0,0,393345,2097152,1
VAR INTS,3,0,,: 1,4,0,758,-2139094613,2097152,0 (1,8,0: 1,0,5,3)
VAR I,3,0,,: 1,4,0,0,259,2097152,0
ENDPROC
PROC SET_RADDISTR_SIZE,2,8,0: 8,0,0,0,393216,2097152,2
VAR RADDISTR,3,3,,: 7,T_RADDISTR,0,0,131,2097152,0
VAR N,3,0,,: 1,4,0,0,259,2097152,0
ENDPROC
PROC REMOVE_REAL,2,8,0: 4,0,0,0,393345,2097152,1
VAR REALS,3,0,,: 2,8,0,871,-2139094613,2097152,0 (1,8,0: 1,0,5,3)
VAR I,3,0,,: 1,4,0,0,259,2097152,0
ENDPROC
PROC ADD_INT,2,8,0: 1,4,0,0,393345,2097152,2
VAR INTS,3,0,,: 1,4,0,550,-2139094613,2097152,0 (1,8,0: 1,0,5,3)
VAR NEW_INT,3,0,,: 1,4,0,0,259,2097152,0
ENDPROC
PROC REMOVE_ATOM,2,8,0: 4,0,0,0,393345,2097152,1
VAR ATOMS,3,0,,: 7,T_ATOM,0,645,-2139094613,2097152,0 (1,8,0: 1,0,5,3)
VAR I,3,0,,: 1,4,0,0,259,2097152,0
ENDPROC
PROC ADD_ATOM,2,8,0: 1,4,0,0,393345,2097152,2
VAR ATOMS,3,0,,: 7,T_ATOM,0,358,-2139094613,2097152,0 (1,8,0: 1,0,5,3)
VAR NEW_ATOM,3,0,,: 7,T_ATOM,0,0,267,2097152,0
ENDPROC
PROC ASSIGN_STRUCT,2,8,0: 8,0,0,0,393216,2097152,2
VAR OUT_STRUCT,3,2,,: 7,T_STRUCT,0,0,395,2097152,0
VAR IN_STRUCT,3,1,,: 7,T_STRUCT,0,0,267,2097152,0
ENDPROC
PROC ASSIGN_ATOM,2,8,0: 8,0,0,0,393216,2097152,2
VAR OUT_ATOM,3,2,,: 7,T_ATOM,0,0,395,2097152,0
VAR IN_ATOM,3,1,,: 7,T_ATOM,0,0,267,2097152,0
ENDPROC
PROC DLT,2,8,0: 2,0,0,0,262273,0,1
VAR I,3,0,,: 1,0,0,0,259,2097152,0
VAR J,3,0,,: 1,0,0,0,259,2097152,0
ENDPROC
PROC GET_R_CRD_ATOM,2,8,0: 2,8,0,0,393345,2097152,2
VAR ATOM,3,0,,: 7,T_ATOM,0,0,259,2097152,0
VAR CRD,3,0,,: 7,T_COORD,0,0,259,2097152,0
ENDPROC
GENERIC =: ASSIGN_STRUCT,ASSIGN_ATOM
GENERIC GETR: GET_R_ATOMS0,GET_R_CRD_ATOM,GET_R_COORDS2,GET_R_ATOMS2
GENERIC DELETE: DELETE_STRUCT,DELETE_ATOM
GENERIC REINIT: REINIT_REAL_ARRAY,REINIT_ATOM_ARRAY
GENERIC SET_SIZE: SET_RADDISTR_SIZE
GENERIC REMOVE_OBJ: REMOVE_INT_OBJ
GENERIC REMOVE_IND: REMOVE_INT_IND,REMOVE_REAL,REMOVE_ATOM
GENERIC ADD: ADD_INT,ADD_REAL,ADD_ATOM
GENERIC CRD_ATOM: CRD_ATOM8,CRD_ATOM4
END
