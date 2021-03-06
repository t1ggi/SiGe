MODULE MDSTRUCT,0
FILE 0,G:\Last_Sol_old\STRUCT.FOR
USE MDBOUNDS
USE MDCONSTS
USE MDTYPES
TYPE T_ANGLES,1,0,192
  SSS(0): 2,8,0,110,32,2097152,0 (1,8,0: 1,0,5,3)
  GGG(32): 2,8,0,110,32,2097152,0 (1,8,0: 1,0,5,3)
  SGS(64): 2,8,0,110,32,2097152,0 (1,8,0: 1,0,5,3)
  GSG(96): 2,8,0,110,32,2097152,0 (1,8,0: 1,0,5,3)
  GSS(128): 2,8,0,110,32,2097152,0 (1,8,0: 1,0,5,3)
  SGG(160): 2,8,0,110,32,2097152,0 (1,8,0: 1,0,5,3)
ENDTYPE
TYPE T_NUM,1,2,12
  I(0): 1,4,0,0,0,2097152,0
  J(4): 1,4,0,0,0,2097152,0
  K(8): 1,4,0,0,0,2097152,0
ENDTYPE
PROC ELLIPSE_STRUCT,3,8,0: 8,0,0,0,393216,0,2
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
VAR NUM_LAYS,3,0,,: 1,4,0,0,259,2097152,0
VAR PLT,3,0,,: 2,8,0,0,259,2097152,0
ENDPROC
PROC CREATE_STRUCT,9,8,0: 7,T_STRUCT,0,0,262281,0,1
VAR NUM,3,0,,: 1,4,0,0,11,2097152,0
VAR FORM,3,0,,: 1,2,0,0,267,2097152,0
VAR DISTR,3,0,,: 1,2,0,0,259,2097152,0
VAR TYP1,3,0,,: 1,2,0,0,267,2097152,0
VAR TYP2,3,0,,: 1,2,0,0,267,2097152,0
VAR PLT,3,0,,: 2,8,0,0,395,2097152,0
VAR X,3,0,,: 2,8,0,0,11,2097152,0
VAR BOUNDS,3,0,,: 1,2,0,0,259,2097152,0
VAR NUM_CLR,3,0,,: 1,4,0,0,11,2097152,0
ENDPROC
PROC SET_TYPES,4,8,0: 8,0,0,0,393216,0,2
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
VAR X,3,0,,: 2,8,0,0,259,2097152,0
VAR TYP1,3,0,,: 1,2,0,0,259,2097152,0
VAR TYP2,3,0,,: 1,2,0,0,259,2097152,0
ENDPROC
PROC AMORF,2,8,0: 8,0,0,0,262144,0,1
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
VAR AMP,3,0,,: 7,T_AMORF_PARAMS,0,0,259,2097152,0
ENDPROC
PROC BOND_ANGLES,2,8,0: 8,0,0,0,262144,0,1
VAR ATOMS,3,0,,: 7,T_ATOM,0,6816,8388907,2097152,0 (1,8,0: 1,0,5,3)
VAR ANGLES,3,0,,: 7,T_ANGLES,0,0,395,2097152,0
ENDPROC
PROC SET_LAYS,1,8,0: 8,0,0,0,393216,0,2
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
ENDPROC
PROC FIND_CELL,4,8,0: 8,0,0,0,262144,0,1
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
VAR PLT,3,0,,: 2,8,0,0,259,2097152,0
VAR NUM_CELLS,3,0,,: 1,4,0,0,3,2097152,0
VAR SHIFT,3,0,,: 2,0,0,0,267,2097152,0
ENDPROC
PROC CLASTER_STRUCT,4,8,0: 8,0,0,0,393216,0,2
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
VAR C_TYP,3,0,,: 1,2,0,0,259,2097152,0
VAR S_TYP,3,0,,: 1,2,0,0,259,2097152,0
VAR NUM_CLR,3,0,,: 1,4,0,0,259,2097152,0
ENDPROC
PROC SET_ENVIRS,2,8,0: 8,0,0,0,262144,0,1
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
VAR R0,3,0,,: 2,8,0,0,259,2097152,0
ENDPROC
PROC SET_CELL,4,8,0: 8,0,0,0,393216,0,1
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
VAR PLT,3,0,,: 2,8,0,0,259,2097152,0
VAR NUM_CELLS,3,0,,: 1,4,0,0,259,2097152,0
VAR SHIFT,3,0,,: 2,0,0,0,259,2097152,0
ENDPROC
PROC ORD_STRUCT,3,8,0: 8,0,0,0,393216,0,2
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
VAR TYP1,3,0,,: 1,2,0,0,259,2097152,0
VAR TYP2,3,0,,: 1,2,0,0,259,2097152,0
ENDPROC
PROC GET_BOND_TYPE,3,8,0: 1,2,0,0,393345,0,1
VAR I,3,0,,: 1,4,0,0,259,2097152,0
VAR J,3,0,,: 1,4,0,0,259,2097152,0
VAR ATOMS,3,0,,: 7,T_ATOM,0,6306,8388899,2097152,0 (1,8,0: 1,0,5,3)
ENDPROC
PROC GET_ANGLE_TYPE,4,8,0: 1,2,0,0,262273,0,1
VAR I,3,0,,: 1,4,0,0,259,2097152,0
VAR J,3,0,,: 1,4,0,0,259,2097152,0
VAR K,3,0,,: 1,4,0,0,259,2097152,0
VAR ATOMS,3,0,,: 7,T_ATOM,0,6402,8388899,2097152,0 (1,8,0: 1,0,5,3)
ENDPROC
PROC SET_NEIBRS,2,8,0: 8,0,0,0,393216,0,2
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
VAR PLT,3,0,,: 2,8,0,0,259,2097152,0
ENDPROC
PROC CUBIC_STRUCT,3,8,0: 8,0,0,0,393216,0,2
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,387,2097152,0
VAR NUM,3,0,,: 1,4,0,0,259,2097152,0
VAR PLT,3,0,,: 2,8,0,0,259,2097152,0
ENDPROC
PROC SYSCOOR,2,8,0: 8,0,0,0,262144,0,1
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
VAR PLT,3,0,,: 2,8,0,0,259,2097152,0
ENDPROC
PROC FIND_BONDS_LENGTHS,4,8,0: 8,0,0,0,262144,0,1
VAR ATOMS,3,0,,: 7,T_ATOM,0,6634,8388907,2097152,0 (1,8,0: 1,0,5,3)
VAR L_SS,3,0,,: 2,8,0,6641,-2139094997,2097152,0 (1,8,0: 1,0,5,3)
VAR L_SG,3,0,,: 2,8,0,6641,-2139094997,2097152,0 (1,8,0: 1,0,5,3)
VAR L_GG,3,0,,: 2,8,0,6641,-2139094997,2097152,0 (1,8,0: 1,0,5,3)
ENDPROC
PROC SET_LEVELS,1,8,0: 8,0,0,0,393216,0,2
VAR STRUCT,3,3,,: 7,T_STRUCT,0,0,395,2097152,0
ENDPROC
END
