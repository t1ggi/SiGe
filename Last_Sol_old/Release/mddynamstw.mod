MODULE MDDYNAMSTW,0
FILE 0,E:\4 ����\��������\Vasin\Last_Sol\DINAM.FOR
USE MDSTRUCT
USE MDBOUNDS:R=>GETR
USE MDCONSTS
USE MDTYPES
REF R(GETR@MDBOUNDS),2
REF U(U@MDBOUNDS),2
REF DLT(DLT@MDTYPES),2
TYPE T_NBS,1,2,32
  N(0): 1,4,0,103,32,2097152,0 (1,8,0: 1,0,5,3)
ENDTYPE
TYPE T_INDEX,1,2,8
  I(0): 2,0,0,0,0,2097152,0
  J(4): 2,0,0,0,0,2097152,0
ENDTYPE
TYPE T_DIM2,1,2,24
  IJK(0): 7,T_INDEX,0,0,0,2097152,0
  JIK(8): 7,T_INDEX,0,0,0,2097152,0
  IKJ(16): 7,T_INDEX,0,0,0,2097152,0
ENDTYPE
TYPE T_DIM,1,2,12
  IJK(0): 2,0,0,0,0,2097152,0
  JIK(4): 2,0,0,0,0,2097152,0
  IKJ(8): 2,0,0,0,0,2097152,0
ENDTYPE
VAR GET_DINAM_MATRIX,1,0,,: 2,0,0,0,0,0,1
PROC D2COS,3,8,0: 8,0,0,0,262144,0,2
VAR J,3,0,,: 1,0,0,0,11,2097152,0
VAR I,3,0,,: 1,0,0,0,11,2097152,0
VAR K,3,0,,: 1,0,0,0,11,2097152,0
ENDPROC
PROC D2R,2,8,0: 8,0,0,0,393216,0,2
VAR I,3,0,,: 1,0,0,0,267,2097152,0
VAR J,3,0,,: 1,0,0,0,267,2097152,0
ENDPROC
PROC DR,4,8,0: 2,8,0,0,393345,0,2
VAR M,3,0,,: 1,0,0,0,267,2097152,0
VAR N,3,0,,: 1,0,0,0,267,2097152,0
VAR ALPHA,3,0,,: 1,2,0,0,267,2097152,0
VAR P,3,0,,: 1,0,0,0,267,2097152,0
ENDPROC
PROC DCOS_,3,8,0: 8,0,0,0,393216,0,2
VAR J,3,0,,: 1,0,0,0,11,2097152,0
VAR I,3,0,,: 1,0,0,0,11,2097152,0
VAR K,3,0,,: 1,0,0,0,11,2097152,0
ENDPROC
PROC COS_,3,8,0: 2,8,0,0,393345,0,2
VAR J,3,0,,: 1,0,0,0,11,2097152,0
VAR I,3,0,,: 1,0,0,0,11,2097152,0
VAR K,3,0,,: 1,0,0,0,11,2097152,0
ENDPROC
END
