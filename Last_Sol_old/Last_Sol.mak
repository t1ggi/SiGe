# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=Last_Sol - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Last_Sol - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Last_Sol - Win32 Release" && "$(CFG)" !=\
 "Last_Sol - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "Last_Sol.mak" CFG="Last_Sol - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Last_Sol - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "Last_Sol - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "Last_Sol - Win32 Debug"
F90=fl32.exe
RSC=rc.exe

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\Last_Sol.exe" "$(OUTDIR)\MDDYNAMSTW.MOD"\
 "$(OUTDIR)\Last_Sol.bsc"

CLEAN : 
	-@erase ".\Release\MDDYNAMSTW.MOD"
	-@erase ".\Release/MDTYPES.mod"
	-@erase ".\Release/MDSTRUCT.mod"
	-@erase ".\Release/MDCONSTS.mod"
	-@erase ".\Release\mdbounds.mod"
	-@erase ".\Release\Last_Sol.exe"
	-@erase ".\Release\BOUNDS.OBJ"
	-@erase ".\Release\RAMAN.OBJ"
	-@erase ".\Release\POTENTIALS.OBJ"
	-@erase ".\Release\MAIN.OBJ"
	-@erase ".\Release/MDPOTENTS.mod"
	-@erase ".\Release/MDMATRIX_TRANSFS.mod"
	-@erase ".\Release/MDRAMAN_SPECTRES.mod"
	-@erase ".\Release/MDDYNAM_TSF.mod"
	-@erase ".\Release/MDDYNAM_STW.mod"
	-@erase ".\Release\CONSTS.OBJ"
	-@erase ".\Release\MATR_OPERS.OBJ"
	-@erase ".\Release\RELAX.OBJ"
	-@erase ".\Release\RELAX_SUB.OBJ"
	-@erase ".\Release\mdrelax.mod"
	-@erase ".\Release\DINAM.OBJ"
	-@erase ".\Release\STRUCT.OBJ"
	-@erase ".\Release\DINAM_TSF.OBJ"
	-@erase ".\Release\TYPES.OBJ"
	-@erase ".\Release\DINAM_STW.OBJ"
	-@erase ".\Release\Last_Sol.bsc"
	-@erase ".\Release\TYPES.SBR"
	-@erase ".\Release\STRUCT.SBR"
	-@erase ".\Release\RELAX_SUB.SBR"
	-@erase ".\Release\RELAX.SBR"
	-@erase ".\Release\RAMAN.SBR"
	-@erase ".\Release\POTENTIALS.SBR"
	-@erase ".\Release\MATR_OPERS.SBR"
	-@erase ".\Release\MAIN.SBR"
	-@erase ".\Release\DINAM_TSF.SBR"
	-@erase ".\Release\DINAM_STW.SBR"
	-@erase ".\Release\DINAM.SBR"
	-@erase ".\Release\CONSTS.SBR"
	-@erase ".\Release\BOUNDS.SBR"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "Release/" /c /nologo
# ADD F90 /Ox /FR /I "Release/" /c /nologo
F90_PROJ=/Ox /FR"Release/" /I "Release/" /c /nologo /Fo"Release/" 
F90_OBJS=.\Release/
# ADD BASE RSC /l 0x419 /d "NDEBUG"
# ADD RSC /l 0x419 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Last_Sol.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/TYPES.SBR" \
	"$(INTDIR)/STRUCT.SBR" \
	"$(INTDIR)/RELAX_SUB.SBR" \
	"$(INTDIR)/RELAX.SBR" \
	"$(INTDIR)/RAMAN.SBR" \
	"$(INTDIR)/POTENTIALS.SBR" \
	"$(INTDIR)/MATR_OPERS.SBR" \
	"$(INTDIR)/MAIN.SBR" \
	"$(INTDIR)/DINAM_TSF.SBR" \
	"$(INTDIR)/DINAM_STW.SBR" \
	"$(INTDIR)/DINAM.SBR" \
	"$(INTDIR)/CONSTS.SBR" \
	"$(INTDIR)/BOUNDS.SBR"

"$(OUTDIR)\Last_Sol.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/Last_Sol.pdb" /machine:I386 /out:"$(OUTDIR)/Last_Sol.exe" 
LINK32_OBJS= \
	"$(INTDIR)/BOUNDS.OBJ" \
	"$(INTDIR)/RAMAN.OBJ" \
	"$(INTDIR)/POTENTIALS.OBJ" \
	"$(INTDIR)/MAIN.OBJ" \
	"$(INTDIR)/CONSTS.OBJ" \
	"$(INTDIR)/MATR_OPERS.OBJ" \
	"$(INTDIR)/RELAX.OBJ" \
	"$(INTDIR)/RELAX_SUB.OBJ" \
	"$(INTDIR)/DINAM.OBJ" \
	"$(INTDIR)/STRUCT.OBJ" \
	"$(INTDIR)/DINAM_TSF.OBJ" \
	"$(INTDIR)/TYPES.OBJ" \
	"$(INTDIR)/DINAM_STW.OBJ"

"$(OUTDIR)\Last_Sol.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\Last_Sol.exe" "$(OUTDIR)\MDDYNAMSTW.MOD"\
 "$(OUTDIR)\Last_Sol.bsc"

CLEAN : 
	-@erase ".\Debug\MDDYNAMSTW.MOD"
	-@erase ".\Debug/MDTYPES.mod"
	-@erase ".\Debug/MDSTRUCT.mod"
	-@erase ".\Debug/MDCONSTS.mod"
	-@erase ".\Debug\mdbounds.mod"
	-@erase ".\Debug\Last_Sol.bsc"
	-@erase ".\Debug\RELAX_SUB.SBR"
	-@erase ".\Debug\mdrelax.mod"
	-@erase ".\Debug/MDPOTENTS.mod"
	-@erase ".\Debug\TYPES.SBR"
	-@erase ".\Debug\RELAX.SBR"
	-@erase ".\Debug\MAIN.SBR"
	-@erase ".\Debug/MDMATRIX_TRANSFS.mod"
	-@erase ".\Debug/MDRAMAN_SPECTRES.mod"
	-@erase ".\Debug/MDDYNAM_TSF.mod"
	-@erase ".\Debug/MDDYNAM_STW.mod"
	-@erase ".\Debug\DINAM_TSF.SBR"
	-@erase ".\Debug\DINAM.SBR"
	-@erase ".\Debug\BOUNDS.SBR"
	-@erase ".\Debug\CONSTS.SBR"
	-@erase ".\Debug\RAMAN.SBR"
	-@erase ".\Debug\DINAM_STW.SBR"
	-@erase ".\Debug\POTENTIALS.SBR"
	-@erase ".\Debug\STRUCT.SBR"
	-@erase ".\Debug\MATR_OPERS.SBR"
	-@erase ".\Debug\Last_Sol.exe"
	-@erase ".\Debug\POTENTIALS.OBJ"
	-@erase ".\Debug\STRUCT.OBJ"
	-@erase ".\Debug\MATR_OPERS.OBJ"
	-@erase ".\Debug\RELAX_SUB.OBJ"
	-@erase ".\Debug\TYPES.OBJ"
	-@erase ".\Debug\RELAX.OBJ"
	-@erase ".\Debug\MAIN.OBJ"
	-@erase ".\Debug\DINAM_TSF.OBJ"
	-@erase ".\Debug\DINAM.OBJ"
	-@erase ".\Debug\BOUNDS.OBJ"
	-@erase ".\Debug\CONSTS.OBJ"
	-@erase ".\Debug\RAMAN.OBJ"
	-@erase ".\Debug\DINAM_STW.OBJ"
	-@erase ".\Debug\Last_Sol.ilk"
	-@erase ".\Debug\Last_Sol.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "Debug/" /c /nologo
# ADD F90 /FR /Zi /I "Debug/" /c /nologo
F90_PROJ=/FR"Debug/" /Zi /I "Debug/" /c /nologo /Fo"Debug/"\
 /Fd"Debug/Last_Sol.pdb" 
F90_OBJS=.\Debug/
# ADD BASE RSC /l 0x419 /d "_DEBUG"
# ADD RSC /l 0x419 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Last_Sol.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/RELAX_SUB.SBR" \
	"$(INTDIR)/TYPES.SBR" \
	"$(INTDIR)/RELAX.SBR" \
	"$(INTDIR)/MAIN.SBR" \
	"$(INTDIR)/DINAM_TSF.SBR" \
	"$(INTDIR)/DINAM.SBR" \
	"$(INTDIR)/BOUNDS.SBR" \
	"$(INTDIR)/CONSTS.SBR" \
	"$(INTDIR)/RAMAN.SBR" \
	"$(INTDIR)/DINAM_STW.SBR" \
	"$(INTDIR)/POTENTIALS.SBR" \
	"$(INTDIR)/STRUCT.SBR" \
	"$(INTDIR)/MATR_OPERS.SBR"

"$(OUTDIR)\Last_Sol.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/Last_Sol.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/Last_Sol.exe" 
LINK32_OBJS= \
	"$(INTDIR)/POTENTIALS.OBJ" \
	"$(INTDIR)/STRUCT.OBJ" \
	"$(INTDIR)/MATR_OPERS.OBJ" \
	"$(INTDIR)/RELAX_SUB.OBJ" \
	"$(INTDIR)/TYPES.OBJ" \
	"$(INTDIR)/RELAX.OBJ" \
	"$(INTDIR)/MAIN.OBJ" \
	"$(INTDIR)/DINAM_TSF.OBJ" \
	"$(INTDIR)/DINAM.OBJ" \
	"$(INTDIR)/BOUNDS.OBJ" \
	"$(INTDIR)/CONSTS.OBJ" \
	"$(INTDIR)/RAMAN.OBJ" \
	"$(INTDIR)/DINAM_STW.OBJ"

"$(OUTDIR)\Last_Sol.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "Last_Sol - Win32 Release"
# Name "Last_Sol - Win32 Debug"

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\TYPES.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

F90_MODOUT=\
	"MDTYPES"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\TYPES.OBJ" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\TYPES.SBR" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\mdtypes.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

F90_MODOUT=\
	"MDTYPES"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\TYPES.OBJ" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\TYPES.SBR" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\mdtypes.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\STRUCT.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

NODEP_F90_STRUC=\
	".\Release/MDTYPES.mod"\
	".\Release/MDCONSTS.mod"\
	".\Release\mdbounds.mod"\
	
F90_MODOUT=\
	"MDSTRUCT"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\STRUCT.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\STRUCT.SBR" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mdstruct.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_STRUC=\
	".\Debug/MDTYPES.mod"\
	".\Debug/MDCONSTS.mod"\
	".\Debug\mdbounds.mod"\
	
F90_MODOUT=\
	"MDSTRUCT"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\STRUCT.OBJ" : $(SOURCE) $(DEP_F90_STRUC) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\STRUCT.SBR" : $(SOURCE) $(DEP_F90_STRUC) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mdstruct.mod" : $(SOURCE) $(DEP_F90_STRUC) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\RELAX_SUB.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

NODEP_F90_RELAX=\
	".\Release/MDTYPES.mod"\
	".\Release/MDCONSTS.mod"\
	".\Release\mdbounds.mod"\
	".\Release/MDSTRUCT.mod"\
	".\Release\mdrelax.mod"\
	

"$(INTDIR)\RELAX_SUB.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdrelax.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"

"$(INTDIR)\RELAX_SUB.SBR" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdrelax.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"


!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_RELAX=\
	".\Debug/MDTYPES.mod"\
	".\Debug/MDCONSTS.mod"\
	".\Debug\mdbounds.mod"\
	".\Debug/MDSTRUCT.mod"\
	".\Debug\mdrelax.mod"\
	

"$(INTDIR)\RELAX_SUB.OBJ" : $(SOURCE) $(DEP_F90_RELAX) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdrelax.mod"\
 "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"

"$(INTDIR)\RELAX_SUB.SBR" : $(SOURCE) $(DEP_F90_RELAX) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdrelax.mod"\
 "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\RELAX.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

NODEP_F90_RELAX_=\
	".\Release/MDTYPES.mod"\
	".\Release/MDCONSTS.mod"\
	".\Release/MDPOTENTS.mod"\
	".\Release\mdbounds.mod"\
	
F90_MODOUT=\
	"MDRELAX"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\RELAX.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdpotents.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\RELAX.SBR" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdpotents.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mdrelax.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdpotents.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_RELAX_=\
	".\Debug/MDTYPES.mod"\
	".\Debug/MDCONSTS.mod"\
	".\Debug/MDPOTENTS.mod"\
	".\Debug\mdbounds.mod"\
	
F90_MODOUT=\
	"MDRELAX"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\RELAX.OBJ" : $(SOURCE) $(DEP_F90_RELAX_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdpotents.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\RELAX.SBR" : $(SOURCE) $(DEP_F90_RELAX_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdpotents.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mdrelax.mod" : $(SOURCE) $(DEP_F90_RELAX_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdpotents.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\RAMAN.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

NODEP_F90_RAMAN=\
	".\Release/MDCONSTS.mod"\
	".\Release/MDTYPES.mod"\
	".\Release\mdbounds.mod"\
	".\Release/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDRAMAN_SPECTRES"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\RAMAN.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\RAMAN.SBR" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mdraman_spectres.mod" : $(SOURCE) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_RAMAN=\
	".\Debug/MDCONSTS.mod"\
	".\Debug/MDTYPES.mod"\
	".\Debug\mdbounds.mod"\
	".\Debug/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDRAMAN_SPECTRES"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\RAMAN.OBJ" : $(SOURCE) $(DEP_F90_RAMAN) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\RAMAN.SBR" : $(SOURCE) $(DEP_F90_RAMAN) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mdraman_spectres.mod" : $(SOURCE) $(DEP_F90_RAMAN) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\POTENTIALS.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

NODEP_F90_POTEN=\
	".\Release/MDTYPES.mod"\
	".\Release/MDCONSTS.mod"\
	".\Release/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDPOTENTS"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\POTENTIALS.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

"$(INTDIR)\POTENTIALS.SBR" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

"$(INTDIR)\mdpotents.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_POTEN=\
	".\Debug/MDTYPES.mod"\
	".\Debug/MDCONSTS.mod"\
	".\Debug/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDPOTENTS"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\POTENTIALS.OBJ" : $(SOURCE) $(DEP_F90_POTEN) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

"$(INTDIR)\POTENTIALS.SBR" : $(SOURCE) $(DEP_F90_POTEN) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

"$(INTDIR)\mdpotents.mod" : $(SOURCE) $(DEP_F90_POTEN) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\MATR_OPERS.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

NODEP_F90_MATR_=\
	".\Release/MDCONSTS.mod"\
	".\Release/MDTYPES.mod"\
	".\Release\mdbounds.mod"\
	".\Release/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDMATRIX_TRANSFS"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\MATR_OPERS.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\MATR_OPERS.SBR" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mdmatrix_transfs.mod" : $(SOURCE) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_MATR_=\
	".\Debug/MDCONSTS.mod"\
	".\Debug/MDTYPES.mod"\
	".\Debug\mdbounds.mod"\
	".\Debug/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDMATRIX_TRANSFS"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\MATR_OPERS.OBJ" : $(SOURCE) $(DEP_F90_MATR_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\MATR_OPERS.SBR" : $(SOURCE) $(DEP_F90_MATR_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mdmatrix_transfs.mod" : $(SOURCE) $(DEP_F90_MATR_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\MAIN.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

DEP_F90_MAIN_=\
	".\Release/MDTYPES.mod"\
	".\Release/MDCONSTS.mod"\
	".\Release/MDSTRUCT.mod"\
	".\Release/MDPOTENTS.mod"\
	".\Release/MDDYNAM_TSF.mod"\
	".\Release/MDDYNAM_STW.mod"\
	".\Release/MDMATRIX_TRANSFS.mod"\
	".\Release/MDRAMAN_SPECTRES.mod"\
	

"$(INTDIR)\MAIN.OBJ" : $(SOURCE) $(DEP_F90_MAIN_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdpotents.mod"\
 "$(INTDIR)\mdmatrix_transfs.mod" "$(INTDIR)\mdraman_spectres.mod"\
 "$(INTDIR)\mddynam_tsf.mod" "$(INTDIR)\mddynam_stw.mod"\
 "$(INTDIR)\mdconsts.mod"

"$(INTDIR)\MAIN.SBR" : $(SOURCE) $(DEP_F90_MAIN_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdpotents.mod"\
 "$(INTDIR)\mdmatrix_transfs.mod" "$(INTDIR)\mdraman_spectres.mod"\
 "$(INTDIR)\mddynam_tsf.mod" "$(INTDIR)\mddynam_stw.mod"\
 "$(INTDIR)\mdconsts.mod"


!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_MAIN_=\
	".\Debug/MDTYPES.mod"\
	".\Debug/MDCONSTS.mod"\
	".\Debug/MDSTRUCT.mod"\
	".\Debug/MDPOTENTS.mod"\
	".\Debug/MDDYNAM_TSF.mod"\
	".\Debug/MDDYNAM_STW.mod"\
	".\Debug/MDMATRIX_TRANSFS.mod"\
	".\Debug/MDRAMAN_SPECTRES.mod"\
	

"$(INTDIR)\MAIN.OBJ" : $(SOURCE) $(DEP_F90_MAIN_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdpotents.mod"\
 "$(INTDIR)\mdmatrix_transfs.mod" "$(INTDIR)\mdraman_spectres.mod"\
 "$(INTDIR)\mddynam_tsf.mod" "$(INTDIR)\mddynam_stw.mod"\
 "$(INTDIR)\mdconsts.mod"

"$(INTDIR)\MAIN.SBR" : $(SOURCE) $(DEP_F90_MAIN_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdpotents.mod"\
 "$(INTDIR)\mdmatrix_transfs.mod" "$(INTDIR)\mdraman_spectres.mod"\
 "$(INTDIR)\mddynam_tsf.mod" "$(INTDIR)\mddynam_stw.mod"\
 "$(INTDIR)\mdconsts.mod"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\DINAM_TSF.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

NODEP_F90_DINAM=\
	".\Release/MDTYPES.mod"\
	".\Release/MDCONSTS.mod"\
	".\Release\mdbounds.mod"\
	".\Release/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDDYNAM_TSF"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\DINAM_TSF.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\DINAM_TSF.SBR" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mddynam_tsf.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_DINAM=\
	".\Debug/MDTYPES.mod"\
	".\Debug/MDCONSTS.mod"\
	".\Debug\mdbounds.mod"\
	".\Debug/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDDYNAM_TSF"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\DINAM_TSF.OBJ" : $(SOURCE) $(DEP_F90_DINAM) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\DINAM_TSF.SBR" : $(SOURCE) $(DEP_F90_DINAM) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mddynam_tsf.mod" : $(SOURCE) $(DEP_F90_DINAM) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\DINAM_STW.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

NODEP_F90_DINAM_=\
	".\Release/MDTYPES.mod"\
	".\Release/MDCONSTS.mod"\
	".\Release\mdbounds.mod"\
	".\Release/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDDYNAM_STW"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\DINAM_STW.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\DINAM_STW.SBR" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mddynam_stw.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_DINAM_=\
	".\Debug/MDTYPES.mod"\
	".\Debug/MDCONSTS.mod"\
	".\Debug\mdbounds.mod"\
	".\Debug/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDDYNAM_STW"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\DINAM_STW.OBJ" : $(SOURCE) $(DEP_F90_DINAM_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\DINAM_STW.SBR" : $(SOURCE) $(DEP_F90_DINAM_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\mddynam_stw.mod" : $(SOURCE) $(DEP_F90_DINAM_) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\DINAM.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

NODEP_F90_DINAM_F=\
	".\Release/MDTYPES.mod"\
	".\Release/MDCONSTS.mod"\
	".\Release\mdbounds.mod"\
	".\Release/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDDYNAMSTW"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\DINAM.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\DINAM.SBR" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\MDDYNAMSTW.MOD" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod" "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_DINAM_F=\
	".\Debug/MDTYPES.mod"\
	".\Debug/MDCONSTS.mod"\
	".\Debug\mdbounds.mod"\
	".\Debug/MDSTRUCT.mod"\
	
F90_MODOUT=\
	"MDDYNAMSTW"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\DINAM.OBJ" : $(SOURCE) $(DEP_F90_DINAM_F) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\DINAM.SBR" : $(SOURCE) $(DEP_F90_DINAM_F) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

"$(INTDIR)\MDDYNAMSTW.MOD" : $(SOURCE) $(DEP_F90_DINAM_F) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdstruct.mod" "$(INTDIR)\mdconsts.mod"\
 "$(INTDIR)\mdbounds.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\CONSTS.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

F90_MODOUT=\
	"MDCONSTS"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\CONSTS.OBJ" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\CONSTS.SBR" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\mdconsts.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

F90_MODOUT=\
	"MDCONSTS"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\CONSTS.OBJ" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\CONSTS.SBR" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\mdconsts.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\BOUNDS.FOR

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

NODEP_F90_BOUND=\
	".\Release/MDTYPES.mod"\
	".\Release/MDCONSTS.mod"\
	
F90_MODOUT=\
	"MDBOUNDS"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\BOUNDS.OBJ" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

"$(INTDIR)\BOUNDS.SBR" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

"$(INTDIR)\mdbounds.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mdtypes.mod"\
 "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

DEP_F90_BOUND=\
	".\Debug/MDTYPES.mod"\
	".\Debug/MDCONSTS.mod"\
	
F90_MODOUT=\
	"MDBOUNDS"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\BOUNDS.OBJ" : $(SOURCE) $(DEP_F90_BOUND) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

"$(INTDIR)\BOUNDS.SBR" : $(SOURCE) $(DEP_F90_BOUND) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

"$(INTDIR)\mdbounds.mod" : $(SOURCE) $(DEP_F90_BOUND) "$(INTDIR)"\
 "$(INTDIR)\mdtypes.mod" "$(INTDIR)\mdconsts.mod"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RELAX_TYPES.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RELAX_PARAMS.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RELAX_NUMS.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RELAX_NEIBRS.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RELAX_LEVELS.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RELAX_LAYS.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RELAX_COORDS.xls

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RELAX_COORDS.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RELAX.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RAMAN_SPECTRE_si_cub_prdc.xls

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_RAMAN_SPECTRE.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_PHONON_SPECTRE.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_OWN_VECTS.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_DINAM_MATRIX.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_BONDS_LENGTHS.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_BOND_ENERGIES.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\_ANGLES.TXT

!IF  "$(CFG)" == "Last_Sol - Win32 Release"

!ELSEIF  "$(CFG)" == "Last_Sol - Win32 Debug"

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
