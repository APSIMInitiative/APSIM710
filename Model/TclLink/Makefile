# Makefile for tcl component
# Uses 8.4.13 binaries from ActiveState distribution,
#      and Source distribution for include files
# These are stored in $(DISTDIR) and copied here the 1st time
# this makefile is called.

DISTDIR=c:\BuildLibraries\Tcl

# An ActiveX control (TclControl.dll) is also registered for ApsimUI. 
# Visual studio needs this to build the UI; ie you must build this before the UI.

# The dll in the apsim "Model" dir is a stub that loads a secondary dll in Model/TclLink
# The second dll will think it's in a standard tcl installation

include $(APSIM)/Model/Build/Platform.make

PROJECT = TclLink
PROJECTTYPE = dll

# ---------------------------------------------------------------------------
# TCL SECTION
# ---------------------------------------------------------------------------
VER=84
DOTVER=8.4
PATCH=13
TCLVER=$(DOTVER).$(PATCH)
TCLROOT=.\tcl$(TCLVER)\$(TMPDIRNAME)
TKROOT=.\tk$(TCLVER)\$(TMPDIRNAME)

SRC = TclStub.cpp 
LIBS = 

INCLUDEPATH = .\tcl$(TCLVER)\generic;.\tk$(TCLVER)\generic;.\tk$(TCLVER)\xlib

 # This is built before tcllink.dll
PREBUILD=dummy .\bin\TclComponent.dll 

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make

SRC2 = TclLink.cpp TclComponent.cpp TclXtras.cpp ..\ComponentInterface\EntryPoints.cpp
LIBS2 = General ApsimShared ComponentInterface Protocol tcl$(VER) tk$(VER)
SOURCEOBJS2:=	$(SRC2:.cpp=.obj) 
ifdef DEBUG
	LIBS2 := cg32.lib import32.lib $(LIBS2)
else
	LIBS2 :=  import32.lib $(LIBS2)
endif

LIBS2:= $(BCB)\Components\LibXml2\win32\LibXml2.lib $(BCB)\Components\Boost\Libs\FileSystem\FileSystem.lib $(BCB)\Components\Boost\libs\date_time\date_time.lib $(LIBS2) cw32mti.lib 


.\bin\TclComponent.dll: $(SOURCEOBJS2) tcl$(VER).lib tk$(VER).lib dummy
	$(LD) $(LFLAGS) -L$(LIBPATH) $(SOURCEOBJS2) $(SYSOBJS), .\bin\TclComponent.dll,, $(LIBS2), , $(RES)

# ---------------------------------------------------------------------------
# This bit sucks in external files into the build and install dirs
# ---------------------------------------------------------------------------
dummy:
# Sources
	@if not exist .\tcl$(TCLVER) xcopy $(DISTDIR)\tcl$(TCLVER) .\tcl$(TCLVER) /E /Y /I /Q
	@if not exist .\tk$(TCLVER) xcopy $(DISTDIR)\tk$(TCLVER) .\tk$(TCLVER) /E /Y /I /Q
# Binaries
	@if not exist $(APSIM)\Model\TclLink\bin\nul              mkdir $(APSIM)\Model\TclLink\bin
	@if not exist $(APSIM)\Model\TclLink\bin\tcl$(VER).dll    copy $(DISTDIR)\ASTcl\bin\tcl$(VER).dll    $(APSIM)\Model\TclLink\bin\tcl$(VER).dll
	@if not exist $(APSIM)\Model\TclLink\bin\zlib.dll         copy $(DISTDIR)\ASTcl\bin\zlib.dll         $(APSIM)\Model\TclLink\bin\zlib.dll 
	@if not exist $(APSIM)\Model\TclLink\bin\iconv.dll        copy $(DISTDIR)\ASTcl\bin\iconv.dll        $(APSIM)\Model\TclLink\bin\iconv.dll
	@if not exist $(APSIM)\Model\TclLink\bin\tclpip$(VER).dll copy $(DISTDIR)\ASTcl\bin\tclpip$(VER).dll $(APSIM)\Model\TclLink\bin\tclpip$(VER).dll
	@if not exist $(APSIM)\Model\TclLink\bin\tk$(VER).dll     copy $(DISTDIR)\ASTcl\bin\tk$(VER).dll     $(APSIM)\Model\TclLink\bin\tk$(VER).dll
	@if not exist $(APSIM)\Model\TclLink\bin\tclsh$(VER).exe  copy $(DISTDIR)\ASTcl\bin\tclsh$(VER).exe  $(APSIM)\Model\TclLink\bin\tclsh$(VER).exe
	@if not exist $(APSIM)\Model\TclLink\bin\wish$(VER).exe   copy $(DISTDIR)\ASTcl\bin\wish$(VER).exe   $(APSIM)\Model\TclLink\bin\wish$(VER).exe
	@if not exist $(APSIM)\Model\TclLink\lib\nul               mkdir $(APSIM)\Model\TclLink\lib
	@if not exist $(APSIM)\Model\TclLink\lib\tcl$(DOTVER)\nul  xcopy $(DISTDIR)\ASTcl\lib\tcl$(DOTVER) $(APSIM)\Model\TclLink\lib\tcl$(DOTVER) /E /Y /I /Q
	@if not exist $(APSIM)\Model\TclLink\lib\tk$(DOTVER)\nul   xcopy $(DISTDIR)\ASTcl\lib\tk$(DOTVER)  $(APSIM)\Model\TclLink\lib\tk$(DOTVER) /E /Y /I /Q
	@if not exist $(APSIM)\Model\TclLink\lib\tcllib1.8\nul     xcopy $(DISTDIR)\ASTcl\lib\tcllib1.8    $(APSIM)\Model\TclLink\lib\tcllib1.8 /E /Y /I /Q
	@if not exist $(APSIM)\Model\TclLink\lib\Img1.3\nul        xcopy $(DISTDIR)\ASTcl\lib\Img1.3       $(APSIM)\Model\TclLink\lib\Img1.3 /E /Y /I /Q
	@if not exist $(APSIM)\Model\TclLink\lib\tdom0.8.1\nul     xcopy $(DISTDIR)\ASTcl\lib\tdom0.8.1    $(APSIM)\Model\TclLink\lib\tdom0.8.1 /E /Y /I /Q
	@if not exist $(APSIM)\Model\TclLink\lib\tkcon\nul         xcopy $(DISTDIR)\tkcon                  $(APSIM)\Model\TclLink\lib\tkcon /E /Y /I /Q
	@if not exist $(APSIM)\Model\TclLink\lib\BWidget-1.8.0\nul xcopy $(DISTDIR)\BWidget-1.8.0          $(APSIM)\Model\TclLink\lib\BWidget-1.8.0 /E /Y /I /Q
	@if not exist $(APSIM)\Model\TclLink\lib\cdftcl\nul        xcopy $(DISTDIR)\cdftcl                 $(APSIM)\Model\TclLink\lib\cdftcl /E /Y /I /Q
# Do the ActiveX control
	@if not exist $(APSIM)\Model\TclLink\bin\TclControl$(VER).dll copy $(DISTDIR)\TclControl\install\TclControl$(VER).dll $(APSIM)\Model\TclLink\bin\TclControl$(VER).dll
	@$(APSIM)\Model\TclLink\bin\tclsh$(VER).exe  $(DISTDIR)\TclControl\install\install.tcl
# Finally the tcl license
	@if not exist $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms copy $(DISTDIR)\ASTcl\licenses\License.tcl $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms

# The distributed AS libs are in MS coff format. Convert them for borland tools.
tcl$(VER).lib: $(DISTDIR)\ASTcl\lib\tcl$(VER).lib
	$(BCB)\Bin\coff2omf $(DISTDIR)\ASTcl\lib\tcl$(VER).lib tcl$(VER).lib

tk$(VER).lib: $(DISTDIR)\ASTcl\lib\tk$(VER).lib
	$(BCB)\Bin\coff2omf $(DISTDIR)\ASTcl\lib\tk$(VER).lib tk$(VER).lib

clean:
	@if exist $(APSIM)\Model\TclLink\tcl$(DOTVER)\nul  rmdir /s /q $(APSIM)\Model\TclLink\tcl$(DOTVER)
	@if exist $(APSIM)\Model\TclLink\tk$(DOTVER)\nul   rmdir /s /q $(APSIM)\Model\TclLink\tk$(DOTVER)
	@if exist $(APSIM)\Model\TclLink\bin\nul rmdir /s /q $(APSIM)\Model\TclLink\bin
	@if exist $(APSIM)\Model\TclLink\lib\nul rmdir /s /q $(APSIM)\Model\TclLink\lib
	@if exist $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms del /f $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms

