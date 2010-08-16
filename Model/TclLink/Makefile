# Makefile for tcl component
# Uses 8.4.13 binaries from ActiveState distribution,
#      and Source distribution for include files
# These are stored in $(DISTDIR) and copied here the 1st time
# this makefile is called.

DISTDIR=$(APSIM)\..\BuildLibraries\Tcl

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

ifeq ($(PLATFORM),WIN32VS)
 SRC = TclStub.cpp
 LIBS =
 INCLUDES = /I "./tcl$(TCLVER)/generic" /I "./tk$(TCLVER)/generic" /I "./tk$(TCLVER)/xlib"

 # This is built before tcllink.dll
 PREBUILD=dummy .\bin\TclComponent.dll

 SRC2 = TclLink.cpp TclComponent.cpp TclXtras.cpp ../ComponentInterface2/EntryPoints.cpp
 SOURCEOBJS2:=	$(SRC2:.cpp=.obj)
 LIBS2 = ComponentInterface2 ApsimShared General tcl$(VER) tk$(VER)
 LIBS2 := $(foreach library,$(LIBS2),$(library).lib) libxml2.lib
endif

ifeq ($(PLATFORM),Linux)
 INCLUDES := -I /usr/include/tcl$(DOTVER)
 SRC = TclLink.cpp TclComponent.cpp TclXtras.cpp ../ComponentInterface2/CMPComponentEntryPoints.cpp
 LIBS = General ApsimShared ComponentInterface2 
 LDFLAGS := -ltcl -ltk
endif

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make


ifeq ($(PLATFORM),WIN32VS)

.\bin\TclComponent.dll: $(SOURCEOBJS2) tcl$(VER).lib tk$(VER).lib dummy
	echo $(LFLAGS) $(SOURCEOBJS2) $(SYSOBJS) $(LIBPATH) $(LIBS2) > .\bin\TclComponent.rsp
	$(LD) /OUT:"./bin/TclComponent.dll" @.\bin\TclComponent.rsp
	$(TCL) $(APSIM)/Model/Build/mashDllExports.tcl ./bin/TclComponent.dll > .\bin\TclComponent.def
	echo $(LFLAGS) $(SOURCEOBJS2) $(SYSOBJS) /DEF:./bin/TclComponent.def $(LIBPATH) $(LIBS2) > .\bin\TclComponent.rsp
	$(LD) /OUT:"./bin/TclComponent.dll" @.\bin\TclComponent.rsp

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
	@if not exist $(APSIM)\Model\TclLink\tcl$(VER).lib         copy $(DISTDIR)\ASTcl\lib\tcl$(VER).lib $(APSIM)\Model\TclLink\tcl$(VER).lib
	@if not exist $(APSIM)\Model\TclLink\tk$(VER).lib         copy $(DISTDIR)\ASTcl\lib\tk$(VER).lib   $(APSIM)\Model\TclLink\tk$(VER).lib
# Do the ActiveX control
	@if not exist $(APSIM)\Model\TclLink\bin\TclControl$(VER).dll copy $(DISTDIR)\TclControl\install\TclControl$(VER).dll $(APSIM)\Model\TclLink\bin\TclControl$(VER).dll
	@$(APSIM)\Model\TclLink\bin\tclsh$(VER).exe  $(DISTDIR)\TclControl\install\install.tcl
# Finally the tcl license
	@if not exist $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms copy $(DISTDIR)\ASTcl\licenses\License.tcl $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms
endif

clean:
	@if exist $(APSIM)\Model\TclLink\tcl$(DOTVER)\nul  rmdir /s /q $(APSIM)\Model\TclLink\tcl$(DOTVER)
	@if exist $(APSIM)\Model\TclLink\tk$(DOTVER)\nul   rmdir /s /q $(APSIM)\Model\TclLink\tk$(DOTVER)
	@if exist $(APSIM)\Model\TclLink\bin\nul rmdir /s /q $(APSIM)\Model\TclLink\bin
	@if exist $(APSIM)\Model\TclLink\lib\nul rmdir /s /q $(APSIM)\Model\TclLink\lib
	@if exist $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms del /f $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms

