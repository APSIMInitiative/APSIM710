# Makefile for tcl component
# Uses 8.4.13 binaries from ActiveState distribution,
#      and Source distribution for include files
# These are stored in $(DISTDIR) and copied here the 1st time
# this makefile is called.

DISTDIR=$(APSIM)\..\BuildLibraries\Tcl

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
 INCLUDES = /I "./tcl$(TCLVER)/generic" /I "./tk$(TCLVER)/generic" 
 LIBS = General ApsimShared ComponentInterface2 tclstub84
 OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.obj
endif

ifeq ($(PLATFORM),Linux)
 INCLUDES = -I /usr/include/tcl$(DOTVER)
 LIBS = General ApsimShared ComponentInterface2 tclstub8.4
 OBJS = $(APSIM)/Model/ComponentInterface2/CMPComponentEntryPoints.o
endif

SRC = TclLink.cpp TclComponent.cpp 

PREBUILD = dummy

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make


# ---------------------------------------------------------------------------
# This bit sucks in external files into the build and install dirs
# ---------------------------------------------------------------------------
dummy:
ifeq ($(PLATFORM),WIN32VS)
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
	@if not exist $(APSIM)\Model\TclLink\tclstub$(VER).lib     copy $(DISTDIR)\ASTcl\lib\tclstub$(VER).lib $(APSIM)\Model\TclLink\tclstub$(VER).lib
	@if not exist $(APSIM)\Model\TclLink\tk$(VER).lib         copy $(DISTDIR)\ASTcl\lib\tk$(VER).lib   $(APSIM)\Model\TclLink\tk$(VER).lib
# Finally the tcl license
	@if not exist $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms copy $(DISTDIR)\ASTcl\licenses\License.tcl $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms
else
	#if [ -s $(APSIM)/Model/TclLink/lib ]; then 

endif

clean:
ifeq ($(PLATFORM),WIN32VS)
	@if exist $(APSIM)\Model\TclLink\tcl$(DOTVER)\nul  rmdir /s /q $(APSIM)\Model\TclLink\tcl$(DOTVER)
	@if exist $(APSIM)\Model\TclLink\tk$(DOTVER)\nul   rmdir /s /q $(APSIM)\Model\TclLink\tk$(DOTVER)
	@if exist $(APSIM)\Model\TclLink\bin\nul rmdir /s /q $(APSIM)\Model\TclLink\bin
	@if exist $(APSIM)\Model\TclLink\lib\nul rmdir /s /q $(APSIM)\Model\TclLink\lib
	@if exist $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms del /f $(APSIM)\Model\TclLink\Tcl$(TCLVER).license.terms
endif




