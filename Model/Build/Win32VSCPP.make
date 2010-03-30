###########################################
# make file for MS VS2008
###########################################

######uncomment for debug everything###DEBUG=yes

CC="$(VSINSTALLDIR)\VC\bin\cl.exe"
LD="$(VSINSTALLDIR)\VC\bin\link.exe"

BOOST = $(APSIM)\..\BuildLibraries\boost_1_42_0-msvc
LIBXML = $(APSIM)\..\BuildLibraries\libxml2-2.7.6.win32
TCL = $(APSIM)\..\BuildLibraries\tcl\ASTcl\bin\tclsh84.exe

DEFINES := /D "__WIN32__" /D "WIN32" /D "_WINDOWS" /D "_USRDLL" /D "BOOST_REGEX_STATIC_LINK"
INCLUDES := /I $(APSIM)\Model /I $(BOOST) /I $(LIBXML)\include $(INCLUDES)
LIBPATH := /LIBPATH:$(APSIM)\Model /LIBPATH:$(LIBXML)\lib 

# add .lib to all user libraries
LIBS := $(foreach library,$(LIBS),$(library).lib) libxml2.lib 

WARNINGS := /wd4996 /wd4068 /wd4290 /wd4251 /wd4244

CFLAGS := $(INCLUDES) $(DEFINES) /EHsc /W3 /nologo /c /TP /Fd"$(APSIM)\Model\$(PROJECT).pdb"

LFLAGS := /NOLOGO /SUBSYSTEM:CONSOLE /DYNAMICBASE /NXCOMPAT /MACHINE:X86

SYSOBJS := kernel32.lib user32.lib advapi32.lib shell32.lib uuid.lib 

ifeq ($(PROJECTTYPE),exe)
	CFLAGS := $(CFLAGS) /LD
	LFLAGS := $(LFLAGS) 
else
	CFLAGS := $(CFLAGS) /D "$(PROJECT)_EXPORTS"
	LFLAGS := $(LFLAGS) /DLL 
endif

# Optimisation and debug symbols.
ifdef DEBUG
	CFLAGS := $(CFLAGS) /Od /RTCs /ZI /MDd /D "_DEBUG" 
#	LIBS := cg32.lib import32.lib $(LIBS)
#	LFLAGS := $(LFLAGS) 
	LIBPATH := $(LIBPATH) \
/LIBPATH:$(BOOST)\bin.v2\libs\filesystem\build\msvc-9.0\debug\link-static\threading-multi \
/LIBPATH:$(BOOST)\bin.v2\libs\system\build\msvc-9.0\debug\link-static\threading-multi \
/LIBPATH:$(BOOST)\bin.v2\libs\regex\build\msvc-9.0\debug\link-static\threading-multi \
/LIBPATH:$(BOOST)\bin.v2\libs\date_time\build\msvc-9.0\debug\link-static\threading-multi 
else
	CFLAGS := $(CFLAGS) /O2 /MD /GL
#	LFLAGS := $(LFLAGS) 
#	LIBS :=  import32.lib $(LIBS)
	LIBPATH := $(LIBPATH) \
/LIBPATH:$(BOOST)\bin.v2\libs\filesystem\build\msvc-9.0\release\link-static\threading-multi \
/LIBPATH:$(BOOST)\bin.v2\libs\system\build\msvc-9.0\release\link-static\threading-multi \
/LIBPATH:$(BOOST)\bin.v2\libs\regex\build\msvc-9.0\release\link-static\threading-multi \
/LIBPATH:$(BOOST)\bin.v2\libs\date_time\build\msvc-9.0\release\link-static\threading-multi 
endif

# The rules
%.obj:	%.cpp
	$(CC) $(CFLAGS) $(WARNINGS) /Fo"$(dir $<)" $<

SOURCEOBJS:= $(SRC:.cpp=.obj)

ifeq ($(PROJECTTYPE),exe)

$(PROJECT).exe: $(PREBUILD) $(SOURCEOBJS)
	echo $(LFLAGS) $(SOURCEOBJS) $(SYSOBJS) $(LIBPATH) $(LIBS) > $(PROJECT).rsp
	$(LD) /OUT:"$(APSIM)\Model\$(PROJECT).exe" @$(PROJECT).rsp

else

# Ugh. By default, MS dlls export symbols with _@ adornments, eg function "xyz" appears as "_xyz@n"
#   what we do here is build the dll once, generate an unadorned .def file, and build it again with 
#   that def file so that the unadorned names are visible.
$(PROJECT).dll: $(PREBUILD) $(SOURCEOBJS)
	echo $(LFLAGS) $(SOURCEOBJS) $(SYSOBJS) $(LIBPATH) $(LIBS) > $(PROJECT).rsp
	$(LD) /OUT:"$(APSIM)\Model\$(PROJECT).dll" @$(PROJECT).rsp
	$(TCL) $(APSIM)/Model/Build/mashDllExports.tcl $(APSIM)/Model/$(PROJECT).dll > $(PROJECT).def
	echo $(LFLAGS) $(SOURCEOBJS) $(SYSOBJS) /DEF:$(PROJECT).def $(LIBPATH) $(LIBS) > $(PROJECT).rsp
	$(LD) /OUT:"$(APSIM)\Model\$(PROJECT).dll" @$(PROJECT).rsp

endif

