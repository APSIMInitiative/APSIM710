###########################################
# Compiler make file for MS VS2008
#
# The user make file can have the following
# defines:
#    DEFINES
#    INCLUDEPATH
#    LIBPATH
#    LIBS
#    OBJS
#    PROJECTTYPE = exe, libdll or dll
#    DEBUG
#    PRECOMPILEDHEADERS
#    PREBUILD  - a target built before dll (eg custom libs)
#
# NB: It is vital that paths passed to
#     bcc and ilink use '\' character.
#     Doesn't work with '/' characters.
###########################################
#DEBUG=yes
CC="$(VSINSTALLDIR)\VC\bin\cl.exe"
LD="$(VSINSTALLDIR)\VC\bin\link.exe"

BOOST = C:\BuildLibraries\boost_1_42_0
LIBXML = C:\BuildLibraries\libxml2-2.7.6

# add .lib to all user libraries

DEFINES := /D "__WIN32__" /D "WIN32" /D "_WINDOWS" /D "_USRDLL" /D "GENERAL_EXPORTS" /D "BOOST_REGEX_STATIC_LINK"
INCLUDES := /I $(APSIM)\Model /I $(BOOST) /I $(LIBXML)\include
LIBPATH := /LIBPATH:$(APSIM)\Model /LIBPATH:$(LIBXML)\lib \
/LIBPATH:$(BOOST)\bin.v2\libs\filesystem\build\msvc-9.0\debug\link-static\threading-multi \
/LIBPATH:$(BOOST)\bin.v2\libs\system\build\msvc-9.0\debug\link-static\threading-multi \
/LIBPATH:$(BOOST)\bin.v2\libs\regex\build\msvc-9.0\debug\link-static\threading-multi \
/LIBPATH:$(BOOST)\bin.v2\libs\date_time\build\msvc-9.0\debug\link-static\threading-multi 

LIBS := $(foreach library,$(LIBS),$(library).lib) libxml2.lib 

#libboost_filesystem-vc90-mt-1_42.lib libboost_date_time-vc90-mt-1_42.lib 

WARNINGS := /wd4996

CFLAGS := /Od $(INCLUDES) $(DEFINES) /Gm /EHsc /RTC1 /MDd /W3 /nologo /c /ZI /TP

# /Fo"Debug\\" /Fd"Debug\vc90.pdb" 

LFLAGS := /INCREMENTAL /NOLOGO /SUBSYSTEM:WINDOWS /DYNAMICBASE /NXCOMPAT /MACHINE:X86

## gdi32.lib winspool.lib comdlg32.lib odbc32.lib odbccp32.lib ole32.lib oleaut32.lib 

SYSOBJS := kernel32.lib user32.lib advapi32.lib shell32.lib uuid.lib 

ifeq ($(PROJECTTYPE),exe)
   LFLAGS := $(LFLAGS) 
else
   LFLAGS := $(LFLAGS) /DLL
endif

# Optimisation and debug symbols.
#ifdef DEBUG
#	CFLAGS := $(CFLAGS) /D "_DEBUG" 
#	LIBS := cg32.lib import32.lib $(LIBS)
#	LFLAGS := $(LFLAGS) -v
#else
#	CFLAGS := $(CFLAGS) -O2 -w-inl
#	LIBS :=  import32.lib $(LIBS)
#endif

   ifeq ($(PROJECTTYPE),exe)
      SYSOBJS := $(SYSOBJS) 
      CFLAGS := $(CFLAGS) 
      LFLAGS := $(LFLAGS) 
   else
      CFLAGS := $(CFLAGS) 
      LFLAGS := $(LFLAGS) # /MANIFEST /MANIFESTFILE:"$(APSIM)\Model\$(PROJECT).dll.manifest"
      SYSOBJS := $(SYSOBJS) 
   endif
   LIBS := $(LIBS)

# The rules
%.obj:	%.cpp
	$(CC) $(CFLAGS) $(WARNINGS) $<

# Change forward slashes to back slashes.
SRC :=	$(subst /,\,$(SRC))

SOURCEOBJS:=	$(SRC:.cpp=.obj)

# remove all paths on OBJ files.
#OBJSNODIR := $(foreach obj,$(SOURCEOBJS),$(notdir $(obj)))


ifeq ($(PROJECTTYPE),exe)
$(PROJECT).exe: $(PREBUILD) $(SOURCEOBJS)
	$(LD) /OUT:"$(APSIM)\Model\$(PROJECT).exe" $(LFLAGS) $(SOURCEOBJS) $(SYSOBJS) $(LIBPATH) $(LIBS)
else
$(PROJECT).dll: $(PREBUILD) $(SOURCEOBJS)
	$(LD) /OUT:"$(APSIM)\Model\$(PROJECT).dll" $(LFLAGS) $(SOURCEOBJS) $(SYSOBJS) $(LIBPATH) $(LIBS)
endif

