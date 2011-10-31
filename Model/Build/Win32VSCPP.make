###########################################
# make file for MS VS2010
###########################################

######uncomment for debug everything###

###DEBUG=yes

CC="$(VSINSTALLDIR)\VC\bin\cl.exe"
LD="$(VSINSTALLDIR)\VC\bin\link.exe"
MT="$(WindowsSdkDir)\bin\mt.exe"
RC=rc

BOOST = $(APSIM)\..\BuildLibraries\boost_1_44
LIBXML = $(APSIM)\..\BuildLibraries\libxml2-2.7.7.win32
ICONV = $(APSIM)\..\BuildLibraries\libiconv-1.11.1.win32
TCL = $(APSIM)\Model\TclLink\bin\tclsh85.exe

DEFINES := /D "__WIN32__" /D "WIN32" /D "_WINDOWS" /D "_USRDLL" /D "BOOST_REGEX_STATIC_LINK" $(DEFINES)
INCLUDES := /I $(APSIM)\Model /I $(BOOST) /I $(LIBXML)\include /I $(ICONV)\include /I $(APSIM)\Model\$(PROJECT) $(INCLUDES) 
LIBPATH := /LIBPATH:$(APSIM)\Model /LIBPATH:$(LIBXML)\lib /LIBPATH:$(ICONV)\lib 

# add .lib to all user libraries
LIBS := $(foreach library,$(LIBS),$(library).lib) libxml2.lib

WARNINGS := /wd4996 /wd4068 /wd4290 /wd4251 /wd4244

CFLAGS := $(INCLUDES) $(DEFINES) /EHsc /W3 /nologo /c /TP /Fd"$(APSIM)\Model\$(PROJECT).pdb"

LFLAGS := /NOLOGO /SUBSYSTEM:CONSOLE /DYNAMICBASE /NXCOMPAT /MACHINE:X86 /LTCG

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
	LFLAGS := $(LFLAGS) /DEBUG /PDB:"$(APSIM)\Model\$(PROJECT).pdb"
	LIBPATH := $(LIBPATH) /LIBPATH:$(BOOST)\lib
else
	CFLAGS := $(CFLAGS) /O2 /MD /GL
#	LFLAGS := $(LFLAGS)
#	LIBS :=  import32.lib $(LIBS)
	LIBPATH := $(LIBPATH) /LIBPATH:$(BOOST)\lib
endif

ifdef PRECOMPILE
	CFLAGS_NOPCH := $(CFLAGS)
    PRECOMPILEDHEADERS = $(PRECOMPILE:%.h=%.pch)
	CFLAGS := $(CFLAGS) /Yu"$(PRECOMPILE)" /D"PRECOMPILE=""$(PRECOMPILE)"""  /Fp"$(APSIM)\Model\$(PROJECT)\$(PRECOMPILEDHEADERS)"
endif

# The rules
%.obj:	%.cpp
	$(CC) $(CFLAGS) $(WARNINGS) /Fo"$(dir $<)" $<

SOURCEOBJS:= $(SRC:.cpp=.obj)
OBJSSRC:=$(OBJS:.obj=.cpp)

# Normally these are obtained as environment variables, but we want to be sure they are not left undefined
ifeq ($(MAJOR_VERSION),)
  MAJOR_VERSION = 1
  MINOR_VERSION = 0
  BUILD_NUMBER = 0
endif

ifeq ($(PROJECTTYPE),exe)
#### EXEs
RESOBJ = exeres.res
all: $(PRECOMPILEDHEADERS) $(RESOBJ) $(PROJECT).exe
$(PROJECT).exe: $(PREBUILD) $(SOURCEOBJS) $(RESOBJ)
	echo $(LFLAGS) $(SOURCEOBJS) $(SYSOBJS) $(RESOBJ) $(LIBPATH) $(LIBS) > $(PROJECT).rsp
	$(LD) /OUT:"$(APSIM)\Model\$(PROJECT).exe" /MANIFEST @$(PROJECT).rsp
	$(MT) -manifest "$(APSIM)\Model\$(PROJECT).exe.manifest" -outputresource:"$(APSIM)\Model\$(PROJECT).exe;1"

else

RESOBJ = dllres.r
all: $(PRECOMPILEDHEADERS) $(OBJS) $(RESOBJ) $(PROJECT).dll

$(PROJECT).dll: $(PREBUILD) $(SOURCEOBJS) $(RESOBJ) $(OBJS) $(PRECOMPILEDHEADERS)
	echo $(LFLAGS) $(SOURCEOBJS) $(SYSOBJS) $(OBJS) $(RESOBJ) /DEF:$(PROJECT).def $(LIBPATH) $(LIBS) > $(PROJECT).rsp
	$(LD) /OUT:"$(APSIM)\Model\$(PROJECT).dll" /MANIFEST @$(PROJECT).rsp
	$(MT) -manifest "$(APSIM)\Model\$(PROJECT).dll.manifest" -outputresource:"$(APSIM)\Model\$(PROJECT).dll;2"

endif

$(RESOBJ): $(APSIM)/Model/Build/dll.rc
	$(RC) -DPROJ=$(PROJECT) -DMAJOR_VERSION=$(MAJOR_VERSION) -DMINOR_VERSION=$(MINOR_VERSION) -DBUILD_NUMBER=$(BUILD_NUMBER) -fo $@ $<

$(OBJS): $(OBJSSRC)	
	$(CC) $(CFLAGS_NOPCH) $(WARNINGS) /Fo"$(dir $<)" $<
	
$(PRECOMPILEDHEADERS) : $(PRECOMPILE)
	$(CC) $(CFLAGS) $(WARNINGS) /Yc"$(PRECOMPILE)" /Fo"$(dir $<)" $(firstword $(SRC))
	

