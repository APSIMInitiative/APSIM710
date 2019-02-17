###########################################
# make file for MS VS2015 (and some earlier versions)
###########################################

######uncomment for debug everything###

###DEBUG=yes

include $(APSIM)\Model\Build\VersionInfo.make

CC="cl.exe"
LD="link.exe"
MT="mt.exe"
RC=rc

CPPUNIT = $(APSIM)\..\BuildLibraries\cppunit-1.13.2
LIBXML = $(APSIM)\..\BuildLibraries\libxml2-2.9.4.win32
ICONV = $(APSIM)\..\BuildLibraries\libiconv-1.11.1.win32

DEFINES := /D "__WIN32__" /D "WIN32" /D "_WINDOWS" /D "_USRDLL" $(DEFINES)
INCLUDES := /I $(APSIM)\Model /I $(CPPUNIT)\include /I $(LIBXML)\include /I $(ICONV)\include /I $(APSIM)\Model\$(PROJECT) $(INCLUDES) 
LIBPATH := /LIBPATH:$(APSIM)\Model /LIBPATH:$(LIBXML)\lib /LIBPATH:$(ICONV)\lib /LIBPATH:$(CPPUNIT)\lib 

# add .lib to all user libraries
LIBS := $(foreach library,$(LIBS),$(library).lib) libxml2.lib

WARNINGS := /wd4996 /wd4068 /wd4290 /wd4251 /wd4244

CFLAGS := $(INCLUDES) $(DEFINES) /EHsc /W3 /nologo /c /TP /Fd"$(APSIM)\Model\$(PROJECT).pdb"  /Zm1000

LFLAGS := /NOLOGO /SUBSYSTEM:CONSOLE /DYNAMICBASE /NXCOMPAT /MACHINE:X86 /LTCG

SYSOBJS := kernel32.lib user32.lib advapi32.lib shell32.lib uuid.lib

ifeq ($(PROJECTTYPE),exe)
	CFLAGS := $(CFLAGS) /LD
	LFLAGS := $(LFLAGS)
else
#	CFLAGS := $(CFLAGS) /D "$(PROJECT)_EXPORTS"
	LFLAGS := $(LFLAGS) /DLL
endif

# Optimisation and debug symbols.
ifdef DEBUG
	CFLAGS := $(CFLAGS) /Od /RTCs /ZI /MDd /D "_DEBUG"
#	LIBS := cg32.lib import32.lib $(LIBS)
	LFLAGS := $(LFLAGS) /DEBUG /PDB:"$(APSIM)\Model\$(PROJECT).pdb"
   PRECOMPILE:=
else
	CFLAGS := $(CFLAGS) /O2 /MD /GL
#	LFLAGS := $(LFLAGS)
#	LIBS :=  import32.lib $(LIBS)
endif

CFLAGS_NOPCH := $(CFLAGS)

ifdef PRECOMPILE
    PRECOMPILEDHEADERS = $(PRECOMPILE:%.h=%.pch)
	CFLAGS := $(CFLAGS) /Yu"$(PRECOMPILE)" /D"PRECOMPILE=""$(PRECOMPILE)"""  /Fp"$(APSIM)\Model\$(PROJECT)\$(PRECOMPILEDHEADERS)"
endif

# The rules
%.obj:	%.cpp
	$(CC) $(CFLAGS) $(WARNINGS) /Fo"$(dir $<)" $<

SOURCEOBJS:= $(SRC:.cpp=.obj)
OBJSSRC:=$(OBJS:.obj=.cpp)

ifeq ($(PROJECTTYPE),exe)
#### EXEs
all: $(PRECOMPILEDHEADERS)  $(PROJECT).exe
$(PROJECT).exe: $(PREBUILD) $(SOURCEOBJS)
	echo $(LFLAGS) $(SOURCEOBJS) $(SYSOBJS) $(LIBPATH) $(LIBS) > $(PROJECT).rsp
	$(LD) /OUT:"$(APSIM)\Model\$(PROJECT).exe" /MANIFEST @$(PROJECT).rsp
	$(MT) -manifest "$(APSIM)\Model\$(PROJECT).exe.manifest" -outputresource:"$(APSIM)\Model\$(PROJECT).exe;1"

else

all: $(PRECOMPILEDHEADERS) $(OBJS) $(PROJECT).dll

$(PROJECT).dll: $(PREBUILD) $(SOURCEOBJS) $(OBJS) $(PRECOMPILEDHEADERS)
	echo $(LFLAGS) $(SOURCEOBJS) $(SYSOBJS) $(OBJS) /DEF:$(PROJECT).def $(LIBPATH) $(LIBS) > $(PROJECT).rsp
	$(LD) /OUT:"$(APSIM)\Model\$(PROJECT).dll" /MANIFEST @$(PROJECT).rsp
	$(MT) -manifest "$(APSIM)\Model\$(PROJECT).dll.manifest" -outputresource:"$(APSIM)\Model\$(PROJECT).dll;2"

endif

$(RESOBJ): $(APSIM)/Model/Build/dll.rc
	$(RC) -DPROJ=$(PROJECT) -DMAJOR_VERSION=$(MAJOR_VERSION) -DMINOR_VERSION=$(MINOR_VERSION) -DBUILD_NUMBER=$(BUILD_NUMBER) -fo $@ $<

$(OBJS): $(OBJSSRC)	
	$(CC) $(CFLAGS_NOPCH) $(WARNINGS) /Fo"$(dir $<)" $<
	
$(PRECOMPILEDHEADERS) : $(PRECOMPILE)
	$(CC) $(CFLAGS) $(WARNINGS) /Yc"$(PRECOMPILE)" /Fo"$(dir $<)" $(firstword $(SRC))
	

