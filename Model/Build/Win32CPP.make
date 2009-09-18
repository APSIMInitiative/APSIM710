###########################################
# Compiler make file for Borland c++
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
#    VCL
#    PREBUILD  - a target built before dll (eg custom libs)
#
# NB: It is vital that paths passed to
#     bcc and ilink use '\' character.
#     Doesn't work with '/' characters.
###########################################
#DEBUG=yes

BCB = c:\progra~1\Borland\CBuilder6
CC=$(BCB)\Bin\bcc32.exe
LD=$(BCB)\Bin\ilink32
IMPDEF=$(BCB)\Bin\impdef.exe

# add .lib to all user libraries
LIBS := $(foreach library,$(LIBS),$(library).lib)


DEFINES := NO_STRICT;_RTLDLL;BOOST_REGEX_STATIC_LINK;$(DEFINES)
INCLUDEPATH := $(BCB)\include;$(APSIM)\Model;$(BCB)\Components\Boost;$(BCB)\Components\LibXML2\Include;$(INCLUDEPATH)
LIBPATH := $(BCB)\lib\obj;$(BCB)\lib;$(BCB)\components\Boost\libs\regex\build\bcb6;$(APSIM)\Model;$(LIBPATH)
LIBS := $(BCB)\Components\LibXml2\win32\LibXml2.lib $(BCB)\Components\Boost\Libs\FileSystem\FileSystem.lib $(BCB)\Components\Boost\libs\date_time\date_time.lib $(LIBS)

WARNINGS := -w-par
CFLAGS := -Vx -Ve -X- -a8 -b- -tWM -c
LFLAGS := -D""  -x -Gn -Gi
SYSOBJS := Memmgr.Lib

ifeq ($(PROJECTTYPE),exe)
   LFLAGS := $(LFLAGS) -Tpe
else
   LFLAGS := $(LFLAGS) -Tpd
endif

# Optimisation and debug symbols.
ifdef DEBUG
	CFLAGS := $(CFLAGS) -Od -v -y -R -w-inl -vG           # CodeGuard flags: -vG, cg32.lib
	LIBS := cg32.lib import32.lib $(LIBS)
	LFLAGS := $(LFLAGS) -v
else
	CFLAGS := $(CFLAGS) -O2 -w-inl
	LIBS :=  import32.lib $(LIBS)
endif

# VCL stuff
ifdef VCL
   INCLUDEPATH := $(INCLUDEPATH);$(BCB)\components\tms
   LIBPATH := $(LIBPATH);$(BCB)\Components\TeeChart\CBuilder6\Lib;$(BCB)\components\tms
   ifdef DEBUG
      LIBPATH := $(LIBPATH);$(BCB)\lib\debug
   else
      LIBPATH := $(LIBPATH);$(BCB)\lib\release
   endif
   ifeq ($(PROJECTTYPE),exe)
      CFLAGS := $(CFLAGS) -tW
      LFLAGS := $(LFLAGS) -aa
      SYSOBJS := $(SYSOBJS) c0w32.obj
   else
      CFLAGS := $(CFLAGS) -tWD
      LFLAGS := $(LFLAGS) -aa
      SYSOBJS := $(SYSOBJS) c0d32.obj
   endif

   SYSOBJS := $(SYSOBJS) sysinit.obj rtl.bpi vcl.bpi dbrtl.bpi bdertl.bpi vcldb.bpi vclx.bpi nmfast.bpi vcldbx.bpi \
      bcbie.bpi inet.bpi vcljpg.bpi adortl.bpi \
      dbxcds.bpi ibxpress.bpi vclie.bpi inetdbbde.bpi inetdbxpress.bpi \
      dclocx.bpi soaprtl.bpi Tee7C6.bpi TeeUI7C6.bpi TeeDB7C6.bpi TeeGL7C6.bpi \
      qrpt60.bpi TeeQR7C6.bpi TeeLanguage7C6.bpi TeePro7C6.bpi
   LIBS := $(LIBS) cp32mti.lib

   PRECOMPILEDHEADERS := yes
else
   ifeq ($(PROJECTTYPE),exe)
      SYSOBJS := $(SYSOBJS) c0x32.obj
      CFLAGS := $(CFLAGS) -tWC
      LFLAGS := $(LFLAGS) -ap
   else
      CFLAGS := $(CFLAGS) -tWD
      LFLAGS := $(LFLAGS) -aa
      SYSOBJS := $(SYSOBJS) c0d32.obj
   endif
   LIBS := $(LIBS) cw32mti.lib
   DEFINES := $(DEFINES);_NO_VCL
endif

# Precompiled headers
ifdef PRECOMPILEDHEADERS
	CFLAGS := $(CFLAGS) -H=$(TEMP)\vcl60.csm -Hc
endif

# The rules
%.obj:	%.cpp
	$(CC) $(CFLAGS) $(WARNINGS) -I$(INCLUDEPATH) -D$(DEFINES) $<

# Change forward slashes to back slashes.
SRC :=	$(subst /,\,$(SRC))

SOURCEOBJS:=	$(SRC:.cpp=.obj)

# remove all paths on OBJ files.
#OBJSNODIR := $(foreach obj,$(SOURCEOBJS),$(notdir $(obj)))


ifeq ($(PROJECTTYPE),exe)
$(PROJECT).exe: $(PREBUILD) $(SOURCEOBJS)
	$(LD) $(LFLAGS) -L$(LIBPATH) $(SOURCEOBJS) $(SYSOBJS), $(APSIM)\Model\$(PROJECT).exe,, $(LIBS), , $(RES)
else
$(PROJECT).dll: $(PREBUILD) $(SOURCEOBJS)
	$(LD) $(LFLAGS) -L$(LIBPATH) $(SOURCEOBJS) $(SYSOBJS), $(APSIM)\Model\$(PROJECT).dll,, $(LIBS), , $(RES)
	$(IMPDEF) $(PROJECT).def $(APSIM)\Model\$(PROJECT)
	if exist $(APSIM)\Model\def2imp.exe $(APSIM)\Model\def2imp.exe $(PROJECT).def $(PROJECT).imp
endif


