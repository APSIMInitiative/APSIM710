
CPPDEBUGFLAGS=-g
LDDEBUGFLAGS := $(LDDEBUGFLAGS) -lg

BOOST_INCLUDEDIR=-I/usr/include/boost
XML2_INCLUDEDIR=-I/usr/include/libxml2
XML2_LIBDIR= -L/usr/lib

MONO_INCLUDEDIR=-I$(MONO_PREFIX)/include/mono-2.0
MONO_LIBDIR= -L$(MONO_PREFIX)/lib
MONO_DEFINE= -DMONO

# If you have mono 3.8+ installed and get stung by "Configuration system failed" runtime errors, compile with this:
# (relies on https://github.com/mono/mono/commit/57f5187ad29a7913f083a659ea77d90eb8bad4d4)
#MONO_DEFINE= -DMONO -DMONO3

ifdef USE_MONO
EXTRALIBS:= -lmono-2.0 -lm -lrt -lpthread $(EXTRALIBS)
endif

CC=/usr/bin/g++
LD=/usr/bin/ld
# the -fno-omit-frame-pointer is present to disable an option otherwise set by -O3.
# Apparently, for g++ versions before 4.0, using -fomit-frame pointer interferes with use of STDCALL
CFLAGS= -Wall $(MONO_DEFINE) $(BOOST_INCLUDEDIR) $(XML2_INCLUDEDIR) $(GLIB_INCLUDEDIR) $(MONO_INCLUDEDIR) -I$(APSIM)/Model -I$(APSIM)/Model/$(PROJECT) \
-std=c++11 -DBOOST_FILESYSTEM_VERSION=3 -Wno-write-strings -fpermissive -fPIC -O3 -fno-omit-frame-pointer $(CPPDEBUGFLAGS) $(INCLUDES)

#-Wno-deprecated

#-----------------------------------------------------------------------
# Required libraries
LIBS:= -L$(APSIM)/Model $(foreach lib,$(LIBS),-l$(lib)) \
        $(EXTRALIBS)  -lboost_filesystem -lboost_date_time -lboost_thread \
        $(XML2_LIBDIR) -lxml2 $(MONO_LIBDIR)

ifeq ($(PROJECTTYPE),dll)
LDFLAGS:= -Xlinker --warn-common -Xlinker -Bsymbolic -Xlinker -Bsymbolic-functions $(LDFLAGS)
LIBS := $(LIBS) -ldl
endif

ifeq ($(PROJECTTYPE),libdll)
LDFLAGS:= -Xlinker --warn-common -Xlinker --export-dynamic -Xlinker -Bsymbolic -Xlinker -Bsymbolic-functions $(LDFLAGS)
LIBS := $(LIBS) -ldl
endif

ifeq ($(PROJECTTYPE),exe)
LDFLAGS:= -Xlinker --export-dynamic $(LDFLAGS)
endif

ifdef PRECOMPILE
	PRECOMPILEDHEADERS = $(PRECOMPILE).gch
endif

OBJ:=	$(SRC:.cpp=.o)

#-----------------------------------------------------------------------
# The rules
ifeq ($(PROJECTTYPE),exe)
all: $(APSIM)/Model/$(PROJECT).exe $(POSTBUILD)
else
all: $(APSIM)/Model/$(PROJECT).so $(POSTBUILD)
endif

%.o:    %.cpp
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(PRECOMPILEDHEADERS) : $(PRECOMPILE)
	$(CC) $(CPPFLAGS) $(CFLAGS) $<

$(APSIM)/Model/$(PROJECT).exe: $(PREBUILD) $(PRECOMPILEDHEADERS) $(OBJ)
	$(CC) -o $@ $(OBJ) $(OBJS) $(LDFLAGS) $(LIBS) $(LDDEBUGFLAGS)

$(APSIM)/Model/$(PROJECT).so: $(PREBUILD) $(PRECOMPILEDHEADERS) $(OBJ)
	$(CC) -shared -o $@ $(OBJ) $(OBJS) $(LDFLAGS) $(LIBS) $(LDDEBUGFLAGS) $(DEF)

ifeq ($(PROJECTTYPE),libdll)
	/bin/ln -sf $@ $(APSIM)/Model/lib$(PROJECT).so
endif


clean:
	rm -f $(OBJ) $(PRECOMPILEDHEADERS) $(APSIM)/Model/$(PROJECT).exe $(APSIM)/Model/$(PROJECT).so
