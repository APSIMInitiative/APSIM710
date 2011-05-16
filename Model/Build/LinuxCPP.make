
CPPDEBUGFLAGS=-g 
LDDEBUGFLAGS := $(LDDEBUGFLAGS) -lg 

BOOST_INCLUDEDIR=-I/usr/include/boost
XML2_INCLUDEDIR=-I/usr/include/libxml2
XML2_LIBDIR= -L/usr/lib

MONO_INCLUDEDIR=-I$(MONO)/include/mono-2.0
MONO_LIBDIR= -L$(MONO)/lib
MONO_DEFINE= -DMONO
ifdef USE_MONO
EXTRALIBS:= -lmono-2.0 -lm -lrt -lpthread $(EXTRALIBS)
endif

CC=/usr/bin/g++
LD=/usr/bin/ld
CFLAGS= -Wall $(MONO_DEFINE) $(BOOST_INCLUDEDIR) $(XML2_INCLUDEDIR) $(GLIB_INCLUDEDIR) $(MONO_INCLUDEDIR) -I$(APSIM)/Model \
-Wno-write-strings -fpermissive -fPIC -O0 $(CPPDEBUGFLAGS) $(INCLUDES)

#-Wno-deprecated

#-----------------------------------------------------------------------
# Required libraries
LIBS:= -L$(APSIM)/Model $(foreach lib,$(LIBS),-l$(lib)) \
        $(EXTRALIBS) -lboost_filesystem-mt -lboost_regex-mt -lboost_date_time-mt -lboost_thread-mt \
        $(XML2_LIBDIR) -lxml2 $(MONO_LIBDIR)

ifeq ($(PROJECTTYPE),dll)
LDFLAGS:= --no-allow-shlib-undefined --warn-common -Xlinker -Bsymbolic -Xlinker -Bsymbolic-functions  $(LDFLAGS) 
LIBS := $(LIBS) -ldl
endif

ifeq ($(PROJECTTYPE),libdll)
LDFLAGS:= --no-allow-shlib-undefined --warn-common --export-dynamic $(LDFLAGS) 
LIBS := $(LIBS) -ldl
endif

ifeq ($(PROJECTTYPE),exe)
LDFLAGS:= --export-dynamic $(LDFLAGS)
endif


OBJ:=	$(SRC:.cpp=.o)

#-----------------------------------------------------------------------
# The rules
ifeq ($(PROJECTTYPE),exe)
all: $(APSIM)/Model/$(PROJECT).x
else
all: $(APSIM)/Model/$(PROJECT).so
endif

%.o:    %.cpp
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(APSIM)/Model/$(PROJECT).x: $(PREBUILD) $(OBJ)
	$(CC) -o $@ $(OBJ) $(OBJS) $(LDFLAGS) $(LIBS) $(LDDEBUGFLAGS)

$(APSIM)/Model/$(PROJECT).so: $(PREBUILD) $(OBJ)
	$(CC) -shared -o $@ $(OBJ) $(OBJS) $(LDFLAGS) $(LIBS) $(LDDEBUGFLAGS) $(DEF)
ifeq ($(PROJECTTYPE),libdll)
	ln -sf $@ $(APSIM)/Model/lib$(PROJECT).so
endif

clean:
	rm -f $(OBJ) $(APSIM)/Model/$(PROJECT).x $(APSIM)/Model/$(PROJECT).so
