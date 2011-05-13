
CPPDEBUGFLAGS=-g 
LDDEBUGFLAGS := $(LDDEBUGFLAGS) -lg 

BOOST_INCLUDEDIR=-I/usr/include/boost
XML2_INCLUDEDIR=-I/usr/include/libxml2
XML2_LIBDIR= -L/usr/lib

USE_MONO=1
ifdef USE_MONO
MONO_INCLUDEDIR=-I/opt/mono-2.10/include/mono-2.0
MONO_LIBDIR= -L/opt/mono-2.10/lib
EXTRALIBS= -lmono-2.0 -lm -lrt -lpthread $(EXTRALIBS)
MONO_DEFINE= -DMONO
endif

CC=/usr/bin/g++
LD=/usr/bin/ld
CFLAGS= -Wall $(MONO_DEFINE) $(BOOST_INCLUDEDIR) $(XML2_INCLUDEDIR) $(GLIB_INCLUDEDIR) $(MONO_INCLUDEDIR) -I$(APSIM)/Model \
-Wno-write-strings -fpermissive -fPIC -O0 $(CPPDEBUGFLAGS) $(INCLUDES)

#-Wno-deprecated

#-----------------------------------------------------------------------
# Required libraries
LIBS:= $(foreach library,$(LIBS),$(APSIM)/Model/$(library).so) \
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

clean:
	rm -f $(OBJ) $(APSIM)/Model/$(PROJECT).x $(APSIM)/Model/$(PROJECT).so
