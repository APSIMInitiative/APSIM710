
CPPDEBUGFLAGS=-g -Wall
LDDEBUGFLAGS=-lg

BOOST_INCLUDEDIR=-I$(APSIM)/../BuildLibraries/boost_1_42_0
XML2_INCLUDEDIR=-I/usr/include/libxml2
XML2_LIBDIR= -L/usr/lib

CC=/usr/bin/g++
LD=/usr/bin/ld
CFLAGS=$(BOOST_INCLUDEDIR) $(XML2_INCLUDEDIR) -I$(APSIM)/Model 	\
         -fpermissive -fPIC $(CPPDEBUGFLAGS)

#-Wno-deprecated

#-----------------------------------------------------------------------
# Required libraries
ifeq ($(PROJECTTYPE),dll)
LDFLAGS:= --export-dynamic \
-u Main -u doInit1 -u wrapperDLL -u respondToEvent -u alloc_dealloc_instance \
-u getInstance -u getDescription -u getDescriptionLength --no-allow-shlib-undefined
endif

ifeq ($(PROJECTTYPE),libdll)
LDFLAGS:= --export-dynamic --no-allow-shlib-undefined
endif

ifeq ($(PROJECTTYPE),exe)
LDFLAGS:= 
endif

LIBS:= $(foreach library,$(LIBS),$(APSIM)/Model/$(library).so) \
        -lboost_filesystem-mt -lboost_regex-mt -lboost_date_time-mt \
        $(XML2_LIBDIR) -lxml2 $(LDDEBUGFLAGS)

# -Wl,-Map $(APSIM)/Model/$(PROJECT).map

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
	$(CC) -o $@ $(OBJ) $(LDFLAGS) $(LIBS)

$(APSIM)/Model/$(PROJECT).so: $(PREBUILD) $(OBJ)
	$(CC) -shared -o $@ $(OBJ) $(LDFLAGS) $(LIBS)

clean:
	rm -f $(OBJ) $(APSIM)/Model/$(PROJECT).x $(APSIM)/Model/$(PROJECT).so



