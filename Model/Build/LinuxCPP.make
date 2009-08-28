
CPPDEBUGFLAGS=-g
LDDEBUGFLAGS=-lg

BOOST_INCLUDEDIR =-I/usr/include
XML2_INCLUDEDIR =-I/usr/include/libxml2
XML2_LIBDIR =-L/usr/lib

CC=     /usr/bin/g++
CFLAGS=$(BOOST_INCLUDEDIR) $(XML2_INCLUDEDIR) -I$(APSIM)/Model 	\
        -Wno-deprecated -fpermissive -fPIC $(CPPDEBUGFLAGS)

#-----------------------------------------------------------------------
# Required libraries

LDFLAGS= $(XML2_LIBDIR) $(LDDEBUGFLAGS)

LIBS := $(foreach library,$(LIBS),$(APSIM)/Model/$(library).so) -lboost_filesystem-mt -lxml2

OBJ:=	$(SRC:.cpp=.o)

#-----------------------------------------------------------------------
# The rules

all: $(PROJECT)

%.o:    %.cpp
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(LIB): $(OBJ)
	$(CC) -shared -o $(LIB) $(OBJ) $(LDFLAGS) $(LIBS)

$(PROJECT): $(PREBUILD) $(OBJ)
ifeq ($(PROJECTTYPE),exe)
	$(CC) -o $(APSIM)/Model/$(PROJECT) $(OBJ) $(LDFLAGS) $(LIBS)
else
	$(CC) -shared -o $(APSIM)/Model/$(PROJECT).so $(OBJ) $(LDFLAGS) $(LIBS)
endif

clean:
	rm -f $(OBJ) $(PROJECT)

