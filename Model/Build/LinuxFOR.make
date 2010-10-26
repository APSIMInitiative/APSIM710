
CPPDEBUGFLAGS=-g
LDDEBUGFLAGS=-lg
F95DEBUGFLAGS=-g
#DEF=$(APSIM)/Model/Build/Exports.rsp

CC=/usr/bin/g++
LD=/usr/bin/ld
F95 =  /usr/bin/gfortran

# add suffix to all user libraries
LIBS := $(foreach library,$(LIBS),$(APSIM)/Model/$(library).so) -ldl $(LDDEBUGFLAGS)
STATICLIBS := $(foreach library,$(STATICLIBS),$(APSIM)/Model/$(library).a)

F90FLAGS= -D"ml_external=!external" -static -mrtd \
          -fno-underscoring -ffree-line-length-none -finit-local-zero \
	  $(F95DEBUGFLAGS) -O0
F90INCLUDES = 
F90MODS=-I$(APSIM)/Model/FortranInfrastructure -I$(APSIM)/Model/CropTemplate -I$(APSIM)/Model/CropMod 

CFLAGS= -Wall -I$(APSIM)/Model -Wno-write-strings -fpermissive -fPIC \
        $(CPPDEBUGFLAGS) -O0

OBJS:=	$(SRC:.for=.o) 
OBJS:=	$(OBJS:.f90=.o)
OBJS:= $(OBJS:.cpp=.o)

ifeq ($(PROJECTTYPE),libdll)
 LDFLAGS:= --export-dynamic 
all: $(APSIM)/Model/$(PROJECT).so
endif

ifeq ($(PROJECTTYPE),dll)
 LDFLAGS:= --no-allow-shlib-undefined -static -Xlinker -Bsymbolic -Xlinker -Bsymbolic-functions
all: $(APSIM)/Model/$(PROJECT).so
endif

ifeq ($(PROJECTTYPE),lib)
all: $(APSIM)/Model/$(PROJECT).a
endif

%.o: %.for
	$(F95) -x f77-cpp-input -o $@ -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o: %.f90
	$(F95) -x f95-cpp-input -o $@ -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o:    %.cpp
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(APSIM)/Model/$(PROJECT).so: $(OBJS)
	$(CC) -shared -o $(APSIM)/Model/$(PROJECT).so $(LDFLAGS) -lgfortran \
	$(OBJS) $(STATICLIBS) $(LIBS) $(DEF)

$(APSIM)/Model/$(PROJECT).a: $(OBJS)
	ar rv $@ $(OBJS)

clean:
	rm -f *.mod $(OBJS) $(APSIM)/Model/$(PROJECT).so $(APSIM)/Model/$(PROJECT).a

