
CPPDEBUGFLAGS=-g
LDDEBUGFLAGS=-lg
F95DEBUGFLAGS=-g
#DEF=$(APSIM)/Model/Build/Exports.rsp

CC=/usr/bin/g++
LD=/usr/bin/ld
FC =  /usr/bin/gfortran

# add suffix to all user libraries
LIBS := -L$(APSIM)/Model $(foreach lib,$(LIBS),-l$(lib)) -ldl $(LDDEBUGFLAGS)
STATICLIBS := $(foreach library,$(STATICLIBS),$(APSIM)/Model/$(library).a)

F90FLAGS= -D"ml_external=!external" -D"STDCALL(x)=GCC$$ ATTRIBUTES STDCALL :: x" -static \
          -fno-underscoring -ffree-line-length-none -finit-integer=0 -finit-real=zero -finit-logical=false \
          $(F95DEBUGFLAGS) -frounding-math -O3 $(EXTRACOMPILEFLAGS)
F90INCLUDES = 
F90MODS=-I$(APSIM)/Model/FortranInfrastructure -I$(APSIM)/Model/CropTemplate -I$(APSIM)/Model/CropMod 

CFLAGS= -Wall -I$(APSIM)/Model -Wno-write-strings -fpermissive -fPIC \
        $(CPPDEBUGFLAGS) -O3

OBJS:=	$(SRC:.for=.o) 
OBJS:=	$(OBJS:.f90=.o)
OBJS:= $(OBJS:.cpp=.o)

ifeq ($(PROJECTTYPE),libdll)
 LDFLAGS:= --export-dynamic 
all: $(APSIM)/Model/$(PROJECT).so
endif

ifeq ($(PROJECTTYPE),dll)
 LDFLAGS:= -static -Xlinker -Bsymbolic -Xlinker -Bsymbolic-functions
all: $(APSIM)/Model/$(PROJECT).so
endif

ifeq ($(PROJECTTYPE),lib)
all: $(APSIM)/Model/$(PROJECT).a
endif

%.o: %.for
	$(FC) -x f77-cpp-input -o $@ -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o: %.f90
	$(FC) -x f95-cpp-input -o $@ -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o:    %.cpp
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(APSIM)/Model/$(PROJECT).so: $(OBJS)
	$(FC) -shared -o $(APSIM)/Model/$(PROJECT).so $(LDFLAGS) -lgfortran \
	$(OBJS) $(STATICLIBS) $(LIBS) $(DEF)

$(APSIM)/Model/$(PROJECT).a: $(OBJS)
	ar rv $@ $(OBJS)

clean:
	rm -f *.mod $(OBJS) $(APSIM)/Model/$(PROJECT).so $(APSIM)/Model/$(PROJECT).a

