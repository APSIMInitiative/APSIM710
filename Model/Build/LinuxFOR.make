
CPPDEBUGFLAGS=-g 
LDDEBUGFLAGS=-lg

CC=/usr/bin/g++
LD=/usr/bin/ld
F95 =  /opt/intel/Compiler/11.1/072/bin/ia32/ifort

# add suffix to all user libraries
LIBS := $(foreach library,$(LIBS),$(APSIM)/Model/$(library).so) -ldl $(LDDEBUGFLAGS)
STATICLIBS := $(foreach library,$(STATICLIBS),$(APSIM)/Model/$(library).a)

F90FLAGS= -D"ml_external=external" -noD
F90INCLUDES = 
F90MODS=-I$(APSIM)/Model/FortranInfrastructure -I$(APSIM)/Model/CropTemplate -I$(APSIM)/Model/CropMod 

CFLAGS= -Wall -I$(APSIM)/Model -Wno-write-strings -fpermissive -fPIC $(CPPDEBUGFLAGS)

OBJS:=	$(SRC:.for=.o) 
OBJS:=	$(OBJS:.f90=.o)
OBJS:= $(OBJS:.cpp=.o)

ifeq ($(PROJECTTYPE),libdll)
 LDFLAGS:= --export-dynamic 
all: $(APSIM)/Model/$(PROJECT).so
endif

ifeq ($(PROJECTTYPE),dll)
 LDFLAGS:= --no-allow-shlib-undefined -u wrapperDLL -u getInstance -u getDescription -u getDescriptionLength 
all: $(APSIM)/Model/$(PROJECT).so
endif

ifeq ($(PROJECTTYPE),lib)
all: $(APSIM)/Model/$(PROJECT).a
endif

%.o: %.for
	$(F95) -o $@ -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o: %.f90
	$(F95) -o $@ -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o:    %.cpp
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(APSIM)/Model/$(PROJECT).so: $(OBJS)
	$(CC) -shared -o $(APSIM)/Model/$(PROJECT).so $(LDFLAGS) $(OBJS) $(STATICLIBS) $(LIBS) 

$(APSIM)/Model/$(PROJECT).a: $(OBJS)
	ar rv $@ $(OBJS)

clean:
	rm -f *.mod $(OBJS) $(APSIM)/Model/$(PROJECT).so $(APSIM)/Model/$(PROJECT).a

