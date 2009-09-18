
CPPDEBUGFLAGS=-g
LDDEBUGFLAGS=-lg

CC=/usr/bin/g++
LD=ld

# Lahey 6.2 compiler
LF95=/usr/local/lf9562/bin/lf95

# add suffix to all user libraries
LIBS := $(foreach library,$(LIBS),$(APSIM)/Model/$(library).so) \
	-ldl -L/usr/local/lf9562/lib -lfj9i6 -lfj9f6 -lfj9e6 -lfccx86_6a -lfst -lz 

STATICLIBS := $(foreach library,$(STATICLIBS),$(APSIM)/Model/$(library).a)

F90FLAGS= --tp -nco --o0 --pca -nsav -stchk -trace -nchk -nin --ml cdecl --staticlink $(CPPDEBUGFLAGS)

F90INCLUDES = -I$(APSIM)/Model/FortranInfrastructure

F90MODS=-M. -M$(APSIM)/Model/FortranInfrastructure -M$(APSIM)/Model/CropTemplate -M$(APSIM)/Model/CropMod 

OBJS:=	$(SRC:.for=.o)
OBJS:=	$(OBJS:.f90=.o)

ifeq ($(PROJECTTYPE),libdll)
LDFLAGS:= --export-dynamic 
all: $(APSIM)/Model/$(PROJECT).so
endif

ifeq ($(PROJECTTYPE),dll)
LDFLAGS:= --export-dynamic \
-u Main -u doInit1 -u wrapperDLL -u respondToEvent -u alloc_dealloc_instance \
-u getInstance -u getDescription -u getDescriptionLength --no-allow-shlib-undefined
all: $(APSIM)/Model/$(PROJECT).so
endif

ifeq ($(PROJECTTYPE),lib)
all: $(APSIM)/Model/$(PROJECT).a
endif

%.o: %.for
	$(LF95) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o: %.f90
	$(LF95) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

$(APSIM)/Model/$(PROJECT).so: $(OBJS)
	$(CC) -shared -o $(APSIM)/Model/$(PROJECT).so $(LDFLAGS) $(OBJS) $(STATICLIBS) $(LIBS)

$(APSIM)/Model/$(PROJECT).a: $(OBJS)
	ar rv $@ $(OBJS)

clean:
	rm -f $(OBJS) $(APSIM)/Model/$(PROJECT).so $(APSIM)/Model/$(PROJECT).a

