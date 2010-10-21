###########################################
# gfortran compiler switches.
###########################################
FC=gfortran
LD=ld

# add .lib to all user libraries
LIBS := $(foreach library,$(LIBS),../$(library).lib)
STATICLIBS := $(foreach library,$(STATICLIBS),../$(library).a)

F90FLAGS= -cpp -D'ml_external=!' -shared-libgcc -static-libgfortran -fno-underscoring -mrtd -ffree-line-length-none -finit-local-zero -O2 -g
F90INCLUDES = -I$(APSIM)/Model/FortranInfrastructure

F90MODS= -I$(APSIM)/Model/CropTemplate -I$(APSIM)/Model/CropMod

# Generic rules
%.o:	%.for
	$(FC) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o:	%.f90
	$(FC) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

OBJS:=	$(SRC:.for=.o)
OBJS:=	$(OBJS:.f90=.o)

# remove all paths on OBJ files.
OBJSNODIR := $(foreach o,$(OBJS),$(notdir $(o)))

ifeq ($(PROJECTTYPE),libdll)
LDFLAGS:= --export-dynamic
all: $(APSIM)/Model/$(PROJECT).dll
endif

ifeq ($(PROJECTTYPE),dll)
LDFLAGS:= --export-dynamic \
-u Main -u doInit1 -u wrapperDLL -u respondToEvent -u alloc_dealloc_instance \
-u getInstance -u getDescription -u getDescriptionLength --no-allow-shlib-undefined --disable-auto-import  -L "C:\Program Files\gfortran\lib"
all: $(APSIM)/Model/$(PROJECT).dll
$(APSIM)/Model/$(PROJECT).dll: $(OBJS)
	$(FC) -shared -o ../$(PROJECT).dll $(F90FLAGS) $(LDFLAGS) $(OBJSNODIR) $(DEF) $(STATICLIBS) $(LIBS) 
else

ifeq ($(PROJECTTYPE),lib)
all: $(APSIM)/Model/$(PROJECT).a
$(APSIM)/Model/$(PROJECT).a: $(OBJS)
	ar rv $@ $(OBJS)
OBJSNODIR := $(foreach o,$(OBJS),$(notdir $(o)))
OBJSWITHPLUS := $(foreach o,$(OBJSNODIR),+$(o))
$(PROJECT).lib: $(OBJS)
	del /Q $(PROJECT).lib 2>nul
#    cmd /C move /Y $(PROJECT).lib ..

else
   echo "Error: target type $(PROJECTTYPE) unknown"
endif
endif


