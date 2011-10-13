###########################################
# gfortran compiler switches.
###########################################
FC=gfortran
LD=ld
RC=windres

# add .lib to all user libraries
LIBS := $(foreach library,$(LIBS),../$(library).lib)
STATICLIBS := $(foreach library,$(STATICLIBS),../$(library).a)

F90FLAGS= -cpp -D'ml_external=!' -D'STDCALL(x)=GCC$$ ATTRIBUTES STDCALL :: x' -static -static-libgfortran -fno-underscoring -ffree-line-length-none -finit-integer=0 -finit-real=zero -finit-logical=false -O3 -frounding-math -g -march=pentiumpro -mtune=pentiumpro
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

# Normally these are obtained as environment variables, but we want to be sure they are not left undefined
ifeq ($(MAJOR_VERSION),)
  MAJOR_VERSION = 1
  MINOR_VERSION = 0
  BUILD_NUMBER = 0
endif

RESOBJ = dllres.obj
LDFLAGS:= -Xlinker --enable-stdcall-fixup -Xlinker --no-allow-shlib-undefined -Xlinker --disable-auto-import
all: $(APSIM)/Model/$(PROJECT).dll 
$(APSIM)/Model/$(PROJECT).dll: $(OBJS) $(RESOBJ)
	$(FC) -shared -o ../$(PROJECT).dll $(F90FLAGS) $(LDFLAGS) $(OBJSNODIR) $(RESOBJ) $(DEF) $(STATICLIBS) $(LIBS) 

$(RESOBJ): $(APSIM)/Model/Build/dll.rc
	$(RC) -DPROJ=$(PROJECT) -DMAJOR_VERSION=$(MAJOR_VERSION) -DMINOR_VERSION=$(MINOR_VERSION) -DBUILD_NUMBER=$(BUILD_NUMBER) $< $@

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


