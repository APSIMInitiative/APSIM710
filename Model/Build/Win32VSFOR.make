###########################################
# gfortran compiler switches.
###########################################
FC=gfortran
LD=ld
RC=windres
CC=gcc

include $(APSIM)\Model\Build\VersionInfo.make


# add .lib to all user libraries
LIBS := $(foreach library,$(LIBS),../$(library).lib)
STATICLIBS := $(foreach library,$(STATICLIBS),../$(library).a)

F90FLAGS= -cpp -D'ml_external=!' -D'STDCALL(x)=GCC$$ ATTRIBUTES STDCALL :: x' -static -static-libgfortran -fno-underscoring -ffree-line-length-none -finit-integer=0 -finit-real=zero -finit-logical=false -O3 -frounding-math -g -march=pentiumpro -mtune=pentiumpro $(EXTRACOMPILEFLAGS)
CFLAGS= -cpp -D'ml_external=!' -D'STDCALL(x)=GCC$$ ATTRIBUTES STDCALL :: x' -static -static-libgfortran -march=pentiumpro -mtune=pentiumpro $(EXTRACOMPILEFLAGS)
F90INCLUDES = -I$(APSIM)/Model/FortranInfrastructure

F90MODS= -I$(APSIM)/Model/CropTemplate -I$(APSIM)/Model/CropMod

# Generic rules
%.o:	%.for
	$(FC) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o:	%.FOR
	$(FC) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o:	%.f90
	$(FC) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o:	%.F90
	$(FC) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o:	%.cpp
	$(CC) $(CFLAGS) -I$(APSIM)/Model $(WARNINGS) -c $<
   
%.o:	%.c
	$(CC) $(CFLAGS) -I$(APSIM)/Model $(WARNINGS) -c $<

OBJS:=	$(SRC:.for=.o)
OBJS:=	$(OBJS:.f90=.o)
OBJS:=	$(OBJS:.F90=.o)
OBJS:=	$(OBJS:.FOR=.o)
OBJS:=	$(OBJS:.cpp=.o)
OBJS:=	$(OBJS:.c=.o)

# remove all paths on OBJ files.
OBJSNODIR := $(foreach o,$(OBJS),$(notdir $(o)))

ifeq ($(PROJECTTYPE),libdll)
LDFLAGS:= --export-dynamic
all: $(APSIM)/Model/$(PROJECT).dll
endif

ifeq ($(PROJECTTYPE),dll)

RESOBJ = dllres.obj
LDFLAGS:= -Xlinker --enable-stdcall-fixup -Xlinker --no-allow-shlib-undefined -Xlinker --disable-auto-import
all: $(APSIM)/Model/$(PROJECT).dll 
$(APSIM)/Model/$(PROJECT).dll: $(OBJS) $(RESOBJ)
	$(FC) -shared -o ../$(PROJECT).dll $(F90FLAGS) $(LDFLAGS) $(OBJSNODIR) $(RESOBJ) $(DEF) $(STATICLIBS) $(LIBS) $(EXTRALIBS)

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
ifeq ($(PROJECTTYPE),exe)

RESOBJ = dllres.obj
all: $(APSIM)/Model/$(PROJECT).exe 
$(APSIM)/Model/$(PROJECT).exe: $(OBJS)
	$(FC) -o ../$(PROJECT).exe $(F90FLAGS) $(LDFLAGS) $(OBJSNODIR) $(RESOBJ) $(DEF) $(STATICLIBS) $(LIBS) $(EXTRALIBS)

$(RESOBJ): $(APSIM)/Model/Build/dll.rc
	$(RC) -DPROJ=$(PROJECT) -DMAJOR_VERSION=$(MAJOR_VERSION) -DMINOR_VERSION=$(MINOR_VERSION) -DBUILD_NUMBER=$(BUILD_NUMBER) $< $@

else
   echo "Error: target type $(PROJECTTYPE) unknown"
endif
endif
endif


