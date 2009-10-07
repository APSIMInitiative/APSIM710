###########################################
# Lahey FORTRAN compiler switches.
###########################################
LFLM=lm.exe
LF95=lf95.exe

# add .lib to all user libraries
LIBS := $(foreach library,$(LIBS),..\$(library)\$(library).imp)
STATICLIBS := $(foreach library,$(STATICLIBS),..\$(library).lib)

F90FLAGS = -dll -tpp -nco -o0 -pca -libpath $(APSIM)\Model -ml bc -staticlink
F90INCLUDES = ##### -i .;$(APSIM)\Model\FortranInfrastructure
F90MODS=-mod .;$(APSIM)\Model\FortranInfrastructure;$(APSIM)\Model\CropTemplate;$(APSIM)\Model\CropMod

#F90CROPINCLUDES=-i .;$(APSIM)\Model\infra\source;$(APSROOT)\apsim\croptemp\source
#F90CROPMODS=-mod .;$(APSROOT)\apsim\infra\source;$(APSROOT)\apsim\croptemp\source
#F90CROPLIBS=-libPath $(APSROOT)\apsbuild -lib $(APSROOT)\apsim\infra\lib\apsinfra.lib -lib $(APSROOT)\apsim\croptemp\lib\croptemp.lib

# Generic rules
%.obj:	%.for
	$(LF95) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.obj:	%.f90
	$(LF95) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

OBJS:=	$(SRC:.for=.obj)
OBJS:=	$(OBJS:.f90=.obj)

# remove all paths on OBJ files.
OBJSNODIR := $(foreach obj,$(OBJS),$(notdir $(obj)))

ifeq ($(PROJECTTYPE),dll)
   EXPORTS := -export Main,doInit1,wrapperDLL,respondToEvent,alloc_dealloc_instance,getInstance,getDescription,getDescriptionLength
   $(PROJECT).dll: $(OBJS)
	   $(LF95) $(F90FLAGS) $(LIBS) $(STATICLIBS) $(OBJSNODIR) $(EXPORTS) -exe ..\$(PROJECT).dll
else
ifeq ($(PROJECTTYPE),lib)
OBJSNODIR := $(foreach obj,$(OBJS),$(notdir $(obj)))
OBJSWITHPLUS := $(foreach obj,$(OBJSNODIR),+$(obj))
$(PROJECT).lib: $(OBJS)
	cmd /C if exist $(PROJECT).lib del /Q $(PROJECT).lib 
	$(LFLM) $(PROJECT).lib $(OBJSWITHPLUS),,
	cmd /C move /Y $(PROJECT).lib ..

else
   echo "Error: target type $(PROJECTTYPE) unknown"
endif
endif


