
CPPDEBUGFLAGS=-g
LDDEBUGFLAGS=-lg

CC=/usr/bin/g++
LF95=/usr/local/lf9562/bin/lf95

# add .lib to all user libraries
LIBS := $(foreach library,$(LIBS),../$(library).so)
STATICLIBS := $(foreach library,$(STATICLIBS),../$(library).a)

##CPPFLAGS=	-I$(APSROOT)/apsim/infra/source	-M. -M$(APSROOT)/apsim/infra/source $(CPPDEBUGFLAGS)
F90FLAGS= --tp -nco --o0 --pca -nsav -trace -nchk -nin --ml cdecl --staticlink $(CPPDEBUGFLAGS)
##F90FLAGS = --dll --tpp --nco --o0 --pca --ml cdecl -staticlink $(CPPDEBUGFLAGS)
F90INCLUDES = -I$(APSIM)/Model/FortranInfrastructure
F90MODS=-M. -M$(APSIM)/Model/FortranInfrastructure -M$(APSIM)/Model/CropTemplate -M$(APSIM)/Model/CropMod

ifdef APSIMMODEL
   EXPORTS := -export Main,doInit1,wrapperDLL,respondToEvent,alloc_dealloc_instance,getInstance,getDescription,getDescriptionLength
else
   EXPORTS :=
endif

# Generic rules
%.o: %.for
	$(LF95) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o: %.f90
	$(LF95) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

OBJS:=	$(SRC:.for=.o)
OBJS:=	$(OBJS:.f90=.o)

ifeq ($(PROJECTTYPE),dll)
   $(APSIM)/Model/$(PROJECT).so: $(OBJS)
	   $(CC) -o $(APSIM)/Model/$(PROJECT).so $(LIBS) $(STATICLIBS) $(OBJS) 
else
   $(APSIM)/Model/$(PROJECT).a: $(APSIM)/Model/$(PROJECT).a($(OBJS))
endif
clean:
	rm -f $(OBJS) $(APSIM)/Model/$(PROJECT).so $(APSIM)/Model/$(PROJECT).a

