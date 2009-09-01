
CPPDEBUGFLAGS=-g
LDDEBUGFLAGS=-lg

CC=/usr/bin/g++
LF95=/usr/local/lf9562/bin/lf95

# add suffix to all user libraries
LIBS := $(foreach library,$(LIBS),$(APSIM)/Model/$(library).so) -L/usr/local/lf9562/lib -lfj9i6 -lfj9f6 -lfj9e6 -lfccx86_6a -lfst -lz 

STATICLIBS := $(APSIM)/Model/FortranInfrastructure/EntryPointsLinux.o $(foreach library,$(STATICLIBS),$(APSIM)/Model/$(library).a)

F90FLAGS= --tp -nco --o0 --pca -nsav -stchk -trace -nchk -nin --ml cdecl --staticlink $(CPPDEBUGFLAGS)

F90INCLUDES = -I$(APSIM)/Model/FortranInfrastructure

F90MODS=-M. -M$(APSIM)/Model/FortranInfrastructure -M$(APSIM)/Model/CropTemplate -M$(APSIM)/Model/CropMod 

# Generic rules
%.o: %.for
	$(LF95) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

%.o: %.f90
	$(LF95) -c $< $(F90FLAGS) $(F90INCLUDES) $(F90MODS)

OBJS:=	$(SRC:.for=.o)
OBJS:=	$(OBJS:.f90=.o)

ifeq ($(PROJECTTYPE),dll)
   $(APSIM)/Model/$(PROJECT).so: $(OBJS)
	   $(CC) -shared -o $(APSIM)/Model/$(PROJECT).so $(OBJS) $(STATICLIBS) $(LIBS) 
else
   $(APSIM)/Model/$(PROJECT).a: $(OBJS)
	ar rv $@ $(OBJS)
endif
clean:
	rm -f $(OBJS) $(APSIM)/Model/$(PROJECT).so $(APSIM)/Model/$(PROJECT).a

