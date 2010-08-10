include $(APSIM)/Model/Build/Platform.make

PROJECT = ProbeDll

PROJECTTYPE = dll


#-----------------------------------------------------------------------
# The rules
all: $(APSIM)/Model/$(PROJECT).exe


# For some reason if we do a release build, then ProbeDLL throws an exception when probing
# the old PLANT model.

$(APSIM)/Model/$(PROJECT).exe: $(SRC)
	$(VS90COMNTOOLS)\..\IDE\devenv $(PROJECT).sln /build debug

clean:
	$(RM) $(APSIM)\Model\$(PROJECT).exe $(APSIM)\Model\$(PROJECT).dll



