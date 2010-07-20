

#-----------------------------------------------------------------------
# The rules
ifeq ($(PROJECTTYPE),exe)
all: $(APSIM)/Model/$(PROJECT).exe
else
all: $(APSIM)/Model/$(PROJECT).dll
endif


$(APSIM)/Model/$(PROJECT).exe: $(SRC)
	$(VS90COMNTOOLS)\..\IDE\devenv $(PROJECT).sln /build release


$(APSIM)/Model/$(PROJECT).dll: $(SRC)
	$(VS90COMNTOOLS)\..\IDE\devenv $(PROJECT).sln /build release

clean:
	$(RM) $(APSIM)\Model\$(PROJECT).exe $(APSIM)\Model\$(PROJECT).dll



