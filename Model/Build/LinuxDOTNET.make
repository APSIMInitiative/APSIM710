

#-----------------------------------------------------------------------
# The rules
ifeq ($(PROJECTTYPE),exe)
all: $(APSIM)/Model/$(PROJECT).exe
else
all: $(APSIM)/Model/$(PROJECT).dll
endif


$(APSIM)/Model/$(PROJECT).exe: $(PREBUILD)
	xbuild $(PROJECT).sln /target:Build

$(APSIM)/Model/$(PROJECT).dll: $(PREBUILD)
	xbuild $(PROJECT).sln /target:Build

clean:
	rm -f $(APSIM)/Model/$(PROJECT).exe $(APSIM)/Model/$(PROJECT).dll



