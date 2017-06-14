MONO5 := $(shell command -v msbuild 2> /dev/null)

#-----------------------------------------------------------------------
# The rules
ifeq ($(PROJECTTYPE),exe)
all: $(APSIM)/Model/$(PROJECT).exe
else
all: $(APSIM)/Model/$(PROJECT).dll
endif

ifdef MONO5
$(APSIM)/Model/$(PROJECT).exe: $(PREBUILD)

	msbuild $(PROJECT).sln /target:Build /p:DefineConstants=__MonoCS__

$(APSIM)/Model/$(PROJECT).dll: $(PREBUILD)
	msbuild $(PROJECT).sln /target:Build /p:DefineConstants=__MonoCS__

else
$(APSIM)/Model/$(PROJECT).exe: $(PREBUILD)

	xbuild $(PROJECT).sln /target:Build

$(APSIM)/Model/$(PROJECT).dll: $(PREBUILD)
	xbuild $(PROJECT).sln /target:BuildDef

endif

clean:
	rm -f $(APSIM)/Model/$(PROJECT).exe $(APSIM)/Model/$(PROJECT).dll



