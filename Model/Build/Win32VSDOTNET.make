RC=rc

#-----------------------------------------------------------------------
# The rules

include $(APSIM)\Model\Build\VersionInfo.make

ifeq ($(PROJECTTYPE),exe)
   all: $(APSIM)/Model/$(PROJECT).exe
else
   all: $(APSIM)/Model/$(PROJECT).dll
endif

$(APSIM)/Model/$(PROJECT).exe: $(PREBUILD)
	MSBuild $(PROJECT).sln /target:Build /p:Configuration=Release

$(APSIM)/Model/$(PROJECT).dll: $(PREBUILD)
	MSBuild $(PROJECT).sln /target:Build /p:Configuration=Release

	
$(RESOBJ): $(APSIM)/Model/Build/dll.rc
	$(RC) -DPROJ=$(PROJECT) -DMAJOR_VERSION=$(MAJOR_VERSION) -DMINOR_VERSION=$(MINOR_VERSION) -DBUILD_NUMBER=$(BUILD_NUMBER) -fo $@ $<
	
clean:
	$(RM) $(APSIM)\Model\$(PROJECT).exe $(APSIM)\Model\$(PROJECT).dll
