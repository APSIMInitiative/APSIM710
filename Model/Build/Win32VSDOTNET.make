RC=rc

#-----------------------------------------------------------------------
# The rules

include $(APSIM)\Model\Build\VersionInfo.make


ifeq ($(PROJECTTYPE),exe)
   all: $(APSIM)/Model/$(PROJECT).exe
else
   all: $(APSIM)/Model/$(PROJECT).dll
endif

ifdef VS_EXPRESS

ifdef VBPROJ
$(APSIM)/Model/$(PROJECT).exe: $(SRC)
	"$(VS100COMNTOOLS)..\IDE\vbexpress" $(PROJECT).sln /build release

$(APSIM)/Model/$(PROJECT).dll: $(SRC) $(RESOBJ)
	"$(VS100COMNTOOLS)..\IDE\vbexpress" $(PROJECT).sln /build release

else
$(APSIM)/Model/$(PROJECT).exe: $(SRC)
	"$(VS100COMNTOOLS)..\IDE\vcsexpress" $(PROJECT).sln /build release

$(APSIM)/Model/$(PROJECT).dll: $(SRC) $(RESOBJ)
	"$(VS100COMNTOOLS)..\IDE\vcsexpress" $(PROJECT).sln /build release
endif
else
	
$(APSIM)/Model/$(PROJECT).exe: $(SRC)
	"$(VS100COMNTOOLS)..\IDE\devenv" $(PROJECT).sln /build release

$(APSIM)/Model/$(PROJECT).dll: $(SRC) $(RESOBJ)
	"$(VS100COMNTOOLS)..\IDE\devenv" $(PROJECT).sln /build release

endif
	
$(RESOBJ): $(APSIM)/Model/Build/dll.rc
	$(RC) -DPROJ=$(PROJECT) -DMAJOR_VERSION=$(MAJOR_VERSION) -DMINOR_VERSION=$(MINOR_VERSION) -DBUILD_NUMBER=$(BUILD_NUMBER) -fo $@ $<
	
clean:
	$(RM) $(APSIM)\Model\$(PROJECT).exe $(APSIM)\Model\$(PROJECT).dll



