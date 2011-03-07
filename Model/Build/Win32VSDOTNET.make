RC=rc

#-----------------------------------------------------------------------
# The rules
ifeq ($(PROJECTTYPE),exe)
all: $(APSIM)/Model/$(PROJECT).exe
else
# Normally these are obtained as environment variables, but we want to be sure they are not left undefined
ifeq ($(MAJOR_VERSION),)
  MAJOR_VERSION = 1
  MINOR_VERSION = 0
  BUILD_NUMBER = 0
endif

all: $(APSIM)/Model/$(PROJECT).dll

endif


$(APSIM)/Model/$(PROJECT).exe: $(SRC)
	"$(VS90COMNTOOLS)..\IDE\devenv" $(PROJECT).sln /build release


$(APSIM)/Model/$(PROJECT).dll: $(SRC) $(RESOBJ)
	"$(VS90COMNTOOLS)..\IDE\devenv" $(PROJECT).sln /build release

$(RESOBJ): $(APSIM)/Model/Build/dll.rc
	$(RC) -DPROJ=$(PROJECT) -DMAJOR_VERSION=$(MAJOR_VERSION) -DMINOR_VERSION=$(MINOR_VERSION) -DBUILD_NUMBER=$(BUILD_NUMBER) -fo $@ $<
	
clean:
	$(RM) $(APSIM)\Model\$(PROJECT).exe $(APSIM)\Model\$(PROJECT).dll



