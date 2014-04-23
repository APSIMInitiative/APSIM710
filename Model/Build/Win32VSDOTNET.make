RC=rc

#-----------------------------------------------------------------------
# The rules

include $(APSIM)\Model\Build\VersionInfo.make

ifeq ($(PROJECTTYPE),exe)
   all: $(APSIM)/Model/$(PROJECT).exe
else
   all: $(APSIM)/Model/$(PROJECT).dll
endif

# Try to determine whether vcsexpress.exe (the Visual Studio Express module) exists
# If it doesn't we'll assume it's the Professional version
# This isn't as easy as it should be. We can use the make wildcard function to determine
# if a file exists, but first we need to convert backslashes to slashes, and quote any spaces

# Be careful with the following set of lines. Some of the white space is very significant!
empty :=
space := $(empty) $(empty)

EXP1 :=$(subst \,/,$(VS100COMNTOOLS)..\IDE\vcsexpress.exe)
EXP2 := $(subst $(space),\ ,$(EXP1))
EXPRESS=$(wildcard $(EXP2))

ifeq ($(EXPRESS),) 

$(APSIM)/Model/$(PROJECT).exe: $(SRC)
	"$(VS100COMNTOOLS)..\IDE\devenv" $(PROJECT).sln /build release

$(APSIM)/Model/$(PROJECT).dll: $(SRC) $(RESOBJ)
	"$(VS100COMNTOOLS)..\IDE\devenv" $(PROJECT).sln /build release

else	
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
	
endif
	
$(RESOBJ): $(APSIM)/Model/Build/dll.rc
	$(RC) -DPROJ=$(PROJECT) -DMAJOR_VERSION=$(MAJOR_VERSION) -DMINOR_VERSION=$(MINOR_VERSION) -DBUILD_NUMBER=$(BUILD_NUMBER) -fo $@ $<
	
clean:
	$(RM) $(APSIM)\Model\$(PROJECT).exe $(APSIM)\Model\$(PROJECT).dll
