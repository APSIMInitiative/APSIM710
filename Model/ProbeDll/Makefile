include $(APSIM)/Model/Build/Platform.make

PROJECT = ProbeDll

PROJECTTYPE = dll
RC = rc
RESOBJ = dllres.res


#-----------------------------------------------------------------------
# The rules
all: $(APSIM)/Model/$(PROJECT).exe


# For some reason if we do a release build, then ProbeDLL throws an exception when probing
# the old PLANT model.

$(APSIM)/Model/$(PROJECT).exe: $(SRC) $(APSIM)/Model/$(RESOBJ)
	$(VS100COMNTOOLS)\..\IDE\devenv $(PROJECT).sln /build debug

$(APSIM)/Model/$(RESOBJ): $(APSIM)/Model/Build/dll.rc
	$(RC) -DPROJ=$(PROJECT) -DMAJOR_VERSION=$(MAJOR_VERSION) -DMINOR_VERSION=$(MINOR_VERSION) -DBUILD_NUMBER=$(BUILD_NUMBER) -fo $@ $<

clean:
	$(RM) $(APSIM)\Model\$(PROJECT).exe $(APSIM)\Model\$(PROJECT).dll $(APSIM)\Model\$(RESOBJ)



