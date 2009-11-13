

#-----------------------------------------------------------------------
# The rules
ifeq ($(PROJECTTYPE),exe)
all: $(APSIM)/Model/$(PROJECT).exe
else
all: $(APSIM)/Model/$(PROJECT).dll
endif


$(APSIM)/Model/$(PROJECT).exe: $(PREBUILD)
	"C:\Program Files\Microsoft Visual Studio 9.0\Common7\IDE\devenv" $(PROJECT).sln /build release 


$(APSIM)/Model/$(PROJECT).dll: $(PREBUILD)
	"C:\Program Files\Microsoft Visual Studio 9.0\Common7\IDE\devenv" $(PROJECT).sln /build release 

clean:
	$(RM) $(APSIM)\Model\$(PROJECT).exe $(APSIM)\Model\$(PROJECT).dll



