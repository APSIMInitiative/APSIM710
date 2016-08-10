include $(APSIM)/Model/Build/Platform.make

PROJECT = UpdateDotNetProxies

PROJECTTYPE = exe

ifeq ($(PLATFORM),WIN32VS)
# Don't know why this is needed, but we need to explicitly set 32 bit mode
# even when the .sln file tells it to. Without this, it runs as a 64 bit
# process, and will fail to probe all the 32 bit c++/fortran dlls.
dot32: $(APSIM)/Model/$(PROJECT).exe
	"$(WindowsSDK_ExecutablePath_x86)\CorFlags.exe" $(APSIM)\Model\$(PROJECT).exe /32Bit+
endif

include $(APSIM)/Model/Build/$(PLATFORM)DOTNET.make
