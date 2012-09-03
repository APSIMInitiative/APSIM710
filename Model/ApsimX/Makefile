include $(APSIM)/Model/Build/Platform.make

PROJECT = ApsimX

PROJECTTYPE = exe

all:  $(APSIM)/Model/ApsimX.exe \
      $(APSIM)/Model/Plant2X.dll \
      $(APSIM)/Model/Manager2X.dll \
      $(APSIM)/Model/SoilWaterX.dll \
      $(APSIM)/Model/SoilNitrogenX.dll \
      $(APSIM)/Model/ReportDbX.dll \
      $(APSIM)/Model/SurfaceOrganicMatterX.dll \
      $(APSIM)/Model/MicroClimateX.dll

$(APSIM)/Model/ApsimX.exe: $(APSIM)/Model/ApsimX/ApsimX.csproj
ifeq ($(PLATFORM),Linux)
	xbuild $(APSIM)/Model/ApsimX/ApsimX.sln /target:Build
else
	"$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/ApsimX/ApsimX.sln /build release
endif

$(APSIM)/Model/Plant2X.dll: $(APSIM)/Model/ApsimX.exe $(APSIM)/Model/Plant2/Plant2.csproj 
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/Plant2/Plant2.csproj
ifeq ($(PLATFORM),Linux)
	xbuild $(APSIM)/Model/Plant2/Plant2X.csproj /target:Build
else
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/Plant2/Plant2X.csproj /build release
endif

$(APSIM)/Model/Manager2X.dll: $(APSIM)/Model/ApsimX.exe $(APSIM)/Model/Manager2/Manager2.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/Manager2/Manager2.csproj
ifeq ($(PLATFORM),Linux)
	xbuild $(APSIM)/Model/Manager2/Manager2X.csproj /target:Build
else
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/Manager2/Manager2X.csproj /build release
endif

$(APSIM)/Model/SoilWaterX.dll: $(APSIM)/Model/ApsimX.exe $(APSIM)/Model/SoilWater/SoilWater.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/SoilWater/SoilWater.csproj
ifeq ($(PLATFORM),Linux)
	xbuild $(APSIM)/Model/SoilWater/SoilWaterX.csproj /target:Build
else
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/SoilWater/SoilWaterX.csproj /build release
endif

$(APSIM)/Model/ReportDbX.dll: $(APSIM)/Model/ApsimX.exe $(APSIM)/Model/ReportDb/ReportDb.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/ReportDb/ReportDb.csproj
ifeq ($(PLATFORM),Linux)
	xbuild $(APSIM)/Model/ReportDb/ReportDbX.csproj /target:Build
else
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/ReportDb/ReportDbX.csproj /build release
endif

$(APSIM)/Model/SoilNitrogenX.dll: $(APSIM)/Model/ApsimX.exe $(APSIM)/Model/SoilNitrogen/SoilNitrogen.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/SoilNitrogen/SoilNitrogen.csproj
ifeq ($(PLATFORM),Linux)
	xbuild $(APSIM)/Model/SoilNitrogen/SoilNitrogenX.csproj /target:Build
else
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/SoilNitrogen/SoilNitrogenX.csproj /build release	
endif

$(APSIM)/Model/SurfaceOrganicMatterX.dll: $(APSIM)/Model/ApsimX.exe $(APSIM)/Model/SurfaceOrganicMatter/SurfaceOrganicMatter.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/SurfaceOrganicMatter/SurfaceOrganicMatter.csproj
ifeq ($(PLATFORM),Linux)
	xbuild $(APSIM)/Model/SurfaceOrganicMatter/SurfaceOrganicMatterX.csproj /target:Build
else
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/SurfaceOrganicMatter/SurfaceOrganicMatterX.csproj /build release	
endif

$(APSIM)/Model/MicroClimateX.dll: $(APSIM)/Model/ApsimX.exe $(APSIM)/Model/MicroClimate/MicroClimate.vbproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/MicroClimate/MicroClimate.vbproj
ifeq ($(PLATFORM),Linux)
	xbuild $(APSIM)/Model/MicroClimate/MicroClimateX.vbproj /target:Build
else
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/MicroClimate/MicroClimateX.vbproj /build release
endif

include $(APSIM)/Model/Build/$(PLATFORM)DOTNET.make
