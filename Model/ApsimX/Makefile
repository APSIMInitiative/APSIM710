include $(APSIM)/Model/Build/Platform.make

PROJECT = ApsimX

PROJECTTYPE = exe

			
$(APSIM)/Model/ApsimX.exe: $(APSIM)/Model/ModifyProjectForApsimX.exe \
                           $(APSIM)/Model/Plant2/Plant2X.csproj \
                           $(APSIM)/Model/Manager2/Manager2X.csproj \
                           $(APSIM)/Model/SoilWater/SoilWaterX.csproj \
                           $(APSIM)/Model/SoilNitrogen/SoilNitrogenX.csproj \
                           $(APSIM)/Model/ReportDb/ReportDbX.csproj \
                           $(APSIM)/Model/SurfaceOrganicMatter/SurfaceOrganicMatterX.csproj \
                           $(APSIM)/Model/MicroClimate/MicroClimateX.vbproj
	"$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/ApsimX/ApsimX.sln /build release

$(APSIM)/Model/ModifyProjectForApsimX.exe: $(APSIM)/Model/ApsimX/ModifyProjectForApsimX/ModifyProjectForApsimX.cs
	"$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/ApsimX/ModifyProjectForApsimX/ModifyProjectForApsimX.csproj /build release
   
$(APSIM)/Model/Plant2/Plant2X.csproj: $(APSIM)/Model/Plant2/Plant2.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/Plant2/Plant2.csproj

$(APSIM)/Model/Manager2/Manager2X.csproj: $(APSIM)/Model/Manager2/Manager2.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/Manager2/Manager2.csproj

$(APSIM)/Model/SoilWater/SoilWaterX.csproj: $(APSIM)/Model/SoilWater/SoilWater.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/SoilWater/SoilWater.csproj

$(APSIM)/Model/ReportDb/ReportDbX.csproj: $(APSIM)/Model/ReportDb/ReportDb.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/ReportDb/ReportDb.csproj

$(APSIM)/Model/SoilNitrogen/SoilNitrogenX.csproj: $(APSIM)/Model/SoilNitrogen/SoilNitrogen.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/SoilNitrogen/SoilNitrogen.csproj

$(APSIM)/Model/SurfaceOrganicMatter/SurfaceOrganicMatterX.csproj: $(APSIM)/Model/SurfaceOrganicMatter/SurfaceOrganicMatter.csproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/SurfaceOrganicMatter/SurfaceOrganicMatter.csproj

$(APSIM)/Model/MicroClimate/MicroClimateX.vbproj: $(APSIM)/Model/MicroClimate/MicroClimate.vbproj
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/MicroClimate/MicroClimate.vbproj

include $(APSIM)/Model/Build/$(PLATFORM)DOTNET.make
