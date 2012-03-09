include $(APSIM)/Model/Build/Platform.make

PROJECT = ApsimX

PROJECTTYPE = exe

everything: $(APSIM)/Model/ApsimX.exe $(APSIM)/Model/ModifyProjectForApsimX.exe \
			$(APSIM)/Model/Plant2X.dll \
			$(APSIM)/Model/Manager2X.dll \
			$(APSIM)/Model/SoilWaterX.dll \
			$(APSIM)/Model/ReportDbX.dll \
			$(APSIM)/Model/SoilNitrogenX.dll
			

$(APSIM)/Model/ApsimX.exe $(APSIM)/Model/ModifyProjectForApsimX.exe:
	"$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/ApsimX/ApsimX.sln /build release

$(APSIM)/Model/Plant2X.dll: $(APSIM)/Model/Plant2.dll
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/Plant2/Plant2.csproj
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/Plant2/Plant2X.csproj /build release

$(APSIM)/Model/Manager2X.dll: $(APSIM)/Model/Manager2.dll
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/Manager2/Manager2.csproj
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/Manager2/Manager2X.csproj /build release

$(APSIM)/Model/SoilWaterX.dll: $(APSIM)/Model/SoilWater.dll
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/SoilWater/SoilWater.csproj
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/SoilWater/SoilWaterX.csproj /build release

$(APSIM)/Model/ReportDbX.dll: $(APSIM)/Model/ReportDb.dll
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/ReportDb/ReportDb.csproj
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/ReportDb/ReportDbX.csproj /build release

$(APSIM)/Model/SoilNitrogenX.dll: $(APSIM)/Model/SoilNitrogen.dll
	$(RUN) $(APSIM)/Model/ModifyProjectForApsimX.exe $(APSIM)/Model/SoilNitrogen/SoilNitrogen.csproj
	$(RUN) "$(VS100COMNTOOLS)..\IDE\devenv" $(APSIM)/Model/SoilNitrogen/SoilNitrogenX.csproj /build release	

include $(APSIM)/Model/Build/$(PLATFORM)DOTNET.make
