include $(APSIM)/Model/Build/Platform.make

PROJECT = ProcessDataTypesInterface

PROJECTTYPE = exe

everything: $(APSIM)/Model/ProcessDataTypesInterface.exe $(APSIM)/Model/ComponentInterface2/DataTypes.h DOTNETDataTypes.h $(APSIM)/Model/ComponentInterface/DataTypes.h

$(APSIM)/Model/ComponentInterface2/DataTypes.h: Datatypes.xml CI2DataTypes.macro
	$(APSIM)/Model/ProcessDataTypesInterface.exe Datatypes.xml CI2DataTypes.macro

DOTNETDataTypes.h:  Datatypes.xml DOTNETDataTypes.macro
	$(APSIM)/Model/ProcessDataTypesInterface.exe Datatypes.xml DOTNETDataTypes.macro

$(APSIM)/Model/ComponentInterface/DataTypes.h: Datatypes.xml CIDataTypes.macro
	$(APSIM)/Model/ProcessDataTypesInterface.exe Datatypes.xml CIDataTypes.macro 

include $(APSIM)/Model/Build/$(PLATFORM)DOTNET.make


