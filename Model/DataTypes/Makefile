include $(APSIM)/Model/Build/Platform.make

PROJECT = ProcessDataTypesInterface

PROJECTTYPE = exe

ifeq ($(PLATFORM),Linux)
RUN = mono
endif	

everything: $(APSIM)/Model/ProcessDataTypesInterface.exe $(APSIM)/Model/ComponentInterface2/DataTypes.h $(APSIM)/Model/ComponentInterface/DataTypes.h DOTNETDataTypes.cs DataTypes.cs

$(APSIM)/Model/ComponentInterface2/DataTypes.h: Datatypes.xml CI2DataTypes.macro
	$(RUN) $(APSIM)/Model/ProcessDataTypesInterface.exe Datatypes.xml CI2DataTypes.macro

$(APSIM)/Model/ComponentInterface/DataTypes.h: Datatypes.xml CIDataTypes.macro
	$(RUN) $(APSIM)/Model/ProcessDataTypesInterface.exe Datatypes.xml CIDataTypes.macro 

DOTNETDataTypes.cs:  Datatypes.xml CSDOTNETDataTypes.macro
	$(RUN) $(APSIM)/Model/ProcessDataTypesInterface.exe Datatypes.xml CSDOTNETDataTypes.macro

DataTypes.cs:  Datatypes.xml CSDataTypes.macro
	$(RUN) $(APSIM)/Model/ProcessDataTypesInterface.exe Datatypes.xml CSDataTypes.macro
	
include $(APSIM)/Model/Build/$(PLATFORM)DOTNET.make

cleanall: 
	rm -f DOTNETDataTypes.cs \
      DataTypes.cs \
	  $(APSIM)/Model/ProcessDataTypesInterface.exe \
	  $(APSIM)/Model/ComponentInterface2/DataTypes.h \
	  $(APSIM)/Model/ComponentInterface/DataTypes.h

