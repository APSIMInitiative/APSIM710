include $(APSIM)/Model/Build/Platform.make

PROJECT = Cotton

PROJECTTYPE = dll


dummy:
	cmd /c "c:\Program Files\7-Zip\7z.exe" e .\Cotton.7z < c:\CottonPassword.txt
	"$(FrameworkDIR32)$(FrameworkVersion32)\msbuild" $(PROJECT).sln /target:Build /p:Configuration=Release
	del *.cs
	del *.sln
	del *.csproj
