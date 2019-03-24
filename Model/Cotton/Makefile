include $(APSIM)/Model/Build/Platform.make

PROJECT = Cotton

PROJECTTYPE = dll


dummy:
	cmd /c 7z e Cotton.7z -aoa < C:\CottonPassword.txt
	msbuild $(PROJECT).sln /target:Build /p:Configuration=Release
	del *.cs
	del *.sln
	del *.csproj
