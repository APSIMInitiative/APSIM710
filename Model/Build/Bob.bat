
rem ----- Setup the Visual Studio 2010 compiler tools
if "%LIBPATH%" == "" call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"

rem ----- Run Bob.cs
Model\cscs.exe   Model\Build\Bob.cs   Model\Build\BobMain.cs