
rem ----- Setup the Visual Studio 2015 compiler tools
call "%VS140COMNTOOLS%..\..\VC\vcvarsall.bat"

rem ----- Run Bob.cs
Model\cscs.exe   Model\Build\Bob.cs