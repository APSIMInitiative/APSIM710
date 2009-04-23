@echo off
echo ^<html^> > Index.html
echo ^<head^> >> Index.html
echo    ^<title^>APSIM Documentation^</title^> >> Index.html
echo    ^<link rel="stylesheet" href="ApsimWebStyle.css" type="text/css"^> >> Index.html
echo ^</head^> >> Index.html
echo ^<body^> >> Index.html

set DOCPATH=
for /R . %%d in (.) do (
   rem ---------------------------------------------
   rem Look for at least 1 .htm file. If found then
   rem write the directory name with <h2> around it.
   rem ---------------------------------------------
   if NOT "%%~nd" == "Documentation" (
      set DOCPATH=%%~nd/
      dir /B "%%d\*.htm" >nul 2>temp1
      find "File Not Found" temp1 >nul
      if ERRORLEVEL 1 echo    ^<h2^>%%~nd^</h2^> >> Index.html
      if ERRORLEVEL 1 echo    ^<ul^> >> Index.html
   ) else (
      echo    ^<h1^>APSIM Documentation^</h1^> >> Index.html
      )

   rem ---------------------------------------------
   rem For each .htm file found in the directory 
   rem write a href for it.
   rem ---------------------------------------------
   for %%f in ("%%d\*.htm") do if NOT "%%~nf"=="Index" echo       ^<li^>^<a href="!DOCPATH!%%~nf.htm"^>%%~nf^</a^>^</li^> >> Index.html
   if ERRORLEVEL 1 echo    ^</ul^> >> Index.html
   )

echo ^</body^>^</html^> >> Index.html
del temp1
set DOCPATH =
