; Inno Setup Compiler 5.5.9


;APSIM setup script

#include  "ISPPBuiltins.iss"

;#ifndef BOOTLEG
;#include <.\ModelExcludes.iss>
;#include <.\UserInterfaceExcludes.iss>
;#endif

#ifndef ModelExcludedFiles
#define ModelExcludedFiles ""
#endif

#ifndef UserInterfaceExcludedFiles
#define UserInterfaceExcludedFiles ""
#endif

#define Major
#define Minor
#define Rev
#define Build
#define AppVerNo=ParseVersion("..\Model\ApsimUI.exe", Major, Minor, Rev, Build) 

#define ApsimVersion Str(Major) + Str(Minor) + "-r" + Str(Rev)
#define GroupString " " + Str(Major) + "." + Str(Minor) + " r" + Str(Rev)

[Setup]
AppName=APSIM
AppVerName=APSIM{#GroupString}
AppPublisher=Apsim Initiative
AppPublisherURL=https://www.apsim.info
OutputBaseFilename=APSIMSetup
VersionInfoVersion={#AppVerNo}
PrivilegesRequired=admin
PrivilegesRequiredOverridesAllowed=dialog
;LicenseFile=..\license.txt
AppVersion={#AppVerNo}
AppID=APSIM{#ApsimVersion}
DefaultDirName={autopf}\APSIM{#ApsimVersion}
DefaultGroupName=APSIM{#GroupString}
UninstallDisplayIcon={app}\ApsimUI.exe
Compression=lzma2/Max
ChangesAssociations=yes
SetupIconFile=..\UserInterface\Images\Apsim.ico
WizardSmallImageFile=..\UserInterface\Images\Apsim.bmp
WizardImageFile=.\APSIMInitiativeBanner.bmp
;InfoBeforeFile=
VersionInfoCompany=APSIM Initiative4
VersionInfoDescription=Apsim Modelling
VersionInfoProductName=Apsim
VersionInfoProductVersion={#AppVerNo}
OutputManifestFile=files.lst

[Code]
function InitializeSetup(): Boolean;
var
  answer: integer;
  ErrorCode: Integer;
begin
    //check for the .net runtime. If it is not found then show a message.
    if not IsDotNetInstalled(net45, 0) then 
    begin
        answer := MsgBox('The Microsoft .NET Framework 4.5 is required.' + #13#10 + #13#10 +
        'Click OK to go to the web site or Cancel to quit', mbInformation, MB_OKCANCEL);        
        result := false;
        if (answer = MROK) then
        begin
          ShellExecAsOriginalUser('open', 'http://www.microsoft.com/en-au/download/details.aspx?id=42643', '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
        end;
    end
    else
      result := true;
end; 

[InstallDelete]
Name: {localappdata}\VirtualStore\Apsim\*.*; Type: filesandordirs
Name: {localappdata}\VirtualStore\Apsim; Type: dirifempty

[Files]
Source: ..\Apsim.xml; DestDir: {app}
Source: ..\UserInterface\*; Excludes: {#UserInterfaceExcludedFiles}; DestDir: {app}\UserInterface; Flags: recursesubdirs
Source: ..\Examples\*; DestDir: {app}\Examples; Flags: recursesubdirs
Source: ..\Documentation\*; DestDir: {app}\Documentation; Flags: recursesubdirs
Source: ..\Model\*.lnk; DestDir: {app}\Model
Source: ..\Model\*.xml; Excludes: {#ModelExcludedFiles}; DestDir: {app}\Model
Source: ..\Model\*.exe; Excludes: {#ModelExcludedFiles}; DestDir: {app}\Model
Source: ..\Model\*.dll; Excludes: {#ModelExcludedFiles}; DestDir: {app}\Model
Source: ..\Model\RunTime\*.dll; DestDir: {app}\Model
Source: ..\Model\DotNetProxies\DotNetProxies.cs; DestDir: {app}\Model\DotNetProxies
Source: ..\Model\TclLink\bin\*; DestDir: {app}\Model\TclLink\bin
Source: ..\Model\TclLink\lib\*; DestDir: {app}\Model\TclLink\lib; Flags: recursesubdirs
Source: ..\Model\TclLink\CIDataTypes.tcl; DestDir: {app}\Model\TclLink

[INI]

[Tasks]
Name: desktopicon; Description: Create a &desktop icon; GroupDescription: Additional icons:
Name: associate; Description: &Associate .apsim files with Apsim; GroupDescription: Other tasks:

[Icons]
Name: "{autodesktop}\APSIM{#GroupString}"; Filename: "{app}\Model\ApsimUI.exe"; Tasks: desktopicon
Name: "{group}\APSIM User Interface"; Filename: "{app}\Model\ApsimUI.exe"
Name: "{group}\APSoil"; Filename: "{app}\Model\ApsimUI.exe"; Parameters: "/Apsoil"; IconFilename: "{app}\UserInterface\Images\Apsoil.ico"
Name: "{group}\Documentation"; Filename: "{app}\Documentation\default.html"; IconFilename: "{app}\UserInterface\Images\book_blue.ico"

[Registry]
;do the associations
Root: HKA; Subkey: "Software\Classes\.apsim"; ValueType: string; ValueName: ""; ValueData: APSIMFile; Flags: uninsdeletevalue; Tasks: associate
Root: HKA; Subkey: "Software\Classes\APSIMFile"; ValueType: string; ValueName: ""; ValueData: APSIM File; Flags: uninsdeletekey; Tasks: associate
Root: HKA; Subkey: "Software\Classes\APSIMFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: {app}\Model\ApsimUI.exe,0; Tasks: associate
Root: HKA; Subkey: "Software\Classes\APSIMFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Model\ApsimUI.exe"" ""%1"""; Tasks: associate

[Run]
Filename: {app}\Model\ApsimUI.exe; Description: Launch APSIM; Flags: postinstall nowait skipifsilent
