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
AppPublisherURL=http://www.apsim.au
OutputBaseFilename=APSIMSetup
VersionInfoVersion={#AppVerNo}
PrivilegesRequired=poweruser
;LicenseFile=..\license.txt
AppVersion={#AppVerNo}
AppID=APSIM{#ApsimVersion}
DefaultDirName={pf}\APSIM{#ApsimVersion}
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

type
  //
  // Enumeration used to specify a .NET framework version 
  //
  TDotNetFramework = (
    DotNet_v11_4322,  // .NET Framework 1.1
    DotNet_v20_50727, // .NET Framework 2.0
    DotNet_v30,       // .NET Framework 3.0
    DotNet_v35,       // .NET Framework 3.5
    DotNet_v4_Client, // .NET Framework 4.0 Client Profile
    DotNet_v4_Full,   // .NET Framework 4.0 Full Installation
    DotNet_v45);      // .NET Framework 4.5

//
// Checks whether the specified .NET Framework version and service pack
// is installed (See: http://www.kynosarges.de/DotNetVersion.html)
//
// Parameters:
//   Version     - Required .NET Framework version
//   ServicePack - Required service pack level (0: None, 1: SP1, 2: SP2 etc.)
//
function IsDotNetInstalled(Version: TDotNetFramework; ServicePack: cardinal): boolean;
  var
    KeyName      : string;
    Success      : boolean;
    InstallFlag  : cardinal; 
    ReleaseVer   : cardinal;
    ServiceCount : cardinal;
  begin
    // Registry path for the requested .NET Version
    KeyName := 'SOFTWARE\Microsoft\NET Framework Setup\NDP\';

    case Version of
      DotNet_v11_4322:  KeyName := KeyName + 'v1.1.4322';
      DotNet_v20_50727: KeyName := KeyName + 'v2.0.50727';
      DotNet_v30:       KeyName := KeyName + 'v3.0';
      DotNet_v35:       KeyName := KeyName + 'v3.5';
      DotNet_v4_Client: KeyName := KeyName + 'v4\Client';
      DotNet_v4_Full:   KeyName := KeyName + 'v4\Full';
      DotNet_v45:       KeyName := KeyName + 'v4\Full';
    end;

    // .NET 3.0 uses "InstallSuccess" key in subkey Setup
    if (Version = DotNet_v30) then
      Success := RegQueryDWordValue(HKLM, KeyName + '\Setup', 'InstallSuccess', InstallFlag) else
      Success := RegQueryDWordValue(HKLM, KeyName, 'Install', InstallFlag);

    // .NET 4.0/4.5 uses "Servicing" key instead of "SP"
    if (Version = DotNet_v4_Client) or
       (Version = DotNet_v4_Full) or
       (Version = DotNet_v45) then
      Success := Success and RegQueryDWordValue(HKLM, KeyName, 'Servicing', ServiceCount) else
      Success := Success and RegQueryDWordValue(HKLM, KeyName, 'SP', ServiceCount);

    // .NET 4.5 is distinguished from .NET 4.0 by the Release key
    if (Version = DotNet_v45) then
      begin
        Success := Success and RegQueryDWordValue(HKLM, KeyName, 'Release', ReleaseVer);
        Success := Success and (ReleaseVer >= 378389);
      end;

    Result := Success and (InstallFlag = 1) and (ServiceCount >= ServicePack);
  end;

// this is the main function that detects the required version
function IsRequiredDotNetDetected(): Boolean;  
begin
    result := IsDotNetInstalled(DotNet_v45, 0);
end;

function InitializeSetup(): Boolean;
var
  answer: integer;
  ErrorCode: Integer;
begin
    //check for the .net runtime. If it is not found then show a message.
    if not IsRequiredDotNetDetected() then 
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
Name: desktopicon\common; Description: For all users; GroupDescription: Additional icons:; Flags: exclusive
Name: desktopicon\user; Description: For the current user only; GroupDescription: Additional icons:; Flags: exclusive unchecked
Name: associate; Description: &Associate .apsim files with Apsim; GroupDescription: Other tasks:

[Icons]
;Name: "{commonprograms}\APSIM{#GroupString}"; Filename: "{app}\Model\ApsimUI.exe"
Name: "{userdesktop}\APSIM{#GroupString}"; Filename: "{app}\Model\ApsimUI.exe"; Tasks: desktopicon\user
Name: "{commondesktop}\APSIM{#GroupString}"; Filename: "{app}\Model\ApsimUI.exe"; Tasks: desktopicon\common
Name: "{group}\APSIM User Interface"; Filename: "{app}\Model\ApsimUI.exe"
Name: "{group}\APSoil"; Filename: "{app}\Model\ApsimUI.exe"; Parameters: "/Apsoil"; IconFilename: "{app}\UserInterface\Images\Apsoil.ico"
Name: "{group}\Documentation"; Filename: "{app}\Documentation\default.html"; IconFilename: "{app}\UserInterface\Images\book_blue.ico"

[Registry]
;do the associations
Root: HKCR; Subkey: .apsim; ValueType: string; ValueName: ; ValueData: APSIM File; Flags: uninsdeletevalue; Tasks: associate
Root: HKCR; Subkey: APSIM File; ValueType: string; ValueName: ; ValueData: My Program File; Flags: uninsdeletekey; Tasks: associate
Root: HKCR; Subkey: APSIM File\DefaultIcon; ValueType: string; ValueName: ; ValueData: {app}\Model\ApsimUI.exe,0; Tasks: associate
Root: HKCR; Subkey: APSIM File\shell\open\command; ValueType: string; ValueName: ; ValueData: """{app}\Model\ApsimUI.exe"" ""%1"""; Tasks: associate

[Run]
Filename: {app}\Model\ApsimUI.exe; Description: Launch APSIM; Flags: postinstall nowait skipifsilent
