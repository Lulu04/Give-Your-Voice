unit u_crossplatform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms;

// return the path of 'Data' folder in a cross platform way
function GetAppDataFolder: string;
// return the path of 'languages' folder in a cross platform way
function GetAppLanguagesFolder: string;

// return the default path of the project folder in a cross platform way
// Windows: MesDocument\GiveYourVoice\
// Linux:   /home/username/Documents/GiveYourVoice/
//       or /home/username/GiveYourVoice/
// Mac: /Users/username/GiveYourVoice/
function GetAppDefaultProjectFolder: string;
procedure CreateDefaultProjectFolder;

// initialize the subfolder where openalsoft and libsndfile are located, in a cross platform way
procedure InitALSManagerLibrariesSubFolder;

// return os name
function OSName: string;

// Return TRUE if Key is F1 (Windows/Linux) or CONTROL+H (MacOS)
// if it is, Key is redefined to VK_UNKNOWN
function CheckKeyToShowUserGuide(var Key: Word; Shift: TShiftState): boolean;

implementation
uses ALSound, u_common, utilitaire_fichier, LCLType
  {$ifdef Darwin},LazFileUtils{$endif};

{$ifdef Darwin}
function APPIsRunningFromIDE: boolean;
begin
  Result := False;
  if ParamCount > 0 then
    Result := ParamStr(1)='AppIsRunningFromIDE';
end;

function GetAppBundlePath: string;
var f, bundleName: string;
  i: SizeInt;
begin
  {$ifdef LCL}
  f := Application.Location;
  {$else}
  f := ExtractFilePath(ParamStr(0));
  {$endif}
  bundleName := '/'+ApplicationName+'.app';
  i := Pos(bundleName, f);
  if i > 0 then
    Result := ConcatPaths([copy(f, 1, i-1), bundleName])
  else
    Result := f;

  Result := IncludeTrailingPathDelimiter(Result);
end;
{$endif}

function GetAppDataFolder: string;
begin
  {$if defined(Windows)}
    Result := IncludeTrailingPathDelimiter(Application.Location)+'Data\';
  {$elseif defined(Linux)}
    Result := IncludeTrailingPathDelimiter(Application.Location)+'Data/';
  {$elseif defined(Darwin)}
    if APPIsRunningFromIDE then
      Result := CleanAndExpandDirectory(GetAppBundlePath+'../Data/')
    else
      Result := GetAppBundlePath+'Contents/Resources/Data/'
  {$else}
    {$error You can not compile this program on this platform, sorry !}
  {$endif}
end;

function GetAppLanguagesFolder: string;
begin
 {$if defined(Windows)}
   Result := IncludeTrailingPathDelimiter(Application.Location)+'languages\';
 {$elseif defined(Linux)}
   Result := IncludeTrailingPathDelimiter(Application.Location)+'languages/';
 {$elseif defined(Darwin)}
   if APPIsRunningFromIDE then
     Result := CleanAndExpandDirectory(GetAppBundlePath+'../languages/')
   else
     Result := GetAppBundlePath+'Contents/Resources/languages/'
 {$endif}
end;

function GetAppDefaultProjectFolder: string;
begin
  {$if defined(Windows)}
   Result := IncludeTrailingPathDelimiter(RepertoireUserMesDocuments+FOLDER_FOR_PROJECT);
 {$elseif defined(Linux)}
   Result := IncludeTrailingPathDelimiter(GetUserDir);
   if DirectoryExists(Result+'Documents') then
     Result := IncludeTrailingPathDelimiter(Result+'Documents');
   Result := IncludeTrailingPathDelimiter(Result+FOLDER_FOR_PROJECT);
 {$elseif defined(Darwin)}
   Result := IncludeTrailingPathDelimiter(GetUserDir);
   Result := IncludeTrailingPathDelimiter(Result+FOLDER_FOR_PROJECT);
 {$else}
   ALSManager.SetLibrariesSubFolder('x86_64-macos/');
 {$endif}
end;

procedure CreateDefaultProjectFolder;
begin
 if not RepertoireExistant(GetAppDefaultProjectFolder) then
   CreerRepertoire(GetAppDefaultProjectFolder);
end;

procedure InitALSManagerLibrariesSubFolder;
begin
  {$if defined(Windows) and defined(cpu386)}
      ALSManager.SetLibrariesSubFolder('i386-win32\');
  {$elseif defined(Windows) and defined(cpux86_64)}
      ALSManager.SetLibrariesSubFolder('x86_64-win64\');
  {$elseif defined(Linux) and defined(cpu386)}
      ALSManager.SetLibrariesSubFolder('i386-linux/');
  {$elseif defined(Linux) and defined(cpux86_64)}
      ALSManager.SetLibrariesSubFolder('x86_64-linux/');
  {$elseif defined(Darwin) and defined(cpux86_64)}
      ALSManager.SetLibrariesSubFolder('x86_64-macos/');
  {$else}
    {$error You can not compile this program on this platform, sorry !}
  {$endif}
end;

function OSName: string;
begin
  Result := '';
  {$if defined(Windows) and defined(cpu386)}
     Result := 'i386-win32';
  {$elseif defined(Windows) and defined(cpux86_64)}
     Result := 'x86_64-win64';
  {$elseif defined(Linux) and defined(cpu386)}
     Result := 'i386-linux';
  {$elseif defined(Linux) and defined(cpux86_64)}
     Result := 'x86_64-linux';
  {$elseif defined(Darwin)}
     Result := 'MacOS 64';
  {$endif}
end;

function CheckKeyToShowUserGuide(var Key: Word; Shift: TShiftState): boolean;
begin
  Result := False;
  {$if defined(Windows) or defined(Linux)}
  Result := Key = VK_F1;
  {$elseif defined(Darwin)}
  Result := (ssCtrl in Shift) and (Key = VK_H);
  {$endif}
  if Result then
    Key := VK_UNKNOWN;
end;

end.

