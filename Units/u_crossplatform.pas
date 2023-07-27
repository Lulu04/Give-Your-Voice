unit u_crossplatform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms;

// return the path of Application_Location/data folder in a cross platform way
function GetAppDataFolder: string;

// return the default path of the project folder in a cross platform way
// Windows: MesDocument\GiveYourVoice\
// Linux: UserDir/Documents/GiveYourVoice/
//        or UserDir/GiveYourVoice/
// Mac: not yet implemented
function GetAppDefaultProjectFolder: string;
procedure CreateDefaultProjectFolder;

// initialize the subfolder where openalsoft and libsndfile are located, in a cross platform way
procedure InitALSManagerLibrariesSubFolder;

// return os name
function OSName: string;

implementation
uses ALSound, u_common, utilitaire_fichier;

function GetAppDataFolder: string;
begin
  {$if defined(Windows)}
    Result := IncludeTrailingPathDelimiter(Application.Location)+'Data\';
  {$elseif defined(Linux)}
    Result := IncludeTrailingPathDelimiter(Application.Location)+'Data/';
  {$elseif defined(Darwin)}
    Raise Exception.Create('GetAppDataFolder: Not yet implemented on MAC');
  {$else}
    {$error You can not compile this program on this platform, sorry !}
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
   raise Exception.Create('GetAppDefaultProjectFolder: not yet implemented on MAC');
   Result := IncludeTrailingPathDelimiter(GetUserDir);
   Result := IncludeTrailingPathDelimiter(Result+FOLDER_FOR_PROJECT);
 {$else}
   {$error You can not compile this program on this platform, sorry !}
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
  {$elseif defined(Darwin)}
    Raise Exception.Create('InitALSManagerLibrariesSubFolder: not yet implemented on MAC');
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

end.

