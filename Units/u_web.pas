unit u_web;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TResultCheckOnlineVersion = (rcovErrorAccessingInternet,
                               rcovNoNewVersion,
                               rcovNewVersionAvailable,
                               rcovErrorNoTempFolder);

// https://forum.lazarus.freepascal.org/index.php/topic,62587.msg473413.html#msg473413
// return True if a new version of the application exists.
// If yes, newVersion contain the new version
function CheckForNewVersionOnGitHub(out newVersion: string): TResultCheckOnlineVersion;

implementation
uses u_common, u_logfile,
{$ifdef Darwin}
  Process, UTF8Process, u_project, utilitaire_fichier
{$else}
  fphttpclient, opensslsockets
{$endif};


{$ifdef Darwin}
function CheckForNewVersionOnGitHub(out newVersion: string): TResultCheckOnlineVersion;
var process: TProcessUTF8;
  f: string;
  t: TStringList;
begin
  Result := rcovErrorNoTempFolder;
  newVersion := '';

  if not Project.TempFolderExists then exit;

  f := Project.TempFolder+'loadedversion';
  try
    try
      process := TProcessUTF8.Create(NIL);
      process.ParseCmdLine('curl --max-time 8 -o "'+f+'" '+URL_FOR_VERSION_ON_GITHUB);
      process.Options := process.Options+[poWaitOnExit, poNoConsole];
      process.Execute;
    finally
      process.Free;
    end;
    if not FileExists(f) then begin
      Result := rcovErrorAccessingInternet;
      Log.Error('gyv: check for new version fail');
      Log.Mess('curl failed to download version file', 1);
      exit;
    end;

    t := TStringList.Create;
    try
      t.LoadFromFile(f);
      if t.Count = 0 then exit;
      newVersion := t.Strings[0];
      if StrComp(PChar(newVersion), PChar(APP_VERSION)) > 0 then begin
        Result := rcovNewVersionAvailable;
        Log.Info('gyv: found new app version: '+newVersion);
      end else Result := rcovNoNewVersion;
    finally
      t.Free;
    end;
  except
    on E: Exception do begin
      Log.Warning('gyv: check for new version fail'+LineEnding+
                  '    '+E.Message);
      Result := rcovErrorAccessingInternet;
    end;
  end;

  if FichierExistant(f) then SupprimeFichier(f);
end;

{$else}

function CheckForNewVersionOnGitHub(out newVersion: string): TResultCheckOnlineVersion;
var Client: TFpHttpClient;
begin
  Result := rcovNoNewVersion;
  newVersion := '';
  try
    Client := TfpHttpClient.Create(nil);
    try
      // this is important
      Client.AllowRedirect := true; // I am not sure SimpleGet handles redirects.
      // optional browser impersonation. This is sometimes necessary, although there is a very old default.
      Client.RequestHeaders.Add('User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20100101 Firefox/12.0');
      newVersion := Client.Get(URL_FOR_VERSION_ON_GITHUB);
      if newVersion = '' then exit;
      if StrComp(PChar(newVersion), PChar(APP_VERSION)) > 0 then begin
        Result := rcovNewVersionAvailable;
        Log.Info('gyv: found new app version: '+newVersion);
      end;
    finally
      Client.Free;
    end;
  except
    on E: Exception do begin
      Log.Warning('gyv: check for new version fail'+LineEnding+
                  '    '+E.Message);
      Result := rcovErrorAccessingInternet;
    end;
  end;
end;
{$endif}

end.

