unit u_web;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TResultCheckOnlineVersion = (rcovErrorAccessingInternet,
                               rcovNoNewVersion,
                               rcovNewVersionAvailable);

// https://forum.lazarus.freepascal.org/index.php/topic,62587.msg473413.html#msg473413
// return True if a new version of the application exists.
// If yes, newVersion contain the new version
function CheckForNewVersionOnGitHub(out newVersion: string): TResultCheckOnlineVersion;

implementation
uses u_common, fphttpclient, opensslsockets, u_logfile;

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

end.

