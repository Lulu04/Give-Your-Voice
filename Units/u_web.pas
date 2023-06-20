unit u_web;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

// https://forum.lazarus.freepascal.org/index.php/topic,62587.msg473413.html#msg473413
// return True if a new version of the application exists.
// If yes, newVersion contain the... new version and URLLink contains the link
// to the github latest release
function CheckForNewVersionOnGitHub(out newVersion, URLLink: string): boolean;

implementation
uses u_common, fphttpclient, opensslsockets;

function CheckForNewVersionOnGitHub(out newVersion, URLLink: string): boolean;
var
  Client: TFpHttpClient;
  List: TStringList;
begin
  Result := False;
  try
    Client := TfpHttpClient.Create(nil);
    try
      // this is important
      Client.AllowRedirect := true; // I am not sure SimpleGet handles redirects.
      // optional browser impersonation. This is sometimes necessary, although there is a very old default.
      Client.RequestHeaders.Add('User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20100101 Firefox/12.0');
      List := TStringlist.Create;
      try
        List.Text := Client.Get(URL_FOR_VERSION_ON_GITHUB);
        if List.Count < 6 then exit;
        if List.Strings[0] <> '[app]' then exit;
        if List.Strings[1] <> 'Give Your Voice' then exit;
        if List.Strings[2] <> '[version]' then exit;
        if List.Strings[4] <> '[link]' then exit;
        Result := StrComp(PChar(List.Strings[3]), PChar(APP_VERSION)) > 0;
        if Result then begin
          newVersion := List.Strings[3];
          URLLink := List.Strings[5];
        end;
      finally
        List.Free;
      end;
    finally
      Client.Free;
    end;
  except
  end;
end;

end.

