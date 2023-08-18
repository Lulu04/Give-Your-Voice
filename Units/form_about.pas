unit form_about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, BGRABitmap, BGRABitmapTypes;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    PaintBox1: TPaintBox;
    Shape1: TShape;
    BDonate: TSpeedButton;
    BClose: TSpeedButton;
    procedure BCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BDonateClick(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Label8MouseEnter(Sender: TObject);
    procedure Label8MouseLeave(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FLogo: TBGRABitmap;
    procedure AdjustFont;
  public

  end;

var
  FormAbout: TFormAbout;

implementation
uses LCLIntf, u_program_options, u_common, u_resource_string, u_utils,
  u_userdialogs, u_web, form_main, u_crossplatform, LCLType;

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.BDonateClick(Sender: TObject);
begin
  OpenURLToDonate;
end;

procedure TFormAbout.Label8Click(Sender: TObject);
var newVersion: string;
  res: TResultCheckOnlineVersion;
begin
  if Sender = Label8 then
    OpenURL('https://github.com/Lulu04/Give-Your-Voice');

  if Sender = Label10 then
    OpenURL('https://github.com/Lulu04/Give-Your-Voice/issues');

  if Sender = Label11 then begin
    // check for an app update
    Screen.BeginWaitCursor;
    res := CheckForNewVersionOnGitHub(newVersion);
    Screen.EndWaitCursor;
    case res of
      rcovErrorAccessingInternet: ShowMess(SErrorAccessingInternet, SOk, mtInformation);
      rcovNoNewVersion: ShowMess(SAppIsUpToDate, SOk, mtInformation);
      rcovNewVersionAvailable: begin
          if AskConfirmation(Format(SAskForOpenURLForNewAPPVersion, [newVersion, APP_VERSION]),
                             SYes, SNo, mtConfirmation)= mrOk then begin
            // User want to download the new version. We open the url in the browser
            // and send a message to close the main window
            OpenURL(URL_FOR_LATEST_RELEASE_ON_GITHUB);
            FormMain.PostMessageToCloseApp;
            Close;
          end;
      end;
    end;
  end;
end;

procedure TFormAbout.Label8MouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderline];
end;

procedure TFormAbout.Label8MouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
end;

procedure TFormAbout.PaintBox1Paint(Sender: TObject);
begin
  if FLogo = NIL then
    FLogo := GetLogoImage(PaintBox1.ClientWidth, PaintBox1.ClientHeight);

  FLogo.Draw(PaintBox1.Canvas, 0, 0, False);
end;

procedure TFormAbout.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
  ChangeFontHeight([Memo1, Label10, Label11], FDesignFontHeight-2);
{$endif}
{$if defined(LCLCOCOA)}
  BDonate.Flat := False;
  BClose.Flat := False;
  ChangeFontColor([BDonate, BClose], clDefault);
{$endif}
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  AdjustFont;

  Label1.Caption := APP_NAME;
  Label2.Caption := SVersion+' '+APP_VERSION;
  Label8.Caption := 'https://github.com/Lulu04/Give-Your-Voice';

  BClose.Caption := SClose;

  Memo1.Lines.Add('-- '+SCredits+' --');
  Memo1.Lines.Add('');
  Memo1.Lines.Add(Format(SIconAppBy, ['Coralie Ambert', 'alvera_cabrera', 'Freepik']));
  Memo1.Lines.Add('');
  Memo1.Lines.Add('FreePascal/Lazarus');
  Memo1.Lines.Add('       https://www.lazarus-ide.org/');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('BGRABitmap');
  Memo1.Lines.Add('       https://bgrabitmap.github.io/');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('LibSndFile');
  Memo1.Lines.Add('       https://libsndfile.github.io/libsndfile/');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('OpenALSoft');
  Memo1.Lines.Add('       https://openal-soft.org/');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('MP3Gain');
  Memo1.Lines.Add('       https://github.com/Sound-Linux-More/mp3gain');
  Memo1.Lines.Add('');
  Memo1.Lines.Add(SSomeIcons);
  Memo1.Lines.Add('https://materialdesignicons.com/');
  Memo1.Lines.Add('');
end;

procedure TFormAbout.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.FormDestroy(Sender: TObject);
begin
  FLogo.Free;
end;

procedure TFormAbout.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckKeyToShowUserGuide(Key, Shift) then
    ShowGYVUserGuide;

  case Key of
    VK_ESCAPE: Close;
  end;
end;

end.

