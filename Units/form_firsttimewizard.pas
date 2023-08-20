unit form_firsttimewizard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  frame_channel_level, ALSound;

type

  { TFormFirstRun }

  TFormFirstRun = class(TForm)
    BTestPlaybackDevice: TSpeedButton;
    CBCapture: TComboBox;
    CBPlayback: TComboBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Shape1: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure BTestPlaybackDeviceClick(Sender: TObject);
    procedure CBPlaybackSelect(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    FrameChannelsLevel1: TFrameChannelsLevel;
    procedure AdjustFont;
    procedure StopPlaybackTest;
    procedure StopRecordingTest;
    procedure ProcessCapturedBuffer(Sender: TALSCaptureContext; const aBuffer: TALSCaptureFrameBuffer);
    procedure OnLanguageChange;
  public

  end;

var
  FormFirstRun: TFormFirstRun;

implementation

uses u_program_options, u_resource_string, u_audio_utils
  {$if defined(Linux) or defined(Darwin)}, u_common, u_utils{$endif}, i18_utils;

{$R *.lfm}

{ TFormFirstRun }

procedure TFormFirstRun.ComboBox1Select(Sender: TObject);
begin
  SpeedButton1.Enabled := ComboBox1.ItemIndex <> -1;
  if ComboBox1.ItemIndex = -1 then exit;

  ProgramOptions.Language := AppLang.ComboBoxGetSelectedLanguage(ComboBox1);
  ProgramOptions.Save;

  SpeedButton1.Caption := SNext;
  SpeedButton3.Caption := SNext;
  BTestPlaybackDevice.Caption := STest;

  OnLanguageChange;
end;

procedure TFormFirstRun.FormCreate(Sender: TObject);
begin
  FrameChannelsLevel1 := TFrameChannelsLevel.Create(Self);
  FrameChannelsLevel1.Parent := Panel3;
  FrameChannelsLevel1.Align := alClient;
end;

procedure TFormFirstRun.CBPlaybackSelect(Sender: TObject);
begin
  if Sender = CBPlayback then StopPlaybackTest;

  if Sender = CBCapture then begin
    StopRecordingTest;
    if CBCapture.ItemIndex = -1 then exit;
    Capture.Create(CBCapture.ItemIndex);
    Capture.FCaptureContext.OnCaptureBuffer := @ProcessCapturedBuffer;
    Capture.FCaptureContext.MonitoringEnabled := True;
  end;
end;

procedure TFormFirstRun.BTestPlaybackDeviceClick(Sender: TObject);
begin
  case BTestPlaybackDevice.Tag of
    0: begin
      if CBPlayBack.ItemIndex = -1 then exit;
      Playback.Free;
      Playback.Create(CBPlayBack.ItemIndex);
      with Playback.FPlaybackContext.CreateWhiteNoise(1, 1) do begin
        Loop := True;
        Volume.Value := 0.5;
        Play(True);
      end;
      BTestPlaybackDevice.Tag := 1;
      BTestPlaybackDevice.Caption := SStop;
    end;
    1: begin
      StopPlaybackTest;
    end;
  end;
end;

procedure TFormFirstRun.FormShow(Sender: TObject);
var langToUse: TLanguageIdentifier;
begin
  AdjustFont;
  OnLanguageChange;

  Panel1.Visible := False;
  SpeedButton3.Visible := False;
  Panel2.Visible := False;

  // choose a default langage
  if AppLang.OSLanguageIsSupportedByApp then
    langToUse := AppLang.GetOSLanguage
  else
    langToUse := 'en';
  // fill the combobox with the supported languages list
  AppLang.FillComboBoxWithSupportedLanguage(ComboBox1);
  // select the default language
  AppLang.ComboBoxSetSelectedLanguage(ComboBox1, langToUse);
  // save the default language in program options and translate GUI
  ProgramOptions.Language := langToUse;

  SpeedButton1.Caption := SNext;
  SpeedButton3.Caption := SNext;
  BTestPlaybackDevice.Caption := STest;

  CBCapture.Clear;
  CBCapture.Items.AddStrings(ALSManager.ListOfCaptureDeviceName, False);

  CBPlayback.Clear;
  CBPlayback.Items.AddStrings(ALSManager.ListOfPlaybackDeviceName, False);
end;

procedure TFormFirstRun.SpeedButton1Click(Sender: TObject);
begin
  if Sender = SpeedButton1 then begin
    ProgramOptions.Save; // we save the selected language
    SpeedButton1.Visible := False;
    Panel1.Visible := True;
    SpeedButton3.Visible := True;
  end;

  if Sender = SpeedButton3 then begin
    StopPlaybackTest;
    SpeedButton3.Visible := False;
    Panel2.Visible := True;
  end;
end;

procedure TFormFirstRun.SpeedButton2Click(Sender: TObject);
begin
  StopPlaybackTest;
  StopRecordingTest;
  Close;
end;

procedure TFormFirstRun.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  Font.Height := FDesignFontHeight;
  ChangeFontHeight([Label1], 30);
  ChangeFontHeight([Label4, Label6, Label7, Label8, Label9, BTestPlaybackDevice], FDesignFontHeight-2);
  ChangeFontHeight([Label2, Label3, ComboBox1, SpeedButton1, CBCapture,
     CBPlayBack, Speedbutton3, Label5, SpeedButton2], FDesignFontHeight);
  {$if defined(LCLGTK2)}
  ChangeFontColor([ComboBox1, CBCapture, CBPlayback], clBlack);
  {$endif}
{$endif}
{$if defined(LCLCOCOA)}
  SpeedButton1.Flat := False;
  SpeedButton2.Flat := False;
  SpeedButton3.Flat := False;
  ChangeFontColor([BTestPlaybackDevice], clDefault);
{$endif}
end;

procedure TFormFirstRun.StopPlaybackTest;
begin
  BTestPlaybackDevice.Tag := 0;
  BTestPlaybackDevice.Caption := STest;
  Playback.Free;
end;

procedure TFormFirstRun.StopRecordingTest;
begin
  Capture.Free;
end;

procedure TFormFirstRun.ProcessCapturedBuffer(Sender: TALSCaptureContext;
  const aBuffer: TALSCaptureFrameBuffer);
begin
  FrameChannelsLevel1.UpdateProgressBar(aBuffer);
end;

procedure TFormFirstRun.OnLanguageChange;
begin
  Label9.Caption := SMicLevel;
end;

end.

