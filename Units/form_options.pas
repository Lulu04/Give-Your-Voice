unit form_options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  DividerBevel, LCL_utils, LCLTranslator, ExtCtrls,
  frame_channel_level, ALSound;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    CBCapture: TComboBox;
    CBPlayback: TComboBox;
    CheckBox1: TCheckBox;
    CBLanguage: TComboBox;
    DividerBevel1: TDividerBevel;
    DividerBevel6: TDividerBevel;
    Label1: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    BCancel: TSpeedButton;
    BOk: TSpeedButton;
    BTestPlaybackDevice: TSpeedButton;
    Panel1: TPanel;
    Shape1: TShape;
    procedure BTestPlaybackDeviceClick(Sender: TObject);
    procedure CBPlaybackSelect(Sender: TObject);
    procedure CBLanguageSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
  private
    FrameChannelsLevel1: TFrameChannelsLevel;
    CheckedLabelManager1: TCheckedLabelManager;
    FWidgetsCallBackLocked: boolean;
    procedure StopPlaybackTest;
    procedure StopRecordingTest;
    procedure ProcessCapturedBuffer(Sender: TALSCaptureContext; const aBuffer: TALSCaptureFrameBuffer);

    procedure PrefsToWidgets;
    procedure WidgetsToPrefs;
    procedure AdjustFont;
  public

  end;

var
  FormOptions: TFormOptions;

implementation
uses u_program_options, u_audio_utils, u_resource_string, form_main, u_utils,
  u_crossplatform, LCLType
  {$if defined(LINUX) or defined(Darwin)}, u_common{$endif};

{$R *.lfm}

{ TFormOptions }

procedure TFormOptions.FormShow(Sender: TObject);
begin
  AdjustFont;

  CBCapture.Clear;
  CBCapture.Items.AddStrings(ALSManager.ListOfCaptureDeviceName, False);

  CBPlayback.Clear;
  CBPlayback.Items.AddStrings(ALSManager.ListOfPlaybackDeviceName, False);

  BTestPlaybackDevice.Caption := STest;

  FillComboBoxWithAvailableLanguage(CBLanguage);

  PrefsToWidgets;
end;

procedure TFormOptions.BCancelClick(Sender: TObject);
begin
  if Sender = BOk then
    WidgetsToPrefs;

  if Sender = BCancel then begin
    // use the previous device
  end;

  Close;
end;

procedure TFormOptions.StopPlaybackTest;
begin
  BTestPlaybackDevice.Tag := 0;
  BTestPlaybackDevice.Caption := STest;
  Playback.Free;
end;

procedure TFormOptions.StopRecordingTest;
begin
  Capture.Free;
end;

procedure TFormOptions.ProcessCapturedBuffer(Sender: TALSCaptureContext;
  const aBuffer: TALSCaptureFrameBuffer);
begin
  FrameChannelsLevel1.UpdateProgressBar(aBuffer);
end;

procedure TFormOptions.PrefsToWidgets;
begin
  FWidgetsCallBackLocked := True;
  if ProgramOptions.CaptureDeviceIndex >= CBCapture.Items.Count then begin
    ProgramOptions.CaptureDeviceIndex := -1;
    ProgramOptions.Save;
  end;
  CBCapture.ItemIndex := ProgramOptions.CaptureDeviceIndex;
  CBPlaybackSelect(CBCapture);

  LanguageToComboBox(ProgramOptions.Language, CBLanguage);

  CheckBox1.Checked := ProgramOptions.RemoveNoiseWhenRecording;

  if ProgramOptions.PlaybackDeviceIndex >= CBPlayback.Items.Count then begin
    ProgramOptions.PlaybackDeviceIndex := -1;
    ProgramOptions.Save;
  end;
  CBPlayback.ItemIndex := ProgramOptions.PlaybackDeviceIndex;

  FrameChannelsLevel1.InitFromProgramOptions;

  FWidgetsCallBackLocked := False;
end;

procedure TFormOptions.WidgetsToPrefs;
begin
  ProgramOptions.CaptureDeviceIndex := CBCapture.ItemIndex;
  ProgramOptions.PlaybackDeviceIndex := CBPlayback.ItemIndex;

  ProgramOptions.RemoveNoiseWhenRecording := CheckBox1.Checked;

  ProgramOptions.Save;
end;

procedure TFormOptions.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
  ChangeFontHeight([BTestPlaybackDevice], FDesignSmallFontHeight);
  ChangeFontColor([CBLanguage, CBCapture, CBPlayBack, BTestPlaybackDevice], clBlack);
{$endif}
{$if defined(LCLCOCOA)}
  ChangeFontColor([BOk, BCancel, BTestPlaybackDevice], clDefault);
{$endif}
end;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  CheckedLabelManager1 := TCheckedLabelManager.Create;
  CheckedLabelManager1.CaptureLabelClick(Label1);

  FrameChannelsLevel1 := TFrameChannelsLevel.Create(Self);
  FrameChannelsLevel1.Parent := Panel1;
  FrameChannelsLevel1.Align := alClient;
end;

procedure TFormOptions.CBPlaybackSelect(Sender: TObject);
begin
  if Sender = CBPlayback then StopPlaybackTest;

  if Sender = CBCapture then begin
    StopRecordingTest;
    if CBCapture.ItemIndex = -1 then exit;
    Capture.Create(CBCapture.ItemIndex);
    Capture.FCaptureContext.OnCaptureBuffer := @ProcessCapturedBuffer;
    Capture.FCaptureContext.MonitoringEnabled := True;
  end;

  if FWidgetsCallBackLocked then exit;
  BOk.Enabled := True;
end;

procedure TFormOptions.BTestPlaybackDeviceClick(Sender: TObject);
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

procedure TFormOptions.CBLanguageSelect(Sender: TObject);
begin
  if CBLanguage.ItemIndex = -1 then exit;
  ProgramOptions.Language := ComboBoxToLanguage(CBLanguage);

  if FWidgetsCallBackLocked then exit;
  BOk.Enabled := True;
end;

procedure TFormOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  StopPlaybackTest;
  StopRecordingTest;
  FormMain.StartAudioDevice;
end;

procedure TFormOptions.FormDestroy(Sender: TObject);
begin
  CheckedLabelManager1.Free;
end;

procedure TFormOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckKeyToShowUserGuide(Key, Shift) then
    ShowGYVUserGuide;

  case Key of
    VK_ESCAPE: BCancelClick(BCancel);
  end;
end;

end.

