unit form_audiorecording;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin,
  u_common, ALSound, LCL_utils, frame_channel_level;

type

  { TFormRecord }

  TFormRecord = class(TForm)
    BStartRecord: TSpeedButton;
    BSave: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox3: TCheckBox;
    Edit6: TEdit;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    LabelUserMarkAdded: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Notebook1: TNotebook;
    PageSave: TPage;
    PageAskPageNumber: TPage;
    PageProcessing: TPage;
    PageRecording: TPage;
    PageNoiseProfile: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    BAddUserMark: TSpeedButton;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    PanelButtonStop: TPanel;
    PanelButtonPause: TPanel;
    SEBeginPage1: TSpinEdit;
    SEBeginPage: TSpinEdit;
    SEEndPage: TSpinEdit;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Timer1: TTimer;
    procedure BPauseClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure BStartRecordClick(Sender: TObject);
    procedure BStopClick(Sender: TObject);
    procedure BAddUserMarkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private type TTimerMode = (tmProfileNoise, tmShowRecordingTime, tmRecordingError);
  private
    FVoiceRecordingStarted,
    FAskPage,
    FAllowUserToAddUserMark: boolean;
    FCaptureContext: TALSCaptureContext;
    FRecordTimeOrigin,
    FRecordTimeDuration: single;
    FUserMarks: TUserMarks;
    FTimerMode: TTimerMode;
    FCount: integer;
    FFileForNoise,
    FFilename,
    FTitleOfTheRecordingFile: string;
    function GetTemporaryFileForNoise: string;
    procedure AddUserMarkAtCurrentRecordingTime;
    procedure DoRecording;
    procedure AdjustFont;
  private
   CheckedLabelManager1: TCheckedLabelManager;
   FrameChannelsLevel1: TFrameChannelsLevel;
    procedure ProcessCapturedBuffer(Sender: TALSCaptureContext; const aBuffer: TALSCaptureFrameBuffer);
  public

    // Captured audio will be saved in this file
    property Filename: string read FFilename write FFilename;
    // Used to insert an user mark at the right time position
    property RecordTimeOrigin: single read FRecordTimeOrigin write FRecordTimeOrigin;
    // set to TRUE if the recording is for a new chapter in the project
    //  -> a message ask to the user the begin page number of the reading
    property AskPage: boolean read FAskPage write FAskPage;
    // Show/hide the 'Add User Mark' button
    property AllowUserToAddUserMark: boolean read FAllowUserToAddUserMark write FAllowUserToAddUserMark;


    // list of the new user marks inserted while recording
    property UserMarks: TUserMarks read FUserMarks;

    // It's the formated page number and title
    property TitleOfTheRecordingFile: string read FTitleOfTheRecordingFile;

  end;

var
  FormRecord: TFormRecord;

implementation
uses u_project, u_audio_utils, u_program_options, form_main, u_utils,
  u_resource_string, u_userdialogs, u_crossplatform, LCLType,
  utilitaire_fichier, u_logfile;

{$R *.lfm}

{ TFormRecord }

procedure TFormRecord.BPauseClick(Sender: TObject);
begin
  if (FCaptureContext.State = ALS_RECORDING) or
     (FCaptureContext.State = ALS_PAUSED) then
    FCaptureContext.PauseCapture;

  Label3.Visible := FCaptureContext.State = ALS_PAUSED;
  Label3.Tag := 5;
  Label1.Visible := not Label3.Visible;
  BAddUserMark.Enabled := FCaptureContext.State = ALS_RECORDING;
end;

procedure TFormRecord.BSaveClick(Sender: TObject);
var page: String;
  rff: TRecordingFileFormater;
begin
  // construct the title of the filename
  // page
  page := '';
  if CheckBox1.Checked then
    page := rff.FormatPage(SEBeginPage.Value, SEEndPage.Value);

  FTitleOfTheRecordingFile := Trim(Edit6.Text);
  ConcatToString(FTitleOfTheRecordingFile, ' ', page);

  Label10.Visible := CheckBox1.Checked and (SEBeginPage.Value > SEEndPage.Value);

  SEBeginPage.Enabled := CheckBox1.Checked;
  Label9.Enabled := CheckBox1.Checked;
  SEEndPage.Enabled := CheckBox1.Checked;

  rff.Init(FormMain.FrameViewProjectFiles1.GetNextRecordingPrefixNumberInCurrentSection,
           FTitleOfTheRecordingFile, PROJECT_RECORDING_FILE_EXT);
  Label12.Caption := rff.FileName;

  BSave.Enabled := not Label10.Visible;

  if Sender = BSave then begin
    if CheckBox1.Checked then begin
      // save the greatest page number to project file
      if Project.Descriptor.CurrentPageNumber < SEEndPage.Value then begin
        Project.Descriptor.CurrentPageNumber := SEEndPage.Value;
        Project.Save;
      end;
    end;

    ModalResult := mrOk;
  end;
end;

procedure TFormRecord.BStartRecordClick(Sender: TObject);
begin
  if Sender = BStartRecord then begin
    SEBeginPage.Value := SEBeginPage1.Value;
    SEEndPage.Value := SEBeginPage1.Value;
    DoRecording;
  end;

  if Sender = CheckBox3 then begin
    if CheckBox3.Checked then
      FormMain.Hide
    else
      FormMain.Show;
  end;
end;

procedure TFormRecord.BStopClick(Sender: TObject);
begin
  FCaptureContext.StopCapture;
  FCaptureContext.RemoveDCBias := False; // stop removing DCBias

  Log.Info('    captured '+ TimeToString(FRecordTimeDuration, 3));

  Timer1.Enabled := False;

  if FCaptureContext.CaptureError then begin
    Log.Error('    capture context error: '+FCaptureContext.StrCaptureError);
    ShowMess(SFailedToRecord+LineEnding+FCaptureContext.StrCaptureError, SClose, mtError);
    ModalResult := mrCancel;
  end
  else begin
   if ProgramOptions.RemoveNoiseWhenRecording then begin
     // remove the noise
     NoteBook1.PageIndex := NoteBook1.IndexOf(PageProcessing);
     Label6.Caption := SNoiseRemovalInProgress+LineEnding+SPleaseWait;
     Application.ProcessMessages;
     Screen.BeginWaitCursor;
     Notebook1.Enabled := False;
     try
       if not RemoveNoiseOnFile(FFileForNoise, FFilename, -24) then begin
         Log.Error('    Fail to remove noise');
         ShowMess(SNoiseRemovalFailed, SClose, mtWarning);
       end;
       // delete noise file
       SupprimeFichier(FFileForNoise);
     finally
       Screen.EndWaitCursor;
       Notebook1.Enabled := True;
     end;
   end;
   if not FAskPage then begin
     ModalResult := mrOk;
     exit;
   end;

   // Ask to the user the final page of the reading
   NoteBook1.PageIndex := NoteBook1.IndexOf(PageSave);
   // update the file name
   BSaveClick(NIL);
   // hide the channel level
   FrameChannelsLevel1.Visible := False;
  end;
end;

procedure TFormRecord.BAddUserMarkClick(Sender: TObject);
begin
  AddUserMarkAtCurrentRecordingTime;
end;

procedure TFormRecord.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := False;
  FCaptureContext.StopCapture;
  FCaptureContext.RemoveDCBias := False;
  FCaptureContext.OnCaptureBuffer := @FormMain.ProcessCapturedBuffer;

{  if (ModalResult = mrCancel) and FVoiceRecordingStarted then begin
    if AskConfirmation(SCancelThisRecord, SYes, SNo, mtConfirmation)= mrCancel then begin
      ModalResult := mrOk;

    end;
  end;    }

  if ModalResult = mrCancel then begin
    if FichierExistant(FFilename) then
      SupprimeFichier(FFilename);
    if FichierExistant(FFileForNoise) then
      SupprimeFichier(FFileForNoise);
  end;


  // show main form
  if CheckBox3.Checked then
    FormMain.Show;
end;

procedure TFormRecord.FormCreate(Sender: TObject);
begin
  FCaptureContext := Capture.FCaptureContext;

  CheckedLabelManager1 := TCheckedLabelManager.Create;
  CheckedLabelManager1.CaptureLabelsClick([Label8, Label13]);

  FrameChannelsLevel1 := TFrameChannelsLevel.Create(Self);
  FrameChannelsLevel1.Parent := Panel7;
  FrameChannelsLevel1.Align := alClient;
  FrameChannelsLevel1.InitFromProgramOptions;

  RedirectChildsOnClickToContainerOnClick(PanelButtonPause, True);
  RedirectChildsOnClickToContainerOnClick(PanelButtonStop, True);

  FAllowUserToAddUserMark := True;
end;

procedure TFormRecord.FormDestroy(Sender: TObject);
begin
  CheckedLabelManager1.Free;
end;

procedure TFormRecord.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckKeyToShowUserGuide(Key, Shift) then
    ShowGYVUserGuide;

  case Key of
    VK_ESCAPE: ModalResult := mrCancel;

    VK_SPACE: if NoteBook1.PageIndex = NoteBook1.IndexOf(PageRecording) then
      BStopClick(NIL);

    VK_P: if NoteBook1.PageIndex = NoteBook1.IndexOf(PageRecording) then
      BPauseClick(NIL);

    else if ssCtrl in Shift then AddUserMarkAtCurrentRecordingTime;
  end;
end;

procedure TFormRecord.FormShow(Sender: TObject);
begin
  AdjustFont;
  FVoiceRecordingStarted := False;

  Label19.Caption := SStop;
  Label20.Caption := SPause;
  BStartRecord.Caption := SStartRecording;

  BAddUserMark.Caption := SMarkTouchUpToBeDone;
  BAddUserMark.Visible := FAllowUserToAddUserMark;

  FUserMarks.Clear;

  NoteBook1.PageIndex := NoteBook1.IndexOf(PageAskPageNumber);

  Label7.Visible := FAskPage;
  SEBeginPage1.Visible := FAskPage;
  SEBeginPage1.Value := Project.Descriptor.CurrentPageNumber;

  if FAskPage then
    Label16.Caption := Format(SNewRecordFor, [ExtractFileName(FormMain.FrameViewProjectFiles1.SelectedFilename)])
  else
    Label16.Caption := Format(SAdditionTo, [ExtractFileName(FormMain.FrameViewProjectFiles1.SelectedFilename)]);

  FCaptureContext.OnCaptureBuffer := @ProcessCapturedBuffer;
end;

procedure TFormRecord.Timer1Timer(Sender: TObject);
begin
  if FCaptureContext.CaptureError then begin
    ShowMess(SHardwareErrorWhileCapturing+LineEnding+FCaptureContext.StrCaptureError, SClose, mtError);
    ModalResult := mrCancel;
    exit;
  end;

  case FTimerMode of
   tmProfileNoise: begin
     dec(FCount);
     Label5.Caption := FCount.ToString;
   end;

   tmShowRecordingTime: begin
     if FCaptureContext.CaptureError then begin
       // an error occurs during capture
       FCaptureContext.StopCapture;
       ShowMess(SHardwareErrorWhileCapturing, SOk, mtError);
       ModalResult := mrCancel;
       Timer1.Enabled := False;
       exit;
     end;

     // label recording time
     Label2.Caption := TimeToString(FRecordTimeDuration, 1);
     // label paused
     if FCaptureContext.State = ALS_PAUSED then begin
       Label3.Tag := Label3.Tag -1;
       if Label3.Tag <= 0 then begin
         Label3.Tag := 5;
         Label3.Visible := not Label3.Visible;
       end;
     end;
     // label user mark added
     if LabelUserMarkAdded.Tag > 0 then begin
       LabelUserMarkAdded.Tag := LabelUserMarkAdded.Tag - 1;
       LabelUserMarkAdded.Visible := LabelUserMarkAdded.Tag <> 0;
     end;
   end;

   tmRecordingError: begin

   end;
  end;
end;

function TFormRecord.GetTemporaryFileForNoise: string;
begin
  Result := Project.TempFolder+RECORDING_TEMP_NOISE_FILENAME;
end;

procedure TFormRecord.AddUserMarkAtCurrentRecordingTime;
begin
  FUserMarks.Append(FRecordTimeDuration+FRecordTimeOrigin);
  LabelUserMarkAdded.Visible := True;
  LabelUserMarkAdded.Tag := 20;
end;

procedure TFormRecord.DoRecording;
begin
  if ProgramOptions.RemoveNoiseWhenRecording then begin
    Log.Info('    start recording noise to file');
    NoteBook1.PageIndex := NoteBook1.IndexOf(PageNoiseProfile);
    FCount := 3;
    Label5.Caption := FCount.ToString;
    FTimerMode := tmProfileNoise;
    Timer1.Enabled := True;
    // prepare file for noise. samples are float type for more precision when removing noise
    FFileForNoise := GetTemporaryFileForNoise;
    if not FCaptureContext.PrepareSavingToFile(FFileForNoise,
                            ALSMakeFileFormat(SF_FORMAT_WAV, SF_FORMAT_FLOAT)) then begin
      Log.Error('    CaptureContext.PrepareSavingToFile fail while recording noise');
      ShowMess(SErrorWhenPreparingRecordingFileForNoise, SClose, mtError);
      ModalResult := mrCancel;
      exit;
    end;
    // remove DCBias
    FCaptureContext.RemoveDCBias := True;

    FCaptureContext.StartCapture;
    while (FCount > 0) and not (ModalResult = mrCancel) do begin
     Sleep(10);
     Application.ProcessMessages;
    end;
    FCaptureContext.StopCapture;
    Timer1.Enabled := False;

    Application.ProcessMessages;
    if FCaptureContext.CaptureError and not (ModalResult = mrCancel) then begin
      Log.Error('    CaptureContext error while recording noise'+LineEnding+
                '    '+FCaptureContext.StrCaptureError);
      FCaptureContext.RemoveDCBias := False;  // stop removing DCBias
      ShowMess(SFailedToRecordNoise+LineEnding+FCaptureContext.StrCaptureError,
               SClose, mtError);
      ModalResult := mrCancel;
      exit;
    end;
  end;

  if ModalResult = mrCancel then exit;

  NoteBook1.PageIndex := NoteBook1.IndexOf(PageRecording);
  // prepare the recording file
  if not FCaptureContext.PrepareSavingToFile(FFilename, GetRecordAudioFileFormat) then begin
    ShowMess(SErrorWhenPreparingRecordingFile+LineEnding+
                FFilename+LineEnding+FCaptureContext.StrCaptureError, SClose, mtError);
    ModalResult := mrCancel;
    exit;
  end;

  Log.Info('    start recording voice to "'+FFilename+'"');
  FRecordTimeDuration := 0;
  FTimerMode := tmShowRecordingTime;
  Timer1.Interval := 100;
  Timer1.Enabled := True;
  FVoiceRecordingStarted := True;

  // remove DCBias
  FCaptureContext.RemoveDCBias := True;
  FCaptureContext.StartCapture;
end;

procedure TFormRecord.AdjustFont;
begin
{$ifdef LINUX}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);

  ChangeFontHeight([Label19, Label20], FDesignSmallFontHeight);

  ChangeFontColor([Edit6, SEBeginPage1, SEBeginPage, SEEndPage], clBlack);
{$endif}
end;

procedure TFormRecord.ProcessCapturedBuffer(Sender: TALSCaptureContext;
  const aBuffer: TALSCaptureFrameBuffer);
begin
  FrameChannelsLevel1.UpdateProgressBar(aBuffer);

  if Capture.FCaptureContext.State = ALS_RECORDING then
    FRecordTimeDuration := FRecordTimeDuration+aBuffer.FrameCount/Capture.FCaptureContext.Frequency;
end;

end.

