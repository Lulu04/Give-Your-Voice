unit form_mixer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin, ComCtrls, LCLTranslator,
  ALSound, LCL_utils, frame_mixer, u_mixer_undoredo, frame_progressbar,
  PropertyUtils, u_common;

type

  { TFormMixer }

  TFormMixer = class(TForm)
    BBackward: TSpeedButton;
    BHelpMixerEffect: TSpeedButton;
    BShowVolumeEnvelope: TSpeedButton;
    BKeepMix: TSpeedButton;
    BListenMix: TSpeedButton;
    BStayHere: TSpeedButton;
    BForward: TSpeedButton;
    BPlay: TSpeedButton;
    BRedo: TSpeedButton;
    BStop: TSpeedButton;
    BUndo: TSpeedButton;
    BZoomAll: TSpeedButton;
    CBAmplify: TComboBox;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    FSE3: TFloatSpinEdit;
    FSEBassBoost: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelHelp: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Notebook1: TNotebook;
    PagePrepareFile: TPage;
    PageApplyMP3Gain: TPage;
    PageActionAfter: TPage;
    PageMixingProgress: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelView: TPanel;
    PanelStatus: TPanel;
    Panel5: TPanel;
    PanelUndoRedo: TPanel;
    PanelNavigate: TPanel;
    PanelTools: TPanel;
    PanelAudioEffect: TPanel;
    PanelTop: TPanel;
    BCompressor: TSpeedButton;
    BBassBoost: TSpeedButton;
    BAmplify: TSpeedButton;
    BMix: TSpeedButton;
    BCancelMix: TSpeedButton;
    Timer2: TTimer;
    procedure BBassBoostClick(Sender: TObject);
    procedure BPlayClick(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BMixClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    FLoadingFromProgramOption: boolean;
    FFilesToMix: TStringArray;
    function CopyFilesToMixInTemp: boolean;
    procedure DeleteFilesToMixInTemp;
    function NormalizeFilesToMix: boolean;
  private
    FSoundListenMix: TALSSound;
    procedure ProcessProgressBarUserPositionChange(Sender: TObject);
    procedure ProcessUndoRedoChange(Sender: TObject);
    procedure UpdateToolsWidgets;
    procedure AdjustFont;
  private
    ToggleSpeedButtonManager2: TToggleSpeedButtonManager;
    CheckedLabelManager1: TCheckedLabelManager;
    FUserKeepTheMixing: boolean;
    FFirstShow: boolean;
  public
    FrameProgressBar1,
    FrameProgressBar2: TFrameProgressBar;
    ToggleSpeedButtonManager1: TToggleSpeedButtonManager;
    FrameMixer1: TFrameMixer;

    procedure SetAmplifyGain(aValue: single);
    function GetAmplifyGain: single;
    procedure SetBassGain(aValue: single);
    function GetBassGain: single;

    function ShowVolumeEnvelope: boolean;
    function TargetPlatform: TProjectTargetPlatform;

    procedure ShowMixingProgressPanel;
    procedure ShowActionAfterMixingPanel;
    procedure HidePanel;
    procedure DeleteSoundListenMix;

    procedure SaveSessionParamTo(const aProp: TProperties);
    procedure LoadSessionParamFrom(const aProp: TProperties);
    procedure InitSessionParamByDefault;

    property UserKeepTheMixing: boolean read FUserKeepTheMixing;

    property FirstShow: boolean read FFirstShow write FFirstShow;
  end;

var
  FormMixer: TFormMixer;

implementation
uses form_main, u_project, u_userdialogs, u_audio_utils, Math, u_program_options,
  u_resource_string, utilitaire_fichier, u_utils, LCLType,
  form_help, form_mixermetadata, u_crossplatform, LibSndFile;

{$R *.lfm}

{ TFormMixer }

procedure TFormMixer.FormCreate(Sender: TObject);
begin
  FrameMixer1 := TFrameMixer.Create(Self);
  FrameMixer1.Parent := Panel3;
  FrameMixer1.Align := alClient;

  // listen
  FrameProgressBar1 := TFrameProgressBar.Create(Self);
  FrameProgressBar1.Name := 'FrameProgressBar1';
  FrameProgressBar1.Parent := panel2;
  FrameProgressBar1.Align := alClient;
  FrameProgressBar1.OnUserPosChange := @ProcessProgressBarUserPositionChange;

  // mix
  FrameProgressBar2 := TFrameProgressBar.Create(Self);
  FrameProgressBar2.Name := 'FrameProgressBar2';
  FrameProgressBar2.Parent := panel5;
  FrameProgressBar2.Align := alClient;
  FrameProgressBar2.PB.Cursor := crDefault;

  ToggleSpeedButtonManager1 := TToggleSpeedButtonManager.Create;
  ToggleSpeedButtonManager1.ToggleType := tsbLikeCheckBox;
  ToggleSpeedButtonManager1.SetImageIndexes(14, -1);
  ToggleSpeedButtonManager1.Add(BCompressor, False);
  ToggleSpeedButtonManager1.Add(BBassBoost, True);
  ToggleSpeedButtonManager1.Add(BAmplify, True);

  ToggleSpeedButtonManager2 := TToggleSpeedButtonManager.Create;
  ToggleSpeedButtonManager2.ToggleType := tsbLikeCheckBox;
  ToggleSpeedButtonManager2.SetImageIndexes(47, 46);
  ToggleSpeedButtonManager2.SetActivatedColors($001E1E1E, clBlack);
  ToggleSpeedButtonManager2.SetDeactivatedColors($001E1E1E, clBlack);
  ToggleSpeedButtonManager2.Add(BShowVolumeEnvelope, True);

  CheckedLabelManager1 := TCheckedLabelManager.Create;
  CheckedLabelManager1.CaptureLabelClick(Label10);

  MixerUndoRedoManager := TMixerUndoRedoManager.Create;
  MixerUndoRedoManager.OnChange := @ProcessUndoRedoChange;
end;

procedure TFormMixer.BBassBoostClick(Sender: TObject);
//var i: integer;
begin
  // to do: lors d'une preview, l'utilisateur peut modifier les effets pour écouter en temps réel
  // la différence. il faut donc prévenir FrameMixer1 de cela

  if Sender = BCompressor then
    FrameMixer1.SetPreviewCompressor(ToggleSpeedButtonManager1.Checked[BCompressor]);

  if Sender = BBassBoost then
    FrameMixer1.SetPreviewBassBoost(ToggleSpeedButtonManager1.Checked[BBassBoost]);

  if Sender = FSEBassBoost then
    FrameMixer1.SetPreviewBassBoostGain(GetBassGain);

  if (Sender = BAmplify) or (Sender = CBAmplify) then
    FrameMixer1.SetPreviewAmplifyGain(GetAmplifyGain);


  if (Sender = BBassBoost) or (Sender = FSEBassBoost) then begin
 {   FLBEqualizerBassBoost.Mute :=  not ToggleSpeedButtonManager1.Checked[BBassBoost];
    FLBEqualizerBassBoostProp.LowGain  := GetBassGain;
    FLBEqualizerBassBoost.UpdateParameters(FLBEqualizerBassBoostProp);  }
  end;

  if (Sender = BAmplify) or (Sender = CBAmplify) then begin
   { for i:=0 to High(FSounds) do
      FSounds[i].Volume.Value := GetAmplifyGain; }
  end;
end;

procedure TFormMixer.BPlayClick(Sender: TObject);
var s, ext: string;
begin
  if Sender = Label8 then begin
    ext := ExtractFileExt(Label8.Caption);
    s := ChangeFileExt(Label8.Caption, '');
    if not UserInputString(SOutputFileName, SOk, SCancel, mtConfirmation, s,
         False, False, FILENAMEFORBIDENCHARS, SPleaseNoSpecialCharacters) then exit;
    if Trim(s) = '' then exit;
    Label8.Caption := ChangeFileExt(s, ext);
  end;

  if Sender = BPlay then FrameMixer1.StartPreview;
  if Sender = BStop then FrameMixer1.StopPreview;

  if Sender = BBackward then begin
    FrameMixer1.StopPreview;
    FrameMixer1.SetCursorToBegin;
    FrameMixer1.ViewBeginTime := 0;
  end;

  if Sender = BForward then begin
    FrameMixer1.StopPreview;
    FrameMixer1.SetCursorToEnd;
  end;

  if Sender = BZoomAll then FrameMixer1.ViewAll;
  //if Sender = BZoomOnSelection then FrameMixer1.ZoomOnSelection;

  if Sender = BUndo then begin
    FrameMixer1.StopPreview;
    MixerUndoRedoManager.Undo;
  end;
  if Sender = BRedo then begin
    FrameMixer1.StopPreview;
    MixerUndoRedoManager.Redo;
  end;

  if Sender = BShowVolumeEnvelope then
    FrameMixer1.ReconstructTempImageAndRedraw;

  if Sender = BHelpMixerEffect then
    _ShowHelp(SHelpMixerEffect, BHelpMixerEffect);
end;

procedure TFormMixer.ComboBox1Select(Sender: TObject);
begin
  Label8.Caption := FormMain.FrameViewProjectFiles1.GetMixingFilenameForCurrentFolder(TargetPlatform);

  if Sender = ComboBox1 then begin
    case TargetPlatform of
      ptpNone: begin
        CheckBox1.Enabled := True;
        CheckBox1.Checked := False;
      end;

      ptpLitteratureAudio: begin
        CheckBox1.Enabled := False;
        CheckBox1.Checked := True;
        FSE3.Value := 89.0;
      end;

      ptpLibriVox: begin
        CheckBox1.Enabled := False;
        CheckBox1.Checked := True;
        FSE3.Value := 89.0;
      end
      else raise exception.Create('to do');
    end;//case
  end;

  Label10.Enabled := CheckBox1.Enabled;
  FSE3.Enabled := CheckBox1.Enabled;
  Label18.Enabled := CheckBox1.Enabled;

  // update and save program options
  if not FLoadingFromProgramOption then begin
    ProgramOptions.MixedFileTargetPlatform := TargetPlatform;
    ProgramOptions.MixedFileUseMP3Gain := CheckBox1.Checked;
    ProgramOptions.MixedFileMP3GaindBx10 := Trunc(FSE3.Value*10);
    ProgramOptions.Save;
  end;
end;

procedure TFormMixer.FormActivate(Sender: TObject);
begin
  if FFirstShow then begin
    Notebook1.PageIndex := Notebook1.IndexOf(PagePrepareFile);
    PanelStatus.Visible := True;
    PanelTop.Enabled := False;
    Application.ProcessMessages;

    FFilesToMix := FormMain.FrameViewProjectFiles1.GetFilesToMix;

    Screen.BeginWaitCursor;
    FrameMixer1.AdjustTrackHeight;
    FrameMixer1.InitFromViewProjectFiles(FFilesToMix);
    UpdateToolsWidgets;
    Screen.EndWaitCursor;

    HidePanel;
    PanelTop.Enabled := True;
    FFirstShow := False;
  end;
end;

procedure TFormMixer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FrameMixer1.SaveSession;
  FrameMixer1.ClearView;

  MixerUndoRedoManager.Clear;
  DeleteSoundListenMix;
end;

procedure TFormMixer.FormDestroy(Sender: TObject);
begin
  ToggleSpeedButtonManager1.Free;
  ToggleSpeedButtonManager2.Free;
  CheckedLabelManager1.Free;
  MixerUndoRedoManager.Free;
end;

procedure TFormMixer.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckKeyToShowUserGuide(Key, Shift) then begin
    ShowGYVUserGuide;
    exit;
  end;

  if not PanelStatus.Visible then
    FrameMixer1.ProcessKeyDown(Key, Shift);
end;

procedure TFormMixer.FormShow(Sender: TObject);
begin
  if FFirstShow then begin
    AdjustFont;

    FUserKeepTheMixing := False;

    Label7.Caption := SOutputFilename;

    // update hint button
    BCompressor.Hint := BCompressorButtonHint;
    BBassBoost.Hint := BBassBoostButtonHint;
    BAmplify.Hint := BAmplifyVoiceButtonHint;

    // input folder name and output file
    Label6.Caption := FormMain.FrameViewProjectFiles1.TV.Selected.Text;
    Label8.Caption := FormMain.FrameViewProjectFiles1.GetMixingFilenameForCurrentFolder(
                       TProjectTargetPlatform(ComboBox1.ItemIndex));

    // initialize Target platform widgets from program options
    FLoadingFromProgramOption := True;
    ComboBox1.Clear;
    ComboBox1.Items.Add(SNone);
    ComboBox1.Items.Add('www.litteratureaudio.com');
    ComboBox1.Items.Add('LibriVox.org');
    // just in case...
    if (Ord(ProgramOptions.MixedFileTargetPlatform) > ComboBox1.Items.Count-1) or
        (Ord(ProgramOptions.MixedFileTargetPlatform) < 0) then begin
      ProgramOptions.MixedFileTargetPlatform := ptpNone;
      ProgramOptions.Save;
    end;
    ComboBox1.ItemIndex := Ord(ProgramOptions.MixedFileTargetPlatform);
    ComboBox1Select(ComboBox1);
    FLoadingFromProgramOption := False;

    HidePanel;
    LabelHelp.Caption := '';
    FrameMixer1.ClearPBHint;

    FrameMixer1.MIInvertMuteMusic.Caption := SToggleMute;
    FrameMixer1.MIInvertMuteSound.Caption := SToggleMute;
  end;
end;

procedure TFormMixer.BMixClick(Sender: TObject);
var res: boolean;
  dBGain: single;
  metadata: TALSFileMetaData;
begin
  if Sender = BMix then begin
    if FrameMixer1.PreviewIsRunning then exit;
    if Trim(Label8.Caption) = '' then exit;

    // ask the user to validate the metadata
    FormMixerMetaData := TFormMixerMetaData.Create(Self);
    FormMixerMetaData.TargetPlatform := TargetPlatform;
    if FormMixerMetaData.ShowModal = mrOk then begin
      // start mixing
      if FrameMixer1.MixToFile then begin
        if FrameMixer1.Canceled then begin
          if FichierExistant(FrameMixer1.OutputFile) then
            SupprimeFichier(FrameMixer1.OutputFile);
          HidePanel;
        end else begin
          // check gain analysis error
          if CheckBox1.Checked and FrameMixer1.FGainAnalyzer.Error then
            ShowMess(Format(SGainAnalysisError,[FormatFloat('0.0', FSE3.Value)]), SOk, mtError);
          // retrieve the computed gaindB to reach the desired dB
          dBGain := 0.0;
          if CheckBox1.Checked and not FrameMixer1.FGainAnalyzer.Error then
            dBGain := FrameMixer1.FGainAnalyzer.GetTitleGaindB+FSE3.Value-89.0;
          // retrieve metadata
          metadata.InitDefault;
          metadata.Title := Trim(FormMixerMetaData.EditTitle.Text);
          metadata.Artist := Trim(FormMixerMetaData.EditArtist.Text);
          metadata.Album := Trim(FormMixerMetaData.EditAlbum.Text);
          metadata.Genre := Trim(FormMixerMetaData.EditGenre.Text);
          metadata.Date := Trim(FormMixerMetaData.EditDate.Text);
          metadata.Comment := Trim(FormMixerMetaData.EditComment.Text);
          metadata.Software := APP_NAME+' v'+APP_VERSION;
          // show message to user
          Label1.Caption := SGeneratingTheMP3;
          Notebook1.PageIndex := Notebook1.IndexOf(PageApplyMP3Gain);
          Application.ProcessMessages;
          // convert file to mp3, applying gain
          Screen.BeginWaitCursor;
          try
            res := ConvertFileToMP3ConstantBitrate125kbps(FrameMixer1.OutputFile, dBGain, metadata, True);
          finally
            Screen.EndWaitCursor;
          end;
          if not res then begin
            ShowMess('Erreur, le programme n''a pas pu produire le fichier MP3', SOk, mtError);
            SupprimeFichier(FrameMixer1.OutputFile);
          end;
          FrameMixer1.OutputFile := ChangeFileExt(FrameMixer1.OutputFile, '.mp3');

          ShowActionAfterMixingPanel;
        end;
      end;
    end;
    FormMixerMetaData.Free;
    FormMixerMetaData := NIL;
  end;

  if Sender = BCancelMix then FrameMixer1.CancelMix;

  if Sender = BListenMix then begin
    Playback.FPlaybackContext.DeleteAll;
    FSoundListenMix := Playback.FPlaybackContext.AddStream(FrameMixer1.OutputFile);
    if FSoundListenMix.Error then ShowMess(FSoundListenMix.StrError, 'Fermer');
    FSoundListenMix.Play(True);
    FrameProgressBar1.Position := 0;
    Timer2.Enabled := True;
  end;

  if Sender = BStayHere then begin
    // delete the previous generated file
    SupprimeFichier(FrameMixer1.OutputFile);
    HidePanel;
    DeleteSoundListenMix;
  end;

  if Sender = BKeepMix then begin
    HidePanel;
    DeleteSoundListenMix;
    FUserKeepTheMixing := True;
    ModalResult := mrOk;
  end;
end;

procedure TFormMixer.Timer2Timer(Sender: TObject);
begin
  if FSoundListenMix <> NIL then begin
    if FSoundListenMix.State = ALS_PLAYING then
      FrameProgressBar1.Position := FSoundListenMix.TimePosition/FSoundListenMix.TotalDuration
    else
      FrameProgressBar1.Position := 0;
  end
  else ; //ProgressBar1.Position := 0;
end;

function TFormMixer.GetAmplifyGain: single;
begin
  Result := 1.0;
  if ToggleSpeedButtonManager1.Checked[BAmplify] then
    case CBAmplify.ItemIndex of
      -1: Result := 1.0;
      0..6: Result := single(CBAmplify.ItemIndex+2);
    end;
end;

procedure TFormMixer.SetBassGain(aValue: single);
begin
  FSEBassBoost.Value := aValue;
end;

function TFormMixer.GetBassGain: single;
begin
  if not ToggleSpeedButtonManager1.Checked[BBassBoost] then
    Result := 1.0
  else
    Result := FSEBassBoost.Value;
end;

procedure TFormMixer.ShowMixingProgressPanel;
begin
  PanelAudioEffect.Enabled := False;
  PanelTop.Enabled := False;
  Panel3.Enabled := False;

  FrameProgressBar2.Position := 0;
  Notebook1.PageIndex := Notebook1.IndexOf(PageMixingProgress);
  PanelStatus.Visible := True;
  Application.ProcessMessages;
end;

procedure TFormMixer.ShowActionAfterMixingPanel;
begin
  Notebook1.PageIndex := Notebook1.IndexOf(PageActionAfter);
  FSoundListenMix := NIL;
 { Panel1.Enabled := True;
  PanelTop.Enabled := True;
  Panel3.Enabled := True;

  PanelStatus.Visible := False;  }
end;

procedure TFormMixer.HidePanel;
begin
  PanelAudioEffect.Enabled := True;
  PanelTop.Enabled := True;
  Panel3.Enabled := True;
  PanelStatus.Visible := False;
  FrameProgressBar1.Position := 0;
end;

procedure TFormMixer.DeleteSoundListenMix;
begin
  Playback.FPlaybackContext.DeleteAll;
  FSoundListenMix := NIL;
  Timer2.Enabled := False;
end;

procedure TFormMixer.SaveSessionParamTo(const aProp: TProperties);
begin
  aProp.Add('Compressor', ToggleSpeedButtonManager1.Checked[BCompressor]);
  aProp.Add('BassBoost', ToggleSpeedButtonManager1.Checked[BBassBoost]);
  aProp.Add('BassBoostValue', GetBassGain);
  aProp.Add('Amplify', ToggleSpeedButtonManager1.Checked[BAmplify]);
  aProp.Add('AmplifyValue', GetAmplifyGain);
end;

procedure TFormMixer.LoadSessionParamFrom(const aProp: TProperties);
var vb: boolean;
  vsi: single;
begin
  vb := False;
  vsi := 0;

  aProp.BooleanValueOf('Compressor', vb, True);
  ToggleSpeedButtonManager1.Checked[BCompressor] := vb;

  aProp.BooleanValueOf('Amplify', vb, True);
  ToggleSpeedButtonManager1.Checked[BAmplify] := vb;
  aProp.SingleValueOf('AmplifyValue', vsi, 2.0);
  SetAmplifyGain(vsi);

  aProp.BooleanValueOf('BassBoost', vb, True);
  ToggleSpeedButtonManager1.Checked[BBassBoost] := vb;
  aProp.SingleValueOf('BassBoostValue', vsi, 2.0);
  SetBassGain(vsi);
end;

procedure TFormMixer.InitSessionParamByDefault;
begin
  ToggleSpeedButtonManager1.Checked[BCompressor] := True;
  ToggleSpeedButtonManager1.Checked[BAmplify] := True;
  SetAmplifyGain(2.0);
  ToggleSpeedButtonManager1.Checked[BBassBoost] := True;
  SetBassGain(2.0);
end;

function TFormMixer.CopyFilesToMixInTemp: boolean;
var
  i: Integer;
  f: String;
begin
  // copy the files to mix to the temporary folder
  Result := True;
  for i:=0 to High(FFilesToMix) do begin
    f := Project.TempFolder+
         ExtractFileName(FFilesToMix[i]);
    Result := Result and CopieFichier(FFilesToMix[i], f, True);
    FFilesToMix[i] := f;
  end;

  if not Result then
    DeleteFilesToMixInTemp;
end;

procedure TFormMixer.DeleteFilesToMixInTemp;
var
  i: Integer;
begin
  for i:=0 to High(FFilesToMix) do
    SupprimeFichier(FFilesToMix[i]);
  FFilesToMix := NIL;
end;

function TFormMixer.NormalizeFilesToMix: boolean;
var
  i: Integer;
begin
  Result := False;
  if Length(FFilesToMix) = 0 then exit;

  Result := True;
  for i:=0 to High(FFilesToMix) do
    Result := Result and NormalizeAudioFile(FFilesToMix[i], True, -1.0);
end;

procedure TFormMixer.ProcessProgressBarUserPositionChange(Sender: TObject);
var p: single;
begin
  if FSoundListenMix = NIL then begin
    p := FrameProgressBar1.Position; // save position choosen by user
    BMixClick(BListenMix); // start sound from begining (progressBar position is reset to 0)
    FrameProgressBar1.Position := p; // sets user position
  end;

  FSoundListenMix.TimePosition := FSoundListenMix.TotalDuration*FrameProgressBar1.Position;
  if FSoundListenMix.State <> ALS_PLAYING then
    FSoundListenMix.Play(False);
end;

procedure TFormMixer.ProcessUndoRedoChange(Sender: TObject);
begin
  BUndo.Enabled := MixerUndoRedoManager.UndoAvailable;
  BUndo.Hint := MixerUndoRedoManager.CurrentUndoCaption;

  BRedo.Enabled := MixerUndoRedoManager.RedoAvailable;
  BRedo.Hint := MixerUndoRedoManager.CurrentRedoCaption;
end;

procedure TFormMixer.UpdateToolsWidgets;
begin
  BPlay.Enabled := not FrameMixer1.IsEmpty;
  BStop.Enabled := BPlay.Enabled;
  BBackward.Enabled := BPlay.Enabled;
  BForward.Enabled := BPlay.Enabled;

  BZoomAll.Enabled := not FrameMixer1.IsEmpty;
  //BZoomOnSelection.Enabled := FrameMixer1.HaveSelection;

  BUndo.Enabled := MixerUndoRedoManager.UndoAvailable;
  BRedo.Enabled := MixerUndoRedoManager.RedoAvailable;
end;

procedure TFormMixer.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
  ChangeFontHeight([BCompressor, BBassBoost, FSEBassBoost, BAmplify, CBAmplify], FDesignFontHeight-2);
  ChangeFontColor([FSEBassBoost, CBAmplify, ComboBox1, FSE3], clBlack);
{$endif}
{$if defined(LCLCOCOA)}
  BCancelMix.Flat := False;
  BMix.Flat := False;
  BCompressor.Flat := False;
  BAmplify.Flat := False;
  BBassBoost.Flat := False;
  ChangeFontColor([BCompressor, BAmplify, BBassBoost, FSEBassBoost, CBAmplify], clDefault);
{$endif}
end;

function TFormMixer.ShowVolumeEnvelope: boolean;
begin
  Result := ToggleSpeedButtonManager2.Checked[BShowVolumeEnvelope];
end;

function TFormMixer.TargetPlatform: TProjectTargetPlatform;
begin
  Result := TProjectTargetPlatform(ComboBox1.ItemIndex);
end;

procedure TFormMixer.SetAmplifyGain(aValue: single);
begin
  CBAmplify.ItemIndex := EnsureRange(Trunc(aValue)-2, 0, CBAmplify.Items.Count-1);
end;

end.

