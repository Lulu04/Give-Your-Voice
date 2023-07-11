unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, Buttons, StdCtrls, LCLTranslator,
  LMessages, {LCLIntf,}   // messages functions
  ALSound,
  frame_channel_level, frame_viewaudio, frame_view_projectfiles,
  LCL_utils, {frame_trackbar,} u_main_undoredo;

const
  LM_MESSAGE_MainGui = LM_USER+1;
    MESS_MAINGUI_CLOSEAPP = 0;
type

  { TFormMain }

  TFormMain = class(TForm)
    BUserGuide: TSpeedButton;
    BBackward: TSpeedButton;
    BAddUserMark: TSpeedButton;
    BImproveListening: TSpeedButton;
    BWebLinks: TSpeedButton;
    BAbout: TSpeedButton;
    BOptions: TSpeedButton;
    BZoomAll: TSpeedButton;
    BDeleteAllUserMarks: TSpeedButton;
    BInsertSilence: TSpeedButton;
    BDeleteUserMarksInSelection: TSpeedButton;
    BUndo: TSpeedButton;
    BRedo: TSpeedButton;
    BSilenceOnSelection: TSpeedButton;
    BPlay: TSpeedButton;
    BStop: TSpeedButton;
    BPause: TSpeedButton;
    BStartRecordAddition: TSpeedButton;
    BForward: TSpeedButton;
    BZoomOnSelection: TSpeedButton;
    CBBassGain: TComboBox;
    CBVolumeGain: TComboBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CBURLType: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label8: TLabel;
    LabelTitle: TLabel;
    LabelAuthor: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Notebook1: TNotebook;
    Notebook2: TNotebook;
    Notebook3: TNotebook;
    OpenDialog1: TOpenDialog;
    PageURLFreeMusics: TPage;
    PageURLLitteratureAudio: TPage;
    PageURLLibrivox: TPage;
    PageURL: TPage;
    PageImproveListening: TPage;
    PageEmpty: TPage;
    PageModifyRecord: TPage;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel9: TPanel;
    PanelToolZoom: TPanel;
    PanelToolCut: TPanel;
    PanelToolSilence: TPanel;
    PanelToolUndoRedo: TPanel;
    PanelToolsUserMarks: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    BCut: TSpeedButton;
    BActivateImprovedListening: TSpeedButton;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollBox3: TScrollBox;
    BOpenProjectManager: TSpeedButton;
    Splitter1: TSplitter;
    procedure BBackwardClick(Sender: TObject);
    procedure BActivateImprovedListeningClick(Sender: TObject);
    procedure BInsertSilenceMouseDown(Sender: TObject; Button: TMouseButton;{%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BImproveListeningClick(Sender: TObject);
    procedure BSilenceOnSelectionClick(Sender: TObject);
    procedure CBURLTypeSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BOpenProjectManagerClick(Sender: TObject);
    procedure Label8Click(Sender: TObject);
  private
    //FrameTrackBar1: TFrameTrackBar;
    //procedure ProcessUserChangePreAmp(Sender: TObject);
  private
    procedure MainGuiMessageHandler(var Message: TLMessage); message LM_MESSAGE_MainGui;
  private
    FFirstShow: boolean;
    FAudioEngineStarted: boolean;
    procedure ProcessViewAudioSelectionChange(Sender: TObject);
    procedure ProcessUserActionOnFile(Sender: TObject; aAction: TUserAction);
    procedure ProcessUndoRedoChange(Sender: TObject);
    procedure UpdateButtonHint_InsertSilence;
  private
    ToggleSpeedButtonManager2: TToggleSpeedButtonManager;
    procedure AdjustFont;
  private
    FLoadingProgramOption: boolean;
    procedure SetImprovedVolumeFromProgramOptions;
    procedure SetImprovedBassGainFromProgramOptions;
  public
    FrameChannelsLevel1: TFrameChannelsLevel;
    ToggleSpeedButtonManager1: TToggleSpeedButtonManager;
    FrameViewAudio1: TFrameViewAudio;
    FrameViewProjectFiles1: TFrameViewProjectFiles;

    procedure UpdateToolsWidgets;
    procedure ProcessCapturedBuffer(Sender: TALSCaptureContext; const aBuffer: TALSCaptureFrameBuffer);

    procedure StartAudioDevice(aStartCapture: boolean=True; aStartPlayback: boolean=True);

    procedure ProcessProjectReadyChange;
    procedure UpdateProjectInfos;

    procedure ClearViewAudio;

    procedure ApplyRequestedEffect(aSound: TALSSound);

    procedure OnLanguageChange;
    procedure PostMessageToCloseApp;
  end;

var
  FormMain: TFormMain;

implementation
uses u_project, u_common, form_options, u_audio_utils, utilitaire_fichier,
  u_program_options, u_userdialogs, u_resource_string,
  form_firsttimewizard, u_utils, form_about, u_crossplatform, form_project_manager,
  u_web, form_remembertodonate, LCLType, LCLIntf, Clipbrd, Math
  {$ifdef LINUX},u_datamodule{$endif};

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
 {$ifdef LINUX}
  FDesignFontHeight := ScaleDesignToForm(15);
  FDesignSmallFontHeight := ScaleDesignToForm(11);
 {$endif}


  FrameChannelsLevel1 := TFrameChannelsLevel.Create(Self);
  FrameChannelsLevel1.Parent := Panel2;
  FrameChannelsLevel1.Align := alClient;
  FrameChannelsLevel1.DecibelMode := True;

  FrameViewAudio1 := TFrameViewAudio.Create(Self);
  FrameViewAudio1.Parent := Panel7;
  FrameViewAudio1.Align := alClient;
  FrameViewAudio1.OnSelectionChange := @ProcessViewAudioSelectionChange;

  FrameViewProjectFiles1 := TFrameViewProjectFiles.Create(Self);
  FrameViewProjectFiles1.Parent := Panel6;
  FrameViewProjectFiles1.Align := alClient;
  FrameViewProjectFiles1.OnUserAction := @ProcessUserActionOnFile;

  ToggleSpeedButtonManager1 := TToggleSpeedButtonManager.Create;
  ToggleSpeedButtonManager1.ToggleType := tsbLikeCheckBox;
  ToggleSpeedButtonManager1.SetImageIndexes(14, -1);
  ToggleSpeedButtonManager1.Add(BActivateImprovedListening, True);

  ToggleSpeedButtonManager2 := TToggleSpeedButtonManager.Create;
  ToggleSpeedButtonManager2.ToggleType := tsbLikeRadioButton;
  ToggleSpeedButtonManager2.SetActivatedColors($002FAEF2, clBlack);
  ToggleSpeedButtonManager2.SetDeactivatedColors($001E1E1E, clBlack);
  ToggleSpeedButtonManager2.Add(BImproveListening, True);
  ToggleSpeedButtonManager2.Add(BWebLinks, False);

 { FrameTrackBar1 := TFrameTrackBar.Create(Self);
  FrameTrackBar1.Parent := PanelProjectManager;
  FrameTrackBar1.Align := alClient;
  FrameTrackBar1.Init(trHorizontal, False, False, True);
  FrameTrackBar1.FormatDisplayValue(8.0, 'x', '');
  FrameTrackBar1.PercentValue := 0.125;
  FrameTrackBar1.OnChange := @ProcessUserChangePreAmp;   }

  Project := TProject.Create;
  Project.CheckSaveFolders;

  MainUndoRedoManager := TMainUndoRedoManager.Create;
  MainUndoRedoManager.MaxSize := ProgramOptions.MainUndoMaxCount;
  MainUndoRedoManager.OnChange := @ProcessUndoRedoChange;

  InitALSManagerLibrariesSubFolder;
  ALSManager.SetOpenALSoftLogCallback(@u_audio_utils.ProcessLogMessageFromOpenALSoft, NIL);
  ALSManager.LoadLibraries;

  FFirstShow := True;

  CheckedLabelManager.CaptureLabelClick(Label10);
  CheckedLabelManager.CaptureLabelClick(Label14);
  CheckedLabelManager.CaptureLabelClick(Label11);

//  RedirectChildsOnClickToContainerOnClick(PanelProjectManager, False);
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MainUndoRedoManager.Clear; // delete all undo redo files

  if RepertoireExistant(Project.TempFolder) then
    VideLeRepertoire(Project.TempFolder); // delete all temp files

  { TODO : évite un crash en fermant le programme lorsqu'on ajoute un nouvel enregistrement et qu'on le supprime de suite après, dans un dossier qui comporte 1 autre enregistrement }
  FrameViewProjectFiles1.TV.Free;
end;

procedure TFormMain.BBackwardClick(Sender: TObject);
begin
  if Sender = BStartRecordAddition then begin
    // if capture is not ready, try to restart it
    if Capture.FCaptureContext.Error then begin
      Capture.Free;
      StartAudioDevice(True, False);
    end;
    if Capture.FCaptureContext.Error then exit;

    FrameViewAudio1.Stop;
    if not Project.TempFolderExists then exit;
    FrameViewAudio1.CaptureAddition;
  end;

  if Sender = BBackward then begin
    FrameViewAudio1.SetCursorToBegin;
    FrameViewAudio1.OptimizeViewToShowCursorPos;
  end;

  if Sender = BForward then begin
    FrameViewAudio1.SetCursorToEnd;
    FrameViewAudio1.OptimizeViewToShowCursorPos;
  end;

  if Sender = BPlay then FrameViewAudio1.Play;
  if Sender = BStop then FrameViewAudio1.Stop;
  if Sender = BPause then FrameViewAudio1.Pause;

  if Sender = BUndo then FrameViewAudio1.Undo;

  if Sender = BRedo then FrameViewAudio1.Redo;

  if Sender = BZoomAll then FrameViewAudio1.ViewAll;
  if Sender = BZoomOnSelection then FrameViewAudio1.ZoomOnSelection;

  UpdateToolsWidgets;

  // update icon with or without UserMarks in the project files view
  FrameViewProjectFiles1.UpdateIconOnPreviousSelectedFolder;

end;

procedure TFormMain.BActivateImprovedListeningClick(Sender: TObject);
var v: single;
begin
  if FLoadingProgramOption then exit;

  if (Sender = CheckBox4) or (Sender = CBVolumeGain) then begin
    CBVolumeGain.Enabled := CheckBox4.Checked;
    if not CheckBox4.Checked
      then v := 1.0
      else v := 1.5 + CBVolumeGain.Itemindex*0.5;
    ProgramOptions.ListeningImprovedAmplifyGainValue := v;
  end;

  if (Sender = CheckBox3) or (Sender = CBBassGain) then begin
    CBBassGain.Enabled := CheckBox3.Checked;
    if not CheckBox3.Checked
      then v := 1.0
      else v := 1.5 + CBBassGain.Itemindex*0.5;
    ProgramOptions.ListeningImprovedBassGainValue := v;
  end;

  if Sender = CheckBox2 then
    ProgramOptions.ListeningImprovedUseCompressor := CheckBox2.Checked;

  if Sender = BActivateImprovedListening then begin
    ProgramOptions.ListeningImprovedActivated := ToggleSpeedButtonManager1.Checked[BActivateImprovedListening];
    Panel4.Enabled := ProgramOptions.ListeningImprovedActivated;
  end;

  ProgramOptions.Save;

  // update live effect and volume on view audio current sound
  if ProgramOptions.ListeningImprovedActivated then begin
    FrameViewAudio1.SetPlaybackVolume(ProgramOptions.ListeningImprovedAmplifyGainValue);
    FrameViewProjectFiles1.SetPlaybackVolume(ProgramOptions.ListeningImprovedAmplifyGainValue);
    PlayBack.FCompressor.Mute := not ProgramOptions.ListeningImprovedUseCompressor;
    PlayBack.FBassBoostEqualizer.Mute := ProgramOptions.ListeningImprovedBassGainValue = 1.0;
  end else begin
   FrameViewAudio1.SetPlaybackVolume(1.0);
   FrameViewProjectFiles1.SetPlaybackVolume(1.0);
   PlayBack.FCompressor.Mute := True;
   PlayBack.FBassBoostEqualizer.Mute := True;
  end;
end;

procedure TFormMain.BInsertSilenceMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var v: single;
begin
  if Button = mbRight then begin
    v := ProgramOptions.InsertedSilenceDurationMS*0.001;
    if UserInputSingle(SEnterDurationOfSilenceToInsert,
       SOk, SCancel, SSecond, v, 0.05, 1.0) then begin
      ProgramOptions.InsertedSilenceDurationMS := Trunc(v*1000);
      ProgramOptions.Save;
      UpdateButtonHint_InsertSilence;
    end;
  end;
end;

procedure TFormMain.BImproveListeningClick(Sender: TObject);
begin
  if Sender = BImproveListening then begin
    Notebook2.PageIndex := Notebook2.IndexOf(PageImproveListening);
  end;

  if Sender = BWebLinks then begin
    Notebook2.PageIndex := Notebook2.IndexOf(PageURL);
  end;

  if Sender = BAbout then begin
    FrameViewProjectFiles1.StopSound;
    FrameViewAudio1.Stop;
    FormAbout := TFormAbout.Create(NIL);
    FormAbout.ShowModal;
    FormAbout.Free;
  end;

  if Sender = BUserGuide then ShowGYVUserGuide;

  if Sender = BOptions then begin
    FrameViewProjectFiles1.StopSound;
    FrameViewAudio1.Stop;
    FormOptions := TFormOptions.Create(NIL);
    try
      FormOptions.ShowModal;
    finally
      FormOptions.Free;
    end;
    UpdateButtonHint_InsertSilence;
    FrameChannelsLevel1.InitFromProgramOptions;
  end;

end;

procedure TFormMain.BSilenceOnSelectionClick(Sender: TObject);
begin
  if Sender = BCut then FrameViewAudio1.CutSelection;

  if Sender = BSilenceOnSelection then
    FrameViewAudio1.SilenceOnSelection;

  if Sender = BInsertSilence then
    FrameViewAudio1.InsertSilenceAtCursorPos;


  if Sender = BDeleteUserMarksInSelection then begin
    FrameViewAudio1.Stop;
    FrameViewAudio1.DeleteUserMarksInSelection(True);
    UpdateToolsWidgets;
  end;

  if Sender = BDeleteAllUserMarks then begin
    FrameViewAudio1.Stop;
    FrameViewAudio1.DeleteAllUserMarks(True);
    UpdateToolsWidgets;
  end;

  if Sender = BAddUserMark then begin
    FrameViewAudio1.Stop;
    FrameViewAudio1.AddUserMarkAtCursor;
    UpdateToolsWidgets;
  end;

  // update icon with or without UserMarks in the project files view
  FrameViewProjectFiles1.UpdateIconOnPreviousSelectedFolder;

end;

procedure TFormMain.CBURLTypeSelect(Sender: TObject);
begin
  case CBURLType.ItemIndex of
    0: Notebook3.PageIndex := Notebook3.IndexOf(PageURLLitteratureAudio);
    1: Notebook3.PageIndex := Notebook3.IndexOf(PageURLLibriVox);
    else Notebook3.PageIndex := Notebook3.IndexOf(PageURLFreeMusics);
  end;//case

  if not FLoadingProgramOption and
     (CBURLType.ItemIndex <> ProgramOptions.CBPlatformURLIndex) then begin
    ProgramOptions.CBPlatformURLIndex := CBURLType.ItemIndex;
    ProgramOptions.Save;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FrameViewProjectFiles1.Free;
  FrameViewProjectFiles1 := NIL;
  Playback.Free;
  Capture.Free;
  Project.Free;
  MainUndoRedoManager.Free;
  ToggleSpeedButtonManager1.Free;
  ToggleSpeedButtonManager2.Free;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  p: TPoint;
begin
  if not Project.IsReady then exit;

  case Key of
    VK_F1: ShowGYVUserGuide;

    else begin
      p := FrameViewProjectFiles1.ScreenToClient(Mouse.CursorPos);
      if FrameViewProjectFiles1.ClientRect.Contains(p) then
        FrameViewProjectFiles1.ProcessKeyDown(Key, Shift)
      else begin
        p := Panel5.ScreenToClient(Mouse.CursorPos);
        if Panel5.ClientRect.Contains(p) then
          FrameViewAudio1.ProcessKeyDown(Key, Shift);
      end;
    end;
  end;//case

  Key := 0;  // avoid some gadget to react with 'space' key
end;

procedure TFormMain.FormShow(Sender: TObject);
var newVersion: string;
begin
  AdjustFont;
  OnLanguageChange;

  if FFirstShow then begin
    LabelAuthor.Caption := ' ';
    LabelTitle.Caption := ' ';
    Panel3.Height := Round(ClientHeight*6/10);
  end;

  if FFirstTimeProgramIsRunning then begin
    FormFirstRun := TFormFirstRun.Create(NIL);
    FormFirstRun.ShowModal;
    FormFirstRun.Free;
    FFirstTimeProgramIsRunning := False;
    ShowGYVUserGuide;
  end;

  if FFirstShow then begin
    FFirstShow := False;
    // check for new version
    if ProgramOptions.ItsTimeToLookForAppUpdate then begin
      Screen.BeginWaitCursor;
      if CheckForNewVersionOnGitHub(newVersion) then begin
        Screen.EndWaitCursor;
        if AskConfirmation(Format(SAskForOpenURLForNewAPPVersion, [newVersion, APP_VERSION]),
                           SYes, SNo, mtConfirmation)= mrOk then begin
          // User want to download the new version. We open the url in the browser
          // and send a message to close the main window
          OpenURL(URL_FOR_LATEST_RELEASE_ON_GITHUB);
          PostMessageToCloseApp;
        end;
      end else Screen.EndWaitCursor;
    end;

    // if there is any, try to open the project specified as parameter in command line
    if Application.ParamCount > 0 then begin
      if FileIsGYVProject(Application.Params[1]) then
        Project.Load(Application.Params[1]);
    end else begin
      // load last opened project
      if FichierExistant(ProgramOptions.LastOpenedProjectFilename) then
        Project.Load(ProgramOptions.LastOpenedProjectFilename);
    end;

    // check if its time to remember user to donate
    if ProgramOptions.ItsTimeToRememberUserToDonate then
      ShowWindowRemberUserToDonate;

    // show error message from ALSManager
    if ALSManager.Error then
      ShowMessage(SFailedToStartAudioEngine+Lineending+ALSManager.StrError);

    Notebook1.PageIndex := Notebook1.IndexOf(PageEmpty);
    FrameViewProjectFiles1.UpdateColors;
    UpdateToolsWidgets;

    FrameChannelsLevel1.InitFromProgramOptions;

    FLoadingProgramOption := True;
      SetImprovedVolumeFromProgramOptions;
      SetImprovedBassGainFromProgramOptions;
      CheckBox2.Checked := ProgramOptions.ListeningImprovedUseCompressor;

      CBURLType.ItemIndex := EnsureRange(ProgramOptions.CBPlatformURLIndex, 0, CBURLType.Items.Count-1);
      CBURLTypeSelect(CBURLType);
    FLoadingProgramOption := False;

    UpdateButtonHint_InsertSilence;
  end;

  if not FAudioEngineStarted then begin
    FAudioEngineStarted := True;
    StartAudioDevice;
  end;

  // open the project manager if there isn't a project loaded
  if not Project.IsReady then BOpenProjectManagerClick(NIL);
end;

procedure TFormMain.BOpenProjectManagerClick(Sender: TObject);
begin
  FrameViewProjectFiles1.StopSound;
  FrameViewAudio1.Stop;
  FormProjectManager := TFormProjectManager.Create(NIL);
  FormProjectManager.ShowModal;
  FormProjectManager.Free;
end;

procedure TFormMain.Label8Click(Sender: TObject);
var url: String;
begin
  url := '';
  if Sender = Label8 then
    url := 'http://www.musopen.com/';
  if Sender = Label21 then
    url := 'https://commons.wikimedia.org/wiki/Category:Audio_files_of_music_by_genre';
  if Sender = Label22 then
    url := 'https://ccmixter.org/';

  if Sender = Label20 then begin
    url := LITTERATUREAUDIO_URL_GROSFICHIERS;
    Clipboard.AsText := Project.ProjectOutputFolder;
  end;

  if Sender = Label34 then
    url := LITTERATUREAUDIO_URL_ESSAI;

  if Sender = Label36 then
    url := LIBRIVOX_URL_UPLOADER;

  if url <> '' then
    if not OpenURL(url) then
      ShowMess(SFailToOpenURL, SOk);
end;

procedure TFormMain.MainGuiMessageHandler(var Message: TLMessage);
begin
  case Message.wParam of
    MESS_MAINGUI_CLOSEAPP: Close;
  end;
end;

procedure TFormMain.ProcessCapturedBuffer(Sender: TALSCaptureContext;
  const aBuffer: TALSCaptureFrameBuffer);
begin
  FrameChannelsLevel1.UpdateProgressBar(aBuffer);
end;

procedure TFormMain.ProcessViewAudioSelectionChange(Sender: TObject);
begin
  UpdateToolsWidgets;
end;

procedure TFormMain.ProcessUserActionOnFile(Sender: TObject; aAction: TUserAction);
var s: string;
begin
  case aAction of

    faSelectionChange: ClearViewAudio;

    faPlay:;

    faStop:;

    faEdit: begin
     Notebook1.PageIndex := Notebook1.IndexOf(PageModifyRecord);
     s := NomDuDernierSousRepertoire(FrameViewProjectFiles1.SelectedFilename);
     Label13.Caption := s+ExtractFileName(FrameViewProjectFiles1.SelectedFilename);
     Screen.BeginWaitCursor;
     try
       FrameViewAudio1.Clear;
       FrameViewAudio1.LoadSound(FrameViewProjectFiles1.SelectedFilename);
       FrameViewAudio1.LoadUserMarksFromFile(FrameViewProjectFiles1.SelectedFilename);
       FrameViewAudio1.Redraw;
     finally
       Screen.EndWaitCursor;
     end;
     FrameViewProjectFiles1.HidePanel;
     UpdateToolsWidgets;
    end;

    faRenameAudioFile:;

    faDeleteAudioFile: ClearViewAudio;

    faAddSection, faInsertSection: begin
     NoteBook1.PageIndex := NoteBook1.IndexOf(PageEmpty);
    end;

    faRenameSection:;

    faDeleteSection: ClearViewAudio;

    faAddAudioFile: begin // add new recording
     UpdateToolsWidgets;
 {     if not FrameViewAudio1.Capture(FrameViewProjectFiles1.GetNextRecordingPrefixNumberInCurrentSection, False) then begin
        Notebook1.PageIndex := Notebook1.IndexOf(PageEmpty);
        exit;
      end;

      Notebook1.PageIndex := Notebook1.IndexOf(PageModifyRecord);
      Label13.Caption := ExtractFileName(FrameViewAudio1.Filename);

      FrameViewProjectFiles1.AddFileToSelectedFolderInTV(FrameViewAudio1.Filename);  }
    end;

    faInsertNewRecord: begin // insert new record
     UpdateToolsWidgets;
    end;

    faMixSection:;
  end;
end;

{procedure TFormMain.ProcessUserChangePreAmp(Sender: TObject);
begin
  Capture.FCaptureContext.PreAmp := FrameTrackBar1.PercentValue*8.0;
end;  }

procedure TFormMain.ProcessUndoRedoChange(Sender: TObject);
begin
  BUndo.Enabled := MainUndoRedoManager.UndoAvailable;
  BUndo.Hint := MainUndoRedoManager.CurrentUndoCaption;

  BRedo.Enabled := MainUndoRedoManager.RedoAvailable;
  BRedo.Hint := MainUndoRedoManager.CurrentRedoCaption;

  //BUndo.Hint := BUndo.Hint +' Undo count='+MainUndoRedoManager.UndoCount.ToString;
  //BRedo.Hint := BRedo.Hint +' Redo count='+MainUndoRedoManager.RedoCount.ToString;
end;

procedure TFormMain.UpdateToolsWidgets;
var s: string;
  flagAudioModificationEnabled: boolean;
begin
  Panel5.Enabled := Project.IsReady;

  if not FrameViewAudio1.IsEmpty then begin
    flagAudioModificationEnabled := not FileIsInMixedOutputFolder(FrameViewAudio1.Filename);
  end else flagAudioModificationEnabled := False;

  BStartRecordAddition.Enabled := flagAudioModificationEnabled;

  BPlay.Enabled := FrameViewAudio1.HaveSample;
  BStop.Enabled := BPlay.Enabled;
  BPause.Enabled := BPlay.Enabled;
  BBackward.Enabled := BPlay.Enabled;
  BForward.Enabled := BPlay.Enabled;

  BZoomAll.Enabled := FrameViewAudio1.HaveSample;
  BZoomOnSelection.Enabled := FrameViewAudio1.HaveSelection;

  BCut.Enabled := FrameViewAudio1.HaveSelection and flagAudioModificationEnabled;

  BSilenceOnSelection.Enabled := FrameViewAudio1.HaveSelection and flagAudioModificationEnabled;
  BInsertSilence.Enabled := not FrameViewAudio1.HaveSelection and
                            not FrameViewAudio1.IsEmpty and
                            flagAudioModificationEnabled;

  BDeleteUserMarksInSelection.Enabled := FrameViewAudio1.ThereIsUserMarkInSelection and flagAudioModificationEnabled;
  BDeleteAllUserMarks.Enabled := not FrameViewAudio1.UserMarks.IsEmpty; // and flagAudioModificationEnabled;
  BAddUserMark.Enabled := flagAudioModificationEnabled and not FrameViewAudio1.HaveSelection;

  BUndo.Enabled := MainUndoRedoManager.UndoAvailable;
  BRedo.Enabled := MainUndoRedoManager.RedoAvailable;

  Label12.Visible := BStartRecordAddition.Enabled and flagAudioModificationEnabled;
  Label19.Visible := Label12.Visible;
  if Label12.Visible then begin
    if FrameViewAudio1.HaveSelection then
      s := SSelectionWillBeReplacedByNewRecord
    else if FrameViewAudio1.CursorIsAtBegin then
      s := SInsertNewRecordToTheBeginning
    else if FrameViewAudio1.CursorIsAtEnd then
      s := SNewRecordWillBeAddedToTheEnd
    else
      s := SNewRecordWillBeInsertedAtCursor;
    Label12.Caption := s;
    Label12.Left := Label19.Left+Label19.Width div 2 - Label12.Width div 2;
    if Label12.Left < 3 then Label12.Left := 3;
  end;
end;

procedure TFormMain.UpdateButtonHint_InsertSilence;
begin
  BInsertSilence.Hint := Format(SInsertsSilenceAtCursor,
                                [FormatFloat('0.000', ProgramOptions.InsertedSilenceDurationMS*0.001)]);
end;

procedure TFormMain.AdjustFont;
begin
{$ifdef Linux}
  DataModule1.ApplicationProperties1.HintColor := RGBToColor(10,10,0);
  Font.Height := FDesignFontHeight;
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
  ChangeFontHeight([LabelAuthor], 20);
  ChangeFontHeight([LabelTitle], 25);
  Label16.Font.Height := 0;
  Label17.Font.Height := 0;
  ChangeFontColor([CBVolumeGain, CBBassGain, CBURLType], clBlack);

  //ChangeFontHeight([Label1], 12);
{$endif}
end;

procedure TFormMain.SetImprovedVolumeFromProgramOptions;
var i: integer;
  s: single;
begin
  CheckBox4.Checked := ProgramOptions.ListeningImprovedAmplifyGainValue <> 1.0;
  CBVolumeGain.Enabled := CheckBox4.Checked;
  if CBVolumeGain.Enabled then begin
    s := EnsureRange(ProgramOptions.ListeningImprovedAmplifyGainValue, 1.0, 6.0);
    i := Round(s*2)-3;
    CBVolumeGain.ItemIndex := i;
  end;
end;

procedure TFormMain.SetImprovedBassGainFromProgramOptions;
var i: integer;
  s: single;
begin
  CheckBox3.Checked := ProgramOptions.ListeningImprovedBassGainValue <> 1.0;
  CBBassGain.Enabled := CheckBox3.Checked;
  if CBBassGain.Enabled then begin
    s := EnsureRange(ProgramOptions.ListeningImprovedBassGainValue, 1.0, 6.0);
    i := Round(s*2)-3;
    CBBassGain.ItemIndex := i;
  end;
end;

procedure TFormMain.StartAudioDevice(aStartCapture: boolean; aStartPlayback: boolean);
begin
  if aStartCapture then begin
    Capture.Create(ProgramOptions.CaptureDeviceIndex);
    Capture.FCaptureContext.OnCaptureBuffer := @ProcessCapturedBuffer;
    Capture.FCaptureContext.MonitoringEnabled := True;
    if Capture.FCaptureContext.Error then
      ShowMess(SFailedToOpenCaptureDevice+LineEnding+
           Capture.FCaptureContext.StrError, SClose, mtError);
  end;

  if aStartPlayback then begin
    Playback.Create(ProgramOptions.PlaybackDeviceIndex);
    if Playback.FPlaybackContext.Error then
      ShowMess(SFailedToOpenPlaybackDevice+LineEnding+
           Playback.FPlaybackContext.StrError, SClose, mtError);
  end;
end;

procedure TFormMain.ProcessProjectReadyChange;
begin
  UpdateProjectInfos;
  FrameViewProjectFiles1.UpdateFileList;
  FrameViewProjectFiles1.Enabled := Project.IsReady;
  Panel5.Enabled := Project.IsReady;

  OnLanguageChange;
  FrameViewProjectFiles1.OnLanguageChange;
end;

procedure TFormMain.UpdateProjectInfos;
begin
  LabelAuthor.Caption := Project.Descriptor.AuthorFirstName+' '+
                    Project.Descriptor.AuthorLastName;
  LabelTitle.Caption := Project.Descriptor.Title;
end;

procedure TFormMain.ClearViewAudio;
begin
  FrameViewAudio1.Clear;
  NoteBook1.PageIndex := NoteBook1.IndexOf(PageEmpty);
  UpdateToolsWidgets;
end;

procedure TFormMain.ApplyRequestedEffect(aSound: TALSSound);
begin
  if ProgramOptions.ListeningImprovedActivated then begin
    // mute/unmute effects on the chain
    Playback.FCompressor.Mute := not ProgramOptions.ListeningImprovedUseCompressor;
    Playback.FBassBoostEqualizer.Mute := ProgramOptions.ListeningImprovedBassGainValue = 1.0;
    // apply chain effects
    aSound.ApplyEffect(Playback.FCompressor);
    aSound.Volume.Value := ProgramOptions.ListeningImprovedAmplifyGainValue;
  end else begin
    aSound.RemoveAllEffects;
    aSound.Volume.Value := 1.0;
  end;
end;

procedure TFormMain.OnLanguageChange;
begin
  Label36.Caption := 'librivox.org/login/uploader';

  CheckBox2.Hint := BCompressorButtonHint;
  Label10.Hint := BCompressorButtonHint;
  CheckBox4.Hint := BAmplifyVoiceButtonHint;
  Label14.Hint := BAmplifyVoiceButtonHint;
  CheckBox3.Hint := BBassBoostButtonHint;
  Label11.Hint := BBassBoostButtonHint;


  Label7.Caption := SMicLevel;

  FrameViewProjectFiles1.OnLanguageChange;

  // last combobox URL (royalty free musics)
  CBURLType.Items.Strings[CBURLType.Items.Count-1] := SRoyaltyFreeMusics;

  // url librivox
  Label36.Hint := SOpenInBrowser;

  // url litterature audio
  Label34.Hint := SOpenInBrowser;
  Label8.Hint := SOpenInBrowser;
  Label21.Hint := SOpenInBrowser;
  Label22.Hint := SOpenInBrowser;
end;

procedure TFormMain.PostMessageToCloseApp;
begin
  PostMessage(self.Handle, LM_MESSAGE_MainGui, MESS_MAINGUI_CLOSEAPP, 0);
end;


end.

