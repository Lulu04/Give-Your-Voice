unit frame_view_projectfiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ShellCtrls, ExtCtrls, Buttons,
  Menus, ALSound, Types, LCLTranslator, Dialogs,
  frame_progressbar, u_common;

type

  TUserAction = (faSelectionChange,
                 faPlay,
                 faStop,
                 faEdit,
                 faAddAudioFile,
                 faInsertNewRecord,
                 faRenameAudioFile,
                 faDeleteAudioFile,
                 faAddSection,
                 faInsertSection,
                 faRenameSection,
                 faDeleteSection,
                 faMixSection,
                 faImportAudioInSection,
                 faImportAudioAndInsertItBeforeRecord);

  TUserActionEvent = procedure(Sender: TObject; aAction: TUserAction) of object;

  { TFrameViewProjectFiles }

  TFrameViewProjectFiles = class(TFrame)
    MIInsertFile: TMenuItem;
    MIImportAudioAndInsertHere: TMenuItem;
    MIImportAudioInSection: TMenuItem;
    MIZipMP3Folder: TMenuItem;
    MIDeleteZIPFolder: TMenuItem;
    MIClearMP3Folder: TMenuItem;
    MIInsertSection: TMenuItem;
    MIRefresh: TMenuItem;
    MIDeleteFile: TMenuItem;
    MIAddFolder: TMenuItem;
    MIRenameSection: TMenuItem;
    MIDeleteSection: TMenuItem;
    MIRenameFile: TMenuItem;
    OD1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PopupEmpty: TPopupMenu;
    PopupSection: TPopupMenu;
    PopupRecording: TPopupMenu;
    PopupZIPFolder: TPopupMenu;
    PopupMP3Folder: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    BPlayAudioFile: TSpeedButton;
    BStopAudioFile: TSpeedButton;
    BEditFile: TSpeedButton;
    BAddRecord: TSpeedButton;
    BAddSection: TSpeedButton;
    BMixSection: TSpeedButton;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Timer1: TTimer;
    TV: TTreeView;
    procedure PopupEmptyPopup(Sender: TObject);
    procedure BAddRecordClick(Sender: TObject);
    procedure PopupRecordingPopup(Sender: TObject);
    procedure PopupSectionPopup(Sender: TObject);
    procedure PopupMP3FolderPopup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure TVChanging(Sender: TObject; {%H-}Node: TTreeNode; var {%H-}AllowChange: Boolean);
    procedure TVDblClick(Sender: TObject);
    procedure TVMouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure TVMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure TVMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure TVResize(Sender: TObject);
    procedure TVSelectionChanged(Sender: TObject);
  private
    FOnUserAction: TUserActionEvent;
    procedure DoUserActionEvent(aAction: TUserAction);
    procedure DoNewSection(InsertMode: boolean);// True = insert folder before selected
                                               // False =  add folder to the end
    procedure DoRenameFolder(const f: string);
    procedure DoDeleteFolder; // deleted the selected

    procedure DoAddRecord(aInsertMode: boolean);
    procedure DoRenameFile;
    procedure DoDeleteFile;
    procedure DoImportAudioInSection;
    procedure DoImportAudioAndInsertItBeforeSelected;
    procedure ShiftFilePrefixFromNode(aFileNode: TTreeNode; delta: integer);
    function NodeToFilenameOrPath(aNode: TTreeNode): string;
    procedure DoZipMP3Files;
  private
    FSound: TALSSound;
    procedure ShowToolPanel;
    procedure HideToolPanels;
    procedure AdjustFont;
  private
    FrameProgressBar1: TFrameProgressBar;
    procedure ProcessProgressBarUserPosChange(Sender: TObject);
  private
    FPreviousSelectedFolderNode: TTreeNode;
    procedure UpdatePreviousSelectedFolderNode;
  private // each folder (except MP3 and ZIP) have a prefix number like '01 ' to
          // keep the list sorted. User can't modify this number prefix.
    FSectionFolderCount: integer;
    procedure IncSectionFolderCount;
    procedure DecSectionFolderCount;
    function ProjectDigitCountForPrefixNumber: integer;
    function GetFormatedPrefixNumber(aNumber, aDigitCount: integer): string;
    function FolderRespectPrefixNumberRules(const aSubFolder: string): boolean;
    function ShiftPrefixNumber(const aSubFolder: string; aDelta: integer): string;
    function ExtractPrefixNumber(const aSubFolder: string): string;
    function ExtractTitleWithoutPrefixNumber(const aSubFolder: string): string;
  public
    procedure UpdateIconOnPreviousSelectedFolder;
    procedure UpdateUserMarkIconOnNodeFolder(aFolderNode: TTreeNode; const aFolder: string);
    procedure UpdateUserMarkIconOnFileNode(aFileNode: TTreeNode; const aFile: string);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure UpdateFileList;

    // select the TreeView row that contain the filename (not folder name)
    // aFilename is full path filename
    procedure FocusOn(const aFilename: string);

    procedure ProcessKeyDown(Key: Word; {%H-}Shift: TShiftState);
    procedure StopSound;
    procedure StartSound;
    procedure SetPlaybackVolume(aValue: single);

    function WorkingSectionNode: TTreeNode;
    // use only when the selected is a file
    function SelectedParentFolder: string;
    function SelectedFilename: string;

    function SelectedIsRoot: boolean;
    function SelectedIsFolder: boolean;
    function SelectedIsSection: boolean;
    function SelectedIsFolderMP3: boolean;
    function SelectedIsFolderZIP: boolean;
    function SelectedIsFile: boolean;
    function SelectedIsAudioFile: boolean;
    function SelectedIsRecordingFile: boolean;

  public  // recording file prefix utils
    function GetNextRecordingPrefixNumberInCurrentSection: integer;
    function GetRecordingPrefixNumberOfTheSelectedFile: integer;

    procedure AddFileToSelectedFolderInTV(const aFullPathFilename: string);

    // Mixing filename
    function GetMixingFilenameForCurrentFolder(aTargetPlatform: TProjectTargetPlatform): string;
    function GetTitleMetadataForCurrentFolder(aTargetPlatform: TProjectTargetPlatform): string;
    function GetTrackNumberMetadataForCurrentFolder(aTargetPlatform: TProjectTargetPlatform): string;
    function GetFilesToMix: TStringArray;

    procedure UpdateColors;
    procedure HidePanel;

    procedure OnLanguageChange;

    property OnUserAction: TUserActionEvent read FOnUserAction write FOnUserAction;
  end;

implementation
uses u_project, LazFileUtils, utilitaire_fichier, form_main, u_userdialogs,
  u_utils, u_audio_utils, u_program_options, u_resource_string,
  form_ask_sectionname, form_zipmp3, form_mixer, Graphics, Math, LCLType,
  LazUTF8, LCLIntf, form_audiorecording;

{$R *.lfm}

{ TFrameViewProjectFiles }

procedure TFrameViewProjectFiles.TVSelectionChanged(Sender: TObject);
begin
  StopSound;
  ShowToolPanel;

  UpdatePreviousSelectedFolderNode;

  DoUserActionEvent(faSelectionChange);
end;

procedure TFrameViewProjectFiles.DoUserActionEvent(aAction: TUserAction);
begin
  if FOnUserAction <> NIL then
    FOnUserAction(Self, aAction);
end;

procedure TFrameViewProjectFiles.DoNewSection(InsertMode: boolean);
var newName, folder, s: string;
  i: Integer;
  n: TTreeNode;
  p: TPoint;
begin
  if not Project.IsReady then exit;

  FormAskSectionName := TFormAskSectionName.Create(NIL);
  p.x := ClientWidth;
  p.y := ClientHeight div 2;
  p := ClientToScreen(p);
  FormAskSectionName.Left := p.x - FormAskSectionName.Width;
  FormAskSectionName.Top := p.y - FormAskSectionName.Height div 2;
  FormAskSectionName.ModeInsert := InsertMode;
  if FormAskSectionName.ShowModal <> mrOk then begin
    FormAskSectionName.Free;
    exit;
  end;

  newName := FormAskSectionName.WantedName;
  FormAskSectionName.Free;

  if not InsertMode then begin
    // ADD mode
    IncSectionFolderCount;
    folder := IncludeTrailingPathDelimiter(Project.ProjectFolder)+
              GetFormatedPrefixNumber(FSectionFolderCount, ProjectDigitCountForPrefixNumber)+
              newName
  end else begin
    // INSERT mode
    IncSectionFolderCount;
    folder := IncludeTrailingPathDelimiter(Project.ProjectFolder)+
              ExtractPrefixNumber(TV.Selected.Text)+' '+newName;
    // shifts the prefix on the next folders
    n := TV.Items.GetFirstNode;
    for i:=n.Count-1 downto n.IndexOf(TV.Selected) do
      if FolderRespectPrefixNumberRules(n.Items[i].Text) then begin // rules respected ?
        s := IncludeTrailingPathDelimiter(ExtractFilePath(NodeToFilenameOrPath(n.Items[i])))+
             ShiftPrefixNumber(n.Items[i].Text, 1);
        if not RenommerRepertoire(NodeToFilenameOrPath(n.Items[i]), s) then ;
        { TODO : Checker les erreurs, si un repertoire existe deja avant de le renommer }
      end;
  end;

  // if needed, create the sub folder for the new part of the project
  if not RepertoireExistant(folder) then
    if not CreerRepertoire(folder) then begin
      ShowMess(SFailedToCreateFolder+LineEnding+folder, SClose, mtError);
      exit;
    end;

  UpdateFileList;
  FocusOn(folder);
end;

procedure TFrameViewProjectFiles.DoRenameFolder(const f: string);
var
  s, s1: String;
//  flag: boolean;
begin
  if FolderRespectPrefixNumberRules(TV.Selected.Text) then begin
    s1 := ExtractPrefixNumber(TV.Selected.Text)+' ';
    s := Copy(TV.Selected.Text,
              ProjectDigitCountForPrefixNumber+2,
              Length(TV.Selected.Text)-ProjectDigitCountForPrefixNumber+1)
  end else begin
    s1 := '';
    s := TV.Selected.Text;
  end;

  if not UserInputString(SNewName, SOk, SCancel, mtConfirmation, s, False, False,
                         FILENAMEFORBIDENCHARS, SPleaseNoSpecialCharacters) then exit;
  if Trim(s) = '' then exit;

  s := s1 + s;
  if s = TV.Selected.Text then exit;

{  flag := True;
  repeat
   if not UserInputString(SNewName, SOk, SCancel, mtConfirmation, s, False, False,
                          FILENAMEFORBIDENCHARS, SPleaseNoSpecialCharacters) then exit;
    if s = TV.Selected.Text then exit;
    flag := Project.Descriptor.CheckPartSubFolder(s);
    if not flag then ShowMess(SInvalidName, SOk, mtError);
  until flag;  }

  s1 := RepertoireParent(f) + s;
  if RepertoireExistant(s1) then begin
    ShowMess(SFolderWithSameNameAlreadyExists, SOk, mtError);
    exit;
  end;

  if RenommerRepertoire(f, s1) then begin
    TV.Selected.Text := s;
    ShowToolPanel;
    DoUserActionEvent(faRenameSection);
  end else
    ShowMess(SFailedToRename, SOk, mtError);
end;

procedure TFrameViewProjectFiles.DoDeleteFolder;
var flagDeleted: boolean;
  oldFolder, newFolder: string;
  n: TTreeNode;
  i: Integer;
begin
  // NO ! we don't shift the next folder prefix because there no issue to have a
  // gap. If we shifts, there are issues if user take back the folder from the
  // recycle bin -> we have 2 folders with the same prefix

  flagDeleted := MoveFilesToTrash([SelectedFilename]);
  if not flagDeleted then
    flagDeleted := SupprimeRepertoire(SelectedFilename);
  if flagDeleted then begin
    // shifts the prefix on the next folders
    n := TV.Items.GetFirstNode;
    for i:=n.IndexOf(TV.Selected)+1 to n.Count-1 do
      if FolderRespectPrefixNumberRules(n.Items[i].Text)then begin
        oldFolder := NodeToFilenameOrPath(n.Items[i]);
        newFolder := IncludeTrailingPathDelimiter(ExtractFilePath(oldFolder))+
                     ShiftPrefixNumber(n.Items[i].Text, -1);
        if not RenommerRepertoire(oldFolder, newFolder) then
          ShowMess('Erreur de renommage de'+lineending+
                   oldFolder+lineending+'vers'+lineending+newFolder, sok, mterror);
        { TODO : Checker les erreur de renommage }
      end;

    DecSectionFolderCount;

    n := TV.Items.GetFirstNode;
    i := n.IndexOf(TV.Selected);
    UpdateFileList;
    n := TV.Items.GetFirstNode;
    if n.Count > 0 then begin
      i := Min(i+1, n.Count-1);
      n.Items[i].MakeVisible;
    end;

  end else ShowMess(SFailToDeleteThisFolder, SOk);
end;

procedure TFrameViewProjectFiles.DoAddRecord(aInsertMode: boolean);
var F: TFormRecord;
  capturedTempFile, finalFile: string;
  p: TPoint;
  rff: TRecordingFileFormater;
  prefixNumber: integer;
  sectionNode, n: TTreeNode;
begin
  // if capture is not ready, try to restart it
  if Capture.FCaptureContext.Error then begin
    Capture.Free;
    FormMain.StartAudioDevice(True, False);
  end;
  if Capture.FCaptureContext.Error then exit;
  if not Project.TempFolderExists then exit;

  HideToolPanels;
  FormMain.ClearViewAudio;

  capturedTempFile := GetTempFilenameForRecord;

  F := TFormRecord.Create(NIL);

  // centers the recording window on the file's tree
{  p.x := (FormMain.Panel6.Width - F.Width) div 2;
  p.y := (FormMain.Panel6.Height - F.Height) div 2;
  p := FormMain.Panel6.ClientToScreen(p);     }
  p.x := (Width - F.Width) div 2;
  p.y := (Height - F.Height) div 2;
  p := ClientToScreen(p);

  F.Left := p.x;
  F.Top := p.y;
  F.Filename := capturedTempFile;
  F.RecordTimeOrigin := 0;
  F.AskPage := True;
  F.AllowUserToAddUserMark := True;
  if F.ShowModal = mrCancel then begin
    // user have canceled the recording -> we delete its file
    SupprimeFichier(capturedTempFile);
    dec(FRecordFilenameSuffix);
    F.Free;
    exit;
  end;

  // compute the final filename
  if aInsertMode
    then prefixNumber := GetRecordingPrefixNumberOfTheSelectedFile
    else prefixNumber := GetNextRecordingPrefixNumberInCurrentSection;

  rff.Init(prefixNumber, F.TitleOfTheRecordingFile, '.wav');
  if aInsertMode then begin
    ShiftFilePrefixFromNode(TV.Selected, 1); // shift the next files prefix
    finalFile := IncludeTrailingPathDelimiter(SelectedParentFolder)
  end else
    finalFile := IncludeTrailingPathDelimiter(SelectedFilename);
  finalFile := finalFile + rff.FileName;

  // copy the temp recording file to its final filename in the project
  if not CopieFichier(capturedTempFile, finalFile, True) then begin
    if aInsertMode
      then ShowMess(SInsertionOfNewRecordIntoProjectFailed, SClose, mtError)
      else ShowMess(SAddNewRecordIntoProjectFailed, SClose, mtError);
  end else begin
    // delete the temp file
    SupprimeFichier(capturedTempFile);
    // save the user marks
    F.UserMarks.SaveToFile(finalFile);
    // add entry to TV
    sectionNode := WorkingSectionNode;
    if aInsertMode
      then n := TV.Items.Insert(TV.Selected, rff.FileName)
      else n := TV.Items.AddChild(sectionNode, rff.FileName);
    // set the right icon
    UpdateUserMarkIconOnNodeFolder(n, NodeToFilenameOrPath(TV.Selected));
    UpdateUserMarkIconOnFileNode(n, finalFile);
    // alpha sort the node
    sectionNode.AlphaSort;
    // focus on new item
    FocusOn(finalFile);

    // load the last capture in the view
    FormMain.FrameViewAudio1.LoadSound(finalFile);
    FormMain.FrameViewAudio1.LoadUserMarksFromFile(finalFile);

    if aInsertMode
      then DoUserActionEvent(faInsertNewRecord)
      else DoUserActionEvent(faAddAudioFile);
  end;

  F.Free;
end;

procedure TFrameViewProjectFiles.DoRenameFile;
var f, s: string;
begin
  f := SelectedFilename;
  s := ChangeFileExt(TV.Selected.Text, '');
  if not UserInputString(SNewName, SOk, SCancel, mtConfirmation, s, False, False,
                         FILENAMEFORBIDENCHARS, SPleaseNoSpecialCharacters) then exit;
  s := ChangeFileExt(s, ExtractFileExt(TV.Selected.Text));
  s := IncludeTrailingPathDelimiter(ExtractFilePath(f))+ s;
  if s = f then exit;

  if RenommeFichier(f, s) then begin
    // change filename in the mix session file
    RenameAudioFileNameInMixSessionFile(f, s);

    TV.Selected.Text := ExtractFileName(s);
    TV.Selected.Parent.AlphaSort;
    ShowToolPanel;
    DoUserActionEvent(faRenameAudioFile);
  end else begin
    ShowMess(SFailedToRename, SOk, mtError);
    UpdateFileList;
  end;
end;

procedure TFrameViewProjectFiles.DoDeleteFile;
var f: string;
  flagDeleted: Boolean;
begin
  f := SelectedFilename;
  if f = '' then exit;

  if AskConfirmation(SAskConfirmDeleteFile, SDelete, SCancel, mtWarning) = mrOk then begin
    flagDeleted := MoveFilesToTrash([f]);
    if not flagDeleted then
      flagDeleted := SupprimeFichier(f);
    if flagDeleted then begin
      // delete file.usermarks
      SupprimeFichier(ChangeFileExt(f, USER_MARK_FILE_EXT));
      UpdateIconOnPreviousSelectedFolder;

      // shift prefix for the next files
      if TV.Selected.GetNext <> NIL then
        ShiftFilePrefixFromNode(TV.Selected.GetNext, -1);

      TV.Items.Delete(TV.Selected);
      DoUserActionEvent(faDeleteAudioFile);
    end
    else
      ShowMess(SFailToDeleteFile, SOk, mtError);
  end;
end;

procedure TFrameViewProjectFiles.DoImportAudioInSection;
var A: TStringArray;
  i: integer;
  dstPath, dst: string;
  sectionNode, n: TTreeNode;
  rff: TRecordingFileFormater;
begin
  OD1.Title := SAudioFileImport;
  OD1.Filter := ALSManager.DialogFileFilters(SAudioFile, SAllFile);
  if not OD1.Execute then exit;
  A := OD1.Files.ToStringArray;
  if OD1.Files.Count > 1 then begin
    //Afficher une fenêtre où l'utilisateur peut modifier l'ordre d'insertion des fichiers
    // dans l'arborescence
    // il en résulte A trié.
  end;

  // retrieve the section node of the selected item in TV
  sectionNode := TV.Selected;

  dstPath := IncludeTrailingPathDelimiter(NodeToFilenameOrPath(sectionNode));
  for i:=0 to High(A) do begin
      rff.Init(GetNextRecordingPrefixNumberInCurrentSection,
               ChangeFileExt(ExtractFileName(A[i]), ''),
               '.wav');
    dst := ChangeFileExt(ExtractFileName(A[i]), '.wav');
    dst := dstPath + rff.FileName;

    if not ConvertAudioFile(A[i], dst, GetRecordAudioFileFormat, True) then
      ShowMess(SFailedToImportAudioFile+LineEnding+A[i], SOk, mtError)
    else begin
      // conversion succeed -> add item in TV
      n := TV.Items.AddChild(sectionNode, ExtractFileName(dst));
      // set the right icon
      UpdateUserMarkIconOnFileNode(n, dst);
      //UpdateUserMarkIconOnNodeFolder(n, NodeToFilenameOrPath(TV.Selected));
    end;
  end;
end;

procedure TFrameViewProjectFiles.DoImportAudioAndInsertItBeforeSelected;
var A: TStringArray;
  i: integer;
  dstPath, dst: string;
  sectionNode, n: TTreeNode;
  rff: TRecordingFileFormater;
begin
  // insert mode: check if the selected file name respect the recording prefix rules
  if not rff.FileRespectRecordingPrefixRules(TV.Selected.Text) then begin
    ShowMess(SRecordingFileDontRespectNamesRules, SClose, mtError);
    exit;
  end;

  OD1.Title := SAudioFileImport;
  OD1.Filter := ALSManager.DialogFileFilters(SAudioFile, SAllFile);
  if not OD1.Execute then exit;
  A := OD1.Files.ToStringArray;
  if OD1.Files.Count > 1 then begin
    //Afficher une fenêtre où l'utilisateur peut modifier l'ordre d'insertion des fichiers
    // dans l'arborescence
    // il en résulte A trié.
  end;

  // retrieve the section node of the selected item in TV
  sectionNode := TV.Selected.Parent;

  dstPath := IncludeTrailingPathDelimiter(NodeToFilenameOrPath(sectionNode));
  for i:=0 to High(A) do begin
    rff.InitFrom(TV.Selected.Text);
    rff.Title := ChangeFileExt(ExtractFileName(A[i]), '.wav');
    dst := dstPath + rff.FileName;

    // shift prefix of existing recording files with delta=+1
    ShiftFilePrefixFromNode(TV.Selected, 1);

    if not ConvertAudioFile(A[i], dst, GetRecordAudioFileFormat, True) then begin
      // cancel the prefix shifting
      ShiftFilePrefixFromNode(TV.Selected, -1);
      ShowMess(SFailedToImportAudioFile+LineEnding+A[i], SOk, mtError)
    end else begin
      // conversion succeed
      // insert item in TV
      n := TV.Items.Insert(TV.Selected, ExtractFileName(dst));
      // set the right icon
      //UpdateUserMarkIconOnNodeFolder(n, NodeToFilenameOrPath(TV.Selected));
      UpdateUserMarkIconOnFileNode(n, dst);
    end;
  end;
end;

procedure TFrameViewProjectFiles.ShiftFilePrefixFromNode(aFileNode: TTreeNode; delta: integer);
var sectionNode: TTreeNode;
  i: integer;
  rff: TRecordingFileFormater;
  path, src, dst: string;
begin
  if aFileNode = NIL then exit;
  if delta = 0 then exit;

  sectionNode := aFileNode.Parent;
  path := IncludeTrailingPathDelimiter(NodeToFilenameOrPath(sectionNode));

  if delta > 0 then begin
    for i:=sectionNode.Count-1 downTo sectionNode.IndexOf(aFileNode) do
      if rff.FileRespectRecordingPrefixRules(sectionNode.Items[i].Text) then begin
        src := path+sectionNode.Items[i].Text;

        rff.InitFrom(sectionNode.Items[i].Text);
        rff.PrefixNumber := rff.PrefixNumber + delta;
        dst := path+rff.FileName;

        if RenommeFichier(src, dst) then
          sectionNode.Items[i].Text := rff.FileName;
      end
  end else begin
    for i:=sectionNode.IndexOf(aFileNode) to sectionNode.Count-1 do
      if rff.FileRespectRecordingPrefixRules(sectionNode.Items[i].Text) then begin
        src := path+sectionNode.Items[i].Text;

        rff.InitFrom(sectionNode.Items[i].Text);
        rff.PrefixNumber := rff.PrefixNumber + delta;
        dst := path+rff.FileName;

        if RenommeFichier(src, dst) then
          sectionNode.Items[i].Text := rff.FileName;
      end
  end;
end;

function TFrameViewProjectFiles.NodeToFilenameOrPath(aNode: TTreeNode): string;
begin
  Result := '';
  if aNode = NIL then exit;
  while aNode <> TV.Items.GetFirstNode do begin
    if Length(Result) = 0 then
      Result := aNode.Text
    else
      Result := ConcatPaths([aNode.Text, Result]);
    aNode := aNode.Parent;
  end;
  Result := ConcatPaths([Project.ProjectFolder, Result]);
end;

procedure TFrameViewProjectFiles.DoZipMP3Files;
var n: TTreeNode;
begin
  if not Project.CreateProjectZipFolder then exit;
  UpdateFileList;
  n := TV.Items.FindNodeWithText(PROJECT_OUTPUT_FOLDER_ZIP);
  if n <> NIL then n.MakeVisible;

  FormZipMP3 := TFormZipMP3.Create(NIL);
  FormZipMP3.ShowModal;
  FormZipMP3.Free;
end;

procedure TFrameViewProjectFiles.StopSound;
begin
  if ALSManager.Error then exit;

  if FSound <> NIL then begin
    FSound.Kill;
    FSound := NIL;
    Timer1.Enabled := False;
    FrameProgressBar1.Position := 0;
  end;
end;

procedure TFrameViewProjectFiles.StartSound;
var
  f: String;
begin
  if ALSManager.Error then exit;

  f := SelectedFilename;
  if f = '' then exit;
  if IsFolder(f) then exit;

  if FSound <> NIL then StopSound;
  FSound := Playback.FPlaybackContext.AddStream(f);

  // enhances listening only on not mixed audio files
  if not FileIsInMixedOutputFolder(f) then
    FormMain.ApplyRequestedEffect(FSound);

  FSound.Play;
  Timer1.Enabled := True;
end;

procedure TFrameViewProjectFiles.SetPlaybackVolume(aValue: single);
begin
  if FSound <> NIL then
    FSound.Volume.Value := aValue;
end;

function TFrameViewProjectFiles.WorkingSectionNode: TTreeNode;
begin
  if SelectedIsSection then
    Result := TV.Selected
  else
    Result := TV.Selected.Parent;
end;

procedure TFrameViewProjectFiles.ShowToolPanel;
var
  r: TRect;
begin
  HideToolPanels;

  if TV.Selected = NIL then exit;
  r := TV.Selected.DisplayRect(True);

  if SelectedIsRoot then begin
    Panel3.Left := r.Right;
    Panel3.Top := r.CenterPoint.Y-Panel3.Height div 2;
    Panel3.Visible := True;
  end
  else if SelectedIsSection then begin
    if (TV.Selected.Text = PROJECT_OUTPUT_FOLDER_MP3) or
       (TV.Selected.Text = PROJECT_OUTPUT_FOLDER_ZIP) then
     HideToolPanels
    else begin
      Panel2.Left := r.Right+ScaleDesignToForm(10);
      Panel2.Top := r.CenterPoint.Y-Panel2.Height div 2;
      Panel2.Visible := True;
      BMixSection.Enabled := TV.Selected.Count > 0;
    end;
  end
  else if SelectedIsFile then begin
    if (ExtractFileExt(SelectedFilename) = PROJECT_ZIP_FILE_EXT) or
       (FormMain.FrameViewAudio1.Filename = SelectedFilename) then
      HideToolPanels
    else begin
      Panel1.Left := r.Right;
      Panel1.Top := r.CenterPoint.Y-Panel1.Height div 2;
      Panel1.Visible := True;
    end;
  end;
end;

procedure TFrameViewProjectFiles.HideToolPanels;
begin
  Panel1.Visible := False;
  Panel2.Visible := False;
  Panel3.Visible := False;
end;

procedure TFrameViewProjectFiles.AdjustFont;
begin
{$ifdef Linux}
  ChangeFontHeight([Self, TV, BAddSection,
      BPlayAudioFile, BStopAudioFile, BEditFile,
      BAddrecord, BMixSection], FDesignFontHeight);
{$endif}
end;

procedure TFrameViewProjectFiles.ProcessProgressBarUserPosChange(Sender: TObject);
begin
  if FSound <> NIL then begin
    FSound.TimePosition := FSound.TotalDuration*FrameProgressBar1.Position;
  end;
end;

procedure TFrameViewProjectFiles.UpdatePreviousSelectedFolderNode;
var f: string;
  n: TTreeNode;
begin
  FPreviousSelectedFolderNode := NIL;
  n := TV.Selected;

  // no selection ?
  if n = NIL then exit;

  // selected is root ?
  if n = TV.Items.GetFirstNode then exit;

  // if selected is a file we take the parent node = parent folder
  f := NodeToFilenameOrPath(n);
  if IsFile(f) then n := n.Parent;

  // folder is MP3 or ZIP ?
  if (n.Text = PROJECT_OUTPUT_FOLDER_MP3) or
     (n.Text = PROJECT_OUTPUT_FOLDER_ZIP) then exit;

  // that's OK !
  FPreviousSelectedFolderNode := n;
end;

procedure TFrameViewProjectFiles.IncSectionFolderCount;
var i, prefixNumber, digitCount: integer;
  n: TTreeNode;
  oldPath, newPath, nameF: String;
begin
  digitCount := Length((FSectionFolderCount+1).ToString);
  if digitCount > Project.Descriptor.FolderHeader_DigitCountForPrefixNumber then begin
    // increase the digit count on all folders of the project
    n := TV.Items.GetFirstNode;
    for i:=0 to n.Count-1 do
      if FolderRespectPrefixNumberRules(n.Items[i].Text) then begin
        if not TryStrToInt(ExtractPrefixNumber(n.Items[i].Text), prefixNumber) then
          continue;
        nameF := GetFormatedPrefixNumber(prefixNumber, digitCount)+
                 ExtractTitleWithoutPrefixNumber(n.Items[i].Text);

        oldPath := NodeToFilenameOrPath(n.Items[i]);
        newPath := IncludeTrailingPathDelimiter(RepertoireParent(oldPath))+nameF;

        if newPath <> oldPath then
          if RenommerRepertoire(oldPath, newPath) then
            n.Items[i].Text := nameF;
      end;

    // save the digit count in project file
    Project.Descriptor.FolderHeader_DigitCountForPrefixNumber := digitCount;
    Project.Save;
  end;

  inc(FSectionFolderCount);
end;

procedure TFrameViewProjectFiles.DecSectionFolderCount;
begin
  dec(FSectionFolderCount);
end;

function TFrameViewProjectFiles.ProjectDigitCountForPrefixNumber: integer;
begin
  Result := Project.Descriptor.FolderHeader_DigitCountForPrefixNumber;
end;

function TFrameViewProjectFiles.GetFormatedPrefixNumber(aNumber, aDigitCount: integer): string;
begin
  Result := Format('%.'+aDigitCount.ToString+'d ', [aNumber]);
end;

function TFrameViewProjectFiles.FolderRespectPrefixNumberRules(const aSubFolder: string): boolean;
var prefixNum: integer;
begin
  Result := False;
  if Length(aSubFolder) < ProjectDigitCountForPrefixNumber+1 then exit;
  if not TryStrToInt(Copy(aSubFolder, 1, ProjectDigitCountForPrefixNumber), prefixNum) then exit;
  if aSubFolder[ProjectDigitCountForPrefixNumber+1] <> ' ' then exit;
  Result := True;
end;

function TFrameViewProjectFiles.ShiftPrefixNumber(const aSubFolder: string; aDelta: integer): string;
var prefixNum: integer;
begin
  Result := '';
  if not TryStrToInt(Copy(aSubFolder, 1, ProjectDigitCountForPrefixNumber), prefixNum) then exit;

  Result := GetFormatedPrefixNumber(prefixNum+aDelta, ProjectDigitCountForPrefixNumber)+
            Copy(aSubFolder,
                 ProjectDigitCountForPrefixNumber+2,
                 Length(aSubFolder)-ProjectDigitCountForPrefixNumber+1);
end;

function TFrameViewProjectFiles.ExtractPrefixNumber(const aSubFolder: string): string;
begin
  Result := Copy(aSubFolder, 1, ProjectDigitCountForPrefixNumber);
end;

function TFrameViewProjectFiles.ExtractTitleWithoutPrefixNumber(const aSubFolder: string): string;
begin
  Result := Copy(aSubFolder,
                 ProjectDigitCountForPrefixNumber+2,
                 Length(aSubFolder)-ProjectDigitCountForPrefixNumber+1);
end;

procedure TFrameViewProjectFiles.UpdateIconOnPreviousSelectedFolder;
var f: string;
begin
  if FPreviousSelectedFolderNode = NIL then exit;

  // update icon on folder
  f := NodeToFilenameOrPath(FPreviousSelectedFolderNode);
  UpdateUserMarkIconOnNodeFolder(FPreviousSelectedFolderNode, f);

  // update icone on file
  if TV.Selected = NIL then exit;
  f := NodeToFilenameOrPath(TV.Selected);
  UpdateUserMarkIconOnFileNode(TV.Selected, f);
end;

procedure TFrameViewProjectFiles.UpdateUserMarkIconOnNodeFolder(aFolderNode: TTreeNode;
  const aFolder: string);
begin
  if not RepertoireExistant(aFolder) then exit;

  if FolderHaveUserMarksFile(aFolder) then begin
    aFolderNode.SelectedIndex := 37;  // chapter with user marks
    aFolderNode.ImageIndex := 37;
  end else begin
    aFolderNode.SelectedIndex := 18;  // chapter
    aFolderNode.ImageIndex := 18;
  end;
end;

procedure TFrameViewProjectFiles.UpdateUserMarkIconOnFileNode(
  aFileNode: TTreeNode; const aFile: string);
begin
  if not IsFile(aFile) then exit;

  if FichierExistant(ChangeFileExt(aFile, USER_MARK_FILE_EXT)) then begin
    aFileNode.SelectedIndex := 36; // record with user marks
    aFileNode.ImageIndex := 36;
  end else begin
    aFileNode.SelectedIndex := 19; // record icon
    aFileNode.ImageIndex := 19;
  end;
end;

constructor TFrameViewProjectFiles.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameProgressBar1 := TFrameProgressBar.Create(Self);
  FrameProgressBar1.Parent := Panel4;
  FrameProgressBar1.Align := alClient;
  FrameProgressBar1.OnUserPosChange := @ProcessProgressBarUserPosChange;

  AdjustFont;
end;

function TFrameViewProjectFiles.SelectedParentFolder: string;
begin
  Result := ExtractFilePath(SelectedFilename);
end;

function TFrameViewProjectFiles.SelectedFilename: string;
begin
  Result := NodeToFilenameOrPath(TV.Selected);
end;

function TFrameViewProjectFiles.SelectedIsRoot: boolean;
begin
  Result := False;
  if TV.Selected <> NIL then
    Result := TV.Selected = TV.Items.GetFirstNode;
end;

function TFrameViewProjectFiles.SelectedIsFolder: boolean;
begin
  Result := False;
  if TV.Selected = NIL then exit;
  Result := TV.Selected.Level = 1;
end;

function TFrameViewProjectFiles.SelectedIsSection: boolean;
begin
  Result := SelectedIsFolder;
  if not Result then exit;

  Result := (TV.Selected.Text <> PROJECT_OUTPUT_FOLDER_MP3) and
            (TV.Selected.Text <> PROJECT_OUTPUT_FOLDER_ZIP);
end;

function TFrameViewProjectFiles.SelectedIsFolderMP3: boolean;
begin
  Result := SelectedIsFolder;
  if not Result then exit;
  Result := TV.Selected.Text = PROJECT_OUTPUT_FOLDER_MP3;
end;

function TFrameViewProjectFiles.SelectedIsFolderZIP: boolean;
begin
  Result := SelectedIsFolder;
  if not Result then exit;
  Result := TV.Selected.Text = PROJECT_OUTPUT_FOLDER_ZIP;
end;

function TFrameViewProjectFiles.SelectedIsFile: boolean;
begin
  Result := False;
  if TV.Selected = NIL then exit;
  Result := TV.Selected.Level = 2;
end;

function TFrameViewProjectFiles.SelectedIsAudioFile: boolean;
var f: string;
begin
  Result := SelectedIsFile;
  if not Result then exit;
  f := ExtractFileExt(SelectedFileName);
  Result := (f = PROJECT_RECORDING_FILE_EXT) or
            (f = PROJECT_MIXED_FILE_EXT);
end;

function TFrameViewProjectFiles.SelectedIsRecordingFile: boolean;
var f: string;
begin
  Result := SelectedIsFile;
  if not Result then exit;
  f := ExtractFileExt(SelectedFileName);
  Result := (f = PROJECT_RECORDING_FILE_EXT);
end;

function TFrameViewProjectFiles.GetNextRecordingPrefixNumberInCurrentSection: integer;
var n: TTreeNode;
  i: integer;
  rff: TRecordingFileFormater;
begin
  n := WorkingSectionNode;

  Result := 1;
  for i:=0 to n.Count-1 do
    if rff.FileRespectRecordingPrefixRules(n.Items[i].Text) then begin
      rff.InitFrom(n.Items[i].Text);
      if Result <= rff.PrefixNumber then Result := rff.PrefixNumber+1;
    end;
end;

function TFrameViewProjectFiles.GetRecordingPrefixNumberOfTheSelectedFile: integer;
var rff: TRecordingFileFormater;
begin
  rff.InitFrom(TV.Selected.Text);
  Result := rff.PrefixNumber;
end;

procedure TFrameViewProjectFiles.AddFileToSelectedFolderInTV(
  const aFullPathFilename: string);
var procEvent: TNotifyEvent;
  n: TTreeNode;
begin
  // cancel the callback OnUserAction to the main form
  // (avoid to clear/load audio on view when we focus on the new added item
  procEvent := TV.OnSelectionChanged;
  TV.OnSelectionChanged := NIL;

  n := TV.Items.AddChild(TV.Selected, ExtractFileName(aFullPathFilename));

  // set the right icon
  UpdateUserMarkIconOnNodeFolder(n, NodeToFilenameOrPath(TV.Selected));
  UpdateUserMarkIconOnFileNode(n, aFullPathFilename);

  // alpha sort the node
  TV.Selected.AlphaSort;

  // focus on new item
  FocusOn(aFullPathFilename);

  // set OnUserAction callback
  TV.OnSelectionChanged := procEvent;
end;

function TFrameViewProjectFiles.GetMixingFilenameForCurrentFolder(aTargetPlatform: TProjectTargetPlatform): string;
var
  n: TTreeNode;
  titleOnly: string;
begin
  Result := '';
  if TV.Selected = NIL then exit;
  if SelectedIsRoot then exit;
  if SelectedIsFile then exit;

  if SelectedIsSection then
    n := TV.Selected
  else
    n := TV.Selected.Parent;

  if FolderRespectPrefixNumberRules(TV.Selected.Text)
    then titleOnly := ExtractTitleWithoutPrefixNumber(n.Text)
    else titleOnly := n.Text;

  case aTargetPlatform of
    ptpNone: begin
      Result := Project.Descriptor.GetProjectSubFolderName+'_'+titleOnly;
    end;

    ptpLitteratureAudio: begin
      // AuthorFirstName_AuthorLastname+'_-_'+Title+'_'+Chapxx+'_'+TitreChapitre+.mp3
      Result := Project.Descriptor.GetProjectSubFolderName + ' ' + titleOnly;
      Result := ReplaceForbidenChar(RemoveAccent(Result), [' ', ''''], '_');
      Result := ReplaceForbidenChar(Result, FILENAMEFORBIDENCHARS,'_');
    end;

    ptpLibriVox: begin
      Result := ReplaceNonASCII(RemoveAccent(Project.Descriptor.Title), '', True);
      if FolderRespectPrefixNumberRules(TV.Selected.Text) then
        ConcatToString(Result, '_', ExtractPrefixNumber(n.Text));
      ConcatToString(Result, '_', ReplaceNonASCII(RemoveAccent(Project.Descriptor.AuthorLastName), '', True));
{      Result := Result+'_'+ExtractPrefixNumber(n.Text)+'_'+
                ReplaceNonASCII(RemoveAccent(Project.Descriptor.AuthorLastName), '', True); }
    end;
  end;//case

  Result := Result + PROJECT_MIXED_FILE_EXT;
end;

function TFrameViewProjectFiles.GetTitleMetadataForCurrentFolder(aTargetPlatform: TProjectTargetPlatform): string;
var titleOnly, s: String;
begin
  Result := '';
  if not FolderRespectPrefixNumberRules(TV.Selected.Text) then exit;

  titleOnly := ExtractTitleWithoutPrefixNumber(TV.Selected.Text);

  case aTargetPlatform of

    ptpLitteratureAudio: begin
      s := '';
      ConcatToString(s, ' ', Project.Descriptor.AuthorFirstName);
      ConcatToString(s, ' ', Project.Descriptor.AuthorLastName);
      if (s <> '') and (Project.Descriptor.Title <> '') then s := ' - ' + s;
      s := Project.Descriptor.Title + s;
      if titleOnly <> '' then
        s := titleOnly+' - '+s;
      Result := Trim(s);
    end;

    ptpNone,
    ptpLibriVox: begin
      Result := ExtractPrefixNumber(TV.Selected.Text)+' - '+
                titleOnly;
      Result := Trim(Result);
    end;

    else Raise Exception.Create('To do !');

  end;//case
end;

function TFrameViewProjectFiles.GetTrackNumberMetadataForCurrentFolder(
  aTargetPlatform: TProjectTargetPlatform): string;
var v: integer;
begin
  Result := '';
  if not FolderRespectPrefixNumberRules(TV.Selected.Text) then exit;
  if not TryStrToInt(ExtractPrefixNumber(TV.Selected.Text), v) then exit;
  Result := v.ToString;
end;

function TFrameViewProjectFiles.GetFilesToMix: TStringArray;
var folder: string;
  i: integer;
begin
  Result := NIL;
  if SelectedIsRoot then exit;
  if not SelectedIsSection then exit;

  folder := IncludeTrailingPathDelimiter(SelectedFilename);

  SetLength(Result, TV.Selected.Count);
  for i:=0 to TV.Selected.Count-1 do
    Result[i] := folder+TV.Selected.Items[i].Text;
end;

procedure TFrameViewProjectFiles.UpdateColors;
begin
  TV.ExpandSignColor := RGBToColor(255,128,0);
  TV.ExpandSignSize := 15;
  TV.TreeLineColor := TV.ExpandSignColor;
  TV.HotTrackColor := TV.Font.Color; //TV.ExpandSignColor;

  TV.Font.Height := ProgramOptions.ViewFilesFontHeight;
end;

procedure TFrameViewProjectFiles.HidePanel;
begin
  HideToolPanels;
end;

procedure TFrameViewProjectFiles.OnLanguageChange;
begin
  FrameProgressBar1.PB.Hint := SClickToNavigate;
end;

procedure TFrameViewProjectFiles.TVMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  n: TTreeNode;
begin
  n := TV.GetNodeAt(X, Y);
  if n = NIL then
    TV.Selected := NIL;

  if (Button = mbLeft) and (TV.Selected <> NIL) then
    ShowToolPanel;
end;

procedure TFrameViewProjectFiles.TVMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (TV.Selected <> NIL) {and
      not SelectedIsRoot} then begin
    if SelectedIsSection then PopupSection.PopUp
    else if SelectedIsFolderMP3 then PopupMP3Folder.PopUp
    else if SelectedIsFolderZIP then PopupZIPFolder.PopUp
    else if SelectedIsFile then PopupRecording.PopUp;

 {   if SelectedIsSection then begin
      if TV.Selected.Text = PROJECT_OUTPUT_FOLDER_MP3 then
        PopupMP3Folder.PopUp
      else if TV.Selected.Text = PROJECT_OUTPUT_FOLDER_ZIP then
        PopupZIPFolder.PopUp
      else
        PopupSection.PopUp;
    end else begin
      if SelectedIsRecordingFile then PopupRecording.PopUp;
    end; }

  end;

  if (Button = mbRight) and (TV.Selected = NIL) then
    PopupEmpty.PopUp;
end;

procedure TFrameViewProjectFiles.TVMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var h: integer;
begin
  StopSound;
  Panel1.Visible := False;
  Panel2.Visible := False;

  if ssCtrl in shift then begin
    h := TV.Font.Height;
    if WheelDelta < 0 then
      dec(h)
    else inc(h);
    TV.Font.Height := EnsureRange(h, 13, 30);

    ProgramOptions.ViewFilesFontHeight := TV.Font.Height;
    //ProgramOptions.Save;
    Handled := True;
  end;
end;

procedure TFrameViewProjectFiles.TVResize(Sender: TObject);
begin
  HideToolPanels;
end;

procedure TFrameViewProjectFiles.BAddRecordClick(Sender: TObject);
var f, s: String;
begin
  f := SelectedFilename;

  if Sender = BPlayAudioFile then StartSound;
  if Sender = BStopAudioFile then StopSound;

  if Sender = BEditFile then begin
    if f = '' then exit;
    if ALSManager.Error then exit;
    StopSound;
    DoUserActionEvent(faEdit);
  end;

  if Sender = MIDeleteFile then begin
    StopSound;
    DoDeleteFile;
  end;

  if (Sender = BAddRecord) and
     Project.TempFolderExists and
     not ALSManager.Error then DoAddRecord(False);

  if (Sender = BMixSection) and
     not ALSManager.Error then begin
    if FolderHaveUserMarksFile(NodeToFilenameOrPath(TV.Selected)) then
      if AskConfirmation(SAskUserToMixFileWithUserMarks, sYes, SCancel, mtConfirmation) = mrCancel then exit;
    DoUserActionEvent(faMixSection);
    Capture.FCaptureContext.MonitoringEnabled := False;
    FormMixer.FirstShow := True;
    if FormMixer.ShowModal = mrOk then begin
      UpdateFileList;
      FocusOn(FormMixer.FrameMixer1.OutputFile);
    end;
    Capture.FCaptureContext.MonitoringEnabled := True;
  end;

  if (Sender = MIImportAudioInSection) and
     not ALSManager.Error then begin
    DoUserActionEvent(faImportAudioInSection);
    DoImportAudioInSection;
  end;

  if (Sender = MIImportAudioAndInsertHere) and
     not ALSManager.Error then begin
    StopSound;
    DoUserActionEvent(faImportAudioAndInsertItBeforeRecord);
    DoImportAudioAndInsertItBeforeSelected;
  end;

  if (Sender = MIInsertFile) and
     Project.TempFolderExists and
     not ALSManager.Error then begin
    StopSound;
    DoUserActionEvent(faInsertNewRecord);
    DoAddRecord(True);
  end;

  if (Sender = BAddSection) or (Sender = MIAddFolder) then begin
    DoUserActionEvent(faAddSection);
    DoNewSection(False);
  end;

  if Sender = MIInsertSection then begin
    StopSound;
    DoUserActionEvent(faInsertSection);
    DoNewSection(True);
  end;

  if Sender = MIDeleteSection then begin
    if f = '' then exit;
    s := Format(SAskConfirmDeleteFolder, [TV.Selected.Text, LineEnding]);
    if AskConfirmation(s, SDelete, SCancel, mtWarning) = mrOk then
      DoDeleteFolder;
    FPreviousSelectedFolderNode := NIL;
  end;

  if Sender = MIRenameSection then DoRenameFolder(f);

  if Sender = MIRenameFile then DoRenameFile;

  if Sender = MIClearMP3Folder then begin
    if not RepertoireExistant(Project.ProjectOutputFolder) then exit;
    if RepertoireEstVide(Project.ProjectOutputFolder) then exit;
    if AskConfirmation(SAskBeforeClearMP3Folder, SYes, SCancel, mtConfirmation) = mrOk then begin
      VideLeRepertoire(Project.ProjectOutputFolder);
      UpdateFileList;
    end;
  end;

  if Sender = MIZipMP3Folder then DoZipMP3Files;

  if Sender = MIDeleteZIPFolder then begin
    if not RepertoireExistant(Project.ProjectZIPFolder) then exit;
    if not RepertoireEstVide(Project.ProjectZIPFolder) then
      if AskConfirmation(SAskBeforeDeleteZIPFolder, SYes, SCancel, mtConfirmation) <> mrOk then exit;
    VideLeRepertoire(Project.ProjectZIPFolder);
    SupprimeRepertoire(Project.ProjectZIPFolder);
    UpdateFileList;
  end;

  if Sender = MIRefresh then UpdateFileList;
end;

procedure TFrameViewProjectFiles.PopupRecordingPopup(Sender: TObject);
begin
  MIInsertFile.Visible := SelectedIsRecordingFile;
  MIImportAudioAndInsertHere.Visible := SelectedIsRecordingFile;
  Separator2.Visible := SelectedIsRecordingFile;
end;

procedure TFrameViewProjectFiles.PopupSectionPopup(Sender: TObject);
begin
  MIInsertSection.Enabled := TV.Selected <> NIL;
end;

procedure TFrameViewProjectFiles.PopupMP3FolderPopup(Sender: TObject);
begin
  MIZipMP3Folder.Enabled := Project.OutputFolderExists and
                            not RepertoireEstVide(Project.ProjectOutputFolder);
  MIClearMP3Folder.Enabled := MIZipMP3Folder.Enabled;
end;

procedure TFrameViewProjectFiles.PopupEmptyPopup(Sender: TObject);
begin
  MIAddFolder.Caption := BAddSection.Hint;
end;

procedure TFrameViewProjectFiles.Timer1Timer(Sender: TObject);
begin
  if FSound <> NIL then begin
    if FSound.State = ALS_PLAYING then
      FrameProgressBar1.Position := FSound.TimePosition/FSound.TotalDuration
    else StopSound;
  end
  else FrameProgressBar1.Position := 0;
end;

procedure TFrameViewProjectFiles.TVAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var r: TRect;
  fc: TColor;
  p: TPoint;
  ft: TFontStyles;
  w: integer;
  txt: String;
begin
  if (Stage = cdPostPaint) and  // all is painted
     (Node.Level = 1) and // its a folder
     (Node.Text <> PROJECT_OUTPUT_FOLDER_MP3) and   // not MP3
     (Node.Text <> PROJECT_OUTPUT_FOLDER_ZIP) then begin  // not ZIP
    // SECTION
    ft := TV.Font.Style;
    r := Node.DisplayRect(True);
    With TV.Canvas do begin
      // background
      if cdsSelected in State then
        Brush.Color := TV.SelectionColor
      else
        Brush.Color := TV.BackgroundColor;

      Brush.Style := bsSolid;
      Pen.Style := psClear;
      FillRect(r);

      // hot track (mouse is over)
      p := TV.ScreenToClient(Mouse.CursorPos);
      if TV.GetNodeAt(p.x, p.y) = Node then
        TV.Font.Style := TV.Font.Style+[fsUnderline];

      // text
      Brush.Style := bsClear;
      if FolderRespectPrefixNumberRules(Node.Text) then begin
        fc := TV.Font.Color;
        TV.Font.Color := PercentColorRelative(TV.Font.Color, 0.4);
        TV.Font.Style := TV.Font.Style-[fsBold];
        txt := ExtractPrefixNumber(Node.Text)+' ';
        w := GetTextWidth(txt);
        TextOut(r.Left+2, r.Top+(r.Height-TV.Font.Height) div 2, txt);

        TV.Font.Color := fc;
        TV.Font.Style := TV.Font.Style+[fsBold];
        TextOut(r.Left+2+w, r.Top+(r.Height-TV.Font.Height) div 2, ExtractTitleWithoutPrefixNumber(Node.Text));
      end else begin
        TV.Font.Style := TV.Font.Style+[fsBold];
        TextOut(r.Left+2, r.Top+(r.Height-TV.Font.Height) div 2, Node.Text);
      end;
      TV.Font.Style := ft;
    end;
    PaintImages := True;
    DefaultDraw := false;
  end else begin
    PaintImages := True;
    DefaultDraw := True;
  end;
end;

procedure TFrameViewProjectFiles.TVChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  UpdateIconOnPreviousSelectedFolder;
end;

procedure TFrameViewProjectFiles.TVDblClick(Sender: TObject);
begin
  if SelectedIsAudioFile then
    if FormMain.FrameViewAudio1.Filename <> SelectedFilename then
      BAddRecordClick(BEditFile);
end;

procedure TFrameViewProjectFiles.UpdateFileList;
var s, sel: string;
  mp3FolderNode, zipFolderNode: TTreeNode;
  procedure ScruteLeDossier(Dossier: string; aNode: TTreeNode);
  var
    sr: TSearchRec;
    n : TTreeNode;
  begin
   if LazFileutils.FindFirstUTF8(Dossier+DirectorySeparator+'*', faAnyFile, sr ) = 0 then begin
     repeat
      if Sr.Attr and faDirectory > 0 then begin
        if not((Sr.Name = '.') or (Sr.Name = '..')) then begin
          // found a folder
          n := TV.Items.AddChild(aNode, Sr.Name);
          if Sr.Name = PROJECT_OUTPUT_FOLDER_MP3 then begin
            mp3FolderNode := n;     // MP3 folder
            n.SelectedIndex := 32;
            n.ImageIndex := n.SelectedIndex;
          end else
          if Sr.Name = PROJECT_OUTPUT_FOLDER_ZIP then begin
            zipFolderNode := n;     // ZIP folder
            n.SelectedIndex := 41;
            n.ImageIndex := n.SelectedIndex;
          end else begin
            inc(FSectionFolderCount);
            UpdateUserMarkIconOnNodeFolder(n, Dossier+DirectorySeparator+Sr.Name);
          end;
          n.Collapse(True);
          ScruteLeDossier( Dossier + DirectorySeparator + Sr.Name, n);
        end;
      end else if (ExtractFileExt(Sr.Name) <> PROJECT_FILE_EXT) and
                  (ExtractFileName(Sr.Name) <> MIX_SESSION_FILENAME) and
                  (ExtractFileExt(Sr.Name) <> USER_MARK_FILE_EXT) then begin
        // found a file
        n := TV.Items.AddChild(aNode, Sr.Name);
        if ExtractFileName(Dossier) = PROJECT_OUTPUT_FOLDER_MP3 then begin
          n.SelectedIndex := 32; // mp3 icon
          n.ImageIndex := n.SelectedIndex;
        end else if ExtractFileName(Dossier) = PROJECT_OUTPUT_FOLDER_ZIP then begin
          n.SelectedIndex := 41; // zip icon
          n.ImageIndex := n.SelectedIndex;
        end else begin
          if FichierExistant(Dossier + DirectorySeparator + ChangeFileExt(Sr.Name, USER_MARK_FILE_EXT)) then begin
            n.SelectedIndex := 36; // record with user marks
            n.ImageIndex := n.SelectedIndex;
          end else begin
            n.SelectedIndex := 19; // record icon
            n.ImageIndex := n.SelectedIndex;
          end;
        end;
      end;
     until LazFileutils.FindNextUTF8(Sr) <> 0;
   end;
   LazFileutils.FindCloseUTF8(Sr);
  end;
begin
  StopSound;
  HideToolPanels;

  FPreviousSelectedFolderNode := NIL;
  mp3FolderNode := NIL;
  zipFolderNode := NIL;
  FSectionFolderCount := 0;

  // save the selected item
  sel := '';
  if TV.Selected <> NIL then
    sel := TV.Selected.GetTextPath;

  TV.Items.Clear;
  if not Project.IsReady then exit;

  TV.BeginUpdate;
  if Project.IsReady then begin
    s := Project.Descriptor.GetProjectSubFolderName;
  end
  else s := SProjectContent;
  with TV.Items.Add(NIL, s) do begin
    SelectedIndex := 17;
    ImageIndex := 17;
  end;
  ScruteLeDossier(Project.ProjectFolder, TV.Items.GetFirstNode); // on lance la recherche récursive
  TV.EndUpdate;
  TV.Items.GetFirstNode.Expand(False);
  TV.AlphaSort;

  // Force MP3 and ZIP folder to the end of the view
  if (mp3FolderNode <> NIL) and (mp3FolderNode <> TV.Items.GetLastNode) then begin
    mp3FolderNode.MoveTo(TV.Items.GetLastNode, naAddChild);
  end;
  if (zipFolderNode <> NIL) and (zipFolderNode <> TV.Items.GetLastNode) then begin
    zipFolderNode.MoveTo(TV.Items.GetLastNode, naAddChild);
  end;

  if sel <> '' then
    TV.Selected := TV.Items.FindNodeWithTextPath(sel);
end;

procedure TFrameViewProjectFiles.FocusOn(const aFilename: string);
var
  folderToFound, textToFound: String;
  n: TTreeNode;
begin
  folderToFound := NomDuDernierSousRepertoire(ExtractFilePath(aFilename));
  folderToFound := ExcludeTrailingPathDelimiter(folderToFound);
  n := TV.Items.FindNodeWithText(folderToFound);
  if n = NIL then exit;

  textToFound := ExtractFileName(aFilename);
  n := n.FindNode(textToFound);

  TV.Selected := n;
  //n.MakeVisible;
  //TV.MakeSelectionVisible;
end;

procedure TFrameViewProjectFiles.ProcessKeyDown(Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE: begin
      if SelectedIsAudioFile then begin
        if SelectedFilename = FormMain.FrameViewAudio1.Filename then begin
          // we start/stop sound from the view "audio edition" if the selected is currently edited
          FormMain.FrameViewAudio1.ProcessKeyDown(Key, Shift);
          exit;
        end;
        if FSound <> NIL then
          StopSound
        else begin
          StartSound;
          FormMain.FrameViewAudio1.Stop;
        end;
      end;
    end;
  end;
end;

end.

