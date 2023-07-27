unit form_project_manager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ComCtrls, ExtCtrls,
  Zipper,
  frame_progressbar;

type

  { TFormProjectManager }

  TFormProjectManager = class(TForm)
    BCancel: TSpeedButton;
    BCancelZipper: TSpeedButton;
    BNewProject: TSpeedButton;
    BDeleteProject: TSpeedButton;
    BZipProject: TSpeedButton;
    BUnzipProject: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Notebook1: TNotebook;
    PageZipper: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelZipProgress: TPanel;
    PanelToolsFolder: TPanel;
    PanelToolsProject: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Shape1: TShape;
    BSearchFolder: TSpeedButton;
    BOpenProject: TSpeedButton;
    TVProjects: TTreeView;
    TVPaths: TTreeView;
    procedure BOpenProjectClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure TVPathsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure TVPathsSelectionChanged(Sender: TObject);
    procedure TVProjectsDblClick(Sender: TObject);
    procedure TVProjectsSelectionChanged(Sender: TObject);
  private
    FProjectFileFound: TStringList;
    function GetSelectedProjectFile: string;
    procedure UpdateWorkingDirectoryView;
    procedure UpdateProjectList;
    procedure UpdateWidgets;
    function SelectedIsProject: boolean;
    function SelectedFolderExists: boolean;
    function SelectedProjectFolder: string;
    function SelectedProjectMP3Folder: string;
    function SelectedProjectZIPFolder: string;
  private
    FrameProgressBar1,
    FrameProgressBar2: TFrameProgressBar;
    FFileCountToProcess,
    FProcessedFileCount: integer;
    FZipper: TZipper;
    FUnzipper: TUnzipper;
    function SelectedIsZipped: boolean;
    function GetSelectedProjectZipFilename: string;
    procedure PrepareGUIToZipUnzip;
    procedure PrepareGUIAfterZipUnzip;
    procedure DoZipSelectedProject;
    procedure DoUnzipSelectedProject;
    procedure DoDeleteProject;

    procedure ProcessZipperOnProgressEvent(Sender: TObject; const Pct: Double);
    procedure ProcessZipperOnStartFileEvent(Sender: TObject; Const AFileName: String);

    procedure AdjustFont;
  public
  end;

var
  FormProjectManager: TFormProjectManager;

implementation

uses u_program_options, u_crossplatform, u_common, u_project, u_utils,
  utilitaire_fichier, LCLType, form_main, u_userdialogs, u_resource_string,
  FileUtil, Math;

{$R *.lfm}

{ TFormProjectManager }

procedure TFormProjectManager.BOpenProjectClick(Sender: TObject);
begin
  if (Sender = BOpenProject) and SelectedIsProject then begin
    FormMain.FrameViewAudio1.Clear;
    FormMain.NoteBook1.PageIndex := FormMain.NoteBook1.IndexOf(FormMain.PageEmpty);
    //Project.OpenDialog.InitialDir := Project.ProjectsFolder;
    Project.Load(GetSelectedProjectFile);
    ModalResult := mrOk;
  end;

  if (Sender = BNewProject) and SelectedFolderExists then begin
    FormMain.FrameViewAudio1.Clear;
    FormMain.NoteBook1.PageIndex := FormMain.NoteBook1.IndexOf(FormMain.PageEmpty);
    Project.New;
    if Project.IsReady then ModalResult := mrOk;
  end;

  if Sender = BCancel then begin
    modalResult := mrCancel;
  end;

  if Sender = BSearchFolder then begin
    SelectDirectoryDialog1.InitialDir := GetAppDefaultProjectFolder;
    if not SelectDirectoryDialog1.Execute then exit;

    with ProgramOptions do begin
      UserWorkingDirectoryIndex := UserWorkingDirectoryList.AddItem(SelectDirectoryDialog1.FileName);
      Save;
    end;
    UpdateWorkingDirectoryView;
  end;

  if Sender = BZipProject then begin
    if Project.Filename = GetSelectedProjectFile then
      Project.Close;
    DoZipSelectedProject;
  end;

  if (Sender = BCancelZipper) and (FZipper <> NIL) then
    FZipper.Terminate;
  if (Sender = BCancelZipper) and (FUnzipper <> NIL) then
    FUnzipper.Terminate;

  if Sender = BUnzipProject then begin
    DoUnzipSelectedProject;
  end;

  if Sender = BDeleteProject then begin
    DoDeleteProject;
  end;
end;

procedure TFormProjectManager.FormActivate(Sender: TObject);
begin
  // ask to user if s/he want to read the user guide
  if FormProjectManager_FAskUserToShowUserGuide then begin
    Application.ProcessMessages;
    if AskConfirmation(SDoYouWantToShowUserGuide, SYes, SNo, mtConfirmation) = mrOk then
      ShowGYVUserGuide;
    FormProjectManager_FAskUserToShowUserGuide := False;
  end;
end;

procedure TFormProjectManager.FormCreate(Sender: TObject);
begin
  FProjectFileFound := TStringList.Create;

  FrameProgressBar1 := TFrameProgressBar.Create(Self);
  FrameProgressBar1.Name := 'FrameProgressBar1';
  FrameProgressBar1.Parent := Panel1;
  FrameProgressBar1.Align := alClient;

  FrameProgressBar2 := TFrameProgressBar.Create(Self);
  FrameProgressBar2.Name := 'FrameProgressBar2';
  FrameProgressBar2.Parent := Panel2;
  FrameProgressBar2.Align := alClient;
end;

procedure TFormProjectManager.FormDestroy(Sender: TObject);
begin
  FProjectFileFound.Free;
end;

procedure TFormProjectManager.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: ModalResult := mrCancel;
    VK_F1: ShowGYVUserGuide;
  end;
end;

procedure TFormProjectManager.FormShow(Sender: TObject);
begin
  AdjustFont;

  BCancel.Caption := SClose;
  BCancelZipper.Caption := SCancel;

  // fill the folder list
  UpdateWorkingDirectoryView;
end;

procedure TFormProjectManager.TVPathsChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange :=  Node <> (Sender as TTreeView).Items.GetFirstNode;
end;

procedure TFormProjectManager.TVPathsSelectionChanged(Sender: TObject);
begin
  TVProjects.Items.Clear;
  if TVPaths.Selected = NIL then exit;

  ProgramOptions.UserWorkingDirectoryIndex := TVPaths.Items.GetFirstNode.IndexOf(TVPaths.Selected);
  ProgramOptions.Save;

  UpdateProjectList;
  UpdateWidgets;
end;

procedure TFormProjectManager.TVProjectsDblClick(Sender: TObject);
begin
  if SelectedIsProject and not SelectedIsZipped then
    BOpenProjectClick(BOpenProject);
end;

procedure TFormProjectManager.TVProjectsSelectionChanged(Sender: TObject);
begin
  UpdateWidgets;
end;

procedure TFormProjectManager.UpdateProjectList;
var t, tProjectFile: TStringList;
  n: TTreeNode;
  des: TProjectDescriptor;
  i: integer;
  fileExt: string;
begin
  Screen.BeginWaitCursor;
  try
    FProjectFileFound.Clear;
    TVProjects.Items.Clear;
    TVProjects.BeginUpdate;

    // add the root with the project's Folder
    with TVProjects.Items.Add(NIL, SelectedProjectFolder) do begin
      SelectedIndex := 31;
      ImageIndex := 31;
    end;

    n := TVProjects.Items.GetFirstNode;

    // get the content of the selected folder
    t := GetDirectoryContent(SelectedProjectFolder, [PROJECT_FILE_EXT, '.zip'], True, 1);
    t.Sort;

    // discard all zip files that are in sub-folder (in project's zip folder)
    for i:=t.Count-1 downto 0 do begin
      if (ExtractFileExt(t.Strings[i]) = PROJECT_ZIP_FILE_EXT) and
         (ExtractFilePath(t.Strings[i]) <> '') then begin
        t.Delete(i);
      end;
    end;

    tProjectFile := TStringList.Create;
    for i:=0 to t.Count-1 do begin
      fileExt := ExtractFileExt(t.Strings[i]);

      case fileExt of
        PROJECT_FILE_EXT: begin
         try  // found file.gyv
           tProjectFile.LoadFromFile(SelectedProjectFolder+t.Strings[i]);
           if ApplicationHeaderIsValid(tProjectFile) then begin
             // add node
             des.LoadFrom(tProjectFile);
             // add entry in list of project found and link between TVProjects and FProjectFileFound
             with TVProjects.Items.AddChild(n, des.GetProjectTitleWithoutPath) do begin
               Data := Pointer(FProjectFileFound.Add(t.Strings[i]));
               SelectedIndex := 17;
               ImageIndex := 17;
             end;
           end;
         except
         end;
        end;

        '.zip': begin
         with TVProjects.Items.AddChild(n, t.Strings[i]) do begin
           Data := Pointer(FProjectFileFound.Add(t.Strings[i]));
           SelectedIndex := 41;
           ImageIndex := 41;
         end;
        end;

      end;//case
    end;

    t.Free;
    tProjectFile.Free;

    TVProjects.EndUpdate;
    TVProjects.Items.GetFirstNode.Expand(False);
  finally
    Screen.EndWaitCursor;
  end;
end;

function TFormProjectManager.GetSelectedProjectFile: string;
var i: int64;
begin
  i := int64(TVProjects.Selected.Data);
  Result := SelectedProjectFolder+FProjectFileFound.Strings[i];
end;

function TFormProjectManager.GetSelectedProjectZipFilename: string;
begin
  Result := ChangeFileExt(ExtractFileName(GetSelectedProjectFile), PROJECT_ZIP_FILE_EXT);
  Result := RepertoireParent(ExtractFilePath(GetSelectedProjectFile))+Result;
end;

procedure TFormProjectManager.PrepareGUIToZipUnzip;
begin
  Label2.Caption := '';
  Label4.Caption := '';
  FrameProgressBar1.Position := 0.0;
  FrameProgressBar2.Position := 0.0;
  PanelZipProgress.Visible := True;
  PanelToolsProject.Enabled := False;
  PanelToolsFolder.Enabled := False;
  TVPaths.Enabled := False;
  TVProjects.Enabled := False;
end;

procedure TFormProjectManager.PrepareGUIAfterZipUnzip;
begin
  PanelZipProgress.Visible := False;
  PanelToolsProject.Enabled := True;
  PanelToolsFolder.Enabled := True;
  TVPaths.Enabled := True;
  TVProjects.Enabled := True;
  Application.ProcessMessages;
end;

procedure TFormProjectManager.UpdateWorkingDirectoryView;
var i: integer;
  n: TTreeNode;
begin
  TVPaths.BeginUpdate;
  TVPaths.Items.Clear;
  // add the root with the project's Folder
  with TVPaths.Items.Add(NIL, 'Répertoire de travail') do begin
    //SelectedIndex := 31;
    //ImageIndex := 31;
  end;
  n := TVPaths.Items.GetFirstNode;
  for i:=0 to High(ProgramOptions.UserWorkingDirectoryList.Items) do begin
   with TVPaths.Items.AddChild(n, ProgramOptions.UserWorkingDirectoryList.Items[i]) do begin
     SelectedIndex := 31;
     ImageIndex := 31;
   end;
  end;
  TVPaths.EndUpdate;

  // select one
  if n.Count > 0 then begin
   TVPaths.Selected := n.Items[EnsureRange(ProgramOptions.UserWorkingDirectoryIndex, 0, n.Count-1)];
  end;
  TVPaths.Items.GetFirstNode.Expand(False);
end;

procedure TFormProjectManager.UpdateWidgets;
begin
  BOpenProject.Enabled := SelectedIsProject and not SelectedIsZipped;
  BNewProject.Enabled := SelectedFolderExists;
  BDeleteProject.Enabled := SelectedIsProject;

  BZipProject.Enabled := SelectedIsProject and not SelectedIsZipped;
  BUnzipProject.Enabled := SelectedIsProject and SelectedIsZipped;
end;

function TFormProjectManager.SelectedIsProject: boolean;
begin
  Result := (TVProjects.Selected <> NIL) and (TVProjects.Selected <> TVProjects.Items.GetFirstNode);
end;

function TFormProjectManager.SelectedIsZipped: boolean;
begin
  Result := ExtractFileExt(GetSelectedProjectFile) = PROJECT_ZIP_FILE_EXT;
end;

function TFormProjectManager.SelectedFolderExists: boolean;
begin
  Result := RepertoireExistant(SelectedProjectFolder);
end;

function TFormProjectManager.SelectedProjectFolder: string;
begin
  if TVPaths.Selected <> NIL
    then Result := IncludeTrailingPathDelimiter(TVPaths.Selected.Text)
    else Result := '';
end;

function TFormProjectManager.SelectedProjectMP3Folder: string;
begin
  Result := ExtractFilePath(GetSelectedProjectFile);
  if Result <> '' then
    Result := Result + PROJECT_OUTPUT_FOLDER_MP3 + DirectorySeparator;
end;

function TFormProjectManager.SelectedProjectZIPFolder: string;
begin
  Result := ExtractFilePath(GetSelectedProjectFile);
  if Result <> '' then
    Result := Result + PROJECT_OUTPUT_FOLDER_ZIP + DirectorySeparator;
end;

procedure TFormProjectManager.DoZipSelectedProject;
var folder, folderToZip, zipMainFolder, zipFilename: string;
  t: TStringList;
  i: integer;
  flagError: boolean;
  unzipper: TUnZipper;
begin
  if SelectedIsZipped then exit;

  // ask user if we delete mp3 file before compress
  folder := SelectedProjectMP3Folder;
  if RepertoireExistant(folder) and not RepertoireEstVide(folder) then
    if AskConfirmation(SAskToEraseMP3FilesBeforeZip, SYes, SNo, mtConfirmation) = mrOk then
      VideLeRepertoire(folder);

  // ask user if we delete the project's zip folder before compress
  folder := SelectedProjectZIPFolder;
  if RepertoireExistant(folder) then
    if AskConfirmation(SAskToEraseZIPFolderBeforeZip, SYes, SNo, mtConfirmation) = mrOk then
      SupprimeRepertoire(folder);

  folderToZip := IncludeTrailingPathDelimiter(ExtractFilePath(GetSelectedProjectFile));
  // get the project's folder content
  t := ContenuDuRepertoire(folderToZip, [], True, True);

  if t.Count = 0 then exit;

  PrepareGUIToZipUnzip;

  FFileCountToProcess := t.Count;
  FProcessedFileCount := 0;

  zipFilename := GetSelectedProjectZipFilename;
  zipMainFolder := IncludeTrailingPathDelimiter(ChangeFileExt(ExtractFileName(zipFilename), ''));

  FZipper := TZipper.Create;
  Application.ProcessMessages;
  flagError := False;
  try
    try
     FZipper.FileName := zipFilename;
     for i:=0 to t.Count-1 do
       FZipper.Entries.AddFileEntry(folderToZip + t.Strings[i], zipMainFolder+t.Strings[i]);
     FZipper.OnProgress := @ProcessZipperOnProgressEvent;
     FZipper.OnStartFile := @ProcessZipperOnStartFileEvent;
     // By default zipper writes file names in encoding of the IBM PC, CP437.
     // UTF8 encoding is written when UseLanguageEncoding is true.
     FZipper.UseLanguageEncoding := true;  // Requires FPC 3.2+
     FZipper.ZipAllFiles;
    except
     flagError := true;
    end;

    // verify the zip
    if not FZipper.Terminated and not flagError then begin
      unzipper := TUnZipper.Create;
      try
       unzipper.UseUTF8 := True;
       unzipper.FileName := zipFilename;
       unzipper.OutputPath := ''; // Project.TempFolder;
       unzipper.Examine;
       flagError := unzipper.Entries.Count <> FZipper.Entries.Count;
       i := 0;
       while (i < unzipper.Entries.Count) and not flagError do begin
        flagError := unzipper.Entries[i].Size <> FZipper.Entries[i].Size;
        inc(i);
       end;
      finally
       unzipper.Free;
      end;
    end;

    // show error message
    if not FZipper.Terminated and flagError then
      ShowMess(SZipProjectFailed, SClose, mtError);

    // user have canceled ?
    if (FZipper.Terminated or flagError) and FichierExistant(zipFilename) then
      SupprimeFichier(zipFilename);

    // all is right -> delete the folder
    if not FZipper.Terminated and not flagError then
      SupprimeRepertoire(folderToZip);

  finally
    PrepareGUIAfterZipUnzip;
    t.Free;
    FZipper.Free;
    FZipper := NIL;
  end;

  Application.ProcessMessages;
  i := TVProjects.Items.GetFirstNode.IndexOf(TVProjects.Selected);
  UpdateProjectList;
  if i <> -1 then
    TVProjects.Selected := TVProjects.Items.GetFirstNode.Items[i];
end;

procedure TFormProjectManager.DoUnzipSelectedProject;
var folderCreated: string;
  flagError: boolean;
  i: integer;
begin
  if not SelectedIsZipped then exit;

  if SelectedProjectFolder = '' then exit;

  PrepareGUIToZipUnzip;

  folderCreated := SelectedProjectFolder+ChangeFileExt(ExtractFileName(GetSelectedProjectFile), '');

  flagError := False;
  FUnzipper := TUnzipper.Create;
  try
    FUnzipper.UseUTF8 := True;
    FUnzipper.FileName := GetSelectedProjectFile;
    FUnzipper.OutputPath := SelectedProjectFolder;
    FUnzipper.OnProgress := @ProcessZipperOnProgressEvent;
    FUnzipper.OnStartFile := @ProcessZipperOnStartFileEvent;
    try
      FUnzipper.Examine;
      FFileCountToProcess := FUnzipper.Entries.Count;
      FProcessedFileCount := 0;
      FUnzipper.UnZipAllFiles;
    except
      flagError := True;
    end;

    // show error message
    if not FUnzipper.Terminated and flagError then
      ShowMess(SUnzipProjectFailed, SClose, mtError);

    // user have canceled ?
    if (FUnzipper.Terminated or flagError) and RepertoireExistant(folderCreated) then
      SupprimeRepertoire(folderCreated);

    // all is right -> delete the zip file
    if not FUnzipper.Terminated and not flagError then
      SupprimeFichier(GetSelectedProjectFile);
  finally
    PrepareGUIAfterZipUnzip;
    FUnzipper.Free;
    FUnzipper := NIL;
  end;

  Application.ProcessMessages;
  i := TVProjects.Items.GetFirstNode.IndexOf(TVProjects.Selected);
  UpdateProjectList;
  if i <> -1 then
    TVProjects.Selected := TVProjects.Items.GetFirstNode.Items[i];
end;

procedure TFormProjectManager.DoDeleteProject;
var res: string;
  flagDeleted: boolean;
begin
  res := '';
  if not UserInputString(SWarningBeforeDeletingProject, SOk,
                     SCancel, mtWarning, res, False, False) then exit;
  if LowerCase(res) <> LowerCase(SDelete) then exit;

  if SelectedIsZipped then begin
    flagDeleted := MoveFilesToTrash(GetSelectedProjectFile);
    if not flagDeleted then
      flagDeleted := SupprimeFichier(GetSelectedProjectFile)
  end else begin
    flagDeleted := MoveFilesToTrash(ExtractFilePath(GetSelectedProjectFile));
    if not flagDeleted then
      flagDeleted := SupprimeRepertoire(ExtractFilePath(GetSelectedProjectFile));
  end;
  if not flagDeleted then
    ShowMess(SFailedToDeleteProject, SOk, mtError);

  UpdateProjectList;
end;

procedure TFormProjectManager.ProcessZipperOnProgressEvent(Sender: TObject;
  const Pct: Double);
begin
  FrameProgressBar1.Position := Pct*0.01;
  Application.ProcessMessages;
end;

procedure TFormProjectManager.ProcessZipperOnStartFileEvent(Sender: TObject;
  const AFileName: String);
begin
  Label2.Caption := NomDuDernierSousRepertoire(ExtractFilePath(AFileName))+
                    ExtractFilename(AFileName);
  inc(FProcessedFileCount);
  Label4.Caption := FProcessedFileCount.ToString+'/'+FFileCountToProcess.ToString;
  FrameProgressBar2.Position := FProcessedFileCount/FFileCountToProcess;
  Application.ProcessMessages;
end;

procedure TFormProjectManager.AdjustFont;
begin
{$ifdef LINUX}
  Font.Height := FDesignFontHeight;
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
{$endif}
end;

end.

