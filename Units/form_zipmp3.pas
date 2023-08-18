unit form_zipmp3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, frame_zipfiles, LCL_utils;

type

  { TFormZipMP3 }

  TFormZipMP3 = class(TForm)
    BCheckAll: TSpeedButton;
    BCheckFileForLitAudio: TSpeedButton;
    BZipFiles: TSpeedButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PanelZipProgress: TPanel;
    Panel2: TPanel;
    BUncheckAll: TSpeedButton;
    Shape1: TShape;
    TV: TTreeView;
    procedure BZipFilesClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure TVMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
  private
    FrameZipFiles1: TFrameZipFiles;
    ToggleSpeedButtonManager1: TToggleSpeedButtonManager;
    procedure UpdateFileList;
    function GetSelectedFiles: TStringArray;
    function AllFilesAreSelected: boolean;
    procedure DoZipFiles;
    procedure SetStateAll(aState: boolean);
    function FormatedOutputZipFilename: string;
    procedure UpdateBZipFilesButtonState;
    procedure AdjustFont;
  public

  end;

var
  FormZipMP3: TFormZipMP3;

implementation
uses LazFileutils, LCLType, u_project, u_resource_string, u_utils, u_common,
  u_userdialogs, form_main, u_crossplatform, u_program_options, utilitaire_fichier;

{$R *.lfm}

{ TFormZipMP3 }

procedure TFormZipMP3.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckKeyToShowUserGuide(Key, Shift) then
    ShowGYVUserGuide;

  case Key of
    VK_ESCAPE: ModalResult := mrCancel;
  end;
end;

procedure TFormZipMP3.Edit1Change(Sender: TObject);
begin
  // error label
  Label4.Visible := StringHaveForbiddenChar(Edit1.Text, FILENAMEFORBIDENCHARS);

  if Label4.Visible
    then Label6.Caption := ''
    else Label6.Caption := FormatedOutputZipFilename;

  UpdateBZipFilesButtonState;
end;

procedure TFormZipMP3.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // update program options
  if ProgramOptions.MP3ToZipIncludeFileForLitteratureAudio <> ToggleSpeedButtonManager1.Checked[BCheckFileForLitAudio] then begin
    ProgramOptions.MP3ToZipIncludeFileForLitteratureAudio := ToggleSpeedButtonManager1.Checked[BCheckFileForLitAudio];
    ProgramOptions.Save;
  end;
end;

procedure TFormZipMP3.FormCreate(Sender: TObject);
begin
  FrameZipFiles1 := TFrameZipFiles.Create(Self);
  FrameZipFiles1.Parent := PanelZipProgress;
  FrameZipFiles1.Align := alClient;

  ToggleSpeedButtonManager1 := TToggleSpeedButtonManager.Create;
  ToggleSpeedButtonManager1.ToggleType := tsbLikeCheckBox;
  ToggleSpeedButtonManager1.SetImageIndexes(49, 48);
  ToggleSpeedButtonManager1.Add(BCheckFileForLitAudio, False);
end;

procedure TFormZipMP3.FormDestroy(Sender: TObject);
begin
  ToggleSpeedButtonManager1.Free;
end;

procedure TFormZipMP3.BZipFilesClick(Sender: TObject);
begin
  if Sender = BZipFiles then DoZipFiles;

  if Sender = BUncheckAll then SetStateAll(False);
  if Sender = BCheckAll then SetStateAll(True);

  UpdateBZipFilesButtonState;
end;

procedure TFormZipMP3.FormShow(Sender: TObject);
var s: string;
begin
  AdjustFont;

  Label7.Caption := Format(SMP3ToZipIncludeFile, [LITTERATUREAUDIO_TEXT_FILENAME_FOR_ZIP_ARCHIVE]);
  Label4.Caption := SPleaseNoSpecialCharacters;
  BUncheckAll.Caption := SNone;
  BCheckAll.Caption := SAll;
  FrameZipFiles1.ClearLabels;
  PanelZipProgress.Visible := False;

  // read programOptions
  ToggleSpeedButtonManager1.Checked[BCheckFileForLitAudio] := ProgramOptions.MP3ToZipIncludeFileForLitteratureAudio;

  // zip filename by default
  s := '';
  ConcatToString(s, ' ', Project.Descriptor.AuthorFirstName);
  ConcatToString(s, ' ', Project.Descriptor.AuthorLastName);
  ConcatToString(s, ' - ', Project.Descriptor.Title);
  Edit1.Text := s;
  Label6.Caption := FormatedOutputZipFilename;
  BZipFiles.Enabled := False;

  UpdateFileList;
end;

procedure TFormZipMP3.Label7Click(Sender: TObject);
begin
  if Sender = Label7 then
    ToggleSpeedButtonManager1.ToogleState(BCheckFileForLitAudio);

  Label6.Caption := FormatedOutputZipFilename;
end;

procedure TFormZipMP3.TVMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var n: TTreeNode;
begin
  n := TV.GetNodeAt(X, Y);
  if n = NIL then exit;
  if n.ImageIndex = 49 then begin
    n.SelectedIndex := 48;
    n.ImageIndex := 48;
  end else begin
    n.SelectedIndex := 49;
    n.ImageIndex := 49;
  end;

  BZipFiles.Enabled := not Label4.Visible and (Length(GetSelectedFiles) > 0);
end;

procedure TFormZipMP3.UpdateFileList;
  procedure ScruteLeDossier(Dossier: string; aNode: TTreeNode);
  var sr: TSearchRec;
    n : TTreeNode;
  begin
   if LazFileutils.FindFirstUTF8(Dossier+DirectorySeparator+'*', faAnyFile, sr ) = 0 then begin
     repeat
      if Sr.Attr and faDirectory > 0 then begin
        if not((Sr.Name = '.') or (Sr.Name = '..')) then begin
          // found a folder ?? ignore it
        end;
      end else if ExtractFileExt(Sr.Name) = PROJECT_MIXED_FILE_EXT then begin
        // found a zip file
        n := TV.Items.AddChild(aNode, Sr.Name);
        n.SelectedIndex := 48; // not selected
        n.ImageIndex := 48;
      end;
     until LazFileutils.FindNextUTF8(Sr) <> 0;
   end;
   LazFileutils.FindCloseUTF8(Sr);
  end;
begin
  TV.Items.Clear;
  if not Project.IsReady then exit;

  TV.BeginUpdate;

  with TV.Items.Add(NIL, '') do begin
    SelectedIndex := 32;
    ImageIndex := 32;
  end;
  ScruteLeDossier(Project.ProjectOutputFolder, TV.Items.GetFirstNode); // on lance la recherche récursive
  TV.EndUpdate;
  TV.Items.GetFirstNode.Expand(False);
  TV.AlphaSort;
end;

function TFormZipMP3.GetSelectedFiles: TStringArray;
var n: TTreeNode;
  i, c: integer;
begin
  Result := NIL;
  n := TV.Items.GetFirstNode;
  if n = NIL then exit;
  if n.Count = 0 then exit;

  c := 0;
  for i:=0 to n.Count-1 do
    if n.Items[i].ImageIndex = 49 then
      inc(c);

  if c = 0 then exit;
  SetLength(Result, c);

  c := 0;
  for i:=0 to n.Count-1 do
    if n.Items[i].ImageIndex = 49 then begin
      Result[c] := Project.ProjectOutputFolder+n.Items[i].Text;
      inc(c);
    end;
end;

function TFormZipMP3.AllFilesAreSelected: boolean;
var n: TTreeNode;
  i: integer;
begin
  Result := False;
  n := TV.Items.GetFirstNode;
  if n = NIl then exit;

  Result := True;
  for i:=0 to n.Count-1 do
    Result := Result and (n.Items[i].ImageIndex = 49);
end;


procedure TFormZipMP3.DoZipFiles;
var
  A: TStringArray;
  folder: string;
  zipFileName: string;
  res: boolean;
begin
  if Trim(Edit1.Text) = '' then exit;
  folder := Project.ProjectZIPFolder;
  if not RepertoireExistant(folder) then exit;

  A := GetSelectedFiles;
  if Length(A) = 0 then exit;

  // Add the file requested by the platform litteratureaudio.com
  if ToggleSpeedButtonManager1.Checked[BCheckFileForLitAudio] then begin
    SetLength(A, Length(A)+1);
    A[High(A)] := GetAppDataFolder+LITTERATUREAUDIO_TEXT_FILENAME_FOR_ZIP_ARCHIVE;
  end;

  PanelZipProgress.Visible := True;
  BUncheckAll.Enabled := False;
  TV.Enabled := False;
  Edit1.Enabled := False;
  BZipFiles.Enabled := False;
  try
  zipFileName := Project.ProjectZIPFolder+FormatedOutputZipFilename;
  res := FrameZipFiles1.ZipFiles(zipFileName, '', A);
    if not res then
     if not FrameZipFiles1.Canceled then
      ShowMess(SZipCreationFailed, SOk, mtError);

    FrameZipFiles1.ClearLabels;

  finally
    PanelZipProgress.Visible := False;
    BUncheckAll.Enabled := True;
    TV.Enabled := True;
    Edit1.Enabled := True;
    BZipFiles.Enabled := True;
  end;

  // update the project file view and make visible the new zipped file
  if res then begin
    FormMain.FrameViewProjectFiles1.UpdateFileList;
    FormMain.FrameViewProjectFiles1.FocusOn(zipFileName);
  end;

  // close the window if all files are selected
  if AllFilesAreSelected then Close;
end;

procedure TFormZipMP3.SetStateAll(aState: boolean);
var n: TTreeNode;
  i: integer;
begin
  TV.BeginUpdate;
  n := TV.Items.GetFirstNode;
  for i:=0 to n.Count-1 do begin
    if aState  then begin
      n.Items[i].ImageIndex := 49;
      n.Items[i].SelectedIndex := 49;
    end else begin
      n.Items[i].ImageIndex := 48;
      n.Items[i].SelectedIndex := 48;
    end;
  end;
  TV.EndUpdate;

  UpdateBZipFilesButtonState;
end;

function TFormZipMP3.FormatedOutputZipFilename: string;
begin
  Result := Trim(Edit1.Text);

  if ToggleSpeedButtonManager1.Checked[BCheckFileForLitAudio] then
    Result := ReplaceForbidenChar(RemoveAccent(Result), ['''',' '], '_');

  Result := Result+PROJECT_ZIP_FILE_EXT;
end;

procedure TFormZipMP3.UpdateBZipFilesButtonState;
begin
    BZipFiles.Enabled := not Label4.Visible and (Length(GetSelectedFiles) > 0);
end;

procedure TFormZipMP3.AdjustFont;
begin
  FrameZipFiles1.AdjustFont;
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
  ChangeFontHeight([Label4], FDesignSmallFontHeight);
  ChangeFontColor([Edit1], clBlack);
{$endif}
{$if defined(LCLCOCOA)}
  BZipFiles.Flat := False;
{$endif}
end;

end.

