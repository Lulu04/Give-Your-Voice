unit u_project;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, project_util, LCLTranslator, u_resource_string, u_common;

type

  { TProjectDescriptor }

  PProjectDescriptor = ^TProjectDescriptor;
  TProjectDescriptor = record
  public
    AuthorFirstName, // prénom de l'auteur
    AuthorLastName,  // nom
    Title: string;   // titre de l'ouvrage

  public  // Last user informations for folder header format
    FolderHeader_DigitCountForPrefixNumber: integer; // digit count for ex: xxx Chap01 Title
    FolderHeader_DigitCountForSectionNumber: integer;// digit count for ex: Chapxx
    FolderHeader_IncludeBookInfo: boolean;
    FolderHeader_BookLetter: string;
    FolderHeader_BookNumber: integer;
    FolderHeader_BookTitle: string;
    FolderHeader_IncludeSectionInfo: boolean;
    FolderHeader_SectionPrefix: string;
    FolderHeader_SectionNumber: integer;

  public  // User metadata
    Artist,
    Album,
    Comment,
    Genre: string;

  public  // User current page number
    CurrentPageNumber: integer;

    procedure InitDefault;
    procedure SaveTo(t: TStringList);
    procedure LoadFrom(t: TStringList);

    // Gives only the name of the sub-folder where the project is saved in
    function GetProjectSubFolderName: string;

    // Gives a string formated as: 'FirstName LastName - BookTitle'
    function GetProjectTitleWithoutPath: string;
  end;

  { TProjectOptions }

  TProjectOptions = record
    AudioSilenceAttenuationTime: single; // time to lower and restore volume when applying silence
    RemoveNoiseWhenRecording: boolean; // True to remove noise when recording
    procedure InitDefault;
    function SaveToString: string;
    procedure LoadFromString(const s: string);
  end;

  { TProject }

  TProject = class(TCustomProject)
  private
    FTempFolder: string;
    function GetProjectFolder: string;
    function GetProjectOutputFolder: string;
    function GetProjectZIPFolder: string;
    function GetProjectsFolder: string;
  public
    Descriptor: TProjectDescriptor;
    Options: TProjectOptions;
    constructor Create;
    function DoNew: boolean; override;
    procedure DoSave(const aFilename: string); override;
    function DoLoad(const aFilename: string): boolean; override;
    procedure DoClose; override;

    procedure OnModifiedChange(aState: boolean); override;

    procedure OnProjectReadyChange; override;

    procedure CheckSaveFolders;

    function CreateProjectZipFolder: boolean;

    function TempFolderExists: boolean;
    function OutputFolderExists: boolean;

    procedure OnLanguageChange;

    // folder for temporary files (common for all projects)
    property TempFolder: string read FTempFolder;
    // folder where to save all projects
    property ProjectsFolder: string read GetProjectsFolder;

    // folder for the current opened project
    property ProjectFolder: string read GetProjectFolder;
    // project folder for the mp3 output files
    property ProjectOutputFolder: string read GetProjectOutputFolder;
    // project folder for ZIP output files
    property ProjectZIPFolder: string read GetProjectZIPFolder;
  end;


var
  Project: TProject;

implementation
uses form_main, PropertyUtils, utilitaire_fichier, u_logfile, form_new_project,
  Controls, u_utils, u_program_options, u_userdialogs, u_main_undoredo,
  u_audio_utils, u_crossplatform, Dialogs;

{ TProjectOptions }

procedure TProjectOptions.InitDefault;
begin
  AudioSilenceAttenuationTime := 0.1;
  RemoveNoiseWhenRecording := True;
end;

function TProjectOptions.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('|');
  prop.Add('AudioSilenceAttenuationTime', AudioSilenceAttenuationTime);
  prop.Add('RemoveNoiseWhenRecording', RemoveNoiseWhenRecording);

  Result := prop.PackedProperty;
end;

procedure TProjectOptions.LoadFromString(const s: string);
var prop: TProperties;
begin
  InitDefault;
  prop.Split(s, '|');
  prop.SingleValueOf('AudioSilenceAttenuationTime', AudioSilenceAttenuationTime, AudioSilenceAttenuationTime);
  prop.BooleanValueOf('RemoveNoiseWhenRecording', RemoveNoiseWhenRecording, RemoveNoiseWhenRecording);
end;

{ TProjectDescriptor }

procedure TProjectDescriptor.InitDefault;
begin
  AuthorFirstName := '';
  AuthorLastName := '';
  Title := '';

  FolderHeader_DigitCountForPrefixNumber := 1;
  FolderHeader_DigitCountForSectionNumber := 2;
  FolderHeader_IncludeBookInfo := False;
  FolderHeader_BookLetter := '';
  FolderHeader_BookNumber := 1;
  FolderHeader_BookTitle := '';
  FolderHeader_IncludeSectionInfo := False;
  FolderHeader_SectionPrefix := '';
  FolderHeader_SectionNumber := 1;

  Artist := '';
  Album := '';
  Comment := '';
  Genre := SAudioBook;

  CurrentPageNumber := 1;
end;

function TProjectDescriptor.GetProjectSubFolderName: string;
begin
  Result := GetProjectTitleWithoutPath;
{  Result := '';
  ConcatToString(Result, ' ', AuthorFirstName);
  ConcatToString(Result, ' ', AuthorLastName);
  if (Result <> '') and (Title <> '') then ConcatToString(Result, '', ' - ');
  ConcatToString(Result, '', Title);  }
end;

function TProjectDescriptor.GetProjectTitleWithoutPath: string;
begin
  Result := AuthorFirstName;
  ConcatToString(Result, ' ', AuthorLastName);
  ConcatToString(Result, ' - ', Title);
end;

procedure TProjectDescriptor.SaveTo(t: TStringList);
var prop: TProperties;
begin
  // descriptor
  prop.Init('|');
  if Length(AuthorFirstName) > 0 then prop.Add('AuthorFirstName', AuthorFirstName);
  if Length(AuthorLastName) > 0 then prop.Add('AuthorLastName', AuthorLastName);
  prop.Add('Title', Title);

  prop.Add('FolderHeader_DigitCountForPrefixNumber', FolderHeader_DigitCountForPrefixNumber);
  prop.Add('FolderHeader_DigitCountForSectionNumber',FolderHeader_DigitCountForSectionNumber);
  prop.Add('FolderHeader_IncludeBookInfo', FolderHeader_IncludeBookInfo);
  prop.Add('FolderHeader_BookLetter', FolderHeader_BookLetter);
  prop.Add('FolderHeader_BookNumber', FolderHeader_BookNumber);
  prop.Add('FolderHeader_BookTitle', FolderHeader_BookTitle);
  prop.Add('FolderHeader_IncludeSectionInfo', FolderHeader_IncludeSectionInfo);
  prop.Add('FolderHeader_SectionPrefix', FolderHeader_SectionPrefix);
  prop.Add('FolderHeader_SectionNumber', FolderHeader_SectionNumber);

  t.Add('[DESCRIPTION]');
  t.Add(prop.PackedProperty);

  // metadata
  prop.Init('|');
  prop.Add('Artist', Artist);
  prop.Add('Album', Album);
  prop.Add('Comment', Comment);
  prop.Add('Genre', Genre);
  t.Add('[USER METADATA]');
  t.Add(prop.PackedProperty);

  // reading
  prop.Init('|');
  prop.Add('CurrentPageNumber', CurrentPageNumber);
  t.Add('[READING]');
  t.Add(prop.PackedProperty);
end;

procedure TProjectDescriptor.LoadFrom(t: TStringList);
var prop: TProperties;
  k: integer;
begin
  InitDefault;

  // descriptor
  k := t.IndexOf('[DESCRIPTION]');
  if (k > -1) and (k < t.Count-1) then begin
    prop.Split(t.Strings[k+1], '|');
    prop.StringValueOf('AuthorFirstName', AuthorFirstName, AuthorFirstName);
    prop.StringValueOf('AuthorLastName', AuthorLastName, AuthorLastName);
    prop.StringValueOf('Title', Title, Title);

    prop.IntegerValueOf('FolderHeader_DigitCountForPrefixNumber', FolderHeader_DigitCountForPrefixNumber, FolderHeader_DigitCountForPrefixNumber);
    prop.IntegerValueOf('FolderHeader_DigitCountForSectionNumber', FolderHeader_DigitCountForSectionNumber, FolderHeader_DigitCountForSectionNumber);
    prop.BooleanValueOf('FolderHeader_IncludeBookInfo', FolderHeader_IncludeBookInfo, FolderHeader_IncludeBookInfo);
    prop.StringValueOf('FolderHeader_BookLetter', FolderHeader_BookLetter, FolderHeader_BookLetter);
    prop.IntegerValueOf('FolderHeader_BookNumber', FolderHeader_BookNumber, FolderHeader_BookNumber);
    prop.StringValueOf('FolderHeader_BookTitle', FolderHeader_BookTitle, FolderHeader_BookTitle);
    prop.BooleanValueOf('FolderHeader_IncludeSectionInfo', FolderHeader_IncludeSectionInfo, FolderHeader_IncludeSectionInfo);
    prop.StringValueOf('FolderHeader_SectionPrefix', FolderHeader_SectionPrefix, FolderHeader_SectionPrefix);
    prop.IntegerValueOf('FolderHeader_SectionNumber', FolderHeader_SectionNumber, FolderHeader_SectionNumber);
  end;

  // metadata
  k := t.IndexOf('[USER METADATA]');
  if (k > -1) and (k < t.Count-1) then begin
    prop.Split(t.Strings[k+1], '|');
    prop.StringValueOf('Artist', Artist, Artist);
    prop.StringValueOf('Album', Album, Album);
    prop.StringValueOf('Comment', Comment, Comment);
    prop.StringValueOf('Genre', Genre, Genre);
  end;

  // reading
  k := t.IndexOf('[READING]');
  if (k > -1) and (k < t.Count-1) then begin
    prop.Split(t.Strings[k+1], '|');
    prop.IntegerValueOf('CurrentPageNumber', CurrentPageNumber, CurrentPageNumber);
  end;
end;

{ TProject }

procedure TProject.CheckSaveFolders;
begin
  FTempFolder := '';

  CreateDefaultProjectFolder;

  FTempFolder := IncludeTrailingPathDelimiter(ProjectsFolder+FOLDER_FOR_TEMP);
  TempFolderExists;
end;

function TProject.CreateProjectZipFolder: boolean;
var zipFolder: string;
begin
  Result := False;
  zipFolder := GetProjectZIPFolder;
  if zipFolder = '' then exit;
  Result := RepertoireExistant(zipFolder);
  if not Result then begin
    Result := CreerRepertoire(zipFolder);
    if not Result then
      ShowMess(SFailedToCreateProjectZipFolder, SClose, mtError);
  end;
end;

procedure TProject.OnLanguageChange;
begin
  FormMain.OnLanguageChange;
end;

function TProject.GetProjectFolder: string;
begin
  if not IsReady then
    Result := ''
  else
    Result := IncludeTrailingPathDelimiter(ExtractFilePath(Filename));
end;

function TProject.GetProjectOutputFolder: string;
begin
  if not IsReady then
    Result := ''
  else
    Result := IncludeTrailingPathDelimiter(ConcatPaths([GetProjectFolder, PROJECT_OUTPUT_FOLDER_MP3]));
end;

function TProject.GetProjectZIPFolder: string;
begin
  if not IsReady then
    Result := ''
  else
    Result := IncludeTrailingPathDelimiter(ConcatPaths([GetProjectFolder, PROJECT_OUTPUT_FOLDER_ZIP]));
end;

function TProject.GetProjectsFolder: string;
begin
  Result := ProgramOptions.GetCurrentProjectFolder;
end;

function TProject.TempFolderExists: boolean;
begin
  Result := False;
  if not RepertoireExistant(ProjectsFolder) then exit;

  Result := RepertoireExistant(FTempFolder);
  if not Result then
    Result := CreerRepertoire(FTempFolder);
end;

function TProject.OutputFolderExists: boolean;
var f: string;
begin
  Result := False;
  if not RepertoireExistant(ProjectsFolder) then exit;

  f := GetProjectOutputFolder;
  Result := RepertoireExistant(f);
  if not Result then
    Result := CreerRepertoire(f);
end;

constructor TProject.Create;
begin
  Inherited Create(PROJECT_FILE_EXT);
  SetFormCaption(FormMain, 'Give Your Voice');
  AddFilterToDialogs(SProject, '*'+PROJECT_FILE_EXT);
end;

function TProject.DoNew: boolean;
var F: TFormNewProject;
  projFolder, s: string;
begin
  Result := False;
  if not RepertoireExistant(ProjectsFolder) then exit;

  F := TFormNewProject.Create(NIL);
  try
    if F.ShowModal <> mrOk then exit;
    F.InitDescriptor(@Descriptor);

    // create the project folder
    projFolder := ConcatPaths([ProjectsFolder,
                               Descriptor.GetProjectSubFolderName]);
    if not RepertoireExistant(projFolder) then
      if not CreerRepertoire(projFolder) then begin
        ShowMess(SFailedToCreateProjectFolder+LineEnding+projFolder, SClose, mtError);
        Result := False;
        exit;
      end;

    s := ConcatPaths([projFolder, Descriptor.GetProjectSubFolderName+PROJECT_FILE_EXT]);
    try
      SaveAs(s);
      Result := True;
    except
      ShowMess(SFailedToSaveNewProject, SClose, mtError);
      exit;
    end;

    // create the output folder for mixed files
    s := IncludeTrailingPathDelimiter(ConcatPaths([projFolder, PROJECT_OUTPUT_FOLDER_MP3]));
    if not RepertoireExistant(s) then
      if not CreerRepertoire(s) then begin
        ShowMess(SFailedToCreateMP3ProjectFolder+LineEnding+s, SClose, mtError);
        Result := False;
        exit;
      end;
  finally
    F.Free;
  end;
end;

procedure TProject.DoSave(const aFilename: string);
var t: TStringList;
begin
  t := TStringList.Create;
  try
    WriteApplicationHeader(t);

    Descriptor.SaveTo(t);

    t.Add('[OPTIONS]');
    t.Add(Options.SaveToString);

    t.SaveToFile(aFilename);
  finally
    t.Free;
  end;
end;

function TProject.DoLoad(const aFilename: string): boolean;
var t: TStringList;
  k: integer;
  projFolder: String;
begin
  Log.AddEmptyLine;
  Log.Info('gyv: try to load project "'+aFilename+'"');
  Result := False;
  t := TStringList.Create;
  try
    try
      t.LoadFromFile(aFilename);
    except
      Log.Error('gyv: fail to load project', 1);
      exit;
    end;

    // check if the file is an application project
    if not ApplicationHeaderIsValid(t) then begin
      ShowMess(SProjectIsNotForThisApplication, SClose, mtError);
      Log.Error('gyv: project header is not valid', 1);
      exit;
    end;

    // load project's descriptor
    Descriptor.LoadFrom(t);

    // load project's options
    k := t.IndexOf('[OPTIONS]');
    if (k > -1) and (k < t.Count-1) then
      Options.LoadFromString(t.Strings[k+1]);

    // check the existence of project's MP3 folder
    projFolder := IncludeTrailingPathDelimiter(ExtractFilePath(aFilename));
    projFolder := IncludeTrailingPathDelimiter(ConcatPaths([projFolder, PROJECT_OUTPUT_FOLDER_MP3]));
    if not RepertoireExistant(projFolder) then
      if not CreerRepertoire(projFolder) then begin
        ShowMess(SFailedToCreateMP3ProjectFolder+LineEnding+projFolder, SClose, mtError);
        Log.Error('gyv: failed to create project MP3 folder', 1);
      end;
    Result := True;
    Log.Info('gyv: project loaded');
    Log.AddEmptyLine;
  finally
    t.Free;
  end;
end;

procedure TProject.DoClose;
begin
  Descriptor.InitDefault;
  Options.InitDefault;
  MainUndoRedoManager.Clear;
end;

procedure TProject.OnModifiedChange(aState: boolean);
begin
  aState := aState;
end;

procedure TProject.OnProjectReadyChange;
begin
  if IsReady then begin
    ProgramOptions.LastOpenedProjectFilename := Filename;
    ProgramOptions.Save;

    FRecordFilenameSuffix := 0;
  end;

  FormMain.ProcessProjectReadyChange;
end;

end.

