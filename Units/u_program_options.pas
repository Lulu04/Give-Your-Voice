unit u_program_options;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  Classes, SysUtils, LCLTranslator,
  u_utils, u_common, project_util, i18_utils;

type
  { TProgramOptions }

  TProgramOptions = record
  private
    procedure InitDefault;
    function SaveToString: string;
    procedure LoadFromString(const s: string);
    function GetLanguage: TLanguageIdentifier;
    procedure SetLanguage(AValue: TLanguageIdentifier);
  public
    PlaybackDeviceIndex,       // index of the audio playback device to use
    CaptureDeviceIndex: integer; // index of the audio capture device to use

    AudioSilenceAttenuationTimeMS: integer; // Ms to lower and restore volume when applying silence
    InsertedSilenceDurationMS: integer; // Ms of silence inserted to audio
    RemoveNoiseWhenRecording: boolean; // True to remove noise when recording

    LastOpenedProjectFilename: string; // path+filename of the last opened project
    MainUndoMaxCount: integer; // max undo kept for main form. Default is 100

    ListeningImprovedActivated: boolean;
    ListeningImprovedUseCompressor: boolean;
    ListeningImprovedBassGainValue: single;
    ListeningImprovedAmplifyGainValue: single;

    ViewFilesFontHeight: integer;
    CBPlatformURLIndex: integer; // combobox itemindex for platform internet links
    MP3ToZipIncludeFileForLitteratureAudio: boolean;

    MicLevelUseDecibel,
    MicLevelShowCaption: boolean;

    MixedFileTargetPlatform: TProjectTargetPlatform;
    MixedFileUseMP3Gain: boolean;
    MixedFileMP3GaindBx10: integer; // Trunc(desired gain for MP3Gain * 10)

    MusicFolder,           // the last folder opened by user to load a music
    SoundFolder: string;   // the last folder opened by user to load a sound

  public  // Project Manager
    UserWorkingDirectoryList: TRecentList; // list of the working directories used by user
    UserWorkingDirectoryIndex: integer;    // the index of the last used working directory
    function GetCurrentProjectFolder: string; // with trailing path delimiter

  public  // Date: we keep only the integer part of TDateTime the number of days since December 30, 1899
    DateOriginCheckForUpdate,
    DateOriginRememberUserToDonate: integer;
    function ItsTimeToLookForAppUpdate: boolean;
    function ItsTimeToRememberUserToDonate: boolean;

    procedure Save;
    procedure Load;
    property Language: TLanguageIdentifier read GetLanguage write SetLanguage;
  end;

var
  ProgramOptions: TProgramOptions;

implementation
uses PropertyUtils, u_project, u_crossplatform, utilitaire_fichier, u_logfile,
  Math, DateUtils;

{ TProgramOptions }

procedure TProgramOptions.InitDefault;
begin
  PlaybackDeviceIndex := 0;
  CaptureDeviceIndex := 0;

  AudioSilenceAttenuationTimeMS := 50;
  InsertedSilenceDurationMS := 100;
  RemoveNoiseWhenRecording := True;
  LastOpenedProjectFilename := '';
  MainUndoMaxCount := 100;

  ListeningImprovedActivated := True;
  ListeningImprovedUseCompressor := True;
  ListeningImprovedBassGainValue := 2.0;
  ListeningImprovedAmplifyGainValue := 2.0;

  ViewFilesFontHeight := 20;
  CBPlatformURLIndex := 1;
  MP3ToZipIncludeFileForLitteratureAudio := False;

  MicLevelUseDecibel := True;
  MicLevelShowCaption := True;

  MixedFileTargetPlatform := ptpNone;
  MixedFileUseMP3Gain := True;
  MixedFileMP3GaindBx10 := 890; // Trunc(desired gain for MP3Gain * 10)

  MusicFolder := '';
  SoundFolder := '';

  UserWorkingDirectoryList.Clear;
  UserWorkingDirectoryList.AddItem(GetAppDefaultProjectFolder);
  UserWorkingDirectoryIndex := 0;

  DateOriginCheckForUpdate := Trunc(Now);
  DateOriginRememberUserToDonate := Trunc(Now);
end;

function TProgramOptions.SaveToString: string;
var prop: TProperties;
begin
  prop.Init('|');
  prop.Add('PlaybackDeviceIndex', PlaybackDeviceIndex);
  prop.Add('CaptureDeviceIndex', CaptureDeviceIndex);
  prop.Add('Language', AppLang.UsedLangID);
  prop.Add('AudioSilenceAttenuationTime', AudioSilenceAttenuationTimeMS);
  prop.Add('InsertedSilenceDurationMS', InsertedSilenceDurationMS);
  prop.Add('RemoveNoiseWhenRecording', RemoveNoiseWhenRecording);
  prop.Add('LastOpenedProjectFilename', LastOpenedProjectFilename);
  prop.Add('MainUndoMaxCount', MainUndoMaxCount);

  prop.Add('ListeningImprovedActivated', ListeningImprovedActivated);
  prop.Add('ListeningImprovedUseCompressor', ListeningImprovedUseCompressor);
  prop.Add('ListeningImprovedBassGainValue', ListeningImprovedBassGainValue);
  prop.Add('ListeningImprovedAmplifyGainValue', ListeningImprovedAmplifyGainValue);

  prop.Add('ViewFilesFontHeight', ViewFilesFontHeight);
  prop.Add('CBPlatformURLIndex', CBPlatformURLIndex);
  prop.Add('MP3ToZipIncludeFileForLitteratureAudio', MP3ToZipIncludeFileForLitteratureAudio);

  prop.Add('MusicFolder', MusicFolder);
  prop.Add('SoundFolder', SoundFolder);

  prop.Add('MicLevelUseDecibel', MicLevelUseDecibel);
  prop.Add('MicLevelShowCaption', MicLevelShowCaption);

  prop.Add('MixedFileTargetPlatform', Ord(MixedFileTargetPlatform));
  prop.Add('MixedFileUseMP3Gain', MixedFileUseMP3Gain);
  prop.Add('MixedFileMP3GaindBx10', MixedFileMP3GaindBx10);

  prop.Add('UserWorkingDirectoryIndex', UserWorkingDirectoryIndex);

  prop.Add('DateOriginCheckForUpdate', DateOriginCheckForUpdate);
  prop.Add('DateOriginRememberUserToDonate', DateOriginRememberUserToDonate);

  Result := prop.PackedProperty;
end;

procedure TProgramOptions.LoadFromString(const s: string);
var prop: TProperties;
  lang: string;
  vi: integer;
begin
  InitDefault;
  prop.Split(s, '|');
  prop.IntegerValueOf('PlaybackDeviceIndex', PlaybackDeviceIndex, PlaybackDeviceIndex);
  prop.IntegerValueOf('CaptureDeviceIndex', CaptureDeviceIndex, CaptureDeviceIndex);

  lang := 'en';
  prop.StringValueOf('Language', lang, lang);
  SetLanguage(lang);

  prop.IntegerValueOf('AudioSilenceAttenuationTimeMS', AudioSilenceAttenuationTimeMS, AudioSilenceAttenuationTimeMS);
  prop.IntegerValueOf('InsertedSilenceDurationMS', InsertedSilenceDurationMS, InsertedSilenceDurationMS);
  prop.BooleanValueOf('RemoveNoiseWhenRecording', RemoveNoiseWhenRecording, RemoveNoiseWhenRecording);
  prop.StringValueOf('LastOpenedProjectFilename', LastOpenedProjectFilename, LastOpenedProjectFilename);
  prop.IntegerValueOf('MainUndoMaxCount', MainUndoMaxCount, MainUndoMaxCount);

  prop.BooleanValueOf('ListeningImprovedActivated', ListeningImprovedActivated, ListeningImprovedActivated);
  prop.BooleanValueOf('ListeningImprovedUseCompressor', ListeningImprovedUseCompressor, ListeningImprovedUseCompressor);
  prop.SingleValueOf('ListeningImprovedBassGainValue', ListeningImprovedBassGainValue, ListeningImprovedBassGainValue);
  prop.SingleValueOf('ListeningImprovedAmplifyGainValue', ListeningImprovedAmplifyGainValue, ListeningImprovedAmplifyGainValue);

  prop.IntegerValueOf('ViewFilesFontHeight', ViewFilesFontHeight, ViewFilesFontHeight);
  prop.IntegerValueOf('CBPlatformURLIndex', CBPlatformURLIndex, CBPlatformURLIndex);
  prop.BooleanValueOf('MP3ToZipIncludeFileForLitteratureAudio', MP3ToZipIncludeFileForLitteratureAudio, MP3ToZipIncludeFileForLitteratureAudio);

  prop.StringValueOf('MusicFolder', MusicFolder, MusicFolder);
  prop.StringValueOf('SoundFolder', SoundFolder, SoundFolder);

  prop.BooleanValueOf('MicLevelUseDecibel', MicLevelUseDecibel, MicLevelUseDecibel);
  prop.BooleanValueOf('MicLevelShowCaption', MicLevelShowCaption, MicLevelShowCaption);

  vi := Ord(MixedFileTargetPlatform);
  prop.IntegerValueOf('MixedFileTargetPlatform', vi, vi);
  MixedFileTargetPlatform := TProjectTargetPlatform(vi);
  prop.BooleanValueOf('MixedFileUseMP3Gain', MixedFileUseMP3Gain, MixedFileUseMP3Gain);
  prop.IntegerValueOf('MixedFileMP3GaindBx10', MixedFileMP3GaindBx10, MixedFileMP3GaindBx10);

  prop.IntegerValueOf('UserWorkingDirectoryIndex', UserWorkingDirectoryIndex, UserWorkingDirectoryIndex);

  prop.IntegerValueOf('DateOriginCheckForUpdate', DateOriginCheckForUpdate, DateOriginCheckForUpdate);
  prop.IntegerValueOf('DateOriginRememberUserToDonate', DateOriginRememberUserToDonate, DateOriginRememberUserToDonate);
end;

function TProgramOptions.GetLanguage: TLanguageIdentifier;
begin
  Result := AppLang.UsedLangID;
end;

procedure TProgramOptions.SetLanguage(AValue: TLanguageIdentifier);
begin
  AppLang.UseLanguage(AValue, GetAppLanguagesFolder);
  if Project <> NIL then
    Project.OnLanguageChange;
end;

function TProgramOptions.GetCurrentProjectFolder: string;
  function CheckDefault: string;
  begin
    Result := GetAppDefaultProjectFolder;
    // check if the default folder exists
    if not RepertoireExistant(Result) then
      if not CreerRepertoire(Result) then
        Result := '';
  end;
begin
  Result := '';
  if UserWorkingDirectoryIndex = 0 then begin
    Result := CheckDefault;
  end else begin
    if not InRange(UserWorkingDirectoryIndex, 0, High(UserWorkingDirectoryList.Items)) then
      UserWorkingDirectoryIndex := 0;

    Result := IncludeTrailingPathDelimiter(UserWorkingDirectoryList.Items[UserWorkingDirectoryIndex]);
    if not RepertoireExistant(Result) then
      Result := CheckDefault;
  end;
end;

function TProgramOptions.ItsTimeToLookForAppUpdate: boolean;
begin
  Result := DaysBetween(Now, ComposeDateTime(DateOriginCheckForUpdate, 0)) >= NUMBER_OF_DAYS_TO_LOOK_FOR_UPDATE;
  if Result then begin
    DateOriginCheckForUpdate := Trunc(Now);
    Save;
  end;
end;

function TProgramOptions.ItsTimeToRememberUserToDonate: boolean;
begin
  Result := DaysBetween(Now, ComposeDateTime(DateOriginRememberUserToDonate, 0)) >= NUMBER_OF_DAYS_TO_REMEMBER_USER_TO_DONATE;
  if Result then begin
    DateOriginRememberUserToDonate := Trunc(Now);
    Save;
  end;
end;

procedure TProgramOptions.Save;
var t: TStringList;
  f: string;
begin
  t := TStringList.Create;
  t.Add('['+APP_NAME+' PROGRAM OPTIONS]');
  t.Add(SaveToString);

  t.Add('[RECENT USER FOLDERS]');
  t.Add(UserWorkingDirectoryList.SaveToString('|'));

  try
    f := CreateAppFolder(FOLDER_FOR_PROJECT)+PROGRAM_OPTIONS_FILENAME;
    t.SaveToFile(f);
  finally
    t.Free;
  end;
end;

procedure TProgramOptions.Load;
var t: TStringList;
  f: string;
  k: integer;
begin
  InitDefault;
  t := TStringList.Create;
  try
    f := CreateAppFolder(FOLDER_FOR_PROJECT)+PROGRAM_OPTIONS_FILENAME;
    if FileExists(f) then begin
      FFirstTimeProgramIsRunning := False;
      t.LoadFromFile(f);
      k := t.IndexOf('['+APP_NAME+' PROGRAM OPTIONS]');
      if (k > -1) and (k < t.Count-1) then
        LoadFromString(t.Strings[k+1]);

      k := t.IndexOf('[RECENT USER FOLDERS]');
      if (k > -1) and (k < t.Count-1) then
        UserWorkingDirectoryList.LoadFromString(t.Strings[k+1], '|');

      Log.Info('gyv: program options loaded');
    end else begin
      // option file not found -> this is the first time the program is run
      FFirstTimeProgramIsRunning := True;
      Log.Warning('gyv: program options not found');
      Log.Warning(f, 1);
    end;
  finally
    t.Free;
  end;
end;

end.

