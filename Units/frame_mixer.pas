unit frame_mixer;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, Buttons,
  ComCtrls, Menus, Dialogs, BGRABitmap, BGRABitmapTypes, u_common, ALSound,
  Types, u_utils, frame_trackbar, {u_mixer_undoredo,} LCL_utils, u_audio_utils;

const
  SLICE_TIME_MS = 10;
  SLICE_TIME = SLICE_TIME_MS*0.001;

  ZOOM_COEFF = 0.3;
  MAX_PIXELPERPOINT_VALUE = 1.0; // 30.0;
  MIN_PIXELPERPOINT_VALUE = 0.00005;

  HALF_WIDTH_POINT_CURVE = 4;
  MIN_TIME_MS_BETWEEN_POINT_CURVE = 100;
  HALF_WIDTH_TRIM_HANDLE = 4;
  MOUSE_MOVE_THRESHOLD = 4;

  VOICE_MAX_GAIN_VALUE = 1.0;
  MUSIC_MAX_GAIN_VALUE = 0.8;
  SOUND_MAX_GAIN_VALUE = 0.8;


type

  TMixerPointCurve = TPointF; // x = [0..1]*duration  y = gain [0..1]
  PMixerPointCurve = ^TMixerPointCurve;
  TArrayOfMixerPointCurve = array of TMixerPointCurve;

type
  { TArrayOfMixerPointCurveHelper }

  TArrayOfMixerPointCurveHelper = type helper for TArrayOfMixerPointCurve
  //private
    //function GetGain(index: integer): single;
  public
    procedure Initdefault;
    procedure AddPoint(aPX, aY: single);
    //property Gain[index:integer]: single read GetGain;
  end;


 { TMixerSoundObject }

 TMixerSoundObject = record
   ApplyFadeAtBeginAndEnd: boolean;
   MaxGainValue: single;
   g: TALSCustomBoundedFParam;   // gain qu'on fait varier à chaque sample pour
                        // effectuer les fadein, fadeout et l'enveloppe de gain
   SamplesToDo: int64;
   AccuSamples: int64;
   ProcessStep: integer;
   procedure InitGFromPos(aTimePosMS: int64);
 public
   Sound: TALSSound;
   Filename,
   Caption: string;
   Levels: TSingleArray;
   RealDuration: double;
   UsedIndexes: TIndexInterval; // the interval of audio levels to use
   ViewIndexes: TIndexInterval; // the interval of audio levels to draw on view
   TimePosMS: int64; // the time position after trimming
   ViewRect: TRect;
   Muted: boolean;

   ID: integer; // use only to recognize the object when sorting

   function SaveToString: string;
   procedure LoadFromString(const s: string);

   procedure LoadAudio;
   function DurationMS: int64; // the duration after trimming
   procedure ComputeViewIndexes(const aViewBeginTime, aViewEndTime: single);
   function EndTimePosMS: int64;

   function HaveWorkingTime(aCurrentTimeMS: int64; out aNextWorkingTimeMS: int64): boolean;
 public
   // trim the audio from the beginning
   function LeftTrimTo(aTimePos: single): boolean;
   // trim the audio from the end
   function RightTrimTo(aTimePos: single): boolean;
 public
   CurveGain: TArrayOfMixerPointCurve;
   WorkingindexCurveGain: integer;
   function MoveGainPoint(aIndex: integer; aX, aY: single): boolean;
   function AddGainPoint(aX, aY: single): integer;
   procedure DeleteGainPoint(aIndex: integer);
   function GetGainValue(aIndex: integer): single;
   // gives the time position of the point, relative to the beginning of the sound
   function GetGainTimeMS(aIndex: integer): int64;
   // gives the absolute time position on the chronology
   function GetAbsGainTimeMS(aIndex: integer): int64;
   // samples count is relative to UsedIndexes.Count
   function GainPointToSampleIndex(aPointIndex: integer): integer;
   // Relative to usedIndexes. -1 if aTimePos is not in this interval
   function AbsTimeMSToSampleIndex(aTimePosMS: int64): integer;
 end;
 PMixerSoundObject = ^TMixerSoundObject;
 TArrayOfMixerSoundObject = array of TMixerSoundObject;


 TMixerMouseOver = (mmoNothing,
                    mmoLeftTrimHandle,
                    mmoRightTrimHandle,
                    mmoBodyObject,
                    mmoGainPoint,
                    mmoGainCurve);
 { TMixerView }

 TMixerView = record
   PB: TPaintBox;
   TempImage,
   FGradientBackground: TBGRABitmap;
   PopupEmpty,
   PopupObject: TPopupMenu;
   Objects: TArrayOfMixerSoundObject;
   VisibleObjectsIndexes: TIndexInterval;

   SelectedObject: PMixerSoundObject;

   ObjectUnderMouse: PMixerSoundObject;
   MouseOverState: TMixerMouseOver;

   IndexGainPointUnderMouse: integer;

   function SaveToString: string;
   procedure LoadFromString(const s: string; ItsVoice: boolean);

   procedure Init(aPB: TPaintBox);
   function AddObject(const aFileName: string; aTimePosMS: int64): PMixerSoundObject;
   procedure DeleteSelectedObject;
   function IndexOf(aObject: PMixerSoundObject): integer;
   function IndexOf(aID: integer): integer;
   function IndexOf(aFilename: string): integer;
   procedure ShiftObjectIfOverlap;
   // an object can not overlaps another on the same view
   procedure MoveAudioObject(aObject: PMixerSoundObject; aDeltaTime: single);
 //    procedure Sort;
   function GetObjectAtTimePos(aTimePos: single): PMixerSoundObject;
   function GetMouseOverState: TMixerMouseOver;
   function TotalDurationMS: int64;

   procedure GetPointCurveLevel(aObject: PMixerSoundObject; IndexPoint: integer; out aX, aY: integer);
   procedure GetPointCurveGain(aObject: PMixerSoundObject; IndexPoint: integer; out aX, aY: integer);
   function GetGainPointUnderMouse(aX, aY: integer; const aColor: TBGRAPixel): PMixerPointCurve;
   function PointIsOverGainCurve(aX, aY: integer; const aColor: TBGRAPixel): boolean;

 public
   IDValue: integer;
   function NextIDValue: integer;
 end;
 PMixerView = ^TMixerView;


  { TFrameMixer }

  TFrameMixer = class(TFrame)
    Label3: TLabel;
    MIInvertMuteSound: TMenuItem;
    MIInvertMuteMusic: TMenuItem;
    MISetVolumeSound: TMenuItem;
    MISetVolumeMusic: TMenuItem;
    MIAddMusic: TMenuItem;
    MIAddSound: TMenuItem;
    MIDeleteMusic: TMenuItem;
    MIDeleteSound: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    PanelVolume: TPanel;
    PBSound: TPaintBox;
    PBMusic: TPaintBox;
    PBVoice: TPaintBox;
    PBTime: TPaintBox;
    PopupMusicEmpty: TPopupMenu;
    PopupSoundEmpty: TPopupMenu;
    PopupMusicObject: TPopupMenu;
    PopupSoundObject: TPopupMenu;
    ScrollBar1: TScrollBar;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    BHideVolumeCursor: TSpeedButton;
    BMute: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure BHideVolumeCursorClick(Sender: TObject);
    procedure BMuteClick(Sender: TObject);
    procedure MIAddMusicClick(Sender: TObject);
    procedure PBMusicPaint(Sender: TObject);
    procedure PBVoiceMouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, {%H-}Y: Integer);
    procedure PBVoiceMouseLeave(Sender: TObject);
    procedure PBVoiceMouseMove(Sender: TObject; Shift: TShiftState; X, {%H-}Y: Integer);
    procedure PBVoiceMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PBVoiceMouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PBVoicePaint(Sender: TObject);
    procedure PBVoiceResize(Sender: TObject);
    procedure PBTimePaint(Sender: TObject);
    procedure PopupMusicObjectPopup(Sender: TObject);
    procedure PopupSoundObjectPopup(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; {%H-}ScrollCode: TScrollCode;
      var {%H-}ScrollPos: Integer);
  private
    FTextStyle: TTextStyle;

    FVoiceView,
    FMusicView,
    FSoundView: TMixerView;

    FPixelPerPoint: single;
    FViewBeginTime: single;
    FScrollBarLocked: boolean;
    FRedrawLocked: boolean;
    procedure DoReset;
    function GetViewEndTime: single;
    procedure RecomputeFirstLastAudioIndexView(p: PMixerView);
    procedure SetViewBeginTime(AValue: single);
    function ComputePixelPerPointToFit(aDuration: single): single;
    function TimeToAbscissa(aTime: single): integer;
    function AbscissaToTime(aX: integer): single;
    function DoAbscissaToTime(aX: integer; aPixelPerPoint: single): single;
    procedure DoZoomOn(aBeginTime, aEndTime: single);
    procedure UpdateScrollBar;
  private
    FColorGainPoint,
    FColorGainCurve: TBGRAPixel;
    procedure RedrawGradientImage(p: PMixerView);
    function PaintBoxToMixerView(aSender: TObject): PMixerView;
    procedure RedrawTempImage(p: PMixerView);
  private
    FMouseHandledByUser: boolean;
    FMouseButtonPressed: TMouseButtonPressed;
    FTimeClickOrigin,
    FTimeSelectionRight,
    FTimeSelectionLeft: single;
    FOnSelectionChange: TNotifyEvent;
    procedure DoSelectionChange;
    procedure LoopSelection(p: PMixerView);
    procedure LoopMoveView;
    procedure LoopMoveAudioBlock(p: PMixerView);
    procedure LoopMoveLeftTrimHandle(p: PMixerView);
    procedure LoopMoveRightTrimHandle(p: PMixerView);
    procedure LoopMoveGainPoint(p: PMixerView);
    procedure AddGainPoint(p: PMixerView);
  private
    FLegendBeginTimePosition,
    FLegendTimeInterval: double;
    procedure ComputeLegendTimeInterval;
    procedure ComputeLegendTimeBeginPosition;
    function GetTotalDuration: single;
    procedure SetPixelPerPoint(AValue: single);
  private
    FOutputFile: string;
    FMixingTimePosMS: int64;
    FMixingSoundIndex: integer;
    FCanceled,
    FDontProcessBuffer: boolean;
    function CreateAllSoundsOnContext(aContext: TALSPlaybackContext;
                                      aTimePosMS: int64): boolean;
    procedure DeleteAllSoundsOnContext(aContext: TALSPlaybackContext);
    procedure ProcessMixOnProgressEvent(Sender: TALSLoopbackContext;
                                aTimePos: double;
                                const {%H-}aFrameBuffer: TALSLoopbackFrameBuffer;
                                var {%H-}SaveBufferToFile, Cancel: boolean);
  private
    procedure AvoidOverlapping(p: PMixerView);
  private
    FPreviewIsRunning: boolean;
    FCursorPlayTimeMS: int64;
    function SessionNameForLoadSave: string;
    function SessionFilenameForLoadSave: string;
    procedure LoadSession;
  private
    FrameTrackBar1: TFrameTrackBar;
    FTargetObjectForCursorVolume: PMixerSoundObject;
    procedure ShowPanelVolume(aY: integer);
    procedure ProcessVolumeCursorChange(Sender: TObject);
  private
    ToggleSpeedButtonManager1: TToggleSpeedButtonManager;
  public
    FGainAnalyzer: TComputeGain89dB;
    procedure AdjustFont;
    procedure ProcessSoundOnCustomDSPEvent(Sender: TALSSound;
                                           const aBuffer: TALSPlaybackBuffer;
                                           aUserData: Pointer);
    procedure SaveSession;
  public
    constructor Create( TheOwner: TComponent ); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure ClearView;

    procedure Redraw;
    procedure ReconstructTempImageAndRedraw;

    procedure ClearPBHint;
    procedure HidePanelVolume;

    procedure InitFromViewProjectFiles(const Filenames: TStringArray);

    procedure AdjustTrackHeight;

    function IsEmpty: boolean;

    procedure SetCursorToBegin;
    procedure SetCursorToEnd;
    procedure SetCursorAt(aTimepos: single);
    procedure ViewAll;
    procedure ZoomOnSelection;
    function HaveSelection: boolean;
    function HaveMusic: boolean;
    function HaveSound: boolean;

    procedure ProcessKeyDown(Key: Word; Shift: TShiftState);

    procedure StartPreview;
    procedure StopPreview;
    function PreviewIsRunning: boolean;
    procedure SetPreviewCompressor(aOn: boolean);
    procedure SetPreviewBassBoost(aOn: boolean);
    procedure SetPreviewBassBoostGain(aValue: single);
    procedure SetPreviewAmplifyGain(aValue: single);

    function MixToFile: boolean;
    procedure CancelMix;
    function Canceled: boolean;

    property PixelPerPoint: single read FPixelPerPoint write SetPixelPerPoint;
    property ViewBeginTime: single read FViewBeginTime write SetViewBeginTime;
    property ViewEndTime: single read GetViewEndTime;
    property TotalDuration: single read GetTotalDuration;
    property OutputFile: string read FOutputFile write FOutputFile;
  end;

implementation

uses form_main, u_userdialogs, form_mixer, u_project,
  u_program_options, u_resource_string, Math, LCLType,
  PropertyUtils, dateutils, als_dsp_utils, u_logfile;

{$R *.lfm}

const
  DECIBEL_MAX_ABSOLUTE_VALUE = -ALS_DECIBEL_MIN_VALUE;

{ TArrayOfMixerPointCurveHelper }

procedure TArrayOfMixerPointCurveHelper.Initdefault;
begin
  SetLength(Self, 2);
  Self[0].x := 0.0; Self[0].y := 1.0;
  Self[1].x := 1.0; Self[1].y := 1.0;
end;

procedure TArrayOfMixerPointCurveHelper.AddPoint(aPX, aY: single);
var p: TMixerPointCurve;
  i: Integer;
begin
  p.x := aPX;
  p.y := aY;

  i := 0;
  if Length(Self) > 0 then
    for i:=0 to High(Self) do
      if aPX < Self[i].x then
        break;

  Insert(p, Self, i);
end;

{ TMixerView }

function TMixerView.SaveToString: string;
var prop: TProperties;
  i: Integer;
begin
  prop.Init('#');

  prop.Add('ObjectCount', integer(Length(Objects)));
  for i:=0 to High(Objects) do begin
    prop.Add('File'+i.ToString, Objects[i].Filename); // keep the full path !!
                                                      // for music files located
                                                      // in another folder than the project
    prop.Add('Data'+i.ToString, Objects[i].SaveToString);
  end;

  Result := prop.PackedProperty;
end;

procedure TMixerView.LoadFromString(const s: string; ItsVoice: boolean);
var prop: TProperties;
  i, j, c:integer;
  temp, temp1: string;
  folder: string;
  atimePosMS: int64;
begin
  prop.Split(s, '#');
  c := 0;
  temp := '';
  temp1 := '';
  if not prop.IntegerValueOf('ObjectCount', c, 0) then exit;

  atimePosMS := 0;

  if ItsVoice then begin
    // on ne créé pas d'object car ils ont été créés à partir de la vue projectfiles
    // et on n'applique les données de session uniquement sur les fichiers existants
    folder := IncludeTrailingPathDelimiter(FormMain.FrameViewProjectFiles1.SelectedFilename);
    for i:=0 to c-1 do begin
      if prop.StringValueOf('File'+i.ToString, temp, '') then
        if prop.StringValueOf('Data'+i.ToString, temp1, '') then begin
          temp := ExtractFilename(temp);
          j := IndexOf(folder+temp);
          if j <> -1 then Objects[j].LoadFromString(temp1);
        end;
    end;

  end else begin
    // on crée les objects et on remplace les données des objects par celles qui sont sauvegardées
    for i:=0 to c-1 do
      if prop.StringValueOf('File'+i.ToString, temp, '') then
        if prop.StringValueOf('Data'+i.ToString, temp1, '') then begin
          with AddObject(temp, atimePosMS)^ do begin
            MaxGainValue := MUSIC_MAX_GAIN_VALUE;
            LoadFromString(temp1);
            atimePosMS := EndTimePosMS;
          end;
        end;
  end;
end;

procedure TMixerView.Init(aPB: TPaintBox);
begin
  PB := aPB;
  Objects := NIL;
  VisibleObjectsIndexes.InitDefault;
  ObjectUnderMouse := NIL;
  SelectedObject := NIL;
  IDValue := 0;
end;

function TMixerView.AddObject(const aFileName: string; aTimePosMS: int64): PMixerSoundObject;
var i, curID: integer;
  o: TMixerSoundObject;
begin
  // search the index where insert
  i := 0;
  if Length(Objects) = 0 then
    i := 0
  else if Objects[High(Objects)].TimePosMS < aTimePosMS then
    i := Length(Objects)
  else
    for i:=0 to High(Objects) do
      if Objects[i].TimePosMS > aTimePosMS then break;

  o.Caption := ExtractFileName(aFileName);
  o.Filename := aFileName;
  o.LoadAudio;
  o.TimePosMS := aTimePosMS;
  o.ID := NextIDValue;
  o.Muted := False;
  curID := o.ID;

  Insert(o, Objects, i);
  ShiftObjectIfOverlap;
  Result := @Objects[IndexOf(curID)];
end;

procedure TMixerView.DeleteSelectedObject;
var
  i: Integer;
begin
  i := IndexOf(SelectedObject);
  if i > -1 then begin
    Delete(Objects, i, 1);
    SelectedObject := NIL;
    IndexGainPointUnderMouse := -1;
    MouseOverState := mmoNothing;
    PB.Hint := '';
  end;
end;

function TMixerView.IndexOf(aObject: PMixerSoundObject): integer;
var
  i: Integer;
begin
  for i:=0 to High(Objects) do
    if @Objects[i] = aObject then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TMixerView.IndexOf(aID: integer): integer;
var
  i: Integer;
begin
  for i:=0 to High(Objects) do
    if Objects[i].ID = aID then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TMixerView.IndexOf(aFilename: string): integer;
var
  i: Integer;
begin
  for i:=0 to High(Objects) do
    if Objects[i].Filename = aFilename then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

procedure TMixerView.ShiftObjectIfOverlap;
var i: integer;
    nextTimeMS: int64;
begin
  nextTimeMS := 0;
  for i:=0 to High(Objects) do
    with Objects[i] do begin
      TimePosMS := Max(TimePosMS, nextTimeMS);
     nextTimeMS := EndTimePosMS;
    end;
end;

procedure TMixerView.MoveAudioObject(aObject: PMixerSoundObject; aDeltaTime: single);
var i, aObjectIndex: Integer;
  deltaMS, minPosMS: int64;
begin
  deltaMS := Round(aDeltaTime*1000);

  aObjectIndex := IndexOf(aObject);
  if aObjectIndex = -1 then exit;

  if aObjectIndex = 0
    then minPosMS := 0
    else minPosMS := Objects[aObjectIndex-1].EndTimePosMS;

  if Objects[aObjectIndex].TimePosMS+deltaMS < minPosMS then
    deltaMS := minPosMS-Objects[aObjectIndex].TimePosMS;

  for i:=aObjectIndex to High(Objects) do
    Objects[i].TimePosMS := Objects[i].TimePosMS + deltaMS;

 // Sort;
end;

{procedure TMixerView.Sort;
var i: Integer;
  flagFinish: boolean;
  temp: array of byte; // TMixerSoundObject;
begin
  if Length(Objects) < 2 then exit;
 // temp.Caption := ''; // avoid hint

  SetLength(temp, SizeOf(TMixerSoundObject));

  repeat
    flagFinish := True;
    for i:=1 to High(Objects) do
      if Objects[i-1].TimePosMS > Objects[i].TimePosMS then begin
        Move(Objects[i-1], temp[0], SizeOf(TMixerSoundObject));
        Move(Objects[i], Objects[i-1], SizeOf(TMixerSoundObject));
        Move(temp[0], Objects[i], SizeOf(TMixerSoundObject));
        flagFinish := False;
      end;
  until flagFinish;
end; }

function TMixerView.GetObjectAtTimePos(aTimePos: single): PMixerSoundObject;
var i: Integer;
  tms: int64;
begin
  Result := NIL;
  tms := Round(aTimePos*1000);

  for i:=0 to High(Objects) do
    if InRange(tms, Objects[i].TimePosMS, Objects[i].EndTimePosMS) then begin
      Result := @Objects[i];
      exit;
    end;
end;

function TMixerView.GetMouseOverState: TMixerMouseOver;
var tsBegin, tsEnd, tsMouse: single;
  p: TPoint;
  gainPoint: PMixerPointCurve;
  function XToTime(aX: integer): single;
  begin
    with FormMixer.FrameMixer1 do
      Result := DoAbscissaToTime(aX, PixelPerPoint)+ViewBeginTime;
  end;

begin
  Result := mmoNothing;
  if SelectedObject = NIL then exit;

  p := PB.ScreenToClient(Mouse.CursorPos);

  gainPoint := GetGainPointUnderMouse(p.X, p.Y, FormMixer.FrameMixer1.FColorGainPoint);
  if gainPoint <> NIL then begin
    Result := mmoGainPoint;
    exit;
  end;

  if PointIsOverGainCurve(p.X, p.Y, FormMixer.FrameMixer1.FColorGainCurve) then begin
    Result := mmoGainCurve;
    exit;
  end;

  tsMouse := XToTime(p.X);

  tsBegin := XToTime(p.X-PB.ScaleDesignToForm(HALF_WIDTH_TRIM_HANDLE));

  tsEnd := XToTime(p.X+PB.ScaleDesignToForm(HALF_WIDTH_TRIM_HANDLE));

  if InRange(SelectedObject^.TimePosMS*0.001, tsBegin, tsEnd) then
    Result := mmoLeftTrimHandle
  else if InRange(SelectedObject^.EndTimePosMS*0.001, tsBegin, tsEnd) then
    Result := mmoRightTrimHandle
  else if InRange(tsMouse, SelectedObject^.TimePosMS*0.001, SelectedObject^.EndTimePosMS*0.001) then
    Result := mmoBodyObject;
end;

function TMixerView.TotalDurationMS: int64;
var
  i: Integer;
begin
  Result := 0;
  for i:=0 to High(Objects) do
    Result := Max(Result, Objects[i].EndTimePosMS);
end;

procedure TMixerView.GetPointCurveLevel(aObject: PMixerSoundObject;
  IndexPoint: integer; out aX, aY: integer);
begin
  with aObject^ do begin
    with FormMixer.FrameMixer1 do
      aX := TimeToAbscissa((TimePosMS+(IndexPoint-UsedIndexes.First)*SLICE_TIME_MS)*0.001
                   - ViewBeginTime);
    aY := TempImage.Height-Round(Levels[IndexPoint]*TempImage.Height);
  end;
end;

procedure TMixerView.GetPointCurveGain(aObject: PMixerSoundObject;
  IndexPoint: integer; out aX, aY: integer);
var p: PMixerPointCurve;
  //durMS: int64;
begin
  with aObject^ do begin
    p := @CurveGain[IndexPoint];
    //durMS := int64((UsedIndexes.Count-1)*SLICE_TIME_MS);
    with FormMixer.FrameMixer1 do
      aX := TimeToAbscissa((TimePosMS+{durMS}DurationMS*p^.x)*0.001-ViewBeginTime);

    aY := PB.ClientHeight-Round(PB.ClientHeight*p^.y);
  end;
end;

function TMixerView.GetGainPointUnderMouse(aX, aY: integer;
  const aColor: TBGRAPixel): PMixerPointCurve;
var timeMouse, timePoint, deltaPoint: single;
  i: Integer;
begin
  Result := NIL;
  indexGainPointUnderMouse := -1;
  if SelectedObject = NIL then exit;
  if TempImage.GetPixel(aX, aY) <> aColor then exit;

  with FormMixer.FrameMixer1 do begin
    timeMouse := DoAbscissaToTime(aX, PixelPerPoint)+ViewBeginTime;
    deltaPoint := DoAbscissaToTime(HALF_WIDTH_POINT_CURVE, PixelPerPoint);
  end;

  for i:=0 to High(SelectedObject^.CurveGain) do
    with SelectedObject^.CurveGain[i] do begin
      timePoint := SelectedObject^.TimePosMS*0.001+x*SelectedObject^.DurationMS*0.001;
      if InRange(timeMouse, timePoint-deltaPoint, timePoint+deltaPoint) then begin
        Result := @SelectedObject^.CurveGain[i];
        IndexGainPointUnderMouse := i;
        exit;
      end;
    end;
end;

function TMixerView.PointIsOverGainCurve(aX, aY: integer;
  const aColor: TBGRAPixel): boolean;
begin
  Result := False;
  if SelectedObject = NIL then exit;
  Result := TempImage.GetPixel(aX, aY) = aColor;
end;

function TMixerView.NextIDValue: integer;
begin
  inc(IDValue);
  Result := IDValue;
end;

{ TMixerSoundObject }

procedure TMixerSoundObject.InitGFromPos(aTimePosMS: int64);
var i: integer;
begin
  i := 0;
  if Length(CurveGain) < 2 then begin
    WorkingindexCurveGain := 0;
    g.Value := 1.0;
  end else
  if aTimePosMS >= EndTimePosMS then begin
    g.Value := 0.0;
    WorkingindexCurveGain := High(CurveGain);
  end else
  if aTimePosMS < TimePosMS then begin
    WorkingindexCurveGain := 1;
    g.Value := GetGainValue(0);
    g.ChangeTo(GetGainValue(1), (GetGainTimeMS(1)-GetGainTimeMS(0))*0.001);
  end else begin
    for i:=0 to High(CurveGain)-1 do
      if GetAbsGainTimeMS(i+1) > aTimePosMS then
        break;   // i = index of point before aTimePosMS

    g.Value := GetGainValue(i);
    g.ChangeTo(GetGainValue(i+1), (GetAbsGainTimeMS(i+1)-GetAbsGainTimeMS(i))*0.001);
    g.OnElapse((aTimePosMS-GetAbsGainTimeMS(i))*0.001);
    WorkingindexCurveGain := i+1;

    SamplesToDo := GainPointToSampleIndex(i+1) - AbsTimeMSToSampleIndex(aTimePosMS);
  end;

end;

function TMixerSoundObject.SaveToString: string;
var prop: TProperties;
  i: integer;
  temp: string;
begin
  prop.Init('"');
  prop.Add('TotalIndexes', Length(Levels));
  prop.Add('UsedIndexes', UsedIndexes.SaveToString);
  prop.Add('TimePosMS', TimePosMS);
  prop.Add('MaxGainValue', MaxGainValue);
  prop.Add('Muted', Muted);

  prop.Add('CurveGainCount', integer(Length(CurveGain)));
  temp := '';
  for i:=0 to High(CurveGain) do begin
    if i > 0 then temp := temp+'&';
    temp := temp+FormatFloatWithDot('0.000', CurveGain[i].x)+'&'+
                 FormatFloatWithDot('0.000', CurveGain[i].y)
  end;
  prop.Add('CurveGainPoint', temp);
  Result := prop.PackedProperty;
end;

procedure TMixerSoundObject.LoadFromString(const s: string);
var prop: TProperties;
  vi, i, previousTotalIndexes: integer;
  vs: string;
  A: TStringArray;
begin
  prop.Split(s, '"');
  vi := 0;
  vs := '';
  previousTotalIndexes := 0;

  prop.IntegerValueOf('TotalIndexes', previousTotalIndexes, Length(Levels));

  if prop.StringValueOf('UsedIndexes', vs, '') then
    UsedIndexes.LoadFromString(vs)
  else begin
    UsedIndexes.First := 0;
    UsedIndexes.Last := High(Levels);
  end;

  if previousTotalIndexes <> Length(Levels) then begin
    UsedIndexes.First := 0;
    UsedIndexes.Last := High(Levels);
  end;

  // in case of audio file length was changed since last session...
  UsedIndexes.First := EnsureRange(UsedIndexes.First, 0, High(Levels));
  UsedIndexes.Last := EnsureRange(UsedIndexes.Last, 0, High(Levels));
  UsedIndexes.Last := Max(UsedIndexes.Last, UsedIndexes.First);

  prop.Int64ValueOf('TimePosMS', TimePosMS, TimePosMS);

  prop.SingleValueOf('MaxGainValue', MaxGainValue, MaxGainValue);
  prop.BooleanValueOf('Muted', Muted, False);

  if prop.IntegerValueOf('CurveGainCount', vi, 0) then begin
    vs := '';
    if prop.StringValueOf('CurveGainPoint', vs, '') then begin
      A := vs.Split(['&']);
      if not Odd(Length(A)) and (Length(A) div 2 = vi) then begin
        SetLength(CurveGain, vi);
        for i:=0 to High(CurveGain) do begin
          CurveGain[i].x := StringToSingle(A[i*2]);
          CurveGain[i].y := StringToSingle(A[i*2+1]);
        end;
      end;
    end;
  end;
end;

procedure TMixerSoundObject.LoadAudio;
var reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  i, levelCount, remainder: integer;
  function ComputeLevelFromBuf(const aBuf: TALSPlaybackBuffer): single;
  var i: Integer;
  begin
    aBuf.ComputeChannelsLevel;

    Result := ALS_DECIBEL_MIN_VALUE;
    for i:=0 to aBuf.ChannelCount-1 do
      if Result < aBuf.ChannelsLeveldB[i] then
        Result := aBuf.ChannelsLeveldB[i];

    Result := (Result+DECIBEL_MAX_ABSOLUTE_VALUE)/DECIBEL_MAX_ABSOLUTE_VALUE;
  end;
begin
  Levels := NIL;
  CurveGain.Initdefault;

  if not reader.OpenRead(Filename) then exit;
  RealDuration := reader.TotalDuration;

  buf.Init(reader.Channels, ALS_SAMPLE_INT16);
  buf.FrameCapacity := Round(reader.Samplerate*SLICE_TIME);

  levelCount := 0;
  remainder := 0;
  DivMod(reader.Frames, buf.FrameCapacity, levelCount, remainder);
// Finaly, we don't take in acount the remainder otherwise user can't repeat a
// looped sound without a short silence between each repeated object...

  SetLength(Levels, levelCount);

  i := 0;
  while (reader.Read(buf, buf.FrameCapacity) > 0) and (i < levelCount) do begin
    Levels[i] := ComputeLevelFromBuf(buf);
    inc(i);
  end;

  reader.Close;
  buf.FreeMemory;

  UsedIndexes.First := 0;
  UsedIndexes.Last := High(Levels);
end;

function TMixerSoundObject.DurationMS: int64;
begin
{  if (UsedIndexes.First = Low(Levels)) and (UsedIndexes.Last = High(Levels)) then
    Result := Round(RealDuration*1000)
  else }
    Result := int64((UsedIndexes.Count-1)*SLICE_TIME_MS);
end;

procedure TMixerSoundObject.ComputeViewIndexes(const aViewBeginTime, aViewEndTime: single);
var viewBeginMS, viewEndMS, deltaMS: int64;
begin
  ViewIndexes.InitDefault;

  viewBeginMS := Trunc(aViewBeginTime*1000);
  viewEndMS := Trunc(aViewEndTime*1000);

  if (TimePosMS >= viewEndMS) or (EndTimePosMS <= viewBeginMS) then exit;

  deltaMS := TimePosMS - viewBeginMS;
  if deltaMS >= 0 then  // the begin of object is in the view ?
    ViewIndexes.First := UsedIndexes.First
  else
    ViewIndexes.First := Min(UsedIndexes.First+Abs(deltaMS) div SLICE_TIME_MS,
                             UsedIndexes.Last);

//    ViewIndexes.First := Min(Abs(deltaMS div SLICE_TIME_MS), UsedIndexes.Last);

  deltaMS := EndTimePosMS - viewEndMS;
  if deltaMS <= 0 then // the end of object is in the view ?
    ViewIndexes.Last := UsedIndexes.Last
  else
    ViewIndexes.Last := EnsureRange(UsedIndexes.Last - (deltaMS div SLICE_TIME_MS)+1,
                                    UsedIndexes.First,
                                    UsedIndexes.Last);

  if ViewIndexes.Last > High(Levels) then
    Raise exception.Create('ViewIndexes.Last: index > High(FLevels)');

  if ViewIndexes.Last < ViewIndexes.First then
    Raise exception.Create('index error');
end;

function TMixerSoundObject.EndTimePosMS: int64;
begin
  Result := TimePosMS + DurationMS;
end;

function TMixerSoundObject.HaveWorkingTime(aCurrentTimeMS: int64; out
  aNextWorkingTimeMS: int64): boolean;
begin
  aNextWorkingTimeMS := -1;

  Result := not Muted;
  if not Result then exit;

  Result := aCurrentTimeMS < endTimePosMS;
  if not Result then begin
    if Sound.State = ALS_PLAYING then begin
      Sound.SetOnCustomDSP(NIL, NIL); // we do it before to stop the sound
                                      // because Stop rewind and prebuf it again
      Sound.Stop;
    end;
    exit;
  end;

  if aCurrentTimeMS >= TimePosMS then begin
    if Sound.State <> ALS_PLAYING then
      Sound.Play(False);

    aNextWorkingTimeMS := EndTimePosMS;
  end else aNextWorkingTimeMS := TimePosMS;
end;

const MIN_TOTAL_DURATION_MS_AFTER_TRIM = 100;

function TMixerSoundObject.LeftTrimTo(aTimePos: single): boolean;
var newTimePosMS, deltaTimeMS: int64;
  deltaIndexes, newFirstIndex: integer;
begin
  Result := False;
  if aTimePos < 0 then exit;

  newTimePosMS := Round(aTimePos*1000);
  newTimePosMS := Max(newTimePosMS, TimePosMS - UsedIndexes.First*SLICE_TIME_MS);

  deltaTimeMS := newTimePosMS - TimePosMS;
  deltaIndexes := deltaTimeMS div SLICE_TIME_MS;
  if deltaIndexes = 0 then exit;

  newFirstIndex := UsedIndexes.First + deltaIndexes;
  newFirstIndex := EnsureRange(newFirstIndex,
                   Low(Levels),
                   UsedIndexes.Last-MIN_TOTAL_DURATION_MS_AFTER_TRIM div SLICE_TIME_MS);

  if newFirstIndex = UsedIndexes.First then exit;

  TimePosMS := newTimePosMS;
  UsedIndexes.First := newFirstIndex;
  Result := True;
end;

function TMixerSoundObject.RightTrimTo(aTimePos: single): boolean;
var deltaTime: single;
  deltaIndexes, newLastIndex: integer;
begin
  Result := False;
  if aTimePos < 0 then exit;

  deltaTime := aTimePos - EndTimePosMS*0.001;
  deltaIndexes := Trunc(deltaTime/SLICE_TIME);
  if deltaIndexes = 0 then exit;
  newLastIndex := UsedIndexes.Last + deltaIndexes;

  newLastIndex := EnsureRange(newLastIndex,
                 UsedIndexes.First+MIN_TOTAL_DURATION_MS_AFTER_TRIM div SLICE_TIME_MS,
                 High(Levels));

  if newLastIndex = UsedIndexes.Last then exit;
  UsedIndexes.Last := newLastIndex;
  Result := True;
end;

function TMixerSoundObject.MoveGainPoint(aIndex: integer; aX, aY: single): boolean;
var minX, maxX: single;
begin
  Result := False;
  if Length(CurveGain) = 0 then exit;
  if (aIndex < 0) or (aIndex > High(CurveGain)) then exit;

  if aIndex = 0 then begin
    minX := 0.0;
    maxX := 0.0;
  end
  else if aIndex = High(CurveGain) then begin
    minX := 1.0;
    maxX := 1.0;
  end else begin
    minX := CurveGain[aIndex-1].x + MIN_TIME_MS_BETWEEN_POINT_CURVE/DurationMS;
    maxX := CurveGain[aIndex+1].x - MIN_TIME_MS_BETWEEN_POINT_CURVE/DurationMS;
  end;

  aX := Max(aX, minX);
  aX := Min(aX, maxX);
//  if (CurveGain[aIndex].x = aX) and (CurveGain[aIndex].y = aY) then exit;
  CurveGain[aIndex].y := EnsureRange(aY, 0.0, 1.0);
  CurveGain[aIndex].x := aX;
  Result := True;
end;


function TMixerSoundObject.AddGainPoint(aX, aY: single): integer;
var newpoint: TMixerPointCurve;
  i: Integer;
begin
  Result := -1;
  if not InRange(aX, 0.0, 1.0) then exit;
  if not InRange(aY, 0.0, 1.0) then exit;
  if DurationMS < MIN_TIME_MS_BETWEEN_POINT_CURVE*3 then exit;

  for i:=0 to High(CurveGain)-1 do begin
    if (aX > CurveGain[i].x + MIN_TIME_MS_BETWEEN_POINT_CURVE/DurationMS) and
       (aX < CurveGain[i+1].x - MIN_TIME_MS_BETWEEN_POINT_CURVE/DurationMS) then begin
      Result := i+1;
      newpoint.x := aX;
      newpoint.y := aY;
      Insert(newpoint, CurveGain, i+1);
      exit;
    end;
  end;
end;

procedure TMixerSoundObject.DeleteGainPoint(aIndex: integer);
begin
  if (Length(CurveGain) = 0) or
     (aIndex < Low(CurveGain)) or (aIndex > High(CurveGain)) then exit;
  Delete(CurveGain, aIndex, 1);
end;

function TMixerSoundObject.GetGainValue(aIndex: integer): single;
begin
  Result := CurveGain[aIndex].y;
  Result := Result*Result * MaxGainValue;
end;

function TMixerSoundObject.GetGainTimeMS(aIndex: integer): int64;
begin
  Result := Round(CurveGain[aIndex].x*DurationMS);
end;

function TMixerSoundObject.GetAbsGainTimeMS(aIndex: integer): int64;
begin
  Result := Round(CurveGain[aIndex].x*DurationMS)+TimePosMS;
end;

function TMixerSoundObject.GainPointToSampleIndex(aPointIndex: integer): integer;
var frameRange: single;
begin
  if (UsedIndexes.First = 0) and (UsedIndexes.Last = High(Levels)) then
    frameRange := single(Sound.FrameCount-1)
  else
    frameRange := UsedIndexes.Count*Sound.SampleRate*SLICE_TIME;

  Result := Round(frameRange*CurveGain[aPointIndex].x);
end;

function TMixerSoundObject.AbsTimeMSToSampleIndex(aTimePosMS: int64): integer;
var frameRange: single;
begin
  if (aTimePosMS < TimePosMS) or (aTimePosMS > EndTimePosMS) then
    Result := -1
  else begin

    if (UsedIndexes.First = 0) and (UsedIndexes.Last = High(Levels)) then
      frameRange := single(Sound.FrameCount-1)
    else
      frameRange := UsedIndexes.Count*Sound.SampleRate*SLICE_TIME;

    Result := Round(frameRange * (aTimePosMS-TimePosMS)/DurationMS);
  end;
end;

{ TFrameMixer }

procedure TFrameMixer.ScrollBar1Scroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  FScrollBarLocked := True;
  ViewBeginTime := ScrollBar1.Position*SLICE_TIME;
  FScrollBarLocked := False;
  Redraw;
end;

procedure TFrameMixer.PBTimePaint(Sender: TObject);
var xx, ybase, yLegend, yTimeMark: integer;
  timePos: double; // seconde
  s: string;
begin
  with PBTime.Canvas do begin
    // background
    Brush.Color := clBlack; // $003A3A3A;
    Brush.Style := bsSolid;
    Pen.Color := clBlack; // $003A3A3A;
    FillRect(PBTime.ClientRect);
    // setting font from the frame's parent
    Font.Assign(Self.Parent.Font);
    Font.Color := clWhite;

    ybase :=PBTime.ClientHeight-2;
    yLegend := 0; // ybase-Self.Font.Height;
    yTimeMark := yLegend+Font.Height;

    // axis
    Pen.Color := Font.Color; // RGBToColor(10,10,10);
    Line(1, ybase, PBTime.ClientWidth-1, ybase);

    Brush.Style := bsClear;

    timePos := FLegendBeginTimePosition;
    xx := TimeToAbscissa(timePos-ViewBeginTime);

    while xx < PBTime.ClientWidth do begin
      s := TimeToString(timePos, 1);
      TextOut(xx-TextWidth(s) div 2, yLegend, s);
      Line(xx, yTimeMark, xx, ybase);
      xx := xx + TimeToAbscissa(FLegendTimeInterval);
      timePos := timePos + FLegendTimeInterval;
    end;
  end;
end;

procedure TFrameMixer.PopupMusicObjectPopup(Sender: TObject);
begin
  MIDeleteMusic.Enabled := not FPreviewIsRunning;
end;

procedure TFrameMixer.PopupSoundObjectPopup(Sender: TObject);
begin
  MIDeleteSound.Enabled := not FPreviewIsRunning;
end;

procedure TFrameMixer.PBVoicePaint(Sender: TObject);
var xx, yy, i: integer;
  t: TBGRABitmap;
  r: TRect;
  p: PMixerView;
  s: string;
begin
  p := PaintBoxToMixerView(Sender);

  t := p^.TempImage.Duplicate(False);

{  // selection area
  if FTimeSelectionLeft < FTimeSelectionRight then begin
    t.Rectangle(TimeToAbscissa(FTimeSelectionLeft-FViewBeginTime), 0,
                TimeToAbscissa(FTimeSelectionRight-FViewBeginTime), t.Height,
                BGRA(255,255,255,35), BGRA(255,255,255,35), dmDrawWithTransparency);
    xx := TimeToAbscissa(FTimeSelectionRight-FViewBeginTime);
    t.VertLine(xx, 0, t.Height, BGRA(255,255,0), dmSet);
  end;  }

  // enlight selected object (if visible)
  if p^.SelectedObject <> NIL then
    if p^.SelectedObject^.ViewIndexes.First <> -1 then
      t.FillRect(p^.SelectedObject^.ViewRect, BGRA(0,255,255,25),dmDrawWithTransparency);

  // enlight audio under mouse
  if (p^.ObjectUnderMouse <> NIL) and
     (p^.ObjectUnderMouse <> p^.SelectedObject) then
    t.FillRect(p^.ObjectUnderMouse^.ViewRect, BGRA(0,255,255,5),dmDrawWithTransparency);

  // cursor and left selection boundary
  if Length(p^.Objects) > 0 then begin
    xx := TimeToAbscissa(FTimeSelectionLeft-FViewBeginTime);
    t.VertLine(xx, 0, t.Height, BGRA(255,255,0), dmSet);
  end;

  t.Draw(p^.PB.Canvas, 0, 0, False);
  t.Free;

  // cursor play
  if (FCursorPlayTimeMS <> -1) and (PixelPerPoint <> 0) then begin
    xx := TimeToAbscissa(FCursorPlayTimeMS*0.001-FViewBeginTime);
    with p^.PB.Canvas do begin
      Pen.Color := RGBToColor(0,255,255);
      Line(xx, 0, xx, p^.PB.ClientHeight);
    end;
  end;

  // filename
  if p^.VisibleObjectsIndexes.First <> -1 then begin
    yy := 0;
    with p^.PB.Canvas do
      for i:=p^.VisibleObjectsIndexes.First to p^.VisibleObjectsIndexes.Last do begin
        if p^.Objects[i].ViewIndexes.First = -1 then continue;
        r := TRect.Create(p^.Objects[i].ViewRect);
        r.Left := r.Left+2;
        r.Right := r.Right-4;
        Font.Color := clWhite;
        s := p^.Objects[i].Caption;
{s:=s+lineending+'TimePosMS:'+p^.Objects[i].TimePosMS.ToString;
s:=s+lineending+'EndTimePosMS:'+p^.Objects[i].EndTimePosMS.ToString;
s:=s+lineending+'DurationMS:'+p^.Objects[i].DurationMS.ToString;
s:=s+lineending+'UsedIndexes ['+p^.Objects[i].UsedIndexes.First.ToString+'..'+p^.Objects[i].UsedIndexes.Last.ToString+']';
s:=s+lineending+'ViewIndexes ['+p^.Objects[i].ViewIndexes.First.ToString+'..'+p^.Objects[i].ViewIndexes.Last.ToString+']';
s:=s+lineending+'Length(Levels)='+Length(p^.Objects[i].Levels).ToString;
s:=s+lineending+'TotalDuration:'+TotalDuration.ToString;
s:=s+lineending+'PixelPerpoint:'+PixelPerPoint.ToString; }
        TextRect(r, 0, yy, s, FTextStyle);
        inc(yy, p^.PB.Font.Height);
        if i div 3 <> 0 then yy := 0;
      end;
  end;

{  PBVoice.Canvas.Brush.Style := bsClear;
  PBVoice.Canvas.TextOut(0,0,'PixelPerPoint '+FormatFloat('0.0000', FPixelPerPoint));
  PBVoice.Canvas.TextOut(0,15,'LegendTimeInterval '+FormatFloat('0.0000', FLegendTimeInterval));   }
end;

procedure TFrameMixer.PBVoiceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var p: PMixerView;
  procedure DoSelectObjectUndermouse;
  var s: string;
  begin
    s := '';
    p^.SelectedObject := p^.ObjectUnderMouse;
    DoSelectionChange;
    p^.PB.Cursor := crHandPoint;
    p^.MouseOverState := mmoBodyObject;
    if p = @FVoiceView then begin
      FMusicView.SelectedObject := NIL;
      FSoundView.SelectedObject := NIL;
    end;
    if p = @FMusicView then begin
      FVoiceView.SelectedObject := NIL;
      FSoundView.SelectedObject := NIL;
      AddUserHelpMessage(s, [SMouseRightClickContextualMenu]);
    end;
    if p = @FSoundView then begin
      FVoiceView.SelectedObject := NIL;
      FMusicView.SelectedObject := NIL;
      AddUserHelpMessage(s, [SMouseRightClickContextualMenu]);
    end;
    if p^.SelectedObject <> NIL then
      AddUserHelpMessage(s, [SCtrlLeftClickForMove]);
    AddUserHelpMessage(s, [SMouseWheelForZoom, SSpaceForListenStop]);
    FormMixer.LabelHelp.Caption := s;
  end;

begin
  if FMouseHandledByUser then exit;

  p := PaintBoxToMixerView(Sender);

  FTimeClickOrigin := AbscissaToTime(X) + ViewBeginTime;

  if Button = mbLeft then begin
    if p^.MouseOverState = mmoGainCurve then begin
      AddGainPoint(p);
      exit;
    end;

    if //(p^.SelectedObject <> p^.ObjectUnderMouse) and
       (p^.MouseOverState <> mmoLeftTrimHandle) and
       (p^.MouseOverState <> mmoRightTrimHandle) and
       (p^.MouseOverState <> mmoGainPoint) and
       (p^.MouseOverState <> mmoGainCurve)then begin
      if p^.SelectedObject <> p^.ObjectUnderMouse then
        HidePanelVolume;
      // select the audio under the mouse
      DoSelectObjectUndermouse;
    end;

    SetCursorAt(AbscissaToTime(X) + ViewBeginTime);

    if p^.SelectedObject <> NIL then begin
      FMouseButtonPressed := mbpLeft;
    end else p^.PB.Cursor := crDefault;
  end;

  // popup menu
  if Button = mbRight then begin
    HidePanelVolume;

    SetCursorAt(AbscissaToTime(X) + ViewBeginTime);
    // before popup, we select the object under the mouse
    p^.ObjectUnderMouse := p^.GetObjectAtTimePos(FTimeClickOrigin);

    if p^.SelectedObject <> p^.ObjectUnderMouse then begin
      DoSelectObjectUndermouse;
      Redraw;
    end;

    //FMouseButtonPressed := mbpRight;
    if p^.SelectedObject <> NIL then begin
      FTargetObjectForCursorVolume := p^.SelectedObject;
      if p^.PopupObject <> NIL then p^.PopupObject.PopUp;
    end else begin
      if (p^.PopupEmpty <> NIL) and
         not FPreviewIsRunning then p^.PopupEmpty.PopUp;
    end;
  end;
end;

procedure TFrameMixer.PBMusicPaint(Sender: TObject);
var pb: TPaintBox;
  back: TColor;
  p: TPoint;
  sEmpty: string;
  fontColor: TColor;
  pm: PMixerView;
begin
  sEmpty := '';
  pm := PaintBoxToMixerView(Sender);

  if Length(pm^.Objects) > 0 then begin
    PBVoicePaint(Sender);
    exit;
  end;
  pb := Sender as TPaintBox;
  back := RGBToColor(15,15,15);

  fontColor := RGBToColor(200,200,200);
  if pb = PBMusic then with PBMusic.Canvas do
    sEmpty := SRightClickToAddMusic;

  if pb = PBSound then with PBSound.Canvas do
    sEmpty := SRightClickToAddSound;

  with pb.Canvas do begin
    Brush.Style := bsSolid;
    Brush.Color := back;
    FillRect(pb.ClientRect);

    p := pb.ClientRect.CenterPoint;
    Font.Color := fontColor;
    p.X := p.X-Font.GetTextWidth(sEmpty) div 2;
    p.Y := p.Y-Font.GetTextHeight(sEmpty) div 2;
    TextOut(p.X, p.Y, sEmpty);
  end;
end;

procedure TFrameMixer.MIAddMusicClick(Sender: TObject);
begin
  if Sender = MIAddMusic then begin
    if FPreviewIsRunning then exit;
    OpenDialog1.Filter := ALSManager.DialogFileFilters(SMusics, SAll);
    if ProgramOptions.MusicFolder <> '' then
      OpenDialog1.InitialDir := ProgramOptions.MusicFolder;
    if not OpenDialog1.Execute then exit;
    ProgramOptions.MusicFolder := ExtractFilePath(OpenDialog1.FileName);
    ProgramOptions.Save;
    with FMusicView.AddObject(OpenDialog1.FileName, Round(FTimeClickOrigin*1000))^ do
      MaxGainValue := MUSIC_MAX_GAIN_VALUE;
    RecomputeFirstLastAudioIndexView(@FMusicView);
    RedrawTempImage(@FMusicView);
    PBMusic.Invalidate;
  end;

  if Sender = MIDeleteMusic then begin
    if FPreviewIsRunning then exit;
    if AskConfirmation(SDeleteThisMusic,
                       SYes, SNo, mtConfirmation) <> mrOk then exit;
    FMusicView.DeleteSelectedObject;
    RecomputeFirstLastAudioIndexView(@FMusicView);
    RedrawTempImage(@FMusicView);
    PBMusic.Invalidate;
  end;

  if Sender = MISetVolumeMusic then begin
    FTargetObjectForCursorVolume := FMusicView.SelectedObject;
    FrameTrackBar1.PercentValue := FTargetObjectForCursorVolume^.MaxGainValue;
    ShowPanelVolume(PBMusic.Top);
  end;

  if Sender = MIAddSound then begin
    if FPreviewIsRunning then exit;
    OpenDialog1.Filter := ALSManager.DialogFileFilters(SSounds, SAll);
    if ProgramOptions.SoundFolder <> '' then
      OpenDialog1.InitialDir := ProgramOptions.SoundFolder;
    if not OpenDialog1.Execute then exit;
    ProgramOptions.SoundFolder := ExtractFilePath(OpenDialog1.FileName);
    ProgramOptions.Save;
    with FSoundView.AddObject(OpenDialog1.FileName, Round(FTimeClickOrigin*1000))^ do
      MaxGainValue := SOUND_MAX_GAIN_VALUE;
    RecomputeFirstLastAudioIndexView(@FSoundView);
    RedrawTempImage(@FSoundView);
    PBSound.Invalidate;
  end;

  if Sender = MIDeleteSound then begin
    if FPreviewIsRunning then exit;
    if AskConfirmation(SDeleteThisSound,
                       SYes, SNo, mtConfirmation) <> mrOk then exit;
    FSoundView.DeleteSelectedObject;
    RecomputeFirstLastAudioIndexView(@FSoundView);
    RedrawTempImage(@FSoundView);
    PBSound.Invalidate;
  end;

  if Sender = MISetVolumeSound then begin
    FTargetObjectForCursorVolume := FSoundView.SelectedObject;
    FrameTrackBar1.PercentValue := FTargetObjectForCursorVolume^.MaxGainValue;
    ShowPanelVolume(PBSound.Top);
  end;
end;

procedure TFrameMixer.BHideVolumeCursorClick(Sender: TObject);
begin
  HidePanelVolume;
end;

procedure TFrameMixer.BMuteClick(Sender: TObject);
begin
  if (Sender = MIInvertMuteMusic) or (Sender = MIInvertMuteSound) then begin
    ToggleSpeedButtonManager1.SetState(BMute, not FTargetObjectForCursorVolume^.Muted);
  end;

  FTargetObjectForCursorVolume^.Muted := ToggleSpeedButtonManager1.Checked[BMute];
  if FPreviewIsRunning then
    FTargetObjectForCursorVolume^.Sound.Mute := FTargetObjectForCursorVolume^.Muted;
  ReconstructTempImageAndRedraw;
end;

procedure TFrameMixer.PBVoiceMouseLeave(Sender: TObject);
var p: PMixerView;
begin
  p := PaintBoxToMixerView(Sender);

  p^.ObjectUnderMouse := NIL;
  p^.PB.Cursor := crDefault;
  FMouseButtonPressed := mbpNone;
  Redraw;
end;

procedure TFrameMixer.PBVoiceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var thresholdDone: boolean;
  mouseTime: single;
  p: PMixerView;
  o: PMixerSoundObject;
  s: String;
begin
  if FMouseHandledByUser then exit;

  p := PaintBoxToMixerView(Sender);

  mouseTime := AbscissaToTime(X)+FViewBeginTime;

  if (FMouseButtonPressed <> mbpNone) then begin
    thresholdDone := TimeToAbscissa(Abs(FTimeClickOrigin-mouseTime)) > ScaleDesignToForm(MOUSE_MOVE_THRESHOLD);
    case FMouseButtonPressed of
      mbpLeft: begin
        if {thresholdDone and} (p^.MouseOverState = mmoGainPoint) then
          LoopMoveGainPoint(p)
        {else if thresholdDone and (p^.MouseOverState = mmoGainCurve) then
          AddGainPoint(p) }
        else if thresholdDone and (p^.MouseOverState = mmoLeftTrimHandle) then
          LoopMoveLeftTrimHandle(p)
        else if thresholdDone and (p^.MouseOverState = mmoRightTrimHandle) then
          LoopMoveRightTrimHandle(p)

        {else if thresholdDone and not (ssCtrl in Shift)then begin
          LoopSelection(p);
          exit;
        end}
        else if thresholdDone and (ssCtrl in Shift) and (p^.MouseOverState = mmoBodyObject) then begin
          LoopMoveAudioBlock(p);
          exit;
        end;
      end;

      mbpRight: if thresholdDone then begin
        FTimeClickOrigin := AbscissaToTime(X);
        LoopMoveView;
        exit;
      end;
    end;
  end else begin   // no mouse button pressed

    if (p^.MouseOverState <> mmoLeftTrimHandle) and
       (p^.MouseOverState <> mmoRightTrimHandle) then begin
      // enlight the audio under the mouse
      o := p^.GetObjectAtTimePos(mouseTime);
      if p^.ObjectUnderMouse <> o then begin
        p^.ObjectUnderMouse := o;
        p^.PB.Invalidate;
        // hint with the name of the object
        p^.PB.ShowHint := o <> NIL;
        if o <> NIL then
          p^.PB.Hint := ExtractFilename(o^.Filename);
      end;

      s := '';
      if o <> NIL
         then s := SLeftClickToSelect
         else if p <> @FVoiceView then s := SMouseRightClickContextualMenu;
      AddUserHelpMessage(s, [SMouseWheelForZoom, SSpaceForListenStop]);
      FormMixer.LabelHelp.Caption := s;
    end;

    // update mouse cursor and help message for user
    p^.MouseOverState := p^.GetMouseOverState;
    case p^.MouseOverState of
      mmoLeftTrimHandle, mmoRightTrimHandle: begin
        p^.PB.Cursor := crHSplit;
        FormMixer.LabelHelp.Caption := SLeftClickAndMoveToTrimAudio;
      end;

      mmoBodyObject: begin
        if ssCtrl in Shift
          then p^.PB.Cursor := crHandPoint
          else p^.PB.Cursor := crDefault;
        FormMixer.LabelHelp.Caption := SCtrlLeftClickForMove;
      end;
      mmoGainPoint: begin
        p^.PB.Cursor := crHandPoint;
        FormMixer.LabelHelp.Caption := SLeftClickToDragOrDeletePoint;
      end;
      mmoGainCurve: begin
        p^.PB.Cursor := crUpArrow;
        FormMixer.LabelHelp.Caption := SLeftClickToAddPoint;
      end;
      else p^.PB.Cursor := crDefault;
    end;
  end;
end;

procedure TFrameMixer.PBVoiceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseButtonPressed := mbpNone;

  if (FTimeSelectionLeft = FTimeSelectionRight) and
     (FTimeSelectionLeft > TotalDuration) then begin
      FTimeSelectionLeft := TotalDuration;
      FTimeSelectionRight := FTimeSelectionLeft;
      Redraw;
     end;
end;

procedure TFrameMixer.PBVoiceMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var v, targetTimePos: single;
begin
  targetTimePos := AbscissaToTime(MousePos.X)+ViewBeginTime;

  FRedrawLocked := True;

  v := PixelPerPoint*ZOOM_COEFF;
  if WheelDelta > 0 then
    PixelPerPoint := PixelPerPoint+v
  else
    PixelPerPoint := PixelPerPoint-v;

  ViewBeginTime := ViewBeginTime+
                   targetTimePos-(AbscissaToTime(MousePos.X)+ViewBeginTime);

  UpdateScrollBar;
  Handled := True;
  FRedrawLocked := False;

  RedrawTempImage(@FVoiceView);
  RedrawTempImage(@FMusicView);
  RedrawTempImage(@FSoundView);
  Redraw;
end;

procedure TFrameMixer.PBVoiceResize(Sender: TObject);
var
  p: PMixerView;
begin
  p := PaintBoxToMixerView(Sender);

  p^.FGradientBackground.SetSize(p^.PB.ClientWidth, p^.PB.ClientHeight);
  RedrawGradientImage(p);

  p^.TempImage.SetSize(p^.PB.ClientWidth, p^.PB.ClientHeight);
  RedrawTempImage(p);

  Redraw;
end;

procedure TFrameMixer.DoReset;
//var i: integer;
begin
{  // delete all sounds
  for i:=0 to High(FVoices) do
    FVoices[i].Sound.Kill;    }

  FPixelPerPoint := 1.0;
  FViewBeginTime := 0.0;
  FLegendTimeInterval := 1.0;

  FTimeSelectionLeft := 0;
  FTimeSelectionRight := 0;

  FCursorPlayTimeMS := -1;

  FVoiceView.Init(PBVoice);
  FVoiceView.PopupEmpty := NIL;
  FVoiceView.PopupObject := NIL;
  RedrawTempImage(@FVoiceView);

  FMusicView.Init(PBMusic);
  FMusicView.PopupEmpty := PopupMusicEmpty;
  FMusicView.PopupObject := PopupMusicObject;
  RedrawTempImage(@FMusicView);

  FSoundView.Init(PBSound);
  FSoundView.PopupEmpty := PopupSoundEmpty;
  FSoundView.PopupObject := PopupSoundObject;
  RedrawTempImage(@FSoundView);

  DoSelectionChange;
end;

function TFrameMixer.GetViewEndTime: single;
begin
  Result := FViewBeginTime+AbscissaToTime(PBVoice.ClientWidth);
end;

procedure TFrameMixer.RecomputeFirstLastAudioIndexView(p: PMixerView);
var pBegin, pEnd: Single;
  i: Integer;
  flag: boolean;
begin
  p^.VisibleObjectsIndexes.InitDefault;
  if Length(p^.Objects) = 0 then exit;

  // retrieve the visible audio objects
  for i:=0 to High(p^.Objects) do begin
    pBegin := p^.Objects[i].TimePosMS * 0.001;
    pEnd := p^.Objects[i].EndTimePosMS * 0.001;

    p^.Objects[i].ViewIndexes.InitDefault;

    flag := InRange(pBegin, ViewBeginTime, ViewEndTime) or
            InRange(pEnd, ViewBeginTime, ViewEndTime) or
            ((pBegin <= ViewBeginTime) and (pEnd >= ViewEndTime));

    if (p^.VisibleObjectsIndexes.First = -1) and flag then begin
      p^.VisibleObjectsIndexes.First := i;
      p^.VisibleObjectsIndexes.Last := i;
    end;


    if (p^.VisibleObjectsIndexes.Last <> -1) and flag  then
       p^.VisibleObjectsIndexes.Last := i;
  end;

  if p^.VisibleObjectsIndexes.First = -1 then exit;

  // retrieve the levels indexes for each visible audio object
  for i:=p^.VisibleObjectsIndexes.First to p^.VisibleObjectsIndexes.Last do
    p^.Objects[i].ComputeViewIndexes(ViewBeginTime, ViewEndTime);
end;

procedure TFrameMixer.SetViewBeginTime(AValue: single);
begin
  if AValue < 0 then AValue := 0;
  if FViewBeginTime = AValue then Exit;
  FViewBeginTime := AValue;

  ComputeLegendTimeBeginPosition;

  if not FScrollBarLocked then ScrollBar1.Position := TimeToAbscissa(FViewBeginTime);

  RecomputeFirstLastAudioIndexView(@FVoiceView);
  RecomputeFirstLastAudioIndexView(@FMusicView);
  RecomputeFirstLastAudioIndexView(@FSoundView);
  if not FRedrawLocked then begin
    RedrawTempImage(@FVoiceView);
    RedrawTempImage(@FMusicView);
    RedrawTempImage(@FSoundView);
  end;
end;

function TFrameMixer.ComputePixelPerPointToFit(aDuration: single): single;
begin
  Result := 1.0;
  if Length(FVoiceView.Objects) > 0 then begin
    while DoAbscissaToTime(PBVoice.ClientWidth, Result) > aDuration do
      Result := Result+Result*ZOOM_COEFF;

    while DoAbscissaToTime(PBVoice.ClientWidth, Result) <= aDuration do
      Result := Result-Result*ZOOM_COEFF;
  end;
end;

function TFrameMixer.TimeToAbscissa(aTime: single): integer;
begin
  Result := Round(aTime*(PixelPerPoint/SLICE_TIME));
end;

function TFrameMixer.AbscissaToTime(aX: integer): single;
begin
  Result :=  DoAbscissaToTime(aX, PixelPerPoint);
  Result := (Trunc(Result*1000) div Trunc(SLICE_TIME*1000))*SLICE_TIME;
end;

function TFrameMixer.DoAbscissaToTime(aX: integer; aPixelPerPoint: single): single;
begin
  Result := aX/aPixelPerPoint*SLICE_TIME;
end;

procedure TFrameMixer.DoZoomOn(aBeginTime, aEndTime: single);
var
  dur: Single;
begin
  dur := aEndTime-aBeginTime;
  if dur <= 0 then exit;

  FRedrawLocked := True;

  PixelPerPoint := ComputePixelPerPointToFit(dur);

  if aBeginTime = 0 then
    ViewBeginTime := 0
  else
    ViewBeginTime := aBeginTime-(AbscissaToTime(PBVoice.ClientWidth)- dur)/2;

  FRedrawLocked := False;


  RecomputeFirstLastAudioIndexView(@FVoiceView);
  RecomputeFirstLastAudioIndexView(@FMusicView);
  RecomputeFirstLastAudioIndexView(@FSoundView);
  ComputeLegendTimeInterval;
  ComputeLegendTimeBeginPosition;
  RedrawTempImage(@FVoiceView);
  RedrawTempImage(@FMusicView);
  RedrawTempImage(@FSoundView);
  Redraw;

  UpdateScrollBar;
end;

procedure TFrameMixer.UpdateScrollBar;
begin
  ScrollBar1.SetParams(Round(FViewBeginTime*100), 0, Round(TotalDuration*100),
       Round(AbscissaToTime(PBVoice.ClientWidth)*100));
end;

procedure TFrameMixer.RedrawGradientImage(p: PMixerView);
begin
  DrawCurveGradient(p^.FGradientBackground, BGRA(90,175,80));
end;

function TFrameMixer.PaintBoxToMixerView(aSender: TObject): PMixerView;
begin
  if aSender = FVoiceView.PB then
    Result := @FVoiceView
  else if aSender = FMusicView.PB then
    Result := @FMusicView
  else if aSender = FSoundView.PB then
    Result := @FSoundView
  else Raise Exception.Create('aSender not found');
end;

procedure TFrameMixer.RedrawTempImage(p: PMixerView);
var i, j, yy, yy1, lastXDrawn, lastYDrawn, newX: integer;
  xx: single;
  yyy, deltaY: single;
  cLow, cBack: TBGRAPixel;
  procedure DrawTrapeze(ax1, ay1, ax2, ay2: integer);
  begin
    yyy := ay1;
    if ax2 > ax1 then
      deltaY := (ay2-ay1)/(ax2-ax1)
    else
      deltaY := 0;
    repeat
      p^.TempImage.VertLine(ax1, Round(yyy), p^.TempImage.Height, cLow, dmSet);
      yyy := yyy+deltaY;
      inc(ax1);
    until ax1 > ax2;
  end;

begin
  lastXDrawn := -1;
  lastYDrawn := p^.TempImage.Height;

  cBack := BGRABlack;
  cLow := BGRAWhite;

  with p^ do begin
    // background
    TempImage.Fill(cBack);

    if Length(Objects) = 0 then exit;
    if VisibleObjectsIndexes.First = -1 then exit;

    for j:=VisibleObjectsIndexes.First to VisibleObjectsIndexes.Last do begin

      with Objects[j] do begin
        if (ViewIndexes.First = -1) or (ViewIndexes.Last = -1) or
           (ViewIndexes.Count < 2) or
           (Length(Levels) < 2) then continue;

        if Muted then begin
          cBack := BGRABlack;
          cLow := BGRA(45,45,45);
          //cHigh := BGRA(45,45,45);
        end else begin
          cBack := BGRABlack;
          cLow := BGRAWhite;
          //cHigh := BGRAWhite;
        end;

        // render audio
        xx := TimeToAbscissa((TimePosMS+(ViewIndexes.First-UsedIndexes.First)*SLICE_TIME_MS)*0.001 - ViewBeginTime);

        ViewRect.Left := Round(xx);
        ViewRect.Top := 0;
        ViewRect.Bottom := TempImage.Height;
        yy := TempImage.Height-Round(Levels[ViewIndexes.First]*TempImage.Height);
        yy1 := TempImage.Height;
        lastXDrawn := Round(xx);
        lastYDrawn := TempImage.Height;

        for i:=ViewIndexes.First to ViewIndexes.Last do begin
          if FPixelPerPoint > 1 then begin
            yy1 := TempImage.Height-Round(Levels[i]*TempImage.Height);

            lastXDrawn := Round(xx);
            newX := Round(xx+FPixelPerPoint);
            //TempImage.Rectangle(lastXDrawn, yy1, newX, TempImage.Height, cLow, cLow, dmSet);
            DrawTrapeze(lastXDrawn, yy1, newX, yy1);
            //TempImage.DrawLine(lastXDrawn, yy, lastXDrawn, yy1, cHigh, True);
            //TempImage.DrawLine(lastXDrawn, yy1, newX, yy1, cHigh, True);

            ViewRect.Right := newX;
            xx := xx+FPixelPerPoint;
            yy := yy1;
          end else begin
            yy := TempImage.Height-Round(Levels[i]*TempImage.Height);
            if yy1 > yy then yy1 := yy;

            newX := Round(xx);
            if lastXDrawn <> newX then begin
              TempImage.VertLine(newX, yy1, TempImage.Height, cLow, dmSet);
              //TempImage.DrawLine(lastXDrawn, lastYDrawn, newX, yy1, cHigh, True);
              //TempImage.DrawLine(lastXDrawn, lastYDrawn+1, newX, yy1+1, cHigh, True);
              lastXDrawn := newX;
              lastYDrawn := yy1;
              yy1 := TempImage.Height;
            end;
            ViewRect.Right := newX;
            xx := xx+FPixelPerPoint;
          end;
        end;
      end;
    end;

    p^.TempImage.BlendImage(0, 0, FGradientBackground, boMultiply);

    if FormMixer.ShowVolumeEnvelope then
    for j:=VisibleObjectsIndexes.First to VisibleObjectsIndexes.Last do begin

      with Objects[j] do begin
        if (ViewIndexes.First = -1) or (ViewIndexes.Last = -1) or
           (ViewIndexes.Count < 2) or
           (Length(Levels) < 2) then continue;

        // render gain curve
        p^.GetPointCurveGain(@Objects[j], 0, lastXDrawn, lastYDrawn);
        for i:=1 to High(CurveGain) do begin
          p^.GetPointCurveGain(@Objects[j], i, newX, yy);
          TempImage.DrawLineAntialias(lastXDrawn-0.5, lastYDrawn-0.5, newX-0.5, yy-0.5, FColorGainCurve, 4);
          lastXDrawn := newX;
          lastYDrawn := yy;
        end;

        // render gain points
        yy1 := ScaleDesignToForm(HALF_WIDTH_POINT_CURVE);
        for i:=0 to High(CurveGain) do begin
          p^.GetPointCurveGain(@Objects[j], i, newX, yy);
          TempImage.FillRect(newX-yy1, yy-yy1, newX+yy1, yy+yy1, FColorGainPoint, dmSet);
        end;

      end;
    end;

  end;
end;

procedure TFrameMixer.DoSelectionChange;
begin
  if FOnSelectionChange <> NIL then
    FOnSelectionChange(Self);
end;

procedure TFrameMixer.LoopSelection(p: PMixerView);
var TimePos: Single;
begin
  if FMouseHandledByUser then exit;
  FMouseHandledByUser := True;

  repeat
    TimePos := AbscissaToTime(p^.PB.ScreenToClient(Mouse.CursorPos).x)+FViewBeginTime;
    if TimePos >= FTimeClickOrigin then begin
      FTimeSelectionRight := TimePos;
      FTimeSelectionLeft := FTimeClickOrigin;
    end
    else begin
      FTimeSelectionRight := FTimeClickOrigin;
      FTimeSelectionLeft := TimePos;
    end;
    Application.ProcessMessages;
    Redraw;
    Sleep(1);
  until FMouseButtonPressed = mbpNone;

  if FTimeSelectionLeft < 0 then FTimeSelectionLeft := 0;
  FTimeSelectionLeft := EnsureRange(FTimeSelectionLeft, 0, TotalDuration);
  if FTimeSelectionRight > TotalDuration then FTimeSelectionRight := TotalDuration;

  DoSelectionChange;

  FMouseHandledByUser := False;
end;

procedure TFrameMixer.LoopMoveView;
var deltaTime, ViewBeginOrigin: Single;
begin
  if FMouseHandledByUser then exit;
  FMouseHandledByUser := True;

  ViewBeginOrigin := ViewBeginTime;
  repeat
    deltaTime := FTimeClickOrigin-AbscissaToTime(PBVoice.ScreenToClient(Mouse.CursorPos).x);
    if deltaTime <> 0 then begin
      ViewBeginTime := ViewBeginOrigin + deltaTime;
      UpdateScrollBar;
      Redraw;
    end;
    Application.ProcessMessages;
    Sleep(1);
  until FMouseButtonPressed = mbpNone;

  FMouseHandledByUser := False;
end;

procedure TFrameMixer.LoopMoveAudioBlock(p: PMixerView);
var deltaTime, timePosOrigin, AmountTimeForShiftView: Single;
  mouseX: LongInt;
  flagRedrawAll: boolean;
begin
  if FMouseHandledByUser then exit;
  if p^.SelectedObject = NIL then exit;

  FMouseHandledByUser := True;
  p^.PB.Cursor := crHandPoint;
  AmountTimeForShiftView := AbscissaToTime(ScaleDesignToForm(7));

  timePosOrigin := AbscissaToTime(p^.PB.ScreenToClient(Mouse.CursorPos).X);
  repeat
    flagRedrawAll := False;
    mouseX := EnsureRange(p^.PB.ScreenToClient(Mouse.CursorPos).x, 0, p^.PB.ClientWidth);
    // shift view if the mouse is at the edge
    if mouseX < ScaleDesignToForm(3) then begin
      flagRedrawAll := True;
      ViewBeginTime := ViewBeginTime - AmountTimeForShiftView;
      timePosOrigin  := timePosOrigin + AmountTimeForShiftView;
    end
    else if mouseX >= p^.PB.ClientWidth-ScaleDesignToForm(3) then begin
      flagRedrawAll := True;
      ViewBeginTime := ViewBeginTime + AmountTimeForShiftView;
      timePosOrigin  := timePosOrigin - AmountTimeForShiftView;
    end;

    mouseX := EnsureRange(p^.PB.ScreenToClient(Mouse.CursorPos).x, 0, p^.PB.ClientWidth);
    deltaTime := AbscissaToTime(mouseX)-timePosOrigin;
    if deltaTime <> 0 then begin
      p^.MoveAudioObject(p^.SelectedObject, deltaTime);
      timePosOrigin := AbscissaToTime(mouseX);
      RecomputeFirstLastAudioIndexView(p);
      if not flagRedrawAll then begin
        RedrawTempImage(p);
        p^.PB.Invalidate;
      end;
    end;

    if flagRedrawAll then Redraw;

    Application.ProcessMessages;
    Sleep(1);
  until FMouseButtonPressed = mbpNone;

  FMouseHandledByUser := False;
  p^.PB.Cursor := crDefault;
end;

procedure TFrameMixer.LoopMoveLeftTrimHandle(p: PMixerView);
var o: PMixerSoundObject;
begin
  if FMouseHandledByUser then exit;
  o := p^.SelectedObject;
  if o = NIL then exit;

  FMouseHandledByUser := True;
  p^.PB.Cursor := crHSplit;

  repeat
    if o^.LeftTrimTo(AbscissaToTime(p^.PB.ScreenToClient(Mouse.CursorPos).x)+ViewBeginTime) then begin
      RecomputeFirstLastAudioIndexView(p);
      RedrawTempImage(p);
      p^.PB.Invalidate;
    end;
    Application.ProcessMessages;
    Sleep(1);
  until FMouseButtonPressed = mbpNone;

  AvoidOverlapping(p);
  RecomputeFirstLastAudioIndexView(p);
  RedrawTempImage(p);
  p^.PB.Invalidate;

  FMouseHandledByUser := False;
end;

procedure TFrameMixer.LoopMoveRightTrimHandle(p: PMixerView);
var o: PMixerSoundObject;
begin
  if FMouseHandledByUser then exit;
  o := p^.SelectedObject;
  if o = NIL then exit;

  FMouseHandledByUser := True;
  p^.PB.Cursor := crHSplit;

  repeat
    if o^.RightTrimTo(AbscissaToTime(p^.PB.ScreenToClient(Mouse.CursorPos).x)+ViewBeginTime) then begin
      RecomputeFirstLastAudioIndexView(p);
      RedrawTempImage(p);
      p^.PB.Invalidate;
    end;
    Application.ProcessMessages;
    Sleep(1);
  until FMouseButtonPressed = mbpNone;

  AvoidOverlapping(p);
  RecomputeFirstLastAudioIndexView(p);
  RedrawTempImage(p);
  p^.PB.Invalidate;

  FMouseHandledByUser := False;
end;

procedure TFrameMixer.LoopMoveGainPoint(p: PMixerView);
var sel: PMixerSoundObject;
  mousePoint: TPoint;
  mouseTimeMS: Int64;
  canDeletePoint: boolean;
  deltaDelete: integer;
begin
  if FMouseHandledByUser then exit;
  sel := p^.SelectedObject;
  if sel = NIL then exit;
  if p^.IndexGainPointUnderMouse = -1 then exit;

  canDeletePoint := (p^.IndexGainPointUnderMouse > 0) and
                    (p^.IndexGainPointUnderMouse < High(p^.SelectedObject^.CurveGain));
  deltaDelete := ScaleDesignToForm(50);
  FMouseHandledByUser := True;
  p^.PB.Cursor := crHandPoint;
  FMouseButtonPressed := mbpLeft;

  repeat
    mousePoint := p^.PB.ScreenToClient(Mouse.CursorPos);
    // check if we delete this point
    if canDeletePoint and
       ((mousePoint.Y < -deltaDelete) or (mousepoint.Y > p^.PB.ClientHeight+deltaDelete)) then begin
      sel^.DeleteGainPoint(p^.IndexGainPointUnderMouse);
      p^.IndexGainPointUnderMouse := -1;
      RedrawTempImage(p);
      p^.PB.Invalidate;
      break;
    end else begin
      mouseTimeMS := Round((AbscissaToTime(mousePoint.X)+ViewBeginTime)*1000);
      mouseTimeMS := EnsureRange(mouseTimeMS, sel^.TimePosMS, sel^.EndTimePosMS);
      mouseTimeMS := mouseTimeMS - sel^.TimePosMS;
      if sel^.MoveGainPoint(p^.IndexGainPointUnderMouse,
                            mouseTimeMS/sel^.DurationMS,
                            (p^.PB.ClientHeight-mousePoint.y)/p^.PB.ClientHeight) then begin
        RedrawTempImage(p);
        p^.PB.Invalidate;
      end;
    end;
    Application.ProcessMessages;
    Sleep(1);
  until FMouseButtonPressed = mbpNone;

  FMouseHandledByUser := False;
end;

procedure TFrameMixer.AddGainPoint(p: PMixerView);
var sel: PMixerSoundObject;
  mousePoint: TPoint;
  mouseTimeMS: Int64;
  i: Integer;
begin
  if FMouseHandledByUser then exit;
  sel := p^.SelectedObject;
  if sel = NIL then exit;

  p^.PB.Cursor := crHandPoint;

  mousePoint := p^.PB.ScreenToClient(Mouse.CursorPos);
  mouseTimeMS := Round((AbscissaToTime(mousePoint.X)+ViewBeginTime)*1000);
  mouseTimeMS := mouseTimeMS - sel^.TimePosMS;

  i := sel^.AddGainPoint(mouseTimeMS/sel^.DurationMS,
                         (p^.PB.ClientHeight-mousePoint.y)/p^.PB.ClientHeight);
  if i = -1 then exit;
  RedrawTempImage(p);
  p^.PB.Invalidate;
  p^.IndexGainPointUnderMouse := i;
  LoopMoveGainPoint(p);
end;

procedure TFrameMixer.ComputeLegendTimeInterval;
  function IncreaseTimeInterval: boolean;
  begin
    Result := False;
    case Trunc(FLegendTimeInterval*10) of   // dixième de s
       1: FLegendTimeInterval :=      2; //  0.2s
       2: FLegendTimeInterval :=      5; //  0.5s
       5: FLegendTimeInterval :=     10; //  1s
      10: FLegendTimeInterval :=     50; //  5s
      50: FLegendTimeInterval :=    300; // 30s
     300: FLegendTimeInterval :=    600; //  1mn
     600: FLegendTimeInterval :=   3000; //  5mn
    3000: FLegendTimeInterval :=  18000; // 30mn
   18000: FLegendTimeInterval :=  36000; //   1h
   36000: FLegendTimeInterval := 180000; //   5h
  180000: FLegendTimeInterval := 360000; //  10h
  360000: FLegendTimeInterval := 864000; //  24h
      else begin
        Result := True;
        exit;
      end;
    end;
    FLegendTimeInterval := FLegendTimeInterval*0.1;
  end;

  function DecreaseTimeInterval: boolean;
  begin
    Result := False;
    case Trunc(FLegendTimeInterval*10) of   // dixième de s
      2: FLegendTimeInterval :=      1;
      5: FLegendTimeInterval :=      2;
     10: FLegendTimeInterval :=      5;
     50: FLegendTimeInterval :=     10;
    300: FLegendTimeInterval :=     50;
    600: FLegendTimeInterval :=    300;
   3000: FLegendTimeInterval :=    600;
  18000: FLegendTimeInterval :=   3000;
  36000: FLegendTimeInterval :=  18000;
  180000: FLegendTimeInterval :=  36000;
  360000: FLegendTimeInterval := 180000;
  864000: FLegendTimeInterval := 360000;
      else begin
        Result := True;
        exit;
      end;
    end;
    FLegendTimeInterval := FLegendTimeInterval*0.1;
  end;
begin
  FLegendTimeInterval := 1.0;

  if TimeToAbscissa(FLegendTimeInterval) < ScaleDesignToForm(100) then begin
    while TimeToAbscissa(FLegendTimeInterval) < ScaleDesignToForm(100) do
      if IncreaseTimeInterval then break;
  end else begin
    while TimeToAbscissa(FLegendTimeInterval) > ScaleDesignToForm(100) do
      if DecreaseTimeInterval then break;
  end;
end;

procedure TFrameMixer.ComputeLegendTimeBeginPosition;
var i: integer;
  viewBTime: double;
begin
  FLegendBeginTimePosition := 0.0;
  viewBTime := ViewBeginTime;
  i := 0;

  while FLegendTimeInterval*(i+1000) < viewBTime do
    inc(i, 1000);

  while FLegendTimeInterval*(i+100) < viewBTime do
    inc(i, 100);

  while FLegendTimeInterval*(i+10) < viewBTime do
    inc(i, 10);

  while FLegendTimeInterval*(i+1) < viewBTime do
    inc(i);

  FLegendBeginTimePosition := FLegendTimeInterval*i;
end;

function TFrameMixer.GetTotalDuration: single;
begin
  Result := FVoiceView.TotalDurationMS*0.001;
  Result := Max(Result, FMusicView.TotalDurationMS*0.001);
  Result := Max(Result, FSoundView.TotalDurationMS*0.001);
end;

procedure TFrameMixer.SetPixelPerPoint(AValue: single);
begin
  AValue := EnsureRange(AValue, MIN_PIXELPERPOINT_VALUE, MAX_PIXELPERPOINT_VALUE);
  if FPixelPerPoint = AValue then Exit;
  FPixelPerPoint := AValue;

  RecomputeFirstLastAudioIndexView(@FVoiceView);
  RecomputeFirstLastAudioIndexView(@FMusicView);
  RecomputeFirstLastAudioIndexView(@FSoundView);

  ComputeLegendTimeInterval;
  if not FRedrawLocked then begin
    RedrawTempImage(@FVoiceView);
    RedrawTempImage(@FMusicView);
    RedrawTempImage(@FSoundView);
    Redraw;
  end;
  UpdateScrollBar;
end;

function TFrameMixer.CreateAllSoundsOnContext(aContext: TALSPlaybackContext;
  aTimePosMS: int64): boolean;
var
  i: Integer;
  function DoCreateSound(aTarget: PMixerSoundObject; aApplyEffect: boolean): boolean;
  var flagDoSetPosition: boolean;
  begin
    with aTarget^ do begin
      ProcessStep := 0;
      AccuSamples := 0;
      SamplesToDo := 0;
      WorkingindexCurveGain := 0;
      g := TALSCustomBoundedFParam.Create(0.0, 1.0, 1.0);

      flagDoSetPosition := (aTimePosMS > TimePosMS) and (aTimePosMS < EndTimePosMS);

      FDontProcessBuffer := True;
      Sound := aContext.AddStream(Filename{, False, @ProcessSoundOnCustomDSPEvent, aTarget});
      FDontProcessBuffer := False;

      if Sound.Error then begin
        ShowMess(SErrorWhileOpeningAudioFile+LineEnding+Filename+
                 LineEnding+Sound.StrError, SClose, mtError);
        Result := False;
        exit;
      end;

      if aApplyEffect then begin
        Sound.Volume.Value := FormMixer.GetAmplifyGain*MaxGainValue;
        Sound.ApplyEffect(Playback.FCompressor);
        Sound.ApplyToneOnAuxSend := True;
      end;

      Sound.SetOnCustomDSP(@ProcessSoundOnCustomDSPEvent, aTarget);
      if flagDoSetPosition then begin
        SamplesToDo := Round(((EndTimePosMS-aTimePosMS)*0.001)*Sound.SampleRate);
        InitGFromPos(aTimePosMS);
        if ApplyFadeAtBeginAndEnd then
          ProcessStep := 1
        else
          ProcessStep := 1;
        Sound.TimePosition := (UsedIndexes.First*SLICE_TIME_MS+
                               (aTimePosMS-TimePosMS)
                              )*0.001;
    {    Log.Info(ExtractFilename(Sound.Filename)+' POSITION:'+Sound.TimePosition.ToString+
                 '  AccuSamples '+AccuSamples.ToString+
                 '  SamplesToDo:'+SamplesToDo.ToString, 1);  }
      end else begin
        Sound.TimePosition := UsedIndexes.First*SLICE_TIME_MS*0.001;
      end;
      aTarget^.Sound.Mute := aTarget^.Muted;
      Result := True;
    end;
  end;
begin
  Result := False;

  for i:=0 to High(FVoiceView.Objects) do begin
    FVoiceView.Objects[i].ApplyFadeAtBeginAndEnd := True;
//    FVoiceView.Objects[i].MaxGainValue := VOICE_MAX_GAIN_VALUE;
    Result := DoCreateSound(@FVoiceView.Objects[i], True);
    if not Result then exit;
  end;

  for i:=0 to High(FMusicView.Objects) do begin
    FMusicView.Objects[i].ApplyFadeAtBeginAndEnd := False;
//    FMusicView.Objects[i].MaxGainValue := MUSIC_MAX_GAIN_VALUE;
    Result := DoCreateSound(@FMusicView.Objects[i], False);
    if not Result then exit;
  end;

  for i:=0 to High(FSoundView.Objects) do begin
    FSoundView.Objects[i].ApplyFadeAtBeginAndEnd := False;
//    FSoundView.Objects[i].MaxGainValue := SOUND_MAX_GAIN_VALUE;
    Result := DoCreateSound(@FSoundView.Objects[i], False);
    if not Result then exit;
  end;
end;

procedure TFrameMixer.DeleteAllSoundsOnContext(aContext: TALSPlaybackContext);
var
  i: Integer;
begin
  for i:=0 to High(FVoiceView.Objects) do
    FVoiceView.Objects[i].g.Free;
  for i:=0 to High(FMusicView.Objects) do
    FMusicView.Objects[i].g.Free;
  for i:=0 to High(FSoundView.Objects) do
    FSoundView.Objects[i].g.Free;
  aContext.DeleteAll;
end;

procedure TFrameMixer.ProcessMixOnProgressEvent(Sender: TALSLoopbackContext;
  aTimePos: double; const aFrameBuffer: TALSLoopbackFrameBuffer;
  var SaveBufferToFile, Cancel: boolean);
begin
  // analyse sample for gain dB
  FGainAnalyzer.AnalyzeSamples(aFrameBuffer);

  FMixingTimePosMS := Round(aTimePos*1000);

  // progressbar
  FormMixer.FrameProgressBar2.Position := aTimePos/TotalDuration;

  if FMixingTimePosMS mod 50 = 0 then Application.ProcessMessages;

  Cancel := FCanceled;
end;

procedure TFrameMixer.AvoidOverlapping(p: PMixerView);
var i: integer;
    nextTimeMS: int64;
begin
  nextTimeMS := 0;
  for i:=0 to High(p^.Objects) do
    with p^.Objects[i] do begin
      TimePosMS := Max(TimePosMS, nextTimeMS);
      nextTimeMS := EndTimePosMS;
    end;
end;

function TFrameMixer.SessionNameForLoadSave: string;
begin
  Result := '[MIX SESSION : '+
            ExtractFilename(FormMain.FrameViewProjectFiles1.SelectedFilename)+
            ']';
end;

function TFrameMixer.SessionFilenameForLoadSave: string;
begin
  Result := IncludeTrailingPathDelimiter(FormMain.FrameViewProjectFiles1.SelectedFilename)+
            MIX_SESSION_FILENAME;
end;

procedure TFrameMixer.LoadSession;
var t: TStringList;
  prop: TProperties;
  temp: string;
begin
  if not FileExists(SessionFilenameForLoadSave) then begin
    FormMixer.InitSessionParamByDefault;
    exit;
  end;

  t := TStringList.Create;
  try
  // load the saved session
    t.LoadFromFile(SessionFilenameForLoadSave);
    if t.Count = 0 then exit;

    prop.Split(t.Strings[0], '|');
    FormMixer.LoadSessionParamFrom(prop);

    temp := '';
    if prop.StringValueOf('VoiceView', temp, '') then
      FVoiceView.LoadFromString(temp, True);
    if prop.StringValueOf('MusicView', temp, '') then
      FMusicView.LoadFromString(temp, False);
    if prop.StringValueOf('SoundView', temp, '') then
      FSoundView.LoadFromString(temp, False);
  finally
    t.Free;
  end;

  // force audio object to not overlaps another
  AvoidOverlapping(@FVoiceView);
  AvoidOverlapping(@FMusicView);
  AvoidOverlapping(@FSoundView);
end;

procedure TFrameMixer.AdjustFont;
begin
  FrameTrackBar1.AdjustFont;
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeight([PBTime, PBVoice, PBMusic, PBSound], FDesignFontHeight);
  ChangeFontHeight([BMute, Label3], FDesignFontHeight-ScaleDesignToForm(2));
{$endif}
{$if defined(LCLCOCOA)}
  BMute.Flat := False;
  ChangeFontColor([BMute], clDefault);
{$endif}
  FrameTrackBar1.AdjustFont;
end;

procedure TFrameMixer.ShowPanelVolume(aY: integer);
var p: TPoint;
begin
  p := ScreenToClient(Mouse.CursorPos);
  p.x := EnsureRange(p.x-PanelVolume.Width div 2, 0, ClientWidth-PanelVolume.Width);
  p.y := EnsureRange(aY, 0, ClientHeight-PanelVolume.Height);
  PanelVolume.Left := p.x;
  PanelVolume.Top := p.y;
  PanelVolume.Visible := True;
  // sets state of mute button
  ToggleSpeedButtonManager1.SetState(BMute, FTargetObjectForCursorVolume^.Muted);
end;

procedure TFrameMixer.HidePanelVolume;
begin
  if PanelVolume.Visible then
    PanelVolume.Visible := False;
end;

procedure TFrameMixer.ProcessVolumeCursorChange(Sender: TObject);
begin
  FTargetObjectForCursorVolume^.MaxGainValue := FrameTrackBar1.PercentValue;
end;

procedure TFrameMixer.ProcessSoundOnCustomDSPEvent(Sender: TALSSound;
  const aBuffer: TALSPlaybackBuffer; aUserData: Pointer);
var
  p: PMixerSoundObject;
  sampleDuration: double;
  sampleindex: integer;
  samplesIn50MS: int64;
//  sndName: string;
  function EndOfBuf: boolean; inline;
  begin
    Result := sampleindex = aBuffer.FrameCount;
  end;
  procedure ApplyGainOnBuf;
  var SquaredGain: single;
  begin
{Log.Info('ApplyGainOnBuf - Begin   sampleindex='+sampleindex.ToString+
         '   SamplesToDo='+p^.SamplesToDo.ToString+
         '   Gain='+p^.g.Value.ToString, 2);  }
    //if (p^.g.State = psNO_CHANGE) and (p^.g.Value = 1.0) then begin
    //  // optimization when the gain is equal to 1.0
    //  while not EndOfBuf and (p^.SamplesToDo > 0) do begin
    //    inc(sampleindex);
    //    dec(p^.SamplesToDo);
    //  end;
    //end else begin
    SquaredGain := p^.MaxGainValue; // * p^.MaxGainValue;
      while not EndOfBuf and (p^.SamplesToDo > 0) do begin
        if aBuffer.UseFloat then
          dsp_AmplifySample_Float(PSingle(aBuffer.Data), sampleindex, aBuffer.ChannelCount, p^.g.Value*SquaredGain)
        else
          dsp_AmplifySample_Smallint(PSmallint(aBuffer.Data), sampleindex, aBuffer.ChannelCount, p^.g.Value*SquaredGain);

        p^.g.OnElapse(sampleDuration);
        inc(sampleindex);
        dec(p^.SamplesToDo);
      end;
    //end;
{Log.Info('ApplyGainOnBuf - End   sampleindex='+sampleindex.ToString+
         '   SamplesToDo='+p^.SamplesToDo.ToString+
         '   Gain='+p^.g.Value.ToString, 2);  }
  end;
  procedure PrepareGainRamp;
  var dur: double;
    beginDeltaSample, endDeltaSample: int64;
  begin
    if p^.WorkingIndexCurveGain > High(p^.CurveGain) then exit;

    if (p^.WorkingIndexCurveGain = 1) and p^.ApplyFadeAtBeginAndEnd and
       (p^.Sound.FrameCount > samplesIn50MS*3)
      then beginDeltaSample := samplesIn50MS
      else beginDeltaSample := 0;

    if (p^.WorkingIndexCurveGain = High(p^.CurveGain)) and p^.ApplyFadeAtBeginAndEnd and
       (p^.Sound.FrameCount > samplesIn50MS*3)
      then endDeltaSample := samplesIn50MS
      else endDeltaSample := 0;

    p^.SamplesToDo := p^.GainPointToSampleIndex(p^.WorkingIndexCurveGain)+beginDeltaSample-
                      (p^.GainPointToSampleIndex(p^.WorkingIndexCurveGain-1)-endDeltaSample)+1;

    dur := (p^.SamplesToDo-1)/Sender.SampleRate;

    p^.g.ChangeTo(p^.GetGainValue(p^.WorkingIndexCurveGain), dur);
{  Log.Info('Prepare Gain ramp from '+p^.g.Value.ToString+'->'+
           p^.GetGainValue(p^.WorkingIndexCurveGain).ToString+
           ' ('+FormatFloat('0.0000', dur)+' s)'+
           '  SamplesToDo='+p^.SamplesToDo.ToString+'/'+p^.Sound.FrameCount.ToString+
           ' AccuSamples='+p^.AccuSamples.ToString, 1);  }
  end;

begin

  p := PMixerSoundObject(aUserData);
  if FDontProcessBuffer or
     (Sender.SampleRate = 0) or
     (aBuffer.FrameCount = 0) or
     (Length(p^.CurveGain) < 2) then exit;

p^.ApplyFadeAtBeginAndEnd := FALSE;

{sndName:=ExtractFilename(Sender.Filename);
p^.ReceivedBufCount := p^.ReceivedBufCount+1; }
  p^.AccuSamples:=p^.AccuSamples+aBuffer.FrameCount;
{Log.AddEmptyLine();
Log.Info('Receive Buffer N°'+p^.ReceivedBufCount.ToString+' from ID '+p^.ID.ToString+' - '+sndName, 0);
Log.Info('frames='+aBuffer.FrameCount.ToString+
         ' AccuSamples='+p^.AccuSamples.ToString+'/'+p^.Sound.FrameCount.ToString+
         ' SamplesToDo='+p^.SamplesToDo.ToString+
         ' WorkingIndexCurveGain='+p^.WorkingIndexCurveGain.ToString);   }
  sampleDuration := 1/Sender.SampleRate;

/////////////////////////////////
// avec les samples
/////////////////////////////////
  samplesIn50MS := Round(0.050*p^.Sound.SampleRate);
  sampleIndex := 0;

  while not {%H-}EndOfBuf do begin

    if p^.SamplesToDo > 0 then
      ApplyGainOnBuf;
    if EndOfBuf then exit;

    if p^.ApplyFadeAtBeginAndEnd then begin
     // with fade
    end else begin
      // without fade
      case p^.ProcessStep of
        0: begin
           // Log.Info(sndName+' FIRST STEP gain from '+p^.GetGainValue(0).ToString+' to '+p^.GetGainValue(1).ToString, 1);
            // prepare the first gain ramp
            p^.g.Value := p^.GetGainValue(0);
            p^.WorkingIndexCurveGain := 1;
            PrepareGainRamp;
            inc(p^.ProcessStep);
        end;

        1: begin
          if p^.WorkingIndexCurveGain < High(p^.CurveGain) then begin
            // NEXT POINT
            p^.WorkingIndexCurveGain := p^.WorkingIndexCurveGain+1;
           { Log.Info(sndName+' NEXT POINT   IndexPointGain '+
                     p^.WorkingIndexCurveGain.ToString+'/'+High(p^.CurveGain).ToString, 1);}
            PrepareGainRamp;
          end else begin
            p^.SamplesToDo := aBuffer.FrameCount - sampleIndex;
           { Log.Info(sndName+' ADJUST samples on END ??'+p^.SamplesToDo.ToString+' samples', 1); }
          end;
        end;

      end;//case
    end;
  end;//while
end;

procedure TFrameMixer.SaveSession;
var t: TStringList;
  prop: TProperties;
begin
  prop.Init('|');
  FormMixer.SaveSessionParamTo(prop);

  prop.Add('VoiceView', FVoiceView.SaveToString);
  if Length(FMusicView.Objects) > 0 then
    prop.Add('MusicView', FMusicView.SaveToString);
  if Length(FSoundView.Objects) > 0 then
    prop.Add('SoundView', FSoundView.SaveToString);

  t := TStringList.Create;
  try
    t.Add(prop.PackedProperty);
    t.SaveToFile(SessionFilenameForLoadSave);
  finally
    t.Free;
  end;
end;

function TFrameMixer.MixToFile: boolean;
var
  i: integer;
  workingTimeMS: int64;
  flag: Boolean;
  mixTime: Extended;
begin
  StopPreview;
  HidePanelVolume;
  Result := False;
  if Length(FVoiceView.Objects) +
     Length(FMusicView.Objects) +
     Length(FSoundView.Objects) = 0 then exit;

  if not Project.OutputFolderExists then exit;

  // we must release the current playback context before create a loop back context...
  Playback.Free;

  // Create the loopback context and its effects
  Playback.CreateLoopback;
  try
    if Playback.FLoopBackContext.Error then begin
      ShowMess(SErrorWhileOpeningLoopbackDevice+LineEnding+
               Playback.FLoopBackContext.StrError, SClose, mtError);
      exit;
    end;

    if not Playback.FCompressor.Ready then
      ShowMess(SCompressorDontWork, SClose, mtWarning);
    if not Playback.FBassBoostEqualizer.Ready then
      ShowMess(SBassBoostDontWork, SClose, mtWarning);
    if Playback.FErrorOnChainEffect then
      ShowMess(SChainEffectDontWorkWell, SClose, mtError);

    // Ask to save the mixing to a wav file
    FOutputFile := Project.ProjectOutputFolder+Trim(FormMixer.Label8.Caption);
    FOutputFile := ChangeFileExt(FOutputFile, '.wav');
    if not Playback.FLoopBackContext.PrepareSavingToFile(FOutputFile,
           ALSMakeFileFormat(ALSound.SF_FORMAT_WAV, ALSound.SF_FORMAT_PCM_16)) then begin
      ShowMess(SErrorWhilePreparingOutputMixFile,
               SClose, mtError);
      exit;
    end;

    FDontProcessBuffer := False;
    // create all sounds and connect them (only Voice) to the effect's chain
    if not CreateAllSoundsOnContext(Playback.FLoopBackContext, 0) then exit;

    // mute some effect according to the effect's buttons state
    Playback.FCompressor.Mute := not FormMixer.ToggleSpeedButtonManager1.Checked[FormMixer.BCompressor];
    Playback.FBassBoostEqualizer.Mute := not FormMixer.ToggleSpeedButtonManager1.Checked[FormMixer.BBassBoost];

    FMixingSoundIndex := 0;
    FMixingTimePosMS := 0;

    Playback.FLoopBackContext.TimeSlice := 0.001;
    Playback.FLoopBackContext.OnProgress := @ProcessMixOnProgressEvent;
    FCanceled := False;
    FormMixer.ShowMixingProgressPanel;

    // prepare gaindB computation
    FGainAnalyzer.InitGainAnalysis(Playback.FLoopBackContext.SampleRate, 1, 32768);

    // GO
    Playback.FLoopBackContext.BeginOfMix;

    repeat
      flag := False;
      mixTime := 10000000.0;

      for i:=0 to High(FVoiceView.Objects) do
        if FVoiceView.Objects[i].HaveWorkingTime(FMixingTimePosMS, workingTimeMS) then begin
          flag := True;
          mixTime := Min(mixTime, (workingTimeMS-FMixingTimePosMS)*0.001);
        end;

      for i:=0 to High(FMusicView.Objects) do
        if FMusicView.Objects[i].HaveWorkingTime(FMixingTimePosMS, workingTimeMS) then begin
          flag := True;
          mixTime := Min(mixTime, (workingTimeMS-FMixingTimePosMS)*0.001);
        end;

      for i:=0 to High(FSoundView.Objects) do
        if FSoundView.Objects[i].HaveWorkingTime(FMixingTimePosMS, workingTimeMS) then begin
          flag := True;
          mixTime := Min(mixTime, (workingTimeMS-FMixingTimePosMS)*0.001);
        end;

      if flag then
        Playback.FLoopBackContext.Mix(mixTime)
      else begin
        flag := False;
        for i:=0 to High(FVoiceView.Objects) do
          flag := flag or (FVoiceView.Objects[i].Sound.State = ALS_PLAYING);
        for i:=0 to High(FMusicView.Objects) do
          flag := flag or (FMusicView.Objects[i].Sound.State = ALS_PLAYING);
        for i:=0 to High(FSoundView.Objects) do
          flag := flag or (FSoundView.Objects[i].Sound.State = ALS_PLAYING);
        if flag then
          Playback.FLoopBackContext.Mix(0.010);
      end;

      Application.ProcessMessages;

    until not flag or FCanceled;
    Playback.FLoopBackContext.EndOfMix;

    FDontProcessBuffer := True;

    Result := not Playback.FLoopBackContext.MixingError;

    if not Result and not FCanceled then
      ShowMess(SMixingError+LineEnding+Playback.FLoopBackContext.MixingStrError,
               SClose, mtError);
    DeleteAllSoundsOnContext(Playback.FLoopbackContext);
  finally
    Playback.FreeLoopback;
    // recreate the playback context
    Playback.Create(programOptions.PlaybackDeviceIndex);
  end;
end;

procedure TFrameMixer.CancelMix;
begin
  FCanceled := True;
end;

function TFrameMixer.Canceled: boolean;
begin
  Result := FCanceled;
end;

procedure TFrameMixer.StartPreview;
var
  i, lastXCursorPlay, newXCursorPlay: integer;
  returnedWorkingTimeMS, nextWorkingTimeMS: int64;
  flag: Boolean;
  timeOrigin: TDateTime;
  procedure InvalidateView;
  begin
    FVoiceView.PB.Invalidate;
    if Length(FMusicView.Objects) > 0 then FMusicView.PB.Invalidate;
    if Length(FSoundView.Objects) > 0 then FSoundView.PB.Invalidate;
  end;

begin
  if FPreviewIsRunning then exit;

  FCursorPlayTimeMS := Round((FTimeSelectionLeft)*1000);

  FDontProcessBuffer := False;
  // create all sounds and connect them to the effect's chain
  if not CreateAllSoundsOnContext(PlayBack.FPlaybackContext, FCursorPlayTimeMS) then begin
    PlayBack.FPlaybackContext.DeleteAll;
    exit;
  end;

  FPreviewIsRunning := True;
  // mute some effect according to the effect's buttons state
  Playback.FCompressor.Mute := not FormMixer.ToggleSpeedButtonManager1.Checked[FormMixer.BCompressor];
  Playback.FBassBoostEqualizer.Mute := not FormMixer.ToggleSpeedButtonManager1.Checked[FormMixer.BBassBoost];

  lastXCursorPlay :=TimeToAbscissa(FTimeSelectionLeft-ViewBeginTime);
  timeOrigin := Now();
  repeat
    FCursorPlayTimeMS := FCursorPlayTimeMS + MilliSecondsBetween( Now(), timeOrigin );
    timeOrigin := Now();

    flag := False;
    nextWorkingTimeMS := High(Int64);

    for i:=0 to High(FVoiceView.Objects) do
      if FVoiceView.Objects[i].HaveWorkingTime(FCursorPlayTimeMS, returnedWorkingTimeMS) then begin
        flag := True;
        nextWorkingTimeMS := Min(nextWorkingTimeMS, returnedWorkingTimeMS);
      end;

    for i:=0 to High(FMusicView.Objects) do
      if FMusicView.Objects[i].HaveWorkingTime(FCursorPlayTimeMS, returnedWorkingTimeMS) then begin
        flag := True;
        nextWorkingTimeMS := Min(nextWorkingTimeMS, returnedWorkingTimeMS);
      end;

    for i:=0 to High(FSoundView.Objects) do
      if FSoundView.Objects[i].HaveWorkingTime(FCursorPlayTimeMS, returnedWorkingTimeMS) then begin
        flag := True;
        nextWorkingTimeMS := Min(nextWorkingTimeMS, returnedWorkingTimeMS);
      end;

    if not flag then begin
      for i:=0 to High(FVoiceView.Objects) do
        flag := flag or (FVoiceView.Objects[i].Sound.State = ALS_PLAYING);
      for i:=0 to High(FMusicView.Objects) do
        flag := flag or (
            (FMusicView.Objects[i].Sound.State = ALS_PLAYING) and not FMusicView.Objects[i].Muted);
      for i:=0 to High(FSoundView.Objects) do
        flag := flag or (
            (FSoundView.Objects[i].Sound.State = ALS_PLAYING) and not FSoundView.Objects[i].Muted);
    end;

    // invalidate the view only if the position of the cursor play changed
    newXCursorPlay := TimeToAbscissa(FCursorPlayTimeMS*0.001-ViewBeginTime);
    if lastXCursorPlay <> newXCursorPlay then begin
      InvalidateView;
      lastXCursorPlay := newXCursorPlay;
    end;

    Application.ProcessMessages;

   { if nextWorkingTimeMS > timeOrigin then
      if MilliSecondsBetween( timeOrigin, nextWorkingTimeMS) > 10 then  }
        Sleep(1);

  until not flag or not FPreviewIsRunning;

  FDontProcessBuffer := True;

  FPreviewIsRunning := False;
  DeleteAllSoundsOnContext(PlayBack.FPlaybackContext);
  FCursorPlayTimeMS := -1;
  InvalidateView;
end;

procedure TFrameMixer.StopPreview;
begin
  FPreviewIsRunning := False;
end;

function TFrameMixer.PreviewIsRunning: boolean;
begin
  Result := FPreviewIsRunning;
end;

procedure TFrameMixer.SetPreviewCompressor(aOn: boolean);
begin
  if FPreviewIsRunning then
    Playback.FCompressor.Mute := not aOn;
end;

procedure TFrameMixer.SetPreviewBassBoost(aOn: boolean);
begin
  if FPreviewIsRunning then
    Playback.FBassBoostEqualizer.Mute := not aOn;
end;

procedure TFrameMixer.SetPreviewBassBoostGain(aValue: single);
begin
  if FPreviewIsRunning then begin
    Playback.FBassBoostEqualizerProp.LowGain  := aValue;
    Playback.FBassBoostEqualizer.UpdateParameters(Playback.FBassBoostEqualizerProp);
  end;
end;

procedure TFrameMixer.SetPreviewAmplifyGain(aValue: single);
var
  i: Integer;
begin
  if FPreviewIsRunning then
    for i:=0 to High(FVoiceView.Objects) do
      FVoiceView.Objects[i].Sound.Volume.Value := aValue;
end;

constructor TFrameMixer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FVoiceView.TempImage := TBGRABitmap.Create(1, 1);
  FVoiceView.FGradientBackground := TBGRABitmap.Create(1, 1);
  FMusicView.TempImage := TBGRABitmap.Create(1, 1);
  FMusicView.FGradientBackground := TBGRABitmap.Create(1, 1);
  FSoundView.TempImage := TBGRABitmap.Create(1, 1);
  FSoundView.FGradientBackground := TBGRABitmap.Create(1, 1);

  // volume cursor
  FrameTrackBar1 := TFrameTrackBar.Create(Self);
  FrameTrackBar1.Parent := Panel1;
  FrameTrackBar1.Align := alClient;
  FrameTrackBar1.Init(trVertical, True, False, True);
  FrameTrackBar1.FormatDisplayValue(100.0, '', '%');
  FrameTrackBar1.OnChange := @ProcessVolumeCursorChange;

  ToggleSpeedButtonManager1 := TToggleSpeedButtonManager.Create;
  ToggleSpeedButtonManager1.SetImageIndexes(5, -1);
  ToggleSpeedButtonManager1.SetActivatedColors(clBtnFace, clBlack);
  ToggleSpeedButtonManager1.SetDeactivatedColors(clBtnFace, clBlack);
  ToggleSpeedButtonManager1.Add(BMute, False);

  // text style for TextRect -> filename for each audio object
  FTextStyle.Alignment := taCenter;
  FTextStyle.Layout := tlTop;
  FTextStyle.Clipping := True;
  FTextStyle.Opaque := False;
  FTextStyle.Wordbreak := True;
 // FTextStyle.EndEllipsis := True;

  FColorGainPoint := BGRA(255,255,255);
  FColorGainCurve := BGRA(0,255,255);

  FGainAnalyzer := TComputeGain89dB.Create;
  DoReset;
end;

destructor TFrameMixer.Destroy;
begin
  FVoiceView.TempImage.Free;
  FVoiceView.FGradientBackground.Free;
  FMusicView.TempImage.Free;
  FMusicView.FGradientBackground.Free;
  FSoundView.TempImage.Free;
  FSoundView.FGradientBackground.Free;
  ToggleSpeedButtonManager1.Free;
  FGainAnalyzer.Free;
  inherited Destroy;
end;

procedure TFrameMixer.EraseBackground(DC: HDC);
begin
end;

procedure TFrameMixer.ClearView;
begin
  HidePanelVolume;
  DoReset;
  Redraw;
  Application.ProcessMessages;
end;

procedure TFrameMixer.Redraw;
begin
  PBTime.Invalidate;
  PBVoice.Invalidate;
  PBMusic.Invalidate;
  PBSound.Invalidate;
end;

procedure TFrameMixer.ReconstructTempImageAndRedraw;
begin
  RedrawTempImage(@FVoiceView);
  RedrawTempImage(@FMusicView);
  RedrawTempImage(@FSoundView);
  Redraw;
end;

procedure TFrameMixer.ClearPBHint;
begin
  PBVoice.Hint := '';
  PBMusic.Hint := '';
  PBSound.Hint := '';
end;

procedure TFrameMixer.InitFromViewProjectFiles(const Filenames: TStringArray);
var timePosMS: int64;
  i: Integer;
begin
  ClearView;

  if Length(Filenames) = 0 then exit;

  FVoiceView.Init(PBVoice);
  FMusicView.Init(PBMusic);
  FSoundView.Init(PBSound);

  timePosMS := 0;
  for i:=0 to High(Filenames) do begin
    with FVoiceView.AddObject(Filenames[i], timePosMS)^ do begin
     MaxGainValue := VOICE_MAX_GAIN_VALUE;
     Muted := False;
    end;
    timePosMS := timePosMS + FVoiceView.Objects[i].DurationMS;
  end;

  LoadSession;

  ViewAll;
  Redraw;
end;

procedure TFrameMixer.AdjustTrackHeight;
var h: Integer;
begin
  h := ClientHeight - PBTime.Height - ScrollBar1.Height;
  Splitter2.Top := ClientHeight - ScrollBar1.Height - h div 4;
  Splitter1.Top := ScrollBar1.Height + h div 2;
end;

function TFrameMixer.IsEmpty: boolean;
begin
  Result := (Length(FVoiceView.Objects) = 0) and
            (Length(FMusicView.Objects) = 0) and
            (Length(FSoundView.Objects) = 0);
end;

procedure TFrameMixer.SetCursorToBegin;
begin
  SetCursorAt(0);
  ViewBeginTime := 0;
end;

procedure TFrameMixer.SetCursorToEnd;
begin
  SetCursorAt(TotalDuration);
end;

procedure TFrameMixer.SetCursorAt(aTimepos: single);
begin
  FTimeSelectionLeft := EnsureRange(aTimepos, 0.0, TotalDuration);
  FTimeSelectionRight := FTimeSelectionLeft;
  Redraw;
  DoSelectionChange;
end;

procedure TFrameMixer.ViewAll;
begin
  if IsEmpty then exit;

  DoZoomOn(0, TotalDuration);
  if PixelPerPoint > 4 then PixelPerPoint := 4;
  Redraw;
end;

procedure TFrameMixer.ZoomOnSelection;
begin
  if IsEmpty then exit;
  if not HaveSelection then exit;

  DoZoomOn(FTimeSelectionLeft, FTimeSelectionRight);
  Redraw;
end;

function TFrameMixer.HaveSelection: boolean;
begin
  Result := FTimeSelectionLeft < FTimeSelectionRight;
end;

function TFrameMixer.HaveMusic: boolean;
begin
  Result := Length(FMusicView.Objects) > 0;
end;

function TFrameMixer.HaveSound: boolean;
begin
  Result := Length(FSoundView.Objects) > 0;
end;

procedure TFrameMixer.ProcessKeyDown(Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE: begin
      if FPreviewIsRunning then
        StopPreview
      else
        StartPreview;
      end;

    VK_A: begin
      if ssAlt in Shift then ViewAll;
    end;
  end;
end;

end.

