unit frame_viewaudio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, LCLType, ExtCtrls, Buttons, StdCtrls,
  BGRABitmap, BGRABitmapTypes,
  ALSound, Types, u_common, u_utils;


const
  SLICE_TIME_MS = 10;
  SLICE_TIME = SLICE_TIME_MS*0.001;
  ZOOM_COEFF = 0.3;
  MAX_PIXELPERPOINT_VALUE = 30.0;
  MIN_PIXELPERPOINT_VALUE = 0.00005;

type

  { TFrameViewAudio }

  TFrameViewAudio = class(TFrame)
    PBTime: TPaintBox;
    Panel2: TPanel;
    PB: TPaintBox;
    ScrollBar1: TScrollBar;
    Timer1: TTimer;
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, {%H-}Y: Integer);
    procedure PBMouseLeave(Sender: TObject);
    procedure PBMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, {%H-}Y: Integer);
    procedure PBMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PBMouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PBPaint(Sender: TObject);
    procedure PBResize(Sender: TObject);
    procedure PBTimePaint(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; {%H-}ScrollCode: TScrollCode; var {%H-}ScrollPos: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    FFilename: string;
    FSampleRate: integer;
    FSound: TALSSound;
    FPixelPerPoint: single;
    FLevels: TSingleArray; // levels [0..1]
    FImage,
    FGradientBackground,
    FMarkerImage: TBGRABitmap;
    FViewBeginIndex: integer;
    FScrollBarLocked: boolean;
    FVisibleIndexes,
    FSelectedIndexes: TIndexInterval;
    function GetFramesCount: int64;
    function TimeToLevelIndex(aTime: single): integer;
    function LevelIndexToTime(aIndex: integer): single;
    procedure RedrawGradientImage;
    procedure RedrawMarkerImage;
    procedure RedrawTempImage;
    procedure SetPixelPerPoint(AValue: single);
    procedure SetViewBeginIndex(AValue: integer);
    procedure RecomputeFirstLastIndexView;
  private
    FLegendBeginTimePosition,
    FLegendTimeInterval: double;
    procedure ComputeLegendTimeInterval;
    procedure ComputeLegendTimeBeginPosition;
  private
    FMouseHandledByUser: boolean;
    FMouseButtonPressed: TMouseButtonPressed;

    FIndexClickOrigin: integer;

    FOnSelectionChange: TNotifyEvent;
    procedure DoSelectionChange;
    procedure DoLoopMoveLeftSelectionBoundary;
    procedure DoLoopMoveRightSelectionBoundary;
    procedure LoopSelection;
    procedure LoopMoveView;
  private
    FFrameCountInSlice: integer;
    function ViewBeginTime: single;
    function GetTotalDuration: single;
    function ComputePixelPerPointToFit(aDeltaIndex: integer): single;
    function TimeToAbscissa(aTime: single): integer;
    function AbscissaToTime(aX: integer): single;
    function DoAbscissaToTime(aX: integer; aPixelPerPoint: single): single;
    function AbscissaToIndex(aX: integer): integer;
    function DoAbscissaToIndex(aX: integer; aPixelPerPoint: single): integer;
    function IndexToAbscissa(aIndex: integer): integer;
    function TimeToIndex(aTime: single): integer;
    function IndexToTime(aIndex: integer): single;
    function TimeToFrameIndex(aTime: single): int64;
    procedure DoZoomOn(aBeginIndex, aEndIndex: integer);
    procedure UpdateScrollBar;
    procedure DoLoad;
    function ComputeLevelFromAudioBuffer(const aBuf: TALSPlaybackBuffer): single;
  private
    FUserMarks: TUserMarks;
    FVisibleUserMarksIndexes: TIndexInterval;
    function GetViewEndTime: single;
    function ViewEndIndex: integer;
    procedure RecomputeVisibleUserMarksIndexes;
    function GetUserMarksIndexesInSelection: TIndexArray;
    procedure AdjustFont;
  public
    constructor Create( TheOwner: TComponent ); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;
    procedure Redraw;

    // True if the view don't have audio to show/edit
    function IsEmpty: boolean;
    function HaveSample: boolean;
    function CursorIsAtBegin: boolean;
    function CursorIsAtEnd: boolean;

    // load audio file in view. Return True if success
    function LoadSound(const aFilename: string): boolean;
    procedure ReloadSound;
    function ReloadPart(aFrameIndexBegin, aFrameIndexEnd: int64): boolean;
    function ReloadPartFromFrameToEnd(aFrameIndex: int64): boolean;

    // Empty the view and delete the actual temp record file
    procedure Clear;

    function HaveSelection: boolean;
    procedure SetSelection(aBeginIndex, aEndIndex: integer);
    procedure SelectNone;
    procedure SelectAll;
    procedure SetCursorToBegin;
    procedure SetCursorToEnd;
    procedure SetCursorToPos(aIndex: integer);
    procedure OptimizeViewToShowCursorPos;
    procedure OptimizeViewToShowSelection;

    function LevelIndexToFrameIndex(aLevelIndex: integer): int64;
    function FrameIndexToLevelIndex(aFrameIndex: int64): integer;

    function DoCutPart(aIndexBegin, aIndexEnd: integer): boolean;
    function DoInsertPart(const aNewDataFile: string; aFrameIndex: int64): boolean;
    function DoSilenceOnPart(aFrameIndexBegin, aFrameIndexEnd: int64): boolean;
    function DoInsertSilence(aFrameIndex: int64): boolean;
    function ProgramOptionsSilenceToSliceCount: integer;

// USER MARKS
    function ThereIsUserMarkInSelection: boolean;
    function GetUserMarksInSelection: TUserMarks;
    procedure AddUserMarkAtCursor;
    procedure DeleteUserMarkAtFrameIndex(aFrameIndex: int64);
    procedure AddUserMarkAtFrameIndex(aFrameIndex: int64);
    procedure DeleteUserMarksInSelection(aDoSaveForUndo: boolean);
    procedure DeleteAllUserMarks(aDoSaveForUndo: boolean);
    procedure MergeUserMarks(const aUserMarks: TUserMarks);
    procedure ReplaceUserMarksBy(const aUserMarks: TUserMarks);
    procedure ShiftUserMarksFromIndex(aStartIndex, aDelta: integer);
    //procedure ShiftUserMarksForFromIndex(aStartIndex, aDelta: integer);
    procedure LoadUserMarksFromFile(const aFileName: string);

    procedure SilenceOnSelection;
    procedure InsertSilenceAtCursorPos;
    procedure CutSelection;
    procedure Undo;
    procedure Redo;

    procedure ViewAll;
    procedure ZoomOnSelection;

    procedure Play;
    procedure Pause;
    procedure Stop;

    // capture audio for an addition
    function CaptureAddition: boolean;
    procedure SetPlaybackVolume(aVolume: single);

    procedure ProcessKeyDown(Key: Word; Shift: TShiftState);

    property UserMarks: TUserMarks read FUserMarks;
    property PixelPerPoint: single read FPixelPerPoint write SetPixelPerPoint;

    property ViewBeginIndex: integer read FViewBeginIndex write SetViewBeginIndex;
    property ViewEndTime: single read GetViewEndTime;
    property TotalDuration: single read GetTotalDuration;

    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property SelectedIndexes: TIndexInterval read FSelectedIndexes;

    property Filename: string read FFilename write FFilename;
    property FramesCount: int64 read GetFramesCount;
    property SampleRate: integer read FSampleRate;
  end;

implementation
uses als_dsp_utils, u_audio_utils, u_program_options, form_main,
  u_main_undoredo, u_userdialogs, form_audiorecording, u_resource_string,
  u_crossplatform, Graphics, Dialogs, Math, utilitaire_fichier, u_logfile,
  utilitaire_bgrabitmap;

{$R *.lfm}

const
  DECIBEL_MAX_ABSOLUTE_VALUE = -ALS_DECIBEL_MIN_VALUE;

{ TFrameViewAudio }

procedure TFrameViewAudio.PBResize(Sender: TObject);
begin
  FGradientBackground.SetSize(PB.ClientWidth, PB.ClientHeight);
  RedrawGradientImage;
  RedrawMarkerImage;

  FImage.SetSize(PB.ClientWidth, PB.ClientHeight);
  RedrawTempImage;

  Redraw;
end;

procedure TFrameViewAudio.PBTimePaint(Sender: TObject);
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

    ybase :=PBTime.ClientHeight-2;
    yLegend := 0; // ybase-Self.Font.Height;
    yTimeMark := yLegend+Font.Height;

    // axis
    Pen.Color := Font.Color;
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

    // line to figure out audio length
    if ViewBeginIndex < High(FLevels) then begin
      xx := Min(PBTime.ClientWidth, IndexToAbscissa(High(FLevels)+1-ViewBeginIndex));
      Pen.Color := $000080FF;
      Line(0, ybase-2, xx, ybase-2);
      Line(0, ybase-1, xx, ybase-1);
      Line(0, ybase, xx, ybase);
      Line(0, ybase+1, xx, ybase+1);
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Line(0, ybase-1, xx, ybase-1);
      Line(0, ybase, xx, ybase);
      Pen.Style := psSolid;
    end;
  end;
end;

procedure TFrameViewAudio.ScrollBar1Scroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  FScrollBarLocked := True;
  ViewBeginIndex := ScrollBar1.Position;
  FScrollBarLocked := False;
  Redraw;
end;

procedure TFrameViewAudio.Timer1Timer(Sender: TObject);
begin
  if FSound.State = ALS_STOPPED then begin
    FSound.Kill;
    FSound := NIL;
    Timer1.Enabled := False;
  end
  else if (FSound.State = ALS_PLAYING) and
          (FSelectedIndexes.Count > 1) and
          (FSound.TimePosition >= LevelIndexToTime(FSelectedIndexes.Last)) then
         FSound.Stop;
  Redraw;
end;

function TFrameViewAudio.GetFramesCount: int64;
begin
  Result := GetAudioFileFramesCount(Filename);
end;

function TFrameViewAudio.TimeToLevelIndex(aTime: single): integer;
begin
  aTime := EnsureRange(aTime, 0.0, TotalDuration);
  Result := EnsureRange(Round(aTime/SLICE_TIME), 0, High(FLevels));
end;

function TFrameViewAudio.LevelIndexToTime(aIndex: integer): single;
begin
  Result := SLICE_TIME*aIndex;
end;

procedure TFrameViewAudio.PBPaint(Sender: TObject);
var xx, i: integer;
  t: TBGRABitmap;
  //debugS: string;
begin

  //FImage.Draw(PB.Canvas, 0, 0, True);
  t := FImage.FilterBlurRadial(1, 1, rbNormal);

  // selection
  if FSelectedIndexes.First < FSelectedIndexes.Last then begin
    t.Rectangle(IndexToAbscissa(FSelectedIndexes.First-FViewBeginIndex), 0,
                IndexToAbscissa(FSelectedIndexes.Last-FViewBeginIndex), t.Height,
                BGRA(255,255,255,35), BGRA(255,255,255,35), dmDrawWithTransparency);
    xx := IndexToAbscissa(FSelectedIndexes.Last-FViewBeginIndex);
    t.VertLine(xx, 0, t.Height, BGRA(255,255,0), dmSet);
  end;

  // user marks
  if FVisibleUserMarksIndexes.First <> -1 then begin
    for i:=FVisibleUserMarksIndexes.First to FVisibleUserMarksIndexes.Last do begin
      if FUserMarks[i] = 0
        then xx := 0
        else xx := TimeToAbscissa(FUserMarks[i]-ViewBeginTime);
      t.PutImage(xx-FMarkerImage.Width div 2, t.Height-FMarkerImage.Height, FMarkerImage, dmDrawWithTransparency);
    end;
  end;

  // cursor and left selection boundary
  if Length(FLevels) > 0 then begin
    xx := IndexToAbscissa(FSelectedIndexes.First-FViewBeginIndex);
    t.VertLine(xx, 0, t.Height, BGRA(255,255,0), dmSet);
  end;

//  FGradientBackground.Draw(PB.Canvas, 0, 0, True);
  t.Draw(PB.Canvas, 0, 0, False);
  t.Free;


  // cursor play
  if (FSound <> NIL) and (PixelPerPoint <> 0) then begin
    xx := TimeToAbscissa(FSound.TimePosition-IndexToTime(FViewBeginIndex));
    with PB.Canvas do begin
      Pen.Color := RGBToColor(0,255,255);
      Line(xx, 0, xx, PB.ClientHeight);
    end;
  end;

{  PB.Canvas.Font.Color := clWhite;
  PB.Canvas.Brush.Style := bsClear;
  debugS := 'PixelPerPoint '+FPixelPerPoint.ToString+Lineending+
            ' LegendTimeInterval '+FormatFloat('0.0000', FLegendTimeInterval)+Lineending+
            ' IndexView ['+FVisibleIndexes.First.ToString+'..'+FVisibleIndexes.Last.ToString+']';
  PB.Canvas.TextOut(0,0,debugS);
  debugS := 'Selection indexes: '+FSelectedIndexes.First.ToString+' à '+FSelectedIndexes.Last.ToString+
            '   FFrameCountInSlice='+FFrameCountInSlice.ToString;
  PB.Canvas.TextOut(0,15,debugS);
  debugS := 'ScrollBar: Position '+ScrollBar1.Position.ToString+
            '  Min '+ScrollBar1.Min.ToString+
            '  Max '+ScrollBar1.Max.ToString+
            '  PageSize '+ScrollBar1.PageSize.ToString;
  PB.Canvas.TextOut(0,30,debugS);
  debugS := 'ViewBeginIndex: '+ViewBeginIndex.ToString;
  PB.Canvas.TextOut(0,45,debugS);
  debugS := 'Cursor time: '+TimeToString(IndexToTime(FSelectedIndexes.First),3);
  PB.Canvas.TextOut(0,60,debugS);  }
end;

procedure TFrameViewAudio.PBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var indexesOverMouse: TIndexInterval;
begin
  FIndexClickOrigin := AbscissaToIndex(X)+ViewBeginIndex;

  indexesOverMouse.First := AbscissaToIndex(X-ScaleDesignToForm(3))+ViewBeginIndex;
  indexesOverMouse.Last := AbscissaToIndex(X+ScaleDesignToForm(3))+ViewBeginIndex;

  if Button = mbLeft then begin
    FMouseButtonPressed := mbpLeft;

    if not indexesOverMouse.Contain(FSelectedIndexes.First) and
       not indexesOverMouse.Contain(FSelectedIndexes.Last) then begin

         if Length(FLevels) = 0 then
           FSelectedIndexes.First := 0
         else
           FSelectedIndexes.First := FIndexClickOrigin;
         if FSelectedIndexes.First > High(FLevels)+1 then
           FSelectedIndexes.First := High(FLevels)+1;
         FSelectedIndexes.Last := FSelectedIndexes.First;
         DoSelectionChange;
         PB.Invalidate;
    end;
  end;

  if Button = mbRight then begin
    FMouseButtonPressed := mbpRight;
    PB.Cursor := crSizeWE;
  end;

  if FSound <> NIL then
    if FSound.State = ALS_PLAYING then
      FSound.TimePosition := IndexToTime(FIndexClickOrigin);
end;

procedure TFrameViewAudio.PBMouseLeave(Sender: TObject);
begin
  if FMouseHandledByUser then
    FMouseButtonPressed := mbpNone;
end;

procedure TFrameViewAudio.PBMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var thresholdDone: boolean;
  indexesOverMouse: TIndexInterval;
begin
  indexesOverMouse.First := AbscissaToIndex(X-ScaleDesignToForm(3))+ViewBeginIndex;
  indexesOverMouse.Last := AbscissaToIndex(X+ScaleDesignToForm(3))+ViewBeginIndex;

  if FMouseButtonPressed <> mbpNone then begin
    thresholdDone := TimeToAbscissa(Abs(IndexToTime(FIndexClickOrigin)-(AbscissaToTime(X)+ViewBeginTime))) > ScaleDesignToForm(5);
    case FMouseButtonPressed of
      mbpLeft: begin
        if HaveSelection then begin
          // check if mouse is over selection boundaries
          if indexesOverMouse.Contain(FSelectedIndexes.First) then
            DoLoopMoveLeftSelectionBoundary;

          if indexesOverMouse.Contain(FSelectedIndexes.Last) then
            DoLoopMoveRightSelectionBoundary;
        end
        else
          if thresholdDone and not FMouseHandledByUser then begin
            LoopSelection;
            exit;
          end;
      end;
      mbpRight: if thresholdDone and not FMouseHandledByUser then begin
        FIndexClickOrigin := AbscissaToIndex(X);
        LoopMoveView;
        exit;
      end;
    end;
  end

  else begin
    if not HaveSelection or FMouseHandledByUser then exit;

    // check if mouse is over selection boundaries
    if indexesOverMouse.Contain(FSelectedIndexes.First) or
       indexesOverMouse.Contain(FSelectedIndexes.Last) then
      PB.Cursor := crSizeWE
    else
      PB.Cursor := crDefault;
  end;


end;

procedure TFrameViewAudio.PBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseButtonPressed := mbpNone;
  PB.Cursor := crDefault;
end;

procedure TFrameViewAudio.PBMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var v: single;
  targetIndex: integer;
begin
  targetIndex := AbscissaToIndex(MousePos.X)+ViewBeginIndex;

  v := PixelPerPoint*ZOOM_COEFF;
  if WheelDelta > 0 then
    PixelPerPoint := PixelPerPoint+v
  else
    PixelPerPoint := PixelPerPoint-v;

  ViewBeginIndex := ViewBeginIndex+
                   targetIndex-(AbscissaToIndex(MousePos.X)+ViewBeginIndex);

  UpdateScrollBar;

  Handled := True;

  Redraw;
end;

procedure TFrameViewAudio.RedrawGradientImage;
begin
  DrawCurveGradient(FGradientBackground, BGRA(90,175,80));
end;

procedure TFrameViewAudio.RedrawMarkerImage;
var im, im1: TBGRABitmap;
begin
  try
    im := SVGFileToBGRABitmap(GetAppDataFolder+'DlgError.svg', ScaleDesignToForm(32), -1);
  except
    im := TBGRABitmap.Create(32, 32, BGRAPixelTransparent);
    im.FontHeight := 32;
    im.TextOut(im.TextSize('!').cx div 2, 0, '!', BGRA(255,255,255));
  end;

  FMarkerImage.SetSize(im.Width, Max(PB.ClientHeight div 3, im.Height));
  FMarkerImage.Fill(BGRAPixelTransparent);

  if FMarkerImage.Height > im.Height then begin
    im1 := TBGRABitmap.Create(ScaleDesignToForm(3), FMarkerImage.Height-im.Height+5, BGRAPixelTransparent);
    DrawMarkerGradient(im1, BGRA(255,85,85));
    FMarkerImage.PutImage((im.Width-im1.Width) div 2,
                  FMarkerImage.Height-im1.Height, im1, dmDrawWithTransparency);
    im1.Free;
  end;

  FMarkerImage.PutImage(0, 0, im, dmDrawWithTransparency);
  im.Free;

{  FMarkerImage.SetSize(ScaleDesignToForm(10), PB.ClientHeight);
  DrawMarkerGradient(FMarkerImage);  }
end;

procedure TFrameViewAudio.RedrawTempImage;
var i, yy, yy1, lastXDrawn, lastYDrawn, newX: integer;
  xx: single;
  yyy, deltaY: single;
  cLow, cHigh, cBack: TBGRAPixel;
  procedure DrawTrapeze(ax1, ay1, ax2, ay2: integer);
  begin
    yyy := ay1;
    if ax2 > ax1 then
      deltaY := (ay2-ay1)/(ax2-ax1)
    else
      deltaY := 0;
    repeat
      FImage.VertLine(ax1, Round(yyy), FImage.Height, cLow, dmSet);
      yyy := yyy+deltaY;
      inc(ax1);
    until ax1 > ax2;
  end;

begin
  lastXDrawn := -1;
  lastYDrawn := FImage.Height;

  cBack := BGRA(15,15,15);
  cLow := BGRA(60,75,45);
  cHigh := BGRA(90,150,90);

  cBack := BGRABlack;
  cLow := BGRAWhite;
  cHigh := BGRAWhite;

  // background
  FImage.Fill(cBack);
  if Length(FLevels) < 2 then exit;
  if FVisibleIndexes.First = -1 then exit;

  xx := 0;
  yy := FImage.Height-Round(FLevels[FVisibleIndexes.First]*FImage.Height);
  yy1 := FImage.Height;
  for i:=FVisibleIndexes.First to FVisibleIndexes.Last do begin

    if FPixelPerPoint > 1 then begin
      yy1 := FImage.Height-Round(FLevels[i]*FImage.Height);

      //FImage.Rectangle(Round(xx), yy1, Round(xx+FPixelPerPoint), FImage.Height, cLow, cLow, dmSet);
      DrawTrapeze(Round(xx), yy1, Round(xx+FPixelPerPoint), yy1);
      //FImage.DrawLine(Round(xx), yy, Round(xx), yy1, cHigh, True);
      //FImage.DrawLine(Round(xx), yy1, Round(xx+FPixelPerPoint), yy1, cHigh, True);

      xx := xx+FPixelPerPoint;
      yy := yy1;
    end else begin
      yy := FImage.Height-Round(FLevels[i]*FImage.Height);
      if yy1 > yy then yy1 := yy;

      newX := Round(xx);
      if lastXDrawn <> newX then begin
        FImage.VertLine(newX, yy1, FImage.Height, cLow, dmSet);
        FImage.DrawLine(lastXDrawn, lastYDrawn, newX, yy1, cHigh, True);
        FImage.DrawLine(lastXDrawn, lastYDrawn+1, newX, yy1+1, cHigh, True);
        lastXDrawn := newX;
        lastYDrawn := yy1;
        yy1 := FImage.Height;
      end;
      xx := xx+FPixelPerPoint;
    end;
  end;

  FImage.BlendImage(0, 0, FGradientBackground, boMultiply);
end;

procedure TFrameViewAudio.SetPixelPerPoint(AValue: single);
begin
  AValue := EnsureRange(AValue, MIN_PIXELPERPOINT_VALUE, MAX_PIXELPERPOINT_VALUE);

  if FPixelPerPoint = AValue then Exit;
  FPixelPerPoint := AValue;

  RecomputeFirstLastIndexView;
  RecomputeVisibleUserMarksIndexes;

  ComputeLegendTimeInterval;
  RedrawTempImage;
  Redraw;
  UpdateScrollBar;
end;

procedure TFrameViewAudio.SetViewBeginIndex(AValue: integer);
begin
  if AValue < 0 then AValue := 0;
  if FViewBeginIndex = AValue then Exit;
  FViewBeginIndex := AValue;

  ComputeLegendTimeBeginPosition;

  if not FScrollBarLocked then ScrollBar1.Position := indexToAbscissa(FViewBeginIndex);

  RecomputeFirstLastIndexView;
  RedrawTempImage;
end;

procedure TFrameViewAudio.RecomputeFirstLastIndexView;
begin
  FVisibleIndexes.First := -1;
  FVisibleIndexes.Last := -1;
  if Length(FLevels) < 2 then exit;

  FVisibleIndexes.First := TimeToLevelIndex(ViewBeginTime);

  FVisibleIndexes.Last := TimeToLevelIndex(ViewBeginTime+AbscissaToTime(PB.ClientWidth));

  RecomputeVisibleUserMarksIndexes;
end;

procedure TFrameViewAudio.ComputeLegendTimeInterval;
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

  if TimeToAbscissa(FLegendTimeInterval) < ScaleDesignToForm(120) then begin
    while TimeToAbscissa(FLegendTimeInterval) < ScaleDesignToForm(120) do
      if IncreaseTimeInterval then break;
  end else begin
    while TimeToAbscissa(FLegendTimeInterval) > ScaleDesignToForm(120) do
      if DecreaseTimeInterval then break;
  end;
end;

procedure TFrameViewAudio.ComputeLegendTimeBeginPosition;
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

procedure TFrameViewAudio.DoSelectionChange;
begin
  if FOnSelectionChange <> NIL then
    FOnSelectionChange(Self);
end;

procedure TFrameViewAudio.DoLoopMoveLeftSelectionBoundary;
var indexMouse: integer;
begin
  if FMouseHandledByUser then exit;
  FMouseHandledByUser := True;
  PB.Cursor := crSizeWE;

  repeat
    indexMouse := AbscissaToIndex(PB.ScreenToClient(Mouse.CursorPos).x)+FViewBeginIndex;
    FSelectedIndexes.First := EnsureRange(indexMouse, 0, FSelectedIndexes.Last-1);
    Application.ProcessMessages;
    PB.Invalidate;
    Sleep(1);
  until FMouseButtonPressed = mbpNone;

  DoSelectionChange;
  FMouseHandledByUser := False;
  PB.Cursor := crDefault;
end;

procedure TFrameViewAudio.DoLoopMoveRightSelectionBoundary;
var indexMouse: integer;
begin
  if FMouseHandledByUser then exit;
  FMouseHandledByUser := True;
  PB.Cursor := crSizeWE;

  repeat
    indexMouse := AbscissaToIndex(PB.ScreenToClient(Mouse.CursorPos).x)+FViewBeginIndex;
    FSelectedIndexes.Last := EnsureRange(indexMouse, FSelectedIndexes.First+1, High(FLevels)+1);
    Application.ProcessMessages;
    PB.Invalidate;
    Sleep(1);
  until FMouseButtonPressed = mbpNone;

  DoSelectionChange;
  FMouseHandledByUser := False;
  PB.Cursor := crDefault;
end;

procedure TFrameViewAudio.LoopSelection;
var indexMouse: integer;
begin
  if FMouseHandledByUser then exit;
  FMouseHandledByUser := True;

  repeat
    indexMouse := AbscissaToIndex(PB.ScreenToClient(Mouse.CursorPos).x)+FViewBeginIndex;
    if indexMouse >= FIndexClickOrigin then begin
      FSelectedIndexes.Last := indexMouse;
      FSelectedIndexes.First := FIndexClickOrigin;
    end
    else begin
      FSelectedIndexes.Last := FIndexClickOrigin;
      FSelectedIndexes.First := indexMouse;
    end;
    Application.ProcessMessages;
    PB.Invalidate;
    Sleep(1);
  until FMouseButtonPressed = mbpNone;

  FSelectedIndexes.First := EnsureRange(FSelectedIndexes.First, 0, High(FLevels)+1);
  if FSelectedIndexes.Last > High(FLevels)+1 then FSelectedIndexes.Last := High(FLevels)+1;

  DoSelectionChange;

  FMouseHandledByUser := False;
end;

procedure TFrameViewAudio.LoopMoveView;
var deltaIndex, ViewBeginOrigin: integer;
begin
  if FMouseHandledByUser then exit;
  FMouseHandledByUser := True;

  ViewBeginOrigin := ViewBeginIndex;
  repeat
    deltaIndex := FIndexClickOrigin-AbscissaToIndex(PB.ScreenToClient(Mouse.CursorPos).x);
    if deltaIndex <> 0 then begin
      ViewBeginIndex := ViewBeginOrigin + deltaIndex;
      UpdateScrollBar;
      Redraw;
    end;
    Application.ProcessMessages;
    Sleep(1);
  until FMouseButtonPressed = mbpNone;

  FMouseHandledByUser := False;
  PB.Cursor := crDefault;
end;

function TFrameViewAudio.ViewBeginTime: single;
begin
  Result := IndexToTime(FViewBeginIndex);
end;

function TFrameViewAudio.HaveSelection: boolean;
begin
  Result := FSelectedIndexes.First < FSelectedIndexes.Last;
end;

function TFrameViewAudio.TimeToAbscissa(aTime: single): integer;
begin
  Result := Round(aTime*(PixelPerPoint/SLICE_TIME));
end;

function TFrameViewAudio.GetTotalDuration: single;
begin
  Result := Length(FLevels)*SLICE_TIME;
end;

function TFrameViewAudio.ComputePixelPerPointToFit(aDeltaIndex: integer): single;
begin
  Result := 1.0;
  if Length(FLevels) > 0 then begin
    while DoAbscissaToIndex(PB.ClientWidth, Result) > aDeltaIndex do
      Result := Result+Result*ZOOM_COEFF;

    while DoAbscissaToIndex(PB.ClientWidth, Result) <= aDeltaIndex do
      Result := Result-Result*ZOOM_COEFF;
  end;
end;

function TFrameViewAudio.AbscissaToTime(aX: integer): single;
begin
  Result :=  DoAbscissaToTime(aX, PixelPerPoint);
  Result := (Trunc(Result*1000) div Trunc(SLICE_TIME*1000))*SLICE_TIME;
end;

function TFrameViewAudio.DoAbscissaToTime(aX: integer; aPixelPerPoint: single): single;
begin
  Result := aX/aPixelPerPoint*SLICE_TIME;
end;

function TFrameViewAudio.AbscissaToIndex(aX: integer): integer;
begin
  Result := DoAbscissaToIndex(aX, PixelPerPoint);
end;

function TFrameViewAudio.DoAbscissaToIndex(aX: integer; aPixelPerPoint: single): integer;
begin
  Result := Trunc(aX / aPixelPerPoint);
end;

function TFrameViewAudio.IndexToAbscissa(aIndex: integer): integer;
begin
  Result := Trunc(aIndex * PixelPerPoint);
end;

function TFrameViewAudio.TimeToIndex(aTime: single): integer;
begin
  Result := Ceil(aTime/SLICE_TIME);
end;

function TFrameViewAudio.IndexToTime(aIndex: integer): single;
begin
  Result := aIndex * SLICE_TIME;
end;

function TFrameViewAudio.TimeToFrameIndex(aTime: single): int64;
begin
  Result := Ceil(aTime*SampleRate);
end;

procedure TFrameViewAudio.DoZoomOn(aBeginIndex, aEndIndex: integer);
var
  delta: integer;
begin
  delta := aEndIndex-aBeginIndex;
  FPixelPerPoint := EnsureRange(ComputePixelPerPointToFit(delta),
                                MIN_PIXELPERPOINT_VALUE,
                                MAX_PIXELPERPOINT_VALUE);
  if aBeginIndex = 0 then
    FViewBeginIndex := 0
  else begin
    FViewBeginIndex := aBeginIndex-
                      (AbscissaToIndex(PB.ClientWidth)- delta) div 2;
    FViewBeginIndex := Max(FViewBeginIndex, 0);
  end;

  RecomputeFirstLastIndexView;
  RecomputeVisibleUserMarksIndexes;
  ComputeLegendTimeInterval;
  ComputeLegendTimeBeginPosition;
  RedrawTempImage;
  Redraw;

  ScrollBar1.SetParams(ViewBeginIndex, 0, Length(FLevels),
                       AbscissaToIndex(PB.ClientWidth));
end;

procedure TFrameViewAudio.UpdateScrollBar;
begin
  ScrollBar1.SetParams(ViewBeginIndex, 0, Length(FLevels),
        AbscissaToIndex(PB.ClientWidth));
end;

procedure TFrameViewAudio.DoLoad;
var reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  i, levelCount, remainder: integer;
begin
  if not reader.OpenRead(Filename) then exit;
  FSampleRate := reader.SampleRate;

  FFrameCountInSlice := reader.TimeToFrameIndex(SLICE_TIME);

  buf.Init(reader.Channels, ALS_SAMPLE_INT16);
  buf.FrameCapacity := FFrameCountInSlice; // Round(reader.Samplerate*SLICE_TIME);

  levelCount := 0;
  remainder := 0;
  DivMod(reader.Frames, buf.FrameCapacity, levelCount, remainder);
  if remainder > 0 then inc(levelCount);

  SetLength(FLevels, levelCount);

  i := 0;
  while reader.Read(buf, buf.FrameCapacity) > 0 do begin
    FLevels[i] := ComputeLevelFromAudioBuffer(buf);
    inc(i);
  end;

  reader.Close;
  buf.FreeMemory;
end;

function TFrameViewAudio.ReloadPart(aFrameIndexBegin, aFrameIndexEnd: int64): boolean;
var reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  i, iBegin, iEnd: integer;
begin
  Result := False;
  try
    if not reader.OpenRead(Filename) then exit;

    if not reader.MoveToFrame(aFrameIndexBegin) then begin
      reader.Close;
      exit;
    end;

    buf.Init(reader.Channels, ALS_SAMPLE_INT16);
    buf.FrameCapacity := FFrameCountInSlice;

    iBegin := Max(0, Floor(aFrameIndexBegin / FFrameCountInSlice));
    iEnd := Min(High(FLevels), Ceil(aFrameIndexEnd / FFrameCountInSlice));

    for i:=iBegin to iEnd do begin
      if reader.Read(buf, buf.FrameCapacity) > 0 then
        FLevels[i] := ComputeLevelFromAudioBuffer(buf)
      else
        break;
    end;

    reader.Close;
    buf.FreeMemory;

    RecomputeFirstLastIndexView;
    RecomputeVisibleUserMarksIndexes;
    RedrawTempImage;
    Result := True;
  except
    on E: Exception do begin
      Log.Error('TFrameViewAudio.ReloadPart('+aFrameIndexBegin.ToString+','+aFrameIndexEnd.ToString+')'+LineEnding+
                '    raise exception: '+E.Message+LineEnding+
                '    on file "'+Filename);
    end;
  end;
end;

function TFrameViewAudio.ReloadPartFromFrameToEnd(aFrameIndex: int64): boolean;
var reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  i, newSize: integer;
begin
  Result := False;
  try
    // open file
    if not reader.OpenRead(Filename) then exit;

    FSampleRate := reader.SampleRate;
    FFrameCountInSlice := reader.TimeToFrameIndex(SLICE_TIME);

    // seek to the right frame index
    if not reader.MoveToFrame(aFrameIndex) then begin
      reader.Close;
      exit;
    end;

    // adjust the size of Levels array
    newSize := Ceil(reader.Frames/FFrameCountInSlice);
    if Length(FLevels) <> newSize then
      SetLength(FLevels, newSize);

    buf.Init(reader.Channels, ALS_SAMPLE_INT16);
    buf.FrameCapacity := FFrameCountInSlice;

    i := Max(0, Floor(aFrameIndex / FFrameCountInSlice));

    while reader.Read(buf, buf.FrameCapacity) > 0 do begin
      FLevels[i] := ComputeLevelFromAudioBuffer(buf);
      inc(i);
    end;

    reader.Close;
    buf.FreeMemory;

    RecomputeFirstLastIndexView;
    RecomputeVisibleUserMarksIndexes;
    RedrawTempImage;
    UpdateScrollBar;
    Result := True;
  except
    on E: Exception do begin
      Log.Error('TFrameViewAudio.ReloadPartFromFrameToEnd('+aFrameIndex.ToString+')'+LineEnding+
                '    raise exception: '+E.Message+LineEnding+
                '    on file "'+Filename);
    end;
  end;

end;

function TFrameViewAudio.ComputeLevelFromAudioBuffer(const aBuf: TALSPlaybackBuffer): single;
var
  i: Integer;
begin
  aBuf.ComputeChannelsLevel;

  Result := ALS_DECIBEL_MIN_VALUE;
  for i:=0 to aBuf.ChannelCount-1 do
    if Result < aBuf.ChannelsLeveldB[i] then
      Result := aBuf.ChannelsLeveldB[i];

  Result := (Result+DECIBEL_MAX_ABSOLUTE_VALUE)/DECIBEL_MAX_ABSOLUTE_VALUE;
end;

procedure TFrameViewAudio.RecomputeVisibleUserMarksIndexes;
var
  i: Integer;
begin
  FVisibleUserMarksIndexes.InitDefault;

  for i:=0 to High(FUserMarks) do begin
    if (TimeToIndex(FUserMarks[i]) >= ViewBeginIndex) and
       (FVisibleUserMarksIndexes.First = -1) then
      FVisibleUserMarksIndexes.First := i;

    if (TimeToIndex(FUserMarks[i]) <= ViewEndIndex) and
       (FVisibleUserMarksIndexes.First <> -1) then
      FVisibleUserMarksIndexes.Last := i;
  end;

  if FVisibleUserMarksIndexes.Last = -1 then
    FVisibleUserMarksIndexes.Last := FVisibleUserMarksIndexes.First;
end;

function TFrameViewAudio.GetViewEndTime: single;
begin
  Result := IndexToTime(FViewBeginIndex+AbscissaToIndex(PB.ClientWidth));
end;

function TFrameViewAudio.ViewEndIndex: integer;
begin
  Result := FViewBeginIndex + AbscissaToIndex(PB.ClientWidth);
end;

function TFrameViewAudio.GetUserMarksIndexesInSelection: TIndexArray;
var i: integer;
begin
  Result := NIL;
  if FUserMarks.Count = 0 then exit;
  if not HaveSelection then exit;

  for i:=0 to High(FUserMarks) do
    if (TimeToIndex(FUserMarks[i]) >= FSelectedIndexes.First) and
       (TimeToIndex(FUserMarks[i]) <= FSelectedIndexes.Last) then
      Insert(i, Result, Length(Result));
end;

procedure TFrameViewAudio.ShiftUserMarksFromIndex(aStartIndex, aDelta: integer);
var i: integer;
  deltaTime: single;
begin
  if FUserMarks.Count = 0 then exit;

  deltaTime := IndexToTime(aDelta);

   for i:=0 to FUserMarks.Count-1 do
    if TimeToIndex(FUserMarks[i]) >= aStartIndex then
      FUserMarks[i] := FUserMarks[i] + deltaTime;
end;

procedure TFrameViewAudio.LoadUserMarksFromFile(const aFileName: string);
begin
  FUserMarks.LoadFromFile(aFileName);
  RecomputeVisibleUserMarksIndexes;
end;

procedure TFrameViewAudio.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeight([PBTime], FDesignFontHeight);
{$endif}
end;

constructor TFrameViewAudio.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AdjustFont;
  FImage := TBGRABitmap.Create(1, 1);
  FGradientBackground := TBGRABitmap.Create(1, 1);
  FMarkerImage := TBGRABitmap.Create(1, 1);
  FPixelPerPoint := 1;
  FViewBeginIndex := 0;
  FLegendTimeInterval := 1.0;
  FVisibleIndexes.InitDefault;
  FVisibleUserMarksIndexes.InitDefault;
end;

destructor TFrameViewAudio.Destroy;
begin
  if FSound <> NIL then
    FSound.Kill;
  FSound := NIL;
  FImage.Free;
  FImage := NIL;
  FGradientBackground.Free;
  FGradientBackground := NIL;
  FMarkerImage.Free;
  FMarkerImage := NIL;
  inherited Destroy;
end;

procedure TFrameViewAudio.EraseBackground(DC: HDC);
begin
end;

procedure TFrameViewAudio.Redraw;
begin
  PB.Invalidate;
  PBTime.Invalidate;
end;

function TFrameViewAudio.IsEmpty: boolean;
begin
  Result := Length(FFilename) = 0;
end;

function TFrameViewAudio.HaveSample: boolean;
begin
  Result := Length(FLevels) > 0;
end;

function TFrameViewAudio.CursorIsAtBegin: boolean;
begin
  Result := FSelectedIndexes.First = 0;
end;

function TFrameViewAudio.CursorIsAtEnd: boolean;
begin
  Result := FSelectedIndexes.First = High(FLevels)+1;
end;

function TFrameViewAudio.LoadSound(const aFilename: string): boolean;
begin
  Result := False;

  Log.Info('gyv: TFrameViewAudio.LoadSound('+aFilename+')');

  FFilename := aFilename;
  FLevels := NIL;
  FUserMarks.Clear;
  MainUndoRedoManager.Clear;

  Result := ReloadPartFromFrameToEnd(0);

  if Result then begin
    ViewAll;
    SetCursorToBegin;
  end else begin
    // loading fail: log error message
    Log.Error('loading failed   '+GetFileInfoForLogMessage(FileName), 1);
  end;
end;

procedure TFrameViewAudio.ReloadSound;
begin
  if ReloadPartFromFrameToEnd(0) then
    Redraw
  else
    Log.Error('gyv: TFrameViewAudio.ReloadSound fail'+LineEnding+
              '    on file "'+FileName+'"'+
              '    '+GetFileInfoForLogMessage(FileName));
end;

procedure TFrameViewAudio.DeleteUserMarksInSelection(aDoSaveForUndo: boolean);
var
  U: TIndexArray;
begin
  U := GetUserMarksIndexesInSelection;
  if Length(U) = 0 then exit;

  if aDoSaveForUndo then begin
    // save the selected user marks for undo
    MainUndoRedoManager.SaveUserMarksValues(FUserMarks, U[0], Length(U));
    MainUndoRedoManager.PushToUndo_DeleteUserMarksInSelection(SDeleteMarkerOnSelection,
           LevelIndexToFrameIndex(FSelectedIndexes.First), LevelIndexToFrameIndex(FSelectedIndexes.Last));
  end;

  Delete(FUserMarks, U[0], Length(U));
  FUserMarks.SaveToFile(FFilename);

  RecomputeVisibleUserMarksIndexes;
  SelectNone;
  Redraw;
end;

procedure TFrameViewAudio.DeleteAllUserMarks(aDoSaveForUndo: boolean);
begin
  if FUserMarks.Count = 0 then exit;

  if aDoSaveForUndo then begin
    // save all user marks for undo
    MainUndoRedoManager.SaveUserMarksValues(FUserMarks, 0, Length(FUserMarks));
    MainUndoRedoManager.PushToUndo_DeleteAllUserMarks(SDeleteAllMarkers,
             0, High(FLevels));
  end;

  FUserMarks.Clear;
  FUserMarks.SaveToFile(FFilename);
  RecomputeVisibleUserMarksIndexes;
  Redraw;
end;

procedure TFrameViewAudio.Clear;
begin
  Stop; // stop audio (just in case)

  FFilename := '';
  FLevels := NIL;
  FVisibleIndexes.InitDefault;

  FUserMarks.Clear;
  FVisibleUserMarksIndexes.InitDefault;

  FSelectedIndexes.First := 0;
  FSelectedIndexes.Last := 0;
  DoSelectionChange;

  MainUndoRedoManager.Clear;

  RedrawTempImage;
  Redraw;
end;

procedure TFrameViewAudio.SetSelection(aBeginIndex, aEndIndex: integer);
var
  v: integer;
begin
  aBeginIndex := EnsureRange(aBeginIndex, 0, High(FLevels)+1);
  aEndIndex := EnsureRange(aEndIndex, 0, High(FLevels)+1);

  if aEndIndex < aBeginIndex then begin
    v := aBeginIndex;
    aBeginIndex := aEndIndex;
    aEndIndex := v;
  end;

  FSelectedIndexes.First := aBeginIndex;
  FSelectedIndexes.Last := aEndIndex;
  Redraw;
  DoSelectionChange;
end;

procedure TFrameViewAudio.SelectNone;
begin
  FSelectedIndexes.Last := FSelectedIndexes.First;
  Redraw;
  DoSelectionChange;
end;

procedure TFrameViewAudio.SelectAll;
begin
  SetSelection(0, High(FLevels)+1);
end;

procedure TFrameViewAudio.SetCursorToBegin;
begin
  FSelectedIndexes.First := 0;
  SelectNone;
end;

procedure TFrameViewAudio.SetCursorToEnd;
begin
  FSelectedIndexes.First := High(FLevels)+1;
  SelectNone;
end;

procedure TFrameViewAudio.SetCursorToPos(aIndex: integer);
begin
  FSelectedIndexes.First := EnsureRange(aIndex, 0, High(FLevels)+1);
  SelectNone;
end;

procedure TFrameViewAudio.OptimizeViewToShowCursorPos;
begin
  if FSelectedIndexes.First = 0 then begin
    // begin of the view
    ViewBeginIndex := 0;
    exit;
  end;

  // cursor is already visible ?
  if Inrange(FSelectedIndexes.First, ViewBeginIndex, ViewEndIndex) then exit;

  // cursor is at the end ?
  if FSelectedIndexes.First = High(FLevels) then begin
    ViewBeginIndex :=  FSelectedIndexes.First-
                       Round(AbscissaToIndex(PB.ClientWidth)*0.7);
    exit;
  end;

  // other case -> center the cursor on the view
  ViewBeginIndex := FSelectedIndexes.First-AbscissaToIndex(PB.ClientWidth) div 2;
end;

procedure TFrameViewAudio.OptimizeViewToShowSelection;
begin
  // there is no selection
  if FSelectedIndexes.First = FSelectedIndexes.Last then begin
    OptimizeViewToShowCursorPos;
    exit;
  end;

  // selection already visible ?
  if Inrange(FSelectedIndexes.First, ViewBeginIndex, ViewEndIndex) and
     Inrange(FSelectedIndexes.Last, ViewBeginIndex, ViewEndIndex) then exit;

  // if the selection can fit in the view with the actual zoom, we shift the view
  if FSelectedIndexes.Count < AbscissaToIndex(PB.ClientWidth) then begin
    ViewBeginIndex := FSelectedIndexes.First-FSelectedIndexes.Count div 2;
    exit;
  end;

  // other case -> change the zoom to fit the selection
  ZoomOnSelection;
end;

function TFrameViewAudio.LevelIndexToFrameIndex(aLevelIndex: integer): int64;
begin
  Result := FFrameCountInSlice * aLevelIndex;
end;

function TFrameViewAudio.FrameIndexToLevelIndex(aFrameIndex: int64): integer;
begin
  Result := Round(aFrameIndex / FFrameCountInSlice);
end;

function TFrameViewAudio.DoCutPart(aIndexBegin, aIndexEnd: integer): boolean;
begin
  Result := CutAudioFile(Filename,
                         LevelIndexToFrameIndex(aIndexBegin),
                         LevelIndexToFrameIndex(aIndexEnd)-1);
  if Result then begin
    // apply the change on the view -> avoid to rescan all the file ! it's faster
    Delete(FLevels, aIndexBegin, aIndexEnd-aIndexBegin);

    RecomputeFirstLastIndexView;
    RedrawTempImage;
    UpdateScrollBar;
  end
end;

function TFrameViewAudio.DoInsertPart(const aNewDataFile: string;
  aFrameIndex: int64): boolean;
begin
  Result := InsertAudioFile(Filename, aNewDataFile, aFrameIndex);

  if Result then
    ReloadSound;
end;

function TFrameViewAudio.DoSilenceOnPart(aFrameIndexBegin, aFrameIndexEnd: int64): boolean;
var attenuationTime: Single;
begin
  if (aFrameIndexEnd-aFrameIndexBegin)/FSampleRate < 0.100 then
    attenuationTime := 0.0
  else
    attenuationTime := ProgramOptions.AudioSilenceAttenuationTimeMS*0.001;

  Result := SilenceAudioFile(Filename,
                             aFrameIndexBegin,
                             aFrameIndexEnd,
                             attenuationTime);
{  Result := SilenceAudioFile2(Filename,
                              aFrameIndexBegin,
                              aFrameIndexEnd,
                              attenuationTime);  }

  if Result then
    ReloadPart(aFrameIndexBegin, aFrameIndexEnd);
end;

function TFrameViewAudio.DoInsertSilence(aFrameIndex: int64): boolean;
var dur: single;
  levelToInsert, i, beginIndex, endIndex: integer;
begin
  // compute the number of audio 'slice' to insert
  levelToInsert := ProgramOptionsSilenceToSliceCount;
  dur := levelToInsert*SLICE_TIME_MS*0.001;

  Result := InsertSilenceInAudioFile(Filename, aFrameIndex, dur);

  if Result then begin
    // insert silence on the view
    beginIndex := FrameIndexToLevelindex(aFrameIndex);
    endIndex := High(FLevels);
    SetLength(FLevels, Length(FLevels)+levelToInsert);

    for i:=endIndex downto beginIndex do // shift
      FLevels[i+levelToInsert] := FLevels[i];

    for i:=beginIndex to beginIndex+levelToInsert-1 do     // set to 0
      FLevels[i] := 0.0;

  //  SetSelection(beginIndex, beginIndex+levelToInsert);

    RecomputeFirstLastIndexView;
    RedrawTempImage;
    UpdateScrollBar;
  end;
end;

function TFrameViewAudio.ProgramOptionsSilenceToSliceCount: integer;
begin
  Result := Ceil(ProgramOptions.InsertedSilenceDurationMS / SLICE_TIME_MS);
end;

function TFrameViewAudio.ThereIsUserMarkInSelection: boolean;
var i: integer;
  t1, t2: single;
begin
  Result := False;
  if not HaveSelection then exit;

  t1 := LevelIndexToTime(FSelectedIndexes.First);
  t2 := LevelIndexToTime(FSelectedIndexes.Last);
  for i:=0 to High(FUserMarks) do begin
    Result := InRange(FUserMarks[i], t1, t2);
    if Result then exit;
  end;
end;

function TFrameViewAudio.GetUserMarksInSelection: TUserMarks;
var t1, t2: single;
  i: integer;
begin
  Result := NIL;
  if not HaveSelection then exit;

  t1 := LevelIndexToTime(FSelectedIndexes.First);
  t2 := LevelIndexToTime(FSelectedIndexes.Last);
  for i:=0 to High(FUserMarks) do
   if InRange(FUserMarks[i], t1, t2) then
     Insert(FUserMarks[i], Result, Length(Result));
end;

procedure TFrameViewAudio.AddUserMarkAtCursor;
begin
  MainUndoRedoManager.PushToUndo_AddUserMarkAtCursor(SAddMarkerAtCursor,
                                LevelIndexToFrameIndex(FSelectedIndexes.First));
  FUserMarks.AddAndSort(LevelIndexToTime(FSelectedIndexes.First));
  FUserMarks.SaveToFile(FFileName);
  RecomputeVisibleUserMarksIndexes;
  Redraw;
end;

procedure TFrameViewAudio.DeleteUserMarkAtFrameIndex(aFrameIndex: int64);
begin
  FUserMarks.DeleteValue(LevelIndexToTime(FrameIndexToLevelIndex(aFrameIndex)));
  FUserMarks.SaveToFile(FFilename);
  RecomputeVisibleUserMarksIndexes;
  Redraw
end;

procedure TFrameViewAudio.AddUserMarkAtFrameIndex(aFrameIndex: int64);
begin
  FUserMarks.AddAndSort(LevelIndexToTime(FrameIndexToLevelIndex(aFrameIndex)));
  FUserMarks.SaveToFile(FFilename);
  RecomputeVisibleUserMarksIndexes;
  Redraw
end;

procedure TFrameViewAudio.MergeUserMarks(const aUserMarks: TUserMarks);
begin
  FUserMarks.AddAndSort(aUserMarks);
  FUserMarks.SaveToFile(FFilename);
  RecomputeVisibleUserMarksIndexes;
  Redraw;
end;

procedure TFrameViewAudio.ReplaceUserMarksBy(const aUserMarks: TUserMarks);
begin
  FUserMarks.Clear;
  FUserMarks.Create(aUserMarks);
  FUserMarks.SaveToFile(FFilename);
  RecomputeVisibleUserMarksIndexes;
end;

procedure TFrameViewAudio.SilenceOnSelection;
var f: string;
  frameBegin, frameEnd: int64;
  res: boolean;
begin
  if not HaveSelection then exit;
  Stop;
  Screen.BeginWaitCursor;

  try
    Log.Info('gyv: TFrameViewAudio.SilenceOnSelection');

    frameBegin := LevelIndexToFrameIndex(FSelectedIndexes.First);
    frameEnd := Min(LevelIndexToFrameIndex(FSelectedIndexes.Last)-1, FramesCount-1);

    // sauve la sélection dans un fichier UndoRedoxxx
    f := MainUndoRedoManager.SaveAudioDataFrom(Filename, frameBegin, frameEnd);
    if f = '' then begin
      if AskConfirmation(SFailToCopyAudio,
                      SContinue, SCancel, mtWarning) <> mrOk then begin
        res := True;
        Log.AddEmptyLine;
        exit;
      end;
    end;

    res := DoSilenceOnPart(frameBegin, frameEnd);

    if res then begin
      Redraw;
      if f <> '' then
        MainUndoRedoManager.PushToUndo_AudioSilenceOnSelection(SSilenceOnSelection);
    end
    else if f <> '' then SupprimeFichier(f);
  finally
    Screen.EndWaitCursor;
  end;

  if not res then begin
    Log.AddEmptyLine;
    ShowMess(SSilenceFailed, SOk, mtError);
  end;
  FormMain.UpdateToolsWidgets;
end;

procedure TFrameViewAudio.InsertSilenceAtCursorPos;
var frameBegin, frameEnd: int64;
  res: boolean;
begin
  if ProgramOptions.InsertedSilenceDurationMS <= 0 then exit;
  if HaveSelection then exit;

  res := False;
  Stop;
  Screen.BeginWaitCursor;
  try
    Log.Info('gyv: TFrameViewAudio.InsertSilenceAtCursorPos');
    // check the frame index
    if (FSelectedIndexes.First < 0) or
       (FSelectedIndexes.First > High(FLevels)+1) then begin
      Log.Error('    bad index='+FSelectedIndexes.First.ToString+LineEnding+
                '    file "'+FileName+'"'+LineEnding+
                '    '+GetFileInfoForLogMessage(Filename));
    end else begin
      frameBegin := LevelIndexToFrameIndex(FSelectedIndexes.First);

      res := DoInsertSilence(frameBegin);

      if res then begin
        frameEnd := frameBegin + ProgramOptionsSilenceToSliceCount*FFrameCountInSlice - 1;

        MainUndoRedoManager.PushToUndo_InsertRecord(SInsertSilence, frameBegin, frameEnd, FUserMarks);

        FUserMarks.AddOffsetToValuesFromTimePos(frameBegin/FSampleRate,
                                                ProgramOptionsSilenceToSliceCount*SLICE_TIME);
        FUserMarks.SaveToFile(FFilename);
        RecomputeVisibleUserMarksIndexes;
        Redraw;
      end;
    end;
  finally
    Screen.EndWaitCursor;
  end;

  if not res then begin
    Log.Error('gyv: audio edition InsertSilenceAtCursorPos failed');
    Log.AddEmptyLine;
    ShowMess(SInsertSilenceFailed, SClose);
  end;
  FormMain.UpdateToolsWidgets;
end;

procedure TFrameViewAudio.CutSelection;
var f: string;
  U: TIndexArray;
  frameEnd: int64;
  res: boolean;
begin
  if not HaveSelection then exit;
  Stop;
  Screen.BeginWaitCursor;

  try
    Log.Info('gyv: TFrameViewAudio.CutSelection');
    // compute the last frame index to copy
    frameEnd := Min(LevelIndexToFrameIndex(FSelectedIndexes.Last)-1, FramesCount-1);

    // save the selected audio in a file for undo
    f := MainUndoRedoManager.SaveAudioDataFrom(Filename,
                                               LevelIndexToFrameIndex(FSelectedIndexes.First),
                                               frameEnd);
    // if selected audio is not saved for undo, ask to the user if he want to continue
    if f = '' then begin
      if AskConfirmation(SFailToCopyAudio, SContinue, SCancel, mtWarning) <> mrOk then begin
        Log.AddEmptyLine;
        exit;
      end;
    end;

    // save all userMarks for undo
    MainUndoRedoManager.SaveUserMarksValues(FUserMarks, 0, Length(FUserMarks));
    // delete the user marks between the selection
    U := GetUserMarksIndexesInSelection;
    if Length(U) > 0 then
      Delete(FUserMarks, U[0], Length(U));
    // shift the higher user marks
    ShiftUserMarksFromIndex(FSelectedIndexes.First, -(FSelectedIndexes.Count-1));

    // cut audio
    res := DoCutPart(FSelectedIndexes.First, FSelectedIndexes.Last);

    if res then begin // audio cut succeed
      FUserMarks.SaveToFile(FFilename);
      RecomputeVisibleUserMarksIndexes;
      SelectNone;
      if f <> '' then begin
        MainUndoRedoManager.PushToUndo_AudioCutAction(SCutTheSelection);
      end;
    end
    else begin // audio cut failed
        Log.Error('    DoCutPart('+FSelectedIndexes.First.ToString+', '+
                                   FSelectedIndexes.Last.ToString+') failed'+LineEnding+
                  '    file "'+Filename+'"'+LineEnding+
                  '    '+GetFileInfoForLogMessage(Filename));
      if f <> '' then SupprimeFichier(f); // delete the audio saved for undo (in temp)
      FUserMarks.LoadFromFile(FFilename); // reload the user marks
      RecomputeVisibleUserMarksIndexes;
      Redraw;
    end;
  finally
    Screen.EndWaitCursor;
  end;

  if not res then begin
    Log.AddEmptyLine;
    ShowMessage(SCutFailed);
  end;
  FormMain.UpdateToolsWidgets;
end;

procedure TFrameViewAudio.Undo;
begin
  Stop;
  Screen.BeginWaitCursor;
  try
    Log.Info('gyv: TFrameViewAudio.Undo');
    MainUndoRedoManager.Undo;
  finally
    Screen.EndWaitCursor;
  end;
  FormMain.UpdateToolsWidgets;
end;

procedure TFrameViewAudio.Redo;
begin
  Stop;
  Screen.BeginWaitCursor;
  try
    Log.Info('gyv: TFrameViewAudio.Redo');
    MainUndoRedoManager.Redo;
  finally
    Screen.EndWaitCursor;
  end;
  FormMain.UpdateToolsWidgets;
end;

procedure TFrameViewAudio.ViewAll;
begin
  if Length(FLevels) = 0 then exit;

  DoZoomOn(0, High(FLevels));
  if PixelPerPoint > 4 then PixelPerPoint := 4;
  Redraw;
end;

procedure TFrameViewAudio.ZoomOnSelection;
begin
  if Length(FLevels) = 0 then exit;
  if not HaveSelection then exit;

  DoZoomOn(FSelectedIndexes.First, FSelectedIndexes.Last);
  Redraw;
end;

procedure TFrameViewAudio.Play;
begin
  if Length(FFilename) = 0 then exit;

  FormMain.FrameViewProjectFiles1.StopSound;

  if FSound <> NIL then FSound.Kill;
  FSound := Playback.FPlaybackContext.AddStream(FFilename);
  FSound.TimePosition := IndexToTime(FSelectedIndexes.First);

  // enhances listening only on not mixed audio files
  if not FileIsInMixedOutputFolder(FFilename) then
    FormMain.ApplyRequestedEffect(FSound);

  FSound.Play(False);
  Timer1.Enabled := True;
end;

procedure TFrameViewAudio.Pause;
begin
  if Length(FFilename) = 0 then exit;
  if FSound <> NIL then FSound.Pause;
end;

procedure TFrameViewAudio.Stop;
begin
  if Length(FFilename) = 0 then exit;

  Timer1.Enabled := False;
  if FSound <> NIL then
    FSound.Kill;
  FSound := NIL;
  Redraw;
end;

function TFrameViewAudio.CaptureAddition: boolean;
var F: TFormRecord;
  capturedFile, copyFileForUndo: string;
  p: TPoint;
  res: Integer;
  capturedFileDuration: single;
  capturedFileIndexCount: integer;
  capturedFileFramesCount, fileInViewFramesCount,
  frameBegin, frameEnd: int64;
begin
  Log.Info('gyv: TFrameViewAudio.CaptureAddition');

  fileInViewFramesCount := GetAudioFileFramesCount(FFilename);

  Result := False;
  if u_audio_utils.Capture.FCaptureContext.Error then exit;

  capturedFile := GetTempFilenameForRecord;

  F := TFormRecord.Create(NIL);
  // center the recording window to the audio view
  p.x := (FormMain.Panel5.Width - F.Width) div 2;
  p.y := FormMain.Panel5.ClientRect.top;
  p := FormMain.Panel5.ClientToScreen(p);

  F.Left := p.x;
  F.Top := p.y;
  F.Filename := capturedFile;
  F.RecordTimeOrigin := LevelIndexToTime(FSelectedIndexes.First);
  F.AskPage := False;
  F.AllowUserToAddUserMark := True;
  res := F.ShowModal;

  if res = mrCancel then begin
    // user have canceled the recording -> we delete its file
    SupprimeFichier(capturedFile);
    dec(FRecordFilenameSuffix);
    F.Free;
    exit;
  end;

  capturedFileFramesCount := GetAudioFileFramesCount(capturedFile);
  capturedFileDuration := GetAudioFileDuration(capturedFile);
  capturedFileIndexCount := TimeToIndex(capturedFileDuration);

  if HaveSelection then begin
    // replace the selection by the new record
    frameBegin := LevelIndexToFrameIndex(FSelectedIndexes.First);
    frameEnd := Min(LevelIndexToFrameIndex(FSelectedIndexes.Last)-1, FramesCount-1);
    // save the selected audio in a temp file for undo
    copyFileForUndo := MainUndoRedoManager.SaveAudioDataFrom(
                             Filename,
                             frameBegin,
                             frameEnd);
    // check if succed
    if copyFileForUndo = '' then begin
      if AskConfirmation(SFailToCopyAudio, SContinue, SCancel, mtWarning) <> mrOk then begin
        Result := True;
        F.Free;
        exit;
      end;
    end;

    // replace the selection by the record
    if ReplacePartByAudioFile(Filename,
                              frameBegin,
                              frameEnd,
                              capturedFile) then begin

      MainUndoRedoManager.PushToUndo_ReplaceSelectionByRecord(
                              SReplaceSelectionByRecord,
                              frameBegin,
                              frameEnd,
                              FUserMarks,
                              capturedFileFramesCount);
      // the new record's user marks are already positioned at the right time.
      // -> we only have to shift the current view user marks (only from the cursor pos)
      //    with the value of the new record duration minus the selection duration
      FUserMarks.AddOffsetToValuesFromTimePos(LevelIndexToTime(FSelectedIndexes.First),
                                              capturedFileDuration);
      ReloadPartFromFrameToEnd(frameBegin);
      SetSelection(FSelectedIndexes.First, FSelectedIndexes.First+capturedFileIndexCount+1);
      OptimizeViewToShowSelection;
      SupprimeFichier(capturedFile); // delete the last capture file
      FUserMarks.AddAndSort(F.UserMarks);
      FUserMarks.SaveToFile(FFilename);
      RecomputeVisibleUserMarksIndexes;
      Redraw;
      Result := True;
    end
    else begin
      ShowMess(SReplacementFailed, SClose, mtError);
      if FichierExistant(copyFileForUndo) then
        SupprimeFichier(copyFileForUndo); // delete the undo file
      FUserMarks.LoadFromFile(FFilename); // reload the previous user marks
    end;
  end
  else if FSelectedIndexes.First = 0 then begin
    // Insert the record to the beginning of the view
    if InsertAudioFile(Filename, capturedFile, 0) then begin
      ReloadSound;
      SetSelection(0, capturedFileIndexCount);
      OptimizeViewToShowSelection;
      SupprimeFichier(capturedFile); // delete the last capture file

      MainUndoRedoManager.PushToUndo_InsertRecord(SInsertRecordToTheBeginning, 0,
                 capturedFileFramesCount-1, FUserMarks);

      // the new record's user marks are already positioned at the right time.
      // -> we only have to shift the current view user marks (only from the cursor pos)
      //    with the value of the new record duration.
      FUserMarks.AddOffsetToValuesFromTimePos(LevelIndexToTime(FSelectedIndexes.First),
                                              capturedFileDuration);
      FUserMarks.AddAndSort(F.UserMarks);
      FUserMarks.SaveToFile(FFilename);
      RecomputeVisibleUserMarksIndexes;
      Redraw;
      Result := True;

    end
    else begin
      ShowMess(STheInsertionAtTheBeginningFailed, SClose, mtError);
      FUserMarks.LoadFromFile(FFilename); // reload the previous user marks
    end;
  end
  else if FSelectedIndexes.First = High(FLevels)+1 then begin
    // Insert the record to the end of the view
    if AppendAudioFiles(Filename, [capturedFile]) then begin
      SupprimeFichier(capturedFile); // delete the last capture file
      ReloadSound;

      MainUndoRedoManager.PushToUndo_InsertRecord(SInsertRecordToTheEnd,
                    fileInViewFramesCount,
                    fileInViewFramesCount+capturedFileFramesCount-1,
                    FUserMarks);

      FUserMarks.AddAndSort(F.UserMarks);
      FUserMarks.SaveToFile(FFilename);
      RecomputeVisibleUserMarksIndexes;
      SetSelection(FSelectedIndexes.First, High(FLevels)+1);
      OptimizeViewToShowSelection;
      Redraw;
      Result := True;
    end
    else begin
      ShowMess(STheInsertionAtTheEndFailed, SClose, mtError);
      FUserMarks.LoadFromFile(FFilename); // reload the previous user marks
    end;
  end
  else begin
    // insert the record at cursor position
    if InsertAudioFile(Filename, capturedFile, LevelIndexToFrameIndex(FSelectedIndexes.First)) then begin
      SupprimeFichier(capturedFile); // delete the last capture file
      ReloadSound;

      MainUndoRedoManager.PushToUndo_InsertRecord(SInsertRecordAtCursor,
                        LevelIndexToFrameIndex(FSelectedIndexes.First),
                        LevelIndexToFrameIndex(FSelectedIndexes.First)+capturedFileFramesCount-1,
                        FUserMarks);

      // the new record's user marks are already positioned at the right time.
      // -> we only have to shift the current view user marks (only from the cursor pos)
      //    with the value of the new record duration.
      FUserMarks.AddOffsetToValuesFromTimePos(LevelIndexToTime(FSelectedIndexes.First),
                                              capturedFileDuration);
      FUserMarks.AddAndSort(F.UserMarks);
      FUserMarks.SaveToFile(FFilename);
      RecomputeVisibleUserMarksIndexes;
      SetSelection(FSelectedIndexes.First, FSelectedIndexes.First+capturedFileIndexCount);
      OptimizeViewToShowSelection;
      Redraw;
      Result := True;

    end
    else begin
      ShowMess(STheInsertionAtCursorFailed, SClose, mtError);
      FUserMarks.LoadFromFile(FFilename); // reload the previous user marks
    end;
  end;

  F.Free;

  if not Result then Log.AddEmptyLine;
end;

procedure TFrameViewAudio.SetPlaybackVolume(aVolume: single);
begin
  // enhances listening only on not mixed audio files
  if FileIsInMixedOutputFolder(FFilename) then exit;

  if FSound <> NIL then
    FSound.Volume.Value := aVolume;
end;

procedure TFrameViewAudio.ProcessKeyDown(Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE: begin
      if FSound <> NIL then begin
        case FSound.State of
          ALS_PLAYING: Stop;
          ALS_PAUSED: Pause;
        end;
      end else Play;
      end;

    VK_DELETE: CutSelection;

    VK_A: begin
      if CommandKey in Shift then SelectAll;
      if ssAlt in Shift then ViewAll;
    end;

    VK_I: InsertSilenceAtCursorPos;

    VK_S: SilenceOnSelection;

    VK_Y: if (CommandKey in Shift) and MainUndoRedoManager.RedoAvailable then Redo;

    VK_Z: if (CommandKey in Shift) and MainUndoRedoManager.UndoAvailable then Undo;

  end;
end;

end.

