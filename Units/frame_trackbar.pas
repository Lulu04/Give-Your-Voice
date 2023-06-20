unit frame_trackbar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, LCLType,
  BGRABitmap, BGRABitmapTypes;

type
  TTrackBarOrientation = (trHorizontal, trVertical);

  { TFrameTrackBar }

  TFrameTrackBar = class(TFrame)
    PB: TPaintBox;
    procedure PBMouseDown(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PBMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PBMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PBPaint(Sender: TObject);
    procedure PBResize(Sender: TObject);
  private type
    TFrameTrackBarCursor = record
      PercentValue: single;
      R: TRect;
      CursorImage: TBGRABitmap;
    end;
    PFrameTrackBarCursor = ^TFrameTrackBarCursor;
  private
    FOrientation: TTrackBarOrientation;
    FReversed,
    FIntervalMode,
    FShowValue: Boolean;
    FCursorMax,          // used for single cursor too
    FCursorMin: TFrameTrackBarCursor;
    FWorkingCursor: PFrameTrackBarCursor;
    FCursorPositionRange: single;
    FOnChange: TNotifyEvent;

    FMouseMovingCursor: boolean;
    FClickOrigin: TPoint;

    FCoefBeforeDisplayValue: single;
    FDisplayPrefix,
    FDisplaySuffix: string;

    function GetIntervalMax: single;
    function GetIntervalMin: single;
    function GetPercentValue: single;
    procedure SetIntervalMax(AValue: single);
    procedure SetIntervalMin(AValue: single);
    procedure SetPercentValue(AValue: single);
    function AxisWidth: integer;
    procedure UpdateCursorsImage;
    procedure DeleteCursorsImage;
    procedure UpdateCursorsRect;
    procedure ProcessValueChange;
    procedure AdjustFont;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground({%H-}DC: HDC); override;

    procedure Init(aOrientation: TTrackBarOrientation; aReversed,
        aActiveIntervalMode, aShowValue: boolean);

    procedure FormatDisplayValue(aCoefToMultiply: single; const aPrefix, aSuffix: string);

    property PercentValue: single read GetPercentValue write SetPercentValue;
    property IntervalMin: single read GetIntervalMin write SetIntervalMin;
    property IntervalMax: single read GetIntervalMax write SetIntervalMax;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
implementation
uses Math, Graphics, u_utils;

{$R *.lfm}

{ TFrameTrackBar }

procedure TFrameTrackBar.PBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FCursorMax.R.Contains(Point(X, Y)) then
  begin
    FMouseMovingCursor := True;
    FWorkingCursor := @FCursorMax;
  end;

  if FIntervalMode and FCursorMin.R.Contains(Point(X, Y)) then
  begin
    FMouseMovingCursor := True;
    FWorkingCursor := @FCursorMin;
  end;

  FClickOrigin := Mouse.CursorPos;
end;

procedure TFrameTrackBar.PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var delta: integer;
begin
  if FMouseMovingCursor then
  begin
    PB.Cursor := crHandPoint;
    case FOrientation of
      trHorizontal:
        if (X >= 0) and (X <= PB.ClientWidth) then
        begin
          delta := Mouse.CursorPos.x-FClickOrigin.x;
          FWorkingCursor^.PercentValue := EnsureRange((FWorkingCursor^.R.Left+delta)/FCursorPositionRange, 0, 1);
          if FWorkingCursor = @FCursorMax then
            if FCursorMin.PercentValue > FCursorMax.PercentValue then
               FCursorMin.PercentValue := FCursorMax.PercentValue;
          if FWorkingCursor = @FCursorMin then
            if FCursorMax.PercentValue < FCursorMin.PercentValue then
               FCursorMax.PercentValue := FCursorMin.PercentValue;
          ProcessValueChange;

          FClickOrigin.x := Mouse.CursorPos.x;
        end;

      trVertical:
        if (Y >= 0) and (Y <= PB.ClientHeight) then
        begin
          delta := Mouse.CursorPos.y-FClickOrigin.y;
          FWorkingCursor^.PercentValue := EnsureRange((FWorkingCursor^.R.Top+delta)/FCursorPositionRange, 0, 1);
          ProcessValueChange;
          FClickOrigin.y := Mouse.CursorPos.y;
        end;
    end;
  end
  else
  if FCursorMax.R.Contains(Point(X, Y)) or
     (FCursorMin.R.Contains(Point(X, Y)) and FIntervalMode) then
    PB.Cursor := crHandPoint
  else
    PB.Cursor := crDefault;
end;

procedure TFrameTrackBar.PBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseMovingCursor := False;
end;

procedure TFrameTrackBar.PBPaint(Sender: TObject);
var t: TBGRABitmap;
  x: integer;
  txt: String;
begin
  with PB.Canvas do begin
    // axis
    if Enabled then
      Pen.Color := PercentColorRelative(Self.Parent.Color, 0.5)
    else
      Pen.Color := clGrayText;
    if FOrientation = trHorizontal then
    begin
      x := PB.ClientHeight div 4;
      Line(FCursorMax.R.Width shr 1, x,
           FCursorMax.R.Width shr 1, PB.ClientHeight-x);
      Line(PB.ClientWidth-FCursorMax.R.Width shr 1, x,
           PB.ClientWidth-FCursorMax.R.Width shr 1, PB.ClientHeight-x);
      Line(FCursorMax.R.Width shr 1, PB.ClientHeight shr 1,
           PB.ClientWidth-FCursorMax.R.Width shr 1, PB.ClientHeight shr 1);
      Line(FCursorMax.R.Width shr 1, PB.ClientHeight shr 1+1,
           PB.ClientWidth-FCursorMax.R.Width shr 1, PB.ClientHeight shr 1+1);
      if FIntervalMode and Enabled then
      begin
        Pen.Color := RGBToColor(192,96,0); // BGRA(192,96,0), 1, BGRA(255,128,0)
        Line(FCursorMin.R.CenterPoint.x, PB.ClientHeight shr 1,
             FCursorMax.R.CenterPoint.x, PB.ClientHeight shr 1);
        Line(FCursorMin.R.CenterPoint.x, PB.ClientHeight shr 1+1,
             FCursorMax.R.CenterPoint.x, PB.ClientHeight shr 1+1);
      end;
    end
    else begin
      x := PB.ClientWidth div 4;
      Line(x, FCursorMax.R.Height shr 1,
           PB.ClientWidth-x, FCursorMax.R.Height shr 1);
      Line(x, PB.ClientHeight-FCursorMax.R.Height shr 1,
           PB.ClientWidth-x, PB.ClientHeight-FCursorMax.R.Height shr 1 );
      Line(PB.ClientWidth shr 1, FCursorMax.R.Height shr 1,
           PB.ClientWidth shr 1, PB.ClientHeight-FCursorMax.R.Height shr 1);
      Line(PB.ClientWidth shr 1+1, FCursorMax.R.Height shr 1,
           PB.ClientWidth shr 1+1, PB.ClientHeight-FCursorMax.R.Height shr 1);
      if FIntervalMode and Enabled then
      begin
        Pen.Color := RGBToColor(192,96,0); // BGRA(192,96,0), 1, BGRA(255,128,0)
        Line(PB.ClientWidth shr 1, FCursorMin.R.CenterPoint.y,
             PB.ClientWidth shr 1, FCursorMax.R.CenterPoint.y);
        Line(PB.ClientWidth shr 1+1, FCursorMin.R.CenterPoint.y,
             PB.ClientWidth shr 1+1, FCursorMax.R.CenterPoint.y);
      end;
    end;
  end;

  // cursor Max
  if Enabled then
    FCursorMax.CursorImage.Draw(PB.Canvas, FCursorMax.R.Left, FCursorMax.R.Top, False)
  else begin
    t := FCursorMax.CursorImage.FilterGrayscale;
    t.Draw(PB.Canvas, FCursorMax.R.Left, FCursorMax.R.Top, False);
    t.Free;
  end;
  // cursor Min
  if FIntervalMode then
    if Enabled then
      FCursorMin.CursorImage.Draw(PB.Canvas, FCursorMin.R.Left, FCursorMin.R.Top, False)
    else begin
      t := FCursorMin.CursorImage.FilterGrayscale;
      t.Draw(PB.Canvas, FCursorMin.R.Left, FCursorMin.R.Top, False);
      t.Free;
    end;

  // label value
  if FShowValue then
  with PB.Canvas do begin
    Brush.Style := bsClear;

    if Enabled then
      Font.Color := Self.Font.Color
    else
      Font.Color := clGrayText;

    txt := FDisplayPrefix+
           FormatFloat('0.0', GetIntervalMax*FCoefBeforeDisplayValue)+
           FDisplaySuffix;
    if FOrientation = trHorizontal then
    begin
      x := GetTextWidth(txt);
      if FCursorMax.R.Right+x <= PB.ClientWidth then
        x := FCursorMax.R.Right
      else
        x := FCursorMax.R.Left-x;
      TextOut(x, FCursorMax.R.Top, txt);
    end
    else begin
      x := GetTextHeight(txt);
      if FCursorMax.R.Top-x >= 0 then
        x := FCursorMax.R.Top-x
      else
        x := FCursorMax.R.Bottom;
      TextOut(FCursorMax.R.Left, x, txt);
    end;

    if FIntervalMode then begin
      txt := FormatFloat('0.0', GetIntervalMin*FCoefBeforeDisplayValue)+FDisplaySuffix;

      if FOrientation = trHorizontal then
      begin
        x := GetTextWidth(txt);
        if FCursorMin.R.Left-x >= 0 then
          x := FCursorMin.R.Left-x
        else
          x := FCursorMin.R.Right;
        TextOut(x, FCursorMin.R.Top, txt);
      end
      else begin
        x := GetTextHeight(txt);
        if FCursorMin.R.Top-x >= 0 then
          x := FCursorMin.R.Top-x
        else
          x := FCursorMin.R.Bottom;
        TextOut(FCursorMin.R.Left, x, txt);
      end;
    end;
  end;
end;

procedure TFrameTrackBar.PBResize(Sender: TObject);
begin
  UpdateCursorsImage;

  if FOrientation = trHorizontal then
    FCursorPositionRange := PB.ClientWidth-FCursorMax.CursorImage.Width
  else
    FCursorPositionRange := PB.ClientHeight-FCursorMax.CursorImage.Height;

  UpdateCursorsRect;
end;

procedure TFrameTrackBar.SetPercentValue(AValue: single);
begin
  SetIntervalMax(AValue);
end;

function TFrameTrackBar.AxisWidth: integer;
begin
  Result := ScaleDesignToForm(2);
end;

function TFrameTrackBar.GetIntervalMax: single;
begin
  Result := FCursorMax.PercentValue;

  if FReversed then Result := 1 - Result;
end;

function TFrameTrackBar.GetIntervalMin: single;
begin
  if FIntervalMode then
    Result := FCursorMin.PercentValue
  else
    Result := FCursorMax.PercentValue;

  if FReversed then Result := 1 - Result;
end;

function TFrameTrackBar.GetPercentValue: single;
begin
  Result := GetIntervalMax;
end;

procedure TFrameTrackBar.SetIntervalMax(AValue: single);
begin
  AValue := EnsureRange(AValue, 0, 1);
  if FReversed then AValue := 1 - AValue;
  if FCursorMax.PercentValue = AValue then exit;

  FCursorMax.PercentValue := AValue;
  ProcessValueChange;
end;

procedure TFrameTrackBar.SetIntervalMin(AValue: single);
begin
  AValue := EnsureRange(AValue, 0, 1);
  if FReversed then AValue := 1 - AValue;
  if FCursorMin.PercentValue = AValue then exit;

  FCursorMin.PercentValue := AValue;
  ProcessValueChange;
end;

procedure TFrameTrackBar.UpdateCursorsImage;
var w, h: integer;
begin
  DeleteCursorsImage;

  if not FIntervalMode then
  begin
    if FOrientation = trHorizontal then
    begin
      w := EnsureRange(Round(PB.Height*0.05), 12, 16);
      h := Min(ScaleDesignToForm(26), PB.Height);
    end
    else begin
      w := Min(ScaleDesignToForm(26), PB.Width);
      h := EnsureRange(Round(PB.Width*0.05), 12, 16);
    end;
    FCursorMax.CursorImage := TBGRABitmap.Create(w, h, BGRAPixelTransparent);
    FCursorMax.CursorImage.RoundRectAntialias(0, 0, w-1, h-1,
          Min(w-1, h-1)*0.5, Min(w-1, h-1)*0.5,
          BGRA(192,96,0), 1, BGRA(255,128,0));
  end
  else begin
    if FOrientation = trHorizontal then
    begin
      w := EnsureRange(Round(PB.ClientHeight*0.05), 12, 16);
      h := (PB.ClientHeight - AxisWidth) div 2;
    end
    else begin
      w := (PB.ClientWidth - AxisWidth) div 2;
      h := EnsureRange(Round(PB.ClientWidth*0.05), 12, 16);
    end;
    FCursorMax.CursorImage := TBGRABitmap.Create(w, h, BGRAPixelTransparent);
    FCursorMax.CursorImage.DrawPolygonAntialias(
              [PointF(0,0), PointF(w-1,0), PointF(w-1,h/2), PointF(w/2,h-1),
               PointF(0,h/2), PointF(0,0)],
              BGRA(192,96,0), 1, BGRA(255,128,0));

    FCursorMin.CursorImage := TBGRABitmap.Create(w, h, BGRAPixelTransparent);
    FCursorMin.CursorImage.DrawPolygonAntialias(
              [PointF(0,h-1), PointF(0,h/2), PointF(w/2,0), PointF(w-1,h/2),
               PointF(w-1,h-1), PointF(0,h-1)],
              BGRA(192,96,0), 1, BGRA(255,128,0));
  end;
end;

procedure TFrameTrackBar.UpdateCursorsRect;
begin
  case FOrientation of
  trHorizontal:
    begin;
      FCursorMax.R.Left := Round(FCursorPositionRange*FCursorMax.PercentValue);
      if FIntervalMode then
      begin
        FCursorMax.R.Top := 0
      end
      else begin
        FCursorMax.R.Top := (PB.Height-FCursorMax.CursorImage.Height) div 2;
      end;
      FCursorMax.R.Width := FCursorMax.CursorImage.Width;
      FCursorMax.R.Height := FCursorMax.CursorImage.Height;

      if FIntervalMode then
      begin
        FCursorMin.R.Left := Round(FCursorPositionRange*FCursorMin.PercentValue);
        FCursorMin.R.Top := PB.Height - FCursorMin.CursorImage.Height;
        FCursorMin.R.Width := FCursorMin.CursorImage.Width;
        FCursorMin.R.Height := FCursorMin.CursorImage.Height;
      end;
    end;
  trVertical:
    begin
      if FIntervalMode then
        FCursorMax.R.Left := 0
      else
        FCursorMax.R.Left := (PB.Width-FCursorMax.CursorImage.Width) div 2;
      FCursorMax.R.Top := Round(FCursorPositionRange*FCursorMax.PercentValue);
      FCursorMax.R.Width := FCursorMax.CursorImage.Width;
      FCursorMax.R.Height := FCursorMax.CursorImage.Height;

      if FIntervalMode then
      begin
        FCursorMin.R.Left := PB.Width - FCursorMin.CursorImage.Width;
        FCursorMin.R.Top := Round(FCursorPositionRange*FCursorMin.PercentValue);
        FCursorMin.R.Width := FCursorMin.CursorImage.Width;
        FCursorMin.R.Height := FCursorMin.CursorImage.Height;
      end;
    end;
  end;
end;

procedure TFrameTrackBar.DeleteCursorsImage;
begin
  if FCursorMax.CursorImage <> NIL then
    FCursorMax.CursorImage.Free;
  FCursorMax.CursorImage := NIL;

  if FCursorMin.CursorImage <> NIL then
    FCursorMin.CursorImage.Free;
  FCursorMin.CursorImage := NIL;
end;

procedure TFrameTrackBar.ProcessValueChange;
begin
  UpdateCursorsRect;
  PB.Invalidate;

  if FOnChange <> NIL then
    FOnChange(Self);
end;

procedure TFrameTrackBar.AdjustFont;
begin

end;

destructor TFrameTrackBar.Destroy;
begin
  DeleteCursorsImage;
  inherited Destroy;
end;

constructor TFrameTrackBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOrientation := trHorizontal;
  UpdateCursorsImage;

  UpdateCursorsRect;
  FCursorMax.PercentValue := 1.0;
  FCursorMin.PercentValue := 0.0;

  FCoefBeforeDisplayValue := 100.0;
  FDisplayPrefix := '';
  FDisplaySuffix := '%';
end;

procedure TFrameTrackBar.EraseBackground(DC: HDC);
begin
end;

procedure TFrameTrackBar.Init(aOrientation: TTrackBarOrientation; aReversed,
  aActiveIntervalMode, aShowValue: boolean);
begin
  AdjustFont;
  FOrientation := aOrientation;
  FReversed := aReversed;
  FIntervalMode := aActiveIntervalMode;
  FShowValue := aShowValue;

  UpdateCursorsImage;
  UpdateCursorsRect;
  PB.Invalidate;
end;

procedure TFrameTrackBar.FormatDisplayValue(aCoefToMultiply: single;
  const aPrefix, aSuffix: string);
begin
  FCoefBeforeDisplayValue := aCoefToMultiply;
  FDisplayPrefix := aPrefix;
  FDisplaySuffix := aSuffix;
end;

end.

