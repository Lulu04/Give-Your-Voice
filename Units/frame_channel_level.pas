unit frame_channel_level;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, Menus, Graphics,
  BGRABitmap, BGRABitmapTypes,
  ALSound;

type

  { TFrameChannelsLevel }

  TFrameChannelsLevel = class(TFrame)
    MIShowCaption: TMenuItem;
    MIDecibel: TMenuItem;
    MIPercent: TMenuItem;
    PB: TPaintBox;
    PopupMenu1: TPopupMenu;
    Shape1: TShape;
    Timer1: TTimer;
    procedure FrameResize(Sender: TObject);
    procedure MIPercentClick(Sender: TObject);
    procedure PBPaint(Sender: TObject);
    procedure PBResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCurrentLeftLevel: single;
    FCaption: string;
    FDecibelMode: boolean;
    procedure SetDecibelMode(AValue: boolean);
    procedure ResetClipColor;
  private
    FClipColor,
    FLevelColor,
    FCaptionColor: TColor;
    FBackGroundImage: TBGRABitmap;
    function GetShowCaption: boolean;
    procedure RedrawBackgroundImage;
    procedure SetShowCaption(AValue: boolean);
    procedure AdjustFont;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateProgressBar(const aBuf: TALSFrameBufferBase);

    procedure InitFromProgramOptions;
    property DecibelMode: boolean read FDecibelMode write SetDecibelMode;
    property ShowCaption: boolean read GetShowCaption write SetShowCaption;
  end;

implementation
uses u_utils, u_program_options {$if defined(Linux) or defined(Darwin)},u_common{$endif};

{$R *.lfm}

{ TFrameChannelsLevel }

procedure TFrameChannelsLevel.Timer1Timer(Sender: TObject);
begin
  if Shape1.Tag > 0 then
    Shape1.Tag := Shape1.Tag -1
  else begin
    ResetClipColor;
    Timer1.Enabled := False;
  end;
end;

procedure TFrameChannelsLevel.PBResize(Sender: TObject);
begin
  RedrawBackgroundImage;
  PB.Invalidate;
end;

procedure TFrameChannelsLevel.PBPaint(Sender: TObject);
var xx: integer;
  rSrc: TRect;
begin
  xx := Round(FCurrentLeftLevel*FBackGroundImage.Width);
  rSrc.Create(0, 0, xx, FBackGroundImage.Height);
  //FBackGroundImage.DrawPart(rSrc, PB.Canvas, 0, 0, True);
  FBackGroundImage.Draw(PB.Canvas, 0, 0, True);
  with PB.Canvas do begin
    if xx < FBackGroundImage.Width then begin
      Brush.Style := bsSolid;
      Brush.Color := Self.Parent.Color;
      Pen.Style := psClear;
      FillRect(xx, 0, PB.ClientWidth, PB.ClientHeight);
    end;

    // caption
    if MIShowCaption.Checked then begin
      Brush.Style := bsClear;
      Font.Color := FCaptionColor;
      TextOut((PB.ClientWidth - TextWidth(FCaption)) div 2,
              (PB.ClientHeight-Font.Height) div 2,
              FCaption);
    end;
  end;
end;

procedure TFrameChannelsLevel.FrameResize(Sender: TObject);
begin
  Shape1.Width := ClientWidth div 20;
end;

procedure TFrameChannelsLevel.MIPercentClick(Sender: TObject);
begin
  if Sender = MIPercent then begin
    SetDecibelMode(False);
    MIPercent.Checked := True;
    ProgramOptions.MicLevelUseDecibel := False;
    ProgramOptions.Save;
  end;

  if Sender = MIDecibel then begin
    SetDecibelMode(True);
    MIDecibel.Checked := True;
    ProgramOptions.MicLevelUseDecibel := True;
    ProgramOptions.Save;
  end;

  if Sender = MIShowCaption then begin
    MIShowCaption.Checked := not MIShowCaption.Checked;
    ProgramOptions.MicLevelShowCaption := MIShowCaption.Checked;
    ProgramOptions.Save;
  end;
end;

procedure TFrameChannelsLevel.SetDecibelMode(AValue: boolean);
begin
  if FDecibelMode = AValue then Exit;
  FDecibelMode := AValue;
  if FDecibelMode then begin
    FCurrentLeftLevel := ALS_DECIBEL_MIN_VALUE; // In decibel.
    FCaption := FormatFloat('0.0', ALS_DECIBEL_MIN_VALUE)+'dB';
  end
  else begin
    FCurrentLeftLevel := 0; // In percent.
    FCaption := FormatFloat('0.0', FCurrentLeftLevel*100)+'%';
  end;
end;

procedure TFrameChannelsLevel.ResetClipColor;
begin
  Shape1.Brush.Color := PercentColor(FLevelColor, -0.5);
end;

procedure TFrameChannelsLevel.RedrawBackgroundImage;
begin
  FBackGroundImage.SetSize(PB.ClientWidth, PB.ClientHeight);
  DrawCurveGradient(FBackGroundImage, ColorToBGRA(FLevelColor), False);
end;

function TFrameChannelsLevel.GetShowCaption: boolean;
begin
  Result := MIShowCaption.Checked;
end;

procedure TFrameChannelsLevel.SetShowCaption(AValue: boolean);
begin
  MIShowCaption.Checked := AValue;
end;

procedure TFrameChannelsLevel.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeight([Self, PB], FDesignFontHeight);
{$endif}
end;

constructor TFrameChannelsLevel.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  AdjustFont;

  FBackGroundImage := TBGRABitmap.Create(1, 1);
  SetDecibelMode(False);

  PB.Font.Height := 13;
  FClipColor := RGBToColor(255,40,20);
  FLevelColor := RGBToColor(90,175,80);
  FCaptionColor := BlackOrWhiteRelative(FLevelColor);
  ResetClipColor;
end;

destructor TFrameChannelsLevel.Destroy;
begin
  FBackGroundImage.Free;
  inherited Destroy;
end;

procedure TFrameChannelsLevel.UpdateProgressBar(const aBuf: TALSFrameBufferBase);
var
  v: single;
  procedure MarkAsClipped(aShape: TShape);
  begin
    aShape.Brush.Color := FClipColor;
    aShape.Tag := 6;
    Timer1.Enabled := True;
  end;
begin
  if not FDecibelMode then
  begin
    // Update progress bar - Percent mode
    v := aBuf.ChannelsLevel[0];
    if v >= FCurrentLeftLevel then
      FCurrentLeftLevel := v
    else
      FCurrentLeftLevel := FCurrentLeftLevel-(FCurrentLeftLevel-v)*0.04;

    FCaption := FormatFloat('0.0', aBuf.ChannelsLevel[0]*100)+'%';
  end
  else
  begin
    // Update progress bar - Decibel mode
    v := ALS_DECIBEL_MAX_VALUE-ALS_DECIBEL_MIN_VALUE;
    v := (aBuf.ChannelsLeveldB[0] + v) / v;
    if v >= FCurrentLeftLevel then
      FCurrentLeftLevel := v
    else
      FCurrentLeftLevel := FCurrentLeftLevel-(FCurrentLeftLevel-v)*0.04;

    FCaption := FormatFloat('0.0', aBuf.ChannelsLeveldB[0])+'dB';
  end;

  PB.Invalidate;

  // if the max amplitude is reach the corresponding shape becomes red to signal it
  if aBuf.ChannelsLevel[0] >= 1.0 then
    MarkAsClipped( Shape1 );
end;

procedure TFrameChannelsLevel.InitFromProgramOptions;
begin
  DecibelMode := ProgramOptions.MicLevelUseDecibel;
  ShowCaption := ProgramOptions.MicLevelShowCaption;
end;

end.

