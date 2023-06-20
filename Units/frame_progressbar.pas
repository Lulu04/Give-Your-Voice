unit frame_progressbar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics, LCLType;

type
  TProgressBarOrientation = (prHorizontal, prVertical);

  { TFrameProgressBar }

  TFrameProgressBar = class(TFrame)
    PB: TPaintBox;
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, {%H-}Y: Integer);
    procedure PBPaint(Sender: TObject);
  private
    FOnUserPosChange: TNotifyEvent;
    FPosition: single;
    FOrientation: TProgressBarOrientation;
    FProgressColor: TColor;
    procedure SetPosition(AValue: single);
    procedure SetProgressColor(AValue: TColor);

  public
    constructor Create(TheOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;

    // position of the progress bar in range [0..1]
    property Position: single read FPosition write SetPosition;
    property OnUserPosChange: TNotifyEvent read FOnUserPosChange write FOnUserPosChange;

    property ProgressColor: TColor read FProgressColor write SetProgressColor;
  end;

implementation
uses Math;

{$R *.lfm}

{ TFrameProgressBar }

procedure TFrameProgressBar.PBPaint(Sender: TObject);
begin
  with PB.Canvas do begin
    // background
    Brush.Color := Parent.Color;
    Brush.Style := bsSolid;
    FillRect(0, 0, PB.Width, PB.Height);

    // progress
    Brush.Color := FProgressColor;
    FillRect(0, 0, Round(FPosition*PB.ClientWidth), PB.Height);
  end;
end;

procedure TFrameProgressBar.PBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (FOnUserPosChange <> NIL) then begin
    FPosition := EnsureRange(X/PB.ClientWidth, 0.0, 1.0);
    PB.Invalidate;
    FOnUserPosChange(Self);
  end;
end;

procedure TFrameProgressBar.SetPosition(AValue: single);
begin
  AValue := EnsureRange(AValue, 0.0, 1.0);
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  PB.Invalidate;
end;

procedure TFrameProgressBar.SetProgressColor(AValue: TColor);
begin
  if FProgressColor=AValue then Exit;
  FProgressColor:=AValue;
end;

constructor TFrameProgressBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOrientation := prHorizontal;
  FProgressColor := clLime;
end;

procedure TFrameProgressBar.EraseBackground(DC: HDC);
begin
end;

end.

