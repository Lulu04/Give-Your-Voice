unit form_user_inputstring;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  frame_editstring, BGRABitmap;

type

  { TFormUserInput }

  TFormUserInput = class(TForm)
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    FImage: TBGRABitmap;
    function GetUserInput: string;
    procedure AdjustFont;
  public
    FrameEditString1: TFrameEditString;
    function Init(const mess, sok, scancel, sinput: string; aMsgType: TMsgDlgType): integer;

    property UserInput: string read GetUserInput;
  end;



implementation
uses LCLType{$if defined(Linux) or defined(Darwin)}, u_common{$endif}, u_utils;

{$R *.lfm}

{ TFormUserInput }

procedure TFormUserInput.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: SpeedButton1Click(NIL);
    VK_ESCAPE: SpeedButton2Click(NIL);
  end;
end;

procedure TFormUserInput.FormShow(Sender: TObject);
begin
  AdjustFont;

  FrameEditString1.SetTheFocus;
end;

procedure TFormUserInput.PaintBox1Paint(Sender: TObject);
begin
  if FImage = NIL then exit;
  FImage.Draw(PaintBox1.Canvas, 0, 0, False);
end;

procedure TFormUserInput.FormCreate(Sender: TObject);
begin
  FrameEditString1:=TFrameEditString.Create(Self);
  FrameEditString1.Parent:=Panel1;
  FrameEditString1.Align:=alClient;
end;

procedure TFormUserInput.SpeedButton1Click(Sender: TObject);
begin
  if FrameEditString1.TextIsValid
    then ModalResult:=mrOk
    else FrameEditString1.SetTheFocus;
end;

procedure TFormUserInput.SpeedButton2Click(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

function TFormUserInput.GetUserInput: string;
begin
  Result:=FrameEditString1.Text;
end;

procedure TFormUserInput.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
  ChangeFontHeight([Speedbutton1, Speedbutton2], FDesignFontHeight);
  FrameEditString1.AdjustFont;
{$endif}
{$if defined(LCLCOCOA)}
  Speedbutton1.Flat := False;
  Speedbutton2.Flat := False;
{$endif}
end;

function TFormUserInput.Init(const mess, sok, scancel, sinput: string;
  aMsgType: TMsgDlgType): integer;
begin
  FrameEditString1.Title:=mess;
  SpeedButton1.Caption:=sok;
  SpeedButton2.Caption:=scancel;
  FrameEditString1.Text:=sinput;

  FImage := MsgDlgTypeToBGRABitmap(aMsgType, PaintBox1.Width, PaintBox1.Height);

  {$ifdef LCLGTK2}
  Hide;
  Application.ProcessMessages;
  {$endif}
  Result:=ShowModal;
  FImage.Free;
  FImage := NIL;
end;

end.

