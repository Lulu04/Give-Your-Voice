unit form_user_showmessage;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, BGRABitmap;

type

  { TFormUserMessage }

  TFormUserMessage = class(TForm)
    Label1: TLabel;
    PaintBox1: TPaintBox;
    Shape1: TShape;
    BOk: TSpeedButton;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Label1Resize(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FImage: TBGRABitmap;
    procedure AdjustFont;
  public
    function Init( const mess, sbutton: string; aMsgType: TMsgDlgType ): integer;
  end;



implementation
uses LCLType, u_utils{$if defined(Linux) or defined(Darwin)}, u_common{$endif};

{$R *.lfm}

{ TFormUserMessage }

procedure TFormUserMessage.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) or (Key=VK_RETURN)
    then ModalResult:=mrCancel;
end;

procedure TFormUserMessage.FormShow(Sender: TObject);
begin
  AdjustFont;
end;

procedure TFormUserMessage.Label1Resize(Sender: TObject);
begin
  // adjust the height of the windows according to the height of the message
  Height:=Label1.Top+
          Label1.Height+
          BOk.Height*2+
          BOk.Height div 2;
end;

procedure TFormUserMessage.BOkClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TFormUserMessage.PaintBox1Paint(Sender: TObject);
begin
  if FImage = NIL then exit;
  FImage.Draw(PaintBox1.Canvas, 0, 0, False);
end;

procedure TFormUserMessage.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
{$endif}
{$if defined(LCLCOCOA)}
  BOk.Flat := False;
  ChangeFontColor([BOk], clDefault);
{$endif}
end;

function TFormUserMessage.Init(const mess, sbutton: string; aMsgType: TMsgDlgType): integer;
begin
  Label1.Caption := mess;
  BOk.Caption := sbutton;

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

