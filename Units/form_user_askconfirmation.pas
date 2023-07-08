unit form_user_askconfirmation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, BGRABitmap;

type

  { TFormUserConfirmation }

  TFormUserConfirmation = class(TForm)
    Label1: TLabel;
    PaintBox1: TPaintBox;
    Shape1: TShape;
    BOk: TSpeedButton;
    BCancel: TSpeedButton;
    procedure FormKeyUp({%H-}Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Label1Resize({%H-}Sender: TObject);
    procedure BOkClick({%H-}Sender: TObject);
    procedure BCancelClick({%H-}Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FImage: TBGRABitmap;
    procedure AdjustFont;
  public
    function Init(const mess, sok, scancel: string; aMsgType: TMsgDlgType): integer;
  end;

var FormUserConfirmation: TFormUserConfirmation;



implementation
uses LCLType, u_utils{$ifdef LINUX}, u_common{$endif};


{$R *.lfm}

{ TFormUserConfirmation }

procedure TFormUserConfirmation.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TFormUserConfirmation.FormShow(Sender: TObject);
begin
  AdjustFont;
end;

procedure TFormUserConfirmation.Label1Resize(Sender: TObject);
begin
  // adjust the height of the windows according to the height of the message
  Height := Label1.Top+
            Label1.Height+
            BOk.Height*2+
            BOk.Height div 2;
end;

procedure TFormUserConfirmation.BOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormUserConfirmation.BCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormUserConfirmation.PaintBox1Paint(Sender: TObject);
begin
  if FImage = NIL then exit;
  FImage.Draw(PaintBox1.Canvas, 0, 0, False);
end;

procedure TFormUserConfirmation.AdjustFont;
begin
{$ifdef LINUX}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
{$endif}
end;

function TFormUserConfirmation.Init(const mess, sok, scancel: string; aMsgType: TMsgDlgType): integer;
begin
  Label1.Caption := mess;
  BOk.Caption := sok;
  BCancel.Caption := scancel;

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

