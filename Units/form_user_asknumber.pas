unit form_user_asknumber;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin;

type

  { TFormUserAskNumber }

  TFormUserAskNumber = class(TForm)
    BCancel: TSpeedButton;
    BOk: TSpeedButton;
    FloatSpinEdit1: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Shape1: TShape;
    SpinEdit1: TSpinEdit;
    procedure BCancelClick(Sender: TObject);
    procedure BOkClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Label1Resize(Sender: TObject);
  private
    procedure InitUnit(s: string);
    procedure AdjustFont;
  public
    function InitInteger(const mess, sok, scancel, aUnit: string; var AValue: integer): integer;
    function InitSingle(const mess, sok, scancel, aUnit: string; var AValue: single): single;
  end;

var
  FormUserAskNumber: TFormUserAskNumber;

implementation
uses LCLType{$if defined(Linux) or defined(Darwin)}, u_common, u_utils{$endif};

{$R *.lfm}

{ TFormUserAskNumber }

procedure TFormUserAskNumber.BOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormUserAskNumber.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TFormUserAskNumber.FormShow(Sender: TObject);
begin
  AdjustFont;
end;

procedure TFormUserAskNumber.Label1Resize(Sender: TObject);
begin
  // adjust the height of the windows according to the height of the message
  Height := Label1.Top+
            Label1.Height+
            BOk.Height*2+
            BOk.Height div 2;
end;

procedure TFormUserAskNumber.InitUnit(s: string);
var c: TControl;
begin
  Label2.Caption := s;
  if FloatSpinEdit1.Visible then
    c := FloatSpinEdit1
  else
    c := SpinEdit1;

  Label2.Left := c.Left+c.Width+ScaleDesignToForm(10);
end;

procedure TFormUserAskNumber.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
  {$if defined(LCLGTK2)}
  ChangeFontColor([FloatSpinEdit1], clBlack);
  {$endif}
{$endif}
{$if defined(LCLCOCOA)}
  BOk.Flat := False;
  BCancel.Flat := False;
{$endif}
end;

procedure TFormUserAskNumber.BCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TFormUserAskNumber.InitInteger(const mess, sok, scancel, aUnit: string;
  var AValue: integer): integer;
begin
  Label1.Caption := mess;
  BOk.Caption := sok;
  BCancel.Caption := scancel;
  SpinEdit1.Value := AValue;
  SpinEdit1.Visible := True;
  FloatSpinEdit1.Visible := False;
  InitUnit(aUnit);

  {$ifdef LCLGTK2}
  Hide;
  Application.ProcessMessages;
  {$endif}

  Result := ShowModal;

  if Result = mrOk then
    AValue := SpinEdit1.Value;
end;

function TFormUserAskNumber.InitSingle(const mess, sok, scancel, aUnit: string;
  var AValue: single): single;
begin
  Label1.Caption := mess;
  BOk.Caption := sok;
  BCancel.Caption := scancel;
  FloatSpinEdit1.Value := AValue;
  SpinEdit1.Visible := False;
  FloatSpinEdit1.Visible := True;
  InitUnit(aUnit);

  {$ifdef LCLGTK2}
  Hide;
  Application.ProcessMessages;
  {$endif}

  Result := ShowModal;

  if Result = mrOk then
    AValue := FloatSpinEdit1.Value;
end;

end.

