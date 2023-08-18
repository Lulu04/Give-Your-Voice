unit form_remembertodonate;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls;

type

  { TFormRememberToDonate }

  TFormRememberToDonate = class(TForm)
    BClose: TSpeedButton;
    BDonate: TSpeedButton;
    Label1: TLabel;
    Shape1: TShape;
    procedure BCloseClick(Sender: TObject);
    procedure BDonateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
   procedure AdjustFont;
  public

  end;

  procedure ShowWindowRemberUserToDonate;

var
  FormRememberToDonate: TFormRememberToDonate;

implementation

uses u_utils{$if defined(Linux) or defined(Darwin)},u_common{$endif};

procedure ShowWindowRemberUserToDonate;
begin
  FormRememberToDonate := TFormRememberToDonate.Create(NIL);
  FormRememberToDonate.ShowModal;
  FormRememberToDonate.Free;
end;

{$R *.lfm}

{ TFormRememberToDonate }

procedure TFormRememberToDonate.BDonateClick(Sender: TObject);
begin
  OpenURLToDonate;
  ModalResult := mrOk;
end;

procedure TFormRememberToDonate.FormShow(Sender: TObject);
begin
  AdjustFont;
end;

procedure TFormRememberToDonate.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
{$endif}
{$if defined(LCLCOCOA)}
  BDonate.Flat := False;
  BClose.Flat := False;
{$endif}
end;

procedure TFormRememberToDonate.BCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

