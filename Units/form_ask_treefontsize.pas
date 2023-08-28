unit form_ask_treefontsize;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin;

type

  { TFormAskTreeFontSize }

  TFormAskTreeFontSize = class(TForm)
    BOk: TSpeedButton;
    Label16: TLabel;
    Shape1: TShape;
    SpinEdit1: TSpinEdit;
    procedure BOkClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    procedure AdjustFont;
  public

  end;

var
  FormAskTreeFontSize: TFormAskTreeFontSize;

implementation
uses u_utils, u_crossplatform, u_resource_string, u_program_options,
  u_datamodule, form_main, LCLType
  {$if defined(LCLGTK2) or defined(LCLCOCOA)} ,u_common{$endif};

{$R *.lfm}

{ TFormAskTreeFontSize }

procedure TFormAskTreeFontSize.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckKeyToShowUserGuide(Key, Shift) then
    ShowGYVUserGuide;

  case Key of
    VK_ESCAPE: ModalResult := mrCancel;
    VK_RETURN: ModalResult := mrOk;
  end;
end;

procedure TFormAskTreeFontSize.BOkClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAskTreeFontSize.FormShow(Sender: TObject);
begin
  AdjustFont;
  BOk.Caption := SOk;

  SpinEdit1.Value := ProgramOptions.ViewFilesFontHeight;
end;

procedure TFormAskTreeFontSize.SpinEdit1Change(Sender: TObject);
begin
  if ProgramOptions.ViewFilesFontHeight <> SpinEdit1.Value then begin
    ProgramOptions.ViewFilesFontHeight := SpinEdit1.Value;
    ProgramOptions.Save;

    FormMain.FrameViewProjectFiles1.TV.Font.Height := ScaleDesignToForm(SpinEdit1.Value);
    DataModule1.RedrawImageForTreeView;
  end;
end;

procedure TFormAskTreeFontSize.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
  {$if defined(LCLGTK2)}
  ChangeFontColor([SpinEdit1], clBlack);
  {$endif}
{$endif}
{$if defined(LCLCOCOA)}
  BOk.Flat := False;
{$endif}
end;

end.

