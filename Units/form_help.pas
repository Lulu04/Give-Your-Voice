unit form_help;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TFormHelp }

  TFormHelp = class(TForm)
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    procedure AdjustFont;
  public
    HelpButton: TSpeedButton;

  end;

procedure _ShowHelp(const aMultiLineTextHelp: string; aHelpButton: TSpeedButton;
                    ShowBigWindow: boolean=False);


implementation
uses form_main, Math, LCLType{$if defined(Linux) or defined(Darwin)},u_utils, u_common{$endif};

procedure _ShowHelp(const aMultiLineTextHelp: string; aHelpButton: TSpeedButton;
                    ShowBigWindow: boolean);
var p: TPoint;
  w, h: integer;
begin
  if aHelpButton.Tag <> 0 then exit;
  aHelpButton.Tag := 1;

  if ShowBigWindow then begin
    w := FormMain.ScaleDesignToForm(800);
    h := FormMain.ScaleDesignToForm(400);
  end else begin
    w := FormMain.ScaleDesignToForm(450);
    h := FormMain.ScaleDesignToForm(200);
  end;

  p := aHelpButton.ClientRect.CenterPoint;
  p := aHelpButton.ClientToScreen(p);

  with TFormHelp.Create(NIL) do begin
    SetBounds(EnsureRange(p.x, FormMain.Left, FormMain.Left+FormMain.Width-w),
              EnsureRange(p.y, FormMain.Top, FormMain.Top+FormMain.Height-h),
              w, h);
    Memo1.Clear;
    Memo1.Lines.AddText(aMultiLineTextHelp);
    HelpButton := aHelpButton;
    Show;
  end;
end;

{$R *.lfm}

{ TFormHelp }

procedure TFormHelp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormHelp.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TFormHelp.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TFormHelp.FormShow(Sender: TObject);
begin
  AdjustFont;
end;

procedure TFormHelp.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  //ChangeFontHeightOnFormChilds(Self, FDesignFontHeight-2);
  Memo1.Font.Name := 'Arial';
  Memo1.Font.Style := [fsBold];
  Memo1.Font.Height := ScaleDesignToForm(FDesignFontHeight-2);
{$endif}
{$if defined(Windows)}
  Memo1.Font.Name := 'Arial';
  Memo1.Font.Style := [fsBold];
  Memo1.Font.Height := ScaleDesignToForm(16);
{$endif}
end;

end.

