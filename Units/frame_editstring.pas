unit frame_editstring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics;


type

  { TFrameEditString }

  TFrameEditString = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Panel1: TPanel;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FOnTextChange: TNotifyEvent;
    function GetFontHeight: integer;
    function GetText: string;
    function GetTitle: string;
    procedure SetFontHeight(AValue: integer);
    procedure SetText(AValue: string);
    procedure SetTitle(AValue: string);
  private
    FForbiddenChars: TStringArray;
    FAllowEmptyString: boolean;
    FOnEnterPressed: TNotifyEvent;
    function GetFontStyle: TFontStyles;
    function GetHaveFocus: boolean;
    function GetReadOnly: boolean;
    function GetTextIsValid: boolean;
    procedure SetFontStyle(AValue: TFontStyles);
    procedure SetReadOnly(AValue: boolean);
  public
    constructor Create(aOwner: TComponent); override;
    procedure AdjustFont;

    procedure SetTheFocus;
    procedure ClearSelection;

    procedure Init(aAllowEmptyString,
                   aNumberOnly: boolean;
                   const aForbidenChar: TStringArray=NIL;
                   const aMessageBadChar: string='');

    property Title: string read GetTitle write SetTitle;
    property Text: string read GetText write SetText;

    property FontHeight: integer read GetFontHeight write SetFontHeight;
    property FontStyles: TFontStyles read GetFontStyle write SetFontStyle;

    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
    property TextIsValid: boolean read GetTextIsValid;
    property HaveFocus: boolean read GetHaveFocus;
    property OnEnterPressed: TNotifyEvent read FOnEnterPressed write FOnEnterPressed;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
  end;

implementation

uses u_common, u_utils, LCLType, LazUTF8;

{$R *.lfm}


{ TFrameEditString }

procedure TFrameEditString.Edit1Change(Sender: TObject);
begin
  if (not FAllowEmptyString and (Edit1.Text = '')) then
    Label2.Visible := False
  else
    Label2.Visible := not TextIsValid and not FAllowEmptyString;

  if FOnTextChange <> NIL then
    FOnTextChange(Self);
end;

procedure TFrameEditString.Edit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if FOnEnterPressed <> NIL then
      FOnEnterPressed(Self);
end;

function TFrameEditString.GetFontHeight: integer;
begin
  Result := Label1.Font.Height;
end;

function TFrameEditString.GetText: string;
begin
  Result := Edit1.Text;
end;

function TFrameEditString.GetTitle: string;
begin
  Result := Label1.Caption;
end;

procedure TFrameEditString.SetFontHeight(AValue: integer);
begin
  Label1.Font.Height := AValue;
  Edit1.Font.Height := AValue;
  Height := AValue*2+Label2.Font.Height+2;
end;

procedure TFrameEditString.SetText(AValue: string);
begin
  Edit1.Text := AValue;
  Edit1Change(NIL);
end;

procedure TFrameEditString.SetTitle(AValue: string);
begin
  Label1.Caption := AValue;
end;

function TFrameEditString.GetTextIsValid: boolean;
var i: integer;
begin
  Result := False;

  if not FAllowEmptyString and (Trim(Edit1.Text) = '') then exit;

  if Edit1.NumbersOnly and not TryStrToInt(Edit1.Text, i) then exit;

  Result := not StringHaveForbiddenChar(Trim(Edit1.Text), FForbiddenChars);
end;

procedure TFrameEditString.SetFontStyle(AValue: TFontStyles);
begin
  Edit1.Font.Style := AValue;
end;

procedure TFrameEditString.SetReadOnly(AValue: boolean);
begin
  Edit1.ReadOnly := AValue;
end;

procedure TFrameEditString.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeight([Self, Label1, Edit1], FDesignFontHeight);
  ChangeFontHeight([Label2], FDesignFontHeight-3);
  ChangeFontColor([Edit1], clBlack);
{$endif}
end;

constructor TFrameEditString.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  AdjustFont;
end;

function TFrameEditString.GetHaveFocus: boolean;
begin
  Result := Edit1.Focused;
end;

function TFrameEditString.GetFontStyle: TFontStyles;
begin
  Result := Edit1.Font.Style;
end;

function TFrameEditString.GetReadOnly: boolean;
begin
  Result := Edit1.ReadOnly;
end;

procedure TFrameEditString.SetTheFocus;
begin
  Edit1.SelectAll;
  Edit1.SetFocus;
end;

procedure TFrameEditString.ClearSelection;
begin
  Edit1.ClearSelection;
end;

procedure TFrameEditString.Init(aAllowEmptyString, aNumberOnly: boolean;
  const aForbidenChar: TStringArray; const aMessageBadChar: string);
begin
  FAllowEmptyString := aAllowEmptyString;
  Label2.Caption := aMessageBadChar;
  Label2.Visible := False;

  if Length(aForbidenChar) = 0 then begin
    FForbiddenChars := NIL;
  end else begin
    FForbiddenChars := Copy(aForbidenChar, 0, Length(aForbidenChar));
  end;

  Edit1.NumbersOnly := aNumberOnly;
  if aNumberOnly then
    Label2.Caption := '';
end;


end.

