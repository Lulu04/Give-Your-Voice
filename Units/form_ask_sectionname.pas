unit form_ask_sectionname;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Buttons, LCL_utils;

type

  { TFormAskSectionName }

  TFormAskSectionName = class(TForm)
    BCancel: TSpeedButton;
    BOk: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Shape1: TShape;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    procedure BOkClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ComboBox1KeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure ComboBox1Select(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    CheckedLabelManager1: TCheckedLabelManager;
    FModeInsert: boolean;
    function GetWantedName: string;
    procedure UpdateNamePreview;
    procedure AdjustFont;
  public
    //initialize before calling showmodal
    property ModeInsert: boolean write FModeInsert;


    property WantedName: string read GetWantedName;
  end;

var
  FormAskSectionName: TFormAskSectionName;

implementation
uses LCLType, u_resource_string, u_project, u_utils, u_common, u_crossplatform,
  Math;

{$R *.lfm}

{ TFormAskSectionName }

procedure TFormAskSectionName.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckKeyToShowUserGuide(Key, Shift) then
    ShowGYVUserGuide;

  case Key of
    VK_ESCAPE: ModalResult := mrCancel;
  end;
end;

procedure TFormAskSectionName.FormShow(Sender: TObject);
begin
  AdjustFont;

  BCancel.Caption := SCancel;
  Label4.Caption := SNumber;
  Label7.Caption := SNumber;
  Label3.Caption := SPrefix;
  Label6.Caption := SPrefix;
  Label9.Caption := STitle;
  Label5.Caption := STitle;
  Label11.Caption := SPleaseNoSpecialCharacters;
  Label12.Caption := SPleaseNoSpecialCharacters;
  Label13.Caption := SPleaseNoSpecialCharacters;
  Label14.Caption := SPleaseNoSpecialCharacters;

  ComboBox1.Clear;
  ComboBox1.Items.Add(SChapter);
  ComboBox1.Items.Add(SText);
  ComboBox1.Items.Add(SPoem);
  ComboBox1.Items.Add(SPart);
  ComboBox1.Items.Add(SSection);
  ComboBox1.Text := 'custom';

  if FModeInsert then
    Label16.Caption := SNameOfTheSectionToInsert
  else
    Label16.Caption := SNameOfTheSectionToAdd;

  // updates widgets from project file
  with Project.Descriptor do begin
    CheckBox1.Checked := FolderHeader_IncludeBookInfo;
    Edit1.Text := FolderHeader_BookLetter;
    SpinEdit1.Value := FolderHeader_BookNumber;
    Edit2.Text := FolderHeader_BookTitle;
    CheckBox2.Checked := FolderHeader_IncludeSectionInfo;
    ComboBox1.Text := FolderHeader_SectionPrefix;
    if FModeInsert then
      SpinEdit2.Value := FolderHeader_SectionNumber
    else
      SpinEdit2.Value := FolderHeader_SectionNumber + 1;
  end;

  Edit4.SetFocus;

  CheckBox1Change(NIL);
end;

procedure TFormAskSectionName.SpinEdit1Change(Sender: TObject);
begin
  UpdateNamePreview;
end;

function TFormAskSectionName.GetWantedName: string;
var s: string;
begin
  Result := '';
  // book number
  if CheckBox1.Checked and not (Label11.Visible or Label12.Visible) then begin
    Result := Trim(Edit1.Text)+
              Format('%d', [SpinEdit1.Value]);
    ConcatToString(Result, ' ', Trim(Edit2.Text));
  end;
  // chapter number
  if CheckBox2.Checked and not Label13.Visible then begin
    s := Trim(ComboBox1.Text)+
         Format('%.'+Project.Descriptor.FolderHeader_DigitCountForSectionNumber.ToString+'d', [SpinEdit2.Value]);
    ConcatToString(Result, ' ', s);
  end;

  ConcatToString(Result, ' ', Trim(Edit4.Text));
end;

procedure TFormAskSectionName.UpdateNamePreview;
var flagError: boolean;
begin
  Label10.Caption := GetWantedName;

  flagError := CheckBox1.Checked and (label11.Visible or label12.Visible);
  flagError := flagError or (CheckBox1.Checked and label13.Visible);
  flagError := flagError or label14.Visible;

  flagError := flagError or ((Trim(Edit4.Text) = '') and not CheckBox2.Checked);

  BOk.Enabled := not flagError;

end;

procedure TFormAskSectionName.AdjustFont;
begin
  {$ifdef Linux}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
  ChangeFontHeight([Label3, Label4, Label5, Label6, Label7,
                    Label11, Label12, Label13, Label14, Label8], FDesignSmallFontHeight);
  ChangeFontColor([Edit1, SpinEdit1, Edit2, ComboBox1, SpinEdit2, Edit4], clBlack);
  {$endif}
end;

procedure TFormAskSectionName.FormCreate(Sender: TObject);
begin
  CheckedLabelManager1 := TCheckedLabelManager.Create;
  CheckedLabelManager1.CaptureLabelsClick([Label1, Label2]);
end;

procedure TFormAskSectionName.BOkClick(Sender: TObject);
begin
  if Sender = BCancel then ModalResult := mrCancel;

  if Sender = BOk then begin
    // save header info in project file
    with Project.Descriptor do begin
      FolderHeader_IncludeBookInfo := CheckBox1.Checked;
      if CheckBox1.Checked then begin
        FolderHeader_BookLetter := Trim(Edit1.Text);
        FolderHeader_BookNumber := SpinEdit1.Value;
        FolderHeader_BookTitle := Trim(Edit2.Text)
      end;

      FolderHeader_IncludeSectionInfo := CheckBox2.Checked;
      if CheckBox2.Checked then begin
        FolderHeader_SectionPrefix := Trim(ComboBox1.Text);
        FolderHeader_SectionNumber := SpinEdit2.Value;
      end;

      FolderHeader_DigitCountForSectionNumber := Max(FolderHeader_DigitCountForSectionNumber,
                                                     Length(SpinEdit2.Value.ToString));
    end;
    Project.Save;

    ModalResult := mrOk;
  end;
end;

procedure TFormAskSectionName.CheckBox1Change(Sender: TObject);
begin
  Panel1.Enabled := CheckBox1.Checked;
  Panel2.Enabled := CheckBox2.Checked;
  UpdateNamePreview;
end;

procedure TFormAskSectionName.ComboBox1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Label13.Visible := StringHaveForbiddenChar(ComboBox1.Text, FILENAMEFORBIDENCHARS);
  UpdateNamePreview;
end;

procedure TFormAskSectionName.ComboBox1Select(Sender: TObject);
begin
  UpdateNamePreview;
end;

procedure TFormAskSectionName.Edit1Change(Sender: TObject);
var editToCheck: TEdit;
  labelError: TLabel;
begin
  labelError := NIL;
  editToCheck := Sender as TEdit;
  if Sender = Edit1 then labelError := label11;
  if Sender = Edit2 then labelError := label12;
  if Sender = Edit4 then labelError := label14;

  labelError.Visible := StringHaveForbiddenChar(editToCheck.Text, FILENAMEFORBIDENCHARS);

  BOk.Enabled := not (label11.Visible or
                      label12.Visible or
                      label13.Visible or
                      label14.Visible);
  UpdateNamePreview;
end;

procedure TFormAskSectionName.FormDestroy(Sender: TObject);
begin
  CheckedLabelManager1.Free;
end;

end.

