unit form_new_project;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls;

type

  { TFormNewProject }

  TFormNewProject = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label11: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label28: TLabel;
    Label4: TLabel;
    Shape1: TShape;
    BCreateProject: TSpeedButton;
    procedure Edit1Change(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure BCreateProjectClick(Sender: TObject);
  private
    procedure AdjustFont;
  public
    procedure InitDescriptor(p: pointer);
  end;

var
  FormNewProject: TFormNewProject;

implementation
uses u_project, u_program_options, u_resource_string, u_userdialogs,
  utilitaire_fichier, u_utils, u_common, u_crossplatform, LCLType;

{$R *.lfm}

{ TFormNewProject }

procedure TFormNewProject.BCreateProjectClick(Sender: TObject);
var d: TProjectDescriptor;
begin
  if (Trim(Edit1.Text) = '') and
     (Trim(Edit2.Text) = '') and
     (Trim(Edit3.Text) = '') then exit;

  if Label11.Visible or
     Label23.Visible or
     Label24.Visible then exit;

  // check if an project with the same name exists
  InitDescriptor(@d);

  if RepertoireExistant(Project.ProjectsFolder+d.GetProjectSubFolderName) then begin
    ShowMess(SProjectWithSameNameExist, SOk, mtError);
    exit;
  end;


  // ProgramOptions target platform
  ProgramOptions.MixedFileMP3GaindBx10 := 890;
  ProgramOptions.MixedFileUseMP3Gain := ProgramOptions.MixedFileTargetPlatform <> ptpNone;
  ProgramOptions.Save;

  ModalResult := mrOk;
end;

procedure TFormNewProject.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
  ChangeFontColor([Edit1, Edit2, Edit3], clBlack);
  ChangeFontHeight([Label11, Label23, Label24], FDesignSmallFontHeight);
{$endif}
{$if defined(LCLCOCOA)}
  BCreateProject.Flat := False;
  ChangeFontColor([BCreateProject, Edit1, Edit2, Edit3], clDefault);
{$endif}
end;

procedure TFormNewProject.InitDescriptor(p: pointer);
var d: PProjectDescriptor;
begin
  d := PProjectDescriptor(p);
  d^.InitDefault;
  d^.AuthorFirstName := Trim(Edit1.Text);
  d^.AuthorLastName := Trim(Edit2.Text);
  d^.Title := Trim(Edit3.Text);
end;

procedure TFormNewProject.Edit1Change(Sender: TObject);
begin
  // error labels
  Label11.Visible := StringHaveForbiddenChar(Edit1.Text, FILENAMEFORBIDENCHARS);
  Label23.Visible := StringHaveForbiddenChar(Edit2.Text, FILENAMEFORBIDENCHARS);
  Label24.Visible := StringHaveForbiddenChar(Edit3.Text, FILENAMEFORBIDENCHARS);
end;

procedure TFormNewProject.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckKeyToShowUserGuide(Key, Shift) then
    ShowGYVUserGuide;

  case Key of
    VK_ESCAPE: ModalResult := mrCancel;
  end;
end;

procedure TFormNewProject.FormShow(Sender: TObject);
begin
  AdjustFont;

  Label11.Caption := SPleaseNoSpecialCharacters;
  Label23.Caption := SPleaseNoSpecialCharacters;
  Label24.Caption := SPleaseNoSpecialCharacters;
end;

end.

