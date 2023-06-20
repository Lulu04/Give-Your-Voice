unit form_mixermetadata;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, u_common;

type

  { TFormMixerMetaData }

  TFormMixerMetaData = class(TForm)
    BMix: TSpeedButton;
    EditAlbum: TEdit;
    EditTrack: TEdit;
    EditGenre: TEdit;
    EditDate: TEdit;
    EditTitle: TEdit;
    EditArtist: TEdit;
    EditComment: TEdit;
    Label17: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Shape1: TShape;
    procedure BMixClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FTargetPlatform: TProjectTargetPlatform;
    procedure AdjustFont;
  public
    property TargetPlatform: TProjectTargetPlatform read FTargetPlatform write FTargetPlatform;
  end;

var
  FormMixerMetaData: TFormMixerMetaData;

implementation
uses u_resource_string, form_main, u_program_options, u_utils, LCLType,
  u_project;

{$R *.lfm}

{ TFormMixerMetaData }

procedure TFormMixerMetaData.FormShow(Sender: TObject);
var s: String;
begin
  AdjustFont;

  EditDate.Text := CurrentYear.ToString;
  EditTitle.Text := FormMain.FrameViewProjectFiles1.GetTitleMetadataForCurrentFolder(FTargetPlatform);
  EditTrack.Text := FormMain.FrameViewProjectFiles1.GetTrackNumberMetadataForCurrentFolder(FTargetPlatform);

  EditGenre.Text := SAudioBook;
  EditComment.Text := Project.Descriptor.Comment;

  case FTargetPlatform of

    ptpLitteratureAudio: begin
      EditArtist.Text := 'www.litteratureaudio.com';
      EditArtist.ReadOnly := True;
      EditAlbum.Text := 'Livres audio gratuits';
      EditAlbum.ReadOnly := True;
    end;

    ptpLibriVox, ptpNone: begin
      if Project.Descriptor.Artist = '' then begin
        s := '';
        ConcatToString(s, ' ', Project.Descriptor.AuthorFirstName);
        ConcatToString(s, ' ', Project.Descriptor.AuthorLastName);
        EditArtist.Text := s+' ';
      end else
        EditArtist.Text := Project.Descriptor.Artist;
      if Project.Descriptor.Artist = '' then
        EditAlbum.Text := Project.Descriptor.Title
      else
        EditAlbum.Text := Project.Descriptor.Album;
    end;

    else Raise Exception.Create('to do !');
  end;//case
end;

procedure TFormMixerMetaData.BMixClick(Sender: TObject);
begin
  if Sender = BMix then begin
    // save user metadata to project file
    Project.Descriptor.Artist := Trim(EditArtist.Text);
    Project.Descriptor.Album := Trim(EditAlbum.Text);
    Project.Descriptor.Comment := Trim(EditComment.Text);
    Project.Descriptor.Genre := Trim(EditGenre.Text);
    Project.Save;

    ModalResult := mrOk;
  end;
end;

procedure TFormMixerMetaData.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: ModalResult := mrCancel;
    VK_F1: ShowGYVUserGuide;
  end;
end;

procedure TFormMixerMetaData.AdjustFont;
begin
  {$ifdef LINUX}
    ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
    ChangeFontHeight([Label30], FDesignSmallFontHeight);
    ChangeFontColor([EditTitle, EditArtist, EditAlbum, EditGenre, EditDate,
                     EditComment, EditTrack], clBlack);
  {$endif}
end;

end.

