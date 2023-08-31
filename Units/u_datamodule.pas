unit u_datamodule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Dialogs, ImgList;

const
  // TOOL BAR 2   (ILToolBar2)
  IMAGE_INDEX_AUDIO = 0;
  IMAGE_INDEX_WEB = 1;
  IMAGE_INDEX_PROJECTMANAGER = 2;
  IMAGE_INDEX_OPTIONS = 3;
  IMAGE_INDEX_ABOUT = 4;
  IMAGE_INDEX_USERGUIDE = 5;

  // TREE VIEW (ILTreeView)
  IMAGE_INDEX_TV_ROOT = 0;
  IMAGE_INDEX_TV_SECTION = 1;
  IMAGE_INDEX_TV_SECTION_WITH_USERMARK = 2;
  IMAGE_INDEX_TV_RECORD = 3;
  IMAGE_INDEX_TV_RECORD_WITH_USERMARK = 4;
  IMAGE_INDEX_TV_FOLDER_MP3 = 5;
  IMAGE_INDEX_TV_FOLDER_ZIP = 6;
  IMAGE_INDEX_TV_FILE_MP3 = IMAGE_INDEX_TV_FOLDER_MP3;
  IMAGE_INDEX_TV_FILE_ZIP = IMAGE_INDEX_TV_FOLDER_ZIP;

  // some ImageList1 image index used by other units
  IMAGE_INDEX_IL1_FOLDER = 8;
  IMAGE_INDEX_IL1_CHECK_UNCHECKED = 42;
  IMAGE_INDEX_IL1_CHECK_CHECKED = 43;

  IMAGE_INDEX_IL1_ROOT = 18;
  IMAGE_INDEX_IL1_SECTION = 19;
  IMAGE_INDEX_IL1_RECORD = 20;
  IMAGE_INDEX_IL1_RECORD_WITH_USERMARK = 31;
  IMAGE_INDEX_IL1_SECTION_WITH_USERMARK = 32;
  IMAGE_INDEX_IL1_FOLDER_MP3 = 27;
  IMAGE_INDEX_IL1_FOLDER_ZIP = 36;
  IMAGE_INDEX_IL1_FILE_MP3 = IMAGE_INDEX_IL1_FOLDER_MP3;
  IMAGE_INDEX_IL1_FILE_ZIP = IMAGE_INDEX_IL1_FOLDER_ZIP;

  IMAGE_INDEX_IL1_MIXER_HIDE_VOLUME_ENVELOPE = 40;
  IMAGE_INDEX_IL1_MIXER_SHOW_VOLUME_ENVELOPE = 41;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    ApplicationProperties1: TApplicationProperties;
    ImageList1: TImageList;
    ILToolBar2: TImageList;
    ILTreeView: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    FPath: string;
    procedure AddImageToImageList(const aSVGFilename: string; aIL: TImageList);
  public
    procedure RedrawImageForTreeView;
    procedure RedrawImageForToolBar2;
    Procedure RedrawImageForCommon;
  end;

var
  DataModule1: TDataModule1;

implementation
uses utilitaire_bgrabitmap, BGRABitmap, u_crossplatform, form_main,
  u_program_options;

{$R *.lfm}

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  FPath := GetAppDataFolder;

  RedrawImageForTreeView;
  RedrawImageForToolBar2;

  RedrawImageForCommon;
end;

procedure TDataModule1.AddImageToImageList(const aSVGFilename: string;
  aIL: TImageList);
var ima: TBGRABitmap;
begin
  ima := SVGFileToBGRABitmap(FPath+aSVGFilename, aIL.Width, -1);
  aIL.Add(ima.Bitmap, NIL);
  ima.Free;
end;

procedure TDataModule1.RedrawImageForTreeView;
begin
  ILTreeView.BeginUpdate;
  ILTreeView.Clear;
  ILTreeView.Width := FormMain.ScaleDesignToForm(ProgramOptions.ViewFilesFontHeight);
  ILTreeView.Height := ILTreeView.Width;

  AddImageToImageList('TreeRoot.svg', ILTreeView);
  AddImageToImageList('Section.svg', ILTreeView);
  AddImageToImageList('SectionWithUserMark.svg', ILTreeView);
  AddImageToImageList('Record.svg', ILTreeView);
  AddImageToImageList('RecordWithUserMark.svg', ILTreeView);
  AddImageToImageList('TreeMP3Folder.svg', ILTreeView);
  AddImageToImageList('TreeZIPFolder.svg', ILTreeView);

  ILTreeView.EndUpdate;
end;

procedure TDataModule1.RedrawImageForToolBar2;
begin
  ILToolBar2.BeginUpdate;
  ILToolBar2.Clear;
  ILToolBar2.Width := FormMain.ScaleDesignToForm(32);
  ILToolBar2.Height := ILToolBar2.Width;

  AddImageToImageList('ToolBar2Audio.svg', ILToolBar2);
  AddImageToImageList('ToolBar2Web.svg', ILToolBar2);
  AddImageToImageList('ProjectManager.svg', ILToolBar2);
  AddImageToImageList('Options.svg', ILToolBar2);
  AddImageToImageList('ToolBar2About.svg', ILToolBar2);
  AddImageToImageList('DlgQuestion.svg', ILToolBar2);

  ILToolBar2.EndUpdate;
end;

procedure TDataModule1.RedrawImageForCommon;
begin
  ImageList1.BeginUpdate;
  ImageList1.Clear;
  ImageList1.Width := FormMain.ScaleDesignToForm(24);
  ImageList1.Height := ImageList1.Width;

  AddImageToImageList('Options.svg', ImageList1);
  AddImageToImageList('ClearBrush.svg', ImageList1);
  AddImageToImageList('Cissor.svg', ImageList1);
  AddImageToImageList('Redo.svg', ImageList1);
  AddImageToImageList('Undo.svg', ImageList1);
  AddImageToImageList('Checked.svg', ImageList1);
  AddImageToImageList('Cancel.svg', ImageList1);
  AddImageToImageList('Edit.svg', ImageList1);
  AddImageToImageList('Folder.svg', ImageList1);
  AddImageToImageList('FolderSearch.svg', ImageList1);
  AddImageToImageList('TrashCan.svg', ImageList1);
  AddImageToImageList('AudioPlay.svg', ImageList1);
  AddImageToImageList('AudioStop.svg', ImageList1);
  AddImageToImageList('AudioPause.svg', ImageList1);
  AddImageToImageList('AudioCursorToBegin.svg', ImageList1);
  AddImageToImageList('AudioCursorToEnd.svg', ImageList1);
  AddImageToImageList('AudioRecord.svg', ImageList1);
  AddImageToImageList('AudioSilenceOnSelection.svg', ImageList1);
  AddImageToImageList('TreeRoot.svg', ImageList1);
  AddImageToImageList('Section.svg', ImageList1);
  AddImageToImageList('Record.svg', ImageList1);
  AddImageToImageList('SectionAdd.svg', ImageList1);
  AddImageToImageList('RecordAdd.svg', ImageList1);
  AddImageToImageList('SectionMix.svg', ImageList1);
  AddImageToImageList('ZoomOnSelection.svg', ImageList1);
  AddImageToImageList('ZoomAll.svg', ImageList1);
  AddImageToImageList('AudioAddSilence.svg', ImageList1);
  AddImageToImageList('TreeMP3Folder.svg', ImageList1);
  AddImageToImageList('UserMarkAdd.svg', ImageList1);
  AddImageToImageList('UserMarkDeleteAll.svg', ImageList1);
  AddImageToImageList('UserMarkDeleteOnSelection.svg', ImageList1);
  AddImageToImageList('RecordWithUserMark.svg', ImageList1);
  AddImageToImageList('SectionWithUserMark.svg', ImageList1);
  AddImageToImageList('ProjectManager.svg', ImageList1);
  AddImageToImageList('ProjectManagerNew.svg', ImageList1);
  AddImageToImageList('ProjectManagerOpen.svg', ImageList1);
  AddImageToImageList('TreeZIPFolder.svg', ImageList1);
  AddImageToImageList('ProjectManagerUnzip.svg', ImageList1);
  AddImageToImageList('ProjectManagerZip.svg', ImageList1);
  AddImageToImageList('RedHeart.svg', ImageList1);
  AddImageToImageList('MixerVolumeEnvelopeHide.svg', ImageList1);
  AddImageToImageList('MixerVolumeEnvelopeShow.svg', ImageList1);
  AddImageToImageList('CheckBoxUnchecked.svg', ImageList1);
  AddImageToImageList('CheckBoxChecked.svg', ImageList1);
  AddImageToImageList('MP3ToZip.svg', ImageList1);
  AddImageToImageList('CheckAll.svg', ImageList1);
  AddImageToImageList('ToolBar2Audio.svg', ImageList1);
  AddImageToImageList('ToolBar2Web.svg', ImageList1);
  AddImageToImageList('SectionImportRecord.svg', ImageList1);
  AddImageToImageList('MixerVolumeCursor.svg', ImageList1);
  AddImageToImageList('DlgQuestion.svg', ImageList1);
  AddImageToImageList('ToolBar2About.svg', ImageList1);
  AddImageToImageList('MixerMuted.svg', ImageList1);
  AddImageToImageList('MixerInsertMusic.svg', ImageList1);
  AddImageToImageList('TreeFontHeight.svg', ImageList1);
  AddImageToImageList('MixerToggleMuted.svg', ImageList1);
  ImageList1.EndUpdate;
end;


end.

