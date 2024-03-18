unit frame_zipfiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  Zipper, frame_progressbar;

type

  { TFrameZipFiles }

  TFrameZipFiles = class(TFrame)
    BCancelZipper: TSpeedButton;
    Label2: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure BCancelZipperClick(Sender: TObject);
  private
    FZipper: TZipper;
    FUnzipper: TUnzipper;
    FFileCountToProcess,
    FProcessedFileCount: integer;
    FCanceled: boolean;
    FrameProgressBar1,
    FrameProgressBar2: TFrameProgressBar;
    procedure ProcessZipperOnProgressEvent(Sender: TObject; const Pct: Double);
    procedure ProcessZipperOnStartFileEvent(Sender: TObject; Const AFileName: String);
  public
    constructor Create(aOwner: TComponent); override;
    procedure AdjustFont;

    function ZipFiles(const zipFilename,
                       aSubFolderContainerInZip: string;
                       const aFilesToInclude: TStringArray): boolean;

    procedure ClearLabels;

    // True if user have canceled the operation
    property Canceled: boolean read FCanceled;
  end;

implementation

uses u_resource_string, utilitaire_fichier
  {$if defined(Linux) or defined(Darwin)},u_utils, u_common{$endif}
  {$if defined(Darwin)}, Process{$endif};

{$R *.lfm}

{ TFrameZipFiles }

procedure TFrameZipFiles.BCancelZipperClick(Sender: TObject);
begin
  if (Sender = BCancelZipper) and (FZipper <> NIL) then
    FZipper.Terminate;
  if (Sender = BCancelZipper) and (FUnzipper <> NIL) then
    FUnzipper.Terminate;
end;

procedure TFrameZipFiles.ProcessZipperOnProgressEvent(Sender: TObject;
  const Pct: Double);
begin
  FrameProgressBar1.Position := Pct*0.01;
  Application.ProcessMessages;
end;

procedure TFrameZipFiles.ProcessZipperOnStartFileEvent(Sender: TObject;
  const AFileName: String);
begin
  Label2.Caption := NomDuDernierSousRepertoire(ExtractFilePath(AFileName))+
                    ExtractFilename(AFileName);
  inc(FProcessedFileCount);
  Label4.Caption := FProcessedFileCount.ToString+'/'+FFileCountToProcess.ToString;
  FrameProgressBar2.Position := FProcessedFileCount/FFileCountToProcess;
  Application.ProcessMessages;
end;

constructor TFrameZipFiles.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FrameProgressBar1 := TFrameProgressBar.Create(Self);
  FrameProgressBar1.Name := 'FrameProgressBar1';
  FrameProgressBar1.Parent := Panel1;
  FrameProgressBar1.Align := alClient;

  FrameProgressBar2 := TFrameProgressBar.Create(Self);
  FrameProgressBar2.Name := 'FrameProgressBar2';
  FrameProgressBar2.Parent := Panel2;
  FrameProgressBar2.Align := alClient;

  BCancelZipper.Caption := SCancel;

  ClearLabels;
end;

procedure TFrameZipFiles.AdjustFont;
begin
{$if defined(LCLGTK2) or defined(LCLCOCOA)}
  ChangeFontHeightOnFormChilds(Self, FDesignFontHeight);
{$endif}
{$if defined(LCLCOCOA)}
  BCancelZipper.Flat := False;
{$endif}
end;

function TFrameZipFiles.ZipFiles(const zipFilename,
  aSubFolderContainerInZip: string; const aFilesToInclude: TStringArray): boolean;
var i: integer;
  flagError: boolean;
  unzipper: TUnZipper;
  zipSubFolder: string;
begin
  FFileCountToProcess := Length(aFilesToInclude);
  FProcessedFileCount := 0;
  FCanceled := False;
  Result := False;
  if FFileCountToProcess = 0 then exit;

  zipSubFolder := aSubFolderContainerInZip;
  if zipSubFolder <> '' then
    zipSubFolder := IncludeTrailingPathDelimiter(aSubFolderContainerInZip);

  FZipper := TZipper.Create;
  flagError := False;
  try
    try
     FZipper.FileName := zipFilename;
     for i:=0 to High(aFilesToInclude) do
       FZipper.Entries.AddFileEntry(aFilesToInclude[i], zipSubFolder+ExtractFileName(aFilesToInclude[i]));
     FZipper.OnProgress := @ProcessZipperOnProgressEvent;
     FZipper.OnStartFile := @ProcessZipperOnStartFileEvent;
     // By default zipper writes file names in encoding of the IBM PC, CP437.
     // UTF8 encoding is written when UseLanguageEncoding is true.
     FZipper.UseLanguageEncoding := true;  // Requires FPC 3.2+
     FZipper.ZipAllFiles;
    except
     flagError := true;
    end;

    // verify the zip
    if not FZipper.Terminated and not flagError then begin
      unzipper := TUnZipper.Create;
      try
       unzipper.UseUTF8 := True;
       unzipper.FileName := zipFilename;
       unzipper.OutputPath := '';
       unzipper.Examine;
       flagError := unzipper.Entries.Count <> FZipper.Entries.Count;
       i := 0;
       while (i < unzipper.Entries.Count) and not flagError do begin
        flagError := unzipper.Entries[i].Size <> FZipper.Entries[i].Size;
        inc(i);
       end;
      finally
       unzipper.Free;
      end;
    end;

    // user have canceled ?
    if (FZipper.Terminated or flagError) and FichierExistant(zipFilename) then
      SupprimeFichier(zipFilename);

    FCanceled := FZipper.Terminated;
  finally
    FZipper.Free;
    FZipper := NIL;
  end;
  Result := not flagError;
end;

procedure TFrameZipFiles.ClearLabels;
begin
  Label2.Caption := ' ';
  Label4.Caption := ' ';
end;

end.

