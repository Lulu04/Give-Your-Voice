unit u_main_undoredo;

{$mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  undo_redo_manager, u_common, u_utils;



type

  // notification types for undo/redo
  TMainUndoRedoNotificationType = (urntCutAudio,
                                   urntSilenceOnSelection,
                                   //urntInsertSilence,
                                   urntDeleteUserMarksOnSelection,
                                   urntDeleteAllUserMarks,
                                   urntAddUserMark,
                                   urntInsertRecord,
                                   urntReplaceSelectionByRecord);

  { TMainUndoRedoItem }

  TMainUndoRedoItem = record
    Caption: string;
    Notification: TMainUndoRedoNotificationType;
    AudioFile: string;
    FrameIndex: TIndexInterval;
    UserMarks: TUserMarks;
    ReplaceIndex: TIndexInterval;
    procedure InitDefault;
    procedure CopyUserMarksFrom(aP: PSingle; aCount: integer);
    procedure CopyUserMarksFrom(const aItem: TMainUndoRedoItem);
    procedure CopyUserMarksFrom(const A: TUserMarks);
  end;

type

  { TMainUndoRedoManager }

  TMainUndoRedoManager = class(specialize TCustomUndoRedoManager<TMainUndoRedoItem>)
  private
    FFileSuffix: integer;
    // gives ex: TempFolder/UndoRedo001 (no extension)
    function GetNextUndoRedoFilename: string;
    function GenerateUndoRedoFilename(aSuffix: integer): string;
  private
    FLastSuccess: boolean;
    FLastSavedFilename: string;
    FLastFrameIndex: TIndexInterval;
    FUserMarksToSave: TUserMarks;
  protected
    procedure DoUndoRedo(ItsUndo: boolean); override;
    procedure DestroyItem(constref aItem: TMainUndoRedoItem); override;
  public
    function SaveAudioDataFrom(const aFilename: string;
                               aFirstFrameIndex,
                               aLastFrameIndex: int64): string;
    procedure SaveUserMarksValues(aUserMarks: TUserMarks; FromIndex, aCount: integer);

    procedure PushToUndo_AudioCutAction(const aCaption: string);

    procedure PushToUndo_AudioSilenceOnSelection(const aCaption: string);

    procedure PushToUndo_DeleteUserMarksInSelection(const aCaption: string;
                                       aFirstFrameIndex, aLastFrameIndex: int64);
    procedure PushToUndo_DeleteAllUserMarks(const aCaption: string;
                                       aFirstFrameIndex, aLastFrameIndex: int64);
    procedure PushToUndo_AddUserMarkAtCursor(const aCaption: string; aFrameIndex: int64);

    procedure PushToUndo_InsertRecord(const aCaption: string;
                                      aFirstFrameIndex, aLastFrameIndex: int64;
                                      const aUserMarks: TUserMarks);

    procedure PushToUndo_ReplaceSelectionByRecord(const aCaption: string;
                                      aFirstFrameIndex, aLastFrameIndex: int64;
                                      const aUserMarks: TUserMarks;
                                      aReplacedFrameCount: int64);

    function CurrentUndoCaption: string;
    function CurrentRedoCaption: string;

    procedure Clear; override;
  end;

var
  MainUndoRedoManager: TMainUndoRedoManager;

implementation
uses u_audio_utils, u_project, form_main, u_userdialogs,
  frame_viewaudio, u_resource_string, utilitaire_fichier, u_logfile, Dialogs;

{ TMainUndoRedoItem }

procedure TMainUndoRedoItem.InitDefault;
begin
  Caption := '';
//  Notification := ?;
  AudioFile := '';
  FrameIndex.InitDefault;
  UserMarks := NIL;
  ReplaceIndex.InitDefault;
end;

procedure TMainUndoRedoItem.CopyUserMarksFrom(aP: PSingle; aCount: integer);
begin
  UserMarks := NIL;
  if aCount <= 0 then exit;
  SetLength(UserMarks, aCount);
  Move(aP[0], UserMarks[0], aCount*SizeOf(Single));
end;

procedure TMainUndoRedoItem.CopyUserMarksFrom(const aItem: TMainUndoRedoItem);
begin
  UserMarks := NIL;
  if Length(aItem.UserMarks) = 0 then exit;
  CopyUserMarksFrom(@aItem.UserMarks[0], Length(aItem.UserMarks));
end;

procedure TMainUndoRedoItem.CopyUserMarksFrom(const A: TUserMarks);
begin
  UserMarks := NIL;
  if Length(A) > 0 then
    CopyUserMarksFrom(@A[0], Length(A));
end;

{ TMainUndoRedoManager }

function TMainUndoRedoManager.GetNextUndoRedoFilename: string;
begin
  inc(FFileSuffix);
  Result := GenerateUndoRedoFilename(FFileSuffix);
end;

function TMainUndoRedoManager.GenerateUndoRedoFilename(aSuffix: integer): string;
begin
  Result := Project.TempFolder+
            UNDO_REDO_BASE_FILENAME+Format('%.3d', [aSuffix]);
end;

procedure TMainUndoRedoManager.DoUndoRedo(ItsUndo: boolean);
var itemPoped, itemToPush: TMainUndoRedoItem;
  renderer: TFrameViewAudio;
  tempFile, s: string;
  procedure ShowError;
  begin
    if ItsUndo
      then s := Format(SUndoCommandFailed, [itemPoped.Caption])
      else s := Format(SRedoCommandFailed, [itemPoped.Caption]);
    ShowMess(s, SClose, mtError);
  end;
begin
  renderer := FormMain.FrameViewAudio1;
  if ItsUndo
    then itemPoped := PopFromUndo
    else itemPoped := PopFromRedo;

  itemToPush.InitDefault;
  itemToPush.Caption := Copy(itemPoped.Caption, 1, Length(itemPoped.Caption));
  itemToPush.Notification := itemPoped.Notification;
  itemToPush.FrameIndex.Create(itemPoped.FrameIndex);
  itemToPush.AudioFile := Copy(itemPoped.AudioFile, 1, Length(itemPoped.AudioFile));
  itemToPush.ReplaceIndex.Create(itemPoped.ReplaceIndex);
//  itemToPush.CopyUserMarksFrom(itemPoped);

  case itemPoped.Notification of

    urntReplaceSelectionByRecord: begin
     // same scenario for undo and redo
     // save the selection to an undoFile
     tempFile := GetNextUndoRedoFilename;
     if not CopyPartOfAudioFile(renderer.Filename,
                                tempFile,
                                itemPoped.ReplaceIndex.First,
                                itemPoped.ReplaceIndex.Last) then begin
       ShowError;
       exit;
     end;
     // replace the selection by the last audio saved
     if not ReplacePartByAudioFile(renderer.Filename,
                                   itemPoped.ReplaceIndex.First,
                                   itemPoped.ReplaceIndex.Last,
                                   itemPoped.AudioFile) then begin
       SupprimeFichier(tempFile);
       ShowError;
       exit;
     end;
     // save the current user marks then replace them by the poped ones
     itemToPush.UserMarks.Create(renderer.UserMarks);
     renderer.ReplaceUserMarksBy(itemPoped.UserMarks);
     // swap indexes
     itemToPush.FrameIndex.SwapWith(@itemToPush.ReplaceIndex);
     // set the new name for saved audio
     itemToPush.AudioFile := tempFile;
     // delete the old saved audio
     SupprimeFichier(itemPoped.AudioFile);
     renderer.ReloadPartFromFrameToEnd(itemPoped.FrameIndex.First);
     renderer.SetSelection(renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.First),
                           renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.Last));

     if ItsUndo
       then PushToRedo(itemToPush)
       else PushToUndo(itemToPush);
    end;

    urntInsertRecord: begin
      if ItsUndo then begin
        // save the selection to an undoFile
        itemToPush.AudioFile := GetNextUndoRedoFilename;
        if not CopyPartOfAudioFile(renderer.Filename,
                                   itemToPush.AudioFile,
                                   itemPoped.FrameIndex.First,
                                   itemPoped.FrameIndex.Last) then begin
          ShowError;
          exit;
        end;
        // cut the part
        if not CutAudioFile(renderer.Filename,
                            itemPoped.FrameIndex.First,
                            itemPoped.FrameIndex.Last) then begin
          ShowError;
          SupprimeFichier(itemToPush.AudioFile);
          exit;
        end;

        renderer.ReloadPartFromFrameToEnd(itemPoped.FrameIndex.First);
        itemToPush.UserMarks.Create(renderer.UserMarks);   // current userMarks in item to push
        renderer.ReplaceUserMarksBy(itemPoped.UserMarks);  // poped userMarks in current

        renderer.SetCursorToPos(renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.First));
        PushToRedo(itemToPush);
      end else begin
        if InsertAudioFile(renderer.Filename,
                           itemPoped.AudioFile,
                           itemPoped.FrameIndex.First) then begin

          renderer.ReloadPartFromFrameToEnd(itemPoped.FrameIndex.First);
          itemToPush.UserMarks.Create(renderer.UserMarks); //current userMarks in item to push
          renderer.ReplaceUserMarksBy(itemPoped.UserMarks);  // poped userMarks in current

          renderer.SetSelection(renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.First),
                                renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.Last));
          SupprimeFichier(itemPoped.AudioFile);
          itemToPush.AudioFile := '';
          PushToUndo(itemToPush);
        end else
          ShowError;
      end;
    end;

    urntCutAudio: begin
     if ItsUndo then begin
       if InsertAudioFile(renderer.Filename,
                          itemPoped.AudioFile, itemPoped.FrameIndex.First) then begin
         itemToPush.UserMarks.Create(renderer.UserMarks);
         renderer.ReplaceUserMarksBy(itemPoped.UserMarks);
         renderer.ReloadPartFromFrameToEnd(itemPoped.FrameIndex.First);
         renderer.SetSelection(renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.First),
                               renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.Last));
         PushToRedo(itemToPush);
       end else
       ShowError;
     end else begin
       if CutAudioFile(renderer.Filename,
                       itemPoped.FrameIndex.First,
                       itemPoped.FrameIndex.Last) then begin
   {    if renderer.DoCutPart(renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.First),
                             renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.Last)) then begin  }
         itemToPush.UserMarks.Create(renderer.UserMarks);
         renderer.ReplaceUserMarksBy(itemPoped.UserMarks);
         renderer.ReloadPartFromFrameToEnd(itemPoped.FrameIndex.First);
         renderer.SetCursorToPos(renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.First));
         PushToUndo(itemToPush);
       end else
       ShowError;
     end;
    end;

    urntSilenceOnSelection: begin
      if ItsUndo then begin
        if ReplacePartByAudioFile(renderer.Filename,
                                  itemPoped.FrameIndex.First,
                                  itemPoped.FrameIndex.Last,
                                  itemPoped.AudioFile) then begin
           PushToRedo(itemToPush);
           renderer.ReloadPart(itemPoped.FrameIndex.First, itemPoped.FrameIndex.Last);
           renderer.SetSelection(renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.First),
                                 renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.Last));
        end
        else ShowError;
      end else begin
        if renderer.DoSilenceOnPart(itemPoped.FrameIndex.First,
                                    itemPoped.FrameIndex.Last) then begin
          renderer.SetSelection(renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.First),
                                renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.Last));
          PushToUndo(itemToPush);
        end
        else ShowError;
      end;
    end;

    urntDeleteUserMarksOnSelection: begin
      if ItsUndo then begin
        renderer.MergeUserMarks(itemPoped.UserMarks);
        renderer.SetSelection(renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.First),
                              renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.Last));
        PushToRedo(itemToPush);
      end else begin
        renderer.SetSelection(renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.First),
                              renderer.FrameIndexToLevelIndex(itemPoped.FrameIndex.Last));
        itemToPush.UserMarks.Create(renderer.GetUserMarksInSelection);
        renderer.DeleteUserMarksInSelection(False);
        PushToUndo(itemToPush);
      end;
    end;

    urntDeleteAllUserMarks: begin
      if ItsUndo then begin
        renderer.MergeUserMarks(itemPoped.UserMarks);
        PushToRedo(itemToPush);
      end else begin
        itemToPush.UserMarks.Create(renderer.UserMarks);
        renderer.DeleteAllUserMarks(False);
        renderer.Redraw;
        PushToUndo(itemToPush);
      end;
    end;

    urntAddUserMark: begin
      if ItsUndo then begin
        renderer.DeleteUserMarkAtFrameIndex(itemPoped.FrameIndex.First);
        PushToRedo(itemToPush);
      end else begin
        renderer.AddUserMarkAtFrameIndex(itemPoped.FrameIndex.First);
        PushToUndo(itemToPush);
      end;
    end;

    else Raise Exception.Create('Case not implemented !!');
  end;
end;

procedure TMainUndoRedoManager.DestroyItem(constref aItem: TMainUndoRedoItem);
begin
  Log.Info('gyv: TMainUndoRedoManager.DestroyItem');
  // delete the undo/redo file
  if FichierExistant(aItem.AudioFile) then
    SupprimeFichier(aItem.AudioFile);
end;

function TMainUndoRedoManager.SaveAudioDataFrom(const aFilename: string;
  aFirstFrameIndex, aLastFrameIndex: int64): string;
var f: string;
begin
  FLastSuccess := False;
  Result := '';

  f := GetNextUndoRedoFilename;

  if not CopyPartOfAudioFile(aFilename, f, aFirstFrameIndex, aLastFrameIndex) then begin
    dec(FFileSuffix);
    if FichierExistant(f) then SupprimeFichier(f);
    Log.Error('    TMainUndoRedoManager.SaveAudioDataFrom: CopyPartOfAudioFile(...) fail'+LineEnding+
              '    src file "'+aFilename+'"'+LineEnding+
              '    '+GetFileInfoForLogMessage(aFilename)+LineEnding+
              '    dst file "'+f+'"'+LineEnding+
              '    frames indexes '+aFirstFrameIndex.ToString+' to '+aLastFrameIndex.ToString);
    exit;
  end;

  FLastSuccess := True;
  FLastSavedFilename := f;
  FLastFrameIndex.Create(aFirstFrameIndex, aLastFrameIndex);
  Result := f;
end;

procedure TMainUndoRedoManager.SaveUserMarksValues(aUserMarks: TUserMarks;
  FromIndex, aCount: integer);
begin
  FUserMarksToSave := NIL;
  if Length(aUserMarks) = 0 then exit;
  if aCount <= 0 then exit;

  FUserMarksToSave := Copy(aUserMarks, FromIndex, aCount);
end;

procedure TMainUndoRedoManager.PushToUndo_AudioCutAction(const aCaption: string);
var o: TMainUndoRedoItem;
begin
  if not FLastSuccess then exit;

  o.Caption := aCaption;
  o.AudioFile := FLastSavedFilename;
  o.FrameIndex.Create(FLastFrameIndex);
  o.Notification := urntCutAudio;
  o.UserMarks.Create(FUserMarksToSave);
  PushToUndo(o);
end;

procedure TMainUndoRedoManager.PushToUndo_AudioSilenceOnSelection(const aCaption: string);
var o: TMainUndoRedoItem;
begin
  if not FLastSuccess then exit;

  o.Caption := aCaption;
  o.AudioFile := FLastSavedFilename;
  o.FrameIndex.Create(FLastFrameIndex);
  o.Notification := urntSilenceOnSelection;
  o.UserMarks.Clear;
  PushToUndo(o);
end;

procedure TMainUndoRedoManager.PushToUndo_DeleteUserMarksInSelection(
  const aCaption: string; aFirstFrameIndex, aLastFrameIndex: int64);
var o: TMainUndoRedoItem;
begin
  o.InitDefault;
  o.Caption := aCaption;
  o.FrameIndex.Create(aFirstFrameIndex, aLastFrameIndex);
  o.CopyUserMarksFrom(@FUserMarksToSave[0], Length(FUserMarksToSave));
  o.Notification := urntDeleteUserMarksOnSelection;
  PushToUndo(o);
end;

procedure TMainUndoRedoManager.PushToUndo_DeleteAllUserMarks(
  const aCaption: string; aFirstFrameIndex, aLastFrameIndex: int64);
var o: TMainUndoRedoItem;
begin
  o.InitDefault;
  o.Caption := aCaption;
  o.FrameIndex.Create(aFirstFrameIndex, aLastFrameIndex);
  o.CopyUserMarksFrom(@FUserMarksToSave[0], Length(FUserMarksToSave));
  o.Notification := urntDeleteAllUserMarks;
  PushToUndo(o);
end;

procedure TMainUndoRedoManager.PushToUndo_AddUserMarkAtCursor(const aCaption: string;
  aFrameIndex: int64);
var o: TMainUndoRedoItem;
begin
  o.InitDefault;
  o.Caption := aCaption;
  o.FrameIndex.Create(aFrameIndex, aFrameIndex);
  o.UserMarks.Clear;
  o.Notification := urntAddUserMark;
  PushToUndo(o);
end;

procedure TMainUndoRedoManager.PushToUndo_InsertRecord(const aCaption: string;
  aFirstFrameIndex, aLastFrameIndex: int64; const aUserMarks: TUserMarks);
var o: TMainUndoRedoItem;
begin
  o.Caption := aCaption;
  o.AudioFile := '';
  o.FrameIndex.Create(aFirstFrameIndex, aLastFrameIndex);
  o.Notification := urntInsertRecord;
  o.CopyUserMarksFrom(aUserMarks);
  PushToUndo(o);
end;

procedure TMainUndoRedoManager.PushToUndo_ReplaceSelectionByRecord(
  const aCaption: string; aFirstFrameIndex, aLastFrameIndex: int64;
  const aUserMarks: TUserMarks; aReplacedFrameCount: int64);
var o: TMainUndoRedoItem;
begin
  if not FLastSuccess then exit;

  o.Caption := aCaption;
  o.AudioFile := FLastSavedFilename;
  o.FrameIndex.Create(aFirstFrameIndex, aLastFrameIndex);
  o.Notification := urntReplaceSelectionByRecord;
  o.CopyUserMarksFrom(aUserMarks);
  o.ReplaceIndex.Create(aFirstFrameIndex, aFirstFrameIndex+aReplacedFrameCount-1);
  PushToUndo(o);
end;

function TMainUndoRedoManager.CurrentUndoCaption: string;
begin
  if not UndoAvailable then
    Result := ''
  else
    Result := Format(SUndoCaption, [UndoPeekCurrent.Caption]);
end;

function TMainUndoRedoManager.CurrentRedoCaption: string;
begin
  if not RedoAvailable then
    Result := ''
  else
    Result := Format(SRedoCaption, [RedoPeekCurrent.Caption]);
end;

procedure TMainUndoRedoManager.Clear;
var i: Integer;
  f: String;
begin
  inherited Clear;
  // delete all undo files
  if (Project.TempFolder <> '') and (FFileSuffix > 0) then
    for i:=1 to FFileSuffix do begin
     f := GenerateUndoRedoFilename(i);
     if FichierExistant(f) then SupprimeFichier(f);
    end;

  FFileSuffix := 0;
end;


end.

