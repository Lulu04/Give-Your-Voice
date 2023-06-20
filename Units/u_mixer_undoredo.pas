unit u_mixer_undoredo;

{$mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils,
  undo_redo_manager;

type

  // notification types for undo/redo
  TMixerUndoRedoNotificationType = (mixurtMoveAudio,
                                    mixurtAddAudio,
                                    mixurtDeleteAudio,
                                    mixurtAddGainPoint,
                                    mixurtChangeGainCurve);

  { TMixerUndoRedoItem }

  TMixerUndoRedoItem = record
    Caption: string;
    Notification: TMixerUndoRedoNotificationType;
    TargetView: Pointer; // PMixerView
    DeltaValue: single;
    procedure InitDefault;
  end;


  { TMixerUndoRedoManager }

  TMixerUndoRedoManager = class(specialize TCustomUndoRedoManager<TMixerUndoRedoItem>)
  protected
    procedure DoUndoRedo(ItsUndo: boolean); override;
    procedure DestroyItem(constref aItem: TMixerUndoRedoItem); override;
  public
    function CurrentUndoCaption: string;
    function CurrentRedoCaption: string;

   // procedure PushToUndo_DeleteAudio(const aCaption: string; aTargetView: pointer;
  end;

var
  MixerUndoRedoManager: TMixerUndoRedoManager;


implementation

uses u_resource_string;

{ TMixerUndoRedoManager }

procedure TMixerUndoRedoManager.DoUndoRedo(ItsUndo: boolean);
begin

end;

procedure TMixerUndoRedoManager.DestroyItem(constref aItem: TMixerUndoRedoItem);
begin
  { TODO : MixerUndoRedo: destroy item when stack is full }
end;

function TMixerUndoRedoManager.CurrentUndoCaption: string;
begin
  if not UndoAvailable then
    Result := ''
  else
    Result := Format(SUndoCaption, [UndoPeekCurrent.Caption]);
end;

function TMixerUndoRedoManager.CurrentRedoCaption: string;
begin
  if not RedoAvailable then
    Result := ''
  else
    Result := Format(SRedoCaption, [RedoPeekCurrent.Caption]);
end;

{ TMixerUndoRedoItem }

procedure TMixerUndoRedoItem.InitDefault;
begin
  Caption := '';
  //Notification: TMixerUndoRedoNotificationType;
  TargetView := NIL;
  DeltaValue := 0;
end;

end.

