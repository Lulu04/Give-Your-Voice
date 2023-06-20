unit u_userdialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;


procedure ShowMess(const mess, sbutton: string; aMsgType: TMsgDlgType=mtCustom);

function AskConfirmation(const mess, sok, scancel: string; aMsgType: TMsgDlgType): integer;

function UserInputInteger(const mess, sok, scancel, aUnit: string;
                          var number: integer): boolean;
function UserInputSingle(const mess, sok, scancel, aUnit: string;
                         var number: single; const aMin, aMax: single): boolean;

function UserIntegerInput(const mess, sok, scancel: string;
                           var number: integer;
                           aMsgType: TMsgDlgType): boolean;

function UserInputString(const mess, sok, scancel: string; aMsgType: TMsgDlgType;
                         var sinput: string;
                         aAllowEmptyString: boolean;
                         aNumberOnly: boolean=False;
                         const aForbidenChar: TStringArray=NIL;
                         const aMessageBadChar: string=''): boolean;


implementation

uses Controls,
     form_user_askconfirmation,
     form_user_showmessage,
     form_user_inputstring,
     form_user_asknumber;

procedure ShowMess(const mess, sbutton: string; aMsgType: TMsgDlgType);
var F: TFormUserMessage;
begin
  F:=TFormUserMessage.Create(NIL);
  F.Init(mess, sbutton, aMsgType);
  F.Free;
end;

function AskConfirmation(const mess, sok, scancel: string; aMsgType: TMsgDlgType): integer;
begin
  Result:=FormUserConfirmation.Init(mess, sok, scancel, aMsgType);
end;

function UserInputString(const mess, sok, scancel: string; aMsgType: TMsgDlgType;
  var sinput: string; aAllowEmptyString: boolean; aNumberOnly: boolean;
  const aForbidenChar: TStringArray; const aMessageBadChar: string): boolean;
var F: TFormUserInput;
begin
  F := TFormUserInput.Create(NIL);

  F.FrameEditString1.Init(aAllowEmptyString, aNumberOnly, aForbidenChar, aMessageBadChar);
  Result := F.Init(mess, sok, scancel, sinput, aMsgType) = mrOk;

  if Result then sinput := F.UserInput;
  F.Free;
end;

function UserInputInteger(const mess, sok, scancel, aUnit: string;
  var number: integer): boolean;
var F: TFormUserAskNumber;
begin
  F := TFormUserAskNumber.Create(NIL);
  Result := F.InitInteger(mess, sok, scancel, aUnit, number) = mrOk;
  F.Free;
end;

function UserInputSingle(const mess, sok, scancel, aUnit: string;
  var number: single; const aMin, aMax: single): boolean;
var F: TFormUserAskNumber;
begin
  F := TFormUserAskNumber.Create(NIL);
  F.FloatSpinEdit1.MinValue := aMin;
  F.FloatSpinEdit1.MaxValue := aMax;
  Result := F.InitSingle(mess, sok, scancel, aUnit, number) = mrOk;
  F.Free;
end;

function UserIntegerInput(const mess, sok, scancel: string;
  var number: integer; aMsgType: TMsgDlgType): boolean;
var F: TFormUserInput;
  snum: string;
  v, old: integer;
begin
  old := number;
  snum := number.ToString;

  F := TFormUserInput.Create(NIL);
  F.FrameEditString1.Init(False, True);
  Result := F.Init(mess, sok, scancel, snum, aMsgType) = mrOk;

  if Result then begin
    if TryStrToInt(snum, v)
      then number:=v
      else number:=old;
  end;
  F.Free;
end;

end.

