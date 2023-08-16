unit u_utils;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}
{$modeswitch TypeHelpers}


interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, StdCtrls, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRAGradients, BGRAGradientScanner,
  u_common;

  function FileIsInMixedOutputFolder(const aFile: string): boolean;
  function FileIsARecording(const aFile: string): boolean;

  function FolderHaveUserMarksFile(const aFolder: string): boolean;

  function TimeToString(aSeconds: double; aDecimalCount: cardinal): string;

  procedure AddUserHelpMessage(var s: string; const aTextsToAdd: TStringArray);
  function AddUserHelpMessage(const aTextsToAdd: TStringArray): string;

  procedure ConcatToString(var Target: string; const aSeparator, ToAdd: string);

  function StringHaveForbiddenChar(const s: string; aForbiddenChars: TStringArray): boolean;

  // replace all non [0..9]+[a..z]+[A..Z] character by another
  function ReplaceNonASCII(const s, aSubstitute: string; aReplaceUpperCharByLowerChar: boolean): string;
  function ReplaceForbidenChar(const s: string;
                               const ForbidenChars: TStringArray;
                               const aSubstitute: string): string;
  function RemoveAccent(const s: string): string;


// APPLICATION utils
  procedure WriteApplicationHeader(t: TStringList);
  function ApplicationHeaderIsValid(t: TStringList): boolean;
  function FileIsGYVProject(const aFilename: string): boolean;
  procedure ShowGYVUserGuide;
  procedure OpenURLToDonate;
  function MsgDlgTypeToBGRABitmap(aMsgType: TMsgDlgType; aWidth, aHeight: integer): TBGRABitmap;


type
// RECORDING FILE utils
  { TRecordingFileFormater }

  TRecordingFileFormater = record
  private const
    PrefixNumberCountForAudioFile = 3;  // ex: "001 title"
    SuffixNumberSeparatorForPage = '-';
    SuffixNumberCountForPage = 4;   // ex: Page0001-0012
  private
    FPrefixNumber: integer;
    FTitle, FExt: string;
    function GetFileName: string;
    function GetStrPrefix: string;
    procedure SetStrPrefix(AValue: string);
  public
    class function FormatPage(aPageBegin, aPageEnd: integer): string; static;

    class function FileRespectRecordingPrefixRules(const aFilename: string): boolean; static;
    class function FormatPrefix(aNumber: integer): string; static;

    procedure Init(aPrefixNumber: integer; const aTitle, aExt: string);
    procedure InitFrom(const aPrefixedNodeText: string);

    function IsPrefixOnly: boolean;

    property Prefix: string read GetStrPrefix write SetStrPrefix;
    property Title: string read FTitle write FTitle;
    property PrefixNumber: integer read FPrefixNumber write FPrefixNumber;

    property FileName: string read GetFileName;
  end;


// MIX SESSION FILE utils
  procedure RenameAudioFileNameInMixSessionFile(const aPreviousAudioFileName,
                                                      aNewAudioFileName: string);


  function GetLogoImage(aWidth, aHeight: integer): TBGRABitmap;

  // Return the color:  c + c*percent   with percent]-1..1[
  function PercentColor( c: Tcolor; percent: single ): Tcolor;
  // if c is brightness, return c - c*percent
  // if c is darkness, return c + c*percent
  function PercentColorRelative(c: TColor; Absolutepercent: single): TColor;
  // if c is brightness, return clBlack
  // if c is darkness, return clWhite
  function BlackOrWhiteRelative(c: TColor): TColor;

  procedure DrawCurveGradient(aBitmap: TBGRABitmap; const aColor: TBGRAPixel; aVertical: boolean=True);
  procedure DrawMarkerGradient(aBitmap: TBGRABitmap; const aColor: TBGRAPixel);

  procedure ChangeFontHeight(aControls: array of TControl; aDesignHeight: integer);
  procedure ChangeFontHeightOnFormChilds(aControl: TWinControl; aDesignHeight: integer);
  procedure ChangeFontColor(aControls: array of TControl; aColor: TColor);

type
  PIndexInterval = ^TIndexInterval;
  { TIndexInterval }

  TIndexInterval = record
    First, Last: int64;
    procedure InitDefault;
    procedure Create(const AnotherIndexinterval: TIndexInterval);
    procedure Create(const aFirstValue, aLastValue: int64);
    function Count: int64;
    function Contain(aValue: int64): boolean;
    function SaveToString: string;
    function LoadFromString(const s: string): boolean;
    procedure SwapWith(AnotherIndexInterval: PIndexInterval);
  end;


// LANGUAGES utils
  function GetOSLanguage: string;
  procedure FillComboBoxWithAvailableLanguage(aCB: TComboBox);
  function ComboBoxToLanguage(aCB: TComboBox): string;
  procedure LanguageToComboBox(aLang: string; aCB: TComboBox);


  type
// HELPER FOR TUserMarks

  { TUserMarksHelper }

  TUserMarksHelper = type helper for TUserMarks
    procedure Create(const aUserMarks: TUserMarks);
    procedure Clear;
    // Add a value to the end of the array
    procedure Append(const aTimeValue: single);
    // Add a value to the right place (time sorted)
    procedure AddAndSort(const aTimeValue: single);
    // Add several values to the right place (time sorted)
    procedure AddAndSort(const aUserMarks: TUserMarks);
    // Delete a value
    procedure DeleteValue(const aTimeValue: single);

    function Count: integer;
    function IsEmpty: boolean;
    procedure AddOffsetToValues(const aOffset: single);
    procedure AddOffsetToValuesFromTimePos(const aTimePos, aOffset: single);

    procedure SaveToFile(const aAudioFileName: string);
    procedure LoadFromFile(const aAudioFileName: string);
  end;


// RECENT USER FOLDERS

  { TRecentList }

  TRecentList = record
    Items: TStringArray;
    procedure Clear;
    function AddItem(const aItem: string): integer;
    procedure DeleteItem(const aItem: string);
    function IndexOf(const aItem: string): integer;
    function Count: integer;
    function SaveToString(const aSeparator: string): string;
    procedure LoadFromString(const s, aSeparator: string);
  end;


implementation
uses LazUTF8, u_program_options, u_crossplatform, u_resource_string,
  Math, utilitaire_fichier, PropertyUtils, utilitaire_bgrabitmap, LCLIntf,
  {$IFDEF windows}
  //Windows,
  {$ELSE}
  Unix,
   {$IFDEF LCLCarbon}
   MacOSAll,
   {$ENDIF}
  {$ENDIF}
  gettext;


function GetOSLanguage: string;
var
  l, fbl: string;
  {$IFDEF LCLCarbon}
  theLocaleRef: CFLocaleRef;
  locale: CFStringRef;
  buffer: StringPtr;
  bufferSize: CFIndex;
  encoding: CFStringEncoding;
  success: boolean;
  {$ENDIF}
begin
  {$IFDEF LCLCarbon}
  theLocaleRef := CFLocaleCopyCurrent;
  locale := CFLocaleGetIdentifier(theLocaleRef);
  encoding := 0;
  bufferSize := 256;
  buffer := new(StringPtr);
  success := CFStringGetPascalString(locale, buffer, bufferSize, encoding);
  if success then
    l := string(buffer^)
  else
    l := '';
  fbl := Copy(l, 1, 2);
  dispose(buffer);
  {$ELSE}
  l := '';
  fbl := '';
  GetLanguageIDs(l, fbl);
  {$ENDIF}
  Result := fbl;
end;


procedure FillComboBoxWithAvailableLanguage(aCB: TComboBox);
begin
  aCB.Clear;
  aCB.Items.Add('English (en)');
  aCB.Items.Add('Français (fr)');
end;

function ComboBoxToLanguage(aCB: TComboBox): string;
var i: integer;
  aItem: string;
begin
  Result := '';
  if aCB.ItemIndex = -1 then exit;
  aItem := aCB.Items.Strings[aCB.ItemIndex];
  i := Pos('(', aItem);
  if i = 0 then exit;
  if Pos(')', aItem) <> i+3 then exit;
  Result := Copy(aItem, i+1, 2);
end;

procedure LanguageToComboBox(aLang: string; aCB: TComboBox);
var i: integer;
begin
  if Length(aLang) = 2 then
    for i:=0 to aCB.Items.Count-1 do
      if Pos('('+aLang+')', aCB.Items.Strings[i]) > 0 then begin
        aCB.ItemIndex := i;
        exit;
      end;

  aCB.ItemIndex := -1;
end;

{ TRecordingFileFormater }

function TRecordingFileFormater.GetFileName: string;
begin
  Result := FormatPrefix(FPrefixNumber);
  if FTitle <> '' then Result := Result+' '+FTitle;
  Result := Result + FExt;
end;

function TRecordingFileFormater.GetStrPrefix: string;
begin
  Result := FormatPrefix(FPrefixNumber);
end;

procedure TRecordingFileFormater.SetStrPrefix(AValue: string);
begin
  FPrefixNumber := AValue.ToInteger;
end;

class function TRecordingFileFormater.FormatPage(aPageBegin, aPageEnd: integer): string;
begin
  Result := SPage+aPageBegin.ToString+SuffixNumberSeparatorForPage+aPageEnd.ToString;
end;

class function TRecordingFileFormater.FileRespectRecordingPrefixRules(const aFilename: string): boolean;
var f: string;
  i: integer;
begin
  Result := False;
  f := ChangeFileExt(ExtractFileName(aFilename), '');
  if f = '' then exit;

  i := Pos(' ', f);
  if i <> 0 then begin
    Result := (i = PrefixNumberCountForAudioFile+1) and // checks if there is a space at right place
              (i < Length(f)); // checks if there is at least 1 character after the space
    if Result then             // check if prefix is a number
      Result := TryStrToInt(Copy(f, 1, PrefixNumberCountForAudioFile), i);
  end else
    Result := (Length(f) = PrefixNumberCountForAudioFile) and // the filename is only made with a prefix
              TryStrToInt(f, i); // check if prefix is a number
end;

class function TRecordingFileFormater.FormatPrefix(aNumber: integer): string;
begin
  Result := Format('%.'+PrefixNumberCountForAudioFile.ToString+'d', [aNumber]);
end;

procedure TRecordingFileFormater.Init(aPrefixNumber: integer; const aTitle, aExt: string);
begin
  FPrefixNumber := aPrefixNumber;
  FTitle := aTitle;
  FExt := aExt;
end;

procedure TRecordingFileFormater.InitFrom(const aPrefixedNodeText: string);
begin
  FPrefixNumber := Copy(aPrefixedNodeText, 1, PrefixNumberCountForAudioFile).ToInteger;
  FTitle := Copy(ChangeFileExt(aPrefixedNodeText, ''), PrefixNumberCountForAudioFile+2, Length(aPrefixedNodeText));
  FExt := ExtractFileExt(aPrefixedNodeText);
end;

function TRecordingFileFormater.IsPrefixOnly: boolean;
{var f: string;
  i: integer; }
begin
  Result := FTitle = '';
{  f := ChangeFileExt(ExtractFileName(Filename), '');
  Result := (Length(f) = PrefixNumberCountForAudioFile);
  if Result then
    Result := TryStrToInt(f, i); }
end;



{ TRecentList }

procedure TRecentList.Clear;
begin
  Items := NIL;
end;

function TRecentList.AddItem(const aItem: string): integer;
begin
  Result := IndexOf(aItem);
  if Result <> -1 then exit;
  Result := Length(Items);
  SetLength(Items, Result+1);
  Items[Result] := aItem;
end;

procedure TRecentList.DeleteItem(const aItem: string);
var i: integer;
begin
  i := IndexOf(aItem);
  if i <> -1 then
    system.Delete(Items, i, 1);
end;

function TRecentList.IndexOf(const aItem: string): integer;
var i: integer;
begin
  for i:=0 to High(Items) do
    if Items[i] = aItem then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TRecentList.Count: integer;
begin
  Result := Length(Items);
end;

function TRecentList.SaveToString(const aSeparator: string): string;
var i: integer;
begin
  Result := '';
  if Length(Items) = 0 then exit;

  Result := Items[0];
  for i:=1 to High(Items) do
    Result := Result + aSeparator + Items[i];
end;

procedure TRecentList.LoadFromString(const s, aSeparator: string);
var A: TStringArray;
begin
  Items := NIL;
  A := s.Split([aSeparator]);
  Items := Copy(A, 0, Length(A));
end;

{ TUserMarksHelper }

procedure TUserMarksHelper.Create(const aUserMarks: TUserMarks);
begin
  Self := NIL;
  Self := Copy(aUserMarks, 0, Length(aUserMarks));
end;

procedure TUserMarksHelper.Clear;
begin
  Self := NIL;
end;

procedure TUserMarksHelper.Append(const aTimeValue: single);
begin
  system.Insert(aTimeValue, Self, Length(Self));
end;

procedure TUserMarksHelper.AddAndSort(const aTimeValue: single);
var i: SizeInt;
begin
  if Length(Self) = 0 then
    Append(aTimeValue)
  else begin
    for i:=0 to High(Self) do
      if Self[i] > aTimeValue then begin
        system.Insert(aTimeValue, Self, i);
        exit;
      end;
    Append(aTimeValue);
  end;
end;

procedure TUserMarksHelper.AddAndSort(const aUserMarks: TUserMarks);
var i: SizeInt;
begin
  if Length(aUserMarks) = 0 then exit;

  if Count = 0 then
    Self := Copy(aUserMarks, 0, aUserMarks.Count)
  else
    for i:=0 to High(aUserMarks) do
      AddAndSort(aUserMarks[i]);
end;

procedure TUserMarksHelper.DeleteValue(const aTimeValue: single);
var i: integer;
begin
  for i:=0 to High(Self) do
    if Self[i] = aTimeValue then begin
      Delete(Self, i, 1);
      exit;
    end;
end;

function TUserMarksHelper.Count: integer;
begin
  Result := system.Length(Self);
end;

function TUserMarksHelper.IsEmpty: boolean;
begin
  Result := Length(Self) = 0;
end;

procedure TUserMarksHelper.AddOffsetToValues(const aOffset: single);
var i: SizeInt;
begin
  for i:=0 to High(Self) do
    Self[i] := Self[i]+aOffset;
end;

procedure TUserMarksHelper.AddOffsetToValuesFromTimePos(const aTimePos,
  aOffset: single);
var i: integer;
begin
  for i:= High(Self) downTo 0 do
    if Self[i] >= aTimePos then
      Self[i] := Self[i] + aOffset
    else exit;
end;

procedure TUserMarksHelper.SaveToFile(const aAudioFileName: string);
var i: integer;
  sep, content, f: string;
  prop: TProperties;
  t: TStringList;
begin
  f := ChangeFileExt(aAudioFileName, USER_MARK_FILE_EXT);
  if Length(Self) = 0 then begin
    // delete the usermarks file
    if FichierExistant(f) then SupprimeFichier(f);
    exit;
  end;

  content := '';
  sep := '';
  for i:=0 to High(Self) do begin
    content := content + sep + FormatFloatWithDot('0.00', Self[i]);
    sep := '#';
  end;

  prop.Init('|');
  prop.Add('UserMarksCount', Length(Self));
  prop.Add('UserMarksList', content);

  t := TStringList.Create;
  t.Add(prop.PackedProperty);
  try
    t.SaveToFile(f);
  finally
    t.Free;
  end;
end;

procedure TUserMarksHelper.LoadFromFile(const aAudioFileName: string);
var i, c: integer;
  content, f: string;
  prop: TProperties;
  t: TStringList;
  A: TStringArray;
begin
  Self := NIL;
  f := ChangeFileExt(aAudioFileName, USER_MARK_FILE_EXT);
  if not FileExists(f) then exit;

  t := TStringList.Create;
  t.Add(prop.PackedProperty);
  try
    t.LoadFromFile(f);
    if t.Count <> 1 then exit;

    prop.Split(t.Strings[0], '|');
    c := 0;
    content := '';
    if not prop.IntegerValueOf('UserMarksCount', c, 0) then exit;
    if c = 0 then exit;

    if not prop.StringValueOf('UserMarksList', content, '') then exit;
    A := content.Split(['#']);
    for i:=0 to High(A) do
      Append(StringToSingle(A[i]));
  finally
    t.Free;
  end;
end;


{ TIndexInterval }

procedure TIndexInterval.InitDefault;
begin
  First := -1;
  Last  := -1;
end;

procedure TIndexInterval.Create(const AnotherIndexinterval: TIndexInterval);
begin
  First := AnotherIndexinterval.First;
  Last := AnotherIndexinterval.Last;
end;

procedure TIndexInterval.Create(const aFirstValue, aLastValue: int64);
begin
  First := aFirstValue;
  Last := aLastValue;
end;

function TIndexInterval.Count: int64;
begin
  if (First < 0) or (Last < 0) then
    Result := 0
  else
    Result := Last - First + 1;
end;

function TIndexInterval.Contain(aValue: int64): boolean;
begin
  Result := (aValue >= First) and (aValue <= Last);
end;

function TIndexInterval.SaveToString: string;
begin
  Result := First.ToString+' '+Last.ToString;
end;

function TIndexInterval.LoadFromString(const s: string): boolean;
var
  A: TStringArray;
begin
  Result := False;
  A := s.Split([' ']);
  if Length(A) <> 2 then exit;

  Result := TryStrToInt64(A[0], First);
  Result := Result and TryStrToInt64(A[1], Last);
end;

procedure TIndexInterval.SwapWith(AnotherIndexInterval: PIndexInterval);
var v: int64;
begin
  v := First;
  First := AnotherIndexInterval^.First;
  AnotherIndexInterval^.First := v;

  v := Last;
  Last := AnotherIndexInterval^.Last;
  AnotherIndexInterval^.Last := v;
end;


function ReplaceForbidenChar(const s: string; const ForbidenChars: TStringArray;
  const aSubstitute: string): string;
var i, j: Integer;
  subChar: string;
  flagFound: boolean;
begin
  Result := '';
  for i:=1 to UTF8Length(s) do begin
    subChar := UTF8Copy(s, i, 1);

    flagFound := False;
    for j:=0 to High(ForbidenChars) do
      flagFound := flagFound or (subChar = ForbidenChars[j]);

    if flagFound then
      Result := Result + aSubstitute
    else
      Result := Result + subChar;
  end;
end;

function RemoveAccent(const s: string): string;
var CurP, EndP: PChar;
  Len: Integer;
  ACodePoint: String;
begin
  Result := '';

  CurP := PChar(s);        // if S='' then PChar(S) returns a pointer to #0
  EndP := CurP + length(s);
  ACodePoint := '';
  while CurP < EndP do
  begin
    Len := {%H-}UTF8CodepointSize(CurP);
    SetLength(ACodePoint, Len);
    Move(CurP^, ACodePoint[1], Len); // A single codepoint is copied from the string.

    case ACodePoint of
     'à', 'á', 'â', 'ã', 'ä', 'å' : ACodePoint := 'a';
     'À‚', 'Á', 'Â', 'Ã', 'Ä', 'Å' : ACodePoint := 'A';
     'è', 'é', 'ê', 'ë' : ACodePoint := 'e';
     'È', 'É', 'Ê', 'Ë' : ACodePoint := 'E';
     'ì', 'í', 'î', 'ï' : ACodePoint := 'i';
     'Ì', 'Í', 'Î', 'Ï' : ACodePoint := 'I';
     'ò', 'ó', 'ô', 'õ', 'ö' : ACodePoint := 'o';
     'Ò', 'Ó', 'Ô', 'Õ', 'Ö' : ACodePoint := 'O';
     'ù', 'ú', 'û', 'ü' : ACodePoint := 'u';
     'Ù', 'Ú', 'Û', 'Ü' : ACodePoint := 'U';
     'ç' : ACodePoint := 'c';
     'Ç' : ACodePoint := 'C';
    end;//case

    Result := Result + ACodePoint;
    inc(CurP, Len);
  end;

end;

function FileIsInMixedOutputFolder(const aFile: string): boolean;
begin
  Result := ExcludeTrailingPathDelimiter(NomDuDernierSousRepertoire(ExtractFilePath(aFile))) = PROJECT_OUTPUT_FOLDER_MP3;
end;

function FileIsARecording(const aFile: string): boolean;
begin
  Result := LowerCase(ExtractFileExt(aFile)) = '.wav';
end;

function FolderHaveUserMarksFile(const aFolder: string): boolean;
var t: TStringList;
begin
  Result := False;
  try
    t := ContenuDuRepertoire(aFolder, USER_MARK_FILE_EXT, False, False);
    Result := t.Count > 0;
  finally
    t.Free;
  end;
end;

function TimeToString(aSeconds: double; aDecimalCount: cardinal): string;
var v, d, h, m, s, ms, pw: integer;
begin
  if aSeconds = 0 then begin
    Result := '0';
    exit;
  end;

  v := Trunc(aSeconds);
  d := v div 86400;
  v := v mod 86400;

  h := v div 3600;
  v := v mod 3600;

  m := v div 60;
  s := v mod 60;

  pw := Trunc(IntPower(10, aDecimalCount));

  ms := Round(Frac(aSeconds)*pw);
  if ms = pw then begin
    ms := 0;
    inc(s);
    if s = 60 then begin
      s := 0;
      inc(m);
      if m = 60 then begin
        m := 0;
        inc(h);
        if h = 24 then begin
          h := 0;
          inc(d);
        end;
      end;
    end;
  end;

  Result := '';
  if d > 0 then
  begin
    Result += d.ToString+'j';
    if h < 10 then Result+='0';
    if h = 0 then Result+='0:';
  end;

  if ( h > 0 ) then begin
   Result += inttostr( h ) + ':';
   if m < 10 then Result += '0';
   if m = 0 then Result+='0:';
  end;

  if m > 0 then begin
    Result += inttostr( m ) + ':';
    if s < 10 then Result+='0';
  end;

  Result += s.ToString;

  if aDecimalCount > 0 then begin
    Result := Result+'.';
    if (ms < 10) and (aDecimalCount = 3) then
      Result+='00'
    else if (ms < 100) and (aDecimalCount = 2) then
      Result+='0';
    Result+=ms.ToString;
  end;
end;

procedure AddUserHelpMessage(var s: string; const aTextsToAdd: TStringArray);
var i: integer;
begin
  for i:=0 to High(aTextsToAdd) do begin
    if Length(s) > 0 then s := s + '    ';
    s := s + aTextsToAdd[i];
  end;
end;

function AddUserHelpMessage(const aTextsToAdd: TStringArray): string;
begin
  Result := '';
  AddUserHelpMessage(Result, aTextsToAdd);
end;

procedure ConcatToString(var Target: string; const aSeparator, ToAdd: string);
var s: string;
begin
  s := Trim(ToAdd);
  if s = '' then exit;
  if Length(Target) > 0 then Target += aSeparator;
  Target += s;
end;

function StringHaveForbiddenChar(const s: string; aForbiddenChars: TStringArray): boolean;
var
  i: Integer;
begin
  Result := True;
  if Length(aForbiddenChars) > 0 then
    for i:=0 to High(aForbiddenChars) do
     if UTF8Pos(aForbiddenChars[i], s) > 0 then exit;

  Result := False;
end;

function ReplaceNonASCII(const s, aSubstitute: string; aReplaceUpperCharByLowerChar: boolean): string;
var i: integer;
  letter: String;
begin
  Result := '';
  if UTF8Length(s) = 0 then exit;
  for i:=1 to UTF8Length(s) do begin
    letter := UTF8Copy(s, i, 1);
    if (Length(letter) = 1) and
       ((letter[1] in ['0'..'9']) or
        (letter[1] in ['a'..'z']) or
        (letter[1] in ['A'..'Z'])) then
      Result := Result+letter
    else Result := Result+aSubstitute;
  end;

  if aReplaceUpperCharByLowerChar then
    Result := LowerCase(Result);
end;

procedure WriteApplicationHeader(t: TStringList);
begin
  t.Add('[APPLICATION]');
  t.Add('Name|'+APP_NAME+'|Version|'+APP_VERSION);
end;

function ApplicationHeaderIsValid(t: TStringList): boolean;
var k: integer;
  prop: TProperties;
  s: string;
begin
  k := t.IndexOf('[APPLICATION]');
  Result := (k <> -1) and (k < t.Count-1);
  if Result then begin
    prop.Split(t.Strings[k+1], '|');
    s := '';
    prop.StringValueOf('Name', s, '');
    Result := s = APP_NAME;
    Result := Result and prop.StringValueOf('Version', s, '');
  end;
end;

function FileIsGYVProject(const aFilename: string): boolean;
var t: TStringList;
begin
  Result := False;
  if Length(aFilename) = 0 then exit;
  if not FileExists(aFilename) then exit;
  if ExtractFileExt(aFilename) <> PROJECT_FILE_EXT then exit;

  t := TStringList.Create;
  try
    try
      t.LoadFromFile(aFilename);
      Result := ApplicationHeaderIsValid(t);
    finally
    end;

  finally
    t.Free;
  end;
end;

procedure ShowGYVUserGuide;
  function TryOpen(const aFile: string): boolean;
  begin
    Result := OpenDocument(GetAppDataFolder+aFile);
    if not Result then
      Result := OpenURL(GetAppDataFolder+aFile);
  end;
begin
  // try to load the user guide in the current language used by app
  // if it fail, try to open the english version
  if not TryOpen(USER_GUIDE_FILE_BASE+ProgramOptions.Language+USER_GUIDE_FILE_EXT) then begin
    TryOpen(USER_GUIDE_FILE_BASE+'en'+USER_GUIDE_FILE_EXT);
  end;
end;

procedure OpenURLToDonate;
begin
  if ProgramOptions.Language = 'fr' then
    OpenURL('https://www.paypal.com/donate/?hosted_button_id=DC6UXENKA7Q3Q')
  else
    OpenURL('https://www.paypal.com/donate/?hosted_button_id=GZAR296S5LYBG');
end;

function MsgDlgTypeToBGRABitmap(aMsgType: TMsgDlgType; aWidth, aHeight: integer): TBGRABitmap;
var im: TBGRABitmap;
begin
  try
    case aMsgType of
      mtWarning: im := TBGRABitmap.Create(GetAppDataFolder+'state_warning_200.png');
      mtError: im := TBGRABitmap.Create(GetAppDataFolder+'state_error_200.png');
      mtInformation: im := TBGRABitmap.Create(GetAppDataFolder+'state_information_200.png');
      mtConfirmation: im := TBGRABitmap.Create(GetAppDataFolder+'state_unknown_200.png');
      else im := TBGRABitmap.Create(aWidth, aHeight, BGRAPixelTransparent);
    end;
  except
    im := TBGRABitmap.Create(aWidth, aHeight, BGRAPixelTransparent);
  end;
  Result := im.Resample(aWidth, aHeight);
  im.Free;
end;

procedure RenameAudioFileNameInMixSessionFile(const aPreviousAudioFileName,
  aNewAudioFileName: string);
var sessionFile: string;
  t: TStringList;
  prop, prop1: TProperties;
  temp: string;
  c: integer;
begin
  sessionFile := IncludeTrailingPathDelimiter(ExtractFilePath(aPreviousAudioFileName))+
                 MIX_SESSION_FILENAME;
  if not FichierExistant(sessionFile) then exit;

  t := TStringList.Create;
  try
    t.LoadFromFile(sessionFile);
    if t.Count = 0 then exit;

    prop.Split(t.Strings[0], '|');

    temp := '';
    if prop.StringValueOf('VoiceView', temp, '') then begin
      prop1.Split(temp, '#');
      c := 0;
      if not prop1.IntegerValueOf('ObjectCount', c, 0) then exit;

      while c > 0 do begin
        if prop1.StringValueOf('File'+c.ToString, temp, '') then begin
            if ExtractFilename(temp) = ExtractFileName(aPreviousAudioFileName) then begin
              // replace file name
              if prop1.ReplaceValue('File'+c.ToString, aNewAudioFileName) then
                // reconstruct the properties
                if prop.ReplaceValue('VoiceView', prop1.PackedProperty) then begin
                  // and save them
                  t.Strings[0] := prop.PackedProperty;
                  t.SaveToFile(sessionFile);
                  // end
                  exit;
                end;
            end;
          end;
        dec(c);
      end;
    end;
  finally
    t.Free;
  end;
end;

function ReplaceSpaceByUnderscore(const s: string): string;
var
  i: Integer;
begin
  Result := '';
  SetLength(Result, Length(s));

  for i:=1 to Length(s) do
    if s[i] = ' ' then
      Result[i] := '_'
    else
      Result[i] := s[i];
end;

function GetLogoImage(aWidth, aHeight: integer): TBGRABitmap;
begin
  try
    Result := SVGFileToBGRABitmap(GetAppDataFolder+'logo.svg', aWidth, aHeight);
  except
    Result := TBGRABitmap.Create(aWidth, aHeight, BGRAPixelTransparent);
  end;
end;

function PercentColor(c: Tcolor; percent: single): Tcolor;
var r, g, b: integer;
begin
  r := Red(c);
  g := Green(c);
  b := Blue(c);
  r := EnsureRange(Round(r + ( r * percent )), 0, 255);
  g := EnsureRange(Round(g + ( g * percent )), 0, 255);
  b := EnsureRange(Round(b + ( b * percent )), 0, 255);
  Result := RGBToColor(r, g, b);
end;

function PercentColorRelative(c: TColor; Absolutepercent: single): TColor;
var r, g, b: integer;
  s: single;
begin
  r := Red(c);
  g := Green(c);
  b := Blue(c);
  if (r+g+b)/(255*3) > 0.5 then
    s := -1
  else
    s := 1;
  r := EnsureRange(Round(r + r * Absolutepercent * s), 0, 255);
  g := EnsureRange(Round(g + g * Absolutepercent * s), 0, 255);
  b := EnsureRange(Round(b + b * Absolutepercent * s), 0, 255);
  Result := RGBToColor(r, g, b);
end;

function BlackOrWhiteRelative(c: TColor): TColor;
begin
  if (Red(c)+Green(c)+Blue(c))/(255*3) > 0.5 then
    Result := RGBToColor(0,0,0)
  else
    Result := RGBToColor(255,255,255);

end;

procedure DrawCurveGradient(aBitmap: TBGRABitmap; const aColor: TBGRAPixel;
  aVertical: boolean);
var g: TBGRAMultiGradient;
  gs: TBGRAGradientScanner;
  pStart, pEnd: TPointF;
  w, h: integer;
begin
  w := aBitmap.Width;
  h := aBitmap.Height;

  aBitmap.Fill(BGRAWhite);
  g := TBGRAMultiGradient.Create(
        [BGRA(255,0,0), aColor, aColor, aColor],
        [0, 0.3, 0.9, 1], True, False);
 // g.InterpolationFunction := @g.CosineInterpolation;
  if aVertical then begin
    pStart.x := w div 2;
    pStart.y := 0;
    pEnd.x := w div 2;
    pEnd.y := h;
  end else begin
    pStart.x := w;
    pStart.y := h div 2;
    pEnd.x := 0;
    pEnd.y := h div 2;
  end;
  gs := TBGRAGradientScanner.Create(g, gtLinear, pStart, pEnd);
  aBitmap.Fill(gs, dmSet);
  g.Free;
  gs.Free;
end;

procedure DrawMarkerGradient(aBitmap: TBGRABitmap; const aColor: TBGRAPixel);
var g: TBGRAMultiGradient;
  gs: TBGRAGradientScanner;
  pStart, pEnd: TPointF;
  w, h: integer;
  cHigh, cLow: TBGRAPixel;
begin
  cHigh :=  aColor;
  cLow := aColor;
  cLow.Lightness := cLow.Lightness shr 1;

  w := aBitmap.Width;
  h := aBitmap.Height;

  aBitmap.Fill(BGRAWhite);
  g := TBGRAMultiGradient.Create(
        [cLow, cHigh, cLow],
        [0, 0.5, 1], True, False);
  //g.InterpolationFunction := @g.CosineInterpolation;
  pStart.x := w;
  pStart.y := h div 2;
  pEnd.x := 0;
  pEnd.y := h div 2;

  gs := TBGRAGradientScanner.Create(g, gtLinear, pStart, pEnd);
  aBitmap.Fill(gs, dmSet);
  g.Free;
  gs.Free;
end;

procedure ChangeFontHeight(aControls: array of TControl; aDesignHeight: integer);
var i: integer;
begin
  for i:=0 to High(aControls) do
    aControls[i].Font.Height := aControls[i].ScaleDesignToForm(aDesignHeight);
end;

procedure ChangeFontHeightOnFormChilds(aControl: TWinControl; aDesignHeight: integer);
var i: integer;
begin
  aControl.Font.Height := aControl.ScaleDesignToForm(aDesignHeight);
  for i:=0 to aControl.ControlCount-1 do
    if aControl.Controls[i] is TWinControl then
      ChangeFontHeightOnFormChilds(TWinControl(aControl.Controls[i]), aDesignHeight)
    else
      aControl.Controls[i].Font.Height := aControl.ScaleDesignToForm(aDesignHeight);
end;

procedure ChangeFontColor(aControls: array of TControl; aColor: TColor);
var i: integer;
begin
  for i:=0 to High(aControls) do
    aControls[i].Font.Color := aColor;
end;


end.

