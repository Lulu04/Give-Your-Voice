unit u_audio_utils;

{$mode ObjFPC}{$H+}
 {$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils, ctypes,
  ALSound, libsndfile;

procedure ProcessLogMessageFromOpenALSoft({%H-}aUserPtr: pointer; aLevel: char; aMessage: PChar; {%H-}aMessageLength: cint);

type

  { TPlaybackContext }

  TPlaybackContext = record
  private
    procedure DeleteContextAndEffect(var aContext: TALSPlaybackContext);
  public
    FCompressor, FBassBoostEqualizer: TALSEffect;
    FCompressorProp: TALSCompressorProperties;
    FBassBoostEqualizerProp: TALSEqualizerProperties;
    FErrorOnChainEffect: boolean;
  public
    FPlaybackContext: TALSPlaybackContext;
    // Creates a context and the chain effect
    // -1 for the default device
    procedure Create(aDeviceIndex: integer);
    // free context and effects
    procedure Free;

  public
    FLoopbackContext: TALSLoopbackContext;
    procedure CreateLoopback;
    procedure FreeLoopback;
  end;

  { TCaptureContext }

  TCaptureContext = record
    FCaptureContext: TALSCaptureContext;
    procedure Create(aDeviceIndex: integer);
    procedure Free;
  end;

var Playback: TPlaybackContext;
    Capture: TCaptureContext;


type

  { TBaseAudioFile }

  TBaseAudioFile = object
    Handle: PSNDFILE;
    Format,
    Channels: integer;
    Frames: int64;
    SampleRate: integer;
    // return a frame index in range [0..Frames-1]
    function TimeToFrameIndex(aTimePos: double): int64;
    // return a duration in seconds from a number of frames
    function FrameToTime(aFrameCount: int64): double;
  private // debug purpose
    FileName: string;
    procedure LogLibSndFileErrorMessage(const aMess: string);
    function GetLibSndFileErrMess: string;
  end;

  { TAudioFileReader }

  TAudioFileReader = object(TBaseAudioFile)
    function OpenRead(const aFileName: string; aPostLogMessage: boolean=True): boolean;
    function ReadShort(p: Pointer; aCount: longword): longword;
    function ReadFloat(p: Pointer; aCount: longword): longword;
    function ReadDouble(p: PDouble; aCount: longword): longword;
    function Read(const aBuf: TALSPlaybackBuffer; aCount: longword): longword;
    function Close: boolean;
    function MoveToFrame(aFrameIndex: int64): boolean;
    function TotalDuration: double;
  end;

  { TAudioFileWriter }

  TAudioFileWriter = object(TBaseAudioFile)
  private
    FCopyBuffer: TALSPlaybackBuffer; // used by copy routines
    procedure InitCopyBuffer(aFramesCapacity: longword);
  public
    function OpenWrite(const aFileName: string;
                       aFormat: TALSFileFormat;
                       aChannels,
                       aSampleRate: integer;
                       aPostLogMessage: boolean=True): boolean;
    function SampleAreFloat: boolean;
    function Close: boolean;
    function WriteShort(p: Pointer; aCount: longword): longword;
    function WriteFloat(p: Pointer; aCount: longword): longword;
    function WriteDouble(p: PDouble; aCount: longword): longword;
    function Write(const aBuf: TALSPlaybackBuffer): longword;
  public
    // copy whole audio
    function CopyAll(const aSrcReader: TAudioFileReader): boolean;
    // copy part of audio by frames
    function CopyPart(const aSrcReader: TAudioFileReader; aFromFrameIndex, aToFrameIndex: int64): boolean;

    procedure CopyMetaDataFrom(const aSrcReader: TAudioFileReader);
    procedure WriteMetaData(const aTitle, aCopyright, aSoftware, aArtist,
                            aComment, aDate, aAlbum, aLicense, aTrackNumber, aGenre: string);
  end;


  { TAudioFileReadWrite }

  TAudioFileReadWrite = object(TBaseAudioFile)
    function OpenReadWrite(const aFileName: string): boolean;
    function Close: boolean;
    function ReadSeek(aTimePos: double): boolean;
    function ReadSeek(aFrameIndex: int64): boolean;
    function Read(const aBuf: TALSPlaybackBuffer; aCount: longword): longword;
    function WriteSeek(aTimePos: double): boolean;
    function WriteSeek(aFrameIndex: int64): boolean;
    function Write(const aBuf: TALSPlaybackBuffer): longword;
  end;


  // the libsndfile format used by the project to save audio data (wav PCM16)
  function GetRecordAudioFileFormat: TALSFileFormat;
  function GetTempFilenameForRecord: string;


  function GetAudioFileDuration(const aFilename: string): single;
  function GetAudioFileFramesCount(const aFilename: string): int64;
  // if succed, return 'samplerate:xxx frames:xxx chan:xx format:xxxx'
  // if fail, return 'LibSndFile can not open the file to retrieve info'
  function GetFileInfoForLogMessage(const aFilename: string): string;

  // from 'path/filename.ext'  return 'tempPath/filename_Suffix.ext'
  function GetTemporaryFilename(const aSourceFilename, aSuffix: string): string;

  function CopyPartOfAudioFile(const aSrcFile, aDstFile: string;
                               aFrameIndexLow, aFrameIndexHigh: int64): boolean;

  function CutAudioFile(const aFilename: string;
                        aFrameIndexLow, aFrameIndexHigh: int64): boolean;

  // aAttenuationTime can be =0.0, in that case samples are directly sets to 0
  function SilenceAudioFile(const aFilename: string;
                            aFrameIndexLow, aFrameIndexHigh: int64;
                            aAttenuationTime: single): boolean;
{  function SilenceAudioFile2(const aFilename: string;
                            aFrameIndexLow, aFrameIndexHigh: int64;
                            aAttenuationTime: single): boolean;   }

  function GenerateAudioFileWithSilence(const aFilename: string;
                                      aChannelsCount, aSampleRate: integer;
                                      aFormat: TALSFileFormat;
                                      aDuration: single): boolean;

  function AppendAudioFiles(aFilenamesToAppend: TStringArray;
                            const aTargetWriter: TAudioFileWriter): boolean;
  function AppendAudioFiles(const aTargetFilename: string;
                            const aFilenamesToAppend: TStringArray): boolean;

  function InsertAudioFile(const aTargetFilename,
                           aFilenameToInsert: string;
                           aInsertAtFrameIndex: int64): boolean;

  function InsertSilenceInAudioFile(const aTargetFilename: string;
                                    aFrameIndex: int64; aDuration: single): boolean;

  function ReplacePartByAudioFile(const aTargetFilename: string;
                                  aFrameIndexLow, aFrameIndexHigh: int64;
                                  const aReplaceByFilename: string): boolean;

  // [-48dB to 0dB]
  function RemoveNoiseOnFile(const aNoiseProfileFilename,
                             aFilenameToClean: string;
                             aGain: double): boolean;

  function NormalizeAudioFile(const aFilename: string;
                              aRemoveDC: boolean;
                              aMaxdB: single): boolean;

  // Convert an audio file to another format
  function ConvertAudioFile(const aSrcFile, aDstFile: string;
                            aDstFileFormat: TALSFileFormat;
                            aForceMono: boolean): boolean;

  function ApplyGainOnFile(const aTargetFilename: string; aGaindB: single): boolean;

  // Convert the specified file to its corresponding MP3 filename
  // Writes metadata and amplify the samples by a gain
  function ConvertFileToMP3ConstantBitrate125kbps(const aSrcFile: string;
                                                  aGaindB: single;
                                                  const aMetadata: TALSFileMetaData;
                                                  aDeleteSrcFileIfSuccess: boolean): boolean;

  // Scans the file aFilename and return True if success.
  // Out values are:
  //    adBGain is the gain in dB to apply to reach 89dB.
  //    aPeak is the file max sample peak, expressed in range [0..1]
  // Work only for mono or stereo file.
  function GetdBLevelForFile(const aFilename: string; out adBGain, aPeak: single): boolean;

type

  { TComputeGain89dB }

  TComputeGain89dB = class
  private
    FPeak: integer;
    FLeftBuffer,
    FRightBuffer: array of double;
    FError: boolean;
  public
    function InitGainAnalysis(aSampleRate, aChannelCount: integer; aBufferFrameCapacity: longword): boolean;
    function AnalyzeSamples(const aBuf: TALSFrameBufferBase): boolean;

    function GetTitleGaindB: double;
    function GetTitleGainLinear: single;

    function GetAlbumGaindB: double;
    function GetAlbumGainLinear: single;

    // Peak value range is [0..1]
    function GetPeakValue: single;

    property Error: boolean read FError;
  end;

 { TAudioRecordToFile = record
  private
    writer: TAudioFileWriter;
  public
    Buf: TALSCaptureFrameBuffer;

    function OpenDevice(const aCaptureDeviceName: string; aFrequency: longword;
      aFormat: TALSCaptureFormat; aBufferTimeSize: double): boolean;
    function CloseDevice: boolean;

    function CreateFile(const aFileName: string; aFormat: TALSFileFormat): boolean;
    function WriteBufferToFile: boolean;
    function CloseFile: boolean;

    function StartCapture: boolean;
    // Fill Buf with captured samples. Buf.FrameCount contain the number of obtained samples
    // return True if there samples available in Buf
    function GetCapturedSamples: boolean;
    function StopCapture: boolean;

  end; }

implementation

uses u_project, u_common, Math, utilitaire_fichier, u_logfile,
  dsp_noiseremoval, u_program_options, form_mixer, als_dsp_utils, u_mp3gain,
  Forms;

procedure ExceptionChannelsCount;
begin
  Raise Exception.Create('Audio buffer can not have different channel count than the file');
end;

type

  { TMonoNoiseRemoval }

  TMonoNoiseRemoval = class
  private
    FGain: double;
    FRemover: TNoiseRemoval;
    FWriter: TAudioFileWriter;
    procedure ProcessCleanedAudio({%H-}ASender: TObject; AData: PSingle; ASampleCount: Integer);
  public
    constructor Create(aSampleRate: integer);
    destructor Destroy; override;
    function ProfileNoiseFrom(const aFilename: string): boolean;
    function RemoveNoiseOn(const aSrcFilename: string; out aCleanedFilename: string): boolean;
    // Gain in dB. Range is [-48dB to 0dB]. Default value -24dB
    property Gain: double read FGain write FGain;
  end;

function InsertAudioFile(const aTargetFilename, aFilenameToInsert: string;
  aInsertAtFrameIndex: int64): boolean;
var readerSrc, readerInsert: TAudioFileReader;
    writer: TAudioFileWriter;
    buf: TALSPlaybackBuffer;
    f: string;
    frameToRead, totalFrame: int64;
begin
  Result := False;
  if aInsertAtFrameIndex < 0 then exit;

  // open the target file in read mode
  if not readerSrc.OpenRead(aTargetFilename) then exit;

  // if index is out of bound -> append the file
  if aInsertAtFrameIndex >= readerSrc.Frames-1 then begin
    readerSrc.Close;
    Result := AppendAudioFiles(aTargetFilename, [aFilenameToInsert]);
    exit;
  end;

  // open the file to insert in read mode
  if not readerInsert.OpenRead(aFilenameToInsert) then begin
    readerSrc.Close;
    exit;
  end;

  // check if the 2 file have same channel's count
  if readerSrc.Channels <> readerInsert.Channels then begin
    readerSrc.Close;
    readerInsert.Close;
    exit;
  end;

  // Open a temporary file in write mode
  f := GetTemporaryFilename(aTargetFilename, '_Insert');
  if not writer.OpenWrite(f, readerSrc.Format, readerSrc.Channels, readerSrc.SampleRate) then begin
    readerSrc.Close;
    readerInsert.Close;
    exit;
  end;

  // Init buffer
  if writer.SampleAreFloat then
    buf.Init(writer.Channels, ALS_SAMPLE_FLOAT32)
  else
    buf.Init(writer.Channels, ALS_SAMPLE_INT16);
  buf.FrameCapacity := 32768;
  if buf.OutOfMemory then begin
    readerSrc.Close;
    readerInsert.Close;
    exit;
  end;

  totalFrame := readerSrc.Frames + readerInsert.Frames;

  // copy the first part of the target filename
  if aInsertAtFrameIndex = 0 then
    frameToRead := 0
  else
    frameToRead := aInsertAtFrameIndex+1;

  while frameToRead > 0 do begin
    readerSrc.Read(buf, Min(buf.FrameCapacity, frameToRead));
    writer.Write(buf);
    dec(frameToRead, buf.FrameCount);
    dec(totalFrame, buf.FrameCount);
  end;

  // copy data from the file to insert
  while readerInsert.Read(buf, buf.FrameCapacity) > 0 do begin
    writer.Write(buf);
    dec(totalFrame, buf.FrameCount);
  end;

  // copy the last part of the target filename
  while readerSrc.Read(buf, buf.FrameCapacity) > 0 do begin
    writer.Write(buf);
    dec(totalFrame, buf.FrameCount);
  end;

  // close all files
  buf.FreeMemory;
  readerSrc.Close;
  readerInsert.Close;
  Result := writer.Close and (totalFrame = 0);
  if not Result then begin
    if FichierExistant(f) then SupprimeFichier(f);
    exit;
  end;

  // copy the temporary file to original
  Result := CopieFichier(f, aTargetFilename, True);
  // delete the temporary file
  if Result then SupprimeFichier(f);
end;

function InsertSilenceInAudioFile(const aTargetFilename: string;
  aFrameIndex: int64; aDuration: single): boolean;
var reader: TAudioFileReader;
    writer: TAudioFileWriter;
    buf: TALSPlaybackBuffer;
    f: string;
    frameAdded: int64;
begin
  Result := False;

  // open the target file in read mode
  if not reader.OpenRead(aTargetFilename) then exit;

  // Open a temporary file in write mode
  f := GetTemporaryFilename(aTargetFilename, '_InsertSilence');
  if not writer.OpenWrite(f, reader.Format, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  // copy the first part
  if aFrameIndex > 0 then
    if not writer.CopyPart(reader, 0, aFrameIndex) then begin
      reader.Close;
      writer.Close;
      SupprimeFichier(f);
      exit;
    end;

  frameAdded := Ceil(reader.SampleRate*aDuration);

  // fill a buffer with '0'
  if writer.SampleAreFloat then
    buf.Init(writer.Channels, ALS_SAMPLE_FLOAT32)
  else
    buf.Init(writer.Channels, ALS_SAMPLE_INT16);
  buf.FrameCapacity := frameAdded;
  if buf.OutOfMemory then begin
    reader.Close;
    writer.Close;
    SupprimeFichier(f);
    exit;
  end;
  buf.FillWithSilence;
  buf.FrameCount := buf.FrameCapacity;

  // insert silence
  Result := writer.Write(buf) = buf.FrameCapacity;

  // copy the last part
  if buf.FrameCapacity < 32768 then begin
    buf.FrameCapacity := 32768;
    if buf.OutOfMemory then begin
      reader.Close;
      writer.Close;
      SupprimeFichier(f);
      Result := False;
      exit;
    end;
  end;

  while reader.Read(buf, buf.FrameCapacity) > 0 do
    Result := Result and (writer.Write(buf) = buf.FrameCount);

  reader.Close;
  Result := Result and writer.Close;

  // copy the temporary file to original
  if Result then
    Result := CopieFichier(f, aTargetFilename, True);
  // delete the temporary file
  SupprimeFichier(f);
  buf.FreeMemory;
end;

function ReplacePartByAudioFile(const aTargetFilename: string; aFrameIndexLow,
  aFrameIndexHigh: int64; const aReplaceByFilename: string): boolean;
var readerSrc, readerNewData: TAudioFileReader;
    writer: TAudioFileWriter;
    f: string;
begin
  Result := False;

  // open the target file in read mode
  if not readerSrc.OpenRead(aTargetFilename) then exit;

  // open the file to insert in read mode
  if not readerNewData.OpenRead(aReplaceByFilename) then begin
    readerSrc.Close;
    exit;
  end;

  // check if the 2 file have same channel's count
  if readerSrc.Channels <> readerNewData.Channels then begin
    readerSrc.Close;
    readerNewData.Close;
    exit;
  end;

  // Open a temporary file in write mode
  f := GetTemporaryFilename(aTargetFilename, '_ReplacePart');
  if not writer.OpenWrite(f, readerSrc.Format, readerSrc.Channels, readerSrc.SampleRate) then begin
    readerSrc.Close;
    readerNewData.Close;
    exit;
  end;

  // copy the first part
  if aFrameIndexLow > 0 then
    Result := writer.CopyPart(readerSrc, 0, aFrameIndexLow-1)
  else Result := True;

  // copy the new data
  if Result then
    Result := writer.CopyAll(readerNewData);

  // copy the remaining part
  if Result and (aFrameIndexHigh < readerSrc.Frames-1) then
    Result := writer.CopyPart(readerSrc, aFrameIndexHigh+1, readerSrc.Frames-1);

  readerSrc.Close;
  readerNewData.Close;
  Result := Result and writer.Close;
  if not Result then exit;

  // copy the temporary file to original
  Result := CopieFichier(f, aTargetFilename, True);
  // delete the temporary file
  SupprimeFichier(f);
end;

function RemoveNoiseOnFile(const aNoiseProfileFilename, aFilenameToClean: string;
  aGain: double): boolean;
var Remover: TMonoNoiseRemoval;
  cleanedFile: string;
begin
  Result := False;

  Remover := TMonoNoiseRemoval.Create(Capture.FCaptureContext.Frequency);
  Remover.Gain := aGain;
  if not Remover.ProfileNoiseFrom(aNoiseProfileFilename) then begin
    Remover.Free;
    exit;
  end;

  Result := Remover.RemoveNoiseOn(aFilenameToClean, cleanedFile);
  Remover.Free;
  if not Result then exit;

  // copy the temporary file to original
  Result := CopieFichier(cleanedFile, aFilenameToClean, True);
  // delete the temporary file
  SupprimeFichier(cleanedFile);
end;

function NormalizeAudioFile(const aFilename: string; aRemoveDC: boolean; aMaxdB: single): boolean;
var reader: TAudioFileReader;
  writer: TAudioFileWriter;
  peak, offset, offset2, ratio: double;
  gain: single;
  processedCount, i, j: integer;
  buf: TALSPlaybackBuffer;
  f: string;
  A: ArrayOfSingle;
  frameToRead: int64;
  p: PSingle;
begin
  Result := False;

  if not reader.OpenRead(aFilename) then exit;

  if reader.Frames = 0 then exit(True);

  // init buffer
  buf.Init(reader.Channels, ALS_SAMPLE_FLOAT32);
  buf.FrameCapacity := 32768;
  if buf.OutOfMemory then begin
    reader.Close;
    exit;
  end;

  // compute DC offset and peak levels (within buf) for the whole file
  offset := 0.0;
  processedCount := 0;
  frameToRead := reader.Frames;
  while reader.Read(buf, Min(buf.FrameCapacity, frameToRead)) > 0 do begin
    buf.ComputeChannelsLevel;

    if aRemoveDC then begin
      A := dsp_Mean_Float(PSingle(buf.Data), buf.FrameCount, buf.ChannelCount);
      offset2 := 0.0;
      for i:=0 to High(A) do
        offset2 := offset2 + A[i];
      offset := offset + offset2/Length(A);
      inc(processedCount);
    end;

    frameToRead := frameToRead-buf.FrameCount;
  end;

  if aRemoveDC then begin
    if processedCount > 0 then
      offset := offset/processedCount
    else
      offset := 0.0;
  end;

  // all samples are processed ?
  if frameToRead > 0 then begin
    reader.Close;
    buf.FreeMemory;
    exit;
  end;

  // get the maximum peak between channels
  peak :=0.0;
  for i:=0 to buf.ChannelCount-1 do
    if peak < buf.ChannelsPeak[i] then peak := buf.ChannelsPeak[i];

  // compute the gain to apply
  ratio := dBToLinear(EnsureRange(aMaxdB, ALS_DECIBEL_MIN_VALUE, ALS_DECIBEL_MAX_VALUE));
  if peak > 0 then
    gain := ratio / (peak - offset)
  else
    gain := 1.0;

  // create a temporary file in write mode
  f := GetTemporaryFilename(aFilename, '_Normalize');
  if not writer.OpenWrite(f, reader.Format, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    buf.FreeMemory;
    if FichierExistant(f) then SupprimeFichier(f);
    exit;
  end;

  // place reader to its beginning
  if not reader.MoveToFrame(0) then begin
    reader.Close;
    writer.Close;
    buf.FreeMemory;
    if FichierExistant(f) then SupprimeFichier(f);
    exit;
  end;

  // read samples, apply offset and gain, write samples
  Result := True;
  frameToRead := reader.Frames;
  while reader.Read(buf, Min(buf.FrameCapacity, frameToRead)) > 0 do begin
    p := PSingle(buf.Data);
    for i:=0 to buf.FrameCount-1 do
      for j:=0 to buf.ChannelCount-1 do begin
        p^ := (p^ + offset) * gain;
        inc(p);
      end;

    Result := Result and (writer.Write(buf) = buf.FrameCount);

    frameToRead := frameToRead - buf.FrameCount;
  end;

  Result := Result and (frameToRead = 0);

  reader.Close;
  Result := Result and writer.Close;
  buf.FreeMemory;

  // copy the temporary file to the original
  if Result then Result := CopieFichier(f, aFilename, True);
  // delete the temporary file
  SupprimeFichier(f);
end;

function ConvertAudioFile(const aSrcFile, aDstFile: string;
  aDstFileFormat: TALSFileFormat; aForceMono: boolean): boolean;
var reader: TAudioFileReader;
  bufRead, bufWrite: TALSPlaybackBuffer;
  writer: TAudioFileWriter;
  metadata: TALSFileMetaData;
  chanWrite, i, j: integer;
  written: longword;
  pRead, pWrite: PSingle;
  sum: single;
begin
  Result := False;

  // open file in read mode
  if not reader.OpenRead(aSrcFile) then exit;

  // read metadata
  metadata.ReadMetaDataFrom(reader.Handle);

  if aForceMono
    then chanWrite := 1
    else chanWrite := reader.Channels;

  // Open dst file in write mode
  if not writer.OpenWrite(aDstFile, aDstFileFormat, chanWrite, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  // write metadata
  metadata.WriteMetaDataTo(writer.Handle);

  // prepare buffers
  bufRead.Init(reader.Channels, ALS_SAMPLE_FLOAT32);
  bufRead.FrameCapacity := 32768;
  if bufRead.OutOfMemory then begin
    reader.Close;
    writer.Close;
    SupprimeFichier(aDstFile);
    exit;
  end;

  if chanWrite = reader.Channels then begin
    // simple copy
    Result := True;
    while (reader.Read(bufRead, bufRead.FrameCapacity) > 0) and Result do begin
      written := writer.Write(bufRead);
      Result := written = bufRead.FrameCount;
    end;

  end else begin
    // converts multi channels to MONO
    bufWrite.Init(chanWrite, bufRead.Format);
    bufWrite.FrameCapacity := bufRead.FrameCapacity;
    if bufWrite.OutOfMemory then begin
      bufRead.FreeMemory;
      reader.Close;
      writer.Close;
      SupprimeFichier(aDstFile);
      exit;
    end;

    Result := True;
    while (reader.Read(bufRead, bufRead.FrameCapacity) > 0) and Result do begin
      pRead := PSingle(bufRead.Data);
      pWrite := PSingle(bufWrite.Data);
      for i:=1 to bufRead.FrameCount do begin
        sum := 0;
        for j:=1 to bufRead.ChannelCount do begin
          sum := sum+pRead^;
          inc(pRead);
        end;
        pWrite^ := sum/bufRead.ChannelCount;
        inc(pWrite);
      end;
      bufWrite.FrameCount := bufRead.FrameCount;
      written := writer.Write(bufWrite);
      Result := written = bufRead.FrameCount;
    end;
    bufWrite.FreeMemory;
  end;

  bufRead.FreeMemory;
  reader.Close;
  Result := Result and writer.Close;

  if not Result then
    SupprimeFichier(aDstFile);
end;

function ApplyGainOnFile(const aTargetFilename: string; aGaindB: single): boolean;
var reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  writer: TAudioFileWriter;
  f: string;
  g: single;
  written: longword;
  metadata: TALSFileMetaData;
begin
  Result := False;
  // open file in read mode
  if not reader.OpenRead(aTargetFilename) then exit;

  // read metadata
  metadata.ReadMetaDataFrom(reader.Handle);

  // Open a temporary file in write mode
  f := GetTemporaryFilename(aTargetFilename, '_ApplyGain');
  if not writer.OpenWrite(f, reader.Format, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  // write metadata
  metadata.WriteMetaDataTo(writer.Handle);

  // prepare buffer
  buf.Init(reader.Channels, ALS_SAMPLE_FLOAT32);
  buf.FrameCapacity := 32768;

  Result := True;
  g := dBToLinear(aGaindB);
  // read audio in buffer and apply gain
  while (reader.Read(buf, buf.FrameCapacity) > 0) and Result do begin
    buf.Amplify(g);
    written := writer.Write(buf);
    Result := written = buf.FrameCount;
  end;

  reader.Close;
  buf.FreeMemory;
  Result := Result and writer.Close;

  if Result then begin
    Result := CopieFichier(f, aTargetFilename, True);
    if Result then
      SupprimeFichier(f);
  end;
end;

function ConvertFileToMP3ConstantBitrate125kbps(const aSrcFile: string;
  aGaindB: single; const aMetadata: TALSFileMetaData; aDeleteSrcFileIfSuccess: boolean): boolean;
var reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  writer: TAudioFileWriter;
  f: string;
  g: single;
  written: longword;
  mode: cint;
  compressLevel: double;
begin
  Result := False;
  // open file in read mode
  if not reader.OpenRead(aSrcFile) then exit;

  // check channels count
  if not (reader.Channels in [1..2]) then begin
    reader.Close;
    exit;
  end;

  // Open a temporary file in write mode
  f := ChangeFileExt(aSrcFile, '.mp3');
  if not writer.OpenWrite(f, SF_FORMAT_MPEG or SF_FORMAT_MPEG_LAYER_III, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  // set the compression level. see libsndfile/src/mpeg_I3_encode.c line 215
  // we must sets the compression level BEFORE setting the bitrate mode !!
  // at 44100Hz the formula is
  //          bitrate = 320 - compression*(320-32)
  //  =>  compression = (320 - bitrate) / (320-32)
  // we want a bitrate = 125kbps => compression = 0.6666666
  compressLevel := 0.6666666;
  if sf_command(writer.Handle,
                SFC_SET_COMPRESSION_LEVEL,
                @compressLevel,
                SizeOf(double)) <> SF_TRUE then begin
    reader.Close;
    writer.Close;
    exit;
  end;

  // set the bitrate mode
  mode := cint(SF_BITRATE_MODE_CONSTANT);
  if sf_command(writer.Handle,
                SFC_SET_BITRATE_MODE, @mode, SizeOf(cint)) <> SF_TRUE then begin
    reader.Close;
    writer.Close;
    exit;
  end;

  // write metadata
  aMetadata.WriteMetaDataTo(writer.Handle);

  // prepare buffer
  buf.Init(reader.Channels, ALS_SAMPLE_FLOAT32);
  buf.FrameCapacity := 32768;

  Result := True;
  g := dBToLinear(aGaindB);
  // read audio in buffer and apply gain
  while (reader.Read(buf, buf.FrameCapacity) > 0) and Result do begin
    if aGaindB <> 0 then
      buf.Amplify(g);
    written := writer.Write(buf);
    Result := written = buf.FrameCount;
  end;

  reader.Close;
  buf.FreeMemory;
  Result := Result and writer.Close;

  if Result and aDeleteSrcFileIfSuccess then
    SupprimeFichier(aSrcFile);
end;

function GetdBLevelForFile(const aFilename: string; out adBGain, aPeak: single): boolean;
var reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  analyzer: TComputeGain89dB;
begin

  Result := reader.OpenRead(aFilename);
  if not Result then exit;

  if (reader.Frames = 0) or not (reader.Channels in [1..2]) then begin
    reader.Close;
    Result := False;
    exit;
  end;

  // prepare read buffer
  buf.Init(reader.Channels, ALS_SAMPLE_INT16);
  buf.FrameCapacity := 32768;

  // prepare analyzer
  analyzer := TComputeGain89dB.Create;
  if not analyzer.InitGainAnalysis(reader.SampleRate, reader.Channels, buf.FrameCapacity) then begin
    analyzer.Free;
    reader.Close;
    Result := False;
    exit;
  end;

  try
    Result := True;
    repeat
      if reader.Read(buf, buf.FrameCapacity) > 0 then
        Result := Result and analyzer.AnalyzeSamples(buf);
    until (buf.FrameCount = 0) or not Result;

    if Result then begin
      adBGain := analyzer.GetTitleGaindB;
      aPeak := analyzer.GetPeakValue;
    end;
  except
    Result := False;
  end;

  reader.Close;
  buf.FreeMemory;
  analyzer.Free;
end;

procedure ProcessLogMessageFromOpenALSoft(aUserPtr: pointer; aLevel: char;
  aMessage: PChar; aMessageLength: cint);
const aprefix='alsoft: ';
begin
  if Log = NIL then exit;

  case aLevel of
    'I': Log.Info(aprefix+StrPas(aMessage));
    'W': Log.Warning(aprefix+StrPas(aMessage));
    'E': Log.Error(aprefix+StrPas(aMessage));
    else Log.Warning(aprefix+StrPas(aMessage));
  end;
end;

function GetRecordAudioFileFormat: TALSFileFormat;
begin
  Result := ALSMakeFileFormat(ALSound.SF_FORMAT_WAV, ALSound.SF_FORMAT_PCM_16);
end;

function GetTempFilenameForRecord: string;
begin
  inc(FRecordFilenameSuffix);
  Result := Project.TempFolder+
            RECORDING_TEMP_FILENAME+
            Format('%.2d',[FRecordFilenameSuffix])+
            RECORDING_TEMP_FILEEXTENSION;
end;

function GetAudioFileDuration(const aFilename: string): single;
var reader: TAudioFileReader;
begin
  Result := 0;
  if reader.OpenRead(aFilename) then begin
    Result := reader.TotalDuration;
    reader.Close;
  end;
end;

function GetAudioFileFramesCount(const aFilename: string): int64;
var reader: TAudioFileReader;
begin
  Result := 0;
  if reader.OpenRead(aFilename) then begin
    Result := reader.Frames;
    reader.Close;
  end;
end;

function GetFileInfoForLogMessage(const aFilename: string): string;
var reader: TAudioFileReader;
begin
  Result := '';
  if reader.OpenRead(aFilename, False) then begin
    Result := 'samplerate:'+reader.SampleRate.ToString+
              ' frames:'+reader.Frames.ToString+
              ' chan:'+reader.Channels.ToString+
              ' format:'+IntToHex(reader.Format, 4);
    reader.Close;
  end else Result := 'LibSndFile can not open the file to retrieve info';
end;

function GetTemporaryFilename(const aSourceFilename, aSuffix: string): string;
begin
  Result := Project.TempFolder+
       ChangeFileExt(ExtractFilename(aSourceFilename), '')+
       aSuffix+
       ExtractFileExt(aSourceFilename);
end;

function CopyPartOfAudioFile(const aSrcFile, aDstFile: string; aFrameIndexLow,
  aFrameIndexHigh: int64): boolean;
var reader: TAudioFileReader;
  writer: TAudioFileWriter;
begin
  Result := False;
  if not reader.OpenRead(aSrcFile) then exit;

  if not writer.OpenWrite(aDstFile, reader.Format, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  Result := writer.CopyPart(reader, aFrameIndexLow, aFrameIndexHigh);
  reader.Close;
  Result := Result and writer.Close;
end;

function CutAudioFile(const aFilename: string; aFrameIndexLow, aFrameIndexHigh: int64): boolean;
var inputFile: TAudioFileReader;
  writer: TAudioFileWriter;
  f: string;
begin
  if aFrameIndexLow = aFrameIndexHigh then begin
    Result := True;
    exit;
  end;

  Result := False;
  if (aFrameIndexLow < 0) or
     (aFrameIndexHigh < 0) or
     (aFrameIndexHigh < aFrameIndexLow) then exit;

  // open the file in read mode
  if not inputFile.OpenRead(aFilename) then exit;

  // Open a temporary file in write mode
  f := GetTemporaryFilename(aFilename, '_Cut');
  if not writer.OpenWrite(f, inputFile.Format, inputFile.Channels, inputFile.SampleRate) then begin
    inputFile.Close;
    exit;
  end;

  // copy the first part
  if aFrameIndexLow > 0 then
    writer.CopyPart(inputFile, 0, aFrameIndexLow-1);

  // copy the last part
  writer.CopyPart(inputFile, aFrameIndexHigh+1, inputFile.Frames-1);

  inputFile.Close;
  if not writer.Close then exit;

  // copy the temporary file to original
  Result := CopieFichier(f, aFilename, True);
  // delete the temporary file
  if Result then SupprimeFichier(f);
end;

function SilenceAudioFile(const aFilename: string; aFrameIndexLow,
  aFrameIndexHigh: int64; aAttenuationTime: single): boolean;
var reader: TAudioFileReader;
  writer: TAudioFileWriter;
  buf: TALSPlaybackBuffer;
  f: string;
  gain: TALSCustomBoundedFParam;
  p: PSingle;
  i, j, iRampUp: integer;
begin
  if (aFrameIndexLow = aFrameIndexHigh) then begin
    Result := True;
    exit;
  end;

  Result := False;
  if (aFrameIndexLow < 0) or
     (aFrameIndexHigh < 0) or
     (aFrameIndexHigh < aFrameIndexLow) or
     (aAttenuationTime < 0) then exit;

  // open the file in read mode
  if not reader.OpenRead(aFilename) then exit;

  if reader.FrameToTime(aFrameIndexHigh-aFrameIndexLow+1) < aAttenuationTime*2 then
    aAttenuationTime := 0;

  // open a temporary file in write mode
  f := GetTemporaryFilename(aFilename, '_Silence');
  if not writer.OpenWrite(f, reader.Format, reader.Channels, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  // copy the first part
  if aFrameIndexLow > 0 then
    if not writer.CopyPart(reader, 0, aFrameIndexLow-1) then begin
      reader.Close;
      writer.Close;
      if FichierExistant(f) then SupprimeFichier(f);
      exit;
    end;

  // read the whole selection in a buffer
  buf.Init(reader.Channels, ALS_SAMPLE_FLOAT32);
  buf.FrameCapacity := aFrameIndexHigh - aFrameIndexLow + 1;
  if buf.OutOfMemory then begin
    reader.Close;
    writer.Close;
    if FichierExistant(f) then SupprimeFichier(f);
    exit;
  end;

  if reader.Read(buf, buf.FrameCapacity) <> buf.FrameCapacity then begin
    reader.Close;
    writer.Close;
    if FichierExistant(f) then SupprimeFichier(f);
    buf.FreeMemory;
    exit;
  end;

  // apply silence on the buffer
  if aAttenuationTime = 0 then
    buf.FillWithSilence
  else begin
    gain := TALSCustomBoundedFParam.Create(0.0, 1.0, 1.0);
    gain.ChangeTo(0.0, aAttenuationTime, ALS_StartFastEndSlow);
    iRampUp := buf.FrameCapacity-Trunc(reader.SampleRate*aAttenuationTime);
    for i:=0 to buf.FrameCapacity-1 do begin
      p := PSingle(buf.DataOffset[i]);
      for j:=0 to buf.ChannelCount-1 do begin
        p^ := p^*gain.Value;
        inc(p);
      end;
      gain.OnElapse(1/reader.SampleRate);
      if (i >= iRampUp) and (gain.State = alspsNO_CHANGE) then
        gain.ChangeTo(1.0, aAttenuationTime, ALS_StartSlowEndFast);
    end;
    gain.Free;
  end;

  // write buffer
  if writer.Write(buf) <> buf.FrameCount then begin
    reader.Close;
    writer.Close;
    if FichierExistant(f) then SupprimeFichier(f);
    buf.FreeMemory;
    exit;
  end;

  // copy the remaining part
  buf.FrameCapacity := 32768;
  Result := True;
  while (reader.Read(buf, buf.FrameCapacity) > 0) and Result do
    Result := Result and (writer.Write(buf) = buf.FrameCount);

  buf.FreeMemory;
  reader.Close;
  Result := Result and writer.Close;

  if Result then
    Result := CopieFichier(f, aFilename, True);
  SupprimeFichier(f);
end;

{function SilenceAudioFile2(const aFilename: string; aFrameIndexLow, aFrameIndexHigh: int64;
  aAttenuationTime: single): boolean;
var audioFile: TAudioFileReadWrite;
  buf: TALSPlaybackBuffer;
  gain: TBoundedFParam;
  p: PSingle;
  i, j, iRampUp: integer;
begin
  if aFrameIndexLow = aFrameIndexHigh then begin
    Result := True;
    exit;
  end;

  Result := False;
  if (aFrameIndexLow < 0) or
     (aFrameIndexHigh < 0) or
     (aFrameIndexHigh < aFrameIndexLow) or
     (aAttenuationTime < 0) then exit;

  // open the file in read/write mode
  if not audioFile.OpenReadWrite(aFilename) then exit;

  if audioFile.FrameToTime(aFrameIndexHigh-aFrameIndexLow) <= aAttenuationTime*2 then
    aAttenuationTime := 0;

  // seek at the right position
  if not audioFile.ReadSeek(aFrameIndexLow) or
     not audioFile.WriteSeek(aFrameIndexLow) then begin
    audioFile.Close;
    exit;
  end;

  // read the whole selection in a buffer
  buf.Init(audioFile.Channels, ALS_SAMPLE_FLOAT32);
  buf.FrameCapacity := aFrameIndexHigh - aFrameIndexLow + 1;
  if buf.OutOfMemory then begin
    audioFile.Close;
    exit;
  end;

  if audioFile.Read(buf, buf.FrameCapacity) <> buf.FrameCapacity then begin
    audioFile.Close;
    buf.FreeMemory;
    exit;
  end;

  // apply silence on the buffer
  if aAttenuationTime = 0 then
    buf.FillWithSilence
  else begin
    gain := TBoundedFParam.Create(0.0, 1.0, 1.0);
    gain.ChangeTo(0.0, aAttenuationTime, ALS_StartFastEndSlow);
    iRampUp := buf.FrameCapacity-Trunc(audioFile.SampleRate*aAttenuationTime);
    for i:=0 to buf.FrameCapacity-1 do begin
      p := PSingle(buf.DataOffset[i]);
      for j:=0 to buf.ChannelCount-1 do begin
        p^ := p^*gain.Value;
        inc(p);
      end;
      gain.OnElapse(1/audioFile.SampleRate);
      if (i >= iRampUp) and (gain.State = psNO_CHANGE) then
        gain.ChangeTo(1.0, aAttenuationTime, ALS_StartSlowEndFast);
    end;
    gain.Free;
  end;

  // write buffer and close the file
  Result := audioFile.Write(buf) = buf.FrameCount;
  buf.FreeMemory;
  Result := Result and audioFile.Close;
end;  }

function GenerateAudioFileWithSilence(const aFilename: string; aChannelsCount,
  aSampleRate: integer; aFormat: TALSFileFormat; aDuration: single): boolean;
var writer: TAudioFileWriter;
  buf: TALSPlaybackBuffer;
  frameToWrite, frameWritten, c: int64;
begin
  Result := False;

  frameToWrite := Ceil(aSampleRate*aDuration);
  if frameToWrite <= 0 then exit;

  buf.Init(aChannelsCount, ALS_SAMPLE_INT16);
  buf.FrameCapacity := 32768;
  if buf.OutOfMemory then exit;
  buf.FillWithSilence;

  Result := writer.OpenWrite(aFilename, aFormat, aChannelsCount, aSampleRate);
  if not Result then begin
    buf.FreeMemory;
    exit;
  end;

  frameWritten := 0;
  c := frameToWrite;
  while c > 0 do begin
    buf.FrameCount := Min(buf.FrameCapacity, c);
    frameWritten := frameWritten + writer.Write(buf);
    dec(c, buf.FrameCount);
  end;

  writer.Close;
  buf.FreeMemory;
  Result := frameWritten = frameToWrite;
end;

function AppendAudioFiles(aFilenamesToAppend: TStringArray;
  const aTargetWriter: TAudioFileWriter): boolean;
var reader: TAudioFileReader;
  i: integer;
begin
  Result := False;

  for i:=0 to High(aFilenamesToAppend) do begin
    if not reader.OpenRead(aFilenamesToAppend[i]) then exit;

    Result := aTargetWriter.CopyAll(reader);
    reader.Close;
    if not Result then exit;
  end;
end;

function AppendAudioFiles(const aTargetFilename: string;
  const aFilenamesToAppend: TStringArray): boolean;
var reader: TAudioFileReader;
  writer: TAudioFileWriter;
  i: integer;
  f: string;
  flagFicOpened: boolean;
begin
  Result := False;

  // open the target file to retrieve its info
  if not reader.OpenRead(aTargetFilename) then exit;

  // open a temporary file in write mode with same attributes as the target file
  f := GetTemporaryFilename(aTargetFilename, '_Append');
  if not writer.OpenWrite(f, reader.Format, reader.Channels, reader.SampleRate) then exit;

  // Copy the content of aTargetFilename
  if not writer.CopyAll(reader) then begin
    reader.Close;
    writer.Close;
    exit;
  end;
  reader.Close;

  // Copy the content of each other files
  Result := True;
  for i:=0 to High(aFilenamesToAppend) do begin
    if Result then Result := reader.OpenRead(aFilenamesToAppend[i]);
    flagFicOpened := Result;
    if Result then Result := writer.CopyAll(reader);
    if flagFicOpened then reader.Close;
  end;

  Result := Result and writer.Close;
  if not Result then exit;

  // copy the temporary file to original
  Result := CopieFichier(f, aTargetFilename, True);
  // delete the temporary file
  if Result then SupprimeFichier(f);
end;

{ TComputeGain89dB }

function TComputeGain89dB.InitGainAnalysis(aSampleRate, aChannelCount: integer;
  aBufferFrameCapacity: longword): boolean;
begin
  Result := u_mp3gain.InitGainAnalysis(aSampleRate) and
            (aChannelCount in [1..2]) and
            (aBufferFrameCapacity > 0);
  FPeak := 0;
  FLeftBuffer := NIL;
  FRightBuffer := NIL;
  SetLength(FLeftBuffer, aBufferFrameCapacity);
  if aChannelCount = 2 then
    SetLength(FRightBuffer, aBufferFrameCapacity);
  FError := not Result;
end;

function TComputeGain89dB.AnalyzeSamples(const aBuf: TALSFrameBufferBase): boolean;
var i: integer;
  p: PSmallInt;
  ps: PSingle;
  v: integer;
begin
  Result := False;
  if FError then exit;
  if aBuf.FrameCount > Length(FLeftBuffer) then exit;

  if aBuf.FrameCount = 0 then begin
    Result := True;
    exit;
  end;

  if aBuf.UseFloat then begin
    ps := PSingle(aBuf.Data);
    case aBuf.ChannelCount of
      1: begin  // mono
        for i:=0 to abuf.FrameCount-1 do begin
          FLeftBuffer[i] := double(ps^*32767);
          v := Abs(Trunc(FLeftBuffer[i]));
          if FPeak < v then FPeak := v;
          inc(ps);
        end;
        Result := u_mp3gain.AnalyzeSamples(@FLeftBuffer[0], NIL, abuf.FrameCount, 1);
      end;
      2: begin  // stereo
        for i:=0 to abuf.FrameCount-1 do begin
          FLeftBuffer[i] := double(ps^*32767);
          FRightBuffer[i] := double(ps[1]*32767);
          v := Abs(Trunc(FLeftBuffer[i]));
          if FPeak < v then FPeak := v;
          v := Abs(Trunc(FRightBuffer[i]));
          if FPeak < v then FPeak := v;
          inc(ps, 2);
        end;
        Result := u_mp3gain.AnalyzeSamples(@FLeftBuffer[0], @FRightBuffer[0], abuf.FrameCount, 2);
      end;
      else Result := False;
    end;
  end else begin
    p := PSmallInt(aBuf.Data);
    case aBuf.ChannelCount of
      1: begin  // mono
        for i:=0 to abuf.FrameCount-1 do begin
          FLeftBuffer[i] := double(p^);
          if FPeak < Abs(p^) then FPeak := Abs(p^);
          inc(p);
        end;
        Result := u_mp3gain.AnalyzeSamples(@FLeftBuffer[0], NIL, abuf.FrameCount, 1);
      end;
      2: begin  // stereo
        for i:=0 to abuf.FrameCount-1 do begin
          FLeftBuffer[i] := double(p^);
          FRightBuffer[i] := double(p[1]);
          if FPeak < Abs(p^) then FPeak := Abs(p^);
          if FPeak < Abs(p[1]) then FPeak := Abs(p[1]);
          inc(p, 2);
        end;
        Result := u_mp3gain.AnalyzeSamples(@FLeftBuffer[0], @FRightBuffer[0], abuf.FrameCount, 2);
      end;
      else Result := False;
    end;
  end;
  FError := not Result;
end;

function TComputeGain89dB.GetTitleGaindB: double;
begin
  Result := u_mp3gain.GetTitleGain;
end;

function TComputeGain89dB.GetTitleGainLinear: single;
begin
  Result := dBToLinear(GetTitleGaindB);
end;

function TComputeGain89dB.GetAlbumGaindB: double;
begin
  Result := u_mp3gain.GetAlbumGain;
end;

function TComputeGain89dB.GetAlbumGainLinear: single;
begin
  Result := dBToLinear(GetAlbumGaindB);
end;

function TComputeGain89dB.GetPeakValue: single;
begin
  Result := FPeak/32768;
end;

{ TCaptureContext }

procedure TCaptureContext.Create(aDeviceIndex: integer);
begin
  if Length(ALSManager.ListOfCaptureDeviceName) = 0 then
    Log.Warning('gyv: OpenAL-Soft return no capture device !');

  FCaptureContext := ALSManager.CreateCaptureContext(aDeviceIndex, 44100,
      ALS_CAPTUREFORMAT_MONO_FLOAT32, 0.100);

  if FCaptureContext.Error then begin
    Log.Error('alsound: failed to create capture context');
    Log.Mess('with device index: '+aDeviceIndex.ToString+'   max device index on this system: '+High(ALSManager.ListOfCaptureDeviceName).ToString, 1);
    if InRange(aDeviceIndex, Low(ALSManager.ListOfCaptureDeviceName), High(ALSManager.ListOfCaptureDeviceName)) then
      Log.Mess('device name: '+ALSManager.ListOfCaptureDeviceName[aDeviceIndex], 1);
    Log.Mess(FCaptureContext.StrError, 1);
  end;
end;

procedure TCaptureContext.Free;
begin
  FCaptureContext.Free;
  FCaptureContext := NIL;
end;

{ TPlaybackContext }

procedure TPlaybackContext.DeleteContextAndEffect(var aContext: TALSPlaybackContext);
begin
  aContext.DeleteAll;
  aContext.DeleteEffect(FCompressor);
  aContext.DeleteEffect(FBassBoostEqualizer);
  aContext.Free;
  aContext := NIL;
end;

procedure TPlaybackContext.Create(aDeviceIndex: integer);
var attribs: TALSContextAttributes;
begin
  if Length(ALSManager.ListOfPlaybackDeviceName) = 0 then
    Log.Warning('gyv: OpenAL-Soft return no playback device !');

  attribs.InitDefault;
  attribs.EnableOutputLimiter := True;
  attribs.ContextUseFloat := True;

  if (aDeviceIndex < 0) or (aDeviceIndex >= Length(ALSManager.ListOfPlaybackDeviceName)) then
    Log.Warning('gyv: TPlaybackContext.Create try to open a playback device with index out of range '+aDeviceIndex.ToString+'/'+(Length(ALSManager.ListOfPlaybackDeviceName)-1).ToString);

  FPlaybackContext := ALSManager.CreatePlaybackContext(aDeviceIndex, attribs);
  if FPlaybackContext.Error then begin
    Log.Error('alsound: failed to create playback context');
    Log.Mess('device index: '+aDeviceIndex.ToString+'   max device index on this system: '+High(ALSManager.ListOfPlaybackDeviceName).ToString, 1);
    if InRange(aDeviceIndex, Low(ALSManager.ListOfPlaybackDeviceName), High(ALSManager.ListOfPlaybackDeviceName)) then
      Log.Mess('device name: '+ALSManager.ListOfPlaybackDeviceName[aDeviceIndex], 1);
    Log.Mess(FPlaybackContext.StrError, 1);
  end;

  FCompressorProp.InitDefault;
  FCompressor := FPlaybackContext.CreateEffect(AL_EFFECT_COMPRESSOR, FCompressorProp);
  if not FCompressor.Ready then
    Log.Error('alsound: failed to create compressor effect for playback context');

  FBassBoostEqualizerProp.InitWithPreset(1); // Bass Boost
  FBassBoostEqualizerProp.LowGain := ProgramOptions.ListeningImprovedBassGainValue;
  FBassBoostEqualizer := FPlaybackContext.CreateEffect(AL_EFFECT_EQUALIZER, FBassBoostEqualizerProp);
  if not FBassBoostEqualizer.Ready then
    Log.Error('alsound: failed to create bass boost equalizer for playback context');

  FErrorOnChainEffect := not FCompressor.ChainWith(FBassBoostEqualizer);
  if FErrorOnChainEffect then
    Log.Error('alsound: failed to chain audio effects for playback context');

  FCompressor.Mute := not ProgramOptions.ListeningImprovedUseCompressor;
  FBassBoostEqualizer.Mute := not (ProgramOptions.ListeningImprovedActivated and
                     (ProgramOptions.ListeningImprovedBassGainValue <> 1.0));

end;

procedure TPlaybackContext.Free;
begin
  if FPlaybackContext = NIL then exit;
  DeleteContextAndEffect(FPlaybackContext);
end;

procedure TPlaybackContext.CreateLoopback;
var attribs: TALSContextAttributes;
begin
  FLoopbackContext := ALSManager.CreateDefaultLoopbackContext;

  attribs.InitDefault;
  attribs.EnableOutputLimiter := True;
  attribs.SetLoopbackMode(44100, ALC_MONO_SOFT, ALC_FLOAT_SOFT);
  FLoopBackContext.InitContext(attribs);

  FCompressorProp.InitDefault;
  FCompressor := FLoopbackContext.CreateEffect(AL_EFFECT_COMPRESSOR, FCompressorProp);

  FBassBoostEqualizerProp.InitWithPreset(1); // Bass Boost
  FBassBoostEqualizerProp.LowGain := FormMixer.GetBassGain;
  FBassBoostEqualizer := FLoopbackContext.CreateEffect(AL_EFFECT_EQUALIZER, FBassBoostEqualizerProp);

  FErrorOnChainEffect := not FCompressor.ChainWith(FBassBoostEqualizer);

  if FLoopbackContext.Error then begin
    Log.Error('alsound: failed to create loopback context');
    Log.Mess(FLoopbackContext.StrError, 1);
  end;
  if not FCompressor.Ready then
    Log.Error('alsound: failed to create compressor effect for loopback context');
  if not FBassBoostEqualizer.Ready then
    Log.Error('alsound: failed to create bass boost equalizer for loopback context');
  if FErrorOnChainEffect then
    Log.Error('alsound: failed to chain audio effects for loopback context');
end;

procedure TPlaybackContext.FreeLoopback;
begin
  if FLoopbackContext = NIL then exit;
  DeleteContextAndEffect(TALSPlaybackContext(FLoopbackContext));
end;

{ TBaseAudioFile }

function TBaseAudioFile.TimeToFrameIndex(aTimePos: double): int64;
begin
  Result := Round(SampleRate*aTimePos);
end;

function TBaseAudioFile.FrameToTime(aFrameCount: int64): double;
begin
  Result := aFrameCount/SampleRate;
end;

procedure TBaseAudioFile.LogLibSndFileErrorMessage(const aMess: string);
begin
  Log.Error('gyv: '+aMess);
  Log.Error('on file "'+FileName+'"', 1);
  Log.Error(GetLibSndFileErrMess);
  Log.AddEmptyLine;
end;

function TBaseAudioFile.GetLibSndFileErrMess: string;
var i: integer;
  errmsg: string;
begin
  if Handle = NIL then begin
    Result := 'Handle = NIL';
    exit;
  end;

  Result := '';

  errmsg := '';
  SetLength(errmsg, 2048);
  sf_command(Handle, SFC_GET_LOG_INFO, @errmsg[1], Length(errmsg));

  i := 1;
  while (errmsg[i] <> #0) and (i < Length(errmsg)) do
    inc(i);
  Result := Copy(errmsg, 1, i);
end;

{ TMonoNoiseRemoval }

procedure TMonoNoiseRemoval.ProcessCleanedAudio(ASender: TObject;
  AData: PSingle; ASampleCount: Integer);
begin
  FWriter.WriteFloat(AData, ASampleCount);
end;

constructor TMonoNoiseRemoval.Create(aSampleRate: integer);
begin
  FRemover := TNoiseRemoval.Create;
  FRemover.Init(aSampleRate);
  FRemover.WriteProc := @ProcessCleanedAudio;
end;

destructor TMonoNoiseRemoval.Destroy;
begin
  FRemover.Free;
  inherited Destroy;
end;

function TMonoNoiseRemoval.ProfileNoiseFrom(const aFilename: string): boolean;
var reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  toSend: int64;
begin
  FRemover.Gain := FGain;
  Result := False;
  if not reader.OpenRead(aFilename) then exit;
  if reader.Channels <> 1 then begin
    reader.Close;
    exit;
  end;
  buf.Init(1, ALS_SAMPLE_FLOAT32);
  buf.FrameCapacity := 1024;
  toSend := reader.Frames;

  Result := True;
  while reader.Read(buf, buf.FrameCapacity) > 0 do begin
    toSend := toSend-buf.FrameCount;
    Result := Result and FRemover.Process(buf.Data, buf.FrameCount, True, True);
  end;

  Result := Result and FRemover.Process(NIL, 0, True, False);

  reader.Close;
  buf.FreeMemory;
end;

function TMonoNoiseRemoval.RemoveNoiseOn(const aSrcFilename: string;
  out aCleanedFilename: string): boolean;
var f: string;
  reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  frameToDo: int64;
  counterTime: single;
begin
  FRemover.Gain := FGain;
  Result := False;
  aCleanedFilename := '';

  if not reader.OpenRead(aSrcFilename) then exit;
  if (reader.Channels <> 1) or
     not FRemover.Init(reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  f := GetTemporaryFilename(aSrcFilename, '_NoiseRemove');
  if not FWriter.OpenWrite(f, reader.Format, 1, reader.SampleRate) then begin
    reader.Close;
    exit;
  end;

  buf.Init(1, ALS_SAMPLE_FLOAT32);
  buf.FrameCapacity := 1024; // Round(reader.SampleRate*0.050); // 50ms of audio

  frameToDo := reader.Frames;
  Result := True;
  counterTime := 0;
  while reader.Read(buf, buf.FrameCapacity) > 0 do begin
    frameToDo := frameToDo-buf.FrameCount;
    Result := Result and FRemover.Process(buf.Data, buf.FrameCount, False, (frameToDo > 0));

    // call application.ProcessMessage every 20ms
    counterTime := counterTime + buf.FrameCount/reader.SampleRate;
    if counterTime > 0.020 then begin
      counterTime := counterTime - 0.020;
      Application.ProcessMessages;
    end;

  end;
  FRemover.Process(NIL, 0, False, False);
  FRemover.Flush;
  FWriter.Close;
  reader.Close;
  buf.FreeMemory;
  if Result then aCleanedFilename := f;
end;

{ TAudioFileReadWrite }

function TAudioFileReadWrite.OpenReadWrite(const aFileName: string): boolean;
var sfinfo: TSF_INFO;
begin
  sfinfo.Format := 0;
  Handle := ALSOpenAudioFile(aFileName, SFM_RDWR, @sfinfo);
  Format := sfinfo.Format;
  Channels := sfinfo.Channels;
  Frames := sfinfo.Frames;
  SampleRate := sfinfo.SampleRate;
  Result := Handle <> NIL;
end;

function TAudioFileReadWrite.Close: boolean;
begin
  Result := False;
  if Handle <> NIL then begin
    sf_write_sync(Handle);
    Result := sf_close(Handle) = 0;
  end;
  Handle := NIL;
end;

function TAudioFileReadWrite.ReadSeek(aTimePos: double): boolean;
begin
  Result := False;
  if Handle <> NIL then
    Result := sf_seek(Handle, Round(SampleRate*aTimePos), SF_SEEK_SET or SFM_READ) <> -1;
end;

function TAudioFileReadWrite.ReadSeek(aFrameIndex: int64): boolean;
begin
  Result := False;
  if Handle <> NIL then
    Result := sf_seek(Handle, aFrameIndex, SF_SEEK_SET or SFM_READ) <> -1;
end;

function TAudioFileReadWrite.Read(const aBuf: TALSPlaybackBuffer;
  aCount: longword): longword;
begin
  if aBuf.ChannelCount <> Channels then
    ExceptionChannelsCount;

  if aBuf.OutOfMemory then begin
    Result := 0;
    exit;
  end;

  if aBuf.UseFloat then
    Result := sf_readf_float(Handle, pcfloat(aBuf.Data), sf_count_t(aCount))
  else
    Result := sf_readf_short(Handle, pcshort(aBuf.Data), sf_count_t(aCount));
  aBuf.FrameCount := longword(Result);
end;

function TAudioFileReadWrite.WriteSeek(aTimePos: double): boolean;
begin
  Result := False;
  if Handle <> NIL then
    Result := sf_seek(Handle, Round(SampleRate*aTimePos), SF_SEEK_SET or SFM_WRITE) <> -1;
end;

function TAudioFileReadWrite.WriteSeek(aFrameIndex: int64): boolean;
begin
  Result := False;
  if Handle <> NIL then
    Result := sf_seek(Handle, aFrameIndex, SF_SEEK_SET or SFM_WRITE) <> -1;
end;

function TAudioFileReadWrite.Write(const aBuf: TALSPlaybackBuffer): longword;
begin
  if aBuf.ChannelCount <> Channels then
    ExceptionChannelsCount;

  if (Handle = NIL) or (aBuf.FrameCount = 0) or aBuf.OutOfMemory then
    Result := 0
  else begin
    if aBuf.UseFloat then
      Result := sf_writef_float(Handle, pcfloat(aBuf.Data), aBuf.FrameCount)
    else
      Result := sf_writef_short(Handle, pcshort(aBuf.Data), aBuf.FrameCount);
  end;
end;

{ TAudioFileWriter }

procedure TAudioFileWriter.InitCopyBuffer(aFramesCapacity: longword);
begin
  if SampleAreFloat
    then FCopyBuffer.Init(Channels, ALS_SAMPLE_FLOAT32)
    else FCopyBuffer.Init(Channels, ALS_SAMPLE_INT16);
  FCopyBuffer.FrameCapacity := aFramesCapacity;

  if FCopyBuffer.OutOfMemory then begin
    Log.Error('gyv: TAudioFileWriter.InitCopyBuffer out of memory when setting FrameCapacity to '+
              aFramesCapacity.ToString);
    Log.Error('on file "'+FileName+'"', 1);
    Log.AddEmptyLine;
  end;
end;

function TAudioFileWriter.OpenWrite(const aFileName: string;
  aFormat: TALSFileFormat; aChannels, aSampleRate: integer;
  aPostLogMessage: boolean): boolean;
var sfinfo: TSF_INFO;
begin
  Format := aFormat;
  Channels := aChannels;
  SampleRate := aSampleRate;

  sfinfo.Format := cint(aFormat);
  sfinfo.Channels := cint(aChannels);
  sfinfo.SampleRate := cint(aSampleRate);
  Handle := ALSOpenAudioFile(aFileName, SFM_WRITE, @sfinfo);
  Result := Handle <> NIL;

  FileName := aFileName;

  if not Result and aPostLogMessage then begin
    Log.Error('gyv: TAudioFileWriter.OpenWrite fail');
    Log.Error('on file "'+FileName+'"', 1);
  end;
end;

function TAudioFileWriter.SampleAreFloat: boolean;
begin
  Result := (Format AND SF_FORMAT_SUBMASK) = SF_FORMAT_FLOAT;
end;

function TAudioFileWriter.Close: boolean;
begin
  Result := False;
  if Handle <> NIL then begin
    sf_write_sync(Handle);
    Result := sf_close(Handle) = 0;
  end;
  Handle := NIL;

  if not Result then
    LogLibSndFileErrorMessage('TAudioFileWriter.Close fail');
end;

function TAudioFileWriter.WriteShort(p: Pointer; aCount: longword): longword;
begin
  if Handle <> NIL then begin
    Result := sf_writef_short(Handle, pcshort(p), aCount);

    if Result <> aCount then
      LogLibSndFileErrorMessage('TAudioFileWriter.WriteShort only write '+
                                Result.ToString+'/'+aCount.ToString+' frames');
  end else Result := 0;
end;

function TAudioFileWriter.WriteFloat(p: Pointer; aCount: longword): longword;
begin
  if Handle <> NIL then begin
    Result := sf_writef_float(Handle, pcfloat(p), aCount);

    if Result <> aCount then
      LogLibSndFileErrorMessage('TAudioFileWriter.WriteFloat only write '+
                                Result.ToString+'/'+aCount.ToString+' frames');
  end else Result := 0;
end;

function TAudioFileWriter.WriteDouble(p: PDouble; aCount: longword): longword;
begin
  if Handle <> NIL then begin
    Result := sf_writef_double(Handle, pcdouble(p), aCount);

    if Result <> aCount then
      LogLibSndFileErrorMessage('TAudioFileWriter.WriteDouble only write '+
                                Result.ToString+'/'+aCount.ToString+' frames');
  end else Result := 0;
end;

function TAudioFileWriter.Write(const aBuf: TALSPlaybackBuffer): longword;
begin
  Result := 0;

  if aBuf.ChannelCount <> Channels then
    ExceptionChannelsCount;

  if (Handle = NIL) or (aBuf.FrameCount = 0) or (aBuf.OutOfMemory) then
    Result := 0
  else begin
    if aBuf.UseFloat then
      Result := sf_writef_float(Handle, pcfloat(aBuf.Data), aBuf.FrameCount)
    else
      Result := sf_writef_short(Handle, pcshort(aBuf.Data), aBuf.FrameCount);

    if Result <> aBuf.FrameCount then
      LogLibSndFileErrorMessage('TAudioFileWriter.Write only write '+
                                Result.ToString+'/'+aBuf.FrameCount.ToString+' frames');
  end;
end;

function TAudioFileWriter.CopyAll(const aSrcReader: TAudioFileReader): boolean;
var frameToRead: longword;
begin
  frameToRead := aSrcReader.Frames;
  if frameToRead = 0 then begin
    Result := True;
    exit;
  end;

  Result := aSrcReader.MoveToFrame(0);
  if not Result then begin
    Log.Error('gyv: TAudioFileWriter.CopyAll failed >> aSrcReader.MoveToFrame(0) failed');
    exit;
  end;

  InitCopyBuffer(Min(32768, frameToRead));
  if FCopyBuffer.OutOfMemory then begin
    Log.Error('gyv: TAudioFileWriter.CopyAll failed >> FCopyBuffer state is out of memory');
    Result := False;
    exit;
  end;

  while (frameToRead > 0) and Result do begin
    aSrcReader.Read(FCopyBuffer, Min(FCopyBuffer.FrameCapacity, frameToRead));
    Result := Result and (Write(FCopyBuffer) = FCopyBuffer.FrameCount);
    frameToRead := frameToRead - FCopyBuffer.FrameCount;
  end;
  Result := Result and (frameToRead = 0);

  FCopyBuffer.FreeMemory;
end;

function TAudioFileWriter.CopyPart(const aSrcReader: TAudioFileReader;
  aFromFrameIndex, aToFrameIndex: int64): boolean;
var frameToRead: integer;
begin
  if aSrcReader.Frames = 0 then begin
    Result := True;
    exit;
  end;

  Result := False;
  if (aFromFrameIndex < 0) or
     (aToFrameIndex < 0) or
     (aToFrameIndex < aFromFrameIndex) then exit;

  if (aFromFrameIndex <= 0) and (aToFrameIndex >= aSrcReader.Frames) then begin
    Result := CopyAll(aSrcReader);
    exit;
  end;

  Result := aSrcReader.MoveToFrame(aFromFrameIndex);
  if not Result then begin
    Log.Error('gyv: TAudioFileWriter.CopyPart failed >> aSrcReader.MoveToFrame'+
              aFromFrameIndex.ToString+') failed');
    exit;
  end;

  frameToRead := aToFrameIndex-aFromFrameIndex+1;

  if frameToRead > 0 then begin
    InitCopyBuffer(Min(32768, frameToRead));
    if FCopyBuffer.OutOfMemory then begin
      Log.Error('gyv: TAudioFileWriter.CopyPart failed >> FCopyBuffer state is out of memory');
      Result := False;
      exit;
    end;

    FCopyBuffer.FrameCount := 1;
    while (frameToRead > 0) and (FCopyBuffer.FrameCount > 0) and Result do begin
      aSrcReader.Read(FCopyBuffer, Min(FCopyBuffer.FrameCapacity, frameToRead));
      Result := Result and (Write(FCopyBuffer) = FCopyBuffer.FrameCount);
      frameToRead := frameToRead - FCopyBuffer.FrameCount;
    end;
    Result := Result and (frameToRead = 0);

    FCopyBuffer.FreeMemory;
  end;
end;

procedure TAudioFileWriter.CopyMetaDataFrom(const aSrcReader: TAudioFileReader);
  procedure DoCopyStr(aStrType: cint);
  var s: PChar;
  begin
    s := sf_get_string(aSrcReader.Handle, aStrType);
    if s <> NIL then sf_set_string(Handle, aStrType, s);
  end;
begin
  DoCopyStr(SF_STR_TITLE);
  DoCopyStr(SF_STR_COPYRIGHT);
  DoCopyStr(SF_STR_SOFTWARE);
  DoCopyStr(SF_STR_ARTIST);
  DoCopyStr(SF_STR_COMMENT);
  DoCopyStr(SF_STR_DATE);
  DoCopyStr(SF_STR_ALBUM);
  DoCopyStr(SF_STR_LICENSE);
  DoCopyStr(SF_STR_TRACKNUMBER);
  DoCopyStr(SF_STR_GENRE);
end;

procedure TAudioFileWriter.WriteMetaData(const aTitle, aCopyright, aSoftware,
  aArtist, aComment, aDate, aAlbum, aLicense, aTrackNumber, aGenre: string);
  procedure DoWriteStr(aStrType: cint; const s: string);
  begin
    if s <> '' then sf_set_string(Handle, aStrType, PChar(s));
  end;
begin
  DoWriteStr(SF_STR_TITLE, aTitle);
  DoWriteStr(SF_STR_COPYRIGHT, aCopyright);
  DoWriteStr(SF_STR_SOFTWARE, aSoftware);
  DoWriteStr(SF_STR_ARTIST, aArtist);
  DoWriteStr(SF_STR_COMMENT, aComment);
  DoWriteStr(SF_STR_DATE, aDate);
  DoWriteStr(SF_STR_ALBUM, aAlbum);
  DoWriteStr(SF_STR_LICENSE, aLicense);
  DoWriteStr(SF_STR_TRACKNUMBER, aTrackNumber);
  DoWriteStr(SF_STR_GENRE, aGenre);
end;

{ TAudioFileReader }

function TAudioFileReader.OpenRead(const aFileName: string; aPostLogMessage: boolean): boolean;
var sfinfo: TSF_INFO;
begin
  sfinfo.Format := 0;
  Handle := ALSOpenAudioFile(aFileName, SFM_READ, @sfinfo);

  Result := Handle <> NIL;
  if Result then begin
    Format := sfinfo.Format;
    Channels := sfinfo.Channels;
    Frames := sfinfo.Frames;
    SampleRate := sfinfo.SampleRate;
  end;

  FileName := aFileName;

  if not Result and aPostLogMessage then begin
    Log.Error('gyv: TAudioFileReader.OpenRead fail');
    Log.Error('on file "'+FileName+'"', 1);
  end;
end;

function TAudioFileReader.Close: boolean;
begin
  Result := True;
  if Handle <> NIL then
    Result := sf_close(Handle) = 0;
  Handle := NIL;

  if not Result then
    LogLibSndFileErrorMessage('TAudioFileReader.Close fail');
end;

function TAudioFileReader.MoveToFrame(aFrameIndex: int64): boolean;
begin
  Result := sf_seek(Handle, aFrameIndex, SF_SEEK_SET) <> -1;

  if not Result then
    LogLibSndFileErrorMessage('TAudioFileReader.MoveToFrame fail to move to frame '+
              aFrameIndex.ToString+'/'+Frames.ToString);
end;

function TAudioFileReader.TotalDuration: double;
begin
  if SampleRate <> 0 then
    Result := Frames/SampleRate
  else begin
    Result := 0;
    LogLibSndFileErrorMessage('TAudioFileReader.TotalDuration fail because SampleRate=0');
  end;
end;

function TAudioFileReader.ReadShort(p: Pointer; aCount: longword): longword;
begin
  Result := sf_readf_short(Handle, pcshort(p), sf_count_t(aCount));
end;

function TAudioFileReader.ReadFloat(p: Pointer; aCount: longword): longword;
begin
  Result := sf_readf_float(Handle, pcfloat(p), sf_count_t(aCount));
end;

function TAudioFileReader.ReadDouble(p: PDouble; aCount: longword): longword;
begin
  Result := sf_readf_double(Handle, pcdouble(p), sf_count_t(aCount));
end;

function TAudioFileReader.Read(const aBuf: TALSPlaybackBuffer; aCount: longword): longword;
begin
  if aBuf.ChannelCount <> Channels then
    ExceptionChannelsCount;

  if aBuf.OutOfMemory then begin
    Result := 0;
    exit;
  end;

  if aBuf.UseFloat then
    Result := sf_readf_float(Handle, pcfloat(aBuf.Data), sf_count_t(aCount))
  else
    Result := sf_readf_short(Handle, pcshort(aBuf.Data), sf_count_t(aCount));
  aBuf.FrameCount := Result;
end;

Initialization
  Playback.FPlaybackContext := NIL;
  Capture.FCaptureContext := NIL;
end.

