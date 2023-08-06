unit u_common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ALSound;

const
  APP_NAME = 'Give Your Voice';
  APP_VERSION = '1.0.4';

  // GitHub
  URL_FOR_VERSION_ON_GITHUB = 'https://raw.githubusercontent.com/Lulu04/Give-Your-Voice/main/version';
  URL_FOR_LATEST_RELEASE_ON_GITHUB = 'https://github.com/Lulu04/Give-Your-Voice/releases/latest';

  PROGRAM_OPTIONS_FILENAME = 'GiveYourVoice.cfg';
  PROGRAM_LOG_FILENAME = 'GiveYourVoice.log';

  PROJECT_FILE_EXT = '.gyv';
  PROJECT_RECORDING_FILE_EXT = '.wav';
  PROJECT_MIXED_FILE_EXT = '.mp3';
  PROJECT_ZIP_FILE_EXT = '.zip';

  // On Windows, the default project folder is UserDocuments\GiveYourVoice\
  // On Linux, default project folder is in User/GiveYourVoice or User/Documents/GiveYourVoice
  // On Mac ???? to do
  FOLDER_FOR_PROJECT = 'GiveYourVoice';
  FOLDER_FOR_TEMP = 'temp';
  PROJECT_OUTPUT_FOLDER_MP3 = 'MP3';
  PROJECT_OUTPUT_FOLDER_ZIP = 'ZIP';


  RECORDING_TEMP_FILENAME = 'record';
  RECORDING_TEMP_FILEEXTENSION = '.wav';
  RECORDING_TEMP_NOISE_FILENAME = 'NoiseCapture.wav';


  UNDO_REDO_BASE_FILENAME = 'undo_redo';

  MIX_SESSION_FILENAME = 'MixSession.cfg';
  USER_MARK_FILE_EXT = '.usermarks';


//  FILENAMEFORBIDENCHARS='/|\;,.:*?!%$#&"''';
  FILENAMEFORBIDENCHARS: TStringArray=('/', '|', '\', ';', ',', '.', ':', '*',
                                       '?', '!', '%', '$', '#', '&', '"', '<',
                                       '>');


  USER_GUIDE_FILE_BASE = 'GYV_UserGuide_';
  USER_GUIDE_FILE_EXT = '.pdf';

  NUMBER_OF_DAYS_TO_LOOK_FOR_UPDATE = 7; // every weeks
  NUMBER_OF_DAYS_TO_REMEMBER_USER_TO_DONATE = 30*4; // every 4 months


 // specific to platform litteratureaudio.com
  LITTERATUREAUDIO_TEXT_FILENAME_FOR_ZIP_ARCHIVE = 'www.litteratureaudio.com.txt';

  LITTERATUREAUDIO_URL_ESSAI = 'https://www.litteratureaudio.com/forum/don-de-voix-vos-essais';
  LITTERATUREAUDIO_URL_GROSFICHIERS = 'https://www.grosfichiers.com/';

 // specific to LibriVox
 LIBRIVOX_URL_UPLOADER = 'https://librivox.org/login/uploader';

type

  TMouseButtonPressed = (mbpNone,
                         mbpLeft,
                         mbpRight,
                         mbpMiddle);

var

  FRecordFilenameSuffix: integer = 0;

  FFirstTimeProgramIsRunning: boolean = FALSE;
  FormProjectManager_FAskUserToShowUserGuide: boolean = FALSE;

{$ifdef LINUX}
  FDesignFontHeight: integer;
  FDesignSmallFontHeight: integer;
{$endif}


type
  TSingleArray = array of single;
  TIndexArray = array of SizeInt;

  TUserMarks = TSingleArray;

  TProjectTargetPlatform = ( ptpNone=0,
                             ptpLitteratureAudio,
                             ptpLibriVox);

{// DISCLAMER FOR LIBRIVOX
const
  LIBRIVOX_DISCLAMER = '<Titre> de <Auteur>. Ceci est un enregistrement LibriVox. '+
                       'Tous nos enregistrement appartiennent au domaine public. '+
                       'Pour vous renseigner à notre sujet ou pour participer, '+
                       'rendez-vous sur LibriVox.org. '+
                       'Enregistré par <votre nom><votre site internet>. '+
                       'Chapitre xxx <titre du chapitre>';

  LIBRIVOX_DISCLAMER_POEM_BEGIN = '<Titre> de <Auteur>. Lu pour LibriVox par <votre nom>.';
  LIBRIVOX_DISCLAMER_POEM_END = 'Fin du poème. Cet enregistrement est dans le domaine public.'; }

implementation

end.

