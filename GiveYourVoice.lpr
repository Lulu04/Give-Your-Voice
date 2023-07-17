program GiveYourVoice;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, form_main, frame_channel_level, u_project, u_common,
  u_audio_utils, form_options, frame_viewaudio, form_new_project, u_datamodule,
  u_utils, frame_view_projectfiles, dsp_noiseremoval, form_audiorecording,
  u_program_options, frame_editstring, form_user_askconfirmation,
  form_user_inputstring, form_user_showmessage, u_userdialogs, frame_trackbar,
  form_mixer, u_main_undoredo, frame_mixer, u_mixer_undoredo, LCL_utils,
  PropertyUtils, utilitaire_fichier, u_logfile, project_util,
  form_user_asknumber, u_resource_string, form_firsttimewizard,
  frame_progressbar, u_mp3gain, form_about, u_crossplatform,
  form_project_manager, form_ask_sectionname, form_help, form_zipmp3,
  frame_zipfiles, form_mixermetadata, u_web, form_remembertodonate;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;

  CreateDefaultProjectFolder;
  Log := TLog.Create(GetAppDefaultProjectFolder+PROGRAM_LOG_FILENAME);
  Log.DeleteLogFile;
  Log.Info('gyv: starting app', 0, True);
  Log.AddEmptyLine();

  ProgramOptions.Load;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFormUserConfirmation, FormUserConfirmation);
  Application.CreateForm(TFormMixer, FormMixer);
  Application.CreateForm(TFormZipMP3, FormZipMP3);
  Application.Run;

  ProgramOptions.Save;
  Log.Info('gyv: closing app', 0, True);

end.

