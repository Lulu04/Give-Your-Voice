unit u_resource_string;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring

SPleaseNoSpecialCharacters = 'svp pas de caractères spéciaux';
SFailedToOpenCaptureDevice='Erreur d''ouverture du périphérique d''enregistrement audio';
SFailedToOpenPlaybackDevice='Erreur d''ouverture du périphérique de lecture audio';

SAskForOpenURLForNewAPPVersion='Give Your Voice version %0:s est maintenant disponible !'+LineEnding+
                               'votre version actuelle est %1:s'+LineEnding+LineEnding+
                               'Voulez-vous fermer le programme et vous rendre sur le site internet pour la télécharger ?';
SAppIsUpToDate='Vous avez déjà la dernière version';
SErrorAccessingInternet='Impossible d''ouvrir une connexion internet';
SDoYouWantToShowUserGuide='Voulez-vous consulter le manuel utilisateur ?';
SVersion='version';
SCredits='Crédits';
SIconAppBy='Icone du programme par %0:s'+LineEnding+
           'Image du micro de %1:s sur %2:s';
SSomeIcons='Quelques icones';

SOk='Ok';
SYes='Oui';
SNo='Non';
SQuit='Quitter';
SStay='Rester';
SClose='Fermer';
SContinue='Continuer';
SCancel='Annuler';
SSecond='seconde';
SDelete='Supprimer';
SNext='Suivant';
SMicLevel='Niveau du micro';

//SProjectManager='GESTIONNAIRE'+LineEnding+'DE PROJETS';
SAskToEraseMP3FilesBeforeZip='Avant de compresser ce projet, voulez-vous effacer les fichiers de son dossier MP3 ?'+LineEnding+
      'Cela réduira la taille du zip.';
SAskToEraseZIPFolderBeforeZip='Avant de compresser ce projet, voulez-vous supprimer son dossier ZIP ?';
SZipProjectFailed='une erreur est apparue lors de la compression du projet...';
SUnzipProjectFailed='une erreur est apparue lors de la décompression du fichier zip...';
SWarningBeforeDeletingProject='Attention, supprimer un projet du disque est définitif.'+LineEnding+
                              'Pour confirmer la suppression, tapez le mot ''supprimer''';
SFailedToDeleteProject='Erreur lors de la suppression du projet';

STest='TESTER';
SStop='STOP';
SPause='PAUSE';

SNoTargetPlatform='aucune plateforme';
SSettingFor='réglages pour';
SNone='aucun';
SAll='tous';
SChapter='Chap';
SText='Texte';
SPoem='Poème';
SPart='Partie';
SSection='Section';
SPage='Page';

SOpenInBrowser='Ouvrir dans votre navigateur internet';
SFailToOpenURL='Échec de l''ouverture du site dans votre navigateur';
SRoyaltyFreeMusics='Musiques libre de droit';

SAudioBook='Livre audio';

SClickToNavigate='Cliquez pour naviguer dans l''audio';

SMouseRightClickContextualMenu='CLIC DROIT=menu contextuel';
SCtrlLeftClickForMove='CTRL+CLIC GAUCHE=glisser pour déplacer l''audio';
SMouseWheelForZoom='MOLETTE souris=zoomer';
SSpaceForListenStop='ESPACE=écouter/stopper';
SLeftClickAndMoveToTrimAudio='CLIC GAUCHE=glisser pour rogner l''audio';
SLeftClickToSelect='CLIC GAUCHE=sélectionner l''audio';
SLeftClickToDragOrDeletePoint='CLIC GAUCHE=glisser pour déplacer le point. Déplacer loin vers le haut ou le bas pour le supprimer';
SLeftClickToAddPoint='CLIC GAUCHE=ajouter un point sur la courbe de volume';
SRightClickToAddMusic='Click droit pour ajouter une musique';
SRightClickToAddSound='Click droit pour ajouter un bruitage';
SDeleteThisMusic='Supprimer cette musique ?';
SMusics='musiques';
SSounds='bruitages';
SDeleteThisSound='Supprimer ce bruitage ?';
SErrorWhileOpeningAudioFile='Erreur lors de l''ouverture du fichier audio';
SErrorWhileOpeningLoopbackDevice='Le programme n''arrive pas à ouvrir un périphérique de mixage(loopback)...';
SCompressorDontWork='L''effet "compresseur" ne fonctionne pas';
SBassBoostDontWork='l''effet "Bass Boost" ne fonctionne pas';
SChainEffectDontWorkWell='Erreur lors de la création de la chaîne d''effets,'+lineending+
                         'les effets ne fonctionneront pas bien...';
SErrorWhilePreparingOutputMixFile='Erreur lors de la préparation du fichier de sorti pour le mixage';
SFailedToSetCompressionLevel='Échec du réglage du débit du MP3...';
SFailedToSetMP3BitrateToConstant='Échec du réglage du bitrate du MP3 sur ''constant''';
SFailedToWriteMetaDataToMP3File='Échec de l''écriture des métadonnées dans le fichier MP3';
SMixingError='Erreur lors du mixage';
SOutputFileName='Fichier de sortie:';
SToggleMute='Inverser muet';

BCompressorButtonHint='Active/désactive le compresseur sur les voix'+LineEnding+
                      'C''est un effet qui réhausse le niveau sonore des passages moins audibles'+LineEnding+
                      'comme les fins de phrases où la voix a tendance à baisser. Recommandé !';
BBassBoostButtonHint='Active/désactive l''amplification des graves sur les voix.'+LineEnding+
                     'Amplifier les graves adouci et arrondi la voix, la rend plus ''chaude''.'+LineEnding+
                     'Cet effet n''est pas très efficace sur des voix féminines.';
BAmplifyVoiceButtonHint='Active/désactive l''amplification des voix.'+LineEnding+
                        'Activez la pour rendre votre voix plus audible';

SGainAnalysisError='Une erreur est survenue lors de l''analyse du fichier.'+LineEnding+
                   'Il ne sera pas normalisé à %0:sdB';
SGeneratingTheMP3='Génération du MP3'+LineEnding+'Patientez svp';

SAskUserToMixFileWithUserMarks='Un ou plusieurs enregistrement(s) de cette section comporte(nt) des marqueurs indiquant que vous avez encore des corrections à y faire.'+
                                LineEnding+'Voulez-vous quand même la mixer ?';
SNameOfTheSectionToInsert='Nom de la section à insérer';
SNameOfTheSectionToAdd='Nom de la section à ajouter';
SNumber='numéro';
SPrefix='préfixe';
STitle='titre';

SFailToDeleteThisFolder='Le programme n''arrive pas à supprimer cette section';
SAskConfirmDeleteFile='Supprimer ce fichier ?';
SFailToDeleteFile='Le programme n''arrive pas à supprimer ce fichier';
SAskConfirmDeleteFolder='Supprimer %0:s ?%1:s';
SNewName='Nouveau nom:';
SFailedToRename='Échec lors de la modification du nom';
SFolderWithSameNameAlreadyExists='Une section portant ce nom existe déjà !';
SInvalidName='Nom non valide';
SProjectContent='Contenu du projet';

SRecordingFileDontRespectNamesRules='Impossible d''exécuter cette action car cet enregistrement '+
                                    'ne respecte pas les règles de nommage du logiciel:'+LineEnding+
                                    '<nombre à 3 chiffres> + <espace> + <titre optionnel>';
SAudioFileImport='Importation de fichier audio';
SFailedToImportAudioFile='Le programme n''a pas pu importer le fichier';
SAudioFile='fichier audio';
SAllFile='tous';


SEnterDurationOfSilenceToInsert='Entrez la durée du silence à insérer:';

SCutFailed='Impossible de couper la sélection...';
SSilenceFailed='La commande ''Silence'' a échoué...';
SInsertSilenceFailed='La commande ''Insérer silence'' a échoué...';
SFailedToStartAudioEngine='Le programme n''arrive pas à initialiser le moteur audio';
SFailToCopyAudio='Le programme n''arrive pas à copier l''audio'+LineEnding+
                 'Vous ne pourrez donc pas annuler l''action...'+LineEnding+
                 'Voulez-vous quand même continuer ?';
SSilenceOnSelection='Silence sur la sélection';
SInsertSilence='Insérer silence';
SCutTheSelection='Couper la sélection';
SReplaceSelectionByRecord='Remplacer la sélection par l''enregistrement';
SReplacementFailed='Le remplacement a échoué...';
SInsertRecordToTheBeginning='Insérer un enregistrement au début';
STheInsertionAtTheBeginningFailed='L''insertion au début a échoué...';
SInsertRecordToTheEnd='Insérer un enregistrement à la fin';
STheInsertionAtTheEndFailed='L''insertion à la fin a échoué...';
SInsertRecordAtCursor='Insérer un enregistrement au curseur';
STheInsertionAtCursorFailed='L''insertion au curseur a échoué...';
SDeleteMarkerOnSelection='Supprimer les marqueurs sur la sélection';
SDeleteAllMarkers='Supprimer tous les marqueurs';
SAddMarkerAtCursor='Ajouter un marqueur au curseur';
SAddNewRecordIntoProjectFailed='L''ajout du nouvel enregistrement au projet a échoué !';
SInsertionOfNewRecordIntoProjectFailed='L''insertion du nouvel enregistrement dans le projet a échoué !';

SUndoCommandFailed='La commande <Annuler %0:s> a échoué...';
SRedoCommandFailed='La commande <Refaire %0:s> a échoué...';
SUndoCaption='Annuler %0:s (Ctrl Z)';
SRedoCaption='Refaire %0:s (Ctrl Y)';

SSelectionWillBeReplacedByNewRecord='la sélection sera remplacée par le nouvel enregistrement';
SInsertNewRecordToTheBeginning='le nouvel enregistrement sera inséré au début';
SNewRecordWillBeAddedToTheEnd='le nouvel enregistrement sera ajouté à la fin';
SNewRecordWillBeInsertedAtCursor='le nouvel enregistrement sera inséré au curseur';

SInsertsSilenceAtCursor='Insère %0:ss de silence au curseur'+LineEnding+
                        '(touche I)'+LineEnding+
                        'Clic droit pour modifier cette durée';

SNewRecordFor='Nouvel enregistrement pour %0:s';
SAdditionTo='Rajout à %0:s';
SStartRecording='Commencer'+LineEnding+'l''enregistrement';
SCancelThisRecord='Annuler cet enregistrement ?';
SHardwareErrorWhileCapturing='Nous sommes désolé, une erreur relative au périphérique audio'+LineEnding+
    'est survenue lors de l''enregistrement...';
SFailedToRecord='Échec de l''enregistrement';
SNoiseRemovalInProgress='Suppression du bruit de fond...';
SPleaseWait='Veuillez patienter svp';
SNoiseRemovalFailed='Échec lors de la suppression du bruit de fond';
SFailedToRecordNoise='Erreur lors de l''enregistrement du bruit de fond';
SErrorWhenPreparingRecordingFileForNoise='Erreur lors de la préparation du fichier pour l''analyse du bruit de fond';
SErrorWhenPreparingRecordingFile='Erreur lors de la préparation du fichier d''enregistrement';
SMarkTouchUpToBeDone='Marquer une retouche à faire'+LineEnding+'(touche Ctrl)';

SProject='Projet';
SProjectIsNotForThisApplication='Le fichier que vous essayez de charger n''est pas un projet Give Your Voice';
SProjectWithSameNameExist='Un projet avec le même nom existe déjà !';
SFailedToCreateProjectFolder='Impossible de créer le répertoire du projet';
SFailedToSaveNewProject='Une erreur est apparue lors de la sauvegarde du nouveau projet...';
SFailedToCreateMP3ProjectFolder='Impossible de créer le répertoire pour les fichiers mixés (MP3)';
SFailedToCreateFolder='Erreur lors de la création du répertoire';


SAskBeforeClearMP3Folder='Vider le répertoire MP3 ?';
SFailedToCreateProjectZipFolder='Erreur lors de la création du répertoire ZIP';
SAskBeforeDeleteZIPFolder='Supprimer le répertoire ZIP et tout son contenu ?';

SZipCreationFailed='La création du ZIP a échoué';
SMP3ToZipIncludeFile='Inclure le fichier %0:s';

// H E L P

// help for new project
{SHelpTargetPlatform='< Choix de la plateforme de publication >'+LineEnding+LineEnding+
  'Vous pouvez choisir ici la plateforme sur laquelle vous envisagez de publier vos enregistrements.'+LineEnding+
  'Give Your Voice s''occupera de respecter les règles imposées par la plateforme sélectionnée, '+
  'comme par exemple, certains champs des métadonnées ou encore le format des fichiers MP3.'+LineEnding+
  'Si vous n''avez pas l''intention de publier vos enregistrement sur une plateforme, sélectionnez ''aucune plateforme''.';
}

{SHelpMetadata='< Métadonnées >'+LineEnding+LineEnding+
  'Les métadonnées sont des informations rajoutées à l''intérieur des fichiers audio.'+LineEnding+
  'Ces informations décrivent par exemple, l''auteur de l''enregistrement, le titre, son genre...';

// help for Project manager
SHelpProjectManager='< Gestionnaire de projets >'+LineEnding+LineEnding+
  'Dans la partie haute, se trouve la liste des répertoires de travail. Choisissez-en un, en cliquant dessus.'+LineEnding+
  '"Rechercher": cliquez dessus pour ajouter un nouveau répertoire de travail comme par exemple, un disque dur externe. '+
  'Une fenêtre va alors s''ouvrir vous permettant d''indiquer son chemin.'+LineEnding+LineEnding+
  'Dans la partie basse, le logiciel affiche la liste des projets trouvés dans le répertoire de travail sélectionné.'+LineEnding+
  '"Nouveau": démarre un nouveau projet de lecture. un assistant nouveau projet va s''ouvrir.'+LineEnding+
  '"Ouvrir": Ouvre le projet sélectionné.'+LineEnding+
  '"Zipper": Lorsqu''un projet est terminé, vous pouvez l''archiver en le compressant au format ZIP. Ceci libèrera de la place sur le disque.'+LineEnding+
  '"Dézipper": Décompresse un projet précédemment compressé.'+LineEnding+
  '"Supprimer": Pour supprimer un projet du disque. Attention, vous perdrez tout ce qu''il contient.';

// help main form
SHelpImproveListening='< Amélioration de l''écoute >'+LineEnding+LineEnding+
  'Lorsque vous écoutez vos enregistrements, il se peut que leur niveaux soit trop faible pour travailler correctement. '+
  'Si c''est le cas, activez cette option et jouez avec les réglages jusqu''à ce que le niveau audio vous convienne.'+LineEnding+
  'Cette option n''est appliquée que sur l''écoute et ne modifie en aucune façon les fichiers audio.'+LineEnding+
  'Cette option n''est pas appliquée sur les fichiers mixés du dossier MP3.';  }

// help mixer form
SHelpMixerEffect='< EFFETS APPLIQUÉS SUR LES VOIX >'+LineEnding+LineEnding+
  'Vous pouvez choisir ici quels effets seront appliqués sur les voix lors du mixage afin de les rendre plus audible.'+LineEnding+
  'Pour cela, lancez la lecture audio en appuyant sur ESPACE et jouez avec les réglages jusqu''à trouver '+
  'le niveau qui vous convienne.'+LineEnding+LineEnding+
  'Soyez vigilant(e) sur les points suivants:'+LineEnding+
  '   + il est conseillé d''activer le compresseur, sauf si votre micro est connecté à une table de mixage qui en possède déjà un'+LineEnding+
  '   + n''abusez pas de l''amplification sinon vos voix vont saturer'+LineEnding+
  '   + garder à peu près les mêmes réglages entre un chapitre et un autre afin de préserver une homogénéité dans vos fichiers finaux.';

{SHelpMixer='< MIXEUR >'+LineEnding+LineEnding+
  'Produit un fichier MP3 à partir d''un ou plusieurs enregistrements d''une même section. '+
  'Le fichier produit sera placé dans le répertoire MP3 du projet.'+LineEnding+LineEnding+
  'Les commandes sont:'+LineEnding+
  '> Sélection d''un enregistrement'+LineEnding+
  '    .clic gauche'+LineEnding+
  '> Déplacer un enregistrement sur la chronologie'+LineEnding+
  '    .sélectionez le'+LineEnding+
  '    .maintenez ctrl + clic gauche et faites glisser'+LineEnding+
  '> Rogner un enregistrement'+LineEnding+
  '    .sélectionez le'+LineEnding+
  '    .placez la souris au début ou à la fin de l''enregistrement'+LineEnding+
  '    .maintenez clic gauche et faites glisser'+LineEnding+
  '> Ajout d''une musique ou d''un bruitage'+LineEnding+
  '    .clic droit sur la piste musique ou la piste bruitage pour faire apparaître le menu contextuel'+LineEnding+
  '    .dans le menu cliquez "Ajouter une musique/son"'+LineEnding+
  '> Suppression d''une musique ou d''un bruitage'+LineEnding+
  '    .sélectionnez la musique ou le son'+LineEnding+
  '    .clic droit pour faire apparaître le menu contextuel'+LineEnding+
  '    .dans le menu cliquez "Supprimer"'+LineEnding+
  '> Réglage du niveau global d''une musique (ou d''un son)'+LineEnding+
  '    .sélectionnez la musique'+LineEnding+
  '    .clic droit dessus pour faire apparaître le menu contextuel'+LineEnding+
  '    .dans le menu cliquez "Ajuster le volume"'+LineEnding+
  '    .un curseur apparait: réglez le au niveau souhaité'+LineEnding+
  '    .bouton "Muet" pour rendre la musique silencieuse'+LineEnding+
  '> Enveloppe de volume'+LineEnding+
  '   permet  d''effectuer des fondus entrant ou sortant sur vos musiques et sons'+LineEnding+
  '   un point positionné tout en bas règle le volume à 0%'+LineEnding+
  '   un point situé tout en haut règle le volume à 100%'+LineEnding+
  '    .sélectionnez une musique ou un son'+LineEnding+
  '    .clic gauche sur un point de la courbe puis faites glisser pour le déplacer'+LineEnding+
  '    .déplacez un point loin vers le haut ou vers le bas pour le supprimer'+LineEnding+
  '    .ajoutez un point en cliquant sur la courbe';  }

implementation

end.

