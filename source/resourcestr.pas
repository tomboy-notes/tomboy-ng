unit ResourceStr;

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

  An attempt to move all resource strings into one unit to facilate
  reuse where possible.  Note that while arranged in blocks labeled
  with the unit that uses them, no reason to limit use to that. }

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

RESOURCESTRING

  { to use replacable parameters, pass an array of parameters to format()

    rsString := 'you have %d pimples';
    label1.caption := format(rsString, [Count])
  }


// notebook.pas

  rsMultipleNoteBooks = 'Settings allow multiple Notebooks';
  rsOneNoteBook = 'Settings allow only one Notebook';
  rsSetTheNotebooks = 'Set the notebooks this note is a member of';
  rsChangeNameofNotebook = 'Change the name of this Notebook';
  rsNumbNotesAffected = 'This will affect %d notes';                 // %d replaced by integer, 0 to big number
  rsEnterNewNotebook = 'Enter a new notebook name please';
  rsNotebookOptionRight = 'Right click for Notebook Options';       // Windows, Linux
  rsNotebookOptionCtrl = 'Ctrl click for Notebook Options';           // Mac
  rsAddNotesToNotebook = 'Add notes to this Notebook';

// SearchForm
  // these are main menu items and string grid headings -
  rsMenuNewNote = 'New Note';
  rsMenuSearch = 'Search';
  rsMenuAbout = 'About';
  rsMenuSync = 'Synchronise';
  rsMenuSettings = 'Settings';
  rsMenuHelp = 'Help';
  rsMenuQuit = 'Quit';
  rsNotebooks = 'Notebooks';
  rsName = 'Name';
  rsLastChange = 'Last Change';

  rsSetupNotesDirFirst = 'Please setup a notes directory first';
  rsSetupSyncFirst = 'Please config sync system first';
  rsCannotFindNote = 'ERROR, cannot find ';                    // is followed by a filename
  rsSearchHint = 'Exact matches for terms between " "';


// SyncGUI
  rsTestingSync = 'Testing Sync';
  rsUnableToSync = 'Unable to sync because ';
  //rsUnableToSyncAuto = 'tomboy-ng is unable to do Auto Sync at the moment.'
  // mention tomboy-ng 'cos user may not be activly using tb when this pops up
  rsRunningSync = 'Running Sync';
  rsAllDone = 'All Done';
  rsPressClose = 'Press Close';
  rsTestingRepo = 'Testing Repo ....';
  rsCreateNewRepo = 'Create a new Repo ?';
  rsUnableToProceed = 'Unable to proceed because';
  rsLookingatNotes = 'Looking at notes ....';
  rsSaveAndSync = 'Press Save and Sync if this looks OK';
  rsSyncError = 'A Sync Error occurred';
  rsLastSync = 'Last Sync';     // Followed by a date and simplified sync report
  rsFileSyncInfo1 = 'tomboy-ng uses File Sync to sync to eg DropBox, Google Drive, a USB drive';
  rsFileSyncInfo2 = 'or uses a remote server over the internet with sshfs';
  rsGithubSyncInfo1 = 'tomboy-ng can use Github to both sync and display or edit notes';
  rsGithubSyncInfo2 = 'you should read the tomboy-ng wiki page for instructions.';


// Settings but only part ...

  //rsChangeNetSync = 'Change Net Sync Repo';          // These are labels on the button used to set sync repo
  rsChangeSync = 'Change Sync Repo';
  rsSyncNotConfig = 'not configured';               // means that the file of net sync is not configured yet.
  rsSetUp = 'Setup';                                // means configure something, eg, one of the Sync modules.
  rsAutosnapshotRun='Completed autosnapshot run.';  // Message on status bar after an AutoSnapshot run.
  rsSnapshotCreated = 'created, do you want to copy it elsewhere ?';      // refers to a just taken snapshot
  rsErrorCopyFile = 'Failed to copy file, does destination dir exist ?';
  rsAutoSyncNotPossible = 'Auto sync not possible right now';               // Auto sync is configured but cannot proceed, probably because drive is not available

  // BackUpView

  rsNewerVersionExits = 'A newer version exists in main repo';
  rsNotPresent = 'Not present in main repo';
  rsCannotDelete = 'Cannot delete ';

  rsOverwriteNote = 'Overwrite newer version of that note';
  rsNoteAlreadyInRepo = 'Note already in Repo';
  rsNoteOpen = 'You have that note open, please close and try again';
  rsCopyFailed = 'Copying orig to Backup directory failed';
  rsRenameFailed = 'ERROR, could not rename Backup File ';
  rsRecoverOK = 'OK, File recovered.';
  rsNotesDeleted =  'Note or notes deleted';


  // CLI -- these are command line help lines that appear when user adds --help
  {$ifdef DARWIN}
    rsMacHelp1 = 'eg   open tomboy-ng.app';
    rsMacHelp2 = 'eg   open tomboy-ng.app --args -o Note.txt|.note';
    {$endif}
    rsHelpDelay = 'Delay startup 2 sec to allow OS to settle';
    rsHelpLang = 'Force Language, en, es, uk, fr, nl';
    rsHelpDebug = 'Direct debug output to SOME.LOG.';
    rsHelpHelp = 'Show this help message and exit.';
    rsHelpVersion = 'Print version and exit';
    //rsHelpRedHat = 'Deprecated';                                // No longer important,
    rsHelpNoSplash = 'Do not show small status/splash window';
    rsHelpDebugSync = 'Show debug messages during Sync';
    rsHelpDebugIndex = 'Show debug msgs while indexing notes';
    rsHelpDebugSpell = 'Show debug messages while spell setup';
    rsHelpConfig = 'Create or use an alternative config';
    rsHelpSingleNote = 'Open indicated note, switch is optional';
    rsHelpImportFile = 'Import file into Note Repo';
    rsHelpSaveExit = 'After import single note, save & exit';
    rsHelpTitleIsFName = 'Use Filename as title for import txt & md';

    // Mainunit

  rsBadNotesFound1 = 'Please go to Settings -> Recover -> Recover Notes';
  rsBadNotesFound2 = 'You should do so to ensure your notes are safe.';
  rsFound = 'Found';
  rsNotes = 'notes';
  rsWARNNOSSYSTRAY = 'WARNING, your Desktop might not display SysTray';

    // R E C O V E R unit

  rsClickSnapShot = 'Click an Available Snapshot';
  rsWeHaveSnapShots = 'We have %d snapshots';
  rsDeletedDamaged = 'OK, deleted %d damaged notes';
  rsBadNotes = 'You have %d bad notes in Notes Directory';
  rsClickBadNote = 'Double click on any Bad Notes';
//  rsNoBadNotes = 'No errors, perhaps you should proceed to Snapshots';
  rsTryRecover_1 = 'Try to recover a bad note by double clicking below,';
  rsTryrecover_2 = 'if that fails, you may be able to recover it from a Snapshot.';
  rsDeleteAndReplace_1 = 'Notes at risk !';
  rsAllRestored = 'Notes and config files Restored, restart suggested.';
  rsDeleteAndReplace_2 = 'Delete all notes in %s and replace with snapshot dated %s ?';
  rsNotesInSnap = 'Notes in Snapshot';                                          // followed by the name of a snapshot

  // RollBack

  rsContentDated = 'Content Dated';
  rsNotAvailable = 'Not Available';
  rsRollBackIntro = 'You can roll back to previous version of this note';

  // EditBox  - lots more to do ..
  rsFindNavRightHint = 'Find : F3 or Ctrl-G';
  rsFindNavLeftHint = 'Backward Find : Shift-F3 or Shift-Ctrl-G';
  rsFindNavRightHintMac = 'Find : Command-G';
  rsFindNavLeftHintMac = 'Backward Find : Shift-Command-G';

  // github sync - I would like to use some of these in other syncs too.

  rsGithubTokenExpired = 'Github Token may have expired';
  rsTestingCredentials = 'Testing Credentials';
  rsLookingServerID = 'Looking for ServerID';
  rsScanRemote  = 'Scanning remote files';
  rsDownloadNotes = 'Downloading notes';
  rsDownLoaded = 'Downloaded';          // followed by a number
  rsUpLoading = 'Uploading';            // followed by a number
  rsUpLoaded = 'Uploaded';              // followed by a number
  rsMetaDirWarning = 'Please remember that to ensure a reliable sync, you must not change files in the Meta directory.';

implementation

end.

