unit ResourceStr;

{ Am attempt to move all resource strings into one unit to facilate
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
  rsNotebookOptionCmd = 'Cmd click for Notebook Options';           // Mac


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
  rsSyncError = 'A Sync Error occured';
  rsLastSync = 'Last Sync';     // Followed by a date and simplified sync report
  rsFileSyncInfo1 = 'tomboy-ng uses File Sync to sync to eg DropBox, Google Drive, a USB drive';
  rsFileSyncInfo2 = 'or use a remote server over the internet with sshfs';


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


  // CLI
      {$ifdef DARWIN}
    rsMacHelp1 = 'eg   open tomboy-ng.app';
    rsMacHelp2 = 'eg   open tomboy-ng.app --args -o Note.txt|.note';
    {$endif}
    rsHelpDelay = 'Delay startup 2 sec to allow OS to settle';
    rsHelpLang = 'Force Language, supported en, es, nl';
    rsHelpDebug = 'Direct debug output to SOME.LOG.';
    rsHelpHelp = 'Show this help message and exit.';
    rsHelpVersion = 'Print version and exit';
    //rsHelpRedHat = 'Deprecated';                                // No longer important,
    rsHelpNoSplash = 'Dont show small status/splash window';
    rsHelpDebugSync = 'Show debug messages during Sync';
    rsHelpDebugIndex = 'Show debug msgs while indexing notes';
    rsHelpDebugSpell = 'Show debug messages while spell setup';
    rsHelpConfig = 'Create or use an alternative config';
    rsHelpSingleNote = 'Open indicated note, switch is optional';
    rsHelpSaveExit = 'After import single note, save & exit';


    // Mainunit

  rsBadNotesFound1 = 'Please go to Settings -> Recover -> Recover Notes';
  rsBadNotesFound2 = 'You should do so to ensure your notes are safe.';
  rsFound = 'Found';
  rsNotes = 'notes';

    // R E C O V E R unit

  rsClickSnapShot = 'Click an Available Snapshot';
  rsWeHaveSnapShots = 'We have %d snapshots';
  rsDeletedDamaged = 'OK, deleted %d damaged notes';

implementation

end.

