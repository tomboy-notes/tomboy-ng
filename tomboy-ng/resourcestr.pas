unit ResourceStr;

{ Am attempt to move all resource strings into one unit to facilate
  reuse where possible.  Note that while arranged in blocks labeled
  with the unit that uses them, no reason to limit use to that. }

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

RESOURCESTRING

// notebook.pas

  rsMultipleNoteBooks = 'Settings allow multiple Notebooks';
  rsOneNoteBook = 'Settings allow only one Notebook';
  rsSetTheNotebooks = 'Set the notebooks this note is a member of';
  rsChangeNameofNotebook = 'Change the name of this Notebook';
  rsNumbNotesAffected = 'This will affect %d notes';                 // %d replaced by integer, 0 to big number
  rsEnterNewNotebook = 'Enter a new notebook name please';


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
  rsSyncNCDefault = 'https://YOURSERVER/index.php/apps/grauphel';


// Settings but only part ...

  //rsChangeNetSync = 'Change Net Sync Repo';          // These are labels on the button used to set sync repo
  //rsChangeSync = 'Change Sync Repo';
  rsSyncNotConfig = 'not configured';               // means that the file of net sync is not configured yet.
  rsSetUp = 'Setup';                               // means configure something, eg, one of the Sync modules.

implementation

end.

