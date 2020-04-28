unit settings;
{
 * Copyright (C) 2017 David Bannon
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

{	This form represents all the settings and will have most (?) of the constants.

	Makes sense for all units to 'use' this unit, ideally in the implmentation
	section.
}

{	HISTORY
	2017/9/27 - Created

	2017/10/10 - added ability to set fonts to small, medium and big

	2017/10/15 - gave the setting form Tabs, lot less cluttered.

	2017/11/25 - added a button to notes path config to use the 'default' path
	that is, similar to what tomboy does. Code to make that path.

	2017/11/28 - put a ; after a line of windows only code.

	2017/12/08 - changed size of Mediun normal font, one size smaller
	2017/12/28 - extensive changes so this form is now Main form. This is
				because Cocoa cannot handle Hide() in main form OnShow event.
				This makes more sense anyway.
	2017/12/28 - Small change to force a Note Directory if user browses away
				from Settings screen that urges them to set it first ! Sigh...
	2017/12/29  Further force of a Note Directory.
	2017/12/30  We now set Search box lablepath after setting up a NotesPath
				Added a caption to tell user we are setting up sync.
	2017/12/30  Added a call to IndexNotes after setting up sync, potentially slow.
	2018/01/25  Changes to support Notebooks
    2018/02/04  Added a Main menu because Macs work better with one.
    2018/02/09  Added a means save export path but only until app exits, not saved to disk.
    2018/02/14  Added check boxes to control search box
    2018/02/23  Added capabily to configure spell check.
    2018/03/18  Added a close button (really a hide button) and an ifdef to close
                on the Mac when ever asked to do so. Have disabled close icon but seems
                it still works on Linux but not mac, so thats OK (but funny).
                Issue #25 relates, untested.
	2018/03/24	Added some checks to make sure spell libary and dictionary mentioned
				in config file is still valid.
    2018/05/12  Extensive changes - MainUnit is now just that.
    2018/05/20  NeedRefresh to indicate when need to refresh menus and mainform status.
    2018/05/23  Added /usr/share/myspell/ to linux dictionary search path.
                Enabled Save button after dictionary selection.
    2018/06/06  Substantial changes. Now create config dir at form creation.
                User no longer manually saves, config file is updated at each change.
                Extensive checks of config and notes directory before proceeding.
    2018/06/14  Moved call to CheckSpelling() from OnShow to OnCreate.
                Select MediumFont in default settings.
    2018/07/22  Removed an errant editbox that somehow appeared over small font button.
    2018/08/18  Now call SpellCheck() after loading settings. Note, if settings file
                has an old library name and hunspell can find a new one, nothing is updated !
    2018/08/23  Ensured that an ini file without a notedir returns a sensible value, TEST
    2018/10/28  Much changes, support Backup management, snapshots and new sync Model.
    2018/11/01  Ensure we have a valid Spell, even after a hide !
    2018/11/05  Set default tab.
    2018/11/29  Change Spelling UI when selecting Library and Dictionary
    2018/12/03  Added show splash screen to settings, -g or an indexing error will force show
    2018/12/03  disable checkshowTomdroid on all except Linux
    2019/03/19  Added setting option to show search box at startup
    2019/04/07  Restructured Main and Popup menus. Untested Win/Mac.
    2019/04/13  Almost rid of NeedRefresh, SearchForm.IndexNotes() instead.
    2019/04/27  Fix for Huge display font.
    2019/05/06  Support saving pos and open on startup in note.
    2019/05/14  Display strings all (?) moved to resourcestrings
    2019/06/11  Moved some checkboxes and renamed 'Display' to 'Notes'.
    2019/09/6   Button to download Help Notes in non-English
    2019/09/07  User can now select a note font.
    2019/12/18  Moved LinkScanRange to EditBox
    2019/12/20  Ensure we have UsualFont set to something even during first start.
    2019/12/24  Ensure we don't try to sync if its not yet setup.
    2020/03/02  Force our guess fixed font if no config file.
    2020/03/08  Don't call search refreshMenu(mkFileMenu after an initial sync, no need
    2020/03/30  Added code to allow user to set display colours.
    2020/04/07  As well as forcing Linux AltHelpNotes into config dir, must also do Windows !
    2020/04/08  Added some code to support SyncNextCloud, see define SHOW_NET_SYNC top of implementation section.
    2020/04/10  Added Net and File sync mode to settings file, make labels consistent
    2020/04/28  Put four random digits in place of the '0000' in GetLocalTime()
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, {FileUtil,} Forms, Controls, Graphics, Dialogs, StdCtrls,
    Buttons, ComCtrls, ExtCtrls, Grids, Menus, EditBtn, FileUtil, BackUpView,
    ncsetup, oauth, LCLIntf;

// Types;

type TSyncOption = (AlwaysAsk, UseServer, UseLocal);	// Relating to sync clash pref in config file

type

    { TSett }

    TSett = class(TForm)
          ButtDefaultNoteDir: TButton;
		  CheckBoxAutoSync: TCheckBox;
		  GroupBoxSync: TGroupBox;
		  Label4: TLabel;
		  LabelFileSync: TLabel;
		  LabelNCSyncURL: TLabel;
		  RadioFileSync: TRadioButton;
		  RadioSyncNC: TRadioButton;
	  ButtonSetColours: TButton;
          ButtonFixedFont: TButton;
          ButtonFont: TButton;
          ButtonHelpNotes: TButton;
          ButtonSetSpellLibrary: TButton;
          ButtonSetDictionary: TButton;
          ButtonManualSnap: TButton;
          ButtonSetSnapDir: TButton;
          ButtonSnapDays: TButton;
          ButtonSnapRecover: TButton;
	  ButtonShowBackUp: TButton;

	  ButtonSetNotePath: TButton;
          CheckAutoStart : TCheckBox;
          CheckCaseSensitive: TCheckBox;
          CheckManyNotebooks: TCheckBox;
          CheckShowSearchAtStart: TCheckBox;
          CheckShowSplash: TCheckBox;
	  CheckShowExtLinks: TCheckBox;
	  CheckShowIntLinks: TCheckBox;
          CheckShowTomdroid: TCheckBox;
          CheckSnapEnabled: TCheckBox;
          CheckSnapMonthly: TCheckBox;
          FontDialog1: TFontDialog;
          GroupBox1: TGroupBox;
	  GroupBox4: TGroupBox;
	  GroupBox5: TGroupBox;
	  Label1: TLabel;
          Label10: TLabel;
          Label11: TLabel;
          Label12: TLabel;
          Label13: TLabel;
          Label14: TLabel;
          Label15: TLabel;
          LabelDicPrompt: TLabel;
          LabelDic: TLabel;
          LabelError: TLabel;
          LabelLibrary: TLabel;
          LabelDicStatus: TLabel;
          LabelLibraryStatus: TLabel;
          LabelSnapDir: TLabel;
	  Label2: TLabel;
	  Label3: TLabel;
	  Label6: TLabel;
	  Label7: TLabel;
	  Label8: TLabel;
	  Label9: TLabel;
	  LabelNotesPath: TLabel;
	  LabelSettingPath: TLabel;
          ListBoxDic: TListBox;
          MenuFriday: TMenuItem;
          MenuSaturday: TMenuItem;
          MenuSunday: TMenuItem;
          MenuMonday: TMenuItem;
          MenuTuesday: TMenuItem;
          MenuWednesday: TMenuItem;
          MenuThursday: TMenuItem;
          OpenDialogLibrary: TOpenDialog;
          OpenDialogDictionary: TOpenDialog;
	  PageControl1: TPageControl;
	  Panel1: TPanel;
	  Panel2: TPanel;
          Panel3: TPanel;
          PopupDay: TPopupMenu;
          PMenuMain: TPopupMenu;
	  RadioAlwaysAsk: TRadioButton;
          RadioFontHuge: TRadioButton;
	  RadioFontBig: TRadioButton;
	  RadioFontMedium: TRadioButton;
	  RadioFontSmall: TRadioButton;
	  RadioUseLocal: TRadioButton;
	  RadioUseServer: TRadioButton;
	  SelectDirectoryDialog1: TSelectDirectoryDialog;
          SelectSnapDir: TSelectDirectoryDialog;
          SpeedButHide: TSpeedButton;
	  SpeedButHelp: TSpeedButton;
          SpeedButtTBMenu: TSpeedButton;
		  SpeedSetupSync: TSpeedButton;
	  StringGridBackUp: TStringGrid;
	  TabBasic: TTabSheet;
	  TabBackUp: TTabSheet;
          TabSpell: TTabSheet;
	  TabSnapshot: TTabSheet;
	  TabSync: TTabSheet;
	  TabDisplay: TTabSheet;
          TimeEdit1: TTimeEdit;
          TimerAutoSync: TTimer;

        procedure ButtDefaultNoteDirClick(Sender: TObject);
	procedure ButtonSetColoursClick(Sender: TObject);
        procedure ButtonFixedFontClick(Sender: TObject);
        procedure ButtonFontClick(Sender: TObject);
        procedure ButtonHelpNotesClick(Sender: TObject);
        procedure ButtonManualSnapClick(Sender: TObject);
        procedure ButtonSetDictionaryClick(Sender: TObject);
        //procedure ButtonSaveConfigClick(Sender: TObject);
	procedure ButtonSetNotePathClick(Sender: TObject);
        procedure ButtonSetSnapDirClick(Sender: TObject);
        procedure ButtonSetSpellLibraryClick(Sender: TObject);
	procedure ButtonShowBackUpClick(Sender: TObject);
        procedure ButtonSnapDaysClick(Sender: TObject);
        procedure ButtonSnapRecoverClick(Sender: TObject);
        procedure ButtonSyncHelpClick(Sender: TObject);
        procedure CheckAutostartChange(Sender: TObject);
        procedure CheckBoxAutoSyncChange(Sender: TObject);
        //procedure CheckManyNotebooksChange(Sender: TObject);
        { Called when ANY of the setting check boxes change so use can save. }
	procedure CheckReadOnlyChange(Sender: TObject);
	procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
	// procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormHide(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
            );
        procedure FormShow(Sender: TObject);
        procedure ListBoxDicClick(Sender: TObject);
	procedure PageControl1Change(Sender: TObject);
	procedure SpeedButHelpClick(Sender: TObject);
        procedure SpeedButHideClick(Sender: TObject);
        procedure SpeedButtTBMenuClick(Sender: TObject);
		procedure SpeedSetupSyncClick(Sender: TObject);
        procedure StringGridBackUpDblClick(Sender: TObject);
        procedure RadioFileSyncChange(Sender: TObject);
        procedure TabBasicResize(Sender: TObject);
        procedure TabSnapshotResize(Sender: TObject);
        procedure TabSpellResize(Sender: TObject);
        procedure TimerAutoSyncTimer(Sender: TObject);
        procedure SetColours;

    private
        UserSetColours : boolean;
        OAuth: TOAuth;
        fExportPath : ANSIString;
        // Reads an existing config file OR writes a new, default one if necessary.
 	procedure CheckConfigFile;
        // Ret true and displays on screen if passed Full name is a usable dictonary
        // sets SpellConfig and triggers a config save if successful
        function CheckDictionary(const FullDicName : string): boolean;
        // Checks and/or makes indicatd dir, warns user if not there and writable.
        function CheckDirectory(DirPath: string): boolean;
        // Returns the number of files that could be dictionaries in indicated directory
        function CheckForDic(const DictPath: ANSIString): integer;
        { If LabelLib has a valid full name (of hunspell library), tests it, otherwise asks hunspell
          to guess some names. In either case, exits if fail, if successful then tries for
          a dictionary, either using default directories and populating listbox or if it finds one
          or a full name was provided in DicFullName, just tests that name.
          If successfull show on screen and saves config }
        procedure CheckSpelling(const DicFullName: string='');
        procedure DicDefaults(var DicPathAlt: string);
        // Returns a good place to save config or user requested place if on cmdline,
        function GetDefaultConfigDir: string;
        // Returns the default place to store notes. It may not be present.
        function GetDefaultNoteDir: string;
                            // Has a list of possible fixed font names, returns the first that 'works'.
        function GetFixedFont(): string;
        function MyBoolStr(const InBool: boolean) : string;

        procedure SetFontSizes;
        // Saves all current settings to disk. Call when any change is made. If unable
        // to write to disk, returns False;
        function SettingsChanged(): boolean;
		function fSyncOK(): boolean;
		procedure SyncSettings;
        //function ZipDate: string;

    public
        AreClosing : boolean;       // False until set true by mainUnit FormClose.
        BackGndColour : TColor;     // Next three set in main unit.
        TextColour : TColor;
        HiColour : TColor;
        TitleColour : TColor;
        UsualFont : string;
        FixedFont : string;
        DefaultFixedFont : string;
        DarkTheme : boolean;
        DebugModeSpell : boolean;
        // Indicates SettingsChanged should not write out a new file cos we are loading from one.
        MaskSettingsChanged : boolean;
        AllowClose : Boolean;           // review need for this
        // Indicates we should re-index notes when form hides
        //NeedRefresh : Boolean;
        FontSmall  : Integer;
     	FontLarge  : Integer;
     	FontHuge   : Integer;
     	FontTitle  : Integer;			// Dont set this to one of the other sizes !
     	FontNormal : Integer;
        { The directory expected to hold existing or new notes }
        NoteDirectory : string;
        { The dir expected to hold config file and, possibly local manifest }
        LocalConfig : string;
        { relevent only when using file sync }
        // RemoteRepo  : string;

        SyncOption : TSyncOption;
        { Indicates we have done a config, not necessarily a valid one }
        HaveConfig : boolean;
        { Indicates user wants to see internal links }
        ShowIntLinks : boolean;
        { Says Notes should be treated as read only, a safe choice }
        NotesReadOnly : boolean;
            { Indicates Spell is configured and LabelLibrary and LabelDic should
            contain valid full file names.}
        SpellConfig : boolean;
        // service functon to other units, returns a string with current datetime
        // in a format like the Tomboy schema.
        function GetLocalTime: ANSIstring;
            { Triggers a Sync, if its not all setup aready and working, user show and error }
        procedure Synchronise();
        property SyncOK : boolean read fSyncOK;
        property ExportPath : ANSIString Read fExportPath write fExportPath;
        // Called after notes are indexed, if settings so indicate, will start auto timer.
        procedure CheckAutoSync();
    end;

var
    Sett : TSett;


const
                                // Note we set DarkTheme colors and all HiLight colours in MainUnit
    Placement = 45;				// where we position an opening window. Its, on average, 1.5 time Placement;



implementation

{$define DISABLE_NET_SYNC}     // disable this define by, eg, putting a 'not' ahead of the '$'

{$R *.lfm}

{ TSett }


uses IniFiles, LazLogger,
    LazFileUtils,   // LazFileUtils needed for TrimFileName(), cross platform stuff;
    Note_Lister,	// List notes in BackUp and Snapshot tab
    SearchUnit,		// So we can call IndexNotes() after altering Notes Dir
    syncGUI,
    syncutils,
    recover,        // Recover lost or damaged files
    mainunit,       // so we can call ShowHelpNote()
    hunspell,       // spelling check
    helpnotes,      // All user to download non-English help Notes
    LCLType,        // Keycodes ....
    Autostart,
    Colours,
    ResourceStr     // only partioally so far ....
    {$ifdef LINUX}, Unix {$endif} ;              // We call a ReReadLocalTime();

var
    Spell: THunspell;
    // Initially the first place we look for dictionaries, later its the path to
    // dictionaries listed in ListBoxDic
     DicPath : AnsiString;

procedure TSett.SetFontSizes;
begin
    if RadioFontHuge.checked then begin
        FontSmall  := 11;
        FontLarge  := 20;
        FontHuge   := 23;
        FontTitle  := 21;			// Dont set this to one of the other sizes !
        FontNormal := 16;
    end;
	if RadioFontBig.checked then begin
    	FontSmall  := 9;
     	FontLarge  := 17;
     	FontHuge   := 20;
     	FontTitle  := 18;			// Dont set this to one of the other sizes !
     	FontNormal := 14;
	end;
    if RadioFontMedium.checked then begin
    	FontSmall  := 8;
 		FontLarge  := 14;
 		FontHuge   := 18;
 		FontTitle  := 16;			// Dont set this to one of the other sizes !
 		FontNormal := 11;
	end;
    if RadioFontSmall.Checked then begin
    	FontSmall  := 7;
 		FontLarge  := 13;
 		FontHuge   := 16;
 		FontTitle  := 14;			// Dont set this to one of the other sizes !
 		FontNormal := 10;
    end;

end;

	{ Make public things agree with internal ones. }
procedure TSett.SyncSettings;
begin
	if NoteDirectory <> '' then begin
        LabelNotespath.Caption := NoteDirectory;
        HaveConfig := (NoteDirectory <> '');
        // CheckShowIntLinks.enabled := true;
        ShowIntLinks := CheckShowIntLinks.Checked;
        SetFontSizes();
	    if RadioAlwaysAsk.Checked then SyncOption := AlwaysAsk
        else if RadioUseLocal.Checked then SyncOption := UseLocal
        else if RadioUseServer.Checked then SyncOption := UseServer;
	end;
end;

procedure TSett.PageControl1Change(Sender: TObject);
begin
	if NoteDirectory = '' then ButtDefaultNoteDirClick(self);
    Label15.Caption := '';
    SpeedButHelp.Visible := (PageControl1.TabIndex = 2);    // Only show for Sync Tab
end;

procedure TSett.SpeedButHelpClick(Sender: TObject);
begin
        MainForm.ShowHelpNote('sync-ng.note');
end;

procedure TSett.SpeedButHideClick(Sender: TObject);
begin
    Hide;
end;

procedure TSett.SpeedButtTBMenuClick(Sender: TObject);
begin
    PMenuMain.Popup;
end;

procedure TSett.SpeedSetupSyncClick(Sender: TObject);
begin
    {  ToDo : here we check if there is an existing local manifest and assume, incorrectly, that
       it must be associated with an existing FileSync. When we understand a bit more about
       nextcloud sync process, fix ! }
    if RadioFileSync.Checked then begin
	        if NoteDirectory = '' then ButtDefaultNoteDirClick(self);
		    if FileExists(LocalConfig + 'manifest.xml') then
	            if mrYes <> QuestionDlg('Warning', rsChangeExistingSync, mtConfirmation, [mrYes, mrNo], 0) then exit;
	        if SelectDirectoryDialog1.Execute then begin
	           FormSync.NoteDirectory := NoteDirectory;
	           FormSync.LocalConfig := LocalConfig;
	           FormSync.SetupSync := True;
	           LabelFileSync.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
	           if mrOK = FormSync.ShowModal then begin
	              SettingsChanged();
	           end else
	               LabelFileSync.Caption := rsSyncNotConfig;
	        end;
	end else begin      // Assuming if its not FileSync, must be NextCloud.....
        // ToDo : as mentioned above, must warn user if they are changing an existing sync.
            if((LabelNCSyncURL.Caption = rsSyncNotConfig) or (length(LabelNCSyncURL.Caption)<5))
                        then FormNCSetup.URL.Text   :=  'https://yourcloudserver/index.php/apps/grauphel'
                        else FormNCSetup.URL.Text   :=  LabelNCSyncURL.Caption;
            FormNCSetup.oauth := OAuth;
            if(FormNCSetup.ShowModal = mrOK ) then begin
                    SettingsChanged();
            end;
	end;
end;

procedure TSett.StringGridBackUpDblClick(Sender: TObject);
var
    BV : TFormBackupView;
    NoteTitle : ANSIstring;
    FileName : string;
begin
	FileName := StringGridBackUp.Cells[3, StringGridBackUp.Row];
    if FileName = '' then exit();
  	if not FileExistsUTF8(Sett.NoteDirectory + 'Backup' + PathDelim + FileName) then begin
      	showmessage('Cannot open ' + Sett.NoteDirectory + 'Backup' + PathDelim + FileName);
      	exit();
  	end;
  	NoteTitle := StringGridBackup.Cells[0, StringGridBackUp.Row];
//  	if length(NoteTitle) > 0 then
//        OpenNote(NoteTitle, FullFileName);
    BV := TFormBackupView.Create(self);
    try
        BV.FileName := FileName;
        BV.NoteTitle := NoteTitle;
        BV.ShowModal;
        if BV.NotesChanged then ButtonShowBackUpClick(self);
    finally
        FreeandNil(BV);
    end;
end;

procedure TSett.RadioFileSyncChange(Sender: TObject);
begin
{   This is crazy, somehow fiddling with Label1's canvas is calling this.
    Don't need it calling Settings right now but ...............  }

if  RadioFileSync.Checked then begin
        if self.LabelFileSync.caption = rsSyncNotConfig then
            SpeedSetUpSync.caption := rsSetUp
        else SpeedSetUpSync.caption := rsChangeSync;
        LabelNCSyncURL.Visible := False;
        LabelFileSync.Visible := True;
    end else begin
        if self.LabelNCSyncURL.caption = rsSyncNotConfig then
            SpeedSetUpSync.caption := rsSetUp
        else SpeedSetUpSync.caption := rsChangeSync;
        LabelNCSyncURL.Visible := True;
        LabelFileSync.Visible := True;
	end;
   { SettingsChanged();       }

end;

procedure TSett.TabBasicResize(Sender: TObject);
begin
    buttonSetNotePath.Width := (TabBasic.Width div 2) - 12;
end;

procedure TSett.TabSnapshotResize(Sender: TObject);
begin
    ButtonManualSnap.Width :=  (TabSnapshot.Width div 2) -10;
end;

procedure TSett.TabSpellResize(Sender: TObject);
begin
    ButtonSetSpellLibrary.Width := (TabSpell.Width div 2) -7;
    ButtonSetDictionary.Width := ButtonSetSpellLibrary.Width;
end;



    { ----------------- S P E L L I N G ----------------------}

ResourceString
    rsSelectLibrary = 'Select your hunspell library';
    rsSelectDictionary = 'Select the dictionary you want to use';
    rsDictionaryLoaded = 'Dictionary Loaded OK';
    rsDictionaryFailed = 'Library Not Loaded';
    rsDictionaryNotFound = 'No Dictionary Found';

procedure TSett.ButtonSetSpellLibraryClick(Sender: TObject);
begin
    OpenDialogLibrary.InitialDir := ExtractFilePath(LabelLibrary.Caption);
    OpenDialogLibrary.Filter := 'Library|libhunspell*';
    OpenDialogLibrary.Title := rsSelectLibrary;
    if OpenDialogLibrary.Execute then begin
        LabelLibrary.Caption := TrimFilename(OpenDialogLibrary.FileName);
        CheckSpelling();
    end;
end;

procedure TSett.ButtonSetDictionaryClick(Sender: TObject);
begin
    OpenDialogDictionary.InitialDir := ExtractFilePath(LabelDic.Caption);
    OpenDialogDictionary.Filter := 'Dictionary|*.dic';
    OpenDialogDictionary.Title := rsSelectDictionary;
    if OpenDialogDictionary.Execute then
        CheckDictionary(TrimFilename(OpenDialogDictionary.FileName));
end;

function TSett.CheckForDic(const DictPath : ANSIString) : integer;
var
    Info : TSearchRec;
begin
    LabelError.Caption := '';
    ListBoxDic.Clear;
    ListBoxDic.Enabled := False;
    if FindFirst(AppendPathDelim(DictPath) + '*.dic', faAnyFile and faDirectory, Info)=0 then begin
        repeat
            ListBoxDic.Items.Add(Info.Name);
        until FindNext(Info) <> 0;
    end;
    FindClose(Info);
    if DebugModeSpell then debugln('CheckForDic searched ' + DictPath + ' and found ' + inttostr(ListBoxDic.Items.Count));
    if ListBoxDic.Items.Count > 0 then begin
        DicPath := DictPath;
        LabelDic.Caption := DictPath;
    end;
    exit(ListBoxDic.Items.Count);
end;


procedure TSett.ListBoxDicClick(Sender: TObject);
begin
    if ListBoxDic.ItemIndex > -1 then
        CheckDictionary(AppendPathDelim(DicPath) + ListBoxDic.Items.Strings[ListBoxDic.ItemIndex]);
end;

function TSett.CheckDictionary(const FullDicName : string) : boolean;
begin
    result := false;
    if fileexists(FullDicName) then begin
        if assigned(Spell) then begin
            SpellConfig := Spell.SetDictionary(FullDicName);
            if SpellConfig then begin
               LabelDicStatus.Caption := rsDictionaryLoaded;
               LabelDic.Caption := FullDicName;
               SettingsChanged();
               // NeedRefresh := True;         // ToDo : April '19, don't need this ???
               Result := True;
            end else begin
                LabelDicStatus.Caption := rsDictionaryNotFound;
            end;
        end;
    end else debugln('ERROR - called CheckDictionary with Spell nil');
    if DebugModeSpell then debugln('CheckDictionary ' + FullDicName + ' return ' + booltostr(Result, True));
end;

procedure TSett.DicDefaults(var DicPathAlt : string);
begin
    DicPathAlt := ExtractFilePath(Application.ExeName);
    {$ifdef WINDOWS}
    DicPath := 'C:\Program Files\LibreOffice 5\share\extensions\dict-en\';
    {$ENDIF}
    {$ifdef DARWIN}
    DicPath := '/Library/Spelling/';
    DicPathAlt := '/Applications/tomboy-ng.app/Contents/Resources/';
    {$endif}
    {$ifdef LINUX}
    DicPath := '/usr/share/hunspell/';
    DicPathAlt := '/usr/share/myspell/';
    {$ENDIF}
end;

procedure TSett.CheckSpelling(const DicFullName : string = '');
var
    DicPathAlt, DicToCheck : AnsiString;

begin
    { The hunspell unit tries to find a library using some educated guesses.
      Once found, its saved in config and we pass that to hunspell as a suggested
      first place to try.
      We set likely dictionary locations here.
    }
    DicToCheck := '';
    LabelError.Caption:='';
    ListBoxDic.enabled:= False;
    LabelDic.Visible := False;
    LabelDicStatus.Visible := False;
    LabelDicPrompt.Visible := False;
    SpellConfig := False;
    if DicFullName = '' then DicDefaults(DicPathAlt);        // startup mode
    DebugModeSpell := Application.HasOption('debug-spell');
    // LabelLibrary.Caption := '/usr/local/Cellar/hunspell/1.6.2/lib/libhunspell-1.6.0.dylib';
    if fileexists(LabelLibrary.Caption) then		// make sure file from config is still valid
    	Spell :=  THunspell.Create(DebugModeSpell, LabelLibrary.Caption)
    else Spell :=  THunspell.Create(DebugModeSpell);
    if Spell.ErrorMessage <> '' then begin
        LabelLibraryStatus.Caption := rsDictionaryFailed;
        exit();
    end;
    if DebugModeSpell then debugln('Library OK, lets look for dictionary');
    LabelLibraryStatus.caption := rsDictionaryLoaded;
    LabelLibrary.Caption := Spell.LibraryFullName;
    LabelDicStatus.Visible := True;
    LabelDic.Visible := True;
    if DicFullName = '' then begin
        if (not DirectoryExistsUTF8(LabelDic.Caption))
                    and (FileExistsUTF8(LabelDic.Caption)) then  // we have a nominated file from config
            if CheckDictionary(LabelDic.Caption) then exit;      // All good, use it !
        if  0 = CheckForDic(DicPath) then begin                  // We'll try our defaults ....
            if 0 = CheckForDic(DicPathAlt) then begin
                LabelDicStatus.Caption := rsDictionaryNotFound;
                exit();
            end;
        end;
     end else DicToCheck := DicFullName;
     if ListBoxDic.Items.Count = 1 then
         DicToCheck := AppendPathDelim(LabelDic.Caption) + ListBoxDic.Items.Strings[0];
    if  ListBoxDic.Items.Count > 1 then begin                     // user must select
        LabelDicStatus.Caption := rsSelectDictionary;
        ListBoxDic.Enabled:= True;
        exit();
    end;
     // if to here, we have 1 candidate dictionary, either exactly 1 found or DicFullName has content
    if CheckDictionary(DicToCheck) then
        if DebugModeSpell then debugln('Spelling Configured.');
end;

{ --------------------- H O U S E    K E E P I NG -------------------- }

procedure TSett.FormHide(Sender: TObject);
begin
    FreeandNil(Spell);
    MaskSettingsChanged := True;
{    if NeedRefresh then begin
        SearchForm.IndexNotes();
        NeedRefresh := False;
    end;    }
end;

procedure TSett.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if {$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif} in Shift then begin
      if key = ord('N') then begin SearchForm.OpenNote(); Key := 0; exit(); end;
      if key = VK_Q then MainForm.Close();
    end;
end;

procedure TSett.FormShow(Sender: TObject);
begin
    //CheckSpelling;
    if not assigned(Spell) then
        Spell := THunspell.Create(Application.HasOption('debug-spell'), LabelLibrary.Caption);
        // user user has 'closed' (ie hide) then Spell was freed.
    MaskSettingsChanged := False;
    Label15.Caption:='';
    StringGridBackUp.Clean;
end;

// We only really close when told by RTSearch that The Exit Menu choice from TrayIcon was clicked.
procedure TSett.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	if AllowClose then begin
    	CloseAction := caFree;
        SearchForm.Close;
	end else CloseAction := caHide;
end;

RESOURCESTRING
    rsSetFileSyncRepo = 'Set File Sync Repo';

procedure TSett.FormCreate(Sender: TObject);
begin
    AreClosing := false;
    Top := 100;
    Left := 300;
    {$ifdef DISABLE_NET_SYNC}
    RadioSyncNC.enabled := false;
    RadioFileSync.Hint := 'NextCloud / Grauphel will be in a future Release';
    RadioFileSync.ShowHint := True;
    {$else}
    OAuth := TOAuth.Create();                  // note, not being freed
    {$endif}
    DefaultFixedFont := GetFixedFont(); // Tests a list of likely suspects.
    PageControl1.ActivePage := TabBasic;
    MaskSettingsChanged := true;
    //NeedRefresh := False;
    ExportPath := '';
    LabelLibrary.Caption := '';
    HaveConfig := false;
    LocalConfig := GetDefaultConfigDir();   // sys dependant unless user has overridden
    LabelSettingPath.Caption := LocalConfig + 'tomboy-ng.cfg';
    NoteDirectory := Sett.GetDefaultNoteDir;
    labelNotesPath.Caption := NoteDirectory;
    CheckShowTomdroid.Enabled := {$ifdef LINUX}True{$else}False{$endif};
    CheckConfigFile();                      // write a new, default one if necessary
    CheckSpelling();
    //MainForm.FillInFileMenus();
end;

procedure TSett.FormDestroy(Sender: TObject);
begin
    FreeandNil(Spell);
end;

{ --------------------- F I L E    I / O --------------------------- }

RESOURCESTRING
    rsErrorCreateDir = 'Unable to Create Directory';
    rsErrorCannotWrite = 'Cannot write into';

function TSett.CheckDirectory(DirPath : string) : boolean;
begin
    Result := False;
    if not DirectoryExistsUTF8(DirPath) then
        ForceDirectoriesUTF8(DirPath);
    if not DirectoryExistsUTF8(DirPath) then begin
        ShowMessage(rsErrorCreateDir + ' [' + DirPath + ']');
        Debugln('Settings is unable to Create Directory [' + DirPath + ']');
        exit(False);
    end;
    if DirectoryIsWritable(DirPath) then
        exit(True);
    ShowMessage(rsErrorCannotWrite + ' [' + DirPath + ']');
    DebugLn('Settings cannot write into [' + DirPath + ']');
end;

function TSett.GetDefaultConfigDir : string;
begin
    Result := '';
    if Application.HasOption('config-dir') then
        Result := Application.GetOptionValue('config-dir');
    if Result = '' then begin
        {$ifdef DARWIN}
        // First we try the right place, if there use it, else try unix place, if
        // its not there, go back to right place.
        Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-ng/Config';
        if not DirectoryExistsUTF8(Result) then begin
            Result := GetAppConfigDirUTF8(False);
            if not DirectoryExistsUTF8(Result) then  // must be new install, put in right place
                Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-ng/Config';
        end;
        {$else}
        Result := GetAppConfigDirUTF8(False);
        {$endif}
    end;
    Result := AppendPathDelim(Result);
    {$ifndef DARWIN}
    MainForm.SetAltHelpPath(Result);    // English help notes in read only space
    {$endif}
end;

function TSett.GetFixedFont() : string;
var  T : string;
    FontNames : array[1..7] of string
      = ('Monospace', 'Monaco', 'Nimbus Mono L', 'Liberation Mono', 'Lucida Console', 'Lucida Sans Typewriter', 'Courier New' );
    // Add as many new names as you like but set array size.  Chooses the first in the list it finds that works
    // Label does not seem to worry about us playing with its canvas.

    function IsMono(FontName : String) : boolean;
    begin
      Label1.Canvas.Font.Name := FontName;
      result := Label1.Canvas.TextWidth('i') = Label1.Canvas.TextWidth('w');
      // I have no idea whats happening here, that line, above, somehow triggers a call
      // to the RadioFileSync OnChange handler. In turn, that calls SettingsChanged before
      // we have setup its config path.  For now, because its not needed yet, I have
      // commented out the code in the OnChange
    end;

    function IsDifferentSizes() : boolean;     // in case they are old non scalable, unlikely but ....
    var
        ASize : integer;
    begin
        Label1.Canvas.Font.Size := 13;
        ASize := Label1.Canvas.TextHeight('H');
        Label1.Canvas.Font.Size := 14;
        if ASize = Label1.Canvas.TextHeight('H')
            then exit(False);
        ASize := Label1.Canvas.TextHeight('H');
        Label1.Canvas.Font.Size := 15;
        If ASize = Label1.Canvas.TextHeight('H')
            then exit(False);
        result := True;
    end;

begin
    Result := '';
    for T in FontNames do begin
        if not IsMono(T) then continue;
        if not IsDifferentSizes() then continue;
        Result := T;
        exit;
    end;
end;

{ Read config file if it exists }
procedure TSett.CheckConfigFile;
var
    ConfigFile : TINIFile;
    ReqFontSize, SyncType : ANSIString;
begin
    if not CheckDirectory(LocalConfig) then exit;
    if fileexists(LabelSettingPath.Caption) then begin
 	    ConfigFile :=  TINIFile.Create(LabelSettingPath.Caption);
 	    try
            MaskSettingsChanged := True;    // should be true anyway ?
   		    NoteDirectory := ConfigFile.readstring('BasicSettings', 'NotesPath', NoteDirectory);
            CheckShowIntLinks.Checked :=
                ('true' = ConfigFile.readstring('BasicSettings', 'ShowIntLinks', 'true'));
            CheckShowExtLinks.Checked :=
                ('true' = ConfigFile.readstring('BasicSettings', 'ShowExtLinks', 'true'));
            CheckManyNoteBooks.checked :=
        	    ('true' = Configfile.readstring('BasicSettings', 'ManyNotebooks', 'false'));
            CheckCaseSensitive.Checked :=
                ('true' = Configfile.readstring('BasicSettings', 'CaseSensitive', 'false'));
            CheckShowTomdroid.Checked :=
                ('true' = Configfile.readstring('BasicSettings', 'ShowTomdroid', 'false'));
            CheckShowSplash.Checked :=
                ('true' = Configfile.ReadString('BasicSettings', 'ShowSplash', 'true'));
            CheckAutostart.Checked :=
                ('true' = Configfile.ReadString('BasicSettings', 'Autostart', 'false'));
            CheckShowSearchAtStart.Checked :=
                ('true' = Configfile.ReadString('BasicSettings', 'ShowSearchAtStart', 'false'));
            ReqFontSize := ConfigFile.readstring('BasicSettings', 'FontSize', 'medium');
            case ReqFontSize of
                'huge'   : RadioFontHuge.Checked := true;
        	    'big'    : RadioFontBig.Checked := true;
                'medium' : RadioFontMedium.Checked := true;
                'small'  : RadioFontSmall.Checked := true;
            end;
            UsualFont := ConfigFile.readstring('BasicSettings', 'UsualFont', GetFontData(Self.Font.Handle).Name);
            ButtonFont.Hint := UsualFont;
            FixedFont := ConfigFile.readstring('BasicSettings', 'FixedFont', DefaultFixedFont);
            if FixedFont = '' then FixedFont := DefaultFixedFont;
            ButtonFixedFont.Hint := FixedFont;
            BackGndColour:=   StringToColor(Configfile.ReadString('BasicSettings', 'BackGndColour', '0'));
            HiColour :=   StringToColor(Configfile.ReadString('BasicSettings', 'HiColour', '0'));
            TextColour := StringToColor(Configfile.ReadString('BasicSettings', 'TextColour', '0'));
            TitleColour :=  StringToColor(Configfile.ReadString('BasicSettings', 'TitleColour', '0'));
            UserSetColours := not ((BackGndColour = 0) and (HiColour = 0) and (TextColour = 0) and (TitleColour = 0));
            // Note - '0' is a valid colour, black. So, what says its not set is they are all '0';
            case ConfigFile.readstring('SyncSettings', 'SyncOption', 'AlwaysAsk') of
                'AlwaysAsk' : begin SyncOption := AlwaysAsk; RadioAlwaysAsk.Checked := True; end;
                'UseLocal'  : begin SyncOption := UseLocal;  RadioUseLocal.Checked  := True; end;
                'UseServer' : begin SyncOption := UseServer; RadioUseServer.Checked := True; end;
		    end;

            LabelFileSync.Caption := ConfigFile.readstring('SyncSettings', 'SyncRepo', '');
            LabelNCSyncURL.Caption := ConfigFile.readstring('SyncSettings', 'SyncRepoNCURL', '');
            if LabelFileSync.Caption = '' then LabelFileSync.Caption := rsSyncNotConfig;
            if LabelNCSyncURL.Caption = '' then LabelNCSyncURL.Caption := rsSyncNotConfig;
            SyncType := ConfigFile.readstring('SyncSettings', 'SyncType', '');          // this is new way to do it, file, nextcloud, etc
            case SyncType of
                '' :    // SyncType not present, check for legacy model
                        Self.RadioFileSync.checked := (ConfigFile.readstring('SyncSettings', 'UseFileSync', 'true') = 'true');
                'file' :  RadioFileSync.checked := true;
                'nextcloud' : self.RadioSyncNC.checked := true;
			end;
            LabelLibrary.Caption := ConfigFile.readstring('Spelling', 'Library', '');
            LabelDic.Caption := ConfigFile.readstring('Spelling', 'Dictionary', '');
            SpellConfig := (LabelLibrary.Caption <> '') and (LabelDic.Caption <> '');     // indicates it worked once...
	    LabelSnapDir.Caption := ConfigFile.readstring('SnapSettings', 'SnapDir', NoteDirectory + 'Snapshot' + PathDelim);
            CheckBoxAutoSync.checked :=
                ('true' = Configfile.ReadString('SyncSettings', 'Autosync', 'false'));
        finally
            ConfigFile.free;
            // MaskSettingsChanged := False;
	    end;
        CheckDirectory(NoteDirectory);
        CheckDirectory(LabelSnapDir.Caption);
	    SyncSettings();
        // NeedRefresh := True;                // ToDo : Needed ??? April 19, dont think so ???
        // ButtonSaveConfig.Enabled := False;
    end else begin      // OK, no config eh ?  We'll set some defaults ...
        if CheckDirectory(NoteDirectory) then begin
            MaskSettingsChanged := False;
            RadioFontMedium.Checked := True;
            CheckShowIntLinks.Checked:= True;
            CheckShowExtLinks.Checked := True;
            CheckBoxAutoSync.Checked := False;
            LabelSnapDir.Caption := NoteDirectory + 'Snapshot' + PathDelim;
            UsualFont := GetFontData(Self.Font.Handle).Name;
            FixedFont := DefaultFixedFont;
            LabelFileSync.Caption := rsSyncNotConfig;
            LabelNCSyncURL.Caption := rsSyncNotConfig;
            RadioFileSync.checked := true;
            if not SettingsChanged() then // write a initial default file, shows user a message on error
                HaveConfig := false;
            MaskSettingsChanged := True;
            HaveConfig := True;
        end else begin
            // Only get to here becasue we have failed to setup an initial notes dir
            // and we don't even have a settings file in place. Directories may not be present.
            LabelNotespath.Caption := 'Please Set a Path to a Notes Directory';
            NoteDirectory := '';
            CheckManyNoteBooks.Checked := False;
            HaveConfig := false;
            Debugln('We have (write) issues with your directories, suggest you do not proceed !');
        end;
    end;
end;

function TSett.fSyncOK() : boolean;
begin
    Result :=
        (RadioFileSync.checked and
        ((LabelFileSync.Caption <> rsSyncNotConfig) and  (LabelFileSync.Caption <> '')))
        or
        (RadioSyncNC.Checked and
        ((LabelNCSyncURL.caption <> rsSyncNotConfig) and (LabelFileSync.Caption <> '')));
        // I don't think we can end up with those labels empty but just in case .....
end;

function TSett.MyBoolStr(const InBool : boolean) : string;
begin
    if InBool then result := 'true' else result := 'false';
end;

function TSett.SettingsChanged() : boolean;
var
	ConfigFile : TINIFile;
begin
    Result := True;
    if MaskSettingsChanged then exit();
    ConfigFile :=  TINIFile.Create(LabelSettingPath.Caption);
    try
        try
            ConfigFile.writestring('BasicSettings', 'NotesPath', NoteDirectory);
            if CheckManyNoteBooks.checked then
                Configfile.writestring('BasicSettings', 'ManyNotebooks', 'true')
            else Configfile.writestring('BasicSettings', 'ManyNotebooks', 'false');
            if CheckCaseSensitive.checked then
                Configfile.writestring('BasicSettings', 'CaseSensitive', 'true')
            else Configfile.writestring('BasicSettings', 'CaseSensitive', 'false');
            if CheckShowIntLinks.Checked then
                ConfigFile.writestring('BasicSettings', 'ShowIntLinks', 'true')
            else ConfigFile.writestring('BasicSettings', 'ShowIntLinks', 'false');
            ConfigFile.writestring('BasicSettings', 'ShowExtLinks',      MyBoolStr(CheckShowExtLinks.Checked));
            ConfigFile.writestring('BasicSettings', 'ShowTomdroid',      MyBoolStr(CheckShowTomdroid.Checked));
            ConfigFile.WriteString('BasicSettings', 'ShowSplash',        MyBoolStr(CheckShowSplash.Checked));
            ConfigFile.WriteString('BasicSettings', 'Autostart',         MyBoolStr(CheckAutostart.Checked));
            ConfigFile.WriteString('BasicSettings', 'ShowSearchAtStart', MyBoolStr(CheckShowSearchAtStart.Checked));
            if RadioFontBig.Checked then
                ConfigFile.writestring('BasicSettings', 'FontSize', 'big')
            else if RadioFontMedium.Checked then
                ConfigFile.writestring('BasicSettings', 'FontSize', 'medium')
            else if RadioFontSmall.Checked then
                ConfigFile.writestring('BasicSettings', 'FontSize', 'small')
            else if RadioFontHuge.Checked then
                ConfigFile.writestring('BasicSettings', 'FontSize', 'huge');
            ConfigFile.writestring('BasicSettings', 'UsualFont', UsualFont);
            ConfigFile.writestring('BasicSettings', 'FixedFont', FixedFont);
            //(Sel_CText = 0) and (Sel_CBack = 0) and (Sel_CHiBack = 0) and (Sel_CTitle = 0)
            if UserSetColours then begin
                ConfigFile.writestring('BasicSettings', 'BackGndColour', ColorToString(BackGndColour));
                ConfigFile.writestring('BasicSettings', 'HiColour', ColorToString(HiColour));
                ConfigFile.writestring('BasicSettings', 'TextColour', ColorToString(TextColour));
                ConfigFile.writestring('BasicSettings', 'TitleColour', ColorToString(TitleColour));
			end else begin
                    ConfigFile.writestring('BasicSettings', 'BackGndColour', '0');
                    ConfigFile.writestring('BasicSettings', 'HiColour', '0');
                    ConfigFile.writestring('BasicSettings', 'TextColour', '0');
                    ConfigFile.writestring('BasicSettings', 'TitleColour', '0');
			end;
	    ConfigFile.WriteString('SyncSettings', 'Autosync', MyBoolStr(CheckBoxAutosync.Checked));
	    if RadioAlwaysAsk.Checked then
                ConfigFile.writestring('SyncSettings', 'SyncOption', 'AlwaysAsk')
            else if RadioUseLocal.Checked then
                ConfigFile.writestring('SyncSettings', 'SyncOption', 'UseLocal')
            else if RadioUseServer.Checked then
                 ConfigFile.writestring('SyncSettings', 'SyncOption', 'UseServer');

            // We don't write UseFileSync anymore but remember it may still be there, should ignore
            if RadioFileSync.checked then
                ConfigFile.writestring('SyncSettings', 'SyncType', 'file')
            else ConfigFile.writestring('SyncSettings', 'SyncType', 'nextcloud');
            if (LabelFileSync.Caption = '') or (LabelFileSync.Caption = rsSyncNotConfig) then
                ConfigFile.writestring('SyncSettings', 'SyncRepo', '')
            else  ConfigFile.writestring('SyncSettings', 'SyncRepo', LabelFileSync.Caption);
            if (LabelNCSyncURL.Caption = '') or (LabelNCSyncURL.Caption = rsSyncNotConfig) then
                ConfigFile.writestring('SyncSettings', 'SyncRepoNCURL', '')
            else  ConfigFile.writestring('SyncSettings', 'SyncRepoNCURL', LabelNCSyncURL.Caption);
            if SpellConfig then begin
                ConfigFile.writestring('Spelling', 'Library', LabelLibrary.Caption);
                ConfigFile.writestring('Spelling', 'Dictionary', LabelDic.Caption);
            end;
        finally
    	    ConfigFile.Free;
        end;
    except on E: Exception do begin
            showmessage('Unable to write config to ' + LabelSettingPath.Caption);
            Result := False;
        end;
    end;
    // debugln('just wrote a settings file out');
end;

function TSett.GetDefaultNoteDir : string;
begin
    {$IFDEF UNIX}
    Result := GetEnvironmentVariable('HOME') + '/.local/share/tomboy-ng/';
    {$ENDIF}
    {$IFDEF DARWIN}
    // try the correct place first, if not there, lets try the old, wrong place
    // if at neither, we go back to correct place.
    Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-ng/Notes/';
    if DirectoryExistsUTF8(Result) then exit;
    Result := GetEnvironmentVariable('HOME') + '/.local/share/tomboy-ng/';
    if not DirectoryExistsUTF8(Result) then
        Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-ng/Notes/';
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := GetEnvironmentVariable('APPDATA') + '\tomboy-ng\notes\';
    // %APPDATA%\Tomboy\notes\
    {$ENDIF}
end;

procedure TSett.ButtDefaultNoteDirClick(Sender: TObject);
begin
    NoteDirectory := GetDefaultNoteDir();
    if not CheckDirectory(NoteDirectory) then
        NoteDirectory := Sett.LabelNotesPath.Caption
    else begin
        SettingsChanged();
        SyncSettings();
        SearchForm.IndexNotes();
        //NeedRefresh := True;
    end;
end;

procedure TSett.SetColours;
begin
    if UserSetColours then exit;        // will have already been set by config or by colour form.
    if DarkTheme then begin
        //debugln('Its definltly a Dark Theme');
        BackGndColour:= clBlack;
        HiColour := clDkGray;
        TextColour := clLtGray;
        TitleColour:= clTeal;
    end else begin
        BackGndColour := clCream;
        HiColour := clYellow;
        TextColour := clBlack;
        TitleColour := clBlue;
    end;
end;

procedure TSett.ButtonSetColoursClick(Sender: TObject);
begin
    FormColours.CBack   := BackGndColour;
    FormColours.CHiBack := HiColour;
    FormColours.CText   := TextColour;
    FormColours.CTitle  := TitleColour;
    case FormColours.ShowModal of
        mrRetry  :  begin
                        UserSetColours := False;
                        SetColours();
                        SettingsChanged();
                    end;
        mrOK     :  begin
	                    BackGndColour := FormColours.CBack;
	                    HiColour := FormColours.CHiBack;
	                    TextColour := FormColours.CText;
	                    TitleColour := FormColours.CTitle;
                         UserSetColours := True;
                        SettingsChanged();
                    end;
//        mrCancel : showmessage('Do nothing');
	end;
end;

procedure TSett.ButtonFixedFontClick(Sender: TObject);
begin
    FontDialog1.Font.Name := FixedFont;
    FontDialog1.Font.Size := 10;
    FontDialog1.Title := 'Select Fixed Spacing Font';
    FontDialog1.PreviewText:= 'abcdef ABCDEF 012345';
    // showmessage(FixedFont);
    FontDialog1.Options := FontDialog1.Options + [fdFixedPitchOnly];
    If FontDialog1.Execute then BEGIN
        FixedFont := FontDialog1.Font.name;
        SettingsChanged();
    end;
    ButtonFixedFont.Hint := FixedFont;
end;

procedure TSett.ButtonFontClick(Sender: TObject);
begin
    FontDialog1.Font.Name := UsualFont;
    FontDialog1.Font.Size := 10;
    FontDialog1.Title := 'Select Usual Font';
    FontDialog1.PreviewText:= 'abcdef ABCDEF 012345';
    If FontDialog1.Execute then BEGIN
        UsualFont := FontDialog1.Font.name;
        SettingsChanged();
    end;
    ButtonFont.Hint := UsualFont;
end;

procedure TSett.ButtonHelpNotesClick(Sender: TObject);
begin
    FormHelpNotes.show;
end;



RESOURCESTRING
    rsDirHasNoNotes = 'That directory does not contain any notes. Thats OK, if I can make my own there.';

	{ Allow user to point to what they want to call their notes dir. If there
      are no notes there, pops up a warning and proceeds. }
procedure TSett.ButtonSetNotePathClick(Sender: TObject);
var
    Info : TSearchRec;
begin
	if SelectDirectoryDialog1.Execute then begin
		NoteDirectory := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
        if CheckDirectory(NoteDirectory) then begin
            if not FindFirst(NoteDirectory + '*.note', faAnyFile and faDirectory, Info)=0 then begin
               showmessage(rsDirHasNoNotes);
		    end;
            FindClose(Info);
            CheckShowIntLinks.enabled := true;
            // CheckReadOnly.enabled := true;
            SettingsChanged();
            SyncSettings();
            SearchForm.IndexNotes();
        end else
            NoteDirectory := LabelNotesPath.caption;
	end;
end;

{ --------------------- S N A P S H O T S ------------------- }
{ Totally unvalidated rule of thumb -
  About a 100 notes = ~ 1Gbytes, we get about 4:1 compression with zipper.
  120ms on lowend laptop.
}

RESOURCESTRING
    rsSnapshotCreated = 'created, do you want to copy it elsewhere ?';
    rsErrorCopyFile = 'Failed to copy file, does destination dir exist ?';

procedure TSett.ButtonManualSnapClick(Sender: TObject);
var
   FR : TFormRecover;
   FullName : string;
begin
    FR := TFormRecover.Create(self);
    try
        FR.NoteDir := NoteDirectory;
        FR.SnapDir := LabelSnapDir.Caption;
        FR.ConfigDir:= AppendPathDelim(Sett.LocalConfig);
        FullName := FR.CreateSnapshot(True, False);
        if mrYes = QuestionDlg('Snapshot created', FullName + ' ' + rsSnapShotCreated
                    , mtConfirmation, [mrYes, mrNo], 0) then
            if SelectSnapDir.Execute then
                if not CopyFile(FullName, TrimFilename(SelectSnapDir.FileName + PathDelim) + ExtractFileNameOnly(FullName) + '.zip') then
                    showmessage(rsErrorCopyFile + ' ' + TrimFilename(SelectSnapDir.FileName + PathDelim) + ExtractFileNameOnly(FullName) + '.zip');
    finally
        FR.Free;
    end;
end;



procedure TSett.ButtonSetSnapDirClick(Sender: TObject);
begin
    SelectSnapDir.FileName := LabelSnapDir.Caption;
    if SelectSnapDir.Execute then begin
		LabelSnapDir.Caption := TrimFilename(SelectSnapDir.FileName + PathDelim);
    end;
    CheckDirectory(LabelSnapDir.Caption);
end;

procedure TSett.ButtonSnapRecoverClick(Sender: TObject);
var
   FR : TFormRecover;
begin
    FR := TFormRecover.Create(self);
    try
        FR.NoteDir := NoteDirectory;
        FR.SnapDir := LabelSnapDir.Caption;
        FR.ConfigDir:= AppendPathDelim(Sett.LocalConfig);
        // Danger Will Robertson ! We cannot assume LocalConfig has a trailing slash !
        FR.Showmodal;
        if FR.RequiresIndex then
            SearchForm.IndexNotes();
    finally
        FR.Free;
    end;
end;

procedure TSett.ButtonSyncHelpClick(Sender: TObject);
begin
    MainForm.ShowHelpNote('sync-ng.note');
end;



procedure TSett.CheckAutostartChange(Sender: TObject);
var
   Auto : TAutoStartCtrl;
begin
    // This is being called at startup, it should only be called when user changes it.
    if not visible then exit;
     Auto := TAutoStartCtrl.Create('tomboy-ng', CheckAutostart.Checked);
     if Auto.ErrorMessage <> '' then
        ShowMessage('Error setting autstart' + Auto.ErrorMessage);
     FreeAndNil(Auto);
     CheckReadOnlyChange(Sender);
end;

procedure TSett.CheckBoxAutoSyncChange(Sender: TObject);
begin
    if LabelFileSync.Caption = '' then
       CheckBoxAutoSync.Checked:= false
    else if CheckBoxAutoSync.Checked then
        CheckAutoSync();
    CheckReadOnlyChange(Sender);
end;


{ ------------------------ S Y N C -------------------------- }

procedure TSett.Synchronise();
begin
    FormSync.NoteDirectory := Sett.NoteDirectory;
    FormSync.LocalConfig := AppendPathDelim(Sett.LocalConfig);

    FormSync.SetupSync := False;

    if FormSync.busy or FormSync.Visible then       // busy should be enough but to be sure ....
        FormSync.Show
    else
        FormSync.ShowModal;
end;

procedure TSett.CheckAutoSync();
begin
    if CheckBoxAutoSync.Checked and (LabelFileSync.Caption <> '') then begin
        TimerAutoSync.Interval:= 15000;     // wait 15 seconds after indexing to allow settling down
        TimerAutoSync.Enabled := true;
    end else
        if Application.HasOption('s', 'debug-sync') then
            debugln('Cannot run auto sync right now');
end;

procedure TSett.TimerAutoSyncTimer(Sender: TObject);
begin
    TimerAutoSync.enabled := False;
    if not SyncOK then exit;
    if CheckBoxAutoSync.checked  and not FormSync.Busy then begin
        FormSync.NoteDirectory := Sett.NoteDirectory;
        FormSync.LocalConfig := AppendPathDelim(Sett.LocalConfig);
        if (RadioFileSync.Checked) then
            FormSync.Transport:=TSyncTransport.SyncFile
        else {if(Sett.SyncNC.Checked)
                    then } FormSync.Transport:=TSyncTransport.SyncNextCloud;

        FormSync.SetupSync := False;

        if FormSync.RunSyncHidden() then begin
            TimerAutoSync.Interval:= 60*60*1000;     // do it again in one hour
            TimerAutoSync.Enabled := true;
        end;
    end;
end;


RESOURCESTRING
    rsDoubleclickNote = 'double click a note ...';

procedure TSett.ButtonShowBackUpClick(Sender: TObject);
var
	NoteLister : TNoteLister;
begin
    NoteLister := TNoteLister.Create;
    NoteLister.WorkingDir:= NoteDirectory + 'Backup' + PathDelim;
    NoteLister.GetNotes();
    NoteLister.LoadStGrid(StringGridBackUp, 4);
    NoteLister.Free;
    StringgridBackUp.AutoSizeColumns;
    Label15.caption := rsDoubleClickNote;
end;

procedure TSett.ButtonSnapDaysClick(Sender: TObject);
begin
    PopupDay.PopUp();
    TimeEdit1.Time := now();
end;

	{ Called when ANY of the setting check boxes change so we can save. }
procedure TSett.CheckReadOnlyChange(Sender: TObject);
begin
    SettingsChanged();      // Write to disk
    SyncSettings();
    if not MaskSettingsChanged then
        if Sender.ClassNameIs('TCheckBox') then begin
            if TCheckBox(Sender).Name = 'CheckShowTomdroid' then begin
                SearchForm.RefreshMenus(mkFileMenu);
                SearchForm.RefreshMenus(mkHelpMenu);
            end;
            if TCheckBox(Sender).Name = 'CheckCaseSensitive' then begin
                SearchForm.CheckCaseSensitive.Checked := TCheckBox(Sender).Checked;
            end;
    end;
end;

function TSett.GetLocalTime: ANSIstring;
	    // The retuned date string includes four digits at the end representing a count
	    // of 100 picoSeconds units. We cannot get that sort of precision and who needs it but
	    // I have realised as tomboy-ng uses the datestring as a key to check that notes
	    // are identical during a blind sync.  So, instead of making those four digits 0000
	    // I will add a random number, not significent for timing but a usefull increase
	    // in certaintly.
var
   ThisMoment : TDateTime;
   Res : ANSIString;
   Off : longint;
   PicoSeconds : string;
begin
    {$ifdef LINUX}
    ReReadLocalTime();    // in case we are near daylight saving time changeover
    {$endif}
    ThisMoment:=Now;
    PicoSeconds := inttostr(random(9999));
    while length(PicoSeconds) < 4 do PicoSeconds := '0' + PicoSeconds;
    Result := FormatDateTime('YYYY-MM-DD',ThisMoment) + 'T'
                    // + FormatDateTime('hh:mm:ss.zzz"0000"',ThisMoment);
                    + FormatDateTime('hh:mm:ss.zzz',ThisMoment) + PicoSeconds;
    Off := GetLocalTimeOffset();
    if (Off div -60) >= 0 then Res := '+'
	else Res := '-';
	if abs(Off div -60) < 10 then Res := Res + '0';
	Res := Res + inttostr(abs(Off div -60)) + ':';
       	if (Off mod 60) = 0 then
		Res := res + '00'
	else Res := Res + inttostr(abs(Off mod 60));
    Result := Result + res;
end;

end.

