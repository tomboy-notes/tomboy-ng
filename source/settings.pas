unit settings;
{    Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

	This form represents all the settings and will have most (?) of the constants.

	Makes sense for all units to 'use' this unit, ideally in the implmentation
	section.
    Launcher for Sync Engines, Backup, Snapshot RollBack systems.
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
    2020/04/04  Don't run autosync in singlenote mode.
    2020/05/11  Moved all handling of the backup files to BackupView
    2020/06/11  check if snapshot ok before flushing old ones.
    2020/06/18  Ensure a default config file is written asap at first start.
    2020/06/18  Removed unnecessary panel on Snap tab
    2020/07/09  New help notes location.
    2020/07/16  Drop Backup tab, merge to Snapshot tab, renamed 'Recover'
    2020/07/24  Moved HELP notes from /usr/share/doc/tomboy-ng to /usr/share/tomboy-ng to suit debian
    2020/08/01  Show better labels in HelpLangCombo.
    2021/01/23  Save Search Auto Refresh check box status.
    2021/04/24  Added setting to enable/disable undo/redo
    2021/05/01  Remove HaveConfig and restructured config startup
    2021/06/01  Add setting to disable Notifications
    2021/09/27  Allow both File and Github sync, maybe its a good idea ??  SelectiveSync.
    2021/10/06  Restructured the way we enter GH Token, all copy and paste now.
    2021/10/26  User selectable date stamp format
    2021/12/03  Rearranged checkboxes to accomodate Searching While u Wait.
    2022/01/21  GTK3 determines what is a monospace font differently.
    2022/01/31  Added a CheckFindToggles to determine if every press of Ctrl-f invokes Find or toggles it on or off.
    2022/03/31  Tidyed up the Github Token controls, now can invoke browser.
    2022/09/02  Removed AutoRefreshSearch checkbox, its the SearchUnit's problem.
    2022/10/21  CheckAutoStart must call its own method to trigger writing files
    2023/01/14  Save Auto Snapshot settings
    2023/02/21  Drop Monospace font to last of priority, its not a real font.
    2023/03/11  Make a bool to indicate Qt is in charge of its colours, eg QT_QPA_PLATFORMTHEME
    2023/03/18  Ensure AltColour and AltBackGndColor are set to something in user defined scheme
}

{$mode objfpc}{$H+}                    //

interface

uses
    Classes, SysUtils, {FileUtil,} Forms, Controls, Graphics, Dialogs, StdCtrls,
    Buttons, ComCtrls, ExtCtrls, Menus, FileUtil, BackUpView,
    LCLIntf, Spin{, notifier}, base64, fpttf;

// Types;

type TSyncOption = (AlwaysAsk, UseServer, UseLocal); 	// Relating to sync clash pref in config file

type

    { TSett }

    TSett = class(TForm)
        ButtDefaultNoteDir: TButton;
        ButtonManualSnap: TButton;
        ButtonShowBackUp: TButton;
        ButtonSnapRecover: TButton;
        CheckAutoSnapEnabled: TCheckBox;
        CheckEscClosesNote: TCheckBox;
        CheckFindToggles: TCheckBox;
        CheckStampBold: TCheckBox;
        CheckStampItalics: TCheckBox;
        CheckStampSmall: TCheckBox;
        CheckSyncEnabled: TCheckBox;
		CheckNotifications: TCheckBox;
        CheckUseUndo: TCheckBox;
        CheckBoxAutoSync: TCheckBox;
        ComboDateFormat: TComboBox;
        ComboSyncType: TComboBox;
        ComboHelpLanguage: TComboBox;
        EditUserName: TEdit;
        GroupBoxUser: TGroupBox;
        GroupBoxToken: TGroupBox;
        GroupBoxSync: TGroupBox;
        Label10: TLabel;
        Label11: TLabel;
        Label16: TLabel;
        Label17: TLabel;
        LabelSyncType: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        LabelSyncInfo2: TLabel;
        Label4: TLabel;
        LabelSyncInfo1: TLabel;
        LabelSyncRepo: TLabel;
        ButtonSetColours: TButton;
        ButtonFixedFont: TButton;
        ButtonFont: TButton;
        ButtonSetSpellLibrary: TButton;
        ButtonSetDictionary: TButton;

        ButtonSetNotePath: TButton;
        CheckAutoStart : TCheckBox;
        CheckManyNotebooks: TCheckBox;
        CheckShowSearchAtStart: TCheckBox;
        CheckShowSplash: TCheckBox;
        CheckShowExtLinks: TCheckBox;
        CheckShowIntLinks: TCheckBox;
        FontDialog1: TFontDialog;
        GroupBox4: TGroupBox;
        GroupBox5: TGroupBox;
        Label1: TLabel;
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
        Label2: TLabel;
        LabelNotesPath: TLabel;
        LabelSettingPath: TLabel;
        LabelSnapDir: TLabel;
        LabelToken: TLabel;
        ListBoxDic: TListBox;
        MenuItemGetToken: TMenuItem;
        MenuItemPasteToken: TMenuItem;
        MenuItemCopyToken: TMenuItem;
        OpenDialogLibrary: TOpenDialog;
        OpenDialogDictionary: TOpenDialog;
        PageControl1: TPageControl;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        PMenuMain: TPopupMenu;
        PopupMenuTokenActions: TPopupMenu;
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
        SpeedTokenActions: TSpeedButton;
        SpeedButtTBMenu: TSpeedButton;
        SpeedSetupSync: TSpeedButton;
        SpinDaysPerSnapshot: TSpinEdit;
        SpinMaxSnapshots: TSpinEdit;
        TabBasic: TTabSheet;
        TabBackUp: TTabSheet;
        TabSpell: TTabSheet;
        TabRecover: TTabSheet;
        TabSync: TTabSheet;
        TabDisplay: TTabSheet;
        TimerAutoSync: TTimer;

        procedure ButtDefaultNoteDirClick(Sender: TObject);
        procedure ButtonSetColoursClick(Sender: TObject);
        procedure ButtonFixedFontClick(Sender: TObject);
        procedure ButtonFontClick(Sender: TObject);
        procedure ButtonManualSnapClick(Sender: TObject);
        procedure ButtonSetDictionaryClick(Sender: TObject);
        procedure ButtonSetNotePathClick(Sender: TObject);
        procedure ButtonSetSnapDirClick(Sender: TObject);
        procedure ButtonSetSpellLibraryClick(Sender: TObject);
        procedure ButtonShowBackUpClick(Sender: TObject);
        procedure ButtonSnapRecoverClick(Sender: TObject);
        procedure CheckAutoSnapEnabledChange(Sender: TObject);
        procedure CheckAutostartChange(Sender: TObject);
        procedure CheckBoxAutoSyncChange(Sender: TObject);
        procedure MenuItemCopyTokenClick(Sender: TObject);
        procedure MenuItemGetTokenClick(Sender: TObject);
        procedure MenuItemPasteTokenClick(Sender: TObject);
                { Called when ANY of the setting check boxes change so we can save. }
        procedure SaveSettings(Sender: TObject);
        procedure CheckSyncEnabledChange(Sender: TObject);
        procedure ComboHelpLanguageChange(Sender: TObject);
        procedure ComboSyncTypeChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
        procedure SpeedTokenActionsClick(Sender: TObject);
        procedure SpinDaysPerSnapshotChange(Sender: TObject);
        procedure TabBasicResize(Sender: TObject);
        procedure TabRecoverResize(Sender: TObject);
        procedure TabSpellResize(Sender: TObject);
        procedure TimerAutoSyncTimer(Sender: TObject);
                        // Sets default colours, depending on dark or light theme
                        // Called from MainForm.ShowForm
        procedure SetColours;

    private
        SyncFileEnabled, SyncGithubEnabled: boolean;
        SyncFileAuto, SyncGithubAuto: boolean;
                        { eg  /run/user/1000/gvfs/smb-share=greybox,share=store2/TB_Sync/
                        Being non empty indicates it worked sometime, ie is Valid }
        SyncFileRepo : string;
                        { eg https://github.com/davidbannon/tb_test, being not empty means it was, at one stage, valid}
        SyncGithubRepo : string;
        AutoRefreshVar : boolean;
        AutoSearchUpdateVar : boolean;          // SWYT - search while you type
        UserSetColours : boolean;
        fExportPath : ANSIString;
        SearchIsCaseSensitive : boolean;
        NextAutoSnapshot : TDateTime;
                        // Sets some default colours (find better way) and sets Colour Button hint.
        procedure CheckUserColours;
                        // Looks in expected place for help notes, populate combo and public vars, HelpNotesPath, HelpNotesLang.
        procedure LoadHelpLanguages();
                        // We load settings from confile or, if not available, sensible defaults, save.
                        // then go on to check and make if necessary, other needed directories.
                        // We proceed even if we cannot save settings, after all, user has been warned.
 	    procedure CheckConfigAndDirs;
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
        procedure DicDefaults(out DicPathAlt: string);
        procedure DoAutoSnapshot;
                            // Returns a good place to save config or user requested place if on cmdline,
        //function GetDefaultConfigDir: string;

                            // Returns the default place to store notes. It may not be present.
        function GetDefaultNoteDir: string;
                            // Has a list of possible fixed font names, returns the first that 'works'.
        function GetFixedFont(): string;
        function MyBoolStr(const InBool: boolean) : string;
		procedure ReadConfigFile;

        procedure SetFontSizes;
                            // Uses value of HelpNotesLang to make the Combobox agree.
        procedure SetHelpLanguage();
                            // Saves all current settings to disk. Call when any change is made. If unable
                            // to write to disk, returns False, If IgnoreMask, writes even if masked.
        function WriteConfigFile(IgnoreMask : boolean = false): boolean;
		function fGetValidSync: boolean;
                // Must be passed either a valid sync repo address, rsSyncNotConfig or ''
        //procedure fSetValidSync(Repo: string);

                            // Sets AutoRefresh and triggers a write of config file
        procedure fSetAutoRefresh(AR : boolean);
                            // Just returns AutoRefresh
        function  fGetAutoRefresh() : boolean;

        procedure fSetAutoSearchUpdate(ASU : boolean);  // sets and then triggers config write
        function  fGetAutoSearchUpdate(): boolean;      // just set AutoSearchUpdateVar

		procedure SyncSettings;
        function fGetCaseSensitive : boolean;
        procedure fSetCaseSensitive(IsIt : boolean);
        //function ZipDate: string;

    public
        HelpNotesPath : string;         // expected path to help note directories for this OS
        HelpNotesLang : string;         // either two char code or ''
        AreClosing : boolean;           // False until set true by mainUnit FormClose.
        BackGndColour : TColor;
        AltBackGndColor : TColor;       // When selected Text looses focus
        TextColour : TColor;
        HiColour : TColor;
        TitleColour : TColor;
        AltColour   : TColor;           // A colour similar to  BackGndColour, alt rows in ListView, buttons in dark mode ?
        LinkColour  : TColor;
        UsualFont : string;
        FixedFont : string;
        DefaultFixedFont : string;
        DarkThemeSwitch : boolean;      // Dark Theme because user provided --dark-theme, set in main unit.
        DarkTheme : boolean;            // Dark Theme because we detected it ourselves. Set by main unit.
        QtOwnsColours : boolean;        // Qt[5,6] is in charge of its own colours, probably using QT_QPA_PLATFORMTHEME, but not for kmemo
        DebugModeSpell : boolean;
        // Indicates SettingsChanged should not write out a new file cos we are loading from one.
        MaskSettingsChanged : boolean;
        AllowClose : Boolean;           // review need for this
        // Indicates we should re-index notes when form hides
        //NeedRefresh : Boolean;
        FontSmall  : Integer;
     	FontLarge  : Integer;
     	FontHuge   : Integer;
     	FontTitle  : Integer;			// Do not set this to one of the other sizes !
     	FontNormal : Integer;
        { The directory expected to hold existing or new notes }
        NoteDirectory : string;
        { The dir expected to hold config file and, possibly local manifest }
        LocalConfig : string;
        SyncOption : TSyncOption;
        { Indicates user wants to see internal links }
        ShowIntLinks : boolean;
        { Says Notes should be treated as read only, a safe choice }
        NotesReadOnly : boolean;
            { Indicates Spell is configured and LabelLibrary and LabelDic should
            contain valid full file names.}
        SpellConfig : boolean;

            { Triggers a Sync, if its not all setup aready and working, user show and error }
        procedure Synchronise();

        property ValidSync : boolean read fGetValidSync;
        property SearchCaseSensitive : boolean read fGetCaseSensitive write fSetCaseSensitive;

                            // Property that triggers write of config when set, reads, sets
        property AutoRefresh : boolean read fGetAutoRefresh write fSetAutoRefresh;
                            // Property SWYT - search while you type
        property AutoSearchUpdate : boolean read fgetAutoSearchUpdate write fSetAutoSearchUpdate;

                            // Does not appear to be implemented
        property ExportPath : ANSIString Read fExportPath write fExportPath;
                            // Called after notes are indexed (from SearchUnit), will start auto timer tha
                            // controls both AutoSync and AutoSnap. Does nothing in SingleNoteMode.
        procedure StartAutoSyncAndSnap();
                            // Public : Returns the SyncFileRepo unless its empty, in which case its
                            // returns unusable rubbish to ensure sync aborts.  However, a special case
                            // applies when setting up, SyncFileRepo will be empty but ComboSyncType set
                            // to ItemsIndex=0 (being file sync) and a valid URL will be in LabelSyncRepo.
        function GetSyncFileRepo() : string;
    end;

var
    Sett : TSett;

const
    Placement = 45;				// where we position an opening window. Its, on average, 1.5 time Placement;



implementation

{$R *.lfm}

//{$DEFINE TESTAUTOSNAP}
{$define DEBUG}

{ TSett }


uses IniFiles, LazLogger,
    LazFileUtils,   // LazFileUtils needed for TrimFileName(), cross platform stuff;
    SearchUnit,		// So we can call IndexNotes() after altering Notes Dir
    syncGUI,
    syncutils,
    tb_utils,
    recover,        // Recover lost or damaged files
    mainunit,       // so we can call ShowHelpNote()
    hunspell,       // spelling check
    LCLType,        // Keycodes ....
    Autostart,
    Colours,
    Clipbrd,
    tb_symbol,
    uQt_Colors,
    ResourceStr;     // only partially so far ....

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
        FontTitle  := 21;			// Do not set this to one of the other sizes !
        FontNormal := 16;
    end;
	if RadioFontBig.checked then begin
    	FontSmall  := 9;
     	FontLarge  := 17;
     	FontHuge   := 20;
     	FontTitle  := 18;			// Do not set this to one of the other sizes !
     	FontNormal := 14;
	end;
    if RadioFontMedium.checked then begin
    	FontSmall  := 8;
 		FontLarge  := 14;
 		FontHuge   := 18;
 		FontTitle  := 16;			// Do not set this to one of the other sizes !
 		FontNormal := 11;
	end;
    if RadioFontSmall.Checked then begin
    	FontSmall  := 7;
 		FontLarge  := 13;
 		FontHuge   := 16;
 		FontTitle  := 14;			// Do not set this to one of the other sizes !
 		FontNormal := 10;
    end;

end;

	{ Make public things agree with internal ones. }
procedure TSett.SyncSettings;
begin
	if NoteDirectory <> '' then begin
        LabelNotespath.Caption := NoteDirectory;
        ShowIntLinks := CheckShowIntLinks.Checked;
        SetFontSizes();
	    if RadioAlwaysAsk.Checked then SyncOption := AlwaysAsk
        else if RadioUseLocal.Checked then SyncOption := UseLocal
        else if RadioUseServer.Checked then SyncOption := UseServer;
	end;
end;

// -------------------------------- Search Settings ----------------------------
function TSett.fGetCaseSensitive : boolean;
begin
    result := SearchIsCaseSensitive;
end;

procedure TSett.fSetCaseSensitive(IsIt : boolean);
begin
    SearchIsCaseSensitive := IsIt;
    if Not MaskSettingsChanged then
        WriteConfigFile();
end;

procedure TSett.PageControl1Change(Sender: TObject);
begin
	if NoteDirectory = '' then ButtDefaultNoteDirClick(self);
    Label15.Caption := '';
    SpeedButHelp.Visible := (PageControl1.TabIndex = 2);    // Only show for Sync Tab
end;

procedure TSett.SpeedButHelpClick(Sender: TObject);
begin
    SearchForm.ShowHelpNote('sync-ng.note');
end;

procedure TSett.SpeedButHideClick(Sender: TObject);
begin
    Hide;
end;

procedure TSett.SpeedButtTBMenuClick(Sender: TObject);
begin
    PMenuMain.Popup;
end;





procedure TSett.TabBasicResize(Sender: TObject);
begin
    buttonSetNotePath.Width := (TabBasic.Width div 2) - 12;
end;

procedure TSett.TabRecoverResize(Sender: TObject);
begin
    ButtonManualSnap.Width :=  (TabRecover.Width div 2) -10;
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
               WriteConfigFile();
               Result := True;
            end else begin
                LabelDicStatus.Caption := rsDictionaryNotFound;
            end;
        end;
    end else debugln('ERROR - called CheckDictionary with Spell nil');
    if DebugModeSpell then debugln('CheckDictionary ' + FullDicName + ' return ' + booltostr(Result, True));
end;

procedure TSett.DicDefaults(out DicPathAlt : string);
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
    DicPathAlt : string = '';
    DicToCheck : string;
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
end;

procedure TSett.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if {$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif} in Shift then begin
      if key = ord('N') then begin SearchForm.OpenNote(''); Key := 0; exit(); end;
      if key = VK_Q then MainForm.Close();
    end;
end;

procedure TSett.FormShow(Sender: TObject);
begin
    if not assigned(Spell) then
        Spell := THunspell.Create(Application.HasOption('debug-spell'), LabelLibrary.Caption);
        // user user has 'closed' (ie hide) then Spell was freed.
    MaskSettingsChanged := False;
    Label15.Caption:='';
    CheckUserColours;
end;

// We only really close when told by RTSearch that The Exit Menu choice from TrayIcon was clicked.
procedure TSett.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	if AllowClose then begin
    	CloseAction := caFree;
        SearchForm.Close;
	end else CloseAction := caHide;
end;

procedure TSett.FormCreate(Sender: TObject);
var
    i : integer;
begin
    gTTFontCache.ReadStandardFonts;         // needed by PDF export and, at some stage, new font selection window.
    Caption := 'tomboy-ng Settings';
    AreClosing := false;
    Top := 100;
    Left := 300;
    LoadHelpLanguages();
    ComboDateFormat.Items.Clear;
    for i := 0 to MaxDateStampIndex do
        ComboDateFormat.Items.add(TB_DateStamp(i));
    ComboDateFormat.ItemIndex := 0;

    DefaultFixedFont := GetFixedFont();     // Tests a list of likely suspects.
    PageControl1.ActivePage := TabBasic;
    MaskSettingsChanged := true;            // don't trigger save while doing setup
    ExportPath := '';
    LabelLibrary.Caption := '';
    //HaveConfig := false;
    LocalConfig := TB_GetDefaultConfigDir();   // sys dependant unless user has overridden
    LabelSettingPath.Caption := LocalConfig + 'tomboy-ng.cfg';
    NoteDirectory := Sett.GetDefaultNoteDir;
    labelNotesPath.Caption := NoteDirectory;
    CheckConfigAndDirs();                      // write a new, default one if necessary
    CheckSpelling();
    ComboSyncType.ItemIndex := 0;               // Defaults to File
    if SyncGithubRepo <> '' then
        ComboSyncType.ItemIndex := 1;           // But if we have Github, show that, more going on
    ComboSyncTypeChange(self);
    //LabelSyncInfo1.Caption := rsFileSyncInfo1;         // ToDo : is this done in ComboSyncTypeChange
    //LabelSyncInfo2.Caption := rsFileSyncInfo2;
    MaskSettingsChanged := False;
end;

procedure TSett.FormDestroy(Sender: TObject);
begin
    FreeandNil(Spell);
end;

{ ------------------------- F I L E    I / O --------------------------- }

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

(*            // This has been moved to TB_Utils
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
    // MainForm.SetAltHelpPath(Result);    // English help notes in read only space
    {$endif}
end; *)

function TSett.GetFixedFont() : string;
var  T : string;
    FontNames : array[1..9] of string
      = ('Ubuntu Mono', 'Monaco', 'Liberation Mono',  'Nimbus Mono PS', 'Nimbus Mono L', 'Lucida Console', 'Lucida Sans Typewriter', 'Courier New',  'Monospace');
    // Add as many new names as you like but set array size.  Chooses the first in the list it finds that works
    // Label does not seem to worry about us playing with its canvas.

    function IsMono(FontName : String) : boolean;
    begin
      Label1.Canvas.Font.Name := FontName;
      {$ifdef LCLGTK3}
      // Might work on Linux generally ?
      // https://forum.lazarus.freepascal.org/index.php/topic,20193.msg194575.html
      Result := Label1.Canvas.Font.IsMonoSpace;
      {$else}
      result := Label1.Canvas.TextWidth('i') = Label1.Canvas.TextWidth('w');
      {$endif}
      // That line above triggers a whole load of GTK3 error messages.

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




    // Will read and apply the config file if available, else sets sensible defaults
    // Is only called at startup and assumes the config dir has been checked and
    // LabelSettingPath.Caption contains an appropriate file name.
procedure TSett.ReadConfigFile;
var
   ConfigFile : TINIFile;
   i : integer;
   Uch : String[10];
begin
    // TiniFile does not care it it does not find the config file, just returns default values.
     ConfigFile :=  TINIFile.Create(LabelSettingPath.Caption);
     try
   	    NoteDirectory := ConfigFile.readstring('BasicSettings', 'NotesPath', NoteDirectory);
        CheckShowIntLinks.Checked :=
            ('true' = ConfigFile.readstring('BasicSettings', 'ShowIntLinks', 'true'));
        CheckShowExtLinks.Checked :=
            ('true' = ConfigFile.readstring('BasicSettings', 'ShowExtLinks', 'true'));
        CheckManyNoteBooks.checked :=
    	    ('true' = Configfile.readstring('BasicSettings', 'ManyNotebooks', 'true'));
        CheckUseUndo.Checked :=
            ('true' = ConfigFile.readstring('BasicSettings', 'UseUndo', 'true'));
        SearchCaseSensitive :=
            ('true' = Configfile.readstring('BasicSettings', 'CaseSensitive', 'false'));
        CheckShowSplash.Checked :=
            ('true' = Configfile.ReadString('BasicSettings', 'ShowSplash', 'true'));
        CheckAutostart.Checked :=
            ('true' = Configfile.ReadString('BasicSettings', 'Autostart', 'false'));
        CheckShowSearchAtStart.Checked :=
            ('true' = Configfile.ReadString('BasicSettings', 'ShowSearchAtStart', 'false'));
        CheckNotifications.Checked :=
            ('true' = Configfile.ReadString('BasicSettings', 'ShowNotifications', 'true'));
        CheckEscClosesNote.Checked :=
            ('true' = Configfile.ReadString('BasicSettings', 'EscClosesNote', 'false'));
        case ConfigFile.readstring('BasicSettings', 'FontSize', 'medium')  of
            'huge'   : RadioFontHuge.Checked := true;
    	    'big'    : RadioFontBig.Checked := true;
            'medium' : RadioFontMedium.Checked := true;
            'small'  : RadioFontSmall.Checked := true;
        end;
        AutoRefresh := ('true' = ConfigFile.readstring('BasicSettings', 'AutoRefresh', 'true'));
        AutoSearchUpdate := ('true' = ConfigFile.readstring('BasicSettings', 'AutoSearchUpdate', 'true'));
        CheckFindToggles.Checked :=
            ('true' = Configfile.ReadString('BasicSettings', 'FindToggles', 'true'));

        // ------------------- F O N T S ----------------------
        UsualFont := ConfigFile.readstring('BasicSettings', 'UsualFont', GetFontData(Self.Font.Handle).Name);
        ButtonFont.Hint := UsualFont;
        FixedFont := ConfigFile.readstring('BasicSettings', 'FixedFont', DefaultFixedFont);
        if FixedFont = '' then FixedFont := DefaultFixedFont;
        ButtonFixedFont.Hint := FixedFont;
        // ------------------- C O L O U R S -------------------
        BackGndColour:=   StringToColor(Configfile.ReadString('BasicSettings', 'BackGndColour', '0'));
        HiColour    := StringToColor(Configfile.ReadString('BasicSettings', 'HiColour',    '0'));
        TextColour  := StringToColor(Configfile.ReadString('BasicSettings', 'TextColour',  '0'));
        TitleColour := StringToColor(Configfile.ReadString('BasicSettings', 'TitleColour', '0'));
        LinkColour  := StringToColor(Configfile.ReadString('BasicSettings', 'LinkColour',  '0'));
        UserSetColours := not ((BackGndColour = 0) and (HiColour = 0) and (TextColour = 0) and (TitleColour = 0) and (LinkColour = 0));
        // Note - '0' is a valid colour, black. So, what says its not set is they are all '0';
        CheckUserColours;
        HelpNotesLang :=  Configfile.ReadString('BasicSettings', 'HelpLanguage', HelpNotesLang);
        SetHelpLanguage();

        // ------------------  S Y N C   S E T T I N G S --------------------------
        case ConfigFile.readstring('SyncSettings', 'SyncOption', 'AlwaysAsk') of
            'AlwaysAsk' : begin SyncOption := AlwaysAsk; RadioAlwaysAsk.Checked := True; end;
            'UseLocal'  : begin SyncOption := UseLocal;  RadioUseLocal.Checked  := True; end;
            'UseServer' : begin SyncOption := UseServer; RadioUseServer.Checked := True; end;
	    end;
        SyncFileRepo := ConfigFile.readstring('SyncSettings', 'SyncRepo', '');     // that is for file sync
        SyncGithubRepo := ConfigFile.readstring('SyncSettings', 'SyncRepoGithub', '');
        SyncFileAuto :=    ('true' = Configfile.ReadString('SyncSettings', 'Autosync', 'false'));
        SyncGithubAuto  := ('true' = Configfile.ReadString('SyncSettings', 'AutosyncGit', 'false'));
        SyncFileEnabled := ('true' = Configfile.ReadString('SyncSettings', 'FileSyncEnabled', 'false'));
        SyncGithubEnabled :=   ('true' = Configfile.ReadString('SyncSettings', 'GitSyncEnabled', 'false'));
        LabelToken.caption := DecodeStringBase64(Configfile.ReadString('SyncSettings', 'GHPassword', ''));
        EditUserName.text := Configfile.ReadString('SyncSettings', 'GHUserName', '');
        ComboSyncTypeChange(self);
        // remember that an old config file might contain stuff about Filesync, nextcloud, random rubbish .....

        // ------------- S P E L L I N G ---------------------------------------
        LabelLibrary.Caption := ConfigFile.readstring('Spelling', 'Library', '');
        LabelDic.Caption := ConfigFile.readstring('Spelling', 'Dictionary', '');
        SpellConfig := (LabelLibrary.Caption <> '') and (LabelDic.Caption <> '');     // indicates it worked once...
        // ------------- S N A P S H O T    S E T T I N G S  -------------------
        LabelSnapDir.Caption := ConfigFile.readstring('SnapSettings', 'SnapDir', NoteDirectory + 'Snapshot' + PathDelim);
        CheckAutoSnapEnabled.Checked := Configfile.ReadBool('Snapshot', 'AutoSnapEnabled', False);
        NextAutoSnapshot             := Configfile.ReadDateTime('Snapshot', 'NextAutoSnapshot', now());
        SpinDaysPerSnapshot.Value    := Configfile.ReadInteger('Snapshot', 'DaysPerSnapshot', 7);
        SpinMaxSnapshots.Value       := Configfile.ReadInteger('Snapshot', 'DaysMaxSnapshots', 20);
        // ------------- D A T E    S T A M P    S E T T I N G -----------------
        CheckStampItalics.Checked := Configfile.ReadBool('DateStampSettings', 'Italics', False);
        CheckStampSmall.Checked   := Configfile.ReadBool('DateStampSettings', 'Small', False);
        CheckStampBold.Checked    := Configfile.ReadBool('DateStampSettings', 'Bold', False);
        ComboDateFormat.ItemIndex := Configfile.ReadInteger('DateStampSettings', 'Format', 0);

        // ----------------- Symbols ---------------------------------------
        for i := 0 to high(SymArray) do begin
                Uch := ConfigFile.readstring('SymbolSettings', 'Symbol_' + inttostr(i), '');
                if Uch <> '' then
                    SymArray[i].SymStr := Uch;
        end;
        FormSymbol.UpdateSymbols();
                //ConfigFile.writestring('SymbolSettings', 'Symbol_' + inttostr(i), SymArray[i].SymStr);

    finally
        ConfigFile.free;
    end;
end;

{ Read config file if it exists or writes a default one. }
procedure TSett.CheckConfigAndDirs;
//var
//    ConfigFile : TINIFile;
begin
    CheckDirectory(LocalConfig);        // so its created if needed, shows message on error.
    ReadConfigFile();   // will ensure sensible default config even if file is not present.
    if LabelSettingPath.Caption = 'LabelSettingPath' then
            showmessage('WARNING, TSett.CheckConfigFile - writing config before setting filename')
    else if not fileexists(LabelSettingPath.Caption) then        // must be first run
        WriteConfigFile(True);                  // write a initial default file, shows user message on error

    CheckDirectory(NoteDirectory);              // user will get a message on error, their problem
    CheckDirectory(NoteDirectory + 'Backup');
    CheckDirectory(LabelSnapDir.Caption);
	SyncSettings();                             // ToDo : can we discard this yet ?
end;


procedure TSett.fSetAutoRefresh(AR: boolean);
begin
    AutoRefreshVar := AR;
    WriteConfigFile();
end;

function TSett.fGetAutoRefresh() : boolean;
begin
    Result := AutoRefreshVar;
end;

procedure TSett.fSetAutoSearchUpdate(ASU: boolean);
begin
    AutoSearchUpdateVar := ASU;
    WriteConfigFile();
end;

function TSett.fGetAutoSearchUpdate: boolean;
begin
    Result := AutoSearchUpdateVar;
end;

function TSett.MyBoolStr(const InBool : boolean) : string;
begin
    if InBool then result := 'true' else result := 'false';
end;

function TSett.WriteConfigFile(IgnoreMask : boolean = false) : boolean;
var
	ConfigFile : TINIFile;
    i : integer;
begin
    Result := True;
    if MaskSettingsChanged and (not IgnoreMask) then exit();
{    if LabelSettingPath.Caption = 'LabelSettingPath' then            // ToDo : I very occasionally create a file called LabelSettingPath, cannot reproduce
        showmessage('WARNING, TSett.SettingsChanged - writing config before setting filename');     }
    ConfigFile :=  TINIFile.Create(LabelSettingPath.Caption);
    try
        try
            ConfigFile.writestring('BasicSettings', 'NotesPath', NoteDirectory);
            Configfile.writestring('BasicSettings', 'ManyNotebooks',     MyBoolStr(CheckManyNoteBooks.checked));
            Configfile.writestring('BasicSettings', 'CaseSensitive',     MyBoolStr(SearchCaseSensitive));
            ConfigFile.writestring('BasicSettings', 'ShowIntLinks',      MyBoolStr(CheckShowIntLinks.Checked));
            ConfigFile.writestring('BasicSettings', 'ShowExtLinks',      MyBoolStr(CheckShowExtLinks.Checked));
            ConfigFile.WriteString('BasicSettings', 'ShowSplash',        MyBoolStr(CheckShowSplash.Checked));
            ConfigFile.WriteString('BasicSettings', 'Autostart',         MyBoolStr(CheckAutostart.Checked));
            ConfigFile.WriteString('BasicSettings', 'ShowSearchAtStart', MyBoolStr(CheckShowSearchAtStart.Checked));
            ConfigFile.WriteString('BasicSettings', 'ShowNotifications', MyBoolStr(CheckNotifications.Checked));
            ConfigFile.WriteString('BasicSettings', 'EscClosesNote',     MyBoolStr(CheckEscClosesNote.Checked));
            ConfigFile.WriteString('BasicSettings', 'AutoRefresh',       MyBoolStr(AutoRefresh));
            ConfigFile.WriteString('BasicSettings', 'UseUndo',           MyBoolStr(CheckUseUndo.Checked));
            ConfigFile.WriteString('BasicSettings', 'AutoSearchUpdate',  MyBoolStr(AutoSearchUpdate));
            ConfigFile.WriteString('BasicSettings', 'FindToggles',       MyBoolStr(CheckFindToggles.checked));
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
                ConfigFile.writestring('BasicSettings', 'HiColour',      ColorToString(HiColour));
                ConfigFile.writestring('BasicSettings', 'TextColour',    ColorToString(TextColour));
                ConfigFile.writestring('BasicSettings', 'TitleColour',   ColorToString(TitleColour));
                ConfigFile.writestring('BasicSettings', 'LinkColour',    ColorToString(LinkColour));
			end else begin
                ConfigFile.writestring('BasicSettings', 'BackGndColour', '0');
                ConfigFile.writestring('BasicSettings', 'HiColour',      '0');
                ConfigFile.writestring('BasicSettings', 'TextColour',    '0');
                ConfigFile.writestring('BasicSettings', 'TitleColour',   '0');
                ConfigFile.writestring('BasicSettings', 'LinkColour',    '0');
			end;
            if HelpNotesLang <> '' then
                ConfigFile.writestring('BasicSettings', 'HelpLanguage', HelpNotesLang);

            // --------- S Y N C    S E T T I N G S ----------------------------
            { Other entries, such as SyncRepoURL, SyncURL, FileSyncRepo, UseFileSync are distractions introduced by a nasty
            pull request I stupidly let through, ignore them, they will not go away unless manually deleted ! }

	        ConfigFile.WriteString('SyncSettings', 'Autosync',    MyBoolStr(SyncFileAuto));
            ConfigFile.WriteString('SyncSettings', 'AutosyncGit', MyBoolStr(SyncGithubAuto));
            ConfigFile.WriteString('SyncSettings', 'FileSyncEnabled', MyBoolStr(SyncFileEnabled));
            ConfigFile.WriteString('SyncSettings', 'GitSyncEnabled', MyBoolStr(SyncGithubEnabled));
	        if RadioAlwaysAsk.Checked then
                ConfigFile.writestring('SyncSettings', 'SyncOption', 'AlwaysAsk')
            else if RadioUseLocal.Checked then
                ConfigFile.writestring('SyncSettings', 'SyncOption', 'UseLocal')
            else if RadioUseServer.Checked then
                 ConfigFile.writestring('SyncSettings', 'SyncOption', 'UseServer');
            //ConfigFile.writestring('SyncSettings', 'SyncType', SyncType);         // Extend sync type here.
            ConfigFile.writestring('SyncSettings', 'SyncRepo', SyncFileRepo);
            ConfigFile.writestring('SyncSettings', 'SyncRepoGithub', SyncGithubRepo);
            ConfigFile.writestring('SyncSettings', 'GHPassword', EncodeStringBase64(LabelToken.Caption));
            ConfigFile.writestring('SyncSettings', 'GHUserName', EditUserName.text);

            // --------- S P E L L     S E T T I N G S ----------------------------
            if SpellConfig then begin
                ConfigFile.writestring('Spelling', 'Library', LabelLibrary.Caption);
                ConfigFile.writestring('Spelling', 'Dictionary', LabelDic.Caption);
            end;

            // --------- S N A P S H O T    S E T T I N G S  -------------------
            configfile.WriteBool('Snapshot', 'AutoSnapEnabled', CheckAutoSnapEnabled.Checked);
            configfile.WriteDateTime('Snapshot', 'NextAutoSnapshot', NextAutoSnapshot);         // Format can (?) be set in SysUtils but does not matter as long as its consistent on this machine
            configfile.WriteInteger('Snapshot',  'DaysPerSnapshot',  SpinDaysPerSnapshot.Value);
            configfile.WriteInteger('Snapshot',  'DaysMaxSnapshots', SpinMaxSnapshots.Value);

            // --------- D A T E    S T A M P    S E T T I N G -----------------
            configfile.Writebool('DateStampSettings', 'Italics',   CheckStampItalics.Checked);
            configfile.Writebool('DateStampSettings', 'Small',     CheckStampSmall.Checked);
            configfile.Writebool('DateStampSettings', 'Bold',      CheckStampBold.Checked);
            configfile.WriteInteger('DateStampSettings', 'Format', ComboDateFormat.ItemIndex);

            // ----------------- Symbols ---------------------------------------
            for i := 0 to high(SymArray) do
                ConfigFile.writestring('SymbolSettings', 'Symbol_' + inttostr(i), SymArray[i].SymStr);

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
        WriteConfigFile();
        SyncSettings();
        SearchForm.IndexNotes(True);
    end;
end;

{ Colors - if its GTK2 or a Qt5 with a QT_QPA_PLATFORMTHEME=[gtk2, qt5ct] then most colors will be right.
    However, the KMemo might be wrong as its always set to a defult light set, ignoring OS.
    So, we must always set Sett's colors for, at least, KMemo to use.
    DarkThemeSwitch tells us to apply the setting to what ever other components we can too.
    SetColors is called by TMainForm.TestDarkThemeInUse during startup, DarkTheme* may have been set.
    It tests for a gtk2, qt5 using qt5ct and defers to qt5ct if possible. Otherwise, sets some
    (hopefully) appropriate colors for either a light or dark theme. These colors
    are always used for the KMemo and possibly, when DarkThemeSwich is used, for what other
    screens I can.   }

procedure TSett.SetColours;
{$if defined(LCLQT5) or defined(LCLQT6)}
var
    Qt_Colors  : TQt_Colors; {$endif}
    // pink = $EEEEFF, White is $FFFFFF, Black is $000000
begin
    {$ifdef DEBUG} debugln({$I %FILE%}, ', ', {$I %CURRENTROUTINE%}, '(), line:', {$I %LINE%}, ' : ',
                   'QT_QPA_PLATFORMTHEME is ' + GetEnvironmentVariable('QT_QPA_PLATFORMTHEME')); {$endif}
    {$if defined(LCLQT5) or defined(LCLQT6)}                       // First we will try the special Qt5 ways of settings colours
    // If user has set QT_QPA_PLATFORMTHEME=gtk2 this bit drops through, all components except KMemo are good.
    Qt_Colors  := TQt_Colors.Create;      // needs some work for qt6
    try
       if Qt_Colors.FoundColors then begin         // Will be false if user not using qt5ct
           BackGndColour:= Qt_Colors_Rec.QColorBackground;
           HiColour   := Qt_Colors_Rec.QColorHighLight+1; // This is, eg Crtl H type highlighting, not selection. +1 to make unique
           AltColour := Qt_Colors_Rec.QColorLessBright;     // Used for selected Text
           TextColour := Qt_Colors_Rec.QColorText;
           TitleColour:= Qt_Colors_Rec.QColorLink;
           LinkColour := Qt_Colors_Rec.QColorLink;
           AltBackGndColor := Qt_Colors_Rec.QColorLessBright; // Selected background colour
           QtOwnsColours := true;
           {$ifdef DEBUG} debugln({$I %FILE%}, ', ', {$I %CURRENTROUTINE%}, '(), line:', {$I %LINE%}, ' : ', 'Colors set by qt5ct.');{$endif}
           exit;
       end;
    finally
        Qt_Colors.Free;
    end;
    {$endif}
    if UserSetColours then exit;        // will have already been set by config or by colour form.
    if DarkTheme or DarkThemeSwitch then begin
            BackGndColour:= $303030;        // KMemo Background
            AltColour  := $606060;          // Some panel's background color
            HiColour   := $600001;          // a dark blue;  This is, eg Crtl H type highlighting, not selection !
            TextColour := clWhite;
            TitleColour:= $B8B800;
            LinkColour := $B8B801;
            AltBackGndColor := clGray;     // Selected text, both focused and unfocused
            {$ifdef DEBUG} debugln({$I %FILE%}, ', ', {$I %CURRENTROUTINE%}, '(), line:', {$I %LINE%}, ' : ', 'Dark Colors set by code.');{$endif}
    end else begin
        BackGndColour := clCream;
        AltColour   := clDefault;
        HiColour    := clYellow-1;
        TextColour  := clBlack;
        TitleColour := clBlue;
        LinkColour  := clBlue+1;       //  One unit of red, no one will notice, but don't subtract 1 from xxxx00  or add 1 to xxxxFF
        AltBackGndColor := clLtGray;
        {$ifdef DEBUG} debugln({$I %FILE%}, ', ', {$I %CURRENTROUTINE%}, '(), line:', {$I %LINE%}, ' : ', 'Light Colors set by code.');{$endif}
    end;
    // if DarkThemeSwitch then color := AltColour;    No, cannot change color of the Tabsheet, looks horrible

end;

procedure TSett.SetHelpLanguage();
var
    HelpIndex : integer = 0;
begin
    while HelpIndex < ComboHelpLanguage.Items.Count do begin
        if HelpNotesLang = copy(ComboHelpLanguage.Items[HelpIndex], 1, 2) then begin
            ComboHelpLanguage.ItemIndex := HelpIndex;
            break;
        end;
        inc(HelpIndex);
    end;
end;

procedure TSett.LoadHelpLanguages();
var
    Info : TSearchRec;
begin
    {$ifdef WINDOWS}HelpNotesPath := AppendPathDelim(ExtractFileDir(Application.ExeName)) + 'HELP' + PathDelim;{$endif}
    {$ifdef LINUX}  HelpNotesPath := '/usr/share/tomboy-ng/HELP/';    {$endif}
    {$ifdef DARWIN} HelpNotesPath := ExtractFileDir(ExtractFileDir(Application.ExeName))+'/Resources/HELP/';{$endif}
    HelpNotesLang:= '';
    ComboHelpLanguage.enabled := False;
    ComboHelpLanguage.Items.Clear;
	if FindFirst(HelpNotesPath + '*', faDirectory, Info)=0 then begin
		repeat
          if (((Info.attr and faDirectory) > 0) and (Info.name[1] <> '.')) then begin
              case Info.Name of
                  'EN' : ComboHelpLanguage.Items.Add('EN - English');
                  'ES' : ComboHelpLanguage.Items.Add('ES - Español');
                  'UK' : ComboHelpLanguage.Items.Add('UK - Українська');
                  'FR' : ComboHelpLanguage.Items.Add('FR - Français');
              otherwise
                    ComboHelpLanguage.Items.Add(Info.Name);
              end;
          end;
        until FindNext(Info) <> 0;
	end;
    FindClose(Info);
    if ComboHelpLanguage.Items.Count > 0 then begin
        ComboHelpLanguage.enabled := True;
        if  ComboHelpLanguage.Items.IndexOf('EN - English') < 0 then      // default to EN if present, else first found.
            HelpNotesLang:= copy(ComboHelpLanguage.Items[0], 1, 2)
        else HelpNotesLang:= 'EN';
    end;
    SetHelpLanguage();
end;

procedure TSett.ComboHelpLanguageChange(Sender: TObject);
begin
    if ComboHelpLanguage.ItemIndex > -1 then begin
        HelpNotesLang:= copy(ComboHelpLanguage.Items[ComboHelpLanguage.ItemIndex], 1, 2);
        WriteConfigFile();
        SearchForm.RefreshMenus(mkHelpMenu);
    end;
end;

procedure TSett.ButtonSetColoursClick(Sender: TObject);
begin
    FormColours.CBack   := BackGndColour;
    FormColours.CHiBack := HiColour;
    FormColours.CText   := TextColour;
    FormColours.CTitle  := TitleColour;
    FormColours.CLink   := LinkColour;
    case FormColours.ShowModal of
        mrRetry  :  begin
                        UserSetColours := False;
                        SetColours();
                        WriteConfigFile();
                    end;
        mrOK     :  begin
	                    BackGndColour := FormColours.CBack;
	                    HiColour := FormColours.CHiBack;
	                    TextColour := FormColours.CText;
	                    TitleColour := FormColours.CTitle;
                        LinkColour := FormColours.CLink;
                        UserSetColours := True;
                        WriteConfigFile();
                    end;
	end;
    CheckUserColours;
end;

procedure TSett.CheckUserColours;
begin
    if UserSetColours then begin
        ButtonSetColours.Hint := 'Custom Colours in use';
        // ToDo : AltBackGndColour may not be appropriate here, wot is ?      And AltColour ????
        AltBackGndColor := clGray;
        AltColour := clLtGray;
    end else
        ButtonSetColours.Hint := 'Default Colours';
end;
procedure TSett.ButtonFixedFontClick(Sender: TObject);
var
    ISMono : boolean = false;
begin
    FontDialog1.Font.Name := FixedFont;
    FontDialog1.Font.Size := 10;
    FontDialog1.Title := 'Select Fixed Spacing Font';
    FontDialog1.PreviewText:= 'abcdef ABCDEF 012345 iiiii wwwww';
    // showmessage(FixedFont);
    FontDialog1.Options := [fdEffects, fdFixedPitchOnly, fdNoSimulations, fdTrueTypeOnly];       // ToDo : this does not show ONLY fixed !
    If FontDialog1.Execute then BEGIN
        FixedFont := FontDialog1.Font.name;
        WriteConfigFile();
    end;
    ButtonFixedFont.Hint := FixedFont;
    Label1.Canvas.Font.Name := FixedFont;
    {$ifdef LCLGTK3}
    // Might work on Linux generally ?  No, not on GTK2 ....
    // https://forum.lazarus.freepascal.org/index.php/topic,20193.msg194575.html
    IsMono := Label1.Canvas.Font.IsMonoSpace;
    {$else}
    IsMono := Label1.Canvas.TextWidth('i') = Label1.Canvas.TextWidth('w');
    // I once experienced some wierd, unrelated things happening doing this, careful !
    {$endif}
    if not IsMono then showmessage('Warning, maybe not a mono spaced font');
end;

procedure TSett.ButtonFontClick(Sender: TObject);
begin
    FontDialog1.Font.Name := UsualFont;
    FontDialog1.Font.Size := 10;
    FontDialog1.Title := 'Select Usual Font';
    FontDialog1.PreviewText:= 'abcdef ABCDEF 012345';
    FontDialog1.Options := [fdEffects, fdNoSimulations, fdTrueTypeOnly];
    If FontDialog1.Execute then BEGIN
        UsualFont := FontDialog1.Font.name;
        WriteConfigFile();
    end;
    ButtonFont.Hint := UsualFont;
end;

RESOURCESTRING
    rsDirHasNoNotes = 'That directory does not contain any notes. That is OK, if I can make my own there.';

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
            SyncFileAuto :=False;
            SyncGithubAuto :=False;
            ComboSyncTypeChange(self);
            WriteConfigFile();
            SyncSettings();
            SearchForm.IndexNotes(True);
        end else
            NoteDirectory := LabelNotesPath.caption;
	end;
end;

{ --------------------- S N A P S H O T S ------------------- }
{ Totally unvalidated rule of thumb -
  About a 200 notes = ~ 400K bytes, we get about 4:1 compression with zipper.
  120ms on lowend laptop.
}

procedure TSett.SpinDaysPerSnapshotChange(Sender: TObject);
begin
    if CheckAutoSnapEnabled.Checked then
        DoAutoSnapShot();
    WriteConfigFile();
end;

procedure TSett.DoAutoSnapshot;
var
   FR : TFormRecover;
   {$ifdef TESTAUTOSNAP} Tick, Tock : qword;{$endif}
   //Notifier : TNotifier;
begin
    if MaskSettingsChanged then
        exit;                   // don't trigger this while GUI is being setup.
    {$ifdef TESTAUTOSNAP}
    Tick := gettickcount64();
    {$endif}
    FR := TFormRecover.Create(self);
    try
        FR.NoteDir := NoteDirectory;
        FR.FullSnapDir := LabelSnapDir.Caption;
        FR.ConfigDir:= AppendPathDelim(Sett.LocalConfig);
        if ('' <> FR.CreateSnapshot(False)) then
            FR.CleanUpSnapshots(SpinMaxSnapshots.Value);
    finally
        FR.Free;
    end;
    // do this after snapshot run to ensure we don't queue up a list of calls.
    {$ifdef TESTAUTOSNAP}
    tock := gettickcount64();
    debugln('DoAutoSnapshot - Finished snapshot, took ' + dbgs(Tock - Tick) + 'mS');
    NextAutoSnapshot := now() + (SpinDaysPerSnapshot.value / (24*60)) ;
    {$else}
    NextAutoSnapshot := now() + SpinDaysPerSnapshot.value;
    {$endif}
    WriteConfigFile();
    SearchForm.UpdateStatusBar(rsAutosnapshotRun);
    if CheckNotifications.Checked then begin
        MainForm.ShowNotification(rsAutosnapshotRun);
        (* {$ifdef LINUX}
        ShowNotification('tomboy-ng', rsAutosnapshotRun);
        {$else}
        MainForm.TrayIcon.BalloonTitle := 'tomboy-ng';
        Mainform.TrayIcon.BalloonHint := 'rsAutosnapshotRun';
        Mainform.TrayIcon.ShowBalloonHint;
        {$endif}   *)
    end;
end;

procedure TSett.CheckAutoSnapEnabledChange(Sender: TObject);
begin
    if CheckAutoSnapEnabled.Checked then begin
        DoAutoSnapShot();
    end;
    SaveSettings(self);
end;

procedure TSett.ButtonManualSnapClick(Sender: TObject);
var
   FR : TFormRecover;
   FullName : string;
begin
    FR := TFormRecover.Create(self);
    try
        Screen.Cursor := crHourGlass;
        FR.NoteDir := NoteDirectory;
        FR.FullSnapDir := LabelSnapDir.Caption;
        FR.ConfigDir:= AppendPathDelim(Sett.LocalConfig);
        FullName := FR.CreateSnapshot(True);
        if mrYes = QuestionDlg('Snapshot created', FullName + ' ' + rsSnapShotCreated
                    , mtConfirmation, [mrYes, mrNo], 0) then
            if SelectSnapDir.Execute then
                if not CopyFile(FullName, TrimFilename(SelectSnapDir.FileName + PathDelim) + ExtractFileNameOnly(FullName) + '.zip') then
                    showmessage(rsErrorCopyFile + ' ' + TrimFilename(SelectSnapDir.FileName + PathDelim) + ExtractFileNameOnly(FullName) + '.zip');
    finally
        FR.Free;
        Screen.Cursor := crDefault;
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
        FR.FullSnapDir := LabelSnapDir.Caption;
        FR.ConfigDir:= AppendPathDelim(Sett.LocalConfig);
        // Danger Will Robertson ! We cannot assume LocalConfig has a trailing slash !
        FR.Showmodal;
        if FR.RequiresIndex then
            SearchForm.IndexNotes(True);
    finally
        FR.Free;
    end;
end;

//                       108/60
// =============================  S Y N C   S T U F F  =========================

{ Note that AutoSync and AutoSnapshot share a timer.  AutoSync runs on each 'tick' of the timer,
  that is, hourly, but autosnapshop looks at NextAutoSnapshot to decide if its time to do its thing.
}
{ This method manages display of all the controls associated with setting
up Sync connections. }
procedure TSett.ComboSyncTypeChange(Sender: TObject);
var
    Ctrl : TControl;
    RememberMask : boolean;
begin
    RememberMask := MaskSettingsChanged;
    MaskSettingsChanged := true;
    case ComboSyncType.ItemIndex of
        0 : begin
                LabelSyncInfo1.caption := rsFileSyncInfo1;
                LabelSyncInfo2.caption := rsFileSyncInfo2;
                CheckBoxAutoSync.Checked := SyncFileAuto;
                CheckSyncEnabled.Checked := SyncFileEnabled;
                for Ctrl in [GroupBoxToken, GroupBoxUser, LabelToken, EditUserName]
                    do Ctrl.Visible := False;
                if SyncFileRepo = '' then
                    LabelSyncRepo.Caption  := rsSyncNotConfig
                else  LabelSyncRepo.Caption := SyncFileRepo;
            end;
        1 : begin
                LabelSyncInfo1.caption := rsGithubSyncInfo1;
                LabelSyncInfo2.caption := rsGithubSyncInfo2;
                CheckBoxAutoSync.Checked := SyncGithubAuto;
                CheckSyncEnabled.Checked := SyncGithubEnabled;
                for Ctrl in [GroupBoxToken, GroupBoxUser, LabelToken, EditUserName]
                    do Ctrl.Visible := True;
                if SyncGithubRepo = '' then
                    LabelSyncRepo.Caption  := rsSyncNotConfig
                else  LabelSyncRepo.Caption  := SyncGithubRepo;
               end;
    end;
    if (LabelSyncRepo.Caption = rsSyncNotConfig)
    or (LabelSyncRepo.Caption = '') then begin
        SpeedSetUpSync.Caption := rsSetUp;
        CheckBoxAutoSync.enabled := false;
        CheckSyncEnabled.enabled := false;
    end else begin
        SpeedSetUpSync.Caption := rsChangeSync;
        CheckBoxAutoSync.enabled := true;
        CheckSyncEnabled.enabled := true;
    end;
   MaskSettingsChanged := RememberMask;
end;


procedure TSett.SpeedSetupSyncClick(Sender: TObject);
var
   SyncType : integer;
begin
    {  Used by both File and Github sync }
    if NoteDirectory = '' then ButtDefaultNoteDirClick(self);

    FormSync.NoteDirectory := NoteDirectory;
    FormSync.LocalConfig := LocalConfig;
    FormSync.SetupSync := True;
    SyncType := ComboSyncType.ItemIndex;
    case SyncType of
      0 : begin                                     // File Sync
            if FileExists(LocalConfig + 'manifest.xml')
            and (mrYes <> QuestionDlg('Warning', rsChangeExistingSync, mtConfirmation, [mrYes, mrNo], 0)) then exit;
            if SelectDirectoryDialog1.Execute then
                LabelSyncRepo.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim)
            else exit();
            SyncFileRepo := '';                     // So Sync unit does not use the old one.
            FormSync.Transport:=TSyncTransport.SyncFile;
            // The Sync unit will get the remote dir from SyncFileRepo
          end;
      1 : begin
            if FileExists(LocalConfig + SyncTransportName(SyncGithub) + PathDelim + 'manifest.xml')
            and (mrYes <> QuestionDlg('Warning', rsChangeExistingSync, mtConfirmation, [mrYes, mrNo], 0)) then exit;
            FormSync.Transport:=TSyncTransport.SyncGithub;
            FormSync.Password := LabelToken.Caption;
            FormSync.UserName := EditUserName.text;
            // SyncGUI will update LabelSyncRepo.Caption if successful.
          end;
    end;
    if mrOK = FormSync.ShowModal then begin
        case SyncType of
            0 : SyncFileRepo := LabelSyncRepo.Caption;
            1 : SyncGithubRepo := LabelSyncRepo.Caption;
        end;
        CheckSyncEnabled.Checked := True;
        WriteConfigFile();
        ComboSyncTypeChange(self);          // update button label
    end;
end;

procedure TSett.SpeedTokenActionsClick(Sender: TObject);
begin
    PopupMenuTokenActions.PopUp;
end;

procedure TSett.MenuItemGetTokenClick(Sender: TObject);
begin
    // ToDo : is it possible to tell if user is logged in at this stage ?
    OpenURL('https://github.com/settings/tokens');
end;

procedure TSett.MenuItemPasteTokenClick(Sender: TObject);
begin
    LabelToken.Caption := Clipboard.AsText;
    SaveSettings(self);
    LabelToken.Hint := '';
end;

procedure TSett.MenuItemCopyTokenClick(Sender: TObject);
begin
    Clipboard.AsText := LabelToken.Caption;
end;

function TSett.fGetValidSync: boolean;
begin
    result := (SyncFileRepo <> '') or (SyncGithubRepo <> '');
end;

procedure TSett.CheckAutostartChange(Sender: TObject);
var
   Auto : TAutoStartCtrl;
begin
    // This is being called at startup, it should only be called when user changes it.
    if not visible then exit;
     Auto := TAutoStartCtrl.Create('tomboy-ng', CheckAutostart.Checked);
     if Auto.ErrorMessage <> '' then
        ShowMessage('Error setting autostart' + Auto.ErrorMessage);
     FreeAndNil(Auto);
     SaveSettings(Sender);
end;

procedure TSett.CheckSyncEnabledChange(Sender: TObject);
begin
    if MAskSettingsChanged then exit;
    case ComboSyncType.ItemIndex of
        0 : SyncFileEnabled := CheckSyncEnabled.Checked;
        1 : SyncGithubEnabled  := CheckSyncEnabled.Checked;
    end;
    SaveSettings(self);
end;

procedure TSett.CheckBoxAutoSyncChange(Sender: TObject);
begin
    // debugln('WARNING - CheckBoxAutoSyncChange called');
    if MAskSettingsChanged then exit;                       // Don't trigger timer during setup
    if CheckBoxAutoSync.Checked then begin
        case ComboSyncType.ItemIndex of
            0 : SyncFileAuto := True;
            1 : SyncGithubAuto  := True;
            otherwise exit;
        end;
        TimerAutoSync.Enabled := false;
        TimerAutoSync.Interval:= 1000;                      // wait a second, then sync. AutoSnap will also be checked.
        TimerAutoSync.Enabled := true;
    end else
        case ComboSyncType.ItemIndex of
            0 : SyncFileAuto := False;
            1 : SyncGithubAuto  := True;
            otherwise exit;
        end;
    SaveSettings(Self);
end;


procedure TSett.Synchronise();
begin
    FormSync.NoteDirectory := Sett.NoteDirectory;
    FormSync.LocalConfig := AppendPathDelim(Sett.LocalConfig);
    // ToDo : this will not be enough when we save our notes in a second thread.
    // Might need to check, somehow, that no threads are still running ?  How ?
    SearchForm.FlushOpenNotes();
    FormSync.SetupSync := False;
    if SyncFileEnabled then begin
        FormSync.AnotherSync := SyncGithubEnabled;
        FormSync.Transport:=TSyncTransport.SyncFile;
        if FormSync.busy or FormSync.Visible then       // busy should be enough but to be sure ....
            FormSync.Show
        else
            FormSync.ShowModal;
    end;
    if SyncGithubEnabled then begin
        FormSync.AnotherSync := False;
        FormSync.Transport:=TSyncTransport.SyncGithub;
        if FormSync.busy or FormSync.Visible then       // busy should be enough but to be sure ....
            FormSync.Show
        else
            FormSync.ShowModal;
    end;
end;

procedure TSett.StartAutoSyncAndSnap();
begin
    if (MainUnit.SingleNoteFileName = '')  then begin
        TimerAutoSync.Interval:= 15000;     // wait 15 seconds after indexing to allow settling down
        TimerAutoSync.Enabled := true;
        // Note that this timer will also trigger checking of AutoSnapshot.  But AutoSnapshot only
        // does something if NextAutoSnapshot is > now(), while AutoSync always runs on timer if enabled.
    end;
end;

function TSett.GetSyncFileRepo(): string;
begin
    if SyncFileRepo <> '' then
        Result := SyncFileRepo
    else
        if (ComboSyncType.ItemIndex = 0)
                    and (LabelSyncRepo.Caption <> rsSyncNotConfig)
                    and (LabelSyncRepo.Caption <> '') then
            result := LabelSyncRepo.Caption
        else
            Result := 'File Sync is not configured';
end;

procedure TSett.TimerAutoSyncTimer(Sender: TObject);
begin
    // TimerAutoSync.enabled := False;
    TimerAutoSync.Interval:= 60*60*1000;                                    // do it again in one hour
    {$IFDEF TESTAUTOSNAP}
    TimerAutoSync.Interval:= 60*1000;
    debugln('WARNING - TESTAUTOSNAP is defined, timer called, MSC is ' + dbgs(MAskSettingsChanged));
    {$ENDIF}
    //if  (ValidSync <> '') and CheckBoxAutoSync.checked  and (not FormSync.Busy) then begin
    if  SyncFileAuto and (SyncFileRepo <> '') and SyncFileEnabled and (not FormSync.Busy) then begin
        FormSync.NoteDirectory := Sett.NoteDirectory;
        FormSync.LocalConfig := AppendPathDelim(Sett.LocalConfig);
        FormSync.Transport:=TSyncTransport.SyncFile;
        FormSync.SetupSync := False;                                        // That is, we are not, now, trying to setup sync
        FormSync.RunSyncHidden()
    end;
    if  SyncGithubAuto and (SyncGithubRepo <> '') and SyncGithubEnabled and (not FormSync.Busy) then begin
        FormSync.NoteDirectory := Sett.NoteDirectory;
        FormSync.LocalConfig := AppendPathDelim(Sett.LocalConfig);
        FormSync.Transport:=TSyncTransport.SyncGithub;
        FormSync.SetupSync := False;
        FormSync.RunSyncHidden()
    end;

    // debugln('Now its about ' + DateTimeToStr(now));
    // debugln('Next Snap due ' + DateTimeToStr(NextAutoSnapshot));
    if CheckAutoSnapEnabled.Checked and (NextAutoSnapshot < now()) then
        DoAutoSnapshot;
end;

procedure TSett.ButtonShowBackUpClick(Sender: TObject);
var
    BV : TFormBackupView;
begin
    BV := TFormBackupView.Create(self);
    try
        BV.ShowModal;
    finally
        FreeandNil(BV);
    end;
end;

procedure TSett.SaveSettings(Sender: TObject);
begin
    WriteConfigFile();      // Write to disk
    SyncSettings();
end;



end.


(* This is getmem's blowfish model, might use it to encrypt both notes and token
   https://forum.lazarus.freepascal.org/index.php/topic,56489.msg419952.html#msg419952

unit uCrypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlowFish, Base64;

  function Encrypt(const AKey, AText: String): String;
  function Decrypt(const AKey, AText: String): String;

implementation

function Encrypt(const AKey, AText: String): String;
var
  SS: TStringStream;
  BES: TBlowFishEncryptStream;
begin
  Result := '';
  if Trim(AText) = '' then
    Exit;
  SS := TStringStream.Create('');
  try
    BES := TBlowFishEncryptStream.Create(AKey, SS);
    try
      BES.Write(Pointer(AText)^, Length(AText));
    finally
      BES.Free;
    end;
    Result := EncodeStringBase64(SS.DataString);
  finally
    SS.Free;
  end;
end;

function Decrypt(const AKey, AText: String): String;
var
  SS: TStringStream;
  BDS: TBlowFishDeCryptStream;
  Str, Txt: String;
begin
  Result := '';
  if Trim(AText) = '' then
    Exit;
  Str := '';
  Txt := DecodeStringBase64(AText);
  SS := TStringStream.Create(Txt);
  try
    BDS := TBlowFishDeCryptStream.Create(AKey, SS);
    try
      SetLength(Str, SS.Size);
      BDS.Read(Pointer(Str)^, SS.Size);
      Result := Str;
    finally
      BDS.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.               *)


