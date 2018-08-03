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

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, {FileUtil,} Forms, Controls, Graphics, Dialogs, StdCtrls,
    Buttons, ComCtrls, ExtCtrls, Grids, Menus, BackUpView
    {$ifdef LINUX}, Unix {$endif} ;              // We call a ReReadLocalTime()
// Types;

type TSyncOption = (AlwaysAsk, UseServer, UseLocal);	// Relating to sync clash

type

    { TSett }

    TSett = class(TForm)
			ButtDefaultNoteDir: TButton;
            ButtonHide: TButton;
			ButtonShowBackUp: TButton;

		ButtonSetNotePath: TButton;
		ButtonSetSynServer: TButton;
        CheckCaseSensitive: TCheckBox;
        CheckAnyCombination: TCheckBox;
		CheckManyNotebooks: TCheckBox;
		CheckShowExtLinks: TCheckBox;
		CheckShowIntLinks: TCheckBox;
        EditLibrary: TEdit;
        EditDic: TEdit;
		GroupBox3: TGroupBox;
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
		LabelWaitForSync: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		Label5: TLabel;
		Label6: TLabel;
		Label7: TLabel;
		Label8: TLabel;
		Label9: TLabel;
		LabelSyncRepo: TLabel;
		LabelLocalConfig: TLabel;
		LabelNotesPath: TLabel;
		LabelSettingPath: TLabel;
        ListBoxDic: TListBox;
		PageControl1: TPageControl;
		Panel1: TPanel;
		Panel2: TPanel;
		RadioAlwaysAsk: TRadioButton;
		RadioFile: TRadioButton;
		RadioFontBig: TRadioButton;
		RadioFontMedium: TRadioButton;
		RadioFontSmall: TRadioButton;
		RadioUseLocal: TRadioButton;
		RadioUseServer: TRadioButton;
		RadioServer: TRadioButton;
		SelectDirectoryDialog1: TSelectDirectoryDialog;
		StringGridBackUp: TStringGrid;
		TabBasic: TTabSheet;
		TabBackUp: TTabSheet;
        TabSpell: TTabSheet;
		TabSnapshot: TTabSheet;
		TabSync: TTabSheet;
		TabDisplay: TTabSheet;
		procedure ButtDefaultNoteDirClick(Sender: TObject);
        procedure ButtonHideClick(Sender: TObject);
        //procedure ButtonSaveConfigClick(Sender: TObject);
		procedure ButtonSetNotePathClick(Sender: TObject);
		procedure ButtonSetSynServerClick(Sender: TObject);
		procedure ButtonShowBackUpClick(Sender: TObject);
        //procedure CheckManyNotebooksChange(Sender: TObject);
        { Called when ANY of the setting check boxes change so use can save. }
		procedure CheckReadOnlyChange(Sender: TObject);
        procedure EditDicKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		// procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormHide(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ListBoxDicClick(Sender: TObject);
		procedure PageControl1Change(Sender: TObject);
        procedure StringGridBackUpDblClick(Sender: TObject);
		//procedure Timer1Timer(Sender: TObject);
   	private
        fExportPath : ANSIString;  { TODO : This will need to be a property }
        // Reads an existing config file OR writes a new, default one if necessary.
 		procedure CheckConfigFile;
        // Checks and/or makes indicatd dir, warns user if not there and writable.
        function CheckDirectory(DirPath: string): boolean;
        function CheckForDict(const DictPath : ANSIString): boolean;
        procedure CheckSpelling;
        // Returns the default place to store notes. It may not be present.
        function GetDefaultNoteDir: string;
        procedure SetFontSizes;
        // Saves all current settings to disk. Call when any change is made.
        procedure SettingsChanged();
		procedure SyncSettings;
    public
        // Indicates SettingsChanged should not write out a new file cos we are loading from one.
        MaskSettingsChanged : boolean;
        AllowClose : Boolean;           // review need for this
        // Indicates we should re-index notes when form hides
        NeedRefresh : Boolean;
        FontSmall  : Integer;
     	FontLarge  : Integer;
     	FontHuge   : Integer;
     	FontTitle  : Integer;			// Dont set this to one of the other sizes !
     	FontNormal : Integer;
        { The directory expected to hold existing or new notes }
        NoteDirectory : string;
        LocalConfig : string;
        RemoteRepo  : string;
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
            { A target for mainunit ? }
        procedure Synchronise();
        property ExportPath : ANSIString Read fExportPath write fExportPath;
    end;

var
    Sett : TSett;


const

    Placement = 45;				// where we position an opening window. Its, on average, 1.5 time Placement;

     HiColor      = clYellow;
     NormalColor  = clDefault; 		// Must somewhere set this to be sure ?

     LinkScanRange = 50;		// when the user changes a Note, we search +/- around
     							// this value for any links that need adjusting.

  	SyncNotConfig = 'not configured';

implementation

{$R *.lfm}

{ TSett }


uses IniFiles, LazLogger,
    LazFileUtils,   // LazFileUtils needed for TrimFileName(), cross platform stuff;
    Note_Lister,	// List notes in BackUp and Snapshot tab
    SearchUnit,		// So we can call IndexNotes() after altering Notes Dir
    syncGUI,
    hunspell;       // spelling check


var
    Spell: THunspell;
     DicPath : AnsiString;

procedure TSett.SetFontSizes;
begin
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
        CheckShowIntLinks.enabled := true;
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

    { ----------------- S P E L L I N G ----------------------}

function TSett.CheckForDict(const DictPath : ANSIString) : boolean;
var
    Info : TSearchRec;
begin
    Result := False;
    LabelError.Caption := '';
    ListBoxDic.Clear;
    ListBoxDic.Enabled := False;
    if FindFirst(AppendPathDelim(DictPath) + '*.dic', faAnyFile and faDirectory, Info)=0 then begin
        repeat
            ListBoxDic.Items.Add(Info.Name);
        until FindNext(Info) <> 0;
    end;
    FindClose(Info);
    if ListBoxDic.Items.Count = 0 then begin
       LabelDicStatus.Caption := 'No Dictionary Found';
       LabelDic.Caption := 'Enter a new path to Dictionaries :';
       EditDic.Text := DictPath;
    end;
    if ListBoxDic.Items.Count = 1 then begin                   // Exactly one returned.
        if not Spell.SetDictionary(AppendPathDelim(DictPath) + ListBoxDic.Items.Strings[0]) then begin
            LabelError.Caption := 'ERROR ' + Spell.ErrorMessage;
            LabelDicStatus.Caption := 'A Dictionary Found, but Failed to Load';
            LabelDic.Caption := 'Enter a new path to Dictionaries :'
        end else begin
            LabelDicStatus.Caption := 'Dictionary Loaded OK';
            LabelDic.Caption := DictPath + ListBoxDic.Items.Strings[0];
        end;
    end;
    if ListBoxDic.Items.Count > 1 then begin
       LabelDicStatus.Caption := 'Choose a dictionary from right';
       LabelDic.Caption := 'or or enter a new dictionary path';
       LabelDicPrompt.Caption := 'Click a Dictionary';
       LabelDicPrompt.Visible := True;
       ListBoxDic.Enabled := True;
    end;
    Result := Spell.GoodToGo;   // only true if count was exactly one or FindDict failed and nothing changed
end;


procedure TSett.ListBoxDicClick(Sender: TObject);
begin
    if ListBoxDic.ItemIndex > -1 then
        SpellConfig := Spell.SetDictionary(AppendPathDelim(DicPath) + ListBoxDic.Items.Strings[ListBoxDic.ItemIndex]);
    LabelError.Caption := Spell.ErrorMessage;
    if SpellConfig then begin
        LabelDicStatus.Caption := 'Dictionary Loaded OK';
        LabelDic.Caption := AppendPathDelim(DicPath) + ListBoxDic.Items.Strings[ListBoxDic.ItemIndex];
        //ButtonSaveConfig.Enabled := True;
        SettingsChanged();
        NeedRefresh := True;
    end else begin
        LabelDicStatus.Caption := 'Dictionary Failed to Load';
        LabelDic.Caption := 'Enter a new path to Dictionaries :'
    end;
end;

procedure TSett.EditDicKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = 13 then begin
        Key := 0;
        DicPath := AppendPathDelim(trim(EditDic.Text));
        SpellConfig := CheckForDict(DicPath);
    end;
end;

procedure TSett.CheckSpelling;
var
    DicPathAlt : AnsiString;
begin
    DicPathAlt := ExtractFilePath(Application.ExeName);
    {$ifdef WINDOWS}
    DicPath := 'C:\Program Files\LibreOffice 5\share\extensions\dict-en\';
    {$ENDIF}
    {$ifdef DARWIN}
    DicPath := '/Applications/Firefox.app/Contents/Resources/dictionaries/';
    {$endif}
    {$ifdef LINUX}
    DicPath := '/usr/share/hunspell/';
    DicPathAlt := '/usr/share/myspell/';
    {$ENDIF}
    LabelError.Caption:='';
    ListBoxDic.enabled:= False;
    LabelDic.Visible := False;
    LabelDicStatus.Visible := False;
    LabelDicPrompt.Visible := False;
    EditLibrary.Text := '';
    EditDic.Text := '';
    EditDic.Visible:= False;
    if fileexists(LabelLibrary.Caption) then		// make sure file from config is still valid
    	Spell :=  THunspell.Create(LabelLibrary.Caption)
    else Spell :=  THunspell.Create();
    if Spell.ErrorMessage = '' then begin
        LabelLibraryStatus.caption := 'Library Loaded OK';
        LabelLibrary.Caption := Spell.LibraryFullName;
        EditDic.Visible := True;
        LabelDicStatus.Visible := True;
        LabelDic.Visible := True;
        // if LabelDic.Caption <> '' then begin
        if fileexists(LabelDic.Caption) then begin	// check that config dic is still valid
            SpellConfig := Spell.SetDictionary(LabelDic.Caption);
            if SpellConfig then begin
               LabelDicStatus.Caption := 'Dictionary Loaded OK';
            end else begin
                LabelDicStatus.Caption := 'No Dictionary Found';
                EditDic.Text := ExtractFilePath(LabelDic.Caption);
                LabelDic.Caption := 'Enter a new path to Dictionaries :';
            end;
        end
        else begin
            SpellConfig := CheckForDict(DicPath);  // if we find 1, use it, 0 try again.
            if ListBoxDic.Items.Count = 0 then
            // if not SpellConfig then
                SpellConfig := CheckForDict(DicPathAlt);
        end;
    end
    else begin
        LabelLibraryStatus.Caption := 'Library Not Loaded';
        LabelLibrary.Caption := 'Enter a full library name below :';
        EditLibrary.Text := Spell.LibraryFullName;
        SpellConfig := False;
    end;
end;

{ --------------------- H O U S E    K E E P I NG -------------------- }

procedure TSett.FormHide(Sender: TObject);
begin
    FreeandNil(Spell);
    MaskSettingsChanged := True;
    if NeedRefresh then begin
        SearchForm.IndexNotes();
        NeedRefresh := False;
    end;
end;

procedure TSett.FormShow(Sender: TObject);
begin
    //CheckSpelling;
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

procedure TSett.FormCreate(Sender: TObject);
begin
    MaskSettingsChanged := true;
    NeedRefresh := False;
    ExportPath := '';
    LabelWaitForSync.Caption := '';
    LabelLibrary.Caption := '';
    HaveConfig := false;
    NoteDirectory := Sett.GetDefaultNoteDir;
    labelNotesPath.Caption := NoteDirectory;
    CheckSpelling();
    CheckConfigFile();                      // write a new, default one if necessary
    if (LabelSyncRepo.Caption = '') or (LabelSyncRepo.Caption = SyncNotConfig) then
        ButtonSetSynServer.Caption := 'Set File Sync Repo';
end;

procedure TSett.FormDestroy(Sender: TObject);
begin
    FreeandNil(Spell);
end;

{ --------------------- F I L E    I / O --------------------------- }

function TSett.CheckDirectory(DirPath : string) : boolean;
begin
    Result := False;
    if not DirectoryExistsUTF8(DirPath) then
        ForceDirectoriesUTF8(DirPath);
    if not DirectoryExistsUTF8(DirPath) then begin
        ShowMessage('Unable to Create Directory [' + DirPath + ']');
        Debugln('Settings is unable to Create Directory [' + DirPath + ']');
        exit(False);
    end;
    if DirectoryIsWritable(DirPath) then
        exit(True);
    ShowMessage('Cannot write into [' + DirPath + ']');
    DebugLn('Settings cannot write into [' + DirPath + ']');
end;

{ Read config file if it exists }
procedure TSett.CheckConfigFile;
var
    ConfigFile : TINIFile;
    ReqFontSize : ANSIString;
begin
    if Application.HasOption('config-dir') then
        LocalConfig := Application.GetOptionValue('config-dir')
    else
        LocalConfig := GetAppConfigDirUTF8(False);
    if LocalConfig = '' then LocalConfig := GetAppConfigDirUTF8(False);
    LabelSettingPath.Caption := AppendPathDelim(LocalConfig) + 'tomboy-ng.cfg';
    LabelLocalConfig.Caption := LocalConfig;
    if not CheckDirectory(LocalConfig) then exit;
    if fileexists(LabelSettingPath.Caption) then begin
 	    ConfigFile :=  TINIFile.Create(LabelSettingPath.Caption);
 	    try
            MaskSettingsChanged := True;    // should be true anyway ?
   		    NoteDirectory := ConfigFile.readstring('BasicSettings', 'NotesPath', '');
            if 'true' = ConfigFile.readstring('BasicSettings', 'ShowIntLinks', 'true') then
                CheckShowIntLinks.Checked := true
            else CheckShowIntLinks.Checked := false;

            CheckManyNoteBooks.checked :=
        	    ('true' = Configfile.readstring('BasicSettings', 'ManyNotebooks', 'false'));

            CheckCaseSensitive.Checked :=
                ('true' = Configfile.readstring('BasicSettings', 'CaseSensitive', 'false'));

            CheckAnyCombination.Checked :=
                ('true' = Configfile.readstring('BasicSettings', 'AnyCombination', 'true'));

            ReqFontSize := ConfigFile.readstring('BasicSettings', 'FontSize', 'medium');
            case ReqFontSize of
        	    'big'    : RadioFontBig.Checked := true;
                'medium' : RadioFontMedium.Checked := true;
                'small'  : RadioFontSmall.Checked := true;
            end;
            case ConfigFile.readstring('SyncSettings', 'SyncOption', 'AlwaysAsk') of
                'AlwaysAsk' : begin SyncOption := AlwaysAsk; RadioAlwaysAsk.Checked := True; end;
                'UseLocal'  : begin SyncOption := UseLocal;  RadioUseLocal.Checked  := True; end;
                'UseServer' : begin SyncOption := UseServer; RadioUseServer.Checked := True; end;
		    end;
            LabelLibrary.Caption := ConfigFile.readstring('Spelling', 'Library', '');
            LabelDic.Caption := ConfigFile.readstring('Spelling', 'Dictionary', '');
            SpellConfig := (LabelLibrary.Caption <> '') and (LabelDic.Caption <> '');     // indicates it worked once...
		    LabelSyncRepo.Caption := ConfigFile.readstring('SyncSettings', 'SyncRepo', SyncNotConfig);
            RemoteRepo := LabelSyncRepo.Caption;
	    finally
            ConfigFile.free;
            // MaskSettingsChanged := False;
	    end;
	    SyncSettings();
        NeedRefresh := True;                // Needed ???
        // ButtonSaveConfig.Enabled := False;
    end else begin
        if CheckDirectory(NoteDirectory) then begin
            MaskSettingsChanged := False;
            RadioFontMedium.Checked := True;
            SettingsChanged();    // write a initial default file
            MaskSettingsChanged := True;
            HaveConfig := True;
        end else begin
            // Only get to here becasue we have failed to setup an initial notes dir
            // and we don't even have a settings file in place. Directories may not be present.
            LabelNotespath.Caption := 'Please Set a Path to a Notes Directory';
            NoteDirectory := '';
            CheckManyNoteBooks.Checked := False;
            HaveConfig := false;
            Debugln('We have issues with you directories, suggest you do not proceed !');
        end;
    end;
end;

procedure TSett.SettingsChanged();
var
	ConfigFile : TINIFile;
begin
    if MaskSettingsChanged then exit();
    ConfigFile :=  TINIFile.Create(LabelSettingPath.Caption);
    try
      ConfigFile.writestring('BasicSettings', 'NotesPath', NoteDirectory);
      ConfigFile.writestring('SyncSettings', 'SyncRepo', RemoteRepo);
      if CheckManyNoteBooks.checked then
      	Configfile.writestring('BasicSettings', 'ManyNotebooks', 'true')
      else Configfile.writestring('BasicSettings', 'ManyNotebooks', 'false');
      if CheckCaseSensitive.checked then
      	Configfile.writestring('BasicSettings', 'CaseSensitive', 'true')
      else Configfile.writestring('BasicSettings', 'CaseSensitive', 'false');
      if CheckAnyCombination.checked then
      	Configfile.writestring('BasicSettings', 'AnyCombination', 'true')
      else Configfile.writestring('BasicSettings', 'AnyCombination', 'false');
      if CheckShowIntLinks.Checked then
          ConfigFile.writestring('BasicSettings', 'ShowIntLinks', 'true')
      else ConfigFile.writestring('BasicSettings', 'ShowIntLinks', 'false');
      if RadioFontBig.Checked then
          ConfigFile.writestring('BasicSettings', 'FontSize', 'big')
      else if RadioFontMedium.Checked then
          ConfigFile.writestring('BasicSettings', 'FontSize', 'medium')
      else if RadioFontSmall.Checked then
          ConfigFile.writestring('BasicSettings', 'FontSize', 'small');
		if RadioAlwaysAsk.Checked then
            ConfigFile.writestring('SyncSettings', 'SyncOption', 'AlwaysAsk')
        else if RadioUseLocal.Checked then
            ConfigFile.writestring('SyncSettings', 'SyncOption', 'UseLocal')
        else if RadioUseServer.Checked then
            ConfigFile.writestring('SyncSettings', 'SyncOption', 'UseServer');

        if SpellConfig then begin
            ConfigFile.writestring('Spelling', 'Library', LabelLibrary.Caption);
            ConfigFile.writestring('Spelling', 'Dictionary', LabelDic.Caption);
        end;
    finally
    	ConfigFile.Free;
    end;
    // debugln('just wrote a settings file out');
end;

function TSett.GetDefaultNoteDir : string;
begin
    {$IFDEF UNIX}
    Result := GetEnvironmentVariable('HOME') + '/.local/share/tomboy-ng/';
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
        NeedRefresh := True;
    end;
end;

procedure TSett.ButtonHideClick(Sender: TObject);
begin
    Hide;
end;

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
               showmessage('That directory does not contain any notes. Thats OK, if I can make my own there.');
		    end;
            FindClose(Info);
            // LabelNotesPath.Caption := NoteDirectory;
		    // ButtonSaveConfig.Enabled := True;
            CheckShowIntLinks.enabled := true;
            // CheckReadOnly.enabled := true;
            SettingsChanged();
            SyncSettings();
            NeedRefresh := True;
        end else
            NoteDirectory := LabelNotesPath.caption;
        // SearchForm.IndexNotes();
	end;
end;

{ ------------------------ S Y N C -------------------------- }

procedure TSett.Synchronise();
begin
    FormSync.NoteDirectory := Sett.NoteDirectory;
    FormSync.LocalConfig := Sett.LocalConfig;
    FormSync.RemoteRepo := Sett.RemoteRepo;
    FormSync.SetupFileSync := False;
    if FormSync.Visible then
        FormSync.Show
    else
    	NeedRefresh := (FormSync.ShowModal = mrOK);
end;


procedure TSett.ButtonSetSynServerClick(Sender: TObject);
begin
    if NoteDirectory = '' then ButtDefaultNoteDirClick(self);
    if SelectDirectoryDialog1.Execute then begin
        LabelWaitForSync.Caption := 'Ok, please wait, might take a few minutes .....';
        Application.ProcessMessages;   		// That forces (eg) the above caption update.
		RemoteRepo := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
        if RemoteRepo = '' then RemoteRepo := SyncNotConfig;
        LabelSyncRepo.Caption := RemoteRepo;
        FormSync.NoteDirectory := NoteDirectory;
        FormSync.LocalConfig := LocalConfig;
        FormSync.RemoteRepo := RemoteRepo;
        FormSync.SetupFileSync := True;
        if mrOK = FormSync.ShowModal then begin
            RemoteRepo := LabelSyncRepo.Caption;
           	// ButtonSaveConfigClick(self);
            ButtonSetSynServer.Caption:='Change File Sync';
        	// OK, user has tested, done first sync, is happy. Save this config.
            SettingsChanged();
        end else begin
        	LabelSyncRepo.Caption := SyncNotConfig;
            RemoteRepo := SyncNotConfig;
		end;
        SearchForm.IndexNotes();
        LabelWaitForSync.Caption := '';
	end;
end;

procedure TSett.ButtonShowBackUpClick(Sender: TObject);
var
	NoteLister : TNoteLister;
begin
    NoteLister := TNoteLister.Create;
    NoteLister.WorkingDir:= NoteDirectory + 'Backup' + PathDelim;
    NoteLister.GetNotes();
    NoteLister.LoadStGrid(StringGridBackUp);
    NoteLister.Free;
    Label15.caption := 'double click a note ...';
end;

	{ Called when ANY of the setting check boxes change so use can save. }
procedure TSett.CheckReadOnlyChange(Sender: TObject);
begin
    // ButtonSaveConfig.Enabled := True;
    SettingsChanged();
    SyncSettings();
end;

function TSett.GetLocalTime: ANSIstring;
var
   ThisMoment : TDateTime;
   Res : ANSIString;
   Off : longint;
begin
   // Note this function is duplicated in TB_Sync.
    {$ifdef LINUX}
    ReReadLocalTime();    // in case we are near daylight saving time changeover
    {$endif}
    ThisMoment:=Now;
    Result := FormatDateTime('YYYY-MM-DD',ThisMoment) + 'T'
                   + FormatDateTime('hh:mm:ss.zzz"0000"',ThisMoment);
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

