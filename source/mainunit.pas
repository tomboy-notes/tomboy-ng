unit Mainunit;
 {  Copyright (C) 2017-2024 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

    ------------------

    The Main unit for the application, it displayes the small splash screen (if
    enabled), manages some of the command line switches, runs the IPC server to
    communicate with other instances, starts single note mode.
    Makes some decisions about Windows Dark Theme.

    Manages display of SysTrayIcon (but events, ie clicks, are in SearchUnit).
    Possible sensible to move the TrayIcon and its popupMenu to Search Unit,
    then have a single method responsible for creating and setting it up.

 }

{$mode objfpc}{$H+}

{   HISTORY
    2018/05/12  Extensive changes - MainUnit is now just that. This is not the same
                unit that used this name previously!
    2018/05/19  Control if we allow opening window to be dismissed and show TrayIcon
                and MainMenu.
    2018/05/20  Alterations to way we startup, wrt mainform status report.
    2018/05/20  Set the recent menu items caption to be 'empty' in case user looks
                before having set a notes directory.
    2018/06/02  Added a cli switch to debug sync

    2018/06/19  Got some stuff for singlenotemode() - almost working.
    2018/06/22  As above but maybe working now ?  DRB
    2018/07/04  Display number of notes found and a warning if indexing error occurred.
    2018/07/11  Added --version and --no-splash to options, this form now has a main menu
                and does not respond to clicks anywhere with the popup menu, seems GTK
                does not like sharing menus (eg between here and the trayIcon) in gtk3 !
                So, in interests of uniformity, everyone gets a Main Menu and no Popup.
    2018/11/01  Now include --debug-log in list of INTERAL switches.
    2018/12/02  Now support Alt-[Left, Right] to turn off or on Bullets.
    2018/12/03  Added show splash screen to settings, -g or an indexing error will force show
    2019/03/19  Added a checkbox to hide screen on future startups
    2019/03/19  Added setting option to show search box at startup
    2019/04/07  Restructured Main and Popup menus. Untested Win/Mac.
    2019/04/13  Mv numb notes to tick line, QT5, drop CheckStatus()
    2019/05/06  Support saving pos and open on startup in note.
    2019/05/14  Display strings all (?) moved to resourcestrings
    2019/06/11  Moved an ifdef
    2019/07/21  Added a TitleColour for dark theme
    2019/08/20  Linux only, looks for (translated) help files in config dir first.
    2019/09/6   Button to download Help Notes in non-English
    2019/09/21  Restructured model for non-english help notes, names in menus !
    2019/10/13  Prevent Dismiss if desktop is Enlightenment, in OnCreateForm()
    2019/11/05  Don't treat %f as a command line file name, its an artifact
    2019/11/08  Tidy up building GTK3 and Qt5 versions, cleaner About
    2019/11/20  Don't assign PopupMenu to TrayIcon on (KDE and Qt5)
    2019/12/08  New "second instance" model.
    2019/12/11  Heavily restructured Startup, Main Menu everywhere !
    2019/12/20  Added option --delay-start for when desktop is slow to determine its (dark) colours
    2020/03/30  Allow user to set display colours.
    2020/04/10  Make help files non modal
    2020/04/12  Force sensible sizes for help notes.
    2020/04/28  Added randomize to create, need it for getlocaltime in settings.
    2020/05/16  Don't prevent closing of splash screen.
    2020/05/23  Don't poke SingleNoteFileName in during create, get it from Mainunit in OnCreate()
    2020/05/26  Improved tabbing
    2020/06/11  remove unused closeASAP, open splash if bad note.
    2020/07/09  New help notes location. A lot moved out of here.
    2020/11/07  Fix multiple About Boxes (via SysTray) issue. Untested on Win/Mac
    2020/11/18  Changed other two buttons to BitBtn so qt5 looks uniform.
    2021/01/04  Pointed Tomdroid menu to tomdroidFile.
    2021/01/23  We now test for a SysTray, show warning and Help is not there.
    2021/04/01  Removed "have config", we always have config, if we cannot save it, user knows....
    2021/05/11  On Gnome Linux, test for libappindicator3 and appindicator shell plugin
    2021/05/15  On Gnome, if plugin is present but disabled, offer to enable it for user.
    2021/05/19  If libappindicator is not present, check for libayatana-appindicator, but only if lcl is patched !
    2021/07/13  Don't do full SysTray check on Gnome with Qt, AccessViolation, just guess.
    2021/11/09  Dont consider libayatana unless compiled with > 2.0.12
    2022/05/03  Add unix username to IPC pipe.
    2022/08/10  Added an about Lazarus to splash screen and simplified the About screen.
    2022/10/20  To Avoid calling IndexNotes() from Import, now function, IndexNewNote()
    2022/11/14  ShowNotifications() now cross platform.
    2023/03/17  Provide better support for dark theme, particularly for Qt5 in qt5ct mode
    2023/04/30  Another Gnome (for 44.0) fix, apparently it does not like an empty menu in the trayicon at show.
    2024/06/01  Dont show menu from TMainForm.TrayIconClick() on Wayland-Gnome as it generats a second
                menu when using -plaform xcb (qt5/6). Slightly different that the related KDE issue.
                Also, only prevent left click on WAYLAND KDE now, sensible KDEs will work OK

    CommandLine Switches

    --delay-start

    --debug-log=some.log

    --dark-theme    Windows only, over rides the registery setting.

    --gnome3    ignored
    -g
    --debug-sync Turn on Verbose mode during sync

    --debug-index Verbose mode when reading the notes directory.

    --debug-spell  Verbose mode when setting up speller

    --config-dir=<some_dir> Directory to keep config and sync manifest in.

    -o note_fullfilename
    --open=<note_fullfilename> Opens, in standalone mode, a note. Don't start
                the SimpleIPC conversation.

    --help -h   Shows and exits (not implemented)
                something to divert debug msg to a file ??
                something to do more debugging ?

    --no-splash Do not show the small opening status/splash window on startup

    --save-exit (Single note only) after import, save and exit.

    --version   Print version no and exit.

}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
    StdCtrls, LCLTranslator, DefaultTranslator, Buttons, simpleipc,
    LCLType, ComCtrls
    {$ifdef LINUX}, x, xlib, process {$endif}             // Relate to testing for SysTray
    {$IFDEF LCLGTK3}, LazGdk3, LazGLib2 {$ENDIF}          // we need declare a GTK3 function that has not yet made it to bindings


  , fpimage,
  fpreadjpeg,
(*  {$ifdef VER3_2_2}     // thats FPC. Note also must deal with fptty listed in Project Inspector.
  fppdf,                // And if the compile process finds fpttf.pp in source dir, it decides to use that one anyway.
  fpttf,                // If (the patched) fpttf.pp is present in source dir, it will be used, else the FPC one rules.
  {$endif}
  fpparsettf, *)
  typinfo;

type

    { TMainForm }

    TMainForm = class(TForm)
//        ApplicationProperties1: TApplicationProperties;      commented out 2023/12/03,no idea why its here, removed from form too
		ButtSysTrayHelp: TBitBtn;
		BitBtnHide: TBitBtn;
		BitBtnQuit: TBitBtn;
        ButtMenu: TBitBtn;
        CheckBoxDontShow: TCheckBox;
        ImageAboutLaz: TImage;
        ImageSpellCross: TImage;
        ImageSpellTick: TImage;
        ImageNotesDirCross: TImage;
        ImageSyncCross: TImage;
        ImageNotesDirTick: TImage;
        ImageSyncTick: TImage;
        LabelBadNoteAdvice: TLabel;
        LabelError: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        LabelNotesFound: TLabel;
        TrayIcon: TTrayIcon;
		procedure BitBtnHideClick(Sender: TObject);
        procedure BitBtnQuitClick(Sender: TObject);
        procedure ButtMenuClick(Sender: TObject);
        procedure ButtonCloseClick(Sender: TObject);
        procedure ButtonConfigClick(Sender: TObject);
        procedure ButtonDismissClick(Sender: TObject);
		procedure ButtSysTrayHelpClick(Sender: TObject);
        procedure CheckBoxDontShowChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormResize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ImageAboutLazClick(Sender: TObject);
        procedure LabelErrorClick(Sender: TObject);
        procedure TrayIconClick(Sender: TObject);
    private
        AboutFrm : TForm;
        NoLeftClickOnTrayIcon : boolean;    // Is false for everyone except KDE.
        UglyGnome : boolean;
        CommsServer : TSimpleIPCServer;
        // Start SimpleIPC server listening for some other second instance.

        procedure StartIPCServer();
        procedure CommMessageReceived(Sender: TObject);
        // Attempt to detect we are in a dark theme, sets relevent colours, if main form
        // is dark, then rest of app will be too except for the KMemo.
        procedure TestDarkThemeInUse();
        {$ifdef LINUX}
        function CheckGnomeExtras(): boolean;
                        // Test for traditional SysTray, if not successful we call look for the
                        // the things that can be added to a Gnome Desktop to make it work.
        function CheckForSysTray(): boolean;
        {$endif}
    public
        UseTrayMenu : boolean;
        PopupMenuSearch : TPopupMenu;       // we create these dynamically
        PopupMenuTray : TPopupMenu;
        MainTBMenu : TPopupMenu;
                                // Cross Platform, Linux libnotify, others TrayIcon Balloon
        procedure ShowNotification(const Message: string; ShowTime: integer = 3000);
                                // Called by the Sett unit when it knows the true config path.
                                // procedure SetAltHelpPath(ConfigPath: string);

        procedure ShowAbout();
                                { Updates status data on MainForm, tick list }
        procedure UpdateNotesFound(Numb: integer);
                                { Opens a note in single note mode. Pass a full file name, a bool that closes whole app
                                on exit and one that indicates ReadOnly mode. }
        procedure SingleNoteMode(FullFileName: string; const CloseOnExit, ViewerMode : boolean);
                                { Shortcut to SingleNoteMode(Fullfilename, True, False) }
        procedure SingleNoteMode(FullFileName: string);
    end;

                                // This here temp, it returns the singlenotefilename from CLI Unit.
    function SingleNoteFileName() : string;

// See https://github.com/salvadorbs/AsuiteComps/blob/beab429e63a120d9e2e25c55b64dc092e1c271e9/library/platform/unix/Hotkeys.Manager.Platform.pas#L93
{$IFDEF LCLGTK3}
// function gdk_x11_window_get_xid(AX11Window: PGdkWindow): guint32; cdecl; external;
function gdk_x11_display_get_xdisplay(AX11Display: PGdkDisplay): PDisplay; cdecl; external;
{$ENDIF}

var
    MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

uses LazLogger, LazFileUtils, LazUTF8,
    settings, ResourceStr,
    SearchUnit,
    {$ifdef LCLGTK2}
    gtk2, gdk2,          // required to fix a bug that clears clipboard contents at close.
    {$endif}
    {$ifdef LINUX}
    {$ifdef LCLGTK2} gtk2extra, {$endif}         // Relate to testing for SysTray
    {$ifdef LCLQT5} qt5, {$endif}                // Relate to testing for SysTray

    Clipbrd,
    {$endif}   // Stop linux clearing clipboard on app exit.
    Editbox,    // Used only in SingleNoteMode
    Note_Lister, cli, LazVersion,
    tb_utils, {$ifdef LINUX}LCLVersion,{$endif} LCLIntf
    {$ifdef Linux}, libnotify {$endif}
    {$ifdef windows}, registry{$endif} ;

function SingleNoteFileName() : string;
begin
    result := SingleNoteName;                   // Thats the global in CLI Unit
end;

procedure TMainForm.SingleNoteMode(FullFileName: string);
begin
     SingleNoteMode(FullFileName, True, False);
end;

procedure TMainForm.SingleNoteMode(FullFileName : string; const CloseOnExit, ViewerMode : boolean);
var
    EBox : TEditBoxForm;
begin
    if DirectoryExistsUTF8(ExtractFilePath(FullFileName))
        or (ExtractFilePath(FullFileName) = '') then begin
        try
            try
            EBox := TEditBoxForm.Create(Application);
            //EBox.SingleNoteFileName := SingleNoteFileName();    // Thats the global from CLI Unit, via a local helper function
            EBox.NoteTitle:= '';
            EBox.NoteFileName := FullFileName;
            Ebox.TemplateIs := '';
            EBox.Dirty := False;
            if ViewerMode then
                EBox.SetReadOnly(False);
            EBox.ShowModal;
            except on E: Exception do begin debugln('!!! EXCEPTION - ' + E.Message); showmessage(E.Message); end;
            end;
        finally
            try
            FreeandNil(EBox);
            except on E: Exception do debugln('!!! EXCEPTION - What ? no FreeAndNil ?' + E.Message);
            end;
        end;
    end else begin
        DebugLn('Sorry, cannot find that directory [' + ExtractFilePath(FullFileName) + ']');
        showmessage('Sorry, cannot find that directory [' + ExtractFilePath(FullFileName) + ']');
    end;
    if CloseOnExit then Close;      // we also use singlenotemode internally in several places
end;



// -----------------------------------------------------------------
//            S T A R T   U P   T H I N G S
// -----------------------------------------------------------------



procedure TMainForm.FormCreate(Sender: TObject);
begin
    debugln('');                             // this initialises lazlogger for the whole app, don't let it happen in a thread !
    AboutFrm := Nil;
    Randomize;                               // used by sett.getlocaltime()
    //HelpList := Nil;
    UseTrayMenu := true;
    if SingleNoteFileName() = '' then
        StartIPCServer()                     // Don't bother to check if we are client, cannot be if we are here.
    else UseTrayMenu := False;
    {$ifdef LCLCARBON}
    UseTrayMenu := false;
    {$endif}
    if UseTrayMenu then begin
        PopupMenuTray := TPopupMenu.Create(Self);
          TrayIcon.PopUpMenu := PopupMenuTray;      // SearchForm will populate it when ready
//        TrayIcon.Show;                            // Gnome does not like showing it before menu is populated, so, call from SearchForm.create
    end;
    LabelBadNoteAdvice.Caption := '';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
    freeandnil(CommsServer);
 //   freeandnil(HelpNotes);
    //if HelpList <> Nil then writeln('Help List has ' + inttostr(HelpList.Count));
    // freeandnil(HelpList);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  {$ifdef LCLGTK2}
  c: PGtkClipboard;
  t: string;
  {$endif}
  // OutFile : TextFile;
  AForm : TForm;
begin
    //debugln('TMainForm.FormClose - at user request');           // ToDo : remove this
    {$ifdef LCLGTK2}
    c := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
    t := Clipboard.AsText;
    gtk_clipboard_set_text(c, PChar(t), Length(t));
    gtk_clipboard_store(c);
    {$endif}
    Sett.AreClosing:=True;
    if assigned(TheMainNoteLister) then begin
      AForm := TheMainNoteLister.FindFirstOpenNote();
      while AForm <> Nil do begin
          AForm.close;
          AForm := TheMainNoteLister.FindNextOpenNote();
      end;
    end;
end;

procedure TMainForm.CommMessageReceived(Sender : TObject);
Var
    S : String;
begin
    // debugln('Here in Main.CommMessageRecieved, a message was received');
    CommsServer .ReadMessage;
    S := CommsServer .StringMessage;
    if S = 'SHOWSEARCH' then begin
        {$ifdef LINUX}SearchForm.Hide;{$endif}  // Form may be open in another workspace, bring it here.
        SearchForm.Show;                        // this creates a notable flicker, might be necessary
        //SearchForm.MoveWindowHere(SearchForm.Caption);
    end else
        if S.StartsWith('REINDEX') then
            SearchForm.IndexNewNote(copy(S, 9, 100), False)
        else debugln('TMainForm.CommMessageReceived - invalid message [' + S + ']');
        // eg REINDEX:48480CC5-EC3E-4AA0-8C83-62886DB291FD.note
end;

procedure TMainForm.StartIPCServer();
begin
        CommsServer  := TSimpleIPCServer.Create(Nil);
        CommsServer.ServerID:='tomboy-ng' {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif}; // on multiuser system, unique
        CommsServer.OnMessageQueued:=@CommMessageReceived;
        CommsServer.Global:=True;                  // anyone can connect
        CommsServer.StartServer({$ifdef WINDOWS}False{$else}True{$endif});  // start listening, threaded
end;


resourcestring
  rsFailedToIndex = 'Failed to index one or more notes.';

{$ifdef LINUX}
function TMainForm.CheckGnomeExtras() : boolean;

var
    {$ifndef LCLQT5}H : TLibHandle;{$endif}
    MayBeNotGnome : boolean = false;
    PlugInName : string = '';           // holds name if CheckPlugIn found it.


    function CheckPlugIn(EnabledOnly : boolean) : boolean;
    var
        AProcess: TProcess;
        List : TStringList = nil;
    begin
        result := false;
        AProcess := TProcess.Create(nil);
        AProcess.Executable:= 'gnome-extensions';
        AProcess.Parameters.Add('list');
        if EnabledOnly then
            AProcess.Parameters.Add('--enabled');
        //AProcess.Parameters.Add(PlugInName);
        AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
        try
            try
                // Next line raise an exception if gnome-extensions is not installed, it is handled.
                // ExitStatus for List is always zero so don't bother checking
	            AProcess.Execute;
	            List := TStringList.Create;
	            List.LoadFromStream(AProcess.Output);
	            //debugln(List.Text);
                if FindInStringList(List, 'ubuntu-appindicators@ubuntu.com') > -1 then              // Ubuntu, Debian
                    PlugInName := 'ubuntu-appindicators@ubuntu.com';
                if FindInStringList(List, 'appindicatorsupport@rgcjonas.gmail.com') > -1 then       // fedora
                    PlugInName := 'appindicatorsupport@rgcjonas.gmail.com';
            except on E: EProcess do MayBeNotGnome := True;             // Says that gnome-extensions is not installed.
			end;
		 finally
		    freeandnil(List);
			freeandnil(AProcess);
         end;
         result := (PlugInName <> '');
    end;

    function EnablePlugIn() : boolean;
    var
        AProcess: TProcess;
    begin
        result := false;
        AProcess := TProcess.Create(nil);
        AProcess.Executable:= 'gnome-extensions';
        AProcess.Parameters.Add('enable');
        AProcess.Parameters.Add(PlugInName);
        //AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
        try
                // We know we have gnome-extensions command and a valid extension name
	            AProcess.Execute;
		finally
            freeandnil(AProcess);
		end;
        result := CheckPlugIn(True);
	end;
    { First we try and find a SysTray Library, libappindicator3 or libayatana-appindicator3.
    If not available, not much we can do, report and exit;
    We then try for enabling it using Gnome Extensions, if not available its either not installed or not gnome.
    If its there but disabled on Gnome we ask user and enable it.
    }
begin
    // Ayatana is supported instead of Cannonical's appindicator in Laz Trunk post 22/05/2021, r65122
    // Does no harm to check here but if its not in Lazarus, its won't be used when needed.
    result := false;
    {$ifdef LCLGTK2}  // It appears QT5 can talk direct to gnome-shell-extension-appindicator ??
    H := LoadLibrary('libappindicator3.so.1');
    if ('' <> getEnvironmentVariable('LAZUSEAPPIND')) and (H = NilHandle) then
        debugln('===== libappindicator3 is not present');
    {$if (lcl_fullversion>2001200)}       // 2.0.12 - Release Versions of Lazarus before 2.2.0 did not know about libayatana
    if H = NilHandle then begin           // OK, lets try for libayatana
        H := LoadLibrary('libayatana-appindicator3.so.1');    // see https://bugs.freepascal.org/view.php?id=38909
        if ('' <> getEnvironmentVariable('LAZUSEAPPIND')) and (H = NilHandle) then
            debugln('===== libayatana-appindicator3 is not present');
    end;
    {$endif}
    if H = NilHandle then begin
        debugln('Failed to Find an AppIndicator Library, SysTray may not work.');
        exit(False);    // nothing to see here folks.
	end else if ('' <> getEnvironmentVariable('LAZUSEAPPIND')) then
         debugln('===== We have an AppIndicator Library');
	unloadLibrary(H);
    {$endif}                            // end of if not QT5
    Result := CheckPlugIn(True);
    if not Result then
        if MaybeNotGnome then
            debugln('===== SysTray not detected, not Gnome Desktop')
            // We also issue that message on a system that supports libappindicator3 without
            // needing (or installed) gnome-shell-extension-appindicator, eg U20.04 Mate
            // Plasma can use just libappindicator3 or Ayatana by itself.
        else
            if CheckPlugIn(False)  then begin      // Ah, its there but not enabled
                debugln('SysTray Plugin for Gnome detected but not enabled');
                // Offer to enable it for user ??
                if IDYES = Application.MessageBox('tomboy-ng : Enable gnome-shell-extension-appindictor ?   .',
    			        'The SysTray extension is installed but not enabled',
       			        MB_ICONQUESTION + MB_YESNO) then
                        Result := EnablePlugIn();
                if Result then showmessage( 'Enabled, please restart tomboy-ng')
                else showmessage('Sorry, failed to enable plugin');
            end else debugln('SysTray Plugin for Gnome not present');
end;

function TMainForm.CheckForSysTray() : boolean;
var
    A : TAtom;
    XDisplay: PDisplay;
    ForceAppInd : string;
begin
    Result := False;
    if (pos('GNOME', upcase(GetEnvironmentVariable('XDG_CURRENT_DESKTOP'))) > 0) then
        UglyGnome := True;
    // Don't test for SysTray under GTK3, will never be there.  One or other AppIndicator
    // is your only chance. And XInternAtom() function SegVs on Gnome DTs so don't try it.
    // Ayatana is supported instead of Cannonical's appindicator in Laz Trunk
    // post 22/05/2021, r65122 and in Lazarus 2.2.0. Important in Bullseye, not Ubuntu < 21.10

    // Instead of forcing a right click on system tray for just KDE systems, I will have to
    // for ALL Wayland using system. Its only necessary on gnome using wayland if qt has -platform xcb
    // but it appears thats now going to have to be, always, the case, as without xcb we find
    // wayland stops user copy n pasting to other apps.

    if (GetEnvironmentVariable('WAYLAND_DISPLAY') <> '')                   // I believe that this is reliable
        and (not Application.HasOption('allow-leftclick')) then
            NoLeftClickOnTrayIcon := True;

    if (pos('KDE', upcase(GetEnvironmentVariable('XDG_CURRENT_DESKTOP'))) > 0 ) then
        exit(True);
        // So far, every KDE I have tested has a SysTray, but some work badley with left click
        // under wayland in 2023, Deb 13 KDE does not need this but too hard to tell reliably
//    {$ifdef LCLQT6}exit(true);{$endif}                 // Sept '24, noted that Qt6 also has SysTray issues on Gnome, why was this here ?
    {$IFnDEF LCLGTK3}
    // Interestingly, by testing we seem to ensure it does work on U2004, even though the test fails !
    {$ifdef LCLGTK2}XDisplay := gdk_display; {$endif}
    {$if defined(LCLQT5) OR defined(LCLQT6)}
        // If using Gnome and QT5, cannot safely test for a SysTray, we'll guess....
        if pos('GNOME', upcase(GetEnvironmentVariable('XDG_CURRENT_DESKTOP'))) > 0 then
            exit(CheckGnomeExtras());
        // XDisplay := QX11Info_display;           // thats Qt5 only
        XDisplay := xlib.XOpenDisplay(nil);        // that should be any x11 based ...
    {$endif LCLQT5}
    // OK, don't call next line in a KDE system .....
    A := XInternAtom(XDisplay, '_NET_SYSTEM_TRAY_S0', False);  // gtk2 or non-Gnome Qt5
    result := (XGetSelectionOwner(XDisplay, A) <> 0);
    ForceAppInd := GetEnvironmentVariable('LAZUSEAPPIND');
    if ForceAppInd <> '' then
            debugln('Tradition Systray Available = ' + booltostr(result, True));
//    if ForceAppInd = 'YES' then
//            result := false;
    {$ENDIF LCLGTK3}
    // if we are false here, its probably because its a recent Gnome Desktop or GTK3, no SysTray.
    // However, if libappindicator3 or Ayatana is installed and the Gnome Shell Extension,
    // appindicators is installed and enabled, it will 'probably' be OK.

    // Downside of above is we do all the Gnome Tests on non gnome desktops if running GTK3
    if UglyGnome and (result = false) then
            Result := CheckGnomeExtras()
            // Thats libappindicator3 and an installed and enabled gnome-shell-extension-appindicator
     else Result := True;        // Now, that is a hope for the best, GTK3 but non Gnome
end;
{$endif}                            // hides CheckForSystemTray() and CheckGnomeExtras() from non Linux



procedure TMainForm.FormShow(Sender: TObject);
var
    NoteID, NoteTitle : string;
(*    {$ifndef LCLGTK2} {$endif} *) Lab : TLabel;  Butt : TBitBtn;
begin
    TestDarkThemeInUse();
//    {$ifndef LCLGTK2}               // GTK2 seems only one we can be sure is auto colours !
        // We honour --dark-theme for most and if we can guess its dark we'll
        // act accordingly.

        if Sett.DarkThemeSwitch then begin           // If Qt is doing its own colours, let it !
            color := Sett.AltColour;
            font.color := Sett.TextColour;               // These do not work for Windows ?
            for Butt in [ButtMenu, BitBtnQuit, BitBtnHide, ButtSysTrayHelp] do
                Butt.Color := Sett.AltColour;
            //ButtMenu.Color := Sett.AltColour;
            //BitBtnQuit.Color := Sett.AltColour;
            //BitBtnHide.Color := Sett.AltColour;
            // ButtSysTrayHelp.Color := Sett.AltColour;
            for Lab in [Label5, LabelNotesFound, Label3, Label4, LabelBadNoteAdvice, LabelError] do
                TLabel(Lab).Font.Color:= Sett.TextColour;
            CheckBoxDontShow.Font.color := Sett.TextColour;
        end;
//    {$endif}
    if SingleNoteFileName() <> '' then begin      // That reads the global in CLI Unit
        SingleNoteMode(SingleNoteFileName);
        exit;
    end;

    LabelBadNoteAdvice.Caption:= '';
    ButtSysTrayHelp.Visible := False;
    {$ifdef LINUX}
    if not CheckForSysTray() then begin
        LabelBadNoteAdvice.Caption := rsWARNNOSSYSTRAY;
        ButtSysTrayHelp.Visible := True;
        ImageAboutLaz.Visible := False;
    end;
    {$endif}

    if TheMainNoteLister.XMLError then begin
        LabelError.Caption := rsFailedToIndex;
        LabelBadNoteAdvice.Caption:= rsBadNotesFound1;
        ImageAboutLaz.Visible := False;
    end else begin
        LabelError.Caption := '';
        if Application.HasOption('no-splash') or (not Sett.CheckShowSplash.Checked) then
            ButtonDismissClick(Self);
    end;

    (* if Application.HasOption('no-splash') or (not Sett.CheckShowSplash.Checked) then begin
         {if AllowDismiss then} ButtonDismissClick(Self);
     end;  *)
    Left := 10;
    Top := 40;

    CheckBoxDontShow.checked := not Sett.CheckShowSplash.Checked;
    if Sett.CheckShowSearchAtStart.Checked then
        SearchForm.Show;
    if TheMainNoteLister.FindFirstOOSNote(NoteTitle, NoteID) then
        repeat
            SearchForm.OpenNote(NoteTitle, Sett.NoteDirectory + NoteID);
        until TheMainNoteLister.FindNextOOSNote(NoteTitle, NoteID) = false;
    FormResize(self);   // Qt5 apparently does not call FormResize at startup.
    if ButtSysTrayHelp.Visible then debugln('You cannot see me');
    if ButtSysTrayHelp.Visible then debugln('On Gnome, install gnome-shell-extension-appindicator, logout, logon and start tomboy-ng again, "yes" to prompt.');

end;

procedure TMainForm.ImageAboutLazClick(Sender: TObject);
begin
    OpenURL('https://www.lazarus-ide.org/');
end;

procedure TMainForm.LabelErrorClick(Sender: TObject);
begin
    if LabelError.Caption <> '' then
        showmessage(rsBadNotesFound1 + #10#13 + rsBadNotesFound2);
end;



procedure TMainForm.UpdateNotesFound(Numb : integer);
begin
    LabelNotesFound.Caption := rsFound + ' ' + inttostr(Numb) + ' ' + rsNotes;
//         ImageConfigCross.Left := ImageConfigTick.Left;
//     ImageConfigTick.Visible := Sett.HaveConfig;
//     ImageConfigCross.Visible := not ImageConfigTick.Visible;

     ImageNotesDirCross.Left := ImageNotesDirTick.Left;
     ImageNotesDirTick.Visible := Numb > 0;
     ImageNotesDirCross.Visible := not ImageNotesDirTick.Visible;

     ImageSpellCross.Left := ImageSpellTick.Left;
     ImageSpellTick.Visible := Sett.SpellConfig;
     ImageSpellCross.Visible := not ImageSpellTick.Visible;

     ImageSyncCross.Left := ImageSyncTick.Left;

     ImageSyncTick.Visible :=  Sett.ValidSync;
     ImageSyncCross.Visible := not ImageSyncTick.Visible;
end;

procedure TMainForm.ButtonDismissClick(Sender: TObject);
begin
    {$ifdef LCLCOCOA}            // ToDo : is this still necessary ? Or can Cocoa hide like other systems ?
    width := 0;
    height := 0;
    {$else}
    hide();
    {$endif}
end;

procedure TMainForm.ButtSysTrayHelpClick(Sender: TObject);
begin
        SearchForm.ShowHelpNote('systray.note');
end;

procedure TMainForm.CheckBoxDontShowChange(Sender: TObject);
var
    OldMask : boolean;
begin
    if Visible then begin
        Sett.CheckShowSplash.Checked := not Sett.CheckShowSplash.Checked;
        OldMask :=  Sett.MaskSettingsChanged;
        Sett.MaskSettingsChanged := False;
        Sett.SaveSettings(Sender);
        Sett.MaskSettingsChanged := OldMask;
    end;
end;


procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
     if ssCtrl in Shift then begin
       if key = ord('N') then begin
         SearchForm.OpenNote('');     // MMNewNoteClick(self);    OK as long as Notes dir is set
         Key := 0;
         exit();
       end;
     end;
end;

procedure TMainForm.FormResize(Sender: TObject);
var
    MN : integer;
begin
    MN := (Width div 3);
    ButtMenu.Width := MN;
    BitBtnHide.Width := MN;
    ButtSysTrayHelp.Left := 2* MN;
    ButtSysTrayHelp.width := MN;
end;


procedure TMainForm.TestDarkThemeInUse();

    {$ifdef WINDOWS}  function WinDarkTheme : boolean;   // we also need to test in High Contrast mode, its not a colour theme.
    var
        RegValue : integer;
        Registry : TRegistry;
    begin
        Registry := TRegistry.Create;
        try
            Registry.RootKey := HKEY_CURRENT_USER;
            if Registry.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize') then begin
                try
                    RegValue := Registry.ReadInteger('AppsUseLightTheme');
                except on
                    E: ERegistryException do exit(True);  // If Key present but not AppsUseLightTheme, default to Dark
                end;
                exit(RegValue = 0);
            end else
                exit(false);
        finally
            Registry.Free;
        end;
    end;
    {$else}
var
  Col : string;
    {$endif}

begin
    if Application.HasOption('dark-theme') then // Manual override always wins unless its GTK2 (GTK3 ?) !
        {$ifndef LCLGTK2} Sett.DarkThemeSwitch := True {$endif}
    else  begin
        Sett.DarkTheme := false;
        Sett.DarkThemeSwitch := false;
        {$ifdef WINDOWS}
        Sett.DarkTheme := WinDarkTheme();
        {$else}
        // if char 3, 5 and 7 are all 'A' or above, we are not in a DarkTheme
        Col := hexstr(qword(GetRGBColorResolvingParent()), 8);
        Sett.DarkTheme := (Col[3] < 'A') and (Col[5] < 'A') and (Col[7] < 'A');
        {$endif}
    end;
    Sett.SetColours;
end;

{ ------------- M E N U   M E T H O D S ----------------}

procedure TMainForm.ButtonConfigClick(Sender: TObject);
begin
    Sett.Show();
end;

procedure TMainForm.ButtonCloseClick(Sender: TObject);
begin

end;

procedure TMainForm.ButtMenuClick(Sender: TObject);
begin
    MainTBMenu.popup(Left + 40, Top + 40);
end;

procedure TMainForm.BitBtnQuitClick(Sender: TObject);
begin
    Sett.CloseNowPlease();  // might delay exiting app up to 5 seconds while it finishes Sync.
    MainForm.Close;
end;

procedure TMainForm.BitBtnHideClick(Sender: TObject);
begin
    {$ifdef LCLCOCOA}     // ToDo : is this still necessary ? Or can Cocoa hide like other systems ?
    width := 0;
    height := 0;
    {$else}
    hide();
    {$endif}
end;

procedure TMainForm.TrayIconClick(Sender: TObject);   // left click on most systems
begin
    if not NoLeftClickOnTrayIcon then                       // At present, only KDE DE, does sweep up x11 users too !
        PopupMenuTray.PopUp()
    else
        if (not UglyGnome) and Sett.CheckNotifications.Checked  then
            ShowNotification('Please Right Click TrayIcon on Wayland Systems', 2000);
    // I need to do this on Wayland using systems, KDE and xcb using Gnome else we get a two menus !
    // but on Gnome with -platform xcb the left still works ?? So, don't send msg
end;

{procedure TMainForm.RecentMenuClicked(Sender: TObject);
begin
 	if TMenuItem(Sender).Caption <> SearchForm.MenuEmpty then
 		SearchForm.OpenNote(TMenuItem(Sender).Caption);
end;  }


// Linux only uses libnotify, Win and MacOS work through TrayIcon
procedure TMainForm.ShowNotification(const Message : string; ShowTime : integer = 3000);
{$ifdef LINUX}
var
    LNotifier : PNotifyNotification;
begin
    notify_init(argv[0]);
    LNotifier := notify_notification_new (pchar('tomboy_ng'), pchar(Message), pchar('dialog-information'));
    notify_notification_set_timeout(LNotifier, ShowTime);                // figure is mS
    notify_notification_show (LNotifier, nil);
    notify_uninit;
{$else}
begin
    TrayIcon.BalloonTitle := 'tomboy-ng';
    TrayIcon.BalloonHint := rsAutosnapshotRun;
    TrayIcon.BalloonTimeOut := ShowTime;
    Mainform.TrayIcon.ShowBalloonHint;
{$endif}
end;



RESOURCESTRING
    rsAbout = 'tomboy-ng notes - cross platform, sync and manage notes.';
    rsAboutVer = 'Version';
    rsAboutBDate = 'Build date';
    rsAboutCPU = 'TargetCPU';
    rsAboutOperatingSystem = 'OS';


procedure TMainForm.ShowAbout();
var
        Stg : string;
    function GetFPC() : string;
    begin
        Result := '';
        {$ifdef Ver3_2_2} result := ' FPC 3.2.2'; {$endif}
        {$ifdef Ver3_2_3} result := ' FPC 3.2.3'; {$endif}
        {$ifdef Ver3_3_1} result := ' FPC 3.3.1'; {$endif}
        {$ifdef Ver3_2_4} result := ' FPC 3.2.4'; {$endif}
    end;
begin
        if AboutFrm <> Nil then begin
            AboutFrm.Show;
            AboutFrm.EnsureVisible();
            exit;
		end;

//		Stg := rsAbout1 + #10 + rsAbout2 + #10 + rsAbout3 + #10
        Stg := rsAbout + #10 + rsAboutVer + ' ' + Version_String;                    // version is in cli unit.
        {$ifdef LCLCOCOA} Stg := Stg + ', 64bit Cocoa'; {$endif}
        {$ifdef LCLQT5}   Stg := Stg + ', QT5';         {$endif}
        {$ifdef LCLQT6}   Stg := Stg + ', QT6';         {$endif}
        {$ifdef LCLGTK3}  Stg := Stg + ', GTK3';        {$endif}
        {$ifdef LCLGTK2}  Stg := Stg + ', GTK2';        {$endif}
        Stg := Stg + #10 + rsAboutBDate + ' ' + {$i %DATE%}
            + '  (Laz ' + inttostr(laz_major) +'.' + inttostr(laz_minor){ + '.' + inttostr(laz_release)}
            + GetFPC() +')'
            + #10
            + rsAboutCPU + ' ' + {$i %FPCTARGETCPU%} + '  '
            + rsAboutOperatingSystem + ' ' + {$i %FPCTARGETOS%}
            + ' ' + GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
        {$if defined(LCLQT5) or defined(LCLQT6)} Stg := Stg + #10 + 'QT_QPA_PLATFORMTHEME : ' +
        GetEnvironmentVariable('QT_QPA_PLATFORMTHEME')
        + #10 + 'QT_QPA_PLAFORM : ' + GetEnvironmentVariable('QT_QPA_PLATFORM');
        {$endif}
        Stg := Stg + #10 + 'https://github.com/tomboy-notes/tomboy-ng';
        AboutFrm := CreateMessageDialog(Stg, mtInformation, [mbClose]);
        AboutFrm.ShowModal;
        AboutFrm.free;
        AboutFrm := Nil;
        //Showmessage(Stg);
end;

end.

