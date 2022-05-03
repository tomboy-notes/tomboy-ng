unit cli;

{$mode objfpc}{$H+}

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

    This unit is active before the GUI section and may decide GUI is not needed.
    Please see included License file.

    History
    2020/06/18  Remove unnecessary debug line.
    2021/12/28  Tidy LongOpts model.
    2022/01/13  When importing file, don't check for its existance, importer will do that
    2022/01/13  When importing note, check if FFileName starts with '~'
    2022/04/07  Tidy up options.
    2022/05/03  Add unix username to IPC pipe.
}

interface

uses
    Classes, SysUtils, Dialogs;

function ContinueToGUI() : boolean ;

var
    SingleNoteName : string = '';    // other unit will want to know.....


const Version_string  = {$I %TOMBOY_NG_VER};

implementation



uses Forms, LCLProc, LazFileUtils, FileUtil, ResourceStr, simpleipc, IniFiles,
    import_notes, tb_utils;

type
  ENoNotesRepoException = class(Exception);

var
    LongOpts : TStringArray;          // See initialization section

const
    ShortOpts = 'hgo:l:vt:m:n:';  // help gnome3 open lang ver import-[txt md note]

                { If something on commandline means don't proceed, ret True }
function CommandLineError(inCommingError : string = '') : boolean;
var
    ErrorMsg : string;
begin
    ErrorMsg := InCommingError;
    Result := false;
    if ErrorMsg = '' then begin
        ErrorMsg := Application.CheckOptions(ShortOpts, LongOpts);
        if Application.HasOption('h', 'help') then
            ErrorMsg := 'Usage -';
    end;
    if ErrorMsg <> '' then begin
        DebugLn(ErrorMsg);
       {$ifdef DARWIN}
       debugln(rsMachelp1);
       debugln(rsMacHelp2);
       {$endif}
       debugln('   --dark-theme');
       debugln('   -l --lang=CCode               ' + rsHelpLang);    // syntax depends on bugfix https://bugs.freepascal.org/view.php?id=35432
       debugln('   -h --help                     ' + rsHelpHelp);
       debugln('   --version                     ' + rsHelpVersion);
       debugln('   --no-splash                   ' + rsHelpNoSplash);
       debugln('   --debug-sync                  ' + rsHelpDebugSync);
       debugln('   --debug-index                 ' + rsHelpDebugIndex);
       debugln('   --debug-spell                 ' + rsHelpDebugSpell);
       debugln('   --config-dir=PATH_to_DIR      ' + rsHelpConfig);
       debugln('   --open-note=PATH_to_NOTE      ' + rsHelpSingleNote);
       debugln('   --debug-log=SOME.LOG          ' + rsHelpDebug);
       // debugln('   --save-exit                ' + rsHelpSaveExit);    // legacy but still allowed.
       debugln('   --import-txt=PATH_to_FILE     ' + rsHelpImportFile + '  also -t');
       debugln('   --import-md=PATH_to_FILE      ' + rsHelpImportFile + '  also -m');
       debugln('   --import-note=PATH_to_NOTE    ' + rsHelpImportFile + '  also -n');
       debugln('   --title-fname                 ' + rsHelpTitleISFName);
       result := true;
    end;
end;

                        { Ret T if we have ONE or more command line Paramaters, not to be confused with
                          a Option, a parameter has no '-'.  Because the only parameter we expect is SingleNoteFileName,
                          we also honour -o --open-note.  More than one such parameter is an error, report to console,
                          ret true but set SingleFileName to ''. }
function HaveCMDParam() : boolean;
var
    Params : TStringArray;
begin
    Result := False;
    if Application.HasOption('o', 'open-note') then begin
       SingleNoteName := Application.GetOptionValue('o', 'open-note');
       exit(True);
    end;
    Params := Application.GetNonOptions(ShortOpts, LongOpts);
    if length(Params) = 1 then begin
        if Params[0] <> '%f' then begin   // MX Linux passes the %f from desktop file during autostart
                SingleNoteName := Params[0];
                exit(True);
        end;
    end;
    if length(Params) > 1 then begin
        CommandLineError('Unrecognised parameters on command line');
        SingleNoteName := '';
        exit(True);
    end;
end;

// ToDo : remove below, new code does not use TStringList
(* function HaveCMDParam() : boolean;
var
    Params : TStringList;
begin
    Result := False;
    if Application.HasOption('o', 'open-note') then begin
       SingleNoteName := Application.GetOptionValue('o', 'open-note');
       exit(True);
    end;
    Params := TStringList.Create;
    try
        Application.GetNonOptions(ShortOpts, LongOpts, Params);
        if Params.Count = 1 then begin
            if Params[0] <> '%f' then begin   // MX Linux passes the %f from desktop file during autostart
                    SingleNoteName := Params[0];
                    exit(True);
            end;
        end;
        if Params.Count > 1 then begin
            CommandLineError('Unrecognised parameters on command line');
            SingleNoteName := '';
            exit(True);
        end;
    finally
        FreeAndNil(Params);
    end;
end;    *)

// Looks for server, if present, sends indicated message and returns true, else false.
function CanSendMessage(Msg : string) : boolean;
var
    CommsClient : TSimpleIPCClient;
begin
    Result := False;
    try
        CommsClient  := TSimpleIPCClient.Create(Nil);
        CommsClient.ServerID:='tomboy-ng' {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif}; // on multiuser system, unique
        if CommsClient.ServerRunning then begin
            CommsClient.Active := true;
            CommsClient.SendStringMessage(Msg);
            CommsClient.Active := false;
            Result := True;
        end;
    finally
        freeandnil(CommsClient);
    end;
end;

                        { Returns the absolute notes dir, raises ENoNotesRepoException if it cannot find it }
function GetNotesDir() : string;
    var
       ConfigFile : TIniFile;
begin
     // TiniFile does not care it it does not find the config file, just returns default values.
     ConfigFile :=  TINIFile.Create(TB_GetDefaultConfigDir + 'tomboy-ng.cfg');
     try
        result := ConfigFile.readstring('BasicSettings', 'NotesPath', '');      // MUST be mentioned in config file
     finally
         ConfigFile.Free;
     end;
     if Result = '' then
         Raise ENoNotesRepoException.create('tomboy-ng does not appear to be configured');
end;

                        { Will take offered file name, report any errror else checks its a  .note,
                        saves it to normal repo and requests a running instance (if there is one) to
                        reindex as necessary. }
procedure Import_Note;
var
    FFileName, Fname : string;
    GUID : TGUID;
begin
     FFileName := Application.GetOptionValue('n', 'import-note');
     {$ifdef UNIX}
    if FFileName[1] = '~' then
        FFileName := GetEnvironmentVariable('HOME') + FFileName.Remove(0,1);
    {$endif}
     if not FileExists(FFileName) then begin
         debugln('Error, request to import nonexisting file : ' + FFileName);
         exit;
     end;
     if GetTitleFromFFN(FFileName, false) = '' then begin
         debugln('Error, request to import invalid Note file : ' + FFileName);
         exit;
     end;
     if IDLooksOK(ExtractFileNameOnly(FFileName)) then
        FName := ExtractFileName(FFileName)
     else begin
        CreateGUID(GUID);
        FName := copy(GUIDToString(GUID), 2, 36) + '.note';
     end;
     // debugln('About to copy ' + FFileName + ' to ' + FName);
     if CopyFile(FFileName, GetNotesDir() + FName) then
        CanSendMessage('REINDEX')
     else debugln('ERROR - failed to copy ' + FFileName + ' to ' + GetNotesDir() + FName);
end;


                        { Will take offered file name, report errror else converts that file to .note,
                        saves it to normal repo and requests a running instance (if there is one) to
                        reindex as necessary. }
procedure Import_Text_MD_File(MD : boolean);
var
    FFileName : string;
    Importer : TImportNotes;
begin
    if MD then
        FFileName := Application.GetOptionValue('t', 'import-md')
    else FFileName := Application.GetOptionValue('t', 'import-txt');
    Importer := TImportNotes.Create;
    try
        try
            Importer.DestinationDir := GetNotesDir();       // might raise ENoNotesRepoException
            Importer.Mode := 'plaintext';
            if MD then Importer.Mode := 'markdown';
            Importer.FirstLineIsTitle := true;
            if Application.HasOption('f', 'title-fname') then
                Importer.FirstLineIsTitle := false;
            Importer.ImportName := FFileName;
            if Importer.Execute() = 0 then
                debugln(Importer.ErrorMsg);
        except on
            //E: ENoNotesRepoException do begin
            E: Exception do begin
                            debugln(E.Message);
                            exit;
                        end;
        end;
    finally
        Importer.Free;
    end;
    CanSendMessage('REINDEX')
end;

function ContinueToGUI() : boolean ;
begin
    if CommandLineError() then exit(False);
    if Application.HasOption('v', 'version') then begin
        debugln('tomboy-ng version ' + Version_String);
        exit(False);
     end;
    if Application.HasOption('t', 'import-txt') then begin
        Import_Text_MD_File(false);
        exit(False);
    end;
    if Application.HasOption('m', 'import-md') then begin
        Import_Text_MD_File(true);
        exit(False);
    end;
    if Application.HasOption('n', 'import-note') then begin
        Import_Note();
        exit(False);
    end;

    if HaveCMDParam() then
         if SingleNoteName = '' then
            exit(False)                 // thats an error, more than one parameter
         else exit(True);               // proceed in SNM
    // Looks like a normal startup
    if CanSendMessage('SHOWSEARCH') then exit(False);
    Result := true;
end;

initialization
    LongOpts := TStringArray.create(                   // a type, not an object, don't free.
        'dark-theme', 'lang:', 'debug-log:',
        'help', 'version', 'no-splash',
        'debug-sync', 'debug-index', 'debug-spell',
        'config-dir:', 'open-note:', 'save-exit',      // -o for open also legal. save-exit is legecy
        'import-txt:', 'import-md:', 'import-note:',   // -t, -m -n respectivly
        'title-fname', 'gnome3');                      // -g and gnome3 is legal but legacy, ignored.

end.

