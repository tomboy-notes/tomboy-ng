unit cli;

{$mode objfpc}{$H+}

{  Copyright (C) 2018 David Bannon

   This unit is active before the GUI section and may decide GUI is not needed.
   Please see included License file.


    History
    2020/06/18  Remove unnecessary debug line.
}

interface

uses
    Classes, SysUtils, Dialogs;

function ContinueToGUI() : boolean ;

var
    SingleNoteName : string = '';    // other unit will want to know.....


const Version_string  = {$I %TOMBOY_NG_VER};

implementation


uses Forms, LCLProc, LazFileUtils, ResourceStr, simpleipc;



                { If something on commandline means don't proceed, ret True }
function CommandLineError(inCommingError : string = '') : boolean;
// WARNING - the options here MUST match the options list in HaveCMDParam() below
var
    ErrorMsg : string;
begin
    ErrorMsg := InCommingError;
    Result := false;
    if ErrorMsg = '' then begin
        ErrorMsg := Application.CheckOptions('hgo:l:', 'lang: debug-log: dark-theme no-splash version help gnome3 open-note: debug-spell debug-sync debug-index config-dir: save-exit');
        if Application.HasOption('h', 'help') then
            ErrorMsg := 'Usage -';
    end;
    if ErrorMsg <> '' then begin
        DebugLn(ErrorMsg);
       {$ifdef DARWIN}
       debugln(rsMachelp1);
       debugln(rsMacHelp2);
       {$endif}
       {$ifdef WINDOWS}debugln('   --dark-theme'); {$endif}
       //debugln('   --delay-start                ' + rsHelpDelay);
       debugln('   -l CCode  --lang=CCode       ' + rsHelpLang);    // syntax depends on bugfix https://bugs.freepascal.org/view.php?id=35432
       debugln('   --debug-log=SOME.LOG         ' + rsHelpDebug);
       debugln('   -h --help                    ' + rsHelpHelp);
       debugln('   --version                    ' + rsHelpVersion);
       // debugln('   -g --gnome3                  ' + rsHelpRedHat);   // must permit its use but does nothing, not needed.
       debugln('   --no-splash                  ' + rsHelpNoSplash);
       debugln('   --debug-sync                 ' + rsHelpDebugSync);
       debugln('   --debug-index                ' + rsHelpDebugIndex);
       debugln('   --debug-spell                ' + rsHelpDebugSpell);
       debugln('   --config-dir=PATH_to_DIR     ' + rsHelpConfig);
       debugln('   -o --open-note=PATH_to_NOTE  ' + rsHelpSingleNote);
       debugln('   --save-exit                  ' + rsHelpSaveExit);
       result := true;
    end;
end;

                        { Ret T if we have ONE or more command line Paramaters, not to be confused with
                          a Option, a parameter has no '-'.  Because the only parameter we expect is SingleNoteFileName,
                          we also honour -o --open-note.  More than one such parameter is an error, report to console,
                          ret true but set SingleFileName to ''. }
function HaveCMDParam() : boolean;
    // WARNING - the options here MUST match the options list in CommandLineError() above
    { ToDo : put options in a TStringList or a set and share, less mistakes ....}
var
    Params : TStringList;
    LongOpts : array [1..12] of string = ('dark-theme', 'lang:', 'debug-log:', 'no-splash', 'version', 'gnome3', 'debug-spell',
            'debug-sync', 'debug-index', 'config-dir:','open-note:', 'save-exit');

begin
    Result := False;
    if Application.HasOption('o', 'open-note') then begin
       SingleNoteName := Application.GetOptionValue('o', 'open-note');
       //UseTrayMenu := False;
       exit(True);
    end;
    Params := TStringList.Create;
    try
        Application.GetNonOptions('hgo:', LongOpts, Params);
        {for I := 0 to Params.Count -1 do
            debugln('Extra Param ' + inttostr(I) + ' is ' + Params[I]);  }
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
end;

function AreWeClient() : boolean;
var
    CommsClient : TSimpleIPCClient;
begin
    Result := False;
    try
        CommsClient  := TSimpleIPCClient.Create(Nil);
        CommsClient.ServerID:='tomboy-ng';
        if CommsClient.ServerRunning then begin
            CommsClient.Active := true;
            CommsClient.SendStringMessage('SHOWSEARCH');
            CommsClient.Active := false;
            Result := True;
        end;
    finally
        freeandnil(CommsClient);
    end;
end;


function ContinueToGUI() : boolean ;
begin
    if CommandLineError() then exit(False);
    if Application.HasOption('version') then begin
        debugln('tomboy-ng version ' + Version_String);
        exit(False);
     end;
    if HaveCMDParam() then
         if SingleNoteName = '' then
            exit(False)                 // thats an error, more than one parameter
         else exit(True);               // proceed in SNM
    // Looks like a normal startup
    if AreWeClient() then exit(False);
    Result := true;
end;

end.

