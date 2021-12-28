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

var
    LongOpts : array [0..12] of string = (
            'dark-theme', 'lang:', 'debug-log:',
            'help', 'version', 'no-splash',
            'debug-sync', 'debug-index', 'debug-spell',
            'config-dir:', 'open-note:', 'save-exit',    // -o for open also legal
            'gnome3');                                   // -g and gnome3 is legal but legacy, ignored.


                { If something on commandline means don't proceed, ret True }
function CommandLineError(inCommingError : string = '') : boolean;
var
    ErrorMsg : string;
begin
    ErrorMsg := InCommingError;
    Result := false;
    if ErrorMsg = '' then begin
        ErrorMsg := Application.CheckOptions('hgo:l:v', LongOpts);   // Allowed single letter switches, help, gnome3, open, language, version
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
       debugln('   --lang=CCode       ' + rsHelpLang);    // syntax depends on bugfix https://bugs.freepascal.org/view.php?id=35432
       debugln('   -h --help                  ' + rsHelpHelp);
       debugln('   --version                  ' + rsHelpVersion);
       debugln('   --no-splash                ' + rsHelpNoSplash);
       debugln('   --debug-sync               ' + rsHelpDebugSync);
       debugln('   --debug-index              ' + rsHelpDebugIndex);
       debugln('   --debug-spell              ' + rsHelpDebugSpell);
       debugln('   --config-dir=PATH_to_DIR   ' + rsHelpConfig);
       debugln('   --open-note=PATH_to_NOTE   ' + rsHelpSingleNote);
       debugln('   --debug-log=SOME.LOG       ' + rsHelpDebug);
       debugln('   --save-exit                ' + rsHelpSaveExit);
       result := true;
    end;
end;

                        { Ret T if we have ONE or more command line Paramaters, not to be confused with
                          a Option, a parameter has no '-'.  Because the only parameter we expect is SingleNoteFileName,
                          we also honour -o --open-note.  More than one such parameter is an error, report to console,
                          ret true but set SingleFileName to ''. }
function HaveCMDParam() : boolean;
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
    if Application.HasOption('v', 'version') then begin
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

