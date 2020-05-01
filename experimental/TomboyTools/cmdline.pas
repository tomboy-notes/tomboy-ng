unit cmdline;

{
    The function 'Finished' is called before any forms are created, if it decides
    that the GUI is not needed, it will manage everything.
    Finished() will retune True if
        * there has been a command line error
        * help is on command line
        * there was enough commandline to tell us what to do and it was done
    Finished() will return False if
        * There were no command line entries.

    commandline options -
    --help      print this, exit
    --tb2md     convert Tomboy to Markdown
    --tb2text   convert Tomboy to plain text

    --note=NoteTitle
    --all-notes
    --tb-dir=dir    default for system if not used
    --dest-dir      current dir if not used

    --check-sync=repo   report on sync repo
    --check-notes



}

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils;

type TToolMode = (
    ModeNotSet,     // No mode setting command line offered, error
    ModeTB2MD,      // Tomboy to Markdown
    ModeTB2Text,    // Tomboy to plain text, line ending as per local
    ModeCheckSync,  // Check the sync repo
    ModeCheckNotes  // Check all notes in the note directory
);

function Finished() : boolean;      // call this from project file, if true we don't need GUI

implementation

uses LCLProc, Forms, LazFileUtils, export_notes;

procedure ShowHelp();
begin
    debugln('');
    DebugLn('TomboyTools, a small set of tools for tomboy-ng and Tomboy');
    debugln('');
    debugln(' --help                         Print this, exit');
    debugln('What do we want to do ? specify export md or text, check sync or notes -');
    debugln(' --tb2md                        Convert Tomboy to Markdown');
    debugln(' --tb2text                      Convert Tomboy to plain text');
    debugln(' --check-sync=repo              Report on sync repo');
    debugln(' --check-notes                  Report on notes');
    debugln('When exporting from Tomboy, spec one of "all", Title or Filename  -');
    debugln(' --note-title="Note Title"      Export one note');
    debugln(' --note-filename=FileName       Filename without path');
    debugln(' --all-notes                    Export all notes');
    debugln(' --tb-dir=dir                   Optional, default is default for system');
    debugln(' --dest-dir                     Optional, default is current dir');
    debugln('');
end;

function GetDefaultNoteDir : string;
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

    // must have either a single note title, ID or --all-notes
procedure ExportSomeNotes(Mode : TToolMode);
var
    Exporter : TExportNote;
begin
    Exporter := TExportNote.Create;
    try
        // Firstly, what are we exporting ?
        if Application.HasOption('note') then
            Exporter.NoteTitle := Application.GetOptionValue('note')
        else if Application.HasOption('note-filename') then
            Exporter.NoteFileName := Application.GetOptionValue('note-filename')
            else if Application.HasOption('all-notes') then
                Exporter.AllNotes := True
                    else begin
                        writeln('ERROR : Exactly what notes do you want to export ? ');
                        exit;
					end;
        // And where from and where to ?
        if  Application.HasOption('tb-dir') then
            Exporter.NoteDir := AppendPathDelim(Application.GetOptionValue('tb-dir'))
        else  Exporter.NoteDir := GetDefaultNoteDir();
        if Application.HasOption('dest-dir') then
            Exporter.DestDir:= appendpathdelim(Application.GetOptionValue('dest-dir'))
        else  Exporter.DestDir:= appendpathdelim(GetCurrentDir());
        if not DirectoryIsWritable(Exporter.DestDir) then begin
            debugln('ERROR : Cannot write to ' + Exporter.DestDir);
            exit;
		end;
        // What format should they be executed in ?
        if Mode = ModeTB2Text then
                Exporter.Mode := 'text'
        else Exporter.Mode := 'md';
        exporter.Execute;
	finally
        Exporter.Free;
	end;
end;


function Finished()  : boolean;
// We assume no non-option parameters, only things we accept are defined in this string
// nor do we have single char options
var
  CmdLineErrorMsg : string;
  AppMode : TToolMode = ModeNotSet;
begin
    if ParamCount = 0 then exit(false)         // no parameters, must be a GUI app
    else Result := True;
    CmdLineErrorMsg := Application.CheckOptions('', 'help tb2md tb2text note-filename: check-sync: check-notes note: all-notes tb-dir: dest-dir:');
    if CmdLineErrorMsg <> '' then begin
        debugln( CmdLineErrorMsg);
        showhelp();
        exit;
	end;
    if Application.HasOption('help') then begin
        showhelp();
        exit;
	end;
    // if to here, we know there were no errors and help was not there.  Maybe nothing there ?
    if Application.HasOption('tb2md') then
        AppMode := ModeTB2MD
    else if  Application.HasOption('tb2text') then
            AppMode := ModeTB2Text
        else  if  Application.HasOption('check-sync') then
                AppMode := ModeCheckSync
            else if Application.HasOption('check-notes') then
                    AppMode := ModeCheckNotes;
    case AppMode of
        ModeNotSet : begin
            showhelp();
            exit;
	    end;
        ModeCheckSync, ModeCheckNotes : begin
	        debugln('Sorry, that mode not implemented yet');
	        exit;
        end;
        ModeTB2MD, ModeTB2Text : begin
            ExportSomeNotes(AppMode);
            exit;
		end;
	end;
end;

end.

