unit cmdline;
{ License - see tomboy-ng license information }

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

-----------------------------------------------------------------

New CLI model, if there is any command line entries, we stay in CLI mode. An empty
command line (after the command itself) will proceed to GUI mode.

    -a --action=[note2md; note2txt; md2note; txt2note] - Required

    -s --source=[dir or KEY]   -d --destination= [dir or KEY]

    The default for the above is tomboy-ng's default on this system and the
    current dir (depending on import / export direction).
    Importing always requires a source dir, exporting always requires a
    a destination. Also can use "TOMBOY" or "GNOTE" to use their local defaults.

    -b --notebook="Notebook Name"   For export and import.
    -n --note="Note Title"          For export only, don't use with -b
    -f --file="File Name"           Without path, name of file to import.
    -l --line                       Use first line as title when importing

    -h --help   Show help page.

    Examples -
    TomboyTools --action=note2md --destination=~/Desktop -b "My Notebook"
    Will exports all notes in "My Notebook" from the tomboy-ng note repo
    to the desktop in markdown format.

    TomboyTools -a md2note -D Tomboy -s ~/Desktop -f myMDfile.md -l
    Will import all the markdown files on my desktop to my Tomboy repo using
    the first line as the title.

    TomboyTools -a note2txt -n "Some Title"
    Will export a note from the tomboy-ng repo called "Some Title" as text
    amd place it in the current working directory.

}



    { License - see tomboy-ng license information }

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils;

type TToolMode = (
    ActNote2md, ActNote2txt, ActMd2note, ActTxt2note, ActGUI, ActExit );

{   ModeNotSet,     // No mode setting command line offered, error
    ModeTB2MD,      // Tomboy to Markdown
    ModeTB2Text,    // Tomboy to plain text, line ending as per local
    ModeCheckSync,  // Check the sync repo
    ModeCheckNotes  // Check all notes in the note directory    }


function Finished() : boolean;      // call this from project file, if true we don't need GUI

function GetDefaultNoteDir(OldTB : boolean = false) : string;

implementation

uses LCLProc, Forms, LazFileUtils, export_notes;

procedure ShowHelp();
begin
    debugln('');
    DebugLn('TomboyTools, a small set of tools for tomboy-ng and Tomboy');
    debugln('with no command line options, will start GUI.');
    debugln('');
    debugln(' --help                         Print this, exit');
    debugln(' -a --action=[note2md; note2txt; md2note; txt2note] - Required');
    debugln(' -s --source=[dir or KEY]   -d --destination= [dir or KEY]');
    debugln(' The default for the above is tomboy-ng''s default on this system and the');
    debugln(' current dir (depending on import / export direction).');
    debugln(' Importing always requires a source dir, exporting always requires a');
    debugln(' a destination. Also can use "TOMBOY" or "GNOTE" to use their local defaults.');

    debugln(' -b --notebook="Notebook Name"   For export and import.');
    debugln(' -n --note="Note Title"          For export only, don''t use with -b ');
    debugln(' -f --file="File Name"           Without path, name of file to import.');
    debugln(' -l --line                       Use first line as title when importing');
    debugln(' -V --verbose                    Tells us what its doing');

    debugln(' ');
    debugln('Examples - ');
    debugln(' TomboyTools --action=note2md --destination=~/Desktop -b "My Notebook"');
    debugln(' Will exports all notes in "My Notebook" from the tomboy-ng note repo');
    debugln(' to the desktop in markdown format.');
    debugln(' ');
    debugln(' TomboyTools -a md2note -d TOMBOY -s ~/Desktop -f myMDfile.md -l');
    debugln(' Will import all the markdown files on my desktop to my Tomboy repo using');
    debugln(' the first line as the title.');
    debugln(' ');
    debugln(' TomboyTools -a note2txt -n "Some Title"');
    debugln(' Will export a note from the tomboy-ng repo called "Some Title" as text');
    debugln(' amd place it in the current working directory.');
end;

function GetDefaultNoteDir(OldTB : boolean = false) : string;
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
    if OldTB then
        delete(Result, pos('-ng', Result), 3);
end;

    // must have either a single note title, ID or --all-notes
procedure ExportSomeNotes(Mode : TToolMode);
var
    Exporter : TExportNote;
    SrcDir, DestDir : string;
begin
    // when exporting, default src is tomboy-ng's repo. Default destination is current dir.
    if Application.HasOption('s', 'source') then begin
        SrcDir := Application.GetOptionValue('s', 'source');
        if SrcDir = 'TOMBOY' then
            SrcDir := GetDefaultNoteDir(True);
    end else
        SrcDir := GetDefaultNoteDir();
    if Application.HasOption('d', 'destination') then
        DestDir := Application.GetOptionValue('d', 'destination')
    else
        DestDir := GetCurrentDirUTF8();
    if not DirectoryIsWritable(DestDir) then begin
        debugln('ERROR, cannot write to ' + DestDir);
        exit();
    end;
    // Hmm, maybe check if there are notes in sourcedir ?
    Exporter := TExportNote.Create;
    try
        // Firstly, what are we exporting ?
        if Application.HasOption('n', 'note') then                              // all-notes is implied by Title and Notebook being empty
            Exporter.NoteTitle := Application.GetOptionValue('n', 'note')       // we don't bother with exporting notes by filename
        else if Application.HasOption('b', 'notebook') then
            Exporter.Notebook := Application.GetOptionValue('b', 'notebook');
        Exporter.NoteDir := AppendPathDelim(SrcDir);
        Exporter.DestDir:= appendpathdelim(DestDir);
        // What format should they be executed in ?
        if Mode = ActNote2Txt then
                Exporter.OutFormat := 'text'
        else Exporter.OutFormat := 'md';
        if Application.HasOption('V', 'verbose') then begin
            debugln('Exporting from ' + SrcDir + ' to ' + DestDir);
            debugln('Notebook   : ' + Exporter.Notebook);
            debugln('Note Title : ' + Exporter.NoteTitle);
        end;
        exporter.Execute;
	finally
        Exporter.Free;
	end;
end;

function CheckCmdOptions() : TToolMode;        // ret an action from TToolMode
var
    CmdLineErrorMsg : string;
    Action : string;
begin
    result := ActExit;
    if ParamCount = 0 then exit(ActGUI);         // no parameters, must be a GUI app
    CmdLineErrorMsg := Application.CheckOptions('Vha:s:d:b:n:f:l', 'verbose help action: source: destination: notebook: note: file: line');
    if CmdLineErrorMsg <> '' then begin
        debugln( CmdLineErrorMsg);
        showhelp();
        exit(ActExit);
	end;
    if Application.HasOption('help') then begin
        showhelp();
        exit(ActExit);
	end;
    // OK, its got cmd parameters without errors but is it correct ?
    if Application.HasOption('a', 'action') then
        Action := Application.GetOptionValue('a', 'action')
    else begin
        showhelp();
        exit(ActExit);
	end;
    // OK, we have an action, is it a valid one ?
    case Action of
        'note2md'  : Result := ActNote2md;
        'note2txt' : Result := ActNote2txt;
        'md2note'  : Result := ActMd2note;
        'txt2note' : Result := ActTxt2note;
    else begin
            showhelp();
            exit(ActExit);
        end;
    end
end;

function Finished()  : boolean;
var
  AppMode : TToolMode = ActGUI;

begin
    result := true;
    AppMode := CheckCmdOptions();
    if AppMode = ActGUI then exit(False);
    if AppMode = ActExit then exit(True);
    // OK, we are here to do stuff, lets do it !
    if AppMode in [ActNote2md, ActNote2Txt] then
        ExportSomeNotes(AppMode);
end;

end.

