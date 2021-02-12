unit cmdline;
{ License - see tomboy-ng license information }

{
    The function 'Finished' is called before any forms are created, if it decides
    that the GUI is not needed, it will manage everything.
    Finished() will return True if
        * there has been a command line error
        * help is on command line
        * there was enough commandline to tell us what to do and it was done
    Finished() will return False if
        * There were no command line entries.


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
    -f --file="File Name"           Without path, name of file to import.     // why do we deal with single notes ???
    -t --title-line                 Use first line as title when importing

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


    Note Title and File Names
    -------------------------
    The default, no relevent option, is -
    Import - Txt or Md file name becomes Title, Underscore to space.
             Set FirstLineIsTitle to True
    Export - Title becomes filename, add 'txt' or 'md', get rid of char unsuited
             to a filename, spaces and bad char all become underscore.
             Title is inserted as first line of new file, if md, marked as such.
             Set FileNameIsTitle to True


    If -t title-line is set, then we do something different -
    Import - Note title is the first line of the input file. In md, thats a title usually
             Set FirstLineIsTitle to False
    Export - File name is retained, probably an ID ?  But not always. Title is
             used as first line of the file, if md, its marked as a title.
             Set FileNameIsTitle to False

    Cleanup - no more than one consecuitive space in title.
            - When importing, if the first line of note content is same as title
              then remove the duplicate.

}



    { License - see tomboy-ng license information }

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils;

type TToolMode = (
    ActNote2md, ActNote2txt, ActMd2note, ActTxt2note, ActGUI, ActExit,
    ActNote2POT    // convert note to a pot file for translation
    );

{   ModeNotSet,     // No mode setting command line offered, error
    ModeTB2MD,      // Tomboy to Markdown
    ModeTB2Text,    // Tomboy to plain text, line ending as per local
    ModeCheckSync,  // Check the sync repo
    ModeCheckNotes  // Check all notes in the note directory    }


                // call this from project file, if true we don't need GUI
                // It is, in a way, the MAIN function, it decides are we GUI or
                // console app.
function Finished() : boolean;

function GetDefaultNoteDir(OldTB : boolean = false) : string;

implementation

uses LCLProc, Forms, LazFileUtils, FileUtil, export_notes, import_notes, note2po;

procedure ShowHelp();
begin
    debugln('');
    DebugLn('TomboyTools, a small set of tools for tomboy-ng (and Tomboy)');
    debugln('With no command line options, will start GUI.');
    debugln('');

    debugln(' -a --action=[note2md; note2txt; md2note; txt2note, note2pot] - Required');
    debugln(' -s --source=[dir or KEY]   -d --destination= [dir or KEY]');
    debugln(' The default for the above is tomboy-ng''s default on this system and the');
    debugln(' current dir (depending on import / export direction). Importing always');
    debugln(' requires a source dir, exporting always requires a destination');
    debugln(' Also can use a KEY, "TOMBOY" or "GNOTE", to use their local defaults.');

    debugln(' -b --notebook="Notebook Name"   For export and import.');
    debugln(' -n --note="Note Title"          For export only, don''t use with -b ');
    debugln(' -f --file="File Name"           Without path, name of file to import.');
    //debugln(' -l --line                       Use first line as title when importing.');

    debugln(' -t --title-line   Retain filename, Note Title is first line of a file.');
    debugln(' -V --verbose      Tells us what its doing');
    debugln(' --help            Print this, exit');

    debugln(' ');
    debugln('Examples - ');
    debugln(' TomboyTools --action=note2md --destination=~/Desktop -b "My Notebook"');
    debugln(' Will exports all notes in "My Notebook" from the tomboy-ng note repo');
    debugln(' to the desktop in markdown format.');
    debugln(' ');
    debugln(' TomboyTools -a md2note -d TOMBOY -s ~/Desktop -f myMDfile.md -t');
    debugln(' Will import all the markdown files on my desktop to my Tomboy repo using');
    debugln(' the first line as the title.');
    debugln(' ');
    debugln(' TomboyTools -a note2txt -n "Some Title"');
    debugln(' Will export a note from the tomboy-ng repo called "Some Title" as text');
    debugln(' amd place it in the current working directory.');
    debugln(' ');
    debugln(' TomboyTools -a txt2note -b "Special Notes"');
    debugln(' Will import all the text files in the current directory into tomboy-ng.');
    debugln(' The file names (without extension) will be used for the note titles.');
    debugln(' Notes will be put in Special Notes notebook which MUST exist.');
    debugln(' ');
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
    ExPot : TExportPOT;
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
    if Mode = ActNote2POT then begin
        ExPot := TExportPOT.Create('tomboy-ng.note');
        if ExPot.ErrorString <> '' then
            debugln('EXport tp POT ' + ExPot.ErrorString);
        ExPot.Free;
        exit;
    end;
    Exporter := TExportNote.Create;
    try
        // Firstly, what are we exporting ?
        if Application.HasOption('n', 'note') then                              // all-notes is implied by Title and Notebook being empty
            Exporter.NoteTitle := Application.GetOptionValue('n', 'note')       // we don't bother with exporting notes by filename
        else if Application.HasOption('b', 'notebook') then
            Exporter.Notebook := Application.GetOptionValue('b', 'notebook');
        Exporter.FileNameIsTitle:= not Application.HasOption('t', 'title-line');
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
    CmdLineErrorMsg := Application.CheckOptions('Vha:s:d:b:n:f:t', 'verbose help action: source: destination: notebook: note: file: title-line');
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
        'note2pot' : result := ActNote2POT;
    else begin
            showhelp();
            exit(ActExit);
        end;
    end
end;


procedure ImportSomeFiles(Mode : TToolMode) ;
var
    SrcDir, DestDir : string;
    FileList : TStringList;
    Importer : TImportNotes;
begin
    // when importing default destination is tomboy-ng's repo. Default source dir is current dir.
    if Application.HasOption('s', 'source') then
        SrcDir := Application.GetOptionValue('s', 'source')
    else
        SrcDir := GetCurrentDirUTF8();
    if Application.HasOption('d', 'destination') then begin
        DestDir := Application.GetOptionValue('d', 'destination');
        if DestDir = 'TOMBOY' then  DestDir := GetDefaultNoteDir(True);
    end else
        DestDir := GetDefaultNoteDir();
    if not DirectoryIsWritable(DestDir) then begin
        debugln('ERROR, cannot write to ' + DestDir);
        exit();
    end;
    FileList := FindAllFiles(SrcDir, '*.txt; *.text', False);
    debugln(FileList.Text);
    Importer := TImportNotes.create;
    try
        Importer.DestinationDir:= DestDir;
        Importer.ImportNames := FileList;
        if Mode = ActTxt2Note then
            Importer.Mode := 'plaintext';
        if Mode = ActMd2Note then
            Importer.Mode := 'markdown';

        if Application.HasOption('b', 'notebook') then
            Importer.NoteBook:= Application.GetOptionValue('b', 'notebook');

        if Application.HasOption('t', 'title-line') then
            Importer.FirstLineIsTitle:= True;;
        Importer.Execute();
    finally
        Importer.Free;
        FileList.Free;
    end;
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
    if AppMode in [ActNote2md, ActNote2Txt, ActNote2POT] then
        ExportSomeNotes(AppMode);
    if AppMode in [ActTxt2note, ActMd2Note] then
        ImportSomeFiles(AppMode);
end;

end.

