unit import_notes;

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------
}

{ Will import either plain text or markdown converting content to note.
  Set DestinationDir, Mode (plaintext, markdown) and optionally FirstLineIsTitle
  (other wise, file name will beome title).
  Then pass a List of full file names, that is, including a path and extension, to convert.

  Things that must be done -

    1.  Notebook, a string that optionally contains the name of a notebook that will be
        assigned to each imported note. This will involve looking to see if there is
        already a notebook of this name, if not creating necessary files.
        Does not make sense if destination dir is not either Tomboy or tomboy-ng.

    2.  On completion, send tomboy-ng a hup if its running so it knows to refresh.

HISTORY :
    2021/08/19   Rewrite much of md import. More use of St.Replace() model.

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    { TImportNotes }

    TImportNotes = class
    private
//        function ChangeBold(var St: string): boolean;
//        function ChangeItalic(var St: string): boolean;
        function ChangeSmallFont(var St: string): boolean;
        function ChangeTag(var St: string; const ChangeFrom, ChangeToLead,
            ChangeToTrail: string): boolean;
        procedure DoLineHeadings(const STL: TStringList);
        function ImportFile(FullFileName: string): boolean;
        function MarkUpMarkDown(Cont: TStringList): boolean;
                            {  Returns the 1 based pos of the passed Tag, Leading says its a leading tag
                               must have whitespace of newline to left and an alpha mumeric to the right.
                               Reverse if Leading is false.  Ret 0 if a suitable tag is not found. }
        function PosMDTag(const St, Tag: string; StartAt: integer;
            const leading: boolean): integer;
                            // Gets passed a List with note content, puts an appropriate
                            // header and footer on.
        function ProcessPlain(Cont: TStringList; const Title: string; LCD : string = '';
                    CDate: string = ''): boolean;

    public
        ErrorMsg : string;              // '' if everything OK, content means something bad happened
        DestinationDir : string;        // Required, dir to save notes to
        Mode : string;                  // ie plaintext, markdown ....
        ImportNames : TStringList;      // A list of full file names to import, default is filename will become title
        FirstLineIsTitle : boolean;     // if true, first line of note becomes title
        KeepFileName : boolean;         // The note will have same base name as import.
        NoteBook : string;              // Maybe empty, if not, imported notes will go into this notebook.  ToDo : make this work
        function Execute(): integer;    // you know all you need, go do it.
        // Alt action for this Unit, converts a StringList that contains
        // markdown to a Note, no file i/o happens, note is returned in
        // the same stringlist. If LCD, CDate are '' then defaults are used.
        function MDtoNote(Content: TStringList; const LCD, CDate: string): boolean;
        constructor Create;
        destructor Destroy; override;
end;



implementation

{ TImportNotes }

uses LazFileUtils, LazUTF8, LCLProc, TB_utils;

function TImportNotes.ProcessPlain(Cont: TStringList; const Title: string;
    LCD: string; CDate : string): boolean;
//var
    //DateSt : string;        // eg '2020-05-19T18:58:37.9513193+10:00';
begin
    if LCD = '' then LCD := TB_GetLocalTime();
    if CDate = '' then CDate := TB_GetLocalTime();
    //LCDateSt := TB_GetLocalTime();
    Cont.Insert(0, '	<text xml:space="preserve"><note-content version="0.1">' + Title);
    Cont.Insert(0, '	<title>' + Title + '</title>');
    Cont.Insert(0, '<note version="0.3" xmlns:link="http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size" xmlns="http://beatniksoftware.com/tomboy">');
    Cont.Insert(0, '<?xml version="1.0" encoding="utf-8"?>');
    Cont.Add('    </note-content></text>');
    Cont.Add('      <last-change-date>' + LCD + '</last-change-date>');
    Cont.Add('      <last-metadata-change-date>' + LCD + '</last-metadata-change-date>');
    Cont.Add('      <create-date>' + CDate + '</create-date>');
    Cont.Add('      <cursor-position>1</cursor-position>');
    Cont.Add('      <width>1000</width>');
    Cont.Add('      <height>626</height>');
    Cont.Add('    	<x>20</x>');
    Cont.Add('    	<y>30</y>');
    Cont.Add('      <tags>');

    if NoteBook <> '' then
        Cont.Add('        <tag>system:notebook:' + NoteBook + '</tag>');

    Cont.Add('      </tags>');
    Cont.Add('    	<open-on-startup>False</open-on-startup>');
    Cont.Add('</note>');
    result := True;
end;

{     Markdown Rules

A line starting with a asterik and a space is a bullet.
Bold text is wrapped in ** at either end. Or __ (that is two underscores) at either end.
Italics is wrapped in * at either end. Again, the underscore at either end will work as well.
Highlight is wrapped in ~~ at either end,
backticks, ` at either end will make code, use for monospace text.
Stikeout is not supported.
a line starting with ###space  is a  bold, large line
a line starting with ##space is a bold, huge line
A line that is followed by some ===== or ------ are headings, huge and Large
we ignore #space, have other ways of finding title.

}

// Iterates over list looking for a line that is either "------" or "======"  and if
// it finds one, removes that line and makes line ABOVE a header.
// Setext - one or more = or - with up to three leading spaces and any number of training whitespace
// Sigh ....

procedure TImportNotes.DoLineHeadings(const STL : TStringList);
var
    i : integer = 1;            // Line 0 is the heading

    // Ret true if passed line is a SeText line, https://spec.commonmark.org/
    function IsSeText(const St : string; Se : char) : boolean;
    var
        j : integer = 1;
        SeCount : integer = 0;
    begin
        Result := false;
        while j <= St.length do begin
            if St[j] = Se then begin
                inc(SeCount);
                inc(j);
                continue;
            end;
            // If its not a Se, nor whitespace, cannot be a heading.
            if (not (St[j] in [ ' ', #10, #13])) then exit(false);
            // only allowed 3 spaces at left
            if (J > 3) and (SeCount = 0) then exit(false);
            inc(j);
        end;
        result := SeCount > 0;
    end;
                    // Will remove current line and make previous line a Heading.
    procedure MakeHeading(IsHuge : boolean);
    var
        St : string;
    begin
        StL.Delete(i);
        if i > 1 then begin
            St := StL[i-1];
            if IsHuge then
                St := '<size:huge>' + St + '</size:huge>'
            else St := '<size:large>' + St + '</size:large>';
            StL.Delete(i-1);
            StL.Insert(i-1, St);
        end;
    end;

begin
    while I < STL.Count do begin        // Must be while, we will alter count as we go
        if IsSeText(Stl[i], '=') then
            MakeHeading(True)
        else if IsSeText(Stl[i], '-') then
                MakeHeading(False)
            else inc(i);
    end;
end;


{ A MD start tag must be at start of line or have whitespace in front and something at the end
that is not whitespace.

And end tag cannot be at start of line, must have a md start tag (and content?) before it
and must have whitespace or newline at the end. Interestingly, here we allow anything between
#32 and '/' to end a tag, I wonder why ?

We count the appropriate angle bracket as whitespace here as it must be a valid XML tag. }

function  TImportNotes.PosMDTag(const St, Tag : string; StartAt : integer; const leading : boolean) : integer;
var
    Stage : integer = 0;
begin
    if Leading then begin
        while Stage < length(St) do begin
            Result :=  St.IndexOf(Tag, Stage);                                // zero based !
            if Result = -1 then exit;                                         // no more candidates
            if (Result = 0)                                                   // Start of line
                    or (St[Result] in [' '..'/', '>']) then                   // has whitespace before tag
                if St[Result+length(Tag)+1] <> ' ' then begin                 // is not followed by whitespace
                    //writeln('LEAD tag=' + tag + ' res=' + inttostr(Result) + ' [' + St + ']');
                    exit(Result+1);
                end;
            Stage := Result+1;
        end;
    end else begin
        Stage := StartAt;                                                     // skip past any end tag we found.
        while Stage < length(St) do begin
            Result :=  St.IndexOf(Tag, Stage);                                // zero based !
            if Result = -1 then exit;
            if  (((Result+length(tag)) >= Length(St))                         // last thing in the line
                    or (St[Result+length(Tag)+1] in [' '..'/', '<'])) then    // has a whitespace or newline afterwards
                if St[Result] <> ' ' then begin                               // must have something that is not whitespace                   // thats before tag
                    //writeln('TRAL tag=' + tag + ' res=' + inttostr(Result) + ' [' + St + ']');
                    exit(Result+1);
                end;
            Stage := Result+1;
        end;
    end;
    Result := -1;
end;

function TImportNotes.ChangeSmallFont(var St : string)  : boolean;
begin
    // MD looks like this <sub>small font</sub> but by time we get here, the angle brackets have been munged.
    St := St.Replace('&lt;sub&gt;', '<size:small>', [rfReplaceAll]);
    St := St.Replace('&lt;/sub&gt;', '</size:small>', [rfReplaceAll]);
    exit(false);
end;


    // Changes any symetrical MD tag to corresponding leading and trailing Tomboy tags
function TImportNotes.ChangeTag(var St : string; const ChangeFrom, ChangeToLead, ChangeToTrail :  string)  : boolean;
var
    PosStart, PosEnd, FromLength : integer;
begin
    Result := False;
    FromLength := length(ChangeFrom);
    PosStart := PosMDTag(St, ChangeFrom, 0, True);
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, ChangeFrom, PosStart+1, False);
        if (PosEnd > 0) then begin
            //writeln('Changing, PosStart=' + inttostr(PosStart) + ' PosEnd =' + inttostr(PosEnd) + ' [' + St + ']');
            delete(St, PosEnd, FromLength);
            insert(ChangeToTrail, St, PosEnd);
            delete(St, PosStart, FromLength);
            insert(ChangeToLead, St, PosStart);
            result := True;
        end;
    end;
end;


//  <size:huge><bold>huge heading</bold></size:huge><size:small>

function TImportNotes.MarkUpMarkDown(Cont : TStringList) : boolean;
var
    Index : integer = 0;
    St : string;
    DropNewLine : boolean = True;
begin
    Result := True;
    while Index < Cont.Count do begin
        St := Cont.Strings[Index];
        if (St = '') then begin
            if DropNewLine then begin
                Cont.Delete(Index);
                DropNewLine := False;
                continue;
            end else DropNewLine := True;
        end else DropNewLine := True;
//        DebugLn('Start  [' + St + ']');
        if copy(St, 1, 4) = '### ' then begin
            delete(St, 1, 4);
            St := '<size:large><bold>' + St + '</bold></size:large>'
        end else
            if copy(St, 1, 3) = '## ' then begin
                delete(St, 1, 3);
                St := '<size:huge><bold>' + St + '</bold></size:huge>'
            end else if copy(St, 1, 2) = '* ' then begin
                delete(St, 1, 2);
                //<list><list-item dir="ltr">Line one</list-item></list>
                St := '<list><list-item dir="ltr">' + St + '</list-item></list>';
            end;
        St := St.Replace('&lt;sub&gt;', '<size:small>', [rfReplaceAll]);
        St := St.Replace('&lt;/sub&gt;', '</size:small>', [rfReplaceAll]);
        St := St.Replace('&lt;underline&gt;', '', [rfReplaceAll]);
        St := St.Replace('&lt;/underline&gt;', '', [rfReplaceAll]);
        St := St.Replace('&lt;highlight&gt;', '<highlight>', [rfReplaceAll]);
        St := St.Replace('&lt;/highlight&gt;', '</highlight>', [rfReplaceAll]);
//        DebugLn('Middle [' + St + ']');
        while ChangeTag(St, '***', '<italic><bold>', '</bold></italic>') do;
        while ChangeTag(St, '**', '<bold>', '</bold>') do;
        while ChangeTag(St, '__', '<bold>', '</bold>') do;
        while ChangeTag(St, '*', '<italic>', '</italic>') do;
        while ChangeTag(St, '_', '<italic>', '</italic>') do;
        while ChangeTag(St, '`', '<monospace>', '</monospace>') do;
        while ChangeTag(St, '~~', '<strikeout>', '</strikeout>') do;
        if copy(St, 1, 2) = '  ' then begin                          // Ah, thats leading space mono
            while length(St) > 0 do
                if St[1] = '' then delete(St, 1, 1)                  // remove those leading spaces
                else break;
            if length(St) > 0 then
                St := '<monospace>' + St + '</monospace>';
        end;
//        DebugLn('Finish [' + St + ']');
        Cont.Strings[Index] := St;
        inc(Index);
    end;
    DoLineHeadings(Cont);
end;


// ToDo : Monospaced becomes code, wrap with backtick. MD:  `Mono or code`  Tomboy:  <monospace>Mono or code</monospace>
//      : Monospace may also be a line that starts with a space. So renders require several spaces
//      : Small font, gets converted to sub script in version in tomboy-ng. So, mark down is
//        wrap text in <sub>little text</sub>     and tomboy version is <size:small>little text</size:small>
//      : Headings, a line, perhaps two or more of just == or ------ says previous line was heading.
//        https://spec.commonmark.org/

function TImportNotes.ImportFile(FullFileName: string): boolean;
var
  Content : TStringList;
  GUID : TGUID;
  NewFileName : string;
  Title : string;
  Index : integer = 0;
begin
    Result := True;
    if FileExists(FullFileName) then begin
        try
            Content := TStringList.Create;
            Content.LoadFromFile(FullFileName);
            while Index < Content.Count do begin
                Content.Strings[Index] := RemoveBadXMLCharacters(Content.Strings[Index]);
                inc(Index);
            end;
            if Mode = 'markdown' then
                MarkUpMarkDown(Content);
            if FirstLineIsTitle then begin
                Title := Content.Strings[0];
                Content.Delete(0);
            end else Title := ExtractFileNameOnly(FullFileName);
            if copy(Title, 1, 2) = '# ' then delete(Title, 1, 2);
            if copy(Title, 1, 2) = '## ' then delete(Title, 1, 3);
            if copy(Title, 1, 2) = '### ' then delete(Title, 1, 4);

            ProcessPlain(Content, Title);
            CreateGUID(GUID);
            if KeepFileName then
                NewFileName := ExtractFileNameOnly(FullFileName) + '.note'
            else NewFileName := copy(GUIDToString(GUID), 2, 36) + '.note';
            Content.SaveToFile(AppendPathDelim(DestinationDir) + NewFileName);
        finally
            freeandnil(Content);
        end;
    end else begin
        ErrorMsg := 'Failed to open import file' + #10  + FullFileName;
        Result := False;
    end;
end;

function TImportNotes.Execute(): integer;
var
  St : string;
begin
    Result := 0;
    if ImportNames = nil then
        exit;
    for St in ImportNames do begin
        if not ImportFile(St) then
            exit;
        inc(Result);
    end;
end;

function TImportNotes.MDtoNote(Content: TStringList; const LCD, CDate: string): boolean;
var
    Title : string;
    Index : integer = 0;
begin
    Result := True;
    while Index < Content.Count do begin
        Content.Strings[Index] := RemoveBadXMLCharacters(Content.Strings[Index]);
        inc(Index);
    end;
    MarkUpMarkDown(Content);
    Title := Content.Strings[0];
    Content.Delete(0);
    Title.Replace('#', '', [rfReplaceAll]);
    ProcessPlain(Content, Title, LCD, CDate);
end;

constructor TImportNotes.Create;
begin

end;

destructor TImportNotes.Destroy;
begin
    inherited Destroy;
end;

end.

