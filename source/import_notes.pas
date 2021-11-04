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
    2021/09/06   Support notebook lists
    2021/09/25   Allow [] meaning an empty list of notebooks.
    2021/09/25   Fixed an "beyond the end of a line" issue in PosMDTag(), only show up on build machine ???
    2021/09/28   Enable multilevel bullets
    2021/10/01   Allow for the fact that a JSON Notebook string may have " or \ escaped.
    2021/10/17   Remove four spaces from left of mono line.
    2021/10/18   Inline MD Tags now support Flanking rules, https://spec.commonmark.org/0.30/#left-flanking-delimiter-run
    2021/10/18   For some reason I was removing embedded underline xml, now I restore it ??
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    { TImportNotes }

    TImportNotes = class
    private
        function ChangeTag(var St: string; const ChangeFrom, ChangeToLead, ChangeToTrail: string): boolean;
        procedure ConvertList(var St: string);
        procedure DoLineHeadings(const STL: TStringList);
                            { Ret True if it finds a matching pair of tags that obay the Flanking rules. If
                            so, sets the out vars to start of each tag. Else rets false. Call until it does
                            ret false. https://spec.commonmark.org/0.30/#left-flanking-delimiter-run }
        function FindMDTags(const St, Tag: string; out LeftFlank, RightFlank: integer
            ): boolean;
        function ImportFile(FullFileName: string) : boolean;
        function MarkUpMarkDown(Cont: TStringList) : boolean;
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
        NoteBook : string;              // Empty is OK, plain text notebook name or JSON array (including [])
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
var
    Start : integer = 1;
    NBName : string = '';
    //DateSt : string;        // eg '2020-05-19T18:58:37.9513193+10:00';

    // Finds Notebook names on the JSON Notebook string. Even allows for Escaped " and \
    // returns string value or empty string if no more available
    function NextNBName() : string;
    var
        InValue : boolean = False;
        InEsc   : boolean = False;
        //i, Index : integer;
    begin
        Result := '';
        inc(Start);
        while Start < length(Notebook)-1 do begin
            //for i := Start+1 to length(Notebook)-1 do begin
            case Notebook[Start] of
            '"' :   if InValue and not InEsc then               // Ah, thats the end of a value.
                        exit
                    else if not InEsc then begin
                        InValue := True;
                        inc(Start);
                        continue;
                    end;                                        // if we are inEsc, let it go through to keeper.
            '\' :   if not InEsc then begin
                        InEsc := True;                          // Must be first.
                        inc(Start);
                        continue;
                    end;
            end;                                                // In every case, if we are InEsc, we allow the use of the char
            InEsc := False;
            if InValue then
                Result := Result + NoteBook[Start];
            inc(Start);
        end;
    end;

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

    // notebook may contain just the name of a notebook, My NoteBook  or  a json array, eg
    // [] or ["template","Man Pages"] or ["Man Pages"]  or ["Man Pages", "Other Notebook"]
    // But Notebook names may have backslash or double inverted commas, escaped with backslash
    if (Notebook <> '') and (Notebook <> '[]') then begin
        if (NoteBook[1] = '[') then begin                       // its a JSON array
            NBName := NextNBName;                    // Uses the regional, 'Start' to keep track
            while NBName <> '' do begin
                if NBName = 'template' then
                    Cont.Add('        <tag>system:template:' + '</tag>')
                else
                    Cont.Add('        <tag>system:notebook:' + RemoveBadXMLCharacters(NBName) + '</tag>');
                NBName := NextNBName;
            end;
        end else                                              // if not an array, just use it as it is, one notebook
            if NoteBook <> '' then
                Cont.Add('        <tag>system:notebook:' + RemoveBadXMLCharacters(NoteBook) + '</tag>');
    end;
    Cont.Add('      </tags>');
    Cont.Add('    	<open-on-startup>False</open-on-startup>');
    Cont.Add('</note>');
    result := True;
end;

{     Markdown Rules

A line starting with a asterik and a space is a bullet. Or some whitespace first
might be a multilevel bullet.
Bold text is wrapped in ** at either end. Or __ (that is two underscores) at either end.
Italics is wrapped in * at either end. Again, the underscore at either end will work as well.
Highlight is wrapped in ~~ at either end,
Stikeout is supported with ~~ at either end.
a line starting with ###space  is a  bold, large line
a line starting with ##space is a bold, huge line
A line that is followed by some ===== or ------ are headings, huge and Large
we ignore #space, have other ways of finding title.
A line starting with four or more spaces is all monospace, remove exactly four spaces
and add tags. In line text that is wrapped in backticks is in line mono, remove
ticks and add tags.
All the inline tags follow Flanking rules, eg LeftFlank must have something immediatly to the right.

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


// ToDo : the flanking rules are grossly incomplete. We must ignore intermediate tags and test for real content.
// ToDo : when we decide its a pair of MD Tags, we should ensure there are no unmatched xml tags between.

function TImportNotes.FindMDTags(const St, Tag : string; out LeftFlank, RightFlank : integer) : boolean;
begin
    LeftFlank := pos(Tag, St, 1);
    if LeftFlank = 0 then exit(False);
    while true do begin
        // OK, is it really a LeftFlank ?  These tests must be improved significently !
        if (St.Length < (LeftFlank + 1 + (2*Tag.Length)))             // no room for content and trailing tag
        or (St[LeftFlank+Tag.Length] = ' ') then begin                // not left flanking, need check better than this !!
            LeftFlank := pos(Tag, St, LeftFlank+1);
            if LeftFlank = 0 then exit(False);
            continue;
        end;
        break;
    end;
    // OK, we do have a LeftFlank, should look at any xml tags we step over here but for now, lets find a RightFlank
    RightFlank := pos(Tag, St, LeftFlank + Tag.Length);
    if (RightFlank = 0) then exit(False);
    while true do begin
        if (St[RightFlank -1] = ' ') then begin
            RightFlank := pos(Tag, St, RightFlank + Tag.Length);
            if (RightFlank = 0) then exit(False);
            continue;
        end;
        exit(True);
    end;
    result := False;
end;

(*
function TImportNotes.FindMDTags(const St, Tag : string; out LeftFlank, RightFlank : integer) : boolean;
begin
    LeftFlank := pos(Tag, St, 1);
    if LeftFlank = 0 then exit(False);
    while LeftFlank > 0 do begin
        // OK, is it really a LeftFlank ?  These tests must be improved significently !
        if (St.Length < (LeftFlank + 1 + (2*Tag.Length)))             // no room for content and trailing tag
        or (St[LeftFlank+Tag.Length] = ' ') then begin                // not left flanking, need check better than this !!
            LeftFlank := pos(Tag, St, LeftFlank+1);
            continue;
        end;
        // OK, we do have a LeftFlank, should test for matched tags here but for now, lets find a RightFlank
        RightFlank := pos(Tag, St, LeftFlank + Tag.Length);
        if (RightFlank = 0) then exit(False);                         // no point in looking further.
        if (St[RightFlank -1] = ' ') then begin
            RightFlank := pos(Tag, St, RightFlank + Tag.Length);
            continue;
        end;
        exit(True);
    end;
    result := False;
end;
*)




function TImportNotes.ChangeTag(var St : string; const ChangeFrom, ChangeToLead, ChangeToTrail :  string)  : boolean;
var
    LFlank, RFlank : integer;
begin
    Result := False;
    if FindMDTags(St, ChangeFrom, LFlank, RFlank) then begin
        delete(St, RFlank, ChangeFrom.Length);
        insert(ChangeToTrail, St, RFlank);
        delete(St, LFlank, ChangeFrom.Length);
        insert(ChangeToLead, St, LFlank);
        result := True;
    end;
end;

{ For our purpose, here, a level one list line starts with an * followed by a
space. For every three spaces before the * its one level deeper. While MD lets
you use other characters, its only a * here folks.  }
procedure TImportNotes.ConvertList(var St : string);
var
    Spaces : integer = 1;       // How many leading spaces.
    I : integer;
    xmltags : string = '';
begin
    while Spaces <= st.length do begin
        if St[Spaces] <> ' ' then break;
        inc(Spaces);
    end;
    // here, number of spaces is Spaces-1;  We are here because either not (Spaces <= St.Length)  or  St[Spaces] <> ' '.
    if (Spaces > st.length) or (St[Spaces] <> '*') then exit;     // either not a * or no room for one.
    // If to here, we know its a (0-n spaces)*, if its a space next, definitly list item.
    inc(Spaces);
    if (Spaces > st.length) or (St[Spaces] <> ' ') then exit;
    dec(Spaces, 2);
    //debugln('TImportNotes.ConvertList Spaces=' + inttostr(Spaces) + ' and St=' + St);
    // So, remove Spaces spaces, the * and one more space.
    delete(St, 1, Spaces+2);
    Spaces := Spaces div 3;      // 0 div 3 = 0
    for i := 0 to Spaces do
        xmltags := xmltags + '<list><list-item dir="ltr">';
    St := xmltags + St;
    xmltags := '';
    for i := 0 to Spaces do                  // When Spaces = 0, we must add one set of tags.
        xmltags := xmltags + '</list-item></list>';
    St := St + xmltags;
    //debugln('TImportNotes.ConvertList St=' + St);
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
            end else ConvertList(St);
(*            end else if copy(St, 1, 2) = '* ' then begin
                delete(St, 1, 2);
                //<list><list-item dir="ltr">Line one</list-item></list>
                St := '<list><list-item dir="ltr">' + St + '</list-item></list>';
            end;   *)
        St := St.Replace('&lt;sub&gt;', '<size:small>', [rfReplaceAll]);
        St := St.Replace('&lt;/sub&gt;', '</size:small>', [rfReplaceAll]);
        St := St.Replace('&lt;underline&gt;', '<underline>', [rfReplaceAll]);
        St := St.Replace('&lt;/underline&gt;', '</underline>', [rfReplaceAll]);
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
        if copy(St, 1, 4) = '    ' then begin                          // Ah, thats leading space mono
            St := St.Remove(0, 4);
            if length(St) > 0 then
                St := '<monospace>' + St + '</monospace>';
        end;
//        DebugLn('Finish [' + St + ']');
        Cont.Strings[Index] := St;
        inc(Index);
    end;
    DoLineHeadings(Cont);
end;

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
    Title := Title.Replace('<underline>', '');
    Title := Title.Replace('</underline>', '');
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

