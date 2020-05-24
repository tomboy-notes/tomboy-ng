unit import_notes;

{ License - see tomboy-ng license information }

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    { TImportNotes }

    TImportNotes = class
    private
        function ChangeBold(var St: string): boolean;
        function ChangeItalic(var St: string): boolean;
        function ChangeSmallFont(var St: string): boolean;
        function ChangeTag(var St: string; const ChangeFrom, ChangeToLead,
            ChangeToTrail: string): boolean;
        function ImportFile(FullFileName: string): boolean;
        function MarkUpMarkDown(Cont: TStringList): boolean;
                            {  Returns the 1 based pos of the passed Tag, Leading says its a leading tag
                               must have whitespace of newline to left and an alpha mumeric to the right.
                               Reverse if Leading is false.  Ret 0 if a suitable tag is not found. }
        function PosMDTag(const St, Tag: string; const leading: boolean): integer;
        function ProcessPlain(Cont: TStringList; const Title: string): boolean;

    public
        ErrorMsg : string;              // '' if everything OK, content means something bad happened
        DestinationDir : string;        // Required, dir to save notes to
        Mode : string;                  // ie plaintext, markdown ....
        ImportNames : TStringList;      // A list of full file names to import, default filename will become title
        FirstLineTitle : boolean;       // if true, first line of note becomes title
        function Execute(): integer;    // you know all you need, go do it.
        constructor Create;
        destructor Destroy; override;
end;



implementation

{ TImportNotes }

uses ttutils, LazFileUtils, LazUTF8, LCLProc;

function TImportNotes.ProcessPlain(Cont : TStringList; const Title : string) : boolean;
var
    DateSt : string;        // eg '2020-05-19T18:58:37.9513193+10:00';
begin
    DateSt := GetTBLocalTime();
    Cont.Insert(0, '	<text xml:space="preserve"><note-content version="0.1">' + Title);
    Cont.Insert(0, '	<title>' + Title + '</title>');
    Cont.Insert(0, '<note version="0.3" xmlns:link="http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size" xmlns="http://beatniksoftware.com/tomboy">');
    Cont.Insert(0, '<?xml version="1.0" encoding="utf-8"?>');
    Cont.Add('    </note-content></text>');
    Cont.Add('      <last-change-date>' + DateSt + '</last-change-date>');
    Cont.Add('      <last-metadata-change-date>' + DateSt + '</last-metadata-change-date>');
    Cont.Add('      <create-date>' + DateSt + '</create-date>');
    Cont.Add('      <cursor-position>1</cursor-position>');
    Cont.Add('      <width>1000</width>');
    Cont.Add('      <height>626</height>');
    Cont.Add('    	<x>20</x>');
    Cont.Add('    	<y>30</y>');
    Cont.Add('      <tags>');
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
we ignore #space, have other ways of finding title.
}



function  TImportNotes.PosMDTag(const St, Tag : string; const leading : boolean) : integer;
var
    Stage : integer = 0;
begin
    {  Tag = '**'
       Leading ->  [sol]**Text   or .. **Text
       Trailing ->   ..Text** ...   or   ...Text**[eol]  }
    if Leading then begin
        while Stage < length(St) do begin
            Result :=  St.IndexOf(Tag, Stage);                      // zero based !
            if Result = -1 then exit;                            // no more candidates
            if (Result = 0) or (St[Result] in [' '..'/']) then              // start of line or has space before tag
                if St[Result+length(Tag)+1] in  ['A'..'z', '0'..'9'] then
                    exit(Result+1);
            Stage := Result+1;
        end;
    end else begin
        while Stage < length(St) do begin
            Result :=  St.IndexOf(Tag, Stage);  // zero based !
            if Result = -1 then exit;
            if ((Result+length(tag)) >= Length(St)) or (St[Result+length(Tag)+1] in [' '..'/']) then    // at end of line or a space after
                if St[Result] in  ['A'..'z', '0'..'9'] then                       // thats before tag
                    exit(Result+1);
            Stage := Result+1;
        end;
    end;
    Result := -1;
end;

function TImportNotes.ChangeBold(var St : string)  : boolean;
var
    PosStart, PosEnd : integer;
begin
    // like this **bold** or __bold__
    Result := False;
    PosStart := PosMDTag(St, '**', True);
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, '**', False);
        if (PosEnd > 0) then begin
            delete(St, PosEnd, 2);
            insert('</bold>', St, PosEnd);
            delete(St, PosStart, 2);
            insert('<bold>', St, PosStart);
            result := True;
        end;
    end;
    PosStart := PosMDTag(St, '__', True);
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, '__', False);
        if (PosEnd > 0) then begin
            delete(St, PosEnd, 2);
            insert('</bold>', St, PosEnd);
            delete(St, PosStart, 2);
            insert('<bold>', St, PosStart);
            result := True;
        end;
    end;
end;

function TImportNotes.ChangeItalic(var St : string)  : boolean;
var
    PosStart, PosEnd : integer;
begin
    // like this *italic* or, confusingly, might be like _this_
    Result := False;
    PosStart := PosMDTag(St, '*', True);
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, '*', False);
        if (PosEnd > 0) then begin
            delete(St, PosEnd, 1);
            insert('</italic>', St, PosEnd);
            delete(St, PosStart, 1);
            insert('<italic>', St, PosStart);
            result := True;
        end;
    end;
    PosStart := PosMDTag(St, '_', True);
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, '_', False);
        if (PosEnd > 0) then begin
            delete(St, PosEnd, 1);
            insert('</italic>', St, PosEnd);
            delete(St, PosStart, 1);
            insert('<italic>', St, PosStart);
            result := True;
        end;
    end;
end;

function TImportNotes.ChangeSmallFont(var St : string)  : boolean;
var
    PosStart, PosEnd : integer;
begin
    // MD looks like this <sub>small font</sub> but by time we get here, the angle brackets have been munged.
    Result := False;
    PosStart := PosMDTag(St, '&lt;sub&gt;', True);              // thats <sub>
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, '&lt;/sub&gt;', False);          // and thats </sub>
        if (PosEnd > 0) then begin
            delete(St, PosEnd, 12);
            insert('</size:small>', St, PosEnd);
            delete(St, PosStart, 11);
            insert('<size:small>', St, PosStart);
            result := True;
        end;
    end;
end;


    // Changes any symertical MD tah to corresponding leading and trailing Tomboy tags
function TImportNotes.ChangeTag(var St : string; const ChangeFrom, ChangeToLead, ChangeToTrail :  string)  : boolean;
var
    PosStart, PosEnd, FromLength : integer;
begin

    Result := False;
    FromLength := length(ChangeFrom);
    PosStart := PosMDTag(St, ChangeFrom, True);
    if (PosStart > 0) then begin
        PosEnd := PosMDTag(St, ChangeFrom, False);
        if (PosEnd > 0) then begin
            delete(St, PosEnd, FromLength);
            insert(ChangeToTrail, St, PosEnd);
            delete(St, PosStart, FromLength);
            insert(ChangeToLead, St, PosStart);
            result := True;
        end;
    end;
end;

{
 <size:huge><bold>huge heading</bold></size:huge><size:small>
}
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
{        while ChangeBold(St) do;
        while ChangeItalic(St) do;

        while ChangeMono(St) do;
        while ChangeSmallFont(St) do;     }

        while ChangeTag(St, '**', '<bold>', '</bold>') do;
        while ChangeTag(St, '__', '<bold>', '</bold>') do;
        while ChangeTag(St, '*', '<italic>', '</italic>') do;
        while ChangeTag(St, '_', '<italic>', '</italic>') do;
        while ChangeTag(St, '`', '<monospace>', '</monospace>') do;
        while ChangeTag(St, '~~', '<strikeout>', '</strikeout>') do;
        while ChangeSmallFont(St) do;
        DebugLn('[' + St + ']');
        Cont.Strings[Index] := St;
        inc(Index);
    end;
end;


// ToDo : Monospaced becomes code, wrap with backtick. MD:  `Mono or code`  Tomboy:  <monospace>Mono or code</monospace>
// ToDo : Small font, gets converted to sub script in version in tomboy-ng. So, mark down is
//        wrap text in <sub>little text</sub>     and tomboy version is <size:small>little text</size:small>

function TImportNotes.ImportFile(FullFileName: string): boolean;
var
  Content : TStringList;
  GUID : TGUID;
  Title : string;
  Index : integer = 0;
begin
    Result := True;
    if FileExists(FullFileName) then begin
        try
            Content := TStringList.Create;
            Content.LoadFromFile(FullFileName);
            while Index < Content.Count do begin
                {if Mode = 'markdown' then begin     // we need do this before angle brackets are munged.
                    St :=  Content.Strings[Index];
                    while ChangeSmallFont(St) do;
                    Content.Strings[Index] := St;
                end;}
                Content.Strings[Index] := RemoveBadXMLCharacters(Content.Strings[Index]);
                inc(Index);
            end;
            if Mode = 'markdown' then
                MarkUpMarkDown(Content);
            if FirstLineTitle then begin
                Title := Content.Strings[0];
                Content.Delete(0);
            end else Title := ExtractFileNameOnly(FullFileName);
            if copy(Title, 1, 2) = '# ' then delete(Title, 1, 2);
            if copy(Title, 1, 2) = '## ' then delete(Title, 1, 3);
            if copy(Title, 1, 2) = '### ' then delete(Title, 1, 4);
            ProcessPlain(Content, Title);
            CreateGUID(GUID);
            Content.SaveToFile(AppendPathDelim(DestinationDir) + copy(GUIDToString(GUID), 2, 36) + '.note');
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

constructor TImportNotes.Create;
begin

end;

destructor TImportNotes.Destroy;
begin
    inherited Destroy;
end;

end.

