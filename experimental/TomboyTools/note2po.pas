unit note2po;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils;

type

{ TNoteNormaliser }

// Will tidy up the location of tags to keep para or sentences together
// As well as making for pretty to look xml, its far easier to parse, when converting to, eg markdown
// Open a note into a string list, create NoteNormaliser and pass the string list to NormaliseList.
TNoteNormaliser = class
    private
	    procedure MoveTagDown(const StL: TStringList; const StIndex, TagSize: integer);
		function MoveTagLeft(var St: string): boolean;
		function MoveTagRight(var St: string): boolean;
        procedure MoveTagUp(const StL: TStringList; const StIndex: integer; var TagSize: integer);
		function OffTagAtStart(St: string): integer;
		function OnTagAtEnd(St: string): integer;
    public
        procedure NormaliseList(STL: TStringList);
end;

{ TExportPOT }

// Export a note as a POT file.
// Normalise, remove header and footer, break into sentences. msgid should be sentance
// including any markup.  msgstr "", save with note title as filename, pot as extension.

TExportPOT = class
    private
        NoteTitle : string;
        // Not sure this belongs here, dir copy of same from commonmark .....
		function BreakThis(const St: string; out Line1, Line2: string): boolean;
  function FindInStringList(const StL: TStringList; const FindMe: string
				): integer;
  procedure InsertPOCodes(StL: TStringList);
        procedure MakePOT(FullFileName : string);
        // converts the list of paragraps to a list of sentences.
        procedure MakeSentances(const StL: TStringList);

		function TitleFromID(FullFileName: string; Munge: boolean): boolean;
		function TrailTagLength(const St: string; const I: integer; out
				TLength: integer): boolean;
    public
        ErrorString : string;
        constructor create(FileName : string);    // looks for note in current dir, save pot file there too.
        constructor create(NoteDir, FileName : string);   // assumes '.note' has been appended.

end;



implementation

uses lazlogger, LazFileUtils, laz2_DOM, laz2_XMLRead;

{ TExportPOT }

procedure TExportPOT.MakePOT(FullFileName: string);
var
    Normaliser : TNoteNormaliser;
    StrLst : TStringList;
    Index : Integer;
begin
    StrLst := TStringList.Create;
    StrLst.LoadFromFile(FullFileName);
    Index := FindInStringList(StrLst, '<title>');       // include < and > in search term so sure its metadate
    if Index > -1 then
        while Index >= -1 do begin
            StrLst.Delete(0);
            dec(Index);
		end;
    Normaliser := TNoteNormaliser.Create;
    Normaliser.NormaliseList(StrLst);
    Index := FindInStringList(StrLst, '</note-content>');       // but G-Note does not bother with nice newlines ????
    while Index < StrLst.Count do StrLst.Delete(Index);
    MakeSentances(StrLst);
    InsertPOCodes(StrLst);
    strLst.SaveToFile(NoteTitle + '.pot');
    Normaliser.Free;
    StrLst.Free;
end;

procedure TExportPOT.InsertPOCodes(StL : TStringList);
var
    St : string = '';
    I : integer = 3;
begin
    STL.Insert(0, '');
    STl.Insert(0, 'msgstr "Content-Type: text/plain; charset=UTF-8"');
    STL.Insert(0, 'msgid ""');

    while I < Stl.Count do begin
        if length(STL.Strings[i]) > 0 then begin        // We have a block !
            St := 'msgid "' + STL.Strings[i] + '"';
            Stl.Strings[i] := St;
		end else begin
            Stl.Strings[i] := 'msgid "<break/>"';
		end;
        inc(i);
        if (i) < STL.Count then
            STL.Insert(i, 'msgstr ""')
        else STL.Append('msgstr ""');
        inc(i);
        if (i) < STL.Count then             // note we always add a blank line and always remove one when importing.
            STL.Insert(i, '')
        else STL.Append('');
        inc(I);
	end;
end;


// ret True and sets TLength if I points to a trailing tag and we found end of tag.
function TExportPOT.TrailTagLength(const St : string; const I : integer; out TLength : integer) : boolean;
var
    Offset : integer = 2;
begin
    if length(St) > (I + 2) then         // there is room for a tag ...
        if (St[I+1] = '<') and (St[I+2] = '/') then         // OK, its a trailing tag.
            while I + Offset < length(St) do begin
                inc(Offset);
                if St[I + Offset] = '>' then begin
                    Tlength := I + Offset;
                    exit(True);
                end;
            end;
    // if to here, we failed to find the end of a trailing tag
    Result := False;
end;

// Look to see if passed line needs to be broken up, if so, put bits into Line1 and Line2
function TExportPOT.BreakThis(const St : string; out Line1, Line2 : string) : boolean;
// We assume that xml does not contain a . is that fair ?
// We define an end of sentence as being a period and a space, '. ' OR a period and start of an xml trailing tag.
// some text.</tag1></tag2>
//          ^      ^      ^
var
    Index : integer = 1;
    Offset : integer = 0;
begin
    //We have to find a period followed by either a space or a trailing tag. First one we find, we act on.
    if St.length < 2 then exit(false);
    while true do begin
        if St[Index] = '.' then begin
            if St.length < (Index +1) then exit(False);         // no room for another sentence
            if St[Index+1] = ' ' then break;                    // that will do nicely thanks
            if TrailTagLength(St, Index, Offset) then break;         // OK, a trailing tag will do
		end;
        inc(Index);
        if Index > St.Length then exit(False);
	end;
    Index := Index + Offset;                                    // its non zero if on a tag
    // if to here, we have either a '. ' or a '.</' and Index points to either . or >

    result := True;
    Line1 := copy(St, 1, Index);
    inc(Index);
    if (length(St) > Index) and (St[Index] = ' ') then inc(Index);        // to cover space in 'something. Another'
    Line2 := copy(St, Index, 1000);
end;


procedure TExportPOT.MakeSentances(const StL: TStringList);
var
    Index : integer = 0;
    Line1, Line2 : string;
begin
    // We look for and end of sentance in the paragraph that is followed by content.  Trailing tags
    // do not count as content, opening tags or text does.  Delete the line and add in two lines if
    // one is found. Take trailing tags in first line.
    while Index < Stl.Count do begin
	    if BreakThis(StL.Strings[Index], Line1, Line2) then begin
	            Stl.Delete(Index);
	            StL.Insert(Index, Line2);
	            StL.Insert(Index, Line1);
		end;
	    inc(Index);     // OK, we now point to next line or Line2 from above.
	end;
end;

constructor TExportPOT.create(FileName: string);
begin
    ErrorString := '';
    if not TitleFromID(FileName, True) then
        exit;
    MakePOT(FileName);
end;

constructor TExportPOT.create(NoteDir, FileName: string);
begin
    ErrorString := '';
    if not TitleFromID(appendPathDelim(NoteDir) + FileName, True) then
        exit;
    MakePOT(appendPathDelim(NoteDir) + FileName);
end;


function TExportPOT.TitleFromID(FullFileName: string; Munge : boolean): boolean;
var
    Doc : TXMLDocument;
    Node : TDOMNode;
    Index : integer = 1;
begin
    if not FileExists(FullFileName) then begin
        ErrorString := 'ERROR : File does not exist = '  + FullFileName;
        exit(False);
	end;
	ReadXMLFile(Doc, FullFileName);
    try
        Node := Doc.DocumentElement.FindNode('title');
        NoteTitle := Node.FirstChild.NodeValue;
    finally
        Doc.free;
    end;
    if Munge then begin
        // remove char that don't belong in a file name
        while Index <= length(NoteTitle) do begin
                if NoteTitle[Index] in [ ' ', ':', '.', '/', '\', '|', '"', '''' ] then begin
                    NoteTitle[Index] := '_';
                end;

                inc(Index);
        end;
        NoteTitle := copy(NoteTitle, 1, 32);
	end;
    result := length(NoteTitle) > 0;
end;

function TExportPOT.FindInStringList(const StL : TStringList; const FindMe : string) : integer;
var
    I : integer = 0;
begin
    while i < StL.Count -1 do begin
        if pos(FindMe, StL.strings[i]) > 0 then
            exit(i);
        inc(i);
	end;
	result := -1;
end;


// ----------------------  N O R M A L I S I N G ------------------------------------

// Deals with 'off' tags that need to be moved up to the para they apply to.
procedure TNoteNormaliser.MoveTagUp(const StL : TStringList; const StIndex : integer; var TagSize : integer);
var
    Tag : string;
begin
    // we have to detect when our line starts with  </note-content> or </text> and
    // terminate processing of this string, they are not text markup.
    Tag := copy(StL.strings[StIndex], 1, TagSize);
    if (Tag = '</note-content>') or (Tag = '</text>') then begin
        TagSize := 0;
        exit;
    end;
    StL.Insert(StIndex, copy(StL.Strings[StIndex], TagSize+1, length(StL.Strings[StIndex])));
    StL.Delete(StIndex+1);
    StL.Insert(StIndex-1, StL.strings[StIndex-1]+Tag);
    StL.Delete(StIndex);
end;

function TNoteNormaliser.MoveTagRight(var St: string): boolean;
var
    Index, TagStart, StartAt : integer;
begin
    Index := Pos('> ', St);
    if Index = 0 then exit(False);
    StartAt := 1;
    repeat
        Index := St.IndexOf('> ', StartAt);
        if Index < 0 then exit(False);
        TagStart := Index;
        while St[TagStart] <> '<' do dec(TagStart);
        if St[TagStart+1] = '/' then begin          // Not interested, an 'off' tag
            StartAt := Index+1;
            continue;
        end else break;
    until false;
    delete(St, Index+2, 1);
    insert(' ', St, TagStart);
    result := True;
end;

{ Will move a tag to the left if it has a space there, ret T if it moved one.}
function TNoteNormaliser.MoveTagLeft(var St: string): boolean;
var
    Index : integer;
begin
    Index := Pos(' </', St);
    if Index = 0 then exit(False);
    delete(St, Index, 1);
    insert(' ', St, St.IndexOf('>', Index)+2);          // 2 ? IndexOf rets a zero based and we want to go one past
    Result := true;
end;

function TNoteNormaliser.OnTagAtEnd(St : string) : integer;
var
    I, L : integer;
begin
    if St = '' then exit(0);
    L := length(st);
    if St[L] <> '>' then exit(0);
    i := 1;
    while St[L-i] <> '<' do begin       // march backwards until we find start of tag
        inc(i);
        if i > L then  begin
            debugln('ERROR : Overrun looking for tag start');
            exit(-1);
		end;
	end;
    if  St[L-i+1] = '/' then exit(0);   // not our problems, tags at the end should be 'off' tags.
    result := i+1;
end;

// Looks for an 'off' tag at the start of a line, they belong further up the list, 0 says none found
function TNoteNormaliser.OffTagAtStart(St : string) : integer;
var
    I : integer = 2;
    L : integer;
begin
    if (St = '') or (St[1] <> '<') or (St[2] <> '/') then   // Hmm, a single unescaed < on a line will crash
        exit(0);
    L := length(St);
    while St[i] <> '>' do begin
        inc(i);
        if i > L then begin
            debugln('ERROR : overrun looking for tag end, line =[' + st + ']');
            exit(-1);
		end;
	end;
    result := i;
end;

// Deals with 'on' tags that need to be moved down to the paras that they apply to
procedure TNoteNormaliser.MoveTagDown(const StL : TStringList; const StIndex, TagSize : integer);
var
    Tag : string;
begin
    Tag := copy(StL.strings[StIndex], length(StL.strings[StIndex])-TagSize+1, TagSize);
    StL.Insert(StIndex, copy(StL.strings[StIndex], 1, length(StL.strings[StIndex])-TagSize));
    StL.Delete(StIndex+1);
    StL.Insert(StIndex+1, Tag+StL.Strings[StIndex+1]);
    StL.Delete(StIndex+2);
end;

procedure TNoteNormaliser.NormaliseList(STL : TStringList);
var
    TagSize, StIndex : integer;
    TempSt : string;
begin
    StIndex := 0;
    while StIndex < StL.Count do begin
        repeat
            TagSize := OnTagAtEnd(StL.Strings[StIndex]);
            if TagSize > 0 then MoveTagDown(StL, StIndex, TagSize);
		until TagSize < 1;          // WARNING, that includes error code, -1
        TempSt := StL.Strings[StIndex];
        while MoveTagLeft(TempSt) do;
        while MoveTagRight(TempSt) do;
        if TempSt <> StL.Strings[StIndex] then begin
            StL.Insert(StIndex, TempSt);
            StL.Delete(StIndex + 1);
        end;
        inc(StIndex);
	end;
    StIndex := StL.Count -1;           // start at bottom and work up
    while StIndex > 0 do begin      // we don't care about the first line.
        repeat
            TagSize := OffTagAtStart(StL.strings[StIndex]);
            if TagSize > 0 then MoveTagUp(StL, StIndex, TagSize);
		until TagSize < 1;
        dec(StIndex);
	end;
    StIndex := StL.Count -1;           // remove any trailing spaces.
    while StIndex > 0 do begin
        TempSt := Stl[StIndex];
        if TempSt.endswith(' ')  then
            Stl[StIndex] := TempSt.TrimRight;
        dec(StIndex);
	end;
end;

end.

