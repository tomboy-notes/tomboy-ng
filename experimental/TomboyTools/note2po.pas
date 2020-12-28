unit note2po;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils;

type



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

uses lazlogger, LazFileUtils, laz2_DOM, laz2_XMLRead, notenormal;

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




end.

