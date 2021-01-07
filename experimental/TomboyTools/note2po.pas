unit note2po;


{ A unit that can export a tomboy note in a kind of .pot format.  xmp retained
  (except for header and footer).  Sentence by sentance with para breaks marked
  with < ps/ > on Roy's recommendation. It can be loaded into POEdit but I would
  not like to work with something that hard to read.

  Jan 6, 2021

  Only available via the command line at present,  use     -a note2pot, hard wired
  to pickup a note called tomboy-ng.note and export something called
  tomboy-ng-help.note
}

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
		function EndOfSentance(const St: string; var Index: integer;
				var foundOne: boolean): boolean;
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
    Index := 0;
    while Index <  StrLst.Count do begin
        StrLst.Strings[Index] := StrLst.Strings[Index] + '< ps/ >';
        if ((Index + 1) < StrLst.Count) and (StrLst.Strings[Index+1] = '') then begin
            while ((Index + 1) < StrLst.Count) and (StrLst.Strings[Index+1] = '') do begin
                StrLst.Strings[Index] := StrLst.Strings[Index] + '< ps/ >';
                StrLst.Delete(Index+1);
            end;
        end else
            inc(Index);
	end;

	MakeSentances(StrLst);
                                    strlst.savetofile('TEMP.TXT');
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
    // There should be no blank lines present now,
    while I < Stl.Count do begin
        St := 'msgid "' + STL.Strings[i] + '"';
        Stl.Strings[i] := St;
        inc(i);
        if (i) < STL.Count then begin
            STL.Insert(i, '');
            STL.Insert(i, 'msgstr ""');
            inc(i, 2);
		end else begin
            STL.Append('msgstr ""');
            STL.Append('');
		    inc(i, 2);              // or we could break ....
		end;
	end;
end;

// Gets called when a period is found, Index points to char after the .
// We have EOS if the . if followed by a space or a trailing tag, we grab all trailing tags.
// ret True and sets Index to char after the > marking end of the tag. So, make sure its safe.
// Wen called, if Index does not point to < then, false. Else it tries to find the end
// of the tag, if it does, it returns true and sets Index to the next (possibly non existing)
// char.  FoundOne indicates we did find one tag, failing to find subsquent ones does NOT reset it.
function TExportPOT.EndOfSentance(const St : string; var Index : integer; var foundOne : boolean) : boolean;
var
    Offset : integer;
    Ch : char;
    Snipit : string = '';
    Len : integer;
begin
    Snipit := copy(St, Index, 1);
    if not ( (copy(St, Index, 1) = ' ')
        or (copy(St, Index, 2) = '</')
        or (copy(St, Index, 7) = '< ps/ >') ) then
            exit(false);
    // Any one of the above makes it an EOS and guarentees that we have that much room at end
    // So, we will set Index to point to the space or the '>' at end of the Tag.

    Offset := Index;
    if St[Index] = ' ' then exit(True);
    while Offset <= St.Length do begin
        Snipit := copy(St, Offset, 1000);
        ch := St[Offset];
        if St[Offset] = '>' then begin
            Index := Offset;
            FoundOne := True;
            exit(True);
        end;
        inc(Offset);
        Len := st.length;
    end;
    // if we got the here, its an error, an unfinished tag perhaps ?
    debugln('ERROR, unfinished tag ' + St);
    result := false;
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
    FoundOne : Boolean = False;
begin
    //We have to find a period followed by either a space or a trailing tag. First one we find, we act on.
    if St.length < 2 then exit(false);
    while true do begin
        if St[Index] = '.' then begin
            FoundOne := False;
            inc(Index);
            if EndOfSentance(St, Index, FoundOne) then begin
                inc(Index);
                while EndOfSentance(St, Index, FoundOne) do inc(Index);        // multiple trailing tags ?
                if Index > St.Length then
                    exit(False)
                else break;                                              // even one is enough !
            end;
            if St.length < (Index +1) then exit(False);         // no room for another sentence
        end;
        inc(Index);
        if Index > St.Length then exit(False);
	end;
    // if to here, we have either a '. ', a '.</' or a '.< ps/ >' and Index points to either . or >
    dec(Index);
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

