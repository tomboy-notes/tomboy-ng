unit export_notes;

{$mode objfpc}{$H+}    {$assertions on}

interface

uses
        Classes, SysUtils;

type

{ UTB2md }

 { TExportNote }

 TExportNote = class
  private

    function ExportAll()             : boolean;
    function ExportFile(ID: string): boolean;
    function ExportMD(ID: string): boolean;
    function ExportText(ID: string): boolean;
	function FindInStringList(const StL: TStringList; const FindMe: string
			): integer;
	function MoveTagRight(var St: string): boolean;
	function RemoveNextTag(var St: String; out Tag: string): integer;
	procedure MoveTagDown(const StL: TStringList; const StIndex, TagSize: integer);
	procedure MoveTagUp(const StL: TStringList; const StIndex: integer;
			var TagSize: integer);
	procedure NormaliseList(STL: TStringList);
	function OffTagAtStart(St: string): integer;
	function OnTagAtEnd(St: string): integer;
	procedure ProcessHeadings(StL: TStringList);
	procedure ProcessMarkUp(StL: TStringList);
	function RemoveTags(var St: string; out Tag: string): boolean;
	function RestoreBadXMLChar(const Str: AnsiString): AnsiString;
    function TitleFromID(ID: string; Munge: boolean; var LenTitle: integer): string;
    function IDfromTitle(Title : string) : string;
    function MoveTagLeft(var St: string): boolean;

  public
    NoteTitle : string;             // If not empty we are exporting just this note.
    AllNotes  : boolean;            // if true, we do all notes in indicated directory.
    NoteDir   : string;             // Dir containg the note or notes to export
    DestDir   : string;             // where to save notes to.
    NoteFileName : string;          // Note Filename without path
    Mode      : string;             // Either 'md' or 'text', maybe add more later.
    ErrorMessage : string;          // Empty unless something bad happened.
    function Execute() : boolean;   // you know all you need, go do it.
    constructor Create;
    destructor Destroy; override;
end;

implementation

{ UTB2md }

uses LCLProc, laz2_DOM, laz2_XMLRead;


function TExportNote.ExportAll(): boolean;
var
    Info : TSearchRec;
begin
    if FindFirst(NoteDir + '*.note', faAnyFile, Info) = 0 then
        try
            repeat
	            ExportFile(copy(Info.Name, 1, length(Info.name) - 5));
            until FindNext(Info) <> 0;
        finally
            FindClose(Info);
        end
    else
        Debugln('ERROR : No notes were found in ' + NoteDir);
    result := true;
end;

function TExportNote.IDfromTitle(Title: string): string;
var
    Info : TSearchRec;
    LTitle : integer;
begin
    if FindFirst(NoteDir + '*.note', faAnyFile, Info) = 0 then
        try
            repeat
                if Title = TitleFromID(copy(Info.Name, 1, length(Info.name) - 5), False, LTitle) then
                    exit(copy(Info.Name, 1, length(Info.name) - 5));
            until FindNext(Info) <> 0;
        finally
            FindClose(Info);
        end
    else
    Debugln('ERROR : No notes was found with Title ' + Title);
    result := '';
end;



// Takes an ID, reads that note converting title to a base filename
function TExportNote.TitleFromID(ID: string; Munge : boolean; var LenTitle : integer): string;
var
    Doc : TXMLDocument;
    Node : TDOMNode;
begin
    if not FileExists(NoteDir + ID + '.note') then begin
        debugln('ERROR : File does not exist = '  + NoteDir + ID + '.note');
        exit('');
	end;
	ReadXMLFile(Doc, NoteDir + ID + '.note');
    try
        Node := Doc.DocumentElement.FindNode('title');
        result := Node.FirstChild.NodeValue;
    finally
        Doc.free;
    end;
    LenTitle := length(Result);
    if Munge then begin
        while pos(' ', Result) > 0 do
            Result[pos(' ', Result)] := '_';
        Result := copy(Result, 1, 32);
	end;
end;

function TExportNote.ExportFile(ID: string): boolean;
begin
    if Mode = 'md' then
        result := ExportMd(ID)
    else result := ExportText(ID);
end;

// Note we restore only < > &,  Tomboy does not encode " or ' in Values (but must in attributes)
function TExportNote.RestoreBadXMLChar(const Str : AnsiString) : AnsiString;
var
    index : longint = 1;
    Start : longint = 1;
begin
  // Don't use UTF8 functions here, we are working with bytes !
  Result := '';
    while Index <= Length(Str) do begin
      if '&lt;' = Copy(Str, Index, 4) then begin
      		Result := Result + Copy(Str, Start, Index - Start) + '<';
            inc(Index);
            Start := Index + 3;
            Continue;
	  end;
      if '&gt;' = Copy(Str, Index, 4) then begin
      		Result := Result + Copy(Str, Start, Index - Start) + '>';
            inc(Index);
            Start := Index + 3;
            Continue;
	  end;
      if '&amp;' = Copy(Str, Index, 5) then begin
      		Result := Result + Copy(Str, Start, Index - Start) + '&';
            inc(Index);
            Start := Index + 4;
            Continue;
	  end;
      inc(Index);
	end;
    Result := Result + Copy(Str, Start, Index - Start);
end;

function TExportNote.FindInStringList(const StL : TStringList; const FindMe : string) : integer;
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

// Returns the length of any 'on' tags at end if string, they apply to next text. 0 indicates none found.
function TExportNote.OnTagAtEnd(St : string) : integer;
var
    I, L : integer;
begin
    if St = '' then exit(0);
    L := length(st);
    if St[L] <> '>' then exit(0);
    i := 1;
    while St[L-i] <> '<' do begin
        inc(i);
        if i > L then  begin
            debugln('ERROR : Overrun looking for tag start');
            exit(-1);
		end;
	end;
    if  St[L-i+1] = '/' then exit(0);
    result := i+1;
end;

// Looks for an 'off' tag at the start of a line, they belong further up the list, 0 says none found
function TExportNote.OffTagAtStart(St : string) : integer;
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
procedure TExportNote.MoveTagDown(const StL : TStringList; const StIndex, TagSize : integer);
var
    Tag : string;
begin
    Tag := copy(StL.strings[StIndex], length(StL.strings[StIndex])-TagSize+1, TagSize);
    StL.Insert(StIndex, copy(StL.strings[StIndex], 1, length(StL.strings[StIndex])-TagSize));
    StL.Delete(StIndex+1);
    StL.Insert(StIndex+1, Tag+StL.Strings[StIndex+1]);
    StL.Delete(StIndex+2);
end;

        // Deals with 'off' tags that need to be moved up to the para they apply to.
procedure TExportNote.MoveTagUp(const StL : TStringList; const StIndex : integer; var TagSize : integer);
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

function TExportNote.MoveTagRight(var St: string): boolean;
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
function TExportNote.MoveTagLeft(var St: string): boolean;
var
    Index : integer;
begin
    Index := Pos(' </', St);
    if Index = 0 then exit(False);
    delete(St, Index, 1);
    insert(' ', St, St.IndexOf('>', Index)+2);          // 2 ? IndexOf rets a zero based and we want to go one past
end;
        { We must ensure that all markup tags are not in the string (ie paragraph) that
          they apply to.  Its common to see the xml file with, eg, a </bold> at the
          start of a line, it actually closes a <bold> in previous line.  And a <bold>
          can be found at the end of a line and it applies to the following line, maybe
          even after several blank lines.  This will mess with md encoding.

          Also need to reposition any 'off' markup tags hard up against text to the left
          So, [texttext</bold> moretext] is OK, [texttext </bold>moretext] is not.
          }
procedure TExportNote.NormaliseList(STL : TStringList);
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

end;

            // Looks for lines that with <size:large><bold> or <size:huge><bold> and
            // end with </bold></size:huge> or </bold></size:large>, makes them headings
procedure TExportNote.ProcessHeadings(StL : TStringList);
var
    i : integer = -1;
    PosI, L : integer;
    Blar : string;
begin
    repeat
        inc(i);
        if (StL.Strings[i] = '') or (StL.strings[i][1] <> '<') then continue;
        if copy(Stl.Strings[i], 1, length('<size:large><bold>')) = '<size:large><bold>' then begin
            blar := Stl.Strings[i];
            PosI := pos('</bold></size:large>', Stl.Strings[i]);
            if PosI = 0 then continue;
            L := length(Stl.Strings[i]);
            if PosI -1 + length('</bold></size:large>') = L then begin
                StL.insert(i, '### ' + copy(Stl.Strings[i], length('<size:large><bold>')+1,
                        L - length('<size:large><bold></bold></size:large>')));
                StL.Delete(i+1);
			end;
		end;
        if copy(Stl.Strings[i], 1, length('<size:huge><bold>')) = '<size:huge><bold>' then begin
            blar := Stl.Strings[i];
            PosI := pos('</bold></size:huge>', Stl.Strings[i]);
            if PosI = 0 then continue;
            L := length(Stl.Strings[i]);
            if PosI -1 + length('</bold></size:huge>') = L then begin
                StL.insert(i, '## ' + copy(Stl.Strings[i], length('<size:huge><bold>')+1,
                        L - length('<size:huge><bold></bold></size:huge>')));
                StL.Delete(i+1);
			end;
		end;
	until I >= StL.Count-1;
end;

function TExportNote.RemoveNextTag(var St : String; out Tag : string) : integer;
var
    TStart, TEnd : integer;
begin
    Tag := '';
    TStart := pos('<', St);
    TEnd   := pos('>', St);
    if (TStart > 0) and (TEnd > 0) and (TEnd > TStart) then begin
        Tag := copy(St, TStart, TEnd - TStart +1);
        delete(St, TStart, TEnd - TStart +1);
        Result := TStart;
	end
	else Result := 0;
end;

        { Here we add general markup tags (except, so far, bullets) excluding headers.
          tomboy-ng will always present 'on' tags in this order -
          size:small ~, highlight, ==, italics, * bold, **.  Tags cannot cross so 'off' is reverse
          Seems Git md works with strikeout = '~~' or '~', other need '~~'
          Git does not do highlight
          }

procedure TExportNote.ProcessMarkUp(StL : TStringList);
var
    BoldOn : boolean = false;
    ItalicOn : boolean = false;
    //highlightOn : boolean = false;
    //SmallOn : boolean = false;
    StrikeOutOn : boolean = false;
    StIndex, ChIndex : integer;
    Tag, TempSt, NewTag : string;
begin
    StIndex := -1;
    ChIndex := 1;
    while StIndex < StL.Count -1 do begin
        inc(StIndex);
        if (length(StL.Strings[StIndex]) < 2) then continue;     // no room for a tag in there.
        TempSt := '';
        if ItalicOn then TempSt := TempSt + '*';
        if BoldOn then TempSt := TempSt + '**';
        if StrikeoutOn then TempSt := TempSt + '~~';

        TempSt := TempSt + StL.Strings[StIndex];
        TempSt := TempSt.Replace('<bold></bold>', '', [rfReplaceAll]);
        TempSt := TempSt.Replace('</bold></italic><bold>', '</bold></italic> <bold>', [rfReplaceAll]);
        // ToDo : replace above line with something generic that puts a space between a string of 'off's and a string of 'on's
        TempSt := TempSt.Replace('<list><list-item dir="ltr">', '* ');
        repeat
            ChIndex := RemoveNextTag(TempSt, Tag);
            case Tag of
                '<bold>' : begin NewTag := '**'; BoldOn := True; end;
                '</bold>' : begin NewTag := '**'; BoldOn := False; end;
                '<italic>' : begin NewTag := '*'; ItalicOn := True; end;
                '</italic>' : begin NewTag := '*'; ItalicOn := False; end;
                //'<highlight>' : begin NewTag := '=='; HighLightOn := True; end;
				//'</highlight>' : begin NewTag := '==';  HighLightOn := False; end;
                '<strikeout>'  : begin NewTag := '~~';  StrikeoutOn := True; end;
                '</strikeout>'  : begin NewTag := '~~';  StrikeoutOn := False; End;
			else
                NewTag := '';
            end;
            if not NewTag.IsEmpty then
                TempSt := TempSt.Insert(ChIndex-1, NewTag);
        until ChIndex < 1;
        if BoldOn then TempSt := TempSt + '**';
        if ItalicOn then TempSt := TempSt + '*';
        if StrikeoutOn then TempSt := TempSt + '~~';
        StL.Insert(StIndex, TempSt);
        StL.Delete(StIndex + 1);
	end;
end;


function TExportNote.ExportMD(ID : string): boolean;
var
    StList : TStringList;
    LTitle : integer;
    Index : integer;
begin
    if not FileExists(NoteDir + ID + '.note') then exit(False);
    //debugln('export ' + NoteDir + ID + '.note to ' + DestDir + TitleFromID(ID, True, LTitle) + '.md');
    result := true;
    StList := TStringList.Create;
    try
        StList.LoadFromFile(NoteDir + ID + '.note');
        Index := FindInStringList(StList, '<title>');       // include < and > in search term so sure its metadate
        if Index > -1 then
            while Index > -1 do begin
                StList.Delete(0);
                dec(Index);
			end;
        // OK, now first line contains the title but might have a style 'on' tag at the end. Normalise
        NormaliseList(StList);
        StList.Delete(0);
        StList.Insert(0, '# ' + TitleFromID(ID, False, LTitle));
        Index := FindInStringList(StList, '</note-content>');
        while Index < StList.Count do StList.Delete(Index);

        ProcessHeadings(StList);
        ProcessMarkUp(StList);
        StList.LineBreak := LineEnding + LineEnding;
        StList.SaveToFile(DestDir + TitleFromID(ID, True, LTitle) + '.md');
	finally

	end;
end;

function TExportNote.RemoveTags(var St : string; out Tag : string) : boolean;
var
    TStart, TEnd : integer;
begin
    Result := True;
    Tag := '';
    TStart := pos('<', St);
    TEnd   := pos('>', St);
    if (TStart > 0) and (TEnd > 0) and (TEnd > TStart) then begin
        Tag := copy(St, TStart, TEnd - TStart +1);
        delete(St, TStart, TEnd - TStart +1)
	end
	else Result := False;
end;

function TExportNote.ExportText(ID : string): boolean;
var
    Doc : TXMLDocument;
    Node : TDOMNode;
    Content : string;
    OutFile: TextFile;
    LTitle : integer;
begin
    if not FileExists(NoteDir + ID + '.note') then exit(False);
    ReadXMLFile(Doc, NoteDir + ID + '.note');
    try
        try
            Node := Doc.DocumentElement.FindNode('text');
            Content := Node.TextContent;
        except on E: Exception do debugln(ID + ' Bad things ' + E.Message);     // ToDo : yeah !
		end;
    finally
        Doc.free;
    end;
    AssignFile(OutFile, DestDir + TitleFromID(ID, True, LTitle) + '.txt');
    insert(LineEnding+LineEnding, Content, LTitle+1);
    try
        try
            Rewrite(OutFile);
            writeln(OutFile, RestoreBadXMLChar(Content));
		finally
            CloseFile(OutFile);
		end;
    except
        on E: EInOutError do begin
                debugln('File handling error occurred updating clean note location. Details: ' + E.Message);
                exit(False);
            end;
    end;
end;

function TExportNote.Execute(): boolean;
var
    ID : string;
begin
    Result := False;
    if AllNotes then ExportAll()
    else if NoteTitle <> '' then begin
        ID := IDfromTitle(NoteTitle);
        if ID = '' then
            debugln('ERROR : Unable to find note with Title = ' + NoteTitle)
        else
            Result := ExportFile(ID);
    end else begin
        if NoteFileName <> '' then
            result := ExportFile(copy(NoteFileName, 1, length(NoteFileName) - 5));
    end;
    // ToDo : add ability to export on basis of filename
end;


constructor TExportNote.Create;
begin

end;

destructor TExportNote.Destroy;
begin

		inherited Destroy;
end;

end.

