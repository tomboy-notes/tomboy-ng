unit commonmark;

{$mode objfpc}{$H+}

{   *  Copyright (C) 2020 David Bannon
    *  See attached licence file.


    HISTORY
    2021/05/16  Taught TitleFromID() to deal with SingleNote mode when ID does not exist in Repo
                Note - this has not been synced with TT version.
}

// ToDo : Look for sequencial lines that contain all monospace text. Such lines needed
// to be merged into one block, instead of using ` or backtick, arround the block, we
// should indent the whole block by four extra spaces. If a line is already indented
// by spaces, add still add four more, that will retain the indent in Markdown
// So for example -
// `Some code`
//    `Indented code`
// Becomes
//     Some code
//         Indented code
{
    New function - IsAllMono(St : String) : boolean;
        returns true if line starts and ends with backtick, note we do not count
        leading whitespace, so "    `Indented code`" is true.
        Must ret false if there are more backticks than just the leading and trailing.
        This method needs some optimisation.

    New function - MakeMonoBlock(St) : string;
        Returns a string with leading and trailing backticks removed, four spaces
        added.

    New Procedure - ConvertMonoBlocks
        Looks at each line, one by one. If it finds a line where IsAllMone()
        returns true convert it. Move on.

This will result in (at least in remarkable) a single mono line appearing in a
block, more space above and below, but I guess that just how MD works.
I could convert only blocks containg two or more lines ......
}

interface

uses
        Classes, SysUtils;

type

{ TExportCommon }

TExportCommon = class        // based on TT export_notes, just takes a note ID and returns markdown

    private
			function FindInStringList(const StL: TStringList; const FindMe: string): integer;
			procedure MoveTagDown(const StL: TStringList; const StIndex, TagSize: integer);
			function MoveTagLeft(var St: string): boolean;
			function MoveTagRight(var St: string): boolean;
			procedure MoveTagUp(const StL: TStringList; const StIndex: integer; var TagSize: integer);
			procedure NormaliseList(STL: TStringList);
			function OffTagAtStart(St: string): integer;
			function OnTagAtEnd(St: string): integer;
			procedure ProcessHeadings(StL: TStringList);
			procedure ProcessMarkUp(StL: TStringList);
			function RemoveNextTag(var St: String; out Tag: string): integer;
			function RemoveRedundentTag(var St: string): boolean;
			function ReplaceAngles(const Str: AnsiString): AnsiString;
			procedure SayDebug(st: string; Always: boolean=false);
			function TitleFromID(ID: string; Munge: boolean; out LenTitle: integer): string;
            function IsAllMono(St : String) : boolean;
            procedure MakeMonoBlock(var St: string);
            procedure ConvertMonoBlocks(STL: TStringList);

    public
        DebugMode : boolean;
        NotesDir : string;       // dir were we expect to find our TB notes

                        { Takes a note ID (no extension) and fills out the passed StringList
                          that must have been created) with a commonmark version of the note.
                          returns an empty list on error. If ID is an ID only, assumes note is
                          on repo, else ID must contain a FFN inc path nad extension for single
                          note mode.}
        function GetMDcontent(ID : string; STL : TstringList) : boolean;


end;


implementation

uses LazFileUtils{$ifdef LCL}, lazlogger {$endif}, laz2_DOM, laz2_XMLRead, tb_utils ;




function TExportCommon.GetMDcontent(ID: string; STL: TstringList): boolean;
{ This is same as function in TT but I have removed parts that do file i/o
  I am thinking I would be better using some XML methods, might avoid g-Note issues too. }

var
    LTitle : integer;
    Index : integer;
    //Title : string = '';
begin

        if IDLooksOK(ID) then
            StL.LoadFromFile(NotesDir + ID + '.note')
        else StL.LoadFromFile(ID);
		Index := FindInStringList(StL, '<title>');       // include < and > in search term so sure its metadate
        if Index > -1 then
            while Index > -1 do begin
                StL.Delete(0);
                dec(Index);
			end;
        // OK, now first line contains the title but some lines may have tags wrong side of \n, so Normalise
        NormaliseList(StL);
        StL.Delete(0);
        StL.Insert(0, TitleFromID(ID, False, LTitle));
        Index := FindInStringList(StL, '</note-content>');       // but G-Note does not bother with nice newlines ????
        while Index < StL.Count do StL.Delete(Index);
        ProcessHeadings(StL);                                    // Makes Title big too !
        ProcessMarkUp(StL);
        ConvertMonoBlocks(STL);
        result := (Stl.Count > 2);
end;

function TExportCommon.ReplaceAngles(const Str : AnsiString) : AnsiString;
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


procedure TExportCommon.SayDebug(st: string; Always : boolean = false);
begin
    if not (DebugMode or Always) then exit;
    {$ifdef LCL}Debugln{$else}writeln{$endif}(St);
end;

function TExportCommon.FindInStringList(const StL : TStringList; const FindMe : string) : integer;
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
function TExportCommon.TitleFromID(ID: string; Munge : boolean; out LenTitle : integer): string;
var
    Doc : TXMLDocument;
    Node : TDOMNode;
    Index : integer = 1;
    FFN : string;
begin
    FFN := NotesDir + ID + '.note';
    if not FileExists(FFN) then begin
          FFN := ID;                        // OK, maybe is a SingleNote ?
          if not FileExists(FFN) then begin
                debugln('ERROR : File does not exist = '  + FFN);
                LenTitle := 0;
                exit('');
	        end;
	end;
	ReadXMLFile(Doc, FFN);
    try
        Node := Doc.DocumentElement.FindNode('title');
        result := Node.FirstChild.NodeValue;
    finally
        Doc.free;
    end;
    if Munge then begin
        // remove char that don't belong in a file name
        while Index <= length(Result) do begin
                if Result[Index] in [ ' ', ':', '.', '/', '\', '|', '"', '''' ] then begin
                    Result[Index] := '_';
                end;

                inc(Index);
        end;
        Result := copy(Result, 1, 32);
	end;
    LenTitle := length(Result);
end;

// -------------- Convert to Fixed width BLOCK text -------------

// We look for a line wrapped in a pair of backticks, allow white space to left
function TExportCommon.IsAllMono(St: String): boolean;
var
    I : integer = 1;
begin
    // for a st = 'abc' the test "if St[3] = '`'" is valid
    if St.CountChar('`') <> 2 then exit(False);
    while I <= St.Length do begin
        if St[i] = '`' then begin
            i := St.Length;
            while (St[i] in [' ', char(10), char(13) ])
                    and (i > 0) do dec(i);
            exit((I > 0) and (St[i] = '`'));             // Absolutly must exit here, we have played with i
		end;
        if  St[i] = ' '  then exit(False);
        inc(i);
	end;
    Result := False;
end;

procedure TExportCommon.MakeMonoBlock(var St : string);
// Remove two backticks, add four leading spaces
begin
    St := St.Remove(St.IndexOf('`'), 1);
    St := St.Remove(St.IndexOf('`'), 1);
    St := '    ' + St;
end;

procedure TExportCommon.ConvertMonoBlocks(STL : TStringList);
var
    I : integer = 0;
    St : string;
    PrevMono : boolean = false;
begin
    while i < Stl.Count do begin
        if PrevMono and (Stl[i] = '') then begin
            StL.Delete(i);
            PrevMono := False;
            continue;
		end;
		if IsAllMono(StL[i]) then begin
            St := STL[i];
            MakeMonoBlock(St);
            STl.Insert(i, St);
            STL.Delete(i+1);
            PrevMono := True;
		end else PrevMono := False;
		inc(i);
	end;
end;

// This version uses the CommonMark model of noting heading with ---- ===== on line underneath
procedure TExportCommon.ProcessHeadings(StL : TStringList);
var
    i : integer = 1;    // Skip first two lines because they are title and the ==== markup.
    PosI, L : integer;
    AddedHeading : Boolean = false;
begin
    // We arrive here with a clean title in first st, lets mark it up as really big.
    StL.Insert(1, '===========');
    repeat
        inc(i);
        if not AddedHeading then begin
            StL.Insert(i, '');
            inc(i);
		end;
        AddedHeading := False;
		if (StL.Strings[i] = '') or (StL.strings[i][1] <> '<') then continue;
		if copy(Stl.Strings[i], 1, length('<size:large>')) = '<size:large>' then begin
            PosI := pos('</size:large>', Stl.Strings[i]);
            if PosI = 0 then continue;
            L := length(Stl.Strings[i]);
            if PosI -1 + length('</size:large>') = L then begin
                StL.insert(i, copy(Stl.Strings[i], length('<size:large>')+1,
                        L - length('<size:large></size:large>')));
                StL.Delete(i+1);
                inc(i);
                StL.Insert(i, '--------');
                AddedHeading := True;
			end;
		end;
        if copy(Stl.Strings[i], 1, length('<size:huge>')) = '<size:huge>' then begin
            PosI := pos('</size:huge>', Stl.Strings[i]);
            if PosI = 0 then continue;
            L := length(Stl.Strings[i]);
            if PosI -1 + length('</size:huge>') = L then begin
                StL.insert(i, copy(Stl.Strings[i], length('<size:huge>')+1,
                        L - length('<size:huge></size:huge>')));
                StL.Delete(i+1);
                inc(i);
                StL.Insert(i, '========');
                AddedHeading := True;
			end;
		end;
	until I >= StL.Count-1;
end;

(* procedure TExportNote.ProcessHeadings(StL : TStringList);
var
    i : integer = -1;
    PosI, L : integer;
    //Blar : string;
begin
    repeat
        inc(i);
        if (StL.Strings[i] = '') or (StL.strings[i][1] <> '<') then continue;
        if copy(Stl.Strings[i], 1, length('<size:large><bold>')) = '<size:large><bold>' then begin
            //blar := Stl.Strings[i];
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
            //blar := Stl.Strings[i];
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
end;          *)


function TExportCommon.RemoveNextTag(var St : String; out Tag : string) : integer;
var
    TStart, TEnd, StartAt : integer;
begin
    StartAt := 0;
    {writeln('==== Working on : [' + St + ']');}
    repeat
        Tag := '';
        TStart := St.IndexOf('<', StartAt) + 1;
        TEnd   := St.IndexOf('>', TStart) +1;
        if (TStart > 0) and (TEnd > 0) and (TEnd > TStart) then begin
            Tag := copy(St, TStart, TEnd - TStart +1);
            {writeln('Tag = [' + Tag + ']');}
            if (Tag = '<sub>') or (Tag = '</sub>') or
                    (Tag = '</html>') or (Tag = '<html>') then begin       // they are MD tags, may be more .....
                StartAt := TEnd +1;
                {writeln('RNT Tag=' + Tag + ' St=[' + St + '] and StartAt=' + inttostr(StartAt));
                writeln('Next IndexOf < is ' + inttostr(St.IndexOf('<', StartAt) + 1));
                writeln('Next IndexOf > is ' + inttostr(St.IndexOf('>', StartAt) + 1));}
                continue;
            end;
            {writeln('Removing Tag [' + Tag + ']');}
            delete(St, TStart, TEnd - TStart +1);
            exit(TStart);
	    end
	    else begin { writeln('Exit TStart=' + inttostr(TSTart) + ' TEnd=' + inttostr(TEnd));} exit(0); end;
    until false;
end;

procedure TExportCommon.ProcessMarkUp(StL : TStringList);
var
    BoldOn : boolean = false;
    ItalicOn : boolean = false;
    MonoOn : boolean = false;
    //highlightOn : boolean = false;
    SmallOn : boolean = false;
    StrikeOutOn : boolean = false;
    StIndex, ChIndex : integer;
    Tag, TempSt, NewTag : string;
begin
    StIndex := -1;
    ChIndex := 1;
    while StIndex < StL.Count -1 do begin
        inc(StIndex);
        if (length(StL.Strings[StIndex]) < 2) then continue;     // no room for a tag in there.
        {writeln('========== Looking at ' + StL.Strings[StIndex]);}
        TempSt := '';
        if ItalicOn then TempSt := TempSt + '*';
        if BoldOn then TempSt := TempSt + '**';
        if StrikeoutOn then TempSt := TempSt + '~~';
        if MonoOn then TempSt := TempSt + '`';
        if SmallOn then TempSt := TempSt + '<html><sub>';

        TempSt := TempSt + StL.Strings[StIndex];
        TempSt := TempSt.Replace('<bold></bold>', '', [rfReplaceAll]);
        TempSt := TempSt.Replace('</bold></italic><bold>', '</bold></italic> <bold>', [rfReplaceAll]);
        // ToDo : replace above line with something generic that puts a space between a string of 'off's and a string of 'on's
        TempSt := TempSt.Replace('<list><list-item dir="ltr">', '* ');
        // ToDo : when we have one bullet point after another, remove one blank line between
        {writeln('==== First Cut ' + TempSt);}
        repeat
            ChIndex := RemoveNextTag(TempSt, Tag);
            case Tag of
                '<bold>' :        begin NewTag := '**';     BoldOn :=      True;  end;
                '</bold>' :       begin NewTag := '**';     BoldOn :=      False; end;
                '<italic>' :      begin NewTag := '*';      ItalicOn :=    True;  end;
                '</italic>' :     begin NewTag := '*';      ItalicOn :=    False; end;
                '<monospace>' :   begin NewTag := '`';      MonoOn :=      True;  end;
				'</monospace>' :  begin NewTag := '`';      MonoOn :=      False; end;
                '<size:small>' :  begin NewTag := '<sub>' ; SmallOn :=     True;  end;
                '</size:small>' : begin NewTag := '</sub>'; SmallOn :=     False; end;
                '<strikeout>'  :  begin NewTag := '~~';     StrikeoutOn := True;  end;               // Does strikeout belong here ??
                '</strikeout>'  : begin NewTag := '~~';     StrikeoutOn := False; End;               // ''
			else
                NewTag := '';
            end;
            if not NewTag.IsEmpty then begin
                TempSt := TempSt.Insert(ChIndex-1, NewTag);
                {writeln('Old Tag was ' + Tag + '  NewTag is ' + NewTag); }
			end;
		until ChIndex < 1;
        if BoldOn then TempSt := TempSt + '**';
        if ItalicOn then TempSt := TempSt + '*';
        if StrikeoutOn then TempSt := TempSt + '~~';
        if MonoOn then TempSt := TempSt + '`';
        if SmallOn then TempSt := TempSt + '</sub></html>';
        TempSt := ReplaceAngles(TempSt);                                  // +++++++++++++++++++++
        StL.Insert(StIndex, TempSt);
        StL.Delete(StIndex + 1);
	end;
end;


// ----------------------  N O R M A L I S I N G ------------------------------------

// Deals with 'off' tags that need to be moved up to the para they apply to.
procedure TExportCommon.MoveTagUp(const StL : TStringList; const StIndex : integer; var TagSize : integer);
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

function TExportCommon.MoveTagRight(var St: string): boolean;
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
function TExportCommon.MoveTagLeft(var St: string): boolean;
var
    Index : integer;
begin
    Index := Pos(' </', St);
    if Index = 0 then exit(False);
    delete(St, Index, 1);
    insert(' ', St, St.IndexOf('>', Index)+2);          // 2 ? IndexOf rets a zero based and we want to go one past
    Result := true;
end;

function TExportCommon.OnTagAtEnd(St : string) : integer;
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
function TExportCommon.OffTagAtStart(St : string) : integer;
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
procedure TExportCommon.MoveTagDown(const StL : TStringList; const StIndex, TagSize : integer);
var
Tag : string;
begin
    Tag := copy(StL.strings[StIndex], length(StL.strings[StIndex])-TagSize+1, TagSize);
    StL.Insert(StIndex, copy(StL.strings[StIndex], 1, length(StL.strings[StIndex])-TagSize));
    StL.Delete(StIndex+1);
    StL.Insert(StIndex+1, Tag+StL.Strings[StIndex+1]);
    StL.Delete(StIndex+2);
end;

// When there is an off tag and and complementry on tag with nothing between they
// are redundent and we remove them right here and now. No excuses !
// Rets True is it made a change, repeat until it finds nothing to do.
function TExportCommon.RemoveRedundentTag(var St : string) : boolean;
var
    OffTag : integer = 0;            // String Helpers are zero based !
    Tag1, Tag2 : string;      // migth get, eg monospace for a fixed spacing tag

    function Removed(const OffSet : integer; TagSt : string; replace : boolean) : boolean;
    begin
        if OffSet < 0 then exit(False);
        if St.Substring(OffSet, length(TagSt)) = TagSt then begin
            St := St.Remove(OffSet, length(Tag1 + Tag2) +1);
            if Replace then
                St := St.Insert(OffSet, ' ');
            exit(True);
		end else Result := False;
	end;

begin
    while(true) do begin
        OffTag := st.IndexOf('</', OffTag);
        if OffTag >= 0 then begin
            Tag1 := St.Substring(OffTag, st.IndexOf('>', OffTag) - OffTag +1);     // ie thats full tag
            Tag2 := Tag1.Remove(Tag1.IndexOf('/', 1), 1);
            // we target Tag1Tag2 or reversed, with and without a space between
            if Removed(OffTag, Tag1+Tag2, False) then exit(True);
            if Removed(OffTag, Tag1+' '+Tag2, True) then exit(True);
            if Removed(OffTag-length(Tag2), Tag2+Tag1, False) then exit(True);
            if Removed(OffTag-length(Tag2), Tag2+' ' + Tag1, True) then exit(True);
            // If still here, that offtag was not associated with an immediate on tag.
            inc(OffTag);
            continue;
		end else exit(False);       // no more offtags left to consider or maybe on offtags at all
	end
     { Loop: Find next offtag, exit false if we cannot find one
      try and find a matching ontag with nothing between.
      If above fails, goto LOOP:
    }
end;

procedure TExportCommon.NormaliseList(STL : TStringList);
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
    StIndex := 0;                      // Redundent, sequencial tags.
    while StIndex < StL.Count do begin
        TempSt := STL[StIndex];
        if RemoveRedundentTag(TempSt) then begin
            while RemoveRedundentTag(TempSt) do;        // in case more than one in line
            StL.Insert(StIndex, TempSt);
            StL.Delete(StIndex + 1);
		end;
		inc(StIndex);
	end;
end;




end.

