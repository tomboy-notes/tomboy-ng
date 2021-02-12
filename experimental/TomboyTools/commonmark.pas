unit commonmark;

{$mode objfpc}{$H+}

{   *  Copyright (C) 2020 David Bannon
    *  See attached licence file.
}
{   Exports a note in a subset of commonmark, this is, at present, direct copy from unit
    of the same name from the so far, ill fated NextCloud Notes tomboy-ng branch.

    This unit is, Feb 2021 just about same as one in tomboy-ng except that here we have
    moved the normalising code into a standalone unit, should do exactly the same in tomboy-ng

    Create the object, optionally give it a directory to look in and set DoPOFile. Call GetMDcontent()
    with an ID (that is, a filename without extension) and a list to fill in with content.
    Free.

    History
        2020-12-22  Extracted from the NextCloud Notes Branch
        2020-??-??  Moved the Normalising code into a stand alone unit.
}

interface

uses
        Classes, SysUtils;

type

{ TExportCommon }

TExportCommon = class        // based on TT export_notes, just takes a note ID and returns markdown

    private
			function FindInStringList(const StL: TStringList; const FindMe: string): integer;
                                    // Make content suitable to write out as a PO file, no merging is going to happen !

			procedure ProcessHeadings(StL: TStringList);
			procedure ProcessMarkUp(StL: TStringList);
			function RemoveNextTag(var St: String; out Tag: string): integer;
			function ReplaceAngles(const Str: AnsiString): AnsiString;
			procedure SayDebug(st: string; Always: boolean=false);
			function TitleFromID(ID: string; Munge: boolean; out LenTitle: integer): string;

    public
        DebugMode : boolean;
        NotesDir : string;       // dir were we expect to find our TB notes

                        { Takes a note ID (no extension) and fills out the passed StringList
                          that must have been created) with a commonmark version of the note.
                          returns an empty list on error. }
        function GetMDcontent(ID : string; STL : TstringList) : boolean;


end;


implementation

uses LazFileUtils{$ifdef LCL}, lazlogger {$endif}, laz2_DOM, laz2_XMLRead, notenormal;


function TExportCommon.GetMDcontent(ID : string; STL : TStringList): boolean;
{ This is same as function in TT but I have removed parts that do file i/o
  I am thinking I would be better using some XML methods, might avoid g-Note issues too. }
var
    LTitle : integer;
    Index : integer;
    Normaliser : TNoteNormaliser;
begin
        StL.LoadFromFile(NotesDir + ID + '.note');
        Index := FindInStringList(StL, '<title>');       // include < and > in search term so sure its metadate
        if Index > -1 then
            while Index > -1 do begin
                StL.Delete(0);
                dec(Index);
			end;
        // OK, now first line contains the title but some lines may have tags wrong side of \n, so Normalise
        Normaliser := TNoteNormaliser.Create;
        Normaliser.NormaliseList(StL);
        Normaliser.Free;
        //NormaliseList(StL);
        StL.Delete(0);
        StL.Insert(0, TitleFromID(ID, False, LTitle));
        Index := FindInStringList(StL, '</note-content>');       // but G-Note does not bother with nice newlines ????
        while Index < StL.Count do StL.Delete(Index);
        ProcessHeadings(StL);                                    // Makes Title big too !
        ProcessMarkUp(StL);
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
begin
    if not FileExists(NotesDir + ID + '.note') then begin
        debugln('ERROR : File does not exist = '  + NotesDir + ID + '.note');
        LenTitle := 0;
        exit('');
	end;
	ReadXMLFile(Doc, NotesDir + ID + '.note');
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

// This version does heading in the leading ### model
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


end.

