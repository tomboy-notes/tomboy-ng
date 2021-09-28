unit commonmark;

{$mode objfpc}{$H+}

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    Exports a note in a subset of commonmark

    This unit is, June 2021 the same as one in tomboy-ng.

    Create the object, optionally give it a directory to look in and set DoPOFile. Call GetMDcontent()
    with an ID (that is, a filename without extension) and a created list to fill in with content.
    Free.

    HISTORY
    2020-12-22  Extracted from the NextCloud Notes Branch
    2020-??-??  Moved the Normalising code into a stand alone unit.
    2021/06/15  Format lines that are all mono differenly so they show as a block.
    2021/06/29  Merged this file back to tomboy-ng
    2021/07/22  Make GetMDcontent more tolerent of passed ID/FFN
    2021/07/30  Now use someutuls fro tb_util instead of implementing itself. Must sync to TB-NG
    2021/07/30  Use the RemoveNoteMetaData( from TT_Utils, need merge TT_utils with TB_Utils
    2021/08/19  Rewrite ProcessMarkup to use ST.Replace() approach
    2021/09/28  Enabled multilevel bullets
}

interface

uses
        Classes, SysUtils;

type

{ TExportCommon }

TExportCommon = class        // based on TT export_notes, just takes a note ID and returns markdown

    private
        function ConvertBullets(Str: string): string;
			function FindInStringList(const StL: TStringList; const FindMe: string): integer;
                                    // Make content suitable to write out as a PO file, no merging is going to happen !

			procedure ProcessHeadings(StL: TStringList);
			procedure ProcessMarkUp(StL: TStringList);
            function IsAllMono(St : String) : boolean;
            procedure MakeMonoBlock(var St: string);
            procedure ConvertMonoBlocks(STL: TStringList);

    public
        DebugMode : boolean;
        NotesDir : string;       // dir were we expect to find our TB notes

                        // Takes a note ID (no extension) or a FFN inc path and .note
                        // and fills out the passed StringList that must have been created)
                        // with a commonmark version of the note.
                        // returns an empty list on error.
        function GetMDcontent(ID : string; STL : TstringList) : boolean;


end;


implementation

uses LazFileUtils{$ifdef LCL}, lazlogger {$endif}, laz2_DOM, laz2_XMLRead, notenormal, tb_utils;


function TExportCommon.GetMDcontent(ID : string; STL : TStringList): boolean;
{ This is same as function in TT but I have removed parts that do file i/o
  I am thinking I would be better using some XML methods, might avoid g-Note issues too. }
var
    Normaliser : TNoteNormaliser;
begin
        if FileExists(ID) then
                StL.LoadFromFile(ID)
        else
            if FileExists(NotesDir + ID + '.note') then
                   StL.LoadFromFile(NotesDir + ID + '.note')
            else exit(False);
        // OK, now first line contains the title but some lines may have tags wrong side of \n, so Normalise
        Normaliser := TNoteNormaliser.Create;
        Normaliser.NormaliseList(StL);
        Normaliser.Free;
        StL.Delete(0);
        STL.Insert(0, GetTitleFromFFN(NotesDir + ID + '.note', False));
        RemoveNoteMetaData(STL);
        ProcessHeadings(StL);                                    // Makes Title big too !
        ProcessMarkUp(StL);
        ConvertMonoBlocks(STL);
        result := (Stl.Count > 2);
end;


(*
procedure TExportCommon.SayDebug(st: string; Always : boolean = false);
begin
    if not (DebugMode or Always) then exit;
    {$ifdef LCL}Debugln{$else}writeln{$endif}(St);
end;
*)

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

// ToDo : fix eg bold monospace
// There is a problem here. We convert all lines that are all mono from `this` to
// haveing four leading spaces, looks much better with blocks of code eg.
// However, Remarkable at least does not display eg bold monospace is presented like
//     **Bold Code**
// It will however do what we want if it finds this -
// **`Bold Code`**
// Lets see what github does and consider staying with backticks for single mono lines.
// Will have to also reverse tag order and that might be messy.

procedure TExportCommon.ConvertMonoBlocks(STL : TStringList);
var
    I : integer = 0;
    St : string;
    PrevMono : boolean = false;
begin
    while i < Stl.Count do begin
        // Remove the unnecessary newlines between blocks.
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

{ must convert upto level 6 bullets to md. We use 3 spaces, ahead of marker to
indicate each level. In the xml, each level is indicated by an additional
wrap of <list><list-item dir="ltr">CONTENT</list-item></list>. Must start with
the deepest bullet and work back up. }
function TExportCommon.ConvertBullets(Str : string) : string;
var
    Pre, Post, Spaces : string;
    i : integer = 5;
    j : integer;
begin
    Result := Str;
    while i >= 0 do begin
        Pre := '';
        Post := '';
        Spaces := '';
        for j := 0 to i do begin
            Pre := Pre + '<list><list-item dir="ltr">';
            Post := Post + '</list-item></list>';
        end;
        for j := 1 to (i*3) do
            Spaces := Spaces + ' ';
        Result := Result.Replace(Pre, Spaces + '* ');
        Result := Result.Replace(Post, '');
        dec(i);
    end
end;

procedure TExportCommon.ProcessMarkUp(StL : TStringList);
var
    StIndex : integer;
    TempSt: string;
begin
    StIndex := -1;
    while StIndex < StL.Count -1 do begin
        inc(StIndex);
        if (length(StL.Strings[StIndex]) < 2) then continue;     // no room for a tag in there.
        TempSt := StL.Strings[StIndex];
        TempSt := TempSt.Replace('<bold>', '**', [rfReplaceAll]);
        TempSt := TempSt.Replace('</bold>', '**', [rfReplaceAll]);
        TempSt := TempSt.Replace('<italic>', '*', [rfReplaceAll]);
        TempSt := TempSt.Replace('</italic>', '*', [rfReplaceAll]);
        TempSt := TempSt.Replace('<monospace>', '`', [rfReplaceAll]);
        TempSt := TempSt.Replace('</monospace>', '`', [rfReplaceAll]);
        TempSt := TempSt.Replace('<size:small>', '<sub>', [rfReplaceAll]);
        TempSt := TempSt.Replace('</size:small>', '</sub>', [rfReplaceAll]);
        TempSt := TempSt.Replace('<strikeout>', '~~', [rfReplaceAll]);
        TempSt := TempSt.Replace('</strikeout>', '~~', [rfReplaceAll]);
        TempSt := TempSt.Replace('<size:large>', '', [rfReplaceAll]);
        TempSt := TempSt.Replace('</size:large>', '', [rfReplaceAll]);
        TempSt := TempSt.Replace('<size:huge>', '', [rfReplaceAll]);
        TempSt := TempSt.Replace('</size:huge>', '', [rfReplaceAll]);
        TempSt := ConvertBullets(TempSt);
//        TempSt := TempSt.Replace('<list><list-item dir="ltr">', '* ');
//        TempSt := TempSt.Replace('</list-item></list>', '');
        TempSt := RestoreBadXMLChar(TempSt);
        StL.Insert(StIndex, TempSt);
        StL.Delete(StIndex + 1);
	end;
end;


end.

