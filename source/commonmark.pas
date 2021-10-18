unit commonmark;

{$mode objfpc}{$H+}

{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    Exports a note in a subset of commonmark


    Create the object, optionally give it a directory to look in (and set DoPOFile ?). Call GetMDcontent()
    with an ID (that is, a filename without extension) and a created list to fill in with content.

    Has some limitations that relate, to some extent, to the MarkDown/CommonMark.
    1) Monospace may be presented as either "leading spaces" or wrapped in BackTicks.
       The Leading Space model is used where the whole line (ie para) is mono, we add
       four spaces and strip out any other markup in the line. Looks better for blocks.
       Backticks are used where the mono is in line. It can have extra markup but
       has to have backticks closest to text, does not work here ...
       So, summary, blocks of Mono cannot show any other markup.
    2) Large or Huge font (in tomboy) is only honoured if on a line by itself, become
       heading lines. Any in line Large or Huge is discarded. Small is preserved
       but cannot be displayed in github flavour of MD.
    3) Only bullet cha allowed in MD list here is * (officially others allowed but not here)
    4) When bullets (lists) mix, lists get priority, the mono drops back to inline mono.
    5) Cannot do highlight (but I try to preserve it during a Tb->GH->TB cycle).

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
    2021/10/17  Rewrite most of monospace code.
}

interface

uses
        Classes, SysUtils;

type

{ TExportCommon }

TExportCommon = class

    private
                        {returns -1 if there is not a tag starting at index (1 based), else ret length
                        of tag if there is a tag but its not the one we want. Ret 0 if right tag found.}
        function CheckForTag(index: integer; const Tag, St: string): integer;
        function ConvertBullets(Str: string): string;
        function ConvertMono(var InSt: string): boolean;
		function FindInStringList(const StL: TStringList; const FindMe: string): integer;
                                    // Make content suitable to write out as a PO file, no merging is going to happen !

		procedure ProcessHeadings(StL: TStringList);
		procedure ProcessMarkUp(StL: TStringList);

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

//        ConvertMonoBlocks(STL);
        result := (Stl.Count > 2);
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


{We have to deal with two sorts of mono, full line where we apply 4 spaces to left
and in-line where we use back ticks.

A four leading space line can have additional spaces and they are preserved. The
'four' is my choice, when converting back, I'll assume, if there are at least four
its mono, tag it up and remove first four spaces.  No other codes are allowed on that
line including backticks. A full line mono is a line that has text, has the mono html
tags at beginning and end of line. But other tags and whitespace are allowed between the start
and mono tag and between the </monospace> and end of line.

github will only display other font styles with mono if we use back tick and then
only if the other tags appear, initiall, before the backtick, thus **`bold mono`**
As the backtick looks very ugly in a block, I will use it only in-line and therefore
leading space mono will need to be stripped of any other enhancements.
}


function TExportCommon.CheckForTag(index : integer; const Tag, St : string) : integer;
begin
    if (St.Length < Index) or (St[Index] <> '<') then exit(-1);
    if copy(St, Index, Tag.Length) = Tag then exit(0);
    // OK, so it should be a tag but not the one we want.
    Result := 1;
    while (St.Length >= (Index + Result)) do begin
        if St[Index+Result] = '>' then exit(Result+1);
        inc(Result);
    end;
    result := -1;
end;

function TExportCommon.ConvertMono(var InSt : string) : boolean;
var
    St : string;
    RetValue, i : integer;
begin
    result := false;
    if pos('<list>', InSt) > 0 then begin          // Lists have priority over Mono
        InSt := InSt.Replace('<monospace>', '`', [rfReplaceAll]);
        InSt := inSt.Replace('</monospace>', '`', [rfReplaceAll]);
    end;
    if (pos('<monospace>', InSt) = 0) or (pos('</monospace>', inSt) = 0) then
        exit;
    St := InSt;
    i := 1;
    while St.Length >= i do
        if St[i] = ' ' then inc(i)
        else break;                                 // whitespace allowed and retained
    RetValue := CheckForTag(i, '<monospace>', St);  // start with first non-space we find
    while RetValue > 0 do begin
        St := St.Remove(i, RetValue);               // Remove any tags that appear before <m>
        RetValue := CheckForTag(i, '<monospace>', St);
    end;
    if RetValue <> 0 then begin
        InSt := InSt.Replace('<monospace>', '`', [rfReplaceAll]);
        InSt := inSt.Replace('</monospace>', '`', [rfReplaceAll]);
        exit;
    end;
    // OK, we now have a leading mono tag, i points to its start, add 11 for next char after tag
    i := St.IndexOf('<', i+11-1) +1;               // we know there is at least one there.
    RetValue :=  CheckForTag(i, '</monospace>', St);         // one based
    while RetValue > 0 do begin                     // a non-target tag, remove
        St := St.Remove(i-1, RetValue);             // zero based
        RetValue := St.IndexOf('<', i) +1;          // 0 based. Can we find another ?
        if RetValue > 0 then begin                  // i remains one based.
            i := RetValue;
            RetValue := CheckForTag(i, '</monospace>', St);
        end;
    end;
    // OK, here i should be pointing to </m>, add tag length and clear away any trailing tags

    // i := pos('</monospace>', St) + 12;              // must still be there.
    i := i + 12;                                    // length </m> tag
    RetValue := CheckForTag(i, '<xxxx>', St);       // only interested in pos or neg numbers
    while RetValue > 0 do begin
        St := St.Remove(i-1, RetValue);             // remove is zero based.
        RetValue :=  CheckForTag(i-1, '<xxxx>', St);       // Kek ?  why -1 ?????
    end;
    if St.Length < i then begin
        St := St.Replace('<monospace>', '', [rfReplaceAll]);
        St := St.Replace('</monospace>', '', [rfReplaceAll]);
        InSt := '    ' + St;                        // yes, we passed all the tests, change to leading space mono
        Result := true;                             // ToDo - should i remove any tags between <m> and </m> ?
    end else begin
        InSt := InSt.Replace('<monospace>', '`', [rfReplaceAll]);
        InSt := inSt.Replace('</monospace>', '`', [rfReplaceAll]);
    end;
    //writeln(InSt);
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
        if not AddedHeading then begin    // this adds a blank line between paras, MD style
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
    DeleteNext : boolean = false;       // no blank lines following Monospace
begin
    StIndex := -1;
    while StIndex < StL.Count -1 do begin
        inc(StIndex);
        if DeleteNext and (Stl[StIndex] = '') then begin
            Stl.Delete(StIndex);
            DeleteNext := False;
            dec(StIndex);
            continue;
        end;
        if (length(StL.Strings[StIndex]) < 2) then continue;     // no room for a tag in there.
        TempSt := StL.Strings[StIndex];
        DeleteNext := ConvertMono(TempSt);
        TempSt := TempSt.Replace('<bold>', '**', [rfReplaceAll]);
        TempSt := TempSt.Replace('</bold>', '**', [rfReplaceAll]);
        TempSt := TempSt.Replace('<italic>', '*', [rfReplaceAll]);
        TempSt := TempSt.Replace('</italic>', '*', [rfReplaceAll]);
//        TempSt := TempSt.Replace('<monospace>', '`', [rfReplaceAll]);
//        TempSt := TempSt.Replace('</monospace>', '`', [rfReplaceAll]);
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

