unit notenormal;
{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------
}

{ A unit to 'normalise' a Tomboy Note, that is ensure tags remain with the para they
  relate to. Makes the xml a lot prettier and, more importantly, heaps easier t parse when exporting.
  One day, may incorporate into the tomboy-ng saving engine.  Needed by the POT and
  CommonMark exporters.

  It recieves a TStringList containing a note (probably directly loaded from disk).

  HISTORY :
    2021/08/19  Bug in RemoveRedundentTags that sometimes ate character after tag pair

}

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

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
		function RemoveRedundentTag(var St: string): boolean;
    public
        procedure NormaliseList(STL: TStringList);
end;


implementation


uses lazlogger;

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

// When there is an off tag and and complementry on tag (or visa versa) with nothing
// between they are redundent and we remove them right here and now. No excuses !
// Rets True if it made a change, repeat until it finds nothing to do.
function TNoteNormaliser.RemoveRedundentTag(var St : string) : boolean;
var
    OffTag : integer = 0;            // String Helpers are zero based !
    Tag1, Tag2 : string;      // might get, eg monospace for a fixed spacing tag

    // Ret T if it finds TagSt starting at Offset (and it removed it)
    function Removed(const OffSet : integer; TagSt : string; replace : boolean) : boolean;
    begin
        if OffSet < 0 then exit(False);
        if St.Substring(OffSet, length(TagSt)) = TagSt then begin
            //St := St.Remove(OffSet, length(Tag1 + Tag2) +1);
            St := St.Remove(OffSet, length(TagSt));
            if Replace then
                St := St.Insert(OffSet, ' ');
            exit(True);
		end else Result := False;
	end;

begin

//if pos('Column Mode', St) > 0 then
//writeln('---- ' + St);

    while(true) do begin
        OffTag := st.IndexOf('</', OffTag);
        if OffTag >= 0 then begin
            Tag1 := St.Substring(OffTag, st.IndexOf('>', OffTag) - OffTag +1);     // ie thats full tag
            Tag2 := Tag1.Remove(Tag1.IndexOf('/', 1), 1);
            // we target Tag1Tag2 or reversed, with and without a space between
            if Removed(OffTag, Tag1+Tag2, False) then exit(True);
            if Removed(OffTag, Tag1+' '+Tag2, True) then exit(True);
            if Removed(OffTag-length(Tag2), Tag2+Tag1, False) then exit(True);       // ERROR HERE
            if Removed(OffTag-length(Tag2), Tag2+' ' + Tag1, True) then exit(True);
            // If still here, that offtag was not associated with an immediate on tag.
            inc(OffTag);
            continue;
		end else exit(False);       // no more offtags left to consider or maybe on offtags at all
	end
     { Loop: Find next offtag, exit false if we cannot find one
      try and find a matching ontag with nothing between.
      If above fails, goto LOOP:  I don't think so.
    }
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
    StIndex := 0;                      // Redundent, sequencial tags.

//writeln('~~~~ Norm 3 ' + STL[10]);

    while StIndex < StL.Count do begin
        TempSt := STL[StIndex];
        if RemoveRedundentTag(TempSt) then begin
            while RemoveRedundentTag(TempSt) do;        // in case more than one in line
            StL.Insert(StIndex, TempSt);
            StL.Delete(StIndex + 1);
		end;
		inc(StIndex);
	end;

//writeln('~~~~ Norm 4 ' + STL[10]);

end;


end.

