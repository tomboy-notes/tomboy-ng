unit ttutils;

{ License - see tomboy-ng license information }

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils;


function RestoreBadXMLChar(const Str : AnsiString) : AnsiString;
                                { Returns index of a string that contains the passed FindMe, -1 if not found }
function FindInStringList(const StL : TStringList; const FindMe : string) : integer;
function RemoveBadXMLCharacters(const InStr : ANSIString; DoQuotes : boolean = false) : ANSIString;



implementation


function FindInStringList(const StL : TStringList; const FindMe : string) : integer;
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

// Note we restore only < > &,  Tomboy does not encode " or ' in Values (but must in attributes)
function RestoreBadXMLChar(const Str : AnsiString) : AnsiString;
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

function RemoveBadXMLCharacters(const InStr : ANSIString; DoQuotes : boolean = false) : ANSIString;
// Don't use UTF8 versions of Copy() and Length(), we are working bytes !
// It appears that Tomboy only processes <, > and & , we also process single and double quote.
// http://xml.silmaril.ie/specials.html
var
   //Res : ANSIString;
   Index : longint = 1;
   Start : longint = 1;
begin
    Result := '';
   while Index <= length(InStr) do begin
   		if InStr[Index] = '<' then begin
             Result := Result + Copy(InStr, Start, Index - Start);
             Result := Result + '&lt;';
             inc(Index);
             Start := Index;
			 continue;
		end;
  		if InStr[Index] = '>' then begin
             Result := Result + Copy(InStr, Start, Index - Start);
             Result := Result + '&gt;';
             inc(Index);
             Start := Index;
			 continue;
		end;
  		if InStr[Index] = '&' then begin
             // debugln('Start=' + inttostr(Start) + ' Index=' + inttostr(Index));
             Result := Result + Copy(InStr, Start, Index - Start);
             Result := Result + '&amp;';
             inc(Index);
             Start := Index;
			 continue;
		end;
        if DoQuotes then begin
      		if InStr[Index] = '''' then begin                // Ahhhh how to escape a single quote ????
                 Result := Result + Copy(InStr, Start, Index - Start);
                 Result := Result + '&apos;';
                 inc(Index);
                 Start := Index;
    			 continue;
    		end;
            if InStr[Index] = '"' then begin
                 Result := Result + Copy(InStr, Start, Index - Start);
                 Result := Result + '&quot;';
                 inc(Index);
                 Start := Index;
                 continue;
		    end;
        end;

        inc(Index);
   end;
   Result := Result + Copy(InStr, Start, Index - Start);
end;


end.

