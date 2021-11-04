unit tb_utils;
{   Copyright (C) 2017-2021 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

}

{ A very simple unit that provides some utilities and simple Date / Time functions specificially
  tuned for tomboy-ng.

  It provides a means to convert a ISO8601 string to a TDataTime and back again
  with microSecond precision.
  Note that while the TDateTime will store it with that sort of precision, existing
  methods like now() are limited to milliSecond.

  TryISO8601ToDate() will accept no decimal places after a second or exactly three.
  No more or no less. So, we will pass unchanged if no decimal point, if there is
  one, we will remove it and process the content ourselves.

  A safe way to make a human readable date time string from any UTC TDateTime -

  var DT : TDateTime; St : string;
  if MyTryISO8601ToDate(DateSt, DT) then
        St := MyFormatDateTime(DT)
  else BadThingsHappened();

  We could make a simpler function that just does it but I have found real problems
  with date strings and am inclined to be careful.

  -------
  This unit is used in both TomboyTools and tomboy-ng, keep them in sync !!!!
  -------


  HISTORY :
  2021/01/29  Added TB_MakeFileName
  2021/05/11  FindInStringList was not checking last line of list
  2021/07/30  Added some methods from TT_Utils, need to merge back to TB-NG
  2021/07/31  A fix to ensure that </note-content> is removed with metadata
  2021/08/02  Merged back here from TomboyTools.
  2021/08/27  Added the constants for multilevel bullets.
  2021/10/26  User selectable date stamp format
}


{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils {$ifndef TESTRIG}, KMemo {$ifdef Linux}, libnotify{$endif}{$endif};

                       // pass 0 to MaxDateStampIndex, various datetime formats
function TB_DateStamp(Index : integer) : string;


                        // True if looks like an ID, 36 char and dash as #9
function IDLooksOK(const ID : string) : boolean;
                        // Gets sent a string that is converted into something suitable to use as base filename
function TB_MakeFileName(const Candidate : string) : string;

function MyFormatDateTime(aUTCDateTime : TDateTime; HumanReadable : boolean = false) : string;

                        // Will take a range of ISO-8601 dates and convert to DateTime, either local or UTC
                        // Uses TryISO8601ToDate for all greater than uSec, then adds uSec back in.
                        // If ReturnUTC is false returns local time
function MyTryISO8601ToDate(DateSt : string; out OutDT : TDateTime; ReturnUTC : boolean = true) : boolean;

function GetUTCOffset() : string;

                        // returns a string with current datetime in a format like the Tomboy schema
function TB_GetLocalTime: ANSIstring;

                        // A version of MyTryISO8601ToDate that does not report errors as well.
function TB_GetGMTFromStr(const DateStr: ANSIString): TDateTime;

                        // Use whenever we are writing content that may contain <>& to XML files
                        // If DoQuotes is true, we also convert ' and " (for xml attributes).
function RemoveBadXMLCharacters(const InStr : ANSIString; DoQuotes : boolean = false) : ANSIString;

                        // Note we restore only < > &,  Tomboy does not encode " or ' in Values (but must in attributes)
function RestoreBadXMLChar(const Str : AnsiString) : AnsiString;

                        // returns a version of passed string with anything between < > removed
function RemoveXml(const St : AnsiString) : AnsiString;
                        // Returns (0-x) index of string that contains passed term, -1 if not present
function FindInStringList(const StL : TStringList; const FindMe : string) : integer;

                        // Passed  FFN, thats <path><ID><.note> and returns the Title, munge indicates
                        // make it suitable for use as a file name, an empty ret string indicates error
function GetTitleFromFFN(FFN: string; Munge : boolean{; out LenTitle : integer}): string;

                        // Remove all content up to and including <note-content ...> and all content
                        // including and after </note-content>.  Because we cannot guarantee that these
                        // lines are on their own, we will need to poke into individual lines.
                        // Maybe tolerant of gnote format.
procedure RemoveNoteMetaData(STL : TStringList);

function SayDebugSafe(st: string) : boolean;

function TB_ReplaceFile(const SourceFile, DestFile : string) : boolean;

                        // Escapes any double inverted commas and backslashs it finds in passed string.
function EscapeJSON(St : string) : string;

                        // Removes a NoteBook tag from a note
function RemoveNoteBookTag(const FullFileName, NB : string) : boolean;

{$ifdef Linux}
// Linux only uses libnotify, Win and MacOS work through TrayIcon
procedure ShowNotification(const Title, Message : string; ShowTime : integer = 6000);
{$endif}

// These are constants that refer to Bullet Levels, we map the KMemo names here.
// Using them requires that we 'use' kmemo here. If not use'd, will still compile.
// Each one MUST resolve to a different value in KMemo, do not overload.

{$if declared(pnuCircleBullets)}      // Defined in KMemo in later versions (mid to late 2021)
const
  BulletOne   = pnuTriangleBullets;
  BulletTwo   = pnuBullets;
  BulletThree = pnuCircleBullets;
  BulletFour  = pnuArrowOneBullets;
  BulletFive  = pnuArrowTwoBullets;
  BulletSix   = pnuLetterlo;
  BulletSeven = pnuRomanLo;
  BulletEight = pnuArabic;
  // BulletNine  = pnuArabic;       // Messes with case statements, 8 is our limit !
{$endif}

const
  MaxDateStampIndex = 4;            // Zero based index to date/Time Formats
implementation

uses dateutils, {$IFDEF LCL}LazLogger, {$ENDIF} {$ifdef LINUX} Unix, {$endif}           // We call a ReReadLocalTime();
        laz2_DOM, laz2_XMLRead, FileUtil;

const ValueMicroSecond=0.000000000011574074;            // ie double(1) / double(24*60*60*1000*1000);

{$ifdef Linux}
// Linux only uses libnotify, Win and MacOS work through TrayIcon
procedure ShowNotification(const Title, Message : string; ShowTime : integer = 6000);
{$ifndef TESTRIG}
var
    LNotifier : PNotifyNotification;
begin
    notify_init(argv[0]);
    LNotifier := notify_notification_new (pchar(Title), pchar(Message), pchar('dialog-information'));
    notify_notification_set_timeout(LNotifier, ShowTime);                // figure is mS
    notify_notification_show (LNotifier, nil);
    notify_uninit;
{$else}
begin
{$endif}
end;

{$endif}

function RemoveNoteBookTag(const FullFileName, NB : string) : boolean;
var
    InFile, OutFile: TextFile;
    InString : string;
begin
    AssignFile(InFile, FullFileName);
    AssignFile(OutFile, FullFileName + '-temp');
    Reset(InFile);
    Rewrite(OutFile);
    while not eof(InFile) do begin
        readln(InFile, InString);
        // Note, this leaves an empty set of <tags></tags>, does that matter ?
        if Pos('<tag>system:notebook:' + NB + '</tag>', InString) = 0 then
            writeln(OutFile, InString);
    end;
    CloseFile(OutFile);
    CloseFile(InFile);
    Result := TB_ReplaceFile(FullFileName + '-temp', FullFileName);
    if not Result then
        debugln('ERROR, RemoveNoteBookTag failed to mv '
                + FullFileName+ '-temp to ' + FullFileName);
end;


function TB_ReplaceFile(const SourceFile, DestFile : string) : boolean;
begin
    if not FileExists(SourceFile) then exit(SayDebugSafe('TB_ReplaceFile Failed to find ' + SourceFile));
    {$ifdef WINDOWS}
    if not DeleteFile(DestFile) then exit(SayDebugSafe('TB_ReplaceFile Failed to delete ' + DestFile));
    {$endif}
    result := RenameFile(SourceFile, DestFile);
    if not Result then SayDebugSafe('TB_ReplaceFile Failed to rename ' + SourceFile + ' to ' + DestFile);
end;

function TB_DateStamp(Index : Integer) : string;
// make sure that you adjust MaxDateStampIndex (above) if adding formats
begin
    result := ' date error ';
    case Index of
        0 : result := FormatDateTime(' YYYY-MM-DD hh:mm:ss ', now());             // ISO 8601, 2020-09-14 08:37
        1 : result := FormatDateTime(' dddd dd mmmm YYYY hh:mm am/pm ', now());   // Monday 29 December 2021 8:37 am    much of the world
        2 : result := FormatDateTime(' dddd, mmmm dd, YYYY hh:mm am/pm ', now()); // Monday, December 29, 2021 8:37 am  US style
        3 : result := FormatDateTime(' mmmm dd, YYYY hh:mm am/pm ', now());       // January 21, 2016 8:37 am           US without DOW
        4 : result := FormatDateTime(' YYYY-MM-DD dddd hh:mm:ss ', now());        // Monday 2020-09-14 08:37            ISO with added DOW
    end;
end;

// Escapes any double inverted commas and backslashs it finds in passed string.
function EscapeJSON(St : string) : string;
begin
      Result := St.Replace('\', '\\', [rfReplaceAll] );
      Result := Result.Replace('"', '\"', [rfReplaceAll] );
end;

function IDLooksOK(const ID : string) : boolean;
  begin
      if length(ID) <> 36 then exit(false);
      if pos('-', ID) <> 9 then exit(false);
      result := True;
  end;

  // Gets sent a string that is converted into something suitable to use as base filename
function TB_MakeFileName(const Candidate : string) : string;
var
   Ch : char;
begin
    Result := StringReplace(Candidate, #32, '_', [rfReplaceAll]);
    for ch in [ '/', '\', '*', '.', '#', '%', '{', '}', '?', '&' ] do
        Result := StringReplace(Result, Ch, '-', [rfReplaceAll]);
    if Result.EndsWith('-') or Result.endswith('_') then
        Result := Result.Remove(Result.Length-1);
end;


function GetUTCOffset() : string;
var
    Off : longint;
begin
    Off := GetLocalTimeOffset();                      // We assume that we are passed a UTC time !
    if (Off div -60) >= 0 then Result := '+'
    else Result := '-';
    if abs(Off div -60) < 10 then Result := Result + '0';
    Result := Result + inttostr(abs(Off div -60)) + ':';
    if (Off mod 60) = 0 then
    Result := result + '00'
    else Result := Result + inttostr(abs(Off mod 60));
end;

function MyFormatDateTime(aUTCDateTime : TDateTime; HumanReadable : boolean = false) : string;
var
    mSec, Cnt : longint;
    Remainder : double;
    DT : TDateTime;
    St : string;
begin
    DT := UniversalTimeToLocal(aUTCDateTime);
    Result := FormatDateTime('YYYY-MM-DD', DT);
    if HumanReadable then
        exit(Result + ' ' + FormatDateTime('hh:mm:ss', DT));      // Gee, that was easy !
    mSec := trunc(Frac(DT) / OneMilliSecond);
    remainder := frac(DT) - (mSec * OneMilliSecond);
    Cnt := trunc((1000*remainder) / OneMilliSecond);
    if Cnt > 999 then Cnt := 999;                     // We are playing down near limits of precision
    St := inttostr(Cnt);
    while length(St) < 4 do St := St + '0';           // NOTE : I require exactly 7 decimal places, you may not !
    Result := Result + 'T' + FormatDateTime('hh:mm:ss.zzz', mSec * OneMilliSecond) + St;
    Result := Result + GetUTCOffset();
end;

function MyTryISO8601ToDate(DateSt : string; out OutDT : TDateTime; ReturnUTC : boolean = true) : boolean;
var
    I : integer;
    St : string = '';
begin
    OutDT := 0.0;
    if DateSt = '' then exit(False);
    Result := True;
    I := pos('.', DateSt);                          // if we have decimal point, we have stuff to do.
    if I > 0 then begin                             // TryISO8601ToDate cannot handle string with decimals of a second
        delete(DateSt, I, 1);                       // Remove decimal point
	    while I  < length(DateSt) do begin
            if DateSt[I] in ['0'..'9'] then begin
                St := St + DateSt[I];               // save digits to use later
                delete(DateSt, I, 1);
            end else break;
		end;
        // The first six digits in St represent microseconds. we will stop there.
        while length(St) > 6 do delete(St, length(St), 1);
        while length(St) < 6 do St := St + '0';
	end;
    if TryISO8601ToDate(DateSt, OutDT, ReturnUTC) then begin               // WARNING - apparently this is a FPC320 only feature
        if I > 0 then
            OutDT := OutDT + (St.ToDouble() * ValueMicroSecond);           // ValueMicroSecond is Regional const,  eg
	end else result := False;                                              // ValueMicroSecond := 1.0 / double(24*60*60*1000*1000);
end;

function TB_GetLocalTime: ANSIstring;
	    // The retuned date string includes four digits at the end representing a count
	    // of 100 picoSeconds units. We cannot get that sort of precision and who needs it but
	    // I have realised as tomboy-ng uses the datestring as a key to check that notes
	    // are identical during a blind sync.  So, instead of making those four digits 0000
	    // I will add a random number, not significent for timing but a usefull increase
	    // in certaintly.
var
   ThisMoment : TDateTime;
   Res : ANSIString;
   Off : longint;
   PicoSeconds : string;
begin
    {$ifdef LINUX}
    ReReadLocalTime();    // in case we are near daylight saving time changeover
    {$endif}
    ThisMoment:=Now;
    PicoSeconds := inttostr(random(9999));
    while length(PicoSeconds) < 4 do PicoSeconds := '0' + PicoSeconds;
    Result := FormatDateTime('YYYY-MM-DD',ThisMoment) + 'T'
                    // + FormatDateTime('hh:mm:ss.zzz"0000"',ThisMoment);
                    + FormatDateTime('hh:mm:ss.zzz',ThisMoment) + PicoSeconds;
    Off := GetLocalTimeOffset();
    if (Off div -60) >= 0 then Res := '+'
	else Res := '-';
	if abs(Off div -60) < 10 then Res := Res + '0';
	Res := Res + inttostr(abs(Off div -60)) + ':';
       	if (Off mod 60) = 0 then
		Res := res + '00'
	else Res := Res + inttostr(abs(Off mod 60));
    Result := Result + res;
end;

function TB_GetGMTFromStr(const DateStr: ANSIString): TDateTime;
begin
    MyTryISO8601ToDate(DateStr, Result, True);
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

function RemoveXml(const St : AnsiString) : AnsiString;
var
    X, Y : integer;
    FoundOne : boolean = false;
begin
    Result := St;
    repeat
        FoundOne := False;
        X := Pos('<', Result);      // don't use UTF8Pos for byte operations
        if X > 0 then begin
            Y := Pos('>', Result);
            if Y > 0 then begin
                Delete(Result, X, Y-X+1);
                FoundOne := True;
            end;
        end;
    until not FoundOne;
    Result := trim(Result);
end;


function FindInStringList(const StL : TStringList; const FindMe : string) : integer;
var
    I : integer = 0;
begin
    if Stl = nil then exit(-1);
    while i < StL.Count {-1} do begin
        if pos(FindMe, StL.strings[i]) > 0 then
            exit(i);
        inc(i);
	end;
	result := -1;
end;

function SayDebugSafe(st: string) : boolean;
begin
    {$ifdef LCL}Debugln{$else}writeln{$endif}(St);
    result := false;
end;

function GetTitleFromFFN(FFN: string; Munge : boolean{; out LenTitle : integer}): string;
var
    Doc : TXMLDocument;
    Node : TDOMNode;
//    Index : integer = 1;
begin
    if not FileExists(FFN) then begin
        SayDebugSafe('ERROR : File does not exist = '  + FFN);
        exit('');
	end;
	ReadXMLFile(Doc, FFN);
    try
        Node := Doc.DocumentElement.FindNode('title');
        result := Node.FirstChild.NodeValue;
    finally
        Doc.free;
    end;
    if Munge then
        Result := TB_MakeFileName(Result);

{    begin
        // remove char that don't belong in a file name
        while Index <= length(Result) do begin
                if Result[Index] in [ ' ', ':', '.', '/', '\', '|', '"', '''' ] then begin
                    Result[Index] := '_';
                end;

                inc(Index);
        end;
        Result := copy(Result, 1, 42);      // Because 42 is the meaning of life
	end;  }
    if Result = '' then SayDebugSafe('Title not found' + FFN);
    //LenTitle := length(Result);
end;


// Remove all content up to and including <note-content ...> and all content
// including and after </note-content>.  Because we cannot guarantee that these
// lines are on their own, we will need to poke into individual lines.
procedure RemoveNoteMetaData(STL : TStringList);
var
    Index, CutOff : integer;
    St : string;
begin
    // First, the trailing end.
    Index := FindInStringList(StL, '</note-content>');       // this is the line its on but we may have content on the same line
    St := Stl[Index];
    CutOff := pos('</note-content>', St);
    if CutOff <> 1 then begin
        delete(St, CutOff, 1000);
        STL.Delete(Index);
        STL.Insert(Index, St);
        inc(Index);
    end;
    // Now Get rid of the remainder.
    while Index < StL.Count do StL.Delete(Index);
    // OK, now the start of the list
    Index := FindInStringList(StL, '<note-content');
    while Index > 0 do begin
        STL.Delete(0);
        dec(Index);
    end;
    St := STL[0];
    CutOff := St.IndexOf('>', St.IndexOf('<note-content')) +1;      // Zero based index !
    delete(St, 1, CutOff);
    STL.Delete(0);
    STL.Insert(0, St);
end;





end.

