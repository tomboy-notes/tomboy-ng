unit TB_datetime;

{ A very simple unit that provides some simple Date / Time functions specificially
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

}


{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils;


function MyFormatDateTime(aUTCDateTime : TDateTime; HumanReadable : boolean = false) : string;

function MyTryISO8601ToDate(DateSt : string; out OutDT : TDateTime; ReturnUTC : boolean = true) : boolean;

function GetUTCOffset() : string;

implementation

uses dateutils;

const ValueMicroSecond=0.000000000011574074;            // ie double(1) / double(24*60*60*1000*1000);

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
    Result := True;
    I := pos('.', DateSt);                          // if we have decimal point, we have stuff to do.
    if I > 0 then begin
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
	end else result := False;                                               // ValueMicroSecond := 1.0 / double(24*60*60*1000*1000);
end;


end.

