unit syncutils;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dateutils, LazLogger;

type TSyncAction=(Nothing, Upload, Download, DeleteLocal, DeleteRemote);

type
  	PNoteInfo=^TNoteInfo;
  	TNoteInfo = record
        // Value : integer;
	    ID : ANSIString;
        LastChangeGMT : TDateTime;  // Compare less or greater than but not Equal !
        CreateDate : ANSIString;
        LastChange : ANSIString;    // leave as strings, need to compare and TDateTime uses real
        Rev : Integer;              // Changed to int to force valid value !
        Deleted: Boolean;
        Action : TSyncAction;
        Title : ANSIString;
    end;

 type                                 { ---------- TNoteInfoList ---------}
   TNoteInfoList = class(TList)
    private
     	function Get(Index: integer): PNoteInfo;
    public
        destructor Destroy; override;
        function Add(ANote : PNoteInfo) : integer;
        function FindID(const ID : ANSIString) : PNoteInfo;
        property Items[Index: integer]: PNoteInfo read Get; default;
    end;

 type                                  { ----------- SyncReport ---------- }
     PSyncReport = ^SyncReport;
     SyncReport = record
         ID : ANSIString;              // Interesting, changing these to ShortString
         Action : ANSIString;          // uses slightly more memory. I wonder if its
         Message : ANSIString;         // quicker though ?  Test at some stage.
         Title : ANSIString;
         FullFileName : ANSIString;
 	end;


 type                                 { --------- TSyncReportList --------- }
     TSyncReportList = class(TList)
     private
     	function Get(Index : integer) : PSyncReport;
     public
         destructor Destroy; override;
         function Add(Report : PSyncReport) : integer;
         property Items[Index : integer] : PSyncReport read Get; default;
 	end;

function GetGMTFromStr(const DateStr: ANSIString): TDateTime;

implementation
{ ----------------  TNoteInfoList ---------------- }

function TNoteInfoList.Add(ANote : PNoteInfo) : integer;
begin
    result := inherited Add(ANote);
end;

{ This will be quite slow with a big list notes, consider an AVLTree ? }
function TNoteInfoList.FindID(const ID: ANSIString): PNoteInfo;
var
    Index : longint;
begin
    Result := Nil;
    for Index := 0 to Count-1 do begin
        if Items[Index]^.ID = ID then begin
            Result := Items[Index];
            exit()
        end;
    end;
end;

destructor TNoteInfoList.Destroy;
var
I : integer;
begin
    for I := 0 to Count-1 do
        dispose(Items[I]);
    inherited;
end;

function TNoteInfoList.Get(Index: integer): PNoteInfo;
begin
    Result := PNoteInfo(inherited get(Index));
end;

function GetGMTFromStr(const DateStr: ANSIString): TDateTime;
var
    TimeZone : TDateTime;
begin
    // A date string should look like this -     2018-01-27T17:13:03.1230000+11:00 33 characters !
    if length(DateStr) <> 33 then begin
        debugln('ERROR received invalid date string - [' + DateStr + ']');
        exit(0);
    end;
    try
	    if not TryEncodeTimeInterval( 	strtoint(copy(DateStr, 29, 2)),				// Hour
	    								strtoint(copy(DateStr, 32, 2)),				// Minutes
			        					0,				// Seconds
	                					0,				// mSeconds
	                					TimeZone)  then DebugLn('Fail on interval encode ');
    except on EConvertError do begin
        	DebugLn('FAIL on converting time interval ' + DateStr);
            DebugLn('Hour ', copy(DateStr, 29, 2), ' minutes ', copy(DateStr, 32, 2));
    	end;
    end;
    try
	    if not TryEncodeDateTime(strtoint(copy(DateStr, 1, 4)),   	// Year
	    			strtoint(copy(DateStr, 6, 2)),              // Month
	                strtoint(copy(DateStr, 9, 2)),				// Day
	                strtoint(copy(DateStr, 12, 2)),				// Hour
	                strtoint(copy(DateStr, 15, 2)),				// Minutes
	                strtoint(copy(DateStr, 18, 2)),				// Seconds
	                strtoint(copy(DateStr, 21, 3)),				// mSeconds
	                Result)  then DebugLn('Fail on date time encode ');
    except on EConvertError do begin
        	DebugLn('FAIL on converting date time ' + DateStr);

    	end;
    end;
    try
	    if DateStr[28] = '+' then
            Result := Result - TimeZone
		else if DateStr[28] = '-' then
            Result := Result + TimeZone
	    else debugLn('******* Bugger, we are not parsing DATE String ********');
    except on EConvertError do begin
        	DebugLn('FAIL on calculating GMT ' + DateStr);

    	end;
    end;
    { writeln('Date is ', DatetoStr(Result), ' ', TimetoStr(Result));  }
end;


{ -----------   TSyncReportList ----------- }

function TSyncReportList.Get(Index: integer): PSyncReport;
begin
    Result := PSyncReport(inherited get(Index));
end;

destructor TSyncReportList.Destroy;
var
    I : integer;
begin
    for I := 0 to Count-1 do dispose(Items[I]);
    inherited Destroy;
end;


function TSyncReportList.Add(Report: PSyncReport): integer;
{var
	Doc : TXMLDocument;
	Node : TDOMNode;       }
begin
{    Report.Title := 'File Not Found';
    if FileExistsUTF8(Report.FullFileName) then begin
		try
            Report.Title := 'Unknown Title';
            try
				ReadXMLFile(Doc, Report.FullFileName);
				Node := Doc.DocumentElement.FindNode('title');
        		Report.Title := Node.FirstChild.NodeValue;
            except 	on EXMLReadError do
            				Report.Title := 'Note has no Title ' + Report.FullFileName;
              		on EAccessViolation do
                            Report.Title := 'Access Violation ' + Report.FullFileName;
			end;
		finally
        	Doc.free;
		end;
    end else debugln('Add : File not found ' + Report.FullFileName);
    result := inherited Add(Report); }
end;

end.

