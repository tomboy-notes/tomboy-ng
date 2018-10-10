unit syncutils;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dateutils, LazLogger;

type TSyncAction=(SyUnset,      // initial state, should not be like this at end.
                SyNothing,      // This note, previously sync'ed has not changed.
                SyUploadNew,    // This a new local note, upload it.
                SyUploadEdit,   // A previously synced note, edited locally, upload.
                SyDownload,     // A new or edited note from elsewhere, download.
                SyDeleteLocal,  // Synced previously but no longer present on server, delete locally
                SyDeleteRemote, // Marked as having been deleted locally, so remove from server.
                SyClash);       // Edited both locally and remotly, policy or user must decide.

        // Indicates the readyness of a sync connection
type TSyncAvailable=(SyncReady,         // We are ready to sync, looks good to go.
                    SyncNoLocal,        // We don't have a local manifest, only an error if config things there should be one.
                    SyncNoRemoteMan,    // No remote manifest, an uninitialized repo perhaps ?
                    SyncNoRemoteRepo,   // Filesystem is OK but does not look like a repo.
                    SyncBadRemote,      // Has either Manifest or '0' dir but not both.
                    SyncNoRemoteDir,    // Perhaps sync device is not mounted ?
                    SyncNoRemoteWrite,  // no write permission, do not proceed!
                    SyncMismatch,       // Its a repo, Captain, but not as we know it.
                    SyncXMLError);      // Housten, we have an XML error in a manifest !

type
  	PNoteInfo=^TNoteInfo;
  	TNoteInfo = record
        // Value : integer;
	    ID : ANSIString;
        LastChangeGMT : TDateTime;  // Compare less or greater than but not Equal !
        CreateDate : ANSIString;
        LastChange : ANSIString;    // leave as strings, need to compare and TDateTime uses real
        Rev : Integer;              // Not used for uploads, Trans knows how to inc its own.
        Deleted: Boolean;           // don't think we use this .....
        Action : TSyncAction;
        Title : ANSIString;
    end;

 type                                 { ---------- TNoteInfoList ---------}

   { TNoteInfoList }

   TNoteInfoList = class(TList)
    private
     	function Get(Index: integer): PNoteInfo;
    public
        destructor Destroy; override;
        function Add(ANote : PNoteInfo) : integer;
        function FindID(const ID : ANSIString) : PNoteInfo;
        function ActionName(Act : TSyncAction) : string;
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


                                    { ------------- TClashRecord ------------- }
        { A couple of types used to manage the data involved in handling
          a sync clash.
        }
 type
    TClashRecord = record
        //Title : ANSIString;
        NoteID : ANSIString;
        //ServerLastChange : ANSIString;
        //LocalLastChange : ANSIString;
        ServerFileName : string;
        LocalFileName : string;
    end;

            // Note that cdDoNothing may not be allowed .....
// type    TClashDecision = (cdDownload, cdUpload, cdDoNothing);

                 {  These next two definitions are how we allow TB_Sync to manipulate the
                   GUI objects around it. We will declare a variable of the type and the
                   calling process will put the address of the functions it wants called
                   in that var when it creates this object. TClashRecord and TClashDecision
                   are defined in SyncUtils.
                }

 //type    TProceedFunction = function(const ClashRec : TClashRecord): TClashDecision of object;
 type    TProceedFunction = function(const ClashRec : TClashRecord): TSyncAction of object;

type    TMarkNoteReadonlyProcedure = procedure(const FileName : string; const WasDeleted : Boolean = False) of object;


function GetGMTFromStr(const DateStr: ANSIString): TDateTime;
function GetLocalTime: ANSIstring;      // Note this function is duplicated in TB_Sync.
        // Returns the LCD string, '' and setting Error to other than '' if something wrong
function GetNoteLastChangeSt(const FullFileName : string; out Error : string) : string;

            { -------------- implementation ---------------}
implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils;

function GetNoteLastChangeSt(const FullFileName : string; out Error : string) : string;
var
	Doc : TXMLDocument;
	Node : TDOMNode;
    // LastChange : string;
begin
    if not FileExistsUTF8(FullFileName) then begin
        Error := 'ERROR - File not found, cant read note change date for remote ' +  FullFileName;
        exit('');
	end;
	try
		ReadXMLFile(Doc, FullFileName);
		Node := Doc.DocumentElement.FindNode('last-change-date');
        Result := Node.FirstChild.NodeValue;
	finally
        Doc.free;		// xml errors are NOT caught in calling process
	end;
end;




function GetLocalTime: ANSIstring;
var
   ThisMoment : TDateTime;
   Res : ANSIString;
   Off : longint;
begin
   // Note this function exits in tomboy-ng's settings unit, here for testing
    {$ifdef LINUX}
    //ReReadLocalTime();    // in case we are near daylight saving time changeover
    {$endif}
    ThisMoment:=Now;
    Result := FormatDateTime('YYYY-MM-DD',ThisMoment) + 'T'
                   + FormatDateTime('hh:mm:ss.zzz"0000"',ThisMoment);
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

function TNoteInfoList.ActionName(Act: TSyncAction): string;
begin
    Result := ' Unknown ';
    case Act of
        SyUnset : Result := ' Unset ';
        SyNothing : Result := ' Nothing ';
        SyUploadNew  : Result := ' UploadNew ';   // we differentiate in case of a write to remote fail.
        SyUpLoadEdit : Result := ' UpLoadEdit ';
        SyDownload: Result := ' Download ';
        SyDeleteLocal  : Result := ' DeleteLocal ';
        SyDeleteRemote : Result := ' DeleteRemote ';
        SyClash : Result := ' Clash ';
    end;
    while length(result) < 15 do Result := Result + ' ';
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
    if DateStr = '' then exit(0);                // Empty string
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

        // Suspect we don't need this ....
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

