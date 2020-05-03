unit syncutils;
{
    A Unit to support the tomboy-ng sync unit
    Copyright (C) 2018 David Bannon
    See attached licence file.

    HISTORY
    2018/10/25  Much testing, support for Tomdroid.
    2018/10/28  Added SafeGetUTCC....
    2018/06/05  Func. to support Tomboy's sync dir names, rev 431 is in ~/4/341
    2019/06/07  Don't check for old sync dir model, for 0 its the same !
    2019/07/19  Added ability to escape ' and " selectivly, attributes ONLY
}
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dateutils, LazLogger
    {$ifdef LINUX}, Unix {$endif}
    ;

type TSyncOption = (AlwaysAsk, UseServer, UseLocal, MakeCopy);	// Relating to sync clash pref in config file

type TSyncTransport=(
                SyncFile,  // Sync to locally available dir, things like smb: mount, google drive etc
                SyncNextCloud,  // Sync to NextCloud using Nextcloud Notes
                SyncAndroid,  // Simple one to one Android Device
                SyncNone); // Not syncing

type TSyncAction=(SynUnset,      // initial state, should not be like this at end.
                SynNothing,      // This note, previously synced has not changed.
                SynUploadNew,    // This a new local note, upload it.
                SynUploadEdit,   // A previously synced note, edited locally, upload.
                SynDownload,     // A new or edited note from elsewhere, download.
                SynDeleteLocal,  // Synced previously but no longer present on server, delete locally
                SynDeleteRemote, // Marked as having been deleted locally, so remove from server.
                SynCopy,         // Make a copy hen replace by server
                SynError,
                SynAllRemote,    // Clash Decision - Use remote note for all subsquent clashes
                SynAllCopy,      // Clash Decision - Make copies for all subsquent clashes
                SynAllLocal,     // Clash Decision - Use local note for all subsquent clashes
                SynAllNewest,    // Clash Decision - Use newest note for all subsquent clashes
                SynAllOldest);   // Clash Decision - Use oldest note for all subsquent clashes

        // Indicates the readyness of a sync connection
type TSyncAvailable=(SyncNotYet,        // Initial state.
                    SyncReady,          // We are ready to sync, looks good to go.
                    SyncNoLocal,        // We dont have a local manifest, only an error if config thinks there should be one.
                    SyncNoRemoteMan,    // No remote manifest, an uninitialized repo perhaps ?
                    SyncNoRemoteRepo,   // Filesystem is OK but does not look like a repo.
                    SyncBadRemote,      // Has either Manifest or '0' dir but not both.
                    SyncNoRemoteDir,    // Perhaps sync device is not mounted, Tomdroid not installed ?
                    SyncNoRemoteWrite,  // no write permission, do not proceed!
                    SyncMismatch,       // Its a repo, Captain, but not as we know it.
                    SyncXMLError,       // Housten, we have an XML error in a manifest !
                    SyncBadError,       // Some other error, must NOT proceed.
                    SyncNetworkError);  // Remove server/device not responding


type
  	PNoteInfo=^TNoteInfo;

        TNoteInfo = record
                  ID : ANSIString;            // The 36 char ID
                  CreateDate : ANSIString;
                  CreateDateGMT : TDateTime;
                  LastChange : ANSIString;
                  LastChangeGMT : TDateTime;
                  LastMetaChange : ANSIString;
                  LastMetaChangeGMT : TDateTime;
                  Version : String;
                  Rev : Integer;
                  LastSync: ANSIString;
                  LastSyncGMT : TDateTime;
                  Action : TSyncAction;
                  Title : String;
                  Content : String;
                  OpenOnStartup : boolean;
                  Pinned : boolean;
                  CursorPosition : integer;
                  SelectBoundPosition : integer;
                  Width : integer;
                  Height : integer;
                  X : integer;
                  Y : integer;
                  Deleted : boolean;
        end;

 type                                 { ---------- TNoteInfoList ---------}

   { TNoteInfoList }

   TNoteInfoList = class(TList)
    private
     	function Get(Index: integer): PNoteInfo;
    public
        ServerID : string;              // Partially implemented, don't rely yet ....
        LastSyncDateSt : string;        // Partially implemented, don't rely yet ....
        LastSyncDate : TDateTime;       // Partially implemented, don't rely yet ....
        LastRev : integer;              // Partially implemented, don't rely yet ....
        destructor Destroy; override;
        function Add(ANote : PNoteInfo) : integer;
        function FindID(const ID : ANSIString) : PNoteInfo;
        function ActionName(Act : TSyncAction) : string;
        property Items[Index: integer]: PNoteInfo read Get; default;
    end;

                                    { ------------- TClashRecord ------------- }
        { A couple of types used to manage the data involved in handling
          a sync clash.
        }
 type
    TClashRecord = record
        //Title : ANSIString;
        //NoteID : ANSIString;
        RemoteNote : PNoteInfo;
        LocalNote : PNoteInfo;
    end;


 type    TClashFunction = function(const ClashRec : TClashRecord): TSyncAction of object;

type    TMarkNoteReadonlyProcedure = procedure(const FileName : string; const WasDeleted : Boolean = False) of object;


      function GetLocalNotePath(path : String; NoteID : string = ''): string;

      // Takes a normal Tomboy DateTime string and converts it to UTC, ie zero offset
      function ConvertDateStrAbsolute(const DateStr : string) : string;

      // A save, error checking method to convert Tomboy's ISO8601 33 char date string
      function SafeGetUTCfromStr(const DateStr : string; out DateTime : TDateTime; out ErrorMsg : string) : boolean;

      // Ret GMT from tomboy date string, 0.0 on error or unlikely date.
      function GetGMTFromStr(const DateStr: ANSIString): TDateTime;

      function GetTimeFromGMT(d : TDateTime) : String;

      function GetCurrentTimeStr() : String;

      // Returns the LCD string, '' and setting Error to other than '' if something wrong
      function GetLocalNoteLastChange(const rep : String; const ID : string; out Error : string) : string;

      // returns false if GUID does not look OK
      function NoteIDLooksOK(const ID : string) : boolean;

      function FileToNote(xml : String; NoteInfo : PNoteInfo) : boolean;

      // Use whenever we are writing content that may contain <>& to XML files
      // If DoQuotes is true, we also convert ' and " (for xml attributes).
      function RemoveBadXMLCharacters(const InStr : ANSIString; DoQuotes : boolean = false) : ANSIString;

      { ret true if it really has removed the indicated file. Has proved
        necessary to do this on two end user's windows boxes. Writes debuglns
        if it has initial problems, returns F and sets ErrorMsg if fails.}
      function SafeWindowsDelete(const FullFileName : string; var ErrorMsg : string) : boolean;

RESOURCESTRING
  rsNewUploads      = 'New Uploads    ';
  rsEditUploads     = 'Edit Uploads   ';
  rsDownloads       = 'Downloads      ';
  rsSynCopies       = 'Copied notes   ';
  rsLocalDeletes    = 'Local Deletes  ';
  rsRemoteDeletes   = 'Remote Deletes ';
  rsDoNothing       = 'Do Nothing     ';
  rsUndecided       = 'Unresolved     ';

  rsNoNotesNeededSync = 'No notes needed syncing. You need to write more.';
  rsNotesWereDealt = ' notes were dealt with.';
  rsChangeExistingSync = 'Change existing sync connection ?';
  rsNotRecommend = 'Generally not recommended.';
  rsNextBitSlow = 'Next bit can be a bit slow, please wait';

            { -------------- implementation ---------------}
implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils;

function SafeWindowsDelete(const FullFileName : string; var ErrorMsg : string) : boolean;
begin
    // This whole block is here because of issue #132 where windows seemed to have problems
    // moving, deleting a note before a new version is copied over. Is the problem
    // that windows deletefileUTF8() is not settings its return value correctly ??
    if not DeleteFile(FullFileName) then begin
    	ErrorMsg := SysErrorMessage(GetLastOSError);
    	Debugln('Failed using DeleteFileUTF8 - file name is :' + FullFilename);
    	Debugln('OS Error Msg : ' + ErrorMsg);
    	if not FileExistsUTF8(FullFileName) then
            debugln('But, FileExists says its gone, proceed !')
        else begin
    		debugln('I can confirm its still there .');
    	    Debugln('Trying a little sleep...');
    	    sleep(10);
    	    if not DeleteFileUTF8(FullFileName) then begin
                if not FileExistsUTF8(FullFileName) then
                    debugln('DeleteFileUTF8 says it failed but FileExists says its gone, proceed !')
                else exit(false);
            end;
        end;
    end;
    Result := true;
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


function GetLocalNoteLastChange(const rep : String; const ID : string; out Error : string) : string;
var
   Doc : TXMLDocument;
   Node : TDOMNode;
   filename : string;
begin
   filename := GetLocalNotePath(rep,ID);

    if not FileExists(filename) then
    begin
        Error := 'ERROR - File not found, cant read note change date for remote ' +  filename;
        exit('');
    end;

    try
       ReadXMLFile(Doc, filename);
       Node := Doc.DocumentElement.FindNode('last-change-date');
       Result := Node.FirstChild.NodeValue;
    except on E:Exception do begin
       Error := E.message;
       debugln(Error);
       Result := '';
       end;
    end;
end;


function GetCurrentTimeStr(): ANSIstring;
var
   ThisMoment : TDateTime;
   Res : ANSIString;
   Off : longint;
begin
    {$ifdef LINUX}
    ReReadLocalTime();    // in case we are near daylight saving time changeover
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
        SynUnset : Result := ' Unset ';
        SynNothing : Result := ' Nothing ';
        SynUploadNew  : Result := ' UploadNew ';   // we differentiate in case of a write to remote fail.
        SynUpLoadEdit : Result := ' UpLoadEdit ';
        SynDownload: Result := ' Download ';
        SynCopy: Result := ' MakeCopy ';
        SynDeleteLocal  : Result := ' DeleteLocal ';
        SynDeleteRemote : Result := ' DeleteRemote ';
        SynError : Result := ' ** ERROR **';
        SynAllLocal : Result := ' AllLocal ';
        SynAllCopy : Result := ' AllCopy ';
        SynAllRemote : Result := ' AllRemote ';
        SynAllNewest : Result := ' AllNewest ';
        SynAllOldest : Result := ' AllOldest ';
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

function SafeGetUTCfromStr(const DateStr : string; out DateTime : TDateTime; out ErrorMsg : string) : boolean;
begin
    ErrorMsg := '';
    if length(DateStr) <> 33 then begin
        ErrorMsg := 'Date String wrong length';
        DateTime := 0.0;
        exit(False);
    end;
    DateTime := GetGMTFromStr(DateStr);
    if DateTime < 1.0 then begin
        ErrorMsg := 'Invalid Date String';
        exit(False);
    end;
    if (DateTime > (now() + 36500)) or (DateTime < (Now() - 36500))  then begin
        ErrorMsg := 'Date beyond expected range';
        DateTime := 0.0;
        exit(False);
    end;
 		// TDateTime has integer part, no. of days, fraction part is fraction of day.
		// 100years ago or in future - Fail !
    exit(True);
end;

function GetLocalNotePath(path : String; NoteID : string = ''): string;
var
   s : String;
begin
    if(length(NoteID)>0)
    then Result := appendpathDelim(ChomppathDelim(path)) + NoteID + '.note'
    else Result := appendpathDelim(ChomppathDelim(path)) ;
end;


// Takes a normal Tomboy DateTime string and converts it to UTC, ie zero offset
function ConvertDateStrAbsolute(const DateStr : string) : string;
var
    Temp : TDateTime;
begin
    if DateStr = '' then exit('');                // Empty string
    // A date string should look like this -  2018-01-27T17:13:03.1230000+11:00 33 characters !
    // but on Android, its always             2018-01-27T17:13:03.1230000+00:00  ie GMT absolute
    if length(DateStr) <> 33 then begin
        debugln('ERROR ConvertDateStrAbsolute received invalid date string - [' + DateStr + ']');
        exit('');
    end;
    Temp := GetGMTFromStr(DateStr) {- GetLocalTimeOffset()};
    Result := FormatDateTime('YYYY-MM-DD',Temp) + 'T'
                   + FormatDateTime('hh:mm:ss.zzz"0000+00:00"',Temp);
end;

function GetTimeFromGMT(d : TDateTime) : String;
begin;
    Result := FormatDateTime('YYYY-MM-DD',d) + 'T'
                   + FormatDateTime('hh:mm:ss.zzz"0000+00:00"',d);

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
            exit(0.0);
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
            exit(0.0);
    	end;
    end;
    { writeln('Date is ', DatetoStr(Result), ' ', TimetoStr(Result));  }
end;

function NoteIDLooksOK(const ID : string) : boolean;
begin
    if length(ID) <> 36 then exit(false);
    if pos('-', ID) <> 9 then exit(false);
    result := True;
end;

function FileToNote(xml : String; NoteInfo : PNoteInfo) : boolean;
var
    Doc : TXMLDocument;
    Node : TDOMNode;

begin
     try
        ReadXMLFile(Doc, xml);
     except on E:Exception do begin debugln(E.message); exit(false); end;
     end;

     try
        Node := Doc.DocumentElement.FindNode('create-date');
        NoteInfo^.CreateDate := Node.FirstChild.NodeValue;
        if NoteInfo^.CreateDate <> '' then
           NoteInfo^.CreateDateGMT := GetGMTFromStr(NoteInfo^.CreateDate)
           else NoteInfo^.CreateDateGMT := 0;

        Node := Doc.DocumentElement.FindNode('last-change-date');
        NoteInfo^.LastChange := Node.FirstChild.NodeValue;
        if NoteInfo^.LastChange <> '' then
           NoteInfo^.LastChangeGMT := GetGMTFromStr(NoteInfo^.LastChange)
        else NoteInfo^.LastChangeGMT := 0;

        Node := Doc.DocumentElement.FindNode('last-metadata-change-date');
        NoteInfo^.LastMetaChange := Node.FirstChild.NodeValue;
        if NoteInfo^.LastMetaChange <> '' then
           NoteInfo^.LastMetaChangeGMT := GetGMTFromStr(NoteInfo^.LastMetaChange)
        else NoteInfo^.LastMetaChangeGMT := 0;

        NoteInfo^.Title := Doc.DocumentElement.FindNode('title').FirstChild.NodeValue;

        Node := Doc.DocumentElement.FindNode('text');
        NoteInfo^.Content := Node.FindNode('note-content').NodeValue;
        NoteInfo^.Version := Node.FindNode('note-content').Attributes.GetNamedItem('version').NodeValue;

        Node := Doc.DocumentElement.FindNode('open-on-startup');
        NoteInfo^.OpenOnStartup := (Node.FirstChild.NodeValue.ToLower = 'true');

        Node := Doc.DocumentElement.FindNode('pinned');
        NoteInfo^.Pinned := (Node.FirstChild.NodeValue.ToLower = 'true');

        Node := Doc.DocumentElement.FindNode('cursor-position');
        NoteInfo^.CursorPosition := StrToInt(Node.FirstChild.NodeValue);
        Node := Doc.DocumentElement.FindNode('selection-bound-position');
        NoteInfo^.SelectBoundPosition := StrToInt(Node.FirstChild.NodeValue);

        Node := Doc.DocumentElement.FindNode('width');
        NoteInfo^.Width := StrToInt(Node.FirstChild.NodeValue);
        Node := Doc.DocumentElement.FindNode('height');
        NoteInfo^.Height := StrToInt(Node.FirstChild.NodeValue);

        Node := Doc.DocumentElement.FindNode('x');
        NoteInfo^.X := StrToInt(Node.FirstChild.NodeValue);
        Node := Doc.DocumentElement.FindNode('y');
        NoteInfo^.Y := StrToInt(Node.FirstChild.NodeValue);

        //NoteInfo^.Source := Doc.ToString;
        NoteInfo^.Deleted := false;

     except on E:Exception do begin Doc.Free; debugln(E.message); exit(false); end;
     end;

     Doc.Free;
     Result := true;
end;



end.

