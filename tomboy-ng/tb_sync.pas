unit TB_Sync;

{
 * Copyright (C) 2017 David Bannon
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

{	A class that knows broard rules about syncing. It will have (at least) two
	children, one for file use, one for remote server use. Only the FileSync is
	implemented here.
	We also derive a simpler class that just knows how to update the local manifest
	file when a tomboy-ng deletes a local note. It checks if the note already
	appears in note-revsions and if it does, moves it to the note-deletions section.
	Does nothing if the note has not yet been synced, that is, it does not appear
	note-revisions.
}

{	2017/12/06	Cleaned up some unimportant debug statements.
	2017/12/29  Added some comments that may help someone understand sync process
				No functional change.
	2017/12/31  Added TTomboyLocalManifest class.
	2018/01/02  Removed a %#4## writeln that was crashing windows.
}


{$mode delphi} 			// This is just me being lazy, in Delphi mode we don't need to
						// dereference pointers to record structures. I should fix it ....

interface

uses
		Classes, SysUtils;

type                              // This record and its list are used for several different things
  	PNoteInfo=^TNoteInfo;         // might possibly be better to specialise a bit ?
  	TNoteInfo = record
        // Value : integer;
		ID : ANSIString;
        GMTDate : TDateTime;
    	CreateDate : ANSIString;
    	LastChange : ANSIString;
        Rev : ANSIString;
        Deleted: Boolean;
        Title : ANSIString;
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

type								{ ------------- TClashRecord ------------- }
	TClashRecord = record
    		Title : ANSIString;
    		NoteID : ANSIString;
            ServerLastChange : ANSIString;
            LocalLastChange : ANSIString;
		end;

type	TClashDecision = (cdDownload, cdUpload, cdDoNothing);

type    TProceedFunction = function(const ClashRec : TClashRecord): TClashDecision of object;


type                                { ---------  TTomboySyncCustom -------- }

{ TTomboySyncCustom }

TTomboySyncCustom = Class
  private
    			{ Contains the local manifest version of Server ID }
	ServerID : ANSIString;
    FNotesDir, FRemoteManifestDir, FLocalManifestDir : ANSIString;
    LocalLastSyncDateSt : ANSIString;
    LocalRevSt : ANSIString;

	function GetNoteTitle(FullFileName: ANSIString): ANSIString;
    procedure SetNotesDir(Dir : ANSIString);
    procedure SetRemoteManifestDir(Dir : ANSIString);
	procedure SetLocalManifestDir(Dir : ANSIString);
                { Adds an item to the Report List, don't call before list is created in DoSync() }
    procedure AddReport(const Action, ID, Path, Message : ANSIString);
    			{ Returns a TDateTime from the passed date time string }
	function GetDateFromStr(const DateStr : ANSIString) : TDateTime;
     			{ Puts data from local mainifest file into a LocalManifest : NoteInfoList
                  Reads both additions and deletions.
                  Returns False if an error occured that would prevent proceeding.
                  Set SkipFile to .T. during initialisation. }
	function ReadLocalManifest(SkipFile : boolean = False) : boolean;
    			{ writes new local manifest file, WriteDeletes is true when just
                  adding a local deleted note to list, other info is preserved.}
    function WriteNewManifest(WriteDeletes : boolean = false) : boolean; virtual;
    			{ Initiates a new revision, safe to call repeatedly. }
	function MakeNewRevision() : boolean; virtual; abstract;
    			{ Reads a note last change date, returning GMTTime and puts actual string into LastChange }
	function GetNoteChangeGMT(const FullFileName : ANSIString; out LastChange : ANSIString) : TDateTime;
  				{ Fills the Regional NoteInfoList with info about the local notes. Reads
    			the actual Notes directory and the notes last change date. }
	procedure GetListLocalNotes(); virtual;
	function UploadNote(const FileName : ANSIString) : boolean; virtual; abstract;
	function DownLoadNote(const FileName, Rev : ANSIString) : boolean; virtual; abstract;
    			{ Does the whole sync process, false if something went wrong, check ErrorMessage }
	procedure UpdateNextManifest(const ID, Rev : ANSIString);
 				{ Returns a string with a timestamp smilar to Tomboy uses in its XML files
          		bit lazy really, this is a copy of one in SaveNote.pas.   				}
	function GetLocalTime(): ANSIstring;
    procedure ClearLists();
    			{ Passes on users choice to Upload, download or nothing. Calls a function
                  passed here from the calling process. }
	function ProceedWith(FileID, Rev : ANSIString) : TClashDecision; virtual; abstract;
  public


    			{ the calling process must pass a function address to this var. It will
                  be called if the sync process finds a sync class where both copies
                  of a note have changed since last sync.}
    ProceedFunction : TProceedFunction;
    			{ if set True, hopefully will prevent most writes to disk. Hopefully... }
	TestMode : Boolean;
                { Reports, to the console, on what its doing }
    VerboseMode : Boolean;
    // DebugMode : boolean;
    			{ A list of data extracted from the existing Remote manifest file }
	NoteInfoListRem : TNoteInfoList;
    			{ A list of data extracted from the existing local manifest file, it
    			has only UID (not filename) and whether or not they have been deleted }
	NoteInfoListLocalManifest : TNoteInfoList;
    			{ A list of data built by examining the notes in notes directory }
	NoteInfoListLoc : TNoteInfoList;
    			{ A list used to make the new manifest files after sync finishes. }
	ManifestList : TNoteInfoList;
    			{ We keep details of all note file actions, whether we do them or not }
    ReportList : TSyncReportList;
	ErrorMessage : ANSIString;
     			{ is empty until a new revision has been initiated }
	NewRevision : ANSIString;
    			{ Indicates we need new mainifests generated, set if any uploads or down loads or deletes happen }
    NewManifest : Boolean;
	LastSyncDate : TDateTime;
    			{ just a temp function to test the Proceed callback, delete at some stage }
    procedure TestProceed();
    			{ reads the last time this client was synced. Puts that into LastSyncDate }
	function SetlastSyncdate() : boolean;
	function DoSync(UseLocal, UseRemote : boolean) : boolean; virtual; abstract;
   				{ reads the local server manifest for the Server ID, puts it in ServerID,
          		returns False if its not present. That would indicate we are not
          		set up to do Sync. }
	function GetLocalServerID() : boolean;
	destructor Destroy; override;
    property NotesDir : ANSIString read FNotesDir write SetNotesDir;
    property RemoteManifestDir : ANSIString read FRemoteManifestDir write SetRemoteManifestDir;
    property LocalManifestDir : ANSIString read FLocalManifestDir write SetLocalManifestDir;
end;


type				                          { -------- TTomboyFileSync ----------}

{ TTomboyFileSync }

TTomboyFileSync = Class(TTomboySyncCustom)

				{ Does the whole sync process, false if something went wrong, check ErrorMessage }
	function DoSync(UseLocal, UseRemote : boolean) : boolean; override;
				{ Returns the current remote manifest revision number }
	function GetCurrentRevision() : longint;
             { Gets the ServerID from remote manifest and compares it to the local value
               previously stored in ServerID. Returns False if the two ServerID's
               don't match or we cannot get the remote one. Call early
               to see if the dir is setup as a Repo. Call AFTER GetLocalServerID()   }
    function CheckRemoteServerID() : boolean;
	        { This function will establish a new connection to a filesync repo. It ALWAYS
	          writes a new local mainifest, will write a new remote mainifest if there
	          is not one already there (with new GUID)  OR if it made changes to remote
	          (which can be only uploads). Returns False on an error, ErrorMessage will
	          be set. Writes Report on success, might have report items even on failure. }
 	function MakeConnection() : boolean;
   				{ Returns True with the remote ServerID in SID. False if something
                  went wrong, ErrorMessage should be set. Assumes the file exists. }
	function GetRemoteServerID(out SID: ANSIString): boolean;




private
	function ProceedWith(FileID, Rev: ANSIString): TClashDecision; override;
				{ Takes a filename (or ID) and Rev, puts the file there. Assumes Dir is OK }
	function UploadNote(const ID : ANSIString) : boolean; override;
				{ Takes a filename (or ID) and Rev, brings the file down. Does Backup if necessary }
	function DownLoadNote(const ID, Rev : ANSIString) : boolean; override;
				{ Returns a string being a full path to the file mentioned, empty string OK }
	function RemotePath(const ID, Rev : ANSIString; ItsANote : Boolean = True) : ANSIString;
				{ Returns a string being a full path to the file mentioned, empty string OK }
	function LocalPath(const ID, Backup: ANSIString; ItsANote : Boolean = True) : ANSIString;
	// procedure GetListRemoteNotes(); override;
				{ Initiates a new revision, true if all OK  }
	function MakeNewRevision() : boolean; override;
				{ writes new local manifest and, if necessay, a server and server rev, sets ErrorMessage }
    function WriteNewManifest(WriteDeletes : boolean = false) : boolean; override;
				{ writes out a remote style manifest to indicated file }
	function WriteRemoteManifest(const FullFileName : AnsiString) : boolean;
	        	{ Compares each entry in Remote list against Local List }
    function CompareUsingRemote() : boolean;
        		{ Compares each entry in Local List against the Remote list }
	function CompareUsingLocal(): boolean;
        		{ iterates over the NoteInfoListRem filling in meta data, if it finds
                  a note with invalid XML, writes a warning to console and removes note
                  from the list, and there its ignored after that.   }
	procedure GetRemoteMetaData;
        		{ Reads the remote manifest file, filling NoteInfoList with metaData
                  Returns False if an error occured that would prevent us proceeding.}
	function ReadRemoteManifest(SkipFile : boolean = false) : boolean;
 end;

 { ------------------------ TTomboyLocalManifest --------------------------}

 { This class is to update the local manifest file (if it exists) when a file that
   has previously be sync'ed is deleted.
 }
TTomboyLocalManifest = Class(TTomboySyncCustom)
  		{ Has no function in this class, here to suppress error messages. }
	function DoSync(UseLocal, UseRemote : boolean) : boolean; override;
    	{ Response procedure for property IDToDelete }
    procedure FNoteToDelete(ID : ANSIString);
    	{ The note ID poked into here will be moved into local manifest deleted
          section if it already appears in note-revision section. Does nothing
          if ID not found in there already or if local manifest does not exist.
          The ID is just that, no path and no '.note' is expected.            }
    property IDToDelete : ANSIString write FNoteToDelete;
private
  				// Has no function in this class.
  	function ProceedWith(FileID, Rev: ANSIString): TClashDecision; override;
  				// Has no function in this class.
	function DownLoadNote(const ID, Rev : ANSIString) : boolean; override;
  				// Has no function in this class.
	function MakeNewRevision() : boolean; override;
  				{ rewrites local manifest with a note id marked as deleted }
	function WriteNewManifest(WriteDeletes : boolean = false) : boolean; override;
    			// Has no function in this class.
	function UploadNote(const ID : ANSIString) : boolean; override;
end;


implementation

uses  laz2_DOM, laz2_XMLRead, FileUtil, LazFileUtils, DateUtils, LazLogger
        {$ifdef LINUX}, Unix {$endif};
 	// If you want to use this as a console app, must add LCL to Required Packages
    // in the Project Inspector. I guess there must be a command line alt ??
    // Thats to get LazFileUtils, preferable to FileUtils (also LCL) due to UFT8
    // But not everything is present in LazFileUtils (ie CopyFile() )
    // so list LazFileUtils after FileUtil


{ ===========================  TTomboyLocalManifest ============================== }

procedure TTomboyLocalManifest.FNoteToDelete(ID: ANSIString);
var
    i : integer;
    NoteInfoP : PNoteInfo;
    Found : boolean = false;
begin
	if not ReadLocalManifest(False) then exit(); 	// read the local manifest
    for I := 0 to NoteInfoListLocalManifest.count -1 do  begin
    	if NoteInfoListLocalManifest.Items[i].ID = ID then begin
        	NoteInfoListLocalManifest.Items[i].Deleted:= True;
            NoteInfoListLocalManifest.Items[i].Title :=
            		GetNoteTitle(NotesDir + PathDelim + 'Backup' + PathDelim + ID + '.note');
            Found := True;
            break;
        end;
	end;
    if not Found then exit();
	ManifestList := TNoteInfoList.Create;   		// This for making new manifests
    for I := 0 to NoteInfoListLocalManifest.count -1 do  begin
        new(NoteInfoP);
        NoteInfoP.Deleted:=NoteInfoListLocalManifest.Items[i].Deleted;
        NoteInfoP.ID:=NoteInfoListLocalManifest.Items[i].ID;
        NoteInfoP.Rev := NoteInfoListLocalManifest.Items[i].Rev;
        NoteInfoP.Title := NoteInfoListLocalManifest.Items[i].Title;
        ManifestList.Add(NoteInfoP);
    	//writeln(NoteInfoListLocalManifest.Items[i].ID + ' ' + NoteInfoListLocalManifest.Items[i].Rev);
	end;
	TestMode := False;		// if true, will write a new manifest with -ref appended.
    WriteNewManifest();

end;

function TTomboyLocalManifest.DoSync(UseLocal, UseRemote: boolean): boolean;
begin
    Result := False;
end;

function TTomboyLocalManifest.ProceedWith(FileID, Rev: ANSIString ): TClashDecision;
begin
	Result := cdDoNothing;
end;

function TTomboyLocalManifest.DownLoadNote(const ID, Rev: ANSIString): boolean;
begin
	Result := False;
end;

function TTomboyLocalManifest.MakeNewRevision: boolean;
begin
	Result := False;
end;

function TTomboyLocalManifest.WriteNewManifest(WriteDeletes : boolean = false) : boolean;
begin
		Result:=inherited WriteNewManifest(True);
end;

function TTomboyLocalManifest.UploadNote(const ID: ANSIString): boolean;
begin
	Result := False;
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
var
	Doc : TXMLDocument;
	Node : TDOMNode;
begin
    Report.Title := 'File Not Found';
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
    end;
    result := inherited Add(Report);
end;


{ ----------  TNoteInfoList ------------- }

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
        if Items[Index].ID = ID then begin
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


{ ==================================== TTomboySyncCustom ======================= }



function TTomboySyncCustom.WriteNewManifest(WriteDeletes : boolean = false) : boolean;
var
    OutStream : TFilestream;
    Buff      : ANSIString;
    Index     : longint;
begin
    Result := False;
	try
	    try
	    	if TestMode then
	        	outstream :=TFilestream.Create(LocalManifestDir + 'manifest.xml-ref', fmCreate)
	        else
	            outstream :=TFilestream.Create(LocalManifestDir + 'manifest.xml', fmCreate);
        except on EAccessViolation do begin
    	    			ErrorMessage := 'Failed to open ' + LocalManifestDir + 'manifest.xml';
    	    			exit();
    	            end;
    	        on EFCreateError do begin
    	    			ErrorMessage := 'Failed to open ' + LocalManifestDir + 'manifest.xml';
    	    			exit();
    	        end;
    	end;
		Buff := '<?xml version="1.0" encoding="utf-8"?>' + LineEnding;
	    OutStream.Write(Buff[1], length(Buff));
	    Buff := '<manifest xmlns="http://beatniksoftware.com/tomboy">' + LineEnding;
	    OutStream.Write(Buff[1], length(Buff));
        if WriteDeletes then
            Buff := '  <last-sync-date>' + LocalLastSyncDateSt
        else
            Buff := '  <last-sync-date>' + GetLocalTime();
	    OutStream.Write(Buff[1], length(Buff));
        Buff := '</last-sync-date>' + LineEnding + '  <last-sync-rev>';
        OutStream.Write(Buff[1], length(Buff));
        if WriteDeletes then
            Buff := LocalRevSt + '</last-sync-rev>' + LineEnding + '  <server-id>'
        else
        	Buff := newRevision + '</last-sync-rev>' + LineEnding + '  <server-id>';
        OutStream.Write(Buff[1], length(Buff));
        Buff := ServerID + '</server-id>' + LineEnding + '  <note-revisions>' + LineEnding ;
        OutStream.Write(Buff[1], length(Buff));
        for Index := 0 to ManifestList.Count -1 do begin
            if not ManifestList.Items[Index].Deleted then begin;
        		Buff := '    <note guid="' + ManifestList.Items[Index].ID + '" latest-revision="'
        			+ ManifestList.Items[Index].Rev + '" />' + LineEnding;
            	OutStream.Write(Buff[1], length(Buff));
            end;
		end;
        Buff := '  </note-revisions>' + LineEnding + '  <note-deletions>' + LineEnding;
        OutStream.Write(Buff[1], length(Buff));
        // <note guid="8937a940-a97f-4744-bafa-076e5f16af61" title="32775" />
        if WriteDeletes then begin
            for Index := 0 to ManifestList.Count -1 do begin
                if ManifestList.Items[Index].Deleted then begin;
            		Buff := '    <note guid="' + ManifestList.Items[Index].ID + '" title="'
            			+ ManifestList.Items[Index].Title + '" />' + LineEnding;
                	OutStream.Write(Buff[1], length(Buff));
                end;
			end;
		end;
		Buff := '  </note-deletions>' + LineEnding + '</manifest>' + LineEnding;
        OutStream.Write(Buff[1], length(Buff));
        Result := True;
    finally
    	OutStream.Free;
	end;
    if VerboseMode then DeBugLn('Debug - written local manifest ');
end;

procedure TTomboySyncCustom.AddReport(const Action, ID, Path, Message: ANSIString);
var
    Report : PSyncReport;
begin
	new(Report);
    Report.ID := ID;
    Report.Action := Action;
    Report.Message := Message;
    Report.FullFileName := Path;
    ReportList.Add(report);
end;

function TTomboySyncCustom.SetlastSyncdate: boolean;
var
	Doc : TXMLDocument;
	// NodeList : TDOMNodeList;
	Node : TDOMNode;
begin
    Result := False;
    try
		ReadXMLFile(Doc, LocalManifestDir + 'manifest.xml');
		Node := Doc.DocumentElement.FindNode('last-sync-date');
        LastSyncDate := GetDateFromStr(Node.FirstChild.NodeValue);
    	Result := True;
    finally
        Doc.free;
	end;
end;

procedure TTomboySyncCustom.ClearLists();
begin
    NoteInfoListRem.Free;
	NoteInfoListRem := nil;
    NoteInfoListLoc.Free;
	NoteInfoListLoc := nil;
    ManifestList.Free;
    ManifestList := nil;
    NoteInfoListLocalManifest.free;
    NoteInfoListLocalManifest := nil;
    ReportList.free;
    ReportList := nil;
    // if DebugMode then DeBugLn('Debug - Disposed of all lists.');
end;

procedure TTomboySyncCustom.TestProceed;
var
    Rec : TClashRecord;
begin
    Rec.NoteID:='ABCDEFG';
    Rec.LocalLastChange:='2017-11-03';
    Rec.ServerLastChange:='2017-11-02';
    ProceedFunction(Rec);
end;

destructor TTomboySyncCustom.Destroy;
begin
    ClearLists();
    inherited;
end;

function TTomboySyncCustom.GetLocalServerID: boolean;
var
	Doc : TXMLDocument;
	// NodeList : TDOMNodeList;
	Node : TDOMNode;
begin
    Result := False;
    try
        try
			    ReadXMLFile(Doc, LocalManifestDir + 'manifest.xml');
			    Node := Doc.DocumentElement.FindNode('server-id');
	            ServerID := Node.FirstChild.NodeValue;
	            Result := True;
        except on EFOpenError do begin
            		ErrorMessage := 'Cannot open local manifest';
                	if VerboseMode then DebugLn('Debug - Cannot open local manifest');
                	exit();
            	end;
				on EXMLReadError do begin
                	ErrorMessage := 'XML error in local manifest ';
                	if VerboseMode then DebugLn('Debug - Cannot open local manifest');
                	exit();
				end;
        end;
	finally
        Doc.free;
	end;
end;


procedure TTomboySyncCustom.UpdateNextManifest(const ID, Rev: ANSIString);
var
    NoteInfoP : PNoteInfo;
begin
    new(NoteInfoP);
    NoteInfoP.ID := ID;
    NoteInfoP.Rev:= Rev;
    NoteInfoP.Deleted := False;
    ManifestList.Add(NoteInfoP);
end;

function TTomboySyncCustom.ReadLocalManifest(SkipFile : boolean = False) : boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    j : integer;
    NoteInfoP : PNoteInfo;
begin
    Result := true;
    NoteInfoListLocalManifest := TNoteInfoList.Create;
    if SkipFile then exit();
    try
    	try
    		ReadXMLFile(Doc, LocalManifestDir + 'manifest.xml');
            Node := Doc.DocumentElement.FindNode('last-sync-date');
        	LocalLastSyncDateSt := Node.FirstChild.NodeValue;
            Node := Doc.DocumentElement.FindNode('last-sync-rev');
        	LocalRevSt := Node.FirstChild.NodeValue;
            NodeList := Doc.DocumentElement.FindNode('note-revisions').ChildNodes;
            if assigned(NodeList) then
               for j := 0 to NodeList.Count-1 do begin
                   new(NoteInfoP);
                   NoteInfoP.ID := NodeList.Item[j].Attributes.GetNamedItem('guid').NodeValue;
                   NoteInfoP.Rev := NodeList.Item[j].Attributes.GetNamedItem('latest-revision').NodeValue;


                   // Danger Will Robinson, danger, I just added that line, will need widespread test


                   NoteInfoP.Deleted := False;
                   NoteInfoListLocalManifest.Add(NoteInfoP);
			   end;
            NodeList := Doc.DocumentElement.FindNode('note-deletions').ChildNodes;
            if assigned(NodeList) then
               for j := 0 to NodeList.Count-1 do begin
                   new(NoteInfoP);
                   NoteInfoP.ID := NodeList.Item[j].Attributes.GetNamedItem('guid').NodeValue;
                   NoteInfoP.Title := NodeList.Item[j].Attributes.GetNamedItem('title').NodeValue;
                   NoteInfoP.Deleted := True;
                   NoteInfoListLocalManifest.Add(NoteInfoP);
   			   end;
		finally
            Doc.Free;
		end;
	except
      on EAccessViolation do Result := false; // probably means we did not find an expected attribute
	end;
end;


procedure TTomboySyncCustom.GetListLocalNotes();
var
    Info : TSearchRec;
    NoteInfo : PNoteInfo;
    LastChange : ANSiString;
begin
    NoteInfoListLoc := TNoteInfoList.Create;
    try
	    if FindFirstUTF8(NotesDir + '*.note', faAnyFile and faDirectory, Info)=0 then begin
	    	repeat
	            new(NoteInfo);
	        	NoteInfo.ID := copy(Info.Name, 1, 36);		// UTF8 issue ?  I don't think GUID can contain UTF8.
	            try
	            	NoteInfo.GMTDate := GetNoteChangeGMT(NotesDir + Info.Name, LastChange);
	            except on E: EXMLReadError do begin
	                	DebugLn('This local note contains invalid XML, will NOT sync it. ', NoteInfo.ID);
	                    DebugLn(E.Message);
                        AddReport('Error', NoteInfo.ID, NotesDir+'*.note', 'XML Error in local note, skipping.');
	                    dispose(NoteInfo);
	                    continue;
	                end;
				end;
	            NoteInfo.LastChange := LastChange;
	            NoteInfoListLoc.Add(NoteInfo);
	    	until FindNextUTF8(Info) <> 0;

	    end else DebugLn('found no local notes ' + NotesDir + '*.note');
	finally
       FindClose(Info);             // don't forget this, potential memory leak.
	end;
end;


function TTomboySyncCustom.GetLocalTime: ANSIstring;
	{ Tomboy uses this as a time stamp - 2017-10-24T17:38:35.7759840+11:00  }
var
   ThisMoment : TDateTime;
   Res : ANSIString;
   Off : longint;
begin
    // this function is duplicated in Savenote.pas, in case this unit used independantly.
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

procedure TTomboySyncCustom.SetNotesDir(Dir: ANSIString);
begin
	FNotesDir := AppendPathDelim(Dir);
end;

procedure TTomboySyncCustom.SetRemoteManifestDir(Dir: ANSIString);
begin
	FRemoteManifestDir := AppendPathDelim(Dir);
end;

procedure TTomboySyncCustom.SetLocalManifestDir(Dir: ANSIString);
begin
    FLocalManifestDir := AppendPathDelim(Dir);
end;

function TTomboySyncCustom.GetNoteTitle(FullFileName : ANSIString) : ANSIString;
var
	Doc : TXMLDocument;
	Node : TDOMNode;
begin
	Result := 'File Not Found';
	if FileExistsUTF8(FullFileName) then begin
		    try
	            Result := 'Unknown Title';
	            try
				    ReadXMLFile(Doc, FullFileName);
				    Node := Doc.DocumentElement.FindNode('title');
	    		    Result := Node.FirstChild.NodeValue;
	            except 	on EXMLReadError do
	        				    Result := 'Note has no Title ' + FullFileName;
	          		    on EAccessViolation do
	                            Result := 'Access Violation ' + FullFileName;
			    end;
		    finally
	    	    Doc.free;
		    end;
    end;
end;


{ ------------------- T TomboyFileSync ------------------------- }


function TTomboyFileSync.ProceedWith(FileID, Rev : ANSIString) : TClashDecision;
var
    ClashRec : TClashRecord;
    ChangeDate : ANSIString;
begin
    ClashRec.NoteID := FileID;
    ClashRec.Title:= GetNoteTitle(LocalPath(FileID, ''));
    GetNoteChangeGMT(LocalPath(FileID, ''), ChangeDate);
    ClashRec.LocalLastChange := ChangeDate;
    GetNoteChangeGMT(RemotePath(FileID, Rev), ChangeDate);
    ClashRec.ServerLastChange := ChangeDate;
    Result := ProceedFunction(Clashrec);
end;


function TTomboyFileSync.CompareUsingLocal() : boolean;
var
    Index : Longint;
    NoteInfoP, NoteInfoP2 : PNoteInfo;
    // NoteInfoP may contain details from the remote server about the local note we are looking at.
    // The local note is always "NoteInfoListLoc.Items[Index]"
begin
    result := false;
    for Index := 0 to NoteInfoListLoc.Count -1 do begin
    	NoteInfoP := NoteInfoListRem.FindID(NoteInfoListLoc.Items[Index].ID);
    	if NoteInfoP = NIL then begin
            // OK, its here locally but not on Server, is it in local manifest ?
            NoteInfoP2 := NoteInfoListLocalManifest.FindID(NoteInfoListLoc.Items[Index].ID);
            { if DebugMode then begin
                write('matching ',NoteInfoListLoc.Items[Index].ID);
                if NoteInfoP2 = nil then writeln(' - Not matched.')
                else writeln(' - matched.')
            end;   }
            if NoteInfoP2 = nil then
                // Ah, its a new local note.
                UpLoadNote(NoteInfoListLoc.Items[Index].ID)
            else begin
                // its in local manifest, either to be deleted (not likely!) or as
                // a previous sync deleted in another client, in either case, delete it.
                NewManifest := True;
                AddReport('Backup', NoteInfoListLoc.Items[Index].ID, LocalPath(NoteInfoListLoc.Items[Index].ID, ''), '');
                AddReport('Delete', NoteInfoListLoc.Items[Index].ID, LocalPath(NoteInfoListLoc.Items[Index].ID, ''), '');
                if testMode then begin
                    if VerboseMode then begin
                    	DebugLn('TEST backup ', LocalPath(NoteInfoListLoc.Items[Index].ID, ''),
                   			LocalPath(NoteInfoListLoc.Items[Index].ID, 'Backup'));
                   		DebugLn('TEST delete ',LocalPath(NoteInfoListLoc.Items[Index].ID, ''));
					end;
				end else begin
                   CopyFile( LocalPath(NoteInfoListLoc.Items[Index].ID, ''),
                    	LocalPath(NoteInfoListLoc.Items[Index].ID, 'Backup'),
                        	[cffOverwriteFile]);
                   DeleteFileUTF8(LocalPath(NoteInfoListLoc.Items[Index].ID, ''));
                   if VerboseMode then DebugLn('Debug - Delete ' + LocalPath(NoteInfoListLoc.Items[Index].ID, ''));
                end;
 			end;
			continue;
        end;

        // OK, if to here, exists in both manifests. Do change dates match ?
		if NoteInfoP.LastChange = NoteInfoListLoc.Items[Index].LastChange then begin
            // Ignore - dates match, mention in new manifest - continue
    		// UpDateRemoteManifest(NoteInfoP.ID, NoteInfoP.Rev);
    		UPDateNextManifest(NoteInfoP.ID, NoteInfoP.Rev );
			continue;
		end;

        // they both exist but don't match, we need do something !
		if NoteInfoP.GMTDate > NoteInfoListLoc.Items[Index].GMTDate then begin
            // That says get remote but what if loc has also changed since last sync ?
            if NoteInfoListLoc.Items[Index].GMTDate > LastSyncDate then begin       // <<== test this code
                // Yes - ask user to indicate continue download, upload or nothing ?
              	case ProceedWith(NoteInfoP.ID, NoteInfoP.Rev) of
            		cdUpload : UpLoadNote(NoteInfoP.ID);
                	cdDownLoad : DownloadNote(NoteInfoP.ID, NoteInfoP.Rev);
                	cdDoNothing : continue;
				end;
         	end else
            	// No - no clashes, just download as planned.
                DownloadNote(NoteInfoP.ID, NoteInfoP.Rev);
            continue;
        end;

        // Ah, after all that, local note is newer, thats easy.
        UpLoadNote(NoteInfoP.ID);

	end;				// end of For loop iterating through the LocalList.
    result := true;
end;


{ for each note in Listrem - is it present ListLocal ?
       		YES - ok, been deal with already - continue
           	NO - is it listed in local manifest as deleted ?
           		YES - remove from remote /rev/.note, don't record in new manifest - continue
               	NO - its a new note from other client, download, list in new manifest - continue  }

function TTomboyFileSync.CompareUsingRemote: boolean;
var
    Index : Longint;
    NoteInfoP, NoteInfoP2 : PNoteInfo;
begin
    result := false;
    for Index := 0 to NoteInfoListRem.Count -1 do begin
    	NoteInfoP := NoteInfoListLoc.FindID(NoteInfoListRem.Items[Index].ID);
        // We only need to deal with ones not found here.
        if NoteInfoP = nil then begin
            // is it listed in local manifest as deleted ?
            NoteInfoP2 := NoteInfoListLocalManifest.FindID(NoteInfoListRem.Items[Index].ID);
            if NoteInfoP2 = Nil then begin
                // Its a note uploaded by another client, we'd better get it.
                DownloadNote(NoteInfoListRem.Items[Index].ID, NoteInfoListRem.Items[Index].Rev);
			end else begin
                if NoteInfoP2.Deleted then begin		// it must be 'deleted', but OK to test - WRONG !
                    { If doing a manual sync, a note that has been removed (not using the app)
                    from the Notes Dir will not get picked up. It does not appear in local manifest
                    as Deleted. Its not present in Notes dir. But is mentioned in Local Manifest as
                    having been seen before. So, this code does nothing about it.
                    But if you reinitialise the sync connection, the local sync manifest is not
                    consulted and it will be brought down. Worth remembering ....
                    }
                    // remove from remote /rev/.note, don't record in new manifest
                    // but do remember to write a new manifest after this session - continue
                    MakeNewRevision();
                    NewManifest := True;
                    if FileExistsUTF8(RemotePath(NoteInfoListRem.Items[Index].ID, NoteInfoListRem.Items[Index].Rev) ) then begin
                        AddReport('Delete from Server', NoteInfoListRem.Items[Index].ID, RemotePath(NoteInfoListRem.Items[Index].ID,NoteInfoListRem.Items[Index].Rev), '');
	                    if TestMode then begin
	                       if VerboseMode then DebugLn('TEST delete ' + RemotePath(NoteInfoListRem.Items[Index].ID,NoteInfoListRem.Items[Index].Rev));
	                    end else
	                        DeleteFileUTF8(RemotePath(NoteInfoListRem.Items[Index].ID,NoteInfoListRem.Items[Index].Rev));
						// end;
					end;
				end else DebugLn('A note we have seen before, not present locally now but not officially deleted. Will leave it alone. ' + NoteInfoListRem.Items[Index].ID);
			end;
		end;
 	end;
    Result := True;
end;


function TTomboyFileSync.UploadNote(const ID : ANSIString): boolean;
begin
    result := false;
    MakeNewRevision();
    AddReport('Upload', ID, LocalPath(ID, ''), '');
    if TestMode then begin
       if VerboseMode then DebugLn('TESTUP copy', LocalPath(ID, ''), RemotePath(ID, NewRevision));
	end else
        if not CopyFile(LocalPath(ID, ''), RemotePath(ID, NewRevision), [cffOverwriteFile]) then begin
            ErrorMessage := 'Failed to copy ' + LocalPath(ID, '') + ' to ' + RemotePath(ID, NewRevision);
            exit();
        end;
	UpdateNextManifest(ID, NewRevision);
    NewManifest := true;
    Result := True;
end;

function TTomboyFileSync.DownLoadNote(const ID, Rev: ANSIString): boolean;
begin
    Result := False;
    AddReport('Download', ID, RemotePath(ID, Rev), '');
    if TestMode then begin
       if FileExistsUTF8(LocalPath(ID, '')) then
       		if VerboseMode then DebugLn('TESTBACKUP copy', LocalPath(ID, ''), LocalPath(ID, 'Backup'));
    end else
        if FileExistsUTF8(LocalPath(ID, '')) then begin
            if VerboseMode then DebugLn('Debug - Copy ' + LocalPath(ID, '') + ' to ' + LocalPath(ID, 'Backup'));
			if not CopyFile( LocalPath(ID, ''), LocalPath(ID, 'Backup'), [cffOverwriteFile]) then begin
	            ErrorMessage := 'Failed to copy ' + LocalPath(ID, '') + ' to ' + LocalPath(ID, 'Backup');
	            // writeln('Failed to copy ' + LocalPath(ID, '') + ' to ' + LocalPath(ID, 'Backup'));             // DEBUG
                AddReport('Error', ID, LocalPath(ID, ''), 'Cannot copy ' + LocalPath(ID, '') + ' to ' + LocalPath(ID, 'Backup'));
	            exit();
			end;
		end;
	if TestMode then begin
       if VerboseMode then DebugLn('TESTDOWN copy', RemotePath(ID, Rev),LocalPath(ID, ''));
    end else begin
        // No test here, it SHOULD be there. Hmm.....
        // if DebugMode then writeln('DOWNLOAD ' + RemotePath(ID, Rev) + ' to ' + LocalPath(ID, ''));
		if not CopyFile(RemotePath(ID, Rev), LocalPath(ID, ''), [cffOverwriteFile]) then begin
             ErrorMessage := 'Failed to copy ' + RemotePath(ID, Rev) + ' to ' + LocalPath(ID, '');
             Addreport('Error', ID, RemotePath(ID, Rev), 'Cannot copy ' + RemotePath(ID, Rev) + ' to ' + LocalPath(ID, ''));
             // writeln('Failed to copy ' + RemotePath(ID, Rev) + ' to ' + LocalPath(ID, ''));					// DEBUG
            exit();
		end;
	end;
	UpdateNextManifest(ID, Rev);
    NewManifest := true;
    Result := True;
end;

function TTomboyFileSync.RemotePath(const ID, Rev: ANSIString; ItsANote : Boolean = True): ANSIString;
begin
    if Rev = '' then
        Result := RemoteManifestDir + ID
    else
		Result := RemoteManifestDir + '0' + PathDelim + Rev + PathDelim + ID;
    if ItsANote then Result := Result + '.note';
end;


function TTomboyFileSync.LocalPath(const ID, Backup : ANSIString; ItsANote : Boolean = True): ANSIString;
begin
    if Backup = '' then
        Result := NotesDir + ID
    else
		Result := NotesDir + Backup + PathDelim + ID;
    if ItsANote then Result := Result + '.note';
end;

function TTomboyFileSync.DoSync(UseLocal, UseRemote: boolean): boolean;
begin
    Result := False;
    { TODO : Lock the repo, file lock ? }
    // Any function here that returns false aborts the sync, it should set ErrorMessage

    	// GetLocalServerID();                      // we call these in parent app.
        // CheckRemoteServerID();
        // if DebugMode then DebugLn('Debug DoSync useLocal ' + UseLocal + ' useRemote ', UseRemote, ' and TestMode ', TestMode);
        if ReportList <> Nil then begin
            // if DebugMode then DebugLn('Debug - Note reuse of list without freeing it.');
            ClearLists();
		end;
		ReportList := TSyncReportList.Create;
    	if UseLocal then SetlastSyncdate();
		if not ReadRemoteManifest(not UseRemote) then exit();	// read the remote manifest
        if not ReadLocalManifest(not UseLocal) then exit(); 	// read the local manifest
    	GetListLocalNotes();						// read the actual Notes directory
        ManifestList := TNoteInfoList.Create;   	// This for making new manifests
    	if not CompareUsingLocal() then exit;
        if not CompareUsingRemote() then exit;
        // We MUST write a new local mainfest if joining even if we have no notes
        if (NewRevision <> '') or (not UseLocal) or NewManifest then
           	if not WriteNewManifest() then exit();
		Result := True;
end;



function TTomboyFileSync.GetCurrentRevision: longint;
var
	Doc : TXMLDocument;
	// NodeList : TDOMNodeList;
	// Node : TDOMNode;
begin
    try
        try
				ReadXMLFile(Doc, RemoteManifestDir + 'manifest.xml');
        		Result := strtoint(Doc.DocumentElement.GetAttribute('revision'));
        except on EFOpenError do begin
            		if VerboseMode then DebugLn('Debug - Cannot get Remote Revision, will start at 0');
                    Result := -1;
        		end;
		end;
	finally
        Doc.free;
	end;
end;

function TTomboyFileSync.GetRemoteServerID(out SID : ANSIString) : boolean;
var
	Doc : TXMLDocument;
begin
    Result := False;
    // This is not always a real error, a new connection eg.
	if not FileExistsUTF8(RemoteManifestDir + 'manifest.xml') then begin
    	ErrorMessage := ' Can not find remote manifest file ' + RemoteManifestDir + 'manifest.xml';
    	exit();
	end;
    try
		try
			ReadXMLFile(Doc, RemoteManifestDir + 'manifest.xml');
            SID := Doc.DocumentElement.GetAttribute('server-id');
            Result := True;
        except on EFOpenError do begin
            	ErrorMessage := ' Error opening ' + RemoteManifestDir + 'manifest.xml';
                exit();
        	end;
          		on EXMLReadError do begin
                	ErrorMessage := ' XML error in ' + RemoteManifestDir + 'manifest.xml';
                    exit();
				end;
		end;
	finally
        Doc.Free;
	end;
end;

function TTomboyFileSync.CheckRemoteServerID: boolean;
	{ This function will be called early on so lets look for any error.
    If the remote manifest is not present, if, Repo not initialised, then
    we get an EFOpenError on ReadXMLFile(). Thats going to be caught by the try
    and function will return false.
    If the tag server-id is not present, or invalid, it won't match the local
    one and, again, function will return false.
    ErrorMessage will be set.
    }
var
    RemoteServerID : ANSIString;
begin
    Result := False;
    if not FileExistsUTF8(RemoteManifestDir + 'manifest.xml') then begin
    	ErrorMessage := ' Can not find remote manifest file ' + RemoteManifestDir + 'manifest.xml';
        exit();
	end;
    if not GetRemoteServerID(RemoteServerID) then
       exit();
    if RemoteServerID = ServerID then Result := True
    else ErrorMessage := ' Error matching local and remote ServerIDs';
end;

function TTomboyFileSync.MakeConnection: boolean;
var
    GUID : TGUID;
begin
	// If we can get a remote ServerID we'll join in, else we'll create a new Repo and sync to it.
    if GetRemoteServerID(ServerID) then begin
        if VerboseMode then DebugLn('Debug - Joining an existing Repo, ', ServerID);
        Result := DoSync(False, True)
	end
	else begin
    	CreateGUID(GUID);
    	ServerID := copy(GUIDToString(GUID), 2, 36);
        if VerboseMode then DebugLn('Debug - Creating a new FileSync Repositary ', ServerID);
        DoSync(False, False);
	end;
end;


function TTomboyFileSync.MakeNewRevision: boolean;
var
    NewDir : ANSIString;
begin
    Result := True;
    if (NewRevision <> '') or TestMode then exit();
    NewDir := RemoteManifestDir + '0';
    if not DirectoryExistsUTF8(NewDir) then
    	CreateDirUTF8(NewDir);
   	NewRevision := inttostr(GetCurrentRevision() + 1);
    NewDir := AppendPathDelim(NewDir) + NewRevision;
    // if DebugMode then writeln('Making a New Revision at ', NewDir);
    if CreateDirUTF8(NewDir) then begin
        if VerboseMode then DebugLn('Debug - Made new rev dir at ', NewDir);
    end else begin
        ErrorMessage := 'ERROR - Unable to create new revision dir ' + NewDir;
        if VerboseMode then DebugLn('Debug - ERROR Cannot make rev dir at ', NewDir);
        Result := false;
        exit();
	end;
	CopyFile(RemoteManifestDir + 'manifest.xml', AppendPathDelim(NewDir) + 'manifest.xml');
    Result := true;
end;

function TTomboyFileSync.WriteRemoteManifest(const FullFileName: AnsiString
		): boolean;
var
        OutStream : TFilestream;
        Buff      : ANSIString;
        Index     : longint;
begin
        Result := False;
        if VerboseMode then DebugLn('Debug - Making remote manifest at ', FullFileName);
    	try
            outstream :=TFilestream.Create(FullFileName, fmCreate);
            Buff := '<?xml version="1.0" encoding="utf-8"?>' + LineEnding;
            OutStream.Write(Buff[1], length(Buff));
            Buff := '<sync revision="' + newRevision + '" server-id="';
            OutStream.Write(Buff[1], length(Buff));
            Buff := ServerID + '">' + LineEnding;
            OutStream.Write(Buff[1], length(Buff));
            for Index := 0 to ManifestList.Count -1 do begin
            	Buff := '<note id="' + ManifestList.Items[Index].ID + '" rev="'
            		+ ManifestList.Items[Index].Rev + '" />' + LineEnding;
                OutStream.Write(Buff[1], length(Buff));
    		end;
            Buff := '</sync>' + LineEnding;
            OutStream.Write(Buff[1], length(Buff));
            Result := True;
        finally
        	OutStream.Free;
    	end;
end;

function TTomboyFileSync.WriteNewManifest(WriteDeletes : boolean = false) : boolean;
var
    Filename : ANSIString;
begin
    	Result := True;
    	if TestMode then exit();
		Result:=inherited WriteNewManifest();			// always
        if Result = False then exit();
        if NewRevision = '' then exit();	// Only need remote manifests if we create a new revision
        Result := False;
        FileName := RemoteManifestDir +'manifest.xml';
        // if DebugMode then writeln('In WriteNewManifest with ', FileName, ' TestMode=', TestMode);
        if not WriteRemoteManifest( FileName) then begin
            ErrorMessage := 'ERROR writing ' + FileName;
        	exit();
		end;
        Result := True;
end;


function TTomboySyncCustom.GetNoteChangeGMT(const FullFileName : ANSIString; out LastChange : ANSIString) : TDateTime;
var
	Doc : TXMLDocument;
	// NodeList : TDOMNodeList;
	Node : TDOMNode;
begin
    if not FileExistsUTF8(FullFileName) then begin
        DebugLn('ERROR - File not found, cant read note change date for ',  FullFileName);
        Result := 0.0;
        exit();
	end;
	try
			ReadXMLFile(Doc, FullFileName);
			Node := Doc.DocumentElement.FindNode('last-change-date');
        	LastChange := Node.FirstChild.NodeValue;
    		Result := GetDateFromStr(Node.FirstChild.NodeValue);
	finally
        Doc.free;		// xml errors are caught in calling process
	end;
end;

function TTomboySyncCustom.GetDateFromStr(const DateStr: ANSIString): TDateTime;
var
    TimeZone : TDateTime;
begin
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
	    if DateStr[28] = '+' then Result := Result - TimeZone
		else if DateStr[28] = '-' then Result := Result + TimeZone
	    else debugLn('******* Bugger, we are not parsing DATE String ********');
    except on EConvertError do begin
        	DebugLn('FAIL on calculating GMT ' + DateStr);

    	end;
    end;
    { writeln('Date is ', DatetoStr(Result), ' ', TimetoStr(Result));  }
end;


procedure TTomboyFileSync.GetRemoteMetaData();
var
    Index : longint = 0;
    // NoteInfo : TNoteInfo;	// carefull, that not a pointer this time.
    Filename : ANSIString;
    LastChange : ANSIString;
begin
    while Index <  NoteInfoListRem.Count do begin
	// for Index := 0 to NoteInfoListRem.Count -1 do begin
        try
        	Filename := RemotePath(NoteInfoListrem.Items[Index].ID, NoteInfoListRem.Items[Index].Rev);
        	NoteInfoListRem.Items[Index].GMTDate := GetNoteChangeGMT(Filename, LastChange);
        	NoteInfoListRem.Items[Index].LastChange := LastChange;
        except on E: EXMLReadError do begin
            	DebugLn('This server note contains invalid XML, will NOT sync it. ', NoteInfoListrem.Items[Index].ID);
                DebugLn(E.Message);
                AddReport('Error', NoteInfoListrem.Items[Index].ID, FileName, 'Invalid XML in remote note, skipped');
                Dispose(NoteInfoListrem.Items[Index]);
                NoteInfoListrem.Delete(Index);
                continue;
            end;
		end;
        inc(Index);
	end;
end;

function TTomboyFileSync.ReadRemoteManifest(SkipFile : boolean = false) : boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    j : integer;
    NoteInfo : PNoteInfo;
begin
    Result := true;
    NoteInfoListRem := TNoteInfoList.Create;
    if SkipFile then exit();
    try
    	try
    		ReadXMLFile(Doc, RemoteManifestDir + 'manifest.xml');
    		NodeList := Doc.DocumentElement.ChildNodes;
    		if assigned(NodeList) then begin
        		for j := 0 to NodeList.Count-1 do begin
                    new(NoteInfo);
                    Node := NodeList.Item[j].Attributes.GetNamedItem('id');
                    NoteInfo.ID := Node.NodeValue;									// OK, from now on ID does not contain '.note';
					Node := NodeList.Item[j].Attributes.GetNamedItem('rev');
                    NoteInfo.Rev := Node.NodeValue;
                    NoteInfoListRem.Add(NoteInfo);
 				end;
		end;
		finally
            Doc.Free;
		end;
	except
      on EAccessViolation do Result := false;	// probably means we did not find an expected attribute
      on EFOpenError do Result := False;		// File is not present.
	end;
    if Result = True then
    	GetRemoteMetaData()
    else
        DebugLn('We failed to read the remote manifest file ', RemoteManifestDir + 'manifest.xml');
end;

end.




