unit sync;

{$mode objfpc}{$H+}

{ Operation of this unit -

Fistly, this unit depends on on the Trans unit, a virtual unit of which the TransFile
has been implemented and TransNet partially. Further Transport layers should be easily
made.

Two seperate approches are needed. In both cases we build a list of all the notes we
know about and what we plan to do for each. The list is built differently for each
case and, then, the same processes are applied to that list. Creating a new Repo is,
effectivly, a variation of the second.

---- Terms ----
LCD - Last Change Date
LSD - Last Sync Date
LocalDelete - a note that has been previously synced but is no longer mentioned in
              Remote Manifest, its been deleted elsewhere.
RemoteDelete - a note that has been previously synced but has been eleted locally,
              remove it from Repo.
NewUpLoad - a note the Repo has not seen before
EditUpLoad - a note, previusly synced and has been edited locally since last sync.


Some globals we'll need to know  -
- Repo, config and notes directory.
- Remote ServerID
- RemoteRevNo (except if its a new repo)
- Local Last Sync Date, as a string.
- Local Last Revision Number

TestConnection() will establish indicated Repo dir exists and we have write permission
there, does it look like an Existing Repo ? If an existing repo, get ServerID and
RemoteRevNo. If we are making a new connection, make a GUID and set those vars appropriatly.

-------- When using an existing Sync ----------
Only after we have established that the local and remote manifests agree on ServerID.
Read remote manifest into list. Don't bother to get LCD from remote notes but do
record them if its aready in Remote Manifest.
For each entry in that list :
        Check if note with same GUID exits in Notes dir, if so -
                LocalChange := Local LCD > Local LSD
                RemChange := Rem Note RevNo > Local RevNo
                if LocalChange AND RemChange its a Clash
                if LocalChange only its an EditUpLoad (update LCD).
                if RemChange only its a download
                if Neither, its a DoNothing.
        If no GUID match, its either a down load or, if its mentioned in Local as
        a delete, its a DeleteRemote.
Then scan through local manifest, a note listed (in main section) that does not
appear in the List is a LocalDelete

Then scan over Notes directory looking for any note not already mentioned in List, it becomes an NewUpLoad (add LCD)

--------- When Joining a Sync Repo -----------
First time sync setup is easy but if has been synced elsewhere and there are Deletes
listed in Local Manifest, warn user (ideally sync with the old repo first).
Local Manifest will be ignored and overwritten if new sync is successful.

When joining, we initially grab just data in remote manifest but then when we look at
local notes, if we find just one potential clash (ie same ID) we go back and redo
the remote data, this time getting LCD for every note. Substantially slower.....
If there are only a few potential clashes, we'd be better to download each relevent
file. But maybe the cost of finding out balances that out ?

if not NewRepo
        Read Local Manifest into List  do need LCD for each note.  (Or, perhaps
        only get LCDs if we find clashes ? Will save time if really is new to client).

For each entry in that list :                    // for new repo, list is empty so effort is trivial
        Check if note with same GUID exits in Notes dir, if so -
                Compare local and remote note's LCD (as a string), if identical
                its a DoNothing else a Clash.
        If no GUID match, its a down load.

Then scan over Notes directory looking for any note not already mentioned in List,
it becomes an NewUpLoad (add LCD).

--------- Processing List -----------------
This is the same process for each of above cases.
Do any required Downloads (backing up first)
Do any required LocalDeletes (backing up first)
(note - we don't need write access to repo until to here.)
if we have any uploads or RemoteDeletes to do,
          write a local copy of Remote Manifest, include LCD if available.
         Make a Remote directory '0' + PathDelim + inttostr(RemoteRevNo+1)
         Copy existing RemoteManifest into above dir (if and only if it exists !)
         Copy the local copy of Remote Manifest (we made a few steps back) into Repo dir.
         Copy any uploads we have to the dir made above.
         To do RemoteDeletes, all we do is not mention them in next Remote Manifest.
Write out a new local manifest, the list has everything we need.

}

interface
uses
    Classes, SysUtils, SyncUtils;


type                       { ----------------- T S Y N C --------------------- }

  { TSync }

  TSync = class

  private
            { Indicates an action to take on a Clash, the user can select this
            action when the first clash happens }
        ProceedAction : TSyncAction;

	         // Where we find Tomboy style notes
	    FNotesDir : string;
	         // Where we find config and local manifest files
	    FConfigDir : string;

            { Scans Notes dir looking for each note in NoteMetaData. Any it finds
              are either clashes or SyNothing, anything left are downloads.
              Used when JOINING an existing repo.}
      function CheckUsingLCD(AssumeNoClash : boolean) : boolean;

            { Looks at a clash and determines if its an up or down depending on
              possible higher order TSyncActions such as newer, older }
      function ResolveAllClash(const Act : TSyncAction; const ID, FullRemoteFileName : ANSIString): TSyncAction;

      { Goes over NoteMetaData (which has only remote notes at this stage) assigning
        an Action to each. Only called when using an established sync connection. }
      function UseRemoteData(): boolean;

        {Notes we have deleted (and existed) here since last sync are marked DeleteRemote
         because we must delete them from the server. For file sync, that means not
         mentioning them in remote manifest any more. Note that while it might seem
         unnecessary, we must inc the revision number.}
      procedure CheckLocalDeletes();

        { Scans over the notes directory for any note not mentioned in NoteMetaData
          and adds it as an syUploadNew as its a new note since last sync. Call
          after all the other methods that help build the NoteMetaDate. }
	  procedure CheckNewNotes();

        {if we have a note prev synced (ie, in local manifest) but not now in RemoteMetaData,
         it was deleted by another client, Mark these as DeleteLocal, will later backup and delete.}
      procedure CheckRemoteDeletes();

       // Just a debug procedure, dumps (some) contents of a list to console
      procedure DisplayNoteInfo(meta: TNoteInfoList);
            // Based on NoteMetaData, calls transport to delete notes from Server.
	  function DoDeletes(): boolean;

      // We call transport for all the notes in the list we need download. Transport does most of the work.
	  function DoDownloads(): boolean;

            { Uploads any files it finds necessary in NoteMetaData. Returns false
              if anything goes wrong (such as a file error) }
	  function DoUploads(): boolean;

            {   Asks Transport for a list of the notes remote server knows about, then sets
                thier Action. At this stage, any previously synced but recently edited notes
                are detected and marked as uploads or clashes. Then looks for any that have
                been deleted by another client, they still appear in local manifest but not
                remote. Then looks to see if we have deleted any previously synced notes.
                Then scans the local notes dir looking for any notes not listed in
                NoteMetaData, they are new and must be uploaded. }
	  function LoadRepoData(ForceLCD : boolean): boolean;

            { Searches list for any clashes, refering each one to user. Done after
              list is filled out in case we want to ask user for general instrucions }
      procedure ProcessClashes();

        // Checks if local note exists, optionally returning with its last change date.
      function LocalNoteExists(const ID : string; out CDate: string; GetDate: boolean=false): Boolean;

        // Call this when we are resolving a sync clash. Note : not possible results make sense !
      function ProceedWith(const ID, FullRemoteFileName: ANSIString): TSyncAction;

                { Reads through Local Manifest file, filling out LocalMetaData,
                LastSyncDateSt and CurrRev. If local manifest does not exist, still
                returns True but CurrRev=0 and LocalLastSyncDateSt='' }
      function ReadLocalManifest(): boolean;

            { Writes a local mainfest file. Assumes NoteMetaData contains valid data about
              new Rev numbers and for uploads, last-change-date. If WriteOK is false then
              don't rev number and don't mention new notes in local manifest.}
      function WriteLocalManifest(const WriteOK, NewRev : boolean) : boolean;

            { We write a remote manifest out localy if we have any uploads or to handle
              the delete from server a note that was deleted locally to do.
              Then call Transport to deal with it. Writing it locally is fast
              and we get to check for and isolate any data errors. Initially
              written to $CONFIG/manifest.xml-remote and copied (moved ?).}
	  function WriteRemoteManifest(out NewRev: boolean): boolean;

   public
            // Indicates what we want this unit to do. Set it and call TestConnection() repeatly...
       RepoAction : TRepoAction;

            // Records local manifests view of serverID, after succefull test,
            // it should be the remote manifest's one too.
        LocalLastSyncID : string;

          { the calling process must pass a function address to this var. It will
            be called if the sync process finds a sync class where both copies
            of a note have changed since last sync.}
        ProceedFunction : TProceedFunction;

          { The calling process must set this to the address of a function to call
            every time a local note is deleted or overwritten during Sync. Its to
            deal with the case where a note is open but unchanged during sync.  }
        MarkNoteReadOnlyProcedure : TMarkNotereadOnlyProcedure;

                // A URL or directory with trailing delim.
        SyncAddress : string;
                // Revision number the client is currently on
        CurrRev : integer;
                // A string of local last sync date. Empty if we have not synced before
        LocalLastSyncDateSt : string;
                // Last time this client synced (not this run), set and tested in call to StartSync()
        LocalLastSyncDate : TDateTime;
                // Write debug messages as we do things.
        DebugMode : boolean;

                // Determine what we'd do and write ref ver of manifests but don't move files around.
        TestRun : boolean;
                // A reason why something failed.
        ErrorString : string;
                // Data about notes in remote manifest that may need be copied down
        NoteMetaData : TNoteInfoList;
                // Data obtained from Local Manifest. Might be empty.....
        LocalMetaData : TNoteInfoList;

        procedure FSetConfigDir(Dir : string);
        property ConfigDir : string read FConfigDir write FSetConfigDir;

        procedure FSetNotesDir(Dir : string);
        Property NotesDir : string read FNotesDir write FSetNotesDir;

                { Reports on contents of a created and filled list }
	    procedure ReportMetaData(out UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing: integer);

                { True=FileSync, False=Network Sync }
        procedure SetMode(Mode : boolean);               // todo - use an enumerated type ....

                { Checks NoteMetaData for valid Actions }
        function CheckMetaData() : boolean;

            { May return : SyncXMLError, SyncNoRemoteDir, SyncNoRemoteWrite,
              SyncNoRemoteRepo, SyncBadRemote, SyncMismatch. Checks if the connecton
              looks viable, either (fileSync) it has right files there and write access
              OR (NetSync) network answers somehow (?). SyncReady can proceed to
              StartSync, else must do something first, setup new connect, consult user etc.}
        function TestConnection() : TSyncAvailable;

            { Do actual sync, but if TestRun=True just report on what you'd do.
              Assumes a Transport has been selected and remote address is set.
              We must already be a member of this sync, ie, its remote ID is recorded
              in our local manifest. }
      function StartSync() : boolean;

      constructor Create();

      destructor Destroy(); override;

  end;

implementation

{ TSync }

uses laz2_DOM, laz2_XMLRead, Trans, TransFile, TransNet, LazLogger, LazFileUtils,
    FileUtil;

var
    Transport : TTomboyTrans;

constructor TSync.Create();
begin
    NoteMetaData := TNoteInfoList.Create;
    LocalMetaData := TNoteInfoList.Create;
    Transport := nil;
    //NewRepo := False;
end;

function TSync.ProceedWith(const ID, FullRemoteFileName : ANSIString) : TSyncAction;
var
    ClashRec : TClashRecord;
    //ChangeDate : ANSIString;
begin
    // Note - we no longer fill in all of clash record, let sdiff unit work it out.
    ClashRec.NoteID := ID;
    ClashRec.ServerFileName := FullRemoteFileName;
    ClashRec.LocalFileName := self.NotesDir + ID + '.note';
    Result := ProceedFunction(Clashrec);
    if Result in [SyAllOldest, SyAllNewest, SyAllLocal, SyAllRemote] then
        ProceedAction := Result;
end;


function TSync.ResolveAllClash(const Act : TSyncAction; const ID, FullRemoteFileName : ANSIString) : TSyncAction;
var
    Temp : string;
begin
    case Act of
        SyAllLocal : exit(SyUpLoadEdit);
        SyAllRemote : exit(SyDownLoad);
        SyAllNewest : if GetGMTFromStr(GetNoteLastChangeSt(NotesDir + ID + '.note', Temp))
                            >  GetGMTFromStr(GetNoteLastChangeSt(FullRemoteFileName, Temp)) then
                        exit(SyUploadEdit)
                      else
                        exit(SyDownLoad);
        SyAllOldest :  if GetGMTFromStr(GetNoteLastChangeSt(NotesDir + ID + '.note', Temp))
                            <  GetGMTFromStr(GetNoteLastChangeSt(FullRemoteFileName, Temp)) then
                        exit(SyUploadEdit)
                      else
                        exit(SyDownLoad);
    end;
end;

procedure TSync.ProcessClashes();
var
    Index : integer;
    RemoteNote : string;
begin
    for Index := 0 to NoteMetaData.Count -1 do
        with NoteMetaData.Items[Index]^ do begin
            if Action = SyClash then begin
                RemoteNote := Transport.DownLoadNote(ID, Rev);
                if ProceedAction = SyUnSet then begin       // let user decide
                    Action := ProceedWith(ID, RemoteNote);
                    if Action in [SyAllOldest, SyAllNewest, SyAllLocal, SyAllRemote] then
                        Action := ResolveAllClash(Action, ID, RemoteNote);
                end else
                    Action := ResolveAllClash(ProceedAction, ID, RemoteNote);  // user has already said "all something"
            end;
    end;
end;

procedure TSync.SetMode(Mode: boolean);
begin
    if Mode then begin
        Transport := TFileSync.Create;
        Transport.NotesDir:=NotesDir;
        Transport.ConfigDir:=ConfigDir;
        SyncAddress := AppendPathDelim(SyncAddress);
        if debugmode then debugln('Notes are stored in ' + NotesDir);
	end
	else Transport := TNetSync.Create;
    Transport.RemoteAddress:= SyncAddress;
end;

procedure TSync.ReportMetaData(out UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing : integer);
var
    Index : integer;
begin
    UpNew := 0; UpEdit := 0; Down := 0;
    DelLoc := 0; DelRem := 0; DoNothing := 0; Clash := 0;
    for Index := 0 to NoteMetaData.Count -1 do begin
        case NoteMetaData.Items[Index]^.Action of
            SyUpLoadNew : inc(UpNew);
            SyUpLoadEdit : inc(UpEdit);
            SyDownLoad : inc(Down);
            SyDeleteLocal : inc(DelLoc);
            SyDeleteRemote : inc(DelRem);
            SyClash : inc(Clash);
            SyNothing : inc(DoNothing);
		end;
    end;
end;

function TSync.CheckMetaData(): boolean;
var
    Index : integer;
begin
    Result := True;
    debugln('-------- In CheckMetaData --------');
    for Index := 0 to NoteMetaData.Count -1 do begin
        if not (NoteMetaData[Index]^.Action in [SyNothing, SyUploadNew, SyUpLoadEdit, SyDownLoad,
                SyDeleteLocal, SyDeleteRemote]) then
           Debugln('ERROR note not assigned ' + NoteMetaData[Index]^.ID + ' '
                   + NoteMetaData.ActionName(NoteMetaData[Index]^.Action) + '  '
                   + NoteMetaData.ActionName(NoteMetaData[Index]^.Action));
        result := False;
    end;
    debugln('NoteMetaData has ' + inttostr(NoteMetaData.Count) + ' entries.');
end;

function TSync.CheckUsingLCD(AssumeNoClash : boolean) : boolean;
var
    Index : integer;
    Info : TSearchRec;
    PNote : PNoteInfo;
    LocLCD : string;
begin
    if FindFirst(NotesDir + '*.note', faAnyFile, Info)=0 then begin
        repeat
            PNote := NoteMetaData.FindID(copy(Info.Name, 1, 36));
            LocLCD := GetNoteLastChangeSt(NotesDir + Info.Name, ErrorString);
            // hmm, not checking for errors there .....
            if PNote <> nil then begin
                if AssumeNoClash then exit(false);      // might be a clash, go fill out LCD in remote data
               if PNote^.LastChange = LocLCD then
                  PNote^.Action := SyNothing
               else PNote^.Action := SyClash;
            end else begin              // this note is a new upload, add it to list.
                new(PNote);
                PNote^.ID := copy(Info.Name, 1, 36);
                PNote^.LastChange:=LocLCD;
                PNote^.Action:= SyUpLoadNew;
                NoteMetaData.Add(PNote);
            end;
        until FindNext(Info) <> 0;
    end;
    FindClose(Info);
    for Index := 0 to NoteMetaData.Count -1 do
        if NoteMetaData.Items[Index]^.Action = SyUnSet then
           NoteMetaData.Items[Index]^.Action := SyDownLoad;
    exit(True);
end;

function TSync.TestConnection(): TSyncAvailable;
var
    ServerID : string;
begin
    if DebugMode then begin
        debugln('Remote address is ' + SyncAddress);
        debugln('Local Config ' + ConfigDir);
        debugln('Notes dir ' + NotesDir);
	end;
    if RepoAction = RepoNew then begin
        LocalLastSyncDate := 0;
        LocalLastSyncDateSt := '';
        Transport.RemoteServerRev:=-1;
        Transport.ANewRepo:= True;
        // means we should prepare for a new repo (but not make it yet), don't check for files.
    end;
    if RepoAction = RepoUse then begin
	    if not ReadLocalManifest() then exit(SyncXMLError);    // Error in local mainfest, OK or no manifest=true
	    LocalLastSyncDate :=  GetGMTFromStr(LocalLastSyncDateSt);
	    if LocalLastSyncDate < 1.0 then begin
		        ErrorString := 'Invalid last sync date in local manifest ' + LocalLastSyncDateSt;
		        exit(SyncXMLError);
        end;
    end;
    if RepoAction = RepoJoin then begin
        LocalLastSyncDate := 0;
        LocalLastSyncDateSt := '';
    end;
    Result := Transport.TestTransport(ServerID);
    if Result <> SyncReady then begin
      ErrorString := Transport.ErrorString;
      exit;
    end;
    if DebugMode then begin
        debugln('CurrRev=' + inttostr(CurrRev) + '   Last Sync=' + LocalLastSyncDateSt
                        + '   Local Entries=' + inttostr(LocalMetaData.Count));
        debugln('Config=' + ConfigDir + ' NotesDir=' + NotesDir);
    end;
    if RepoAction = RepoUse then
	    if ServerID <> LocalLastSyncID then begin
	        ErrorString := 'ServerID Mismatch';
            if DebugMode then
                debugln('ERROR Server ID Mismatch Remote ' + ServerID + ' and local ' + LocalLastSyncID);
	        exit(SyncMismatch);
		end;
end;

procedure TSync.DisplayNoteInfo(meta : TNoteInfoList);
var
    I : Integer;
    St : string;
begin
    debugln('-----------list dump for ' + Meta.ClassName);
    for I := 0 to Meta.Count -1 do begin
        St := ' ' + inttostr(Meta.Items[i]^.Rev);
        while length(St) < 5 do St := St + ' ';
        // St := Meta.ActionName(Meta.Items[i]^.Action);
        debugln(Meta.Items[I]^.ID + St + Meta.ActionName(Meta.Items[i]^.Action)
            + Meta.Items[i]^.LastChange);
    end;
end;


procedure TSync.CheckRemoteDeletes();
var
    Index : integer;
    PNote : PNoteInfo;
begin
    // Iterate over LocalMetaData looking for notes listed a prev synced
    // but are not listed in NoteMetaData. We'll add a entry in RemoteMetaData
    // for any we find.
    for Index := 0 to LocalMetaData.Count -1 do begin
        if not LocalMetaData.Items[Index]^.Deleted then
            if nil = NoteMetaData.FindID(LocalMetaData.Items[Index]^.ID) then begin   // That is, we did not find it
                new(PNote);
                PNote^.ID:= LocalMetaData.Items[Index]^.ID;
                PNote^.Title := LocalMetaData.Items[Index]^.Title;                      // I think we know title, useful debug info here....
                PNote^.Action := SyDeleteLocal;                                       // Was deleted elsewhere, do same here.
                NoteMetaData.Add(PNote);
            end;
    end;
end;

procedure TSync.CheckNewNotes();
var
    Info : TSearchRec;
    PNote : PNoteInfo;
    ID, CDate : string;
begin
    if FindFirst(NotesDir + '*.note', faAnyFile, Info)=0 then begin
        repeat
            ID := copy(Info.Name, 1, 36);
            Debugln('Found [' + NotesDir+ Info.Name + ']');
            PNote := NoteMetaData.FindID(ID);
            if PNote = nil then begin
                if LocalNoteExists(ID, CDate, True) then begin
	                    new(PNote);
	                    Pnote^.ID:=ID;
	                    Pnote^.LastChange:=CDate;
                        // PNote^.Rev := self.;          // don't need, upload knows how to inc
	                    PNote^.Action:=SyUploadNew;
                        NoteMetaData.Add(PNote);
				end else Debugln('Failed to find lastchangedate in ' + Info.Name);
			end;
			until FindNext(Info) <> 0;
        end;
    FindClose(Info);

end;

    // Iterate over LocalMetaData looking for notes that have been deleted
    // locally and put them in RemoteMetaData to be deleted from the server.
procedure TSync.CheckLocalDeletes();
var
    I : integer;
    PNote : PNoteInfo;
begin
    for I := 0 to LocalMetaData.Count -1 do begin
        if LocalMetaData.Items[i]^.Deleted then begin
            PNote := NoteMetaData.FindID(LocalMetaData.Items[i]^.ID);
            if PNote <> nil then
                PNote^.Action := SyDeleteRemote;
        end;
    end;
end;

function TSync.UseRemoteData() : boolean;
var
    I : integer;
    ID : string;    // to make it a bit easier to read souce
    PNote : PNoteInfo;
    LocCDate : string;
    LocChange, RemChange : boolean;
begin
    Result := True;
    for I := 0 to NoteMetaData.Count -1 do begin
        ID := NoteMetaData.Items[I]^.ID;
        if LocalNoteExists(ID, LocCDate) then begin
            LocalNoteExists(ID, LocCDate, True);
            LocChange := GetGMTFromStr(LocCDate) > LocalLastSyncDate;       // TDateTime is a float
            RemChange := NoteMetaData.Items[I]^.Rev > CurrRev;
            if LocChange and RemChange then
                NoteMetaData.Items[I]^.Action := SyClash
            else if LocChange then
                    NoteMetaData.Items[I]^.Action := SyUpLoadEdit
                else if RemChange then
                        NoteMetaData.Items[I]^.Action := SyDownLoad
                    else  NoteMetaData.Items[I]^.Action := SyNothing;
        end else begin      // OK, not here but maybe we deleted it previously ?
            Pnote := LocalMetaData.FindID(ID);
            if PNote <> Nil then begin
                if PNote^.Deleted then
                   NoteMetaData.Items[I]^.Action:=SyDeleteRemote;       // I have deleted that already.
            end else NoteMetaData.Items[I]^.Action:=SyDownload;         // its a new note from elsewhere
        end;
        if NoteMetaData.Items[I]^.Action = SyUnset then debugln('---- missed one -----');
        if NoteMetaData.Items[I]^.Action = SyUpLoadEdit then begin
            NoteMetaData.Items[I]^.LastChange := GetNoteLastChangeSt(NotesDir + ID + '.note', ErrorString);
            debugln('=========== LCD is [' + NoteMetaData.Items[I]^.CreateDate + ']');
        end;
    end;
end;

function TSync.DoDownloads() : boolean;
begin
    Result := True;
    if not TestRun then begin
        debugln('Downloading notes.');
        Result := Transport.DownloadNotes(NoteMetaData);
	end;
	if Result = false then begin
       self.ErrorString:= Transport.ErrorString;
       debugln('ERROR - Download Notes reported ' + ErrorString);
	end;
end;

function TSync.DoDeletes() : boolean;
var
    Index : integer;
begin
    for Index := 0 to NoteMetaData.Count - 1 do begin
        if NoteMetaData[Index]^.Action = SyDeleteRemote then begin
            if DebugMode then
               Debugln('Delete remote note : ' + NoteMetaData.Items[Index]^.ID);
            if not TestRun then
               if not Transport.DeleteNote(NoteMetaData.Items[Index]^.ID,
                            NoteMetaData.Items[Index]^.Rev) then Exit(False);
		end;
	end;
	Result := true;
end;

function TSync.DoUploads() : boolean;
var
    Uploads : TstringList;
    Index : integer;
begin

    debugln('------- RemoteServerRev is ' + inttostr(Transport.RemoteServerRev));
    try
        Uploads := TstringList.Create;
        for Index := 0 to NoteMetaData.Count -1 do begin
            if NoteMetaData.Items[Index]^.Action in [SyUploadEdit, SyUploadNew] then begin
               Uploads.Add(NoteMetaData.Items[Index]^.ID);
               NoteMetaData.Items[Index]^.Rev := Transport.RemoteServerRev + 1;
			end;
		end;
        if not TestRun then
           if not Transport.UploadNotes(Uploads) then begin
              ErrorString := Transport.ErrorString;
              exit(False);
		   end;
	finally
        Uploads.Free;
	end;
	Result := true;
end;

function TSync.LoadRepoData(ForceLCD : boolean): boolean;
begin
    Result := True;
    FreeAndNil(NoteMetaData);
    NoteMetaData := TNoteInfoList.Create;
    case RepoAction of
        RepoUse : Result := Transport.GetNewNotes(NoteMetaData, False);
        RepoJoin : Result := Transport.GetNewNotes(NoteMetaData, ForceLCD);
    end;
    // We do not load remote metadata when creating a new repo !
end;

                        { ---------- The Lets Do it Function ------------- }

function TSync.StartSync(): boolean;
var
    NewRev : boolean;
    // Tick1, Tick2, Tick3, Tick4 : Dword;
begin
    freeandNil(LocalMetaData);
    LocalMetaData := TNoteInfoList.Create;
    if not LoadRepoData(False) then exit(False);     // don't get LCD until we know we need it.
    case RepoAction of
        RepoUse : begin UseRemoteData(); CheckRemoteDeletes(); CheckLocalDeletes(); end;
        RepoJoin : if not CheckUsingLCD(True) then begin    // at least 1 possible clash
                 LoadRepoData(True);                        // start again, getting LCD this time
                 CheckUsingLCD(False);
            end;
        RepoNew : begin
                freeandNil(NoteMetaData);
                NoteMetaData := TNoteInfoList.Create;
            end
    end;
    CheckNewNotes();
    ProcessClashes();
    if DebugMode then DisplayNoteInfo(NoteMetaData);
    if CheckMetaData() then begin
       debugln('------ Error in metadata list ----------');
       exit(False);
	end;

    if not DoDownLoads() then exit(False);          // DoDownloads() will tell us what troubled it.
    Result := WriteRemoteManifest(NewRev);
    if Result then begin                           // false is an ERROR !
        Result := DoDeletes();
        if Result then
            if DoUploads() then
	            WriteLocalManifest(true, NewRev);
    end;
	if not Result then
        WriteLocalManifest(false, false);       // write a recovery local manifest. Downloads only noted.
end;


function TSync.LocalNoteExists(const ID : string; out CDate : string; GetDate : boolean = false) : Boolean;
var
	Doc : TXMLDocument;
	Node : TDOMNode;
    //LastChange : string;
begin
    if not FileExists(NotesDir + ID + '.note') then exit(False);
    if not GetDate then exit(True);
	try
		ReadXMLFile(Doc, NotesDir + ID + '.note');
		Node := Doc.DocumentElement.FindNode('last-change-date');
        if assigned(node) then
            CDate := Node.FirstChild.NodeValue
        else
            CDate := '';
	finally
        Doc.free;
    end;
end;

destructor TSync.Destroy();
begin
    if LocalMetaData <> Nil then
        LocalMetaData.Free;
    if NoteMetaData <> Nil then
        NoteMetaData.Free;
     if Transport <> Nil then
        Transport.Free;
    inherited Destroy();
end;

function TSync.ReadLocalManifest() : boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    j : integer;
    NoteInfoP : PNoteInfo;
    RevStr : string;
begin
    Result := true;
    ErrorString := '';
    if not FileExists(ConfigDir + 'manifest.xml') then begin
        LocalLastSyncDateSt := '';
        CurrRev := 0;
        exit(True);                 // Its not an error, just never synced before
    end;
    if DebugMode then debugln('Reading local mainfest');
    try
    	try
    		ReadXMLFile(Doc, ConfigDir + 'manifest.xml');
            Node := Doc.DocumentElement.FindNode('last-sync-date');
            if assigned(Node) then
                LocalLastSyncDateSt := Node.FirstChild.NodeValue
            else begin
                LocalLastSyncDateSt := '';
                debugln('ERROR, cannot find LSD in ' + ConfigDir + 'manifest.xml');
            end;
            Node := Doc.DocumentElement.FindNode('server-id');
            LocalLastSyncID := Node.FirstChild.NodeValue;
            Node := Doc.DocumentElement.FindNode('last-sync-rev');
            try
                RevStr := Node.FirstChild.NodeValue;
                if RevStr[1] = '"' then
                   Revstr := copy(revStr, 2, length(RevStr) - 2);
        		CurrRev := strtoint(RevStr);
			except
                    on EConvertError do                         // just a plain bad string
                        ErrorString := 'Error converting Local Rev Version ' + RevStr;
					on EObjectCheck do                         // mac does this
                        ErrorString := 'Error in local mainfest, check RevNo';
                    on EAccessViolation do                  	// Lin does this
                        ErrorString := 'Error in local mainfest, check RevNo';
            end;
            if ErrorString <> '' then begin
                CurrRev := 0;
                LocalLastSyncDateSt := '';
                exit(False);
			end;
            if LocalLastSyncID[1] = '"' then
               LocalLastSyncID := copy(LocalLastSyncID, 2, 36);
			NodeList := Doc.DocumentElement.FindNode('note-revisions').ChildNodes;
            if assigned(NodeList) then
               for j := 0 to NodeList.Count-1 do begin
                   new(NoteInfoP);
                   NoteInfoP^.ID := NodeList.Item[j].Attributes.GetNamedItem('guid').NodeValue;
                   NoteInfoP^.Rev := strtoint(NodeList.Item[j].Attributes.GetNamedItem('latest-revision').NodeValue);
                   NoteInfoP^.Deleted := False;
                   LocalMetaData.Add(NoteInfoP);
			   end;
            NodeList := Doc.DocumentElement.FindNode('note-deletions').ChildNodes;
            if assigned(NodeList) then
               for j := 0 to NodeList.Count-1 do begin
                   new(NoteInfoP);
                   NoteInfoP^.ID := NodeList.Item[j].Attributes.GetNamedItem('guid').NodeValue;
                   NoteInfoP^.Title := NodeList.Item[j].Attributes.GetNamedItem('title').NodeValue;
                   NoteInfoP^.Deleted := True;
                   LocalMetaData.Add(NoteInfoP);
   			   end;
		finally
            Doc.Free;
		end;
	except
      on EAccessViolation do begin         // probably means we did not find an expected attribute
              ErrorString := 'Error in local mainfest';
              CurrRev := 0;
              LocalLastSyncDateSt := '';
              Result := false;
          end;
	end;

end;

function TSync.WriteRemoteManifest(out NewRev : boolean): boolean;
var
    OutFile: TextFile;
    Index : integer;
    NewRevString : string;
begin
    result := true;
    NewRev := False;
    for Index := 0 to NoteMetaData.Count - 1 do begin
        if NoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit, SyDeleteRemote] then begin
            NewRev := True;
            break;                  // one is enough, we need a new manifest file !
		end;
    end;
    if Not NewRev then exit(true);      // exit cos no need for new Man, ret true cos no file error,
    NewRevString := inttostr(Transport.RemoteServerRev + 1);
    AssignFile(OutFile, ConfigDir + 'manifest.xml-remote');
    try
	    try
		    Rewrite(OutFile);
	        writeln(OutFile, '<?xml version="1.0" encoding="utf-8"?>');
            write(OutFile, '<sync revision="' + NewRevString);
            writeln(OutFile, '" server-id="' + Transport.RemoteServerID + '">');
	        for Index := 0 to NoteMetaData.Count - 1 do begin
                if NoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit, SyDownLoad, SyNothing] then begin
	                write(OutFile, '  <note id="' + NoteMetaData.Items[Index]^.ID + '" rev="');
                    if NoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit] then
	                    write(OutFile, NewRevString + '"')
	                else write(OutFile, inttostr(NoteMetaData.Items[Index]^.Rev) + '"');
                    if NoteMetaData.Items[Index]^.LastChange = '' then
                       writeln(OutFile, ' />')
                    else writeln(OutFile, ' last-change-date="'
                            + NoteMetaData.Items[Index]^.LastChange + '" />');
				end;
			end;
            writeln(OutFile, '</sync>');
        except
          on E: EInOutError do begin
              Debugln('File handling error occurred. Details: ' + E.Message);
              exit(false);      // file error !
    	  end;
    	end;
	finally
        CloseFile(OutFile);
	end;
        // do a safe version of this -
    if not TestRun then
           result := Transport.DoRemoteManifest(ConfigDir + 'manifest.xml-remote');
end;

procedure TSync.FSetConfigDir(Dir: string);
begin
        FConfigDir := AppendPathDelim(Dir);
        if DirectoryExists(FConfigDir) then begin
           if Not DirectoryIsWritable(FConfigDir) then
              self.ErrorString:= 'Cannot write to config dir';
		end else ErrorString := 'Config dir does not exist';
end;

procedure TSync.FSetNotesDir(Dir: string);
begin
        FNotesDir := AppendPathDelim(Dir);
end;

function TSync.WriteLocalManifest(const WriteOK, NewRev : boolean ): boolean;
{ We try and provide some recovery from a fail to write to remote repo. It should
  not happen but ... If WriteOk is false we write back local manifest that still
  mentions the previous deleted files and does not list locally new and changed
  files. Such files retain their thier previous rev numbers. Test !

  }
var
    OutFile: TextFile;
    Index : integer;
    IncRev : integer = 0;
begin
    result := true;
    if WriteOK and NewRev then IncRev := 1 else IncRev := 0;
    AssignFile(OutFile, ConfigDir + 'manifest.xml-local');
    try
	        try
		        Rewrite(OutFile);
                writeln(OutFile, '<?xml version="1.0" encoding="utf-8"?>');
                writeln(Outfile, '<manifest xmlns="http://beatniksoftware.com/tomboy">');
                writeln(OutFile, '  <last-sync-date>' + GetLocalTime + '</last-sync-date>');
                write(OutFile, '  <last-sync-rev>"' + inttostr(Transport.RemoteServerRev + IncRev));
                writeln(OutFile, '"</last-sync-rev>');
                writeln(OutFile, '  <server-id>"' + Transport.RemoteServerID + '"</server-id>');
                writeln(OutFile, '  <note-revisions>');
		        for Index := 0 to NoteMetaData.Count - 1 do begin
                    if NoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit, SyDownLoad, SyNothing] then begin
                        if (not WriteOK) and (NoteMetaData[Index]^.Action = SyUpLoadNew) then continue;
                        write(Outfile, '    <note guid="' + NoteMetaData[Index]^.ID + '" latest-revision="');
                     {   case NoteMetaData[Index]^.Action of
                            SyUpLoadEdit, SyUpLoadNew : write(OutFile, inttostr(NoteMetaData[Index]^.Rev +IncRev));
                            SyDownload, SyNothing : write(OutFile, inttostr(NoteMetaData[Index]^.Rev));
						end;    }
                        write(OutFile, inttostr(NoteMetaData[Index]^.Rev));
                        writeln(Outfile, '" />');
					end;
				end;
                    writeln(OutFile, '  </note-revisions>'#10'  <note-deletions>');
                    writeln(OutFile, '  </note-deletions>'#10'</manifest>');
		    finally
	            CloseFile(OutFile);
		    end;
    except
      on E: EInOutError do begin
          Debugln('File handling error occurred. Details: ' + E.Message);
          exit(false);
	  end;
	end;
    // if to here, copy the file over top of existing local manifest
	if not TestRun then
       copyfile(ConfigDir + 'manifest.xml-local', ConfigDir + 'manifest.xml');
	   // Transport.SetRemoteRepo(ConfigDir + 'manifest.xml-local');
end;


end.

