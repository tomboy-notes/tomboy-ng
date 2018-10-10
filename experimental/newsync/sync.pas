unit sync;

{$mode objfpc}{$H+}

{ Operation of this unit -
  If TestConnection is successful we'll have a list in NoteNetaData derived from
  the "Remote Manifest" and one from Local Manifest ready to use.

  Call StartSync - that will scan the NoteMetaData assigning an action to each item.
  This is all the notes the server thinks we have. But we'll have some newly created
  ones, some we have deleted locally and some deleted by another client.
  At this stage, all items in remoteMetaData should be assigned, an error otherwise .....

  If we have an item locally that was previously synced (ie is in local manifest) but
  did not appear in RemoteMetaData, then its been deleted by another client and should
  be backed up and deleted locally. In NoteMetaData as DeleteLocal.

  If we have items listed in our local manifest as deleted (and therefore must have been
  previously synced and are now deleted), we'll put them in NoteMetaData marked as
  DeleteRemote, we'll ask server to delete them.

  At this stage, every note mentioned in local manifest should be present in NoteMetaData
  and should be one of - Upload, Download, DeleteRemote or DeleteLocal. We could check !

  Now to detect any notes that are new since last sync, we scan over notes that exist
  in the notes directory - any we find that are not already listed in NoteMetaData
  should be listed, we get its last-change-date and assign it as Upload. Its revision
  number will be current server number plus one (but not set until upload process).

  Scan list to see if we have any uploads to do. If so, we must first make a new
  remote manifest (locally) and then advise remote server about it. This will be
  trigger for remote system to do whatever is necessary for new revision, ie in FileSync
  server will create a new dir called the new revion number, copy the existing remote
  manifest there and then copy the newly created (locally) remote manifest up to
  the root of repo. If this fails, we can gracefully abort with no harm done.

  OK, now at last, move some notes. It does not seem to matter what order we do this in,
  so, download and delete (and backup), then advise server of deletes and then
  finally, upload. Uploads will be associate with new revision number, in the case
  of file sync for example, they will go into the new directory named for the the
  new revision, it is,of course, RemoteServerRev plus one.

  As each note is uploaded, we update its revision number in NoteDataList. That way, each
  note mentioned in NoteDataList has a valid rev number and, in the event of an upload
  failure, un-uploaded notes will not have a (valid) revision number (ie -1).

  Notes that are marked Nothing retain existing revno, Downloads get the revno assigned
  by the Transport layer, uploads get the revnumb we calculate (at upload time) by
  inc-ing the Transport's RemoteServerRev.

  Then, create local and remote manifests from the data in NoteMetaData.

  --------- IMPORTANT - delete a note creates a new rev ------------------------

  Deleting a note, that is a sync operation where the only change is a local note
  has been deleted, is interesting. The current sync model (ie old one, not here)
  does create a new revision, it writes a new manifest identical to previous one except
  it does not list the note in question and gets an updated rev number. It makes a new
  revision directory that contains only the old, ie backup, manifest.

  Tomboy does make a new revision with a delete and we must do it their way.

  DoSync() must cope with no remote repo OR no local repo OR BOTH !  Test Davo, TEST !


  The NEW join new process.
  -------------------------
  1. Create an Sync Object, tell it where config, notes and sync are, setmode.
  2. Call TestConnection() to see how we are setup, in a loop until we get SyncReady
     There are lots of return values that need considering. See SyncUtils.
  3. When you have SyncReady, same as Syncing to a repo we are a member of.



  Running a Sync we are already a member of
  -----------------------------------------
  1. Create an Sync Object, tell it where config, notes and sync are, setmode.
  2. TestConnection()  does some clean up of paths and -
  3.    Call ReadLocalManifest() - it reads local manifest and puts what it finds in
        LocalMetaData list. It does not set action.
  4.    Sets LastSyncDate and LocalSyncDateSt and then does sanity check.
  5.    Calls Transport.TestTransport that will do several tests to see if we are looking
        at a suitable sync repo, (eg on FileSync) gets ServerID from remote manifest
        and and returns with it to compare to LocalLastSyncID.
        At this stage, we know we have matching ServerIDs, suitable permissions in what
        looks like a sync repo.
  6. StartSync() assuming we have got past previous stuff. Calls LoadMetaData because NoteMetaData is empty.
  7.   Tries to WriteRemoteManifest(NewRev);
  8.   DoDeletes() - based on data in NoteMetaData, calls transport to delete notes on server.
  9.   DoUploads() - based on data in NoteMetaData, calls transport to upload notes to server.
  10.  Finally writes a new local manifest.



}

interface
uses
    Classes, SysUtils, SyncUtils;





type                       { ----------------- T S Y N C --------------------- }

  { TSync }

  TSync = class

  private
	         // Where we find Tomboy style notes
	    FNotesDir : string;
	         // Where we find config and local manifest files
	    FConfigDir : string;

      { Goes over NoteMetaData (which has only remote notes at this stage) assigning
        an Action to each. Notes with Rev number same or earlier that local CurrRev
        won't be downloaded but might need to be set to Upload. So this resolves any
        note previosly synced (even if edited since last sync) but not new notes. }
      function CheckAgainstRemote(): boolean;

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
                and detected and marked as uploads or clashes. Then looks for any that have
                been deleted by another client, they still appear in local manifest but not
                remote. Then looks to see if we have deleted any previously synced notes.
                Then scans the local notes dir looking for any notes not listed in
                NoteMetaData, they are new and must be uploaded. }
	  function LoadMetaData(): boolean;

            { Searches list for any clashes, refering each one to user. Done after
              list is filled out in case we want to ask user for general instrucions }
      procedure ProcessClashes();

        // Checks if local note exists, optionally returning with its last change date.
      function LocalNoteExists(const ID : string; out CDate: string; GetDate: boolean=false): Boolean;

        // Call this when we are resolving a sync clash. Note : not possible results make sense !
      function ProceedWith(const ID, FullRemoteFileName: ANSIString): TSyncAction;

      // Reads through Local Manifest file, filling out LocalMetaData, LastSyncDateSt and CurrRev.
      // If local manifest does not exist, still returns True but CurrRev=0 and LocalLastSyncDateSt=''
      function ReadLocalManifest(SkipFile: boolean=False): boolean;


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
            // Indicates we should make a new repo there, set during initial testing.
       NewRepo : boolean;
            // Disregard loacal manifest, its its there or not.
       IgnoreLocalManifest : boolean;
            // Join this Repo, even if Server IDs don't match, implies disregard of local manifest.
       ForceJoin : boolean;

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
        procedure SetMode(Mode : boolean);

                { Checks NoteMetaData for valid Actions }
        function CheckMetaData() : boolean;

            { May return : SyncXMLError, SyncNoRemoteDir, SyncNoRemoteWrite,
              SyncNoRemoteRepo, SyncBadRemote, SyncMismatch. Checks if the connecton
              looks viable, either (fileSync) it has right files there and write access
              OR (NetSync) network answers somehow (?). SyncReady can proceed to
              StartSync, else must do something first, setup new connect, consult user etc.

              Observes NewRepo, IgnoreLocalManifest and ForceJoin }
        function TestConnection() : TSyncAvailable;

            { Non interactive, just tries to Join a sync located at indicated repo, either
              Net or File. We might be the first connection so do init stuff,
              if we have local sync setup available, we will delete it and start from scratch.
              If successful, creates a local mainfest with server ID but no last-sync-date.}
        function JoinSync() : boolean;

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
    NewRepo := False;
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
end;

procedure TSync.ProcessClashes();
var
    Index : integer;
begin
    for Index := 0 to NoteMetaData.Count -1 do
        with NoteMetaData.Items[Index]^ do begin
        if Action = SyClash then begin
            Action := ProceedWith(ID, Transport.DownLoadNote(ID, Rev));
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
    for Index := 0 to NoteMetaData.Count -1 do begin
        if not (NoteMetaData[Index]^.Action in [SyNothing, SyUploadNew, SyUpLoadEdit, SyDownLoad,
                SyDeleteLocal, SyDeleteRemote]) then
           Debugln('ERROR note not assigned ' + NoteMetaData[Index]^.ID + ' '
                   + NoteMetaData.ActionName(NoteMetaData[Index]^.Action));
        result := False;
    end;
    debugln('NoteMetaData has ' + inttostr(NoteMetaData.Count) + ' entries.');
end;

function TSync.TestConnection(): TSyncAvailable;
var
    ServerID : string;
    Nowish : TDateTime;
begin
    if DebugMode then begin
        debugln('Remote address is ' + SyncAddress);
        debugln('Local Config ' + ConfigDir);
        debugln('Notes dir ' + NotesDir);
	end;
    FreeandNil(NoteMetaData);
    NoteMetaData := TNoteInfoList.Create;       // Always start empty !
    FreeandNil(LocalMetaData);
    LocalMetaData := TNoteInfoList.Create;
    if debugmode then
       debugln('--- Making a new NoteMetaData List , IgnoreLocal=' + booltostr(IgnoreLocalManifest, True) + '---');
    if NewRepo then
       Transport.ANewRepo:= True;
    if not IgnoreLocalManifest then begin
	    if not ReadLocalManifest() then exit(SyncXMLError);    // Error in local mainfest, OK or no manifest=true
	    LocalLastSyncDate :=  GetGMTFromStr(LocalLastSyncDateSt);
        Nowish := Now();
	    if LocalLastSyncDate <> 0 then begin
            {if LocalLastSyncDate > now() then
                ErrorString := 'Invalid, future last sync date in local manifest ' + LocalLastSyncDateSt;
            if LocalLastSyncDate < (now() - 36500) then
                ErrorString := 'Invalid, too old last sync date in local manifest ' + LocalLastSyncDateSt;   }

            if (LocalLastSyncDate > (now() + 36500)) or (LocalLastSyncDate < (Now() - 36500))  then begin
		                // TDateTime has integer part no. of days, fraction part is fraction of day.
		                // 100years ago or in future - Fail !
		        ErrorString := 'Invalid last sync date in local manifest ' + LocalLastSyncDateSt;

		        exit(SyncXMLError);
            end;
		end else if DebugMode then
               Debugln('No local manifest, probably a new sync');
    end else begin
        LocalLastSyncDate := 0;
        LocalLastSyncDateSt := '';
	end;
	if DebugMode then begin
        debugln('CurrRev=' + inttostr(CurrRev) + '   Last Sync=' + LocalLastSyncDateSt
                        + '   Local Entries=' + inttostr(LocalMetaData.Count));
        debugln('Config=' + ConfigDir + ' NotesDir=' + NotesDir);
    end;
    Result := Transport.TestTransport(ServerID);
    if Result <> SyncReady then begin
      ErrorString := Transport.ErrorString;
      exit;
    end;
    if not IgnoreLocalManifest then
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

// this method no longer used
function TSync.JoinSync(): boolean;
begin
    Result := LoadMetaData();
    // Result := True;
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

function TSync.CheckAgainstRemote() : boolean;
var
    I : integer;
    ID : string;    // to make it a bit easier to read souce
    PNote : PNoteInfo;
    LocCDate : string;
begin
    Result := True;
    for I := 0 to NoteMetaData.Count -1 do begin
        ID := NoteMetaData.Items[I]^.ID;
        if LocalNoteExists(ID, LocCDate) then begin                        // local copy exits.
            // We may not have a remote date if its remote note's rev is < lastsync


            // when looking at local notes, we must destinguish between ones that have changed since
            // last sync or not. Compare thier lastchangedate with local lastsyncdate.


            // this block is wrong !  Firstly, it does not sit well with flow chart
            // and secondly, what if the note has been edited ???
            {if NoteMetaData.Items[I]^.Rev < self.CurrRev then begin
                NoteMetaData.Items[I]^.Action:=SyNothing;
                continue;
			end;}

            LocalNoteExists(ID, LocCDate, True);
			if debugmode then
               debugln('Change Dates Loc ' + LocCDate + ' and remote ' + NoteMetaData.Items[I]^.LastChange);

            if LocCDate = NoteMetaData.Items[I]^.LastChange then begin   // We have identical note locally
                NoteMetaData.Items[I]^.Action:=SyNothing;
                continue;
            end;
            // Dates don't match, must do something, either upload, download or Clash.
            if GetGMTFromStr(LocCDate) > LocalLastSyncDate then begin // local version changed since last sync !
                if NoteMetaData.Items[I]^.Rev <= CurrRev then         // Remote version unchanged, easy !
                    NoteMetaData.Items[I]^.Action:= SyUpLoadEdit
                else NoteMetaData.Items[I]^.Action := SyClash;        // resolve later
            end else NoteMetaData.Items[I]^.Action:= SyDownload;      // local note not edited since last sync
        end else begin      // OK, not here but maybe we deleted it previously ?
            Pnote := Self.LocalMetaData.FindID(ID);
            if PNote <> Nil then begin
                if PNote^.Deleted then
                   NoteMetaData.Items[I]^.Action:=SyDeleteRemote;       // I have deleted that already.
            end else NoteMetaData.Items[I]^.Action:=SyDownload;         // its a new note from elsewhere
        end;
        if NoteMetaData.Items[I]^.Action = SyUnset then debugln('---- missed one -----');
        if NoteMetaData.Items[I]^.Action = SyUpLoadEdit then
            NoteMetaData.Items[I]^.CreateDate := GetNoteLastChangeSt(NotesDir + ID + '.note', ErrorString);
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

function TSync.LoadMetaData(): boolean;
begin
	if not Transport.GetNewNotes(NoteMetaData, CurrRev) then begin
          ErrorString := Transport.ErrorString;
          exit(False);
	end;
    Result := CheckAgainstRemote(); // don't do anything at this stage, just assign actions.
    CheckRemoteDeletes();           // if we have notes prev synced but not now in NoteMetaData, it was deleted
                                    // by another client, must (backup and) delete here. Mark these as DeleteLocal.
    CheckLocalDeletes();            // Things mentioned as deleted in local manifest, delete remote copy too.
                                    // Mark these as DeleteRemote. Must make new rev.
    CheckNewNotes();                // Scans Notes dir for note new since last sync.
    if DebugMode then DisplayNoteInfo(NoteMetaData);
    if CheckMetaData() then begin
       debugln('------ Error in metadata list ----------');
       exit(False);
	end;
end;

                        { ---------- The Lets Do it Function ------------- }

function TSync.StartSync(): boolean;
var
    NewRev : boolean;
begin
	if NoteMetaData.Count = 0 then
        LoadMetaData;
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

function TSync.ReadLocalManifest(SkipFile : boolean = False) : boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    j : integer;
    NoteInfoP : PNoteInfo;
    RevStr : string;
begin
    Result := true;
    if IgnoreLocalManifest then exit();
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
                LocalLastSyncDateSt := Node.FirstChild.NodeValue;
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
	                    writeln(OutFile, NewRevString + '" />')
	                else writeln(OutFile, inttostr(NoteMetaData.Items[Index]^.Rev) + '" />');
				end;

                // Must add last-change-date to that record, once I have valid date in NoteMetaData

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

