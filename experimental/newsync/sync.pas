unit sync;

{$mode objfpc}{$H+}

{ Operation of this unit -
  If TestConnection is successful we'll have a list derived from the "Remote Manifest"
  and from Local Manifest ready to use.
  Call StartSync - that will scan the RemoteMetaData assigning an action to each item,
  all items should be assigned .....
  If we have an item locally that was previously synced (ie is in local manifest) but
  did not appear in RemoteMetaData, then its been deleted by another client and should
  be backed up and deleted locally.
}

interface
uses
    Classes, SysUtils, SyncUtils;





type                       { ----------------- T S Y N C --------------------- }

  { TSync }

  TSync = class

  private
      fDoFileSync: boolean;
      fDoNetSync: boolean;

      // Goes over Remote list assigning an action to every note after our current rev.
      function CheckAgainstRemote(): boolean;

        {Notes we have deleted (and existed) here since last sync are marked DeleteRemote
         because we must delete them from the server. For file sync, that means not
         mentioning them in remote manifest any more. }
      procedure CheckLocalDeletes();

        {if we have a note prev synced (ie, in local manifest) but not now in RemoteMetaData,
         it was deleted by another client, Mark these as DeleteLocal, will later backup and delete.}
      procedure CheckRemoteDeletes();

       // Just a debug procedure, dumps (some) contents of a list to console
      procedure DisplayNoteInfo(meta: TNoteInfoList);

        // Checks if local note exists, optionally returning with its last change date.
      function LocalNoteExists(const ID : string; out CDate: string; GetDate: boolean=false): Boolean;

        // Call this when we are resolving a sync clash. Note : not fully implemented !
      function ProceedWith(FullRemoteFileName: ANSIString): TClashDecision;

      // Reads through Local Manifest file, filling out LocalMetaData, LastSyncDateSt and CurrRev.
      function ReadLocalManifest(SkipFile: boolean=False): boolean;

    (*			{ Passes on users choice to Upload, download or nothing. Calls a function
                  passed here from the calling process. }
	function ProceedWith(FileID, Rev : ANSIString) : TClashDecision;   *)

  public
          { the calling process must pass a function address to this var. It will
            be called if the sync process finds a sync class where both copies
            of a note have changed since last sync.}
    ProceedFunction : TProceedFunction;

          { The calling process must set this to the address of a function to call
            every time a local note is deleted or overwritten during Sync. Its to
            deal with the case where a note is open but unchanged during sync.  }
    MarkNoteReadOnlyProcedure : TMarkNotereadOnlyProcedure;

            // Where we find Tomboy style notes
    NotesDir : string;
            // Where we find config and local manifest files
    ConfigDir : string;
            // A URL or directory with trailing delim.
    SyncAddress : string;
            // Revision number the client is currently on
    CurrRev : integer;
            // A string of local last sync date. Empty if we have not synced before
    LocalLastSyncDateSt : string;
            // Last time this client synced, set and tested in call to StartSync()
    LocalLastSyncDate : TDateTime;

      DebugMode : boolean;
      TestRun : boolean;
      ErrorString : string;
            // Data about notes in remote manifest that may need be copied down
      RemoteMetaData : TNoteInfoList;
            // Data obtained from Local Manifest. Might be empty.....
      LocalMetaData : TNoteInfoList;
            // A report on what happened (or we make out happened) during a sync
      ReportList : TSyncReportList;


      //constructor Creator(const CNotesDir, CConfigDir : string; const CRev : integer);

      { True=FileSync, False=Network Sync }
      procedure SetMode(Mode : boolean);

      { Returns true if the connecton looks viable, either (fileSync) it has right files
        there and write access OR (NetSync) network answers somehow (?). This is about finding an existing
        connection. If it looks OK, will attempt to get a list of newer notes from Server and will
        read local manifest. Returns False if we are not setup to proceed.}
      function TestConnection() : boolean;

      { Join a sync, either Net or File. We might be the first connection so do init stuff,
        if we already have a SyncID, we'll offer it, if it does not match, tahts an error,
        else we are a new join and expect to be passed back one if we are welcome. }
      function JoinSync(SyncID : string = '') : boolean;

        { Do actual sync, but if TestRun=True just report on what you'd do. }
      function StartSync() : boolean;

      destructor Destroy(); override;

  end;

implementation

{ TSync }

uses laz2_DOM, laz2_XMLRead, Trans, TransFile, TransNet, LazLogger;

var
    Transport : TTomboyTrans;

{constructor TSync.Creator(const CNotesDir, CConfigDir: string; const CRev : integer);
begin
    inherited Create();
    NotesDir := CNotesDir;
    ConfigDir := CConfigDir;
    CurrRev := CRev;
end;  }

    { Here we assemble all the info  we nee to present a good clash
      report to end user. We access directly the remote file in the case
      of FileSync, for Network Sync maybe we will have to download a copy
      to, /temp/ or whatever ?  Need to know :
      * ID
      * Title
      * last-change-date for each
      * full file path to both
    }

function TSync.ProceedWith(FullRemoteFileName : ANSIString) : TClashDecision;
var
    ClashRec : TClashRecord;
    ChangeDate : ANSIString;
begin

    // ToDo - must fill in this info ?

    //ClashRec.NoteID := FileID;
    //ClashRec.Title:= GetNoteTitle(LocalPath(FileID, ''));
    //GetNoteChangeGMT(LocalPath(FileID, ''), ChangeDate);
    //ClashRec.LocalLastChange := ChangeDate;
    //GetNoteChangeGMT(RemotePath(FileID, Rev), ChangeDate);
    //ClashRec.ServerLastChange := ChangeDate;
    ClashRec.ServerFileName := FullRemoteFileName;
    //ClashRec.LocalFileName := LocalPath(FileID, '');
    Result := ProceedFunction(Clashrec);
end;

procedure TSync.SetMode(Mode: boolean);
begin
    if Mode then Transport := TFileSync.Create
    else Transport := TNetSync.Create;
    Transport.RemoteAddress:= SyncAddress;
end;

function TSync.TestConnection(): boolean;
begin
    if LocalMetaData <> Nil then
        LocalMetaData.Free;
    LocalMetaData := TNoteInfoList.Create;
    if not ReadLocalManifest() then exit(False);    // Error in local mainfest
    if DebugMode then begin
        debugln('CurrRev=' + inttostr(CurrRev) + '   Last Sync=' + LocalLastSyncDateSt
                        + '   Local Entries=' + inttostr(LocalMetaData.Count));
        debugln('Config=' + ConfigDir + ' NotesDir=' + NotesDir);
    end;
    if LocalLastSyncDateSt = '' then exit(False);   // we don't have an existing connection setup
    if RemoteMetaData <> Nil then
        RemoteMetaData.Free;
    RemoteMetaData := TNoteInfoList.Create;
    if not Transport.GetNewNotes(RemoteMetaData, CurrRev) then begin
      ErrorString := Transport.ErrorString;
      exit(False);
    end;
    // DisplayNoteInfo(RemoteMetaData);
    Result := True;
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

function TSync.JoinSync(SyncID: string): boolean;
begin
    Result := True;
end;

procedure TSync.CheckRemoteDeletes();
var
    Index : integer;
    PNote : PNoteInfo;
begin
    // Iterate over LocalMetaData looking for notes listed a prev synced
    // but are not listed in RemoteMetaData. We'll add a entry in RemoteMetaData
    // for any we find.
    for Index := 0 to LocalMetaData.Count -1 do begin
        if not LocalMetaData.Items[Index]^.Deleted then
            if nil = RemoteMetaData.FindID(LocalMetaData.Items[Index]^.ID) then begin   // That is, we did not find it
                new(PNote);
                PNote^.ID:= LocalMetaData.Items[Index]^.ID;
                PNote^.Title := LocalMetaData.Items[Index]^.Title;                      // I think we know title, useful debug info here....
                PNote^.Action := DeleteLocal;                                       // Was deleted elsewhere, do same here.
                RemoteMetaData.Add(PNote);
            end;
    end;
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
            PNote := RemoteMetaData.FindID(LocalMetaData.Items[i]^.ID);
            if PNote <> nil then
                PNote^.Action := DeleteRemote;
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
    for I := 0 to RemoteMetaData.Count -1 do begin
        // if RemoteMetaData.Items[I]^.Rev <= self.CurrRev then continue;  // skip old notes - why ???? better to check if they have changed locally
        ID := RemoteMetaData.Items[I]^.ID;
        if ID ='b53b2b13-2e4d-4eaf-833c-6f117c024bcb' then debugln('---- got it -----');
        if LocalNoteExists(ID, LocCDate) then begin                        // local copy exits.
            LocalNoteExists(ID, LocCDate, True);
            if LocCDate = RemoteMetaData.Items[I]^.LastChange then begin   // We have identical note locally
                RemoteMetaData.Items[I]^.Action:=Nothing;
                continue;
            end;
            // Dates don't match, must do something !
            if GetGMTFromStr(LocCDate) > LocalLastSyncDate then begin      // Ahh, local version changed since last sync !
                if RemoteMetaData.Items[I]^.Rev <= self.CurrRev then    // Remote version unchanged, easy - Flow chart does NOT show this test !!!!
                    RemoteMetaData.Items[I]^.Action:= UpLoad
                else RemoteMetaData.Items[I]^.Action := Clash;               // resolve later
                {if DealWithClash(ID, CDate, RemoteMetaData.Items[I]^.LastChange) then
                    RemoteMetaData.Items[I]^.Action:= Upload
                else RemoteMetaData.Items[I]^.Action:= Download;}
            end else RemoteMetaData.Items[I]^.Action:= Download;
        end else begin      // Not here but maybe we deleted it previously ?
            Pnote := Self.LocalMetaData.FindID(ID);
            if PNote <> Nil then begin
                if PNote^.Deleted then
                   RemoteMetaData.Items[I]^.Action:=DeleteRemote;       // I have deleted that already.
            end else RemoteMetaData.Items[I]^.Action:=Download;         // its a new note from elsewhere
        end;
        if RemoteMetaData.Items[I]^.Action = Unset then debugln('---- missed one -----');
    end;
end;

function TSync.StartSync(): boolean;
begin
    LocalLastSyncDate :=  GetGMTFromStr(LocalLastSyncDateSt);
    if (LocalLastSyncDate > now()) or (LocalLastSyncDate < (Now() - 36500))  then begin
        // TDateTime has integer part no. of days, fraction part is fraction of day.
        // we have here in the future or more than 100years ago - Fail !
        ErrorString := 'Invalid last sync date in local manifest';
        exit(False);
    end;
    Result := CheckAgainstRemote(); // don't do anything at this stage, just assign actions.
    CheckRemoteDeletes();           // if we have notes prev synced but not now in RemoteMetaData, it was deleted
                                    // by another client, must (backup and) delete here. Mark these as DeleteLocal.
    CheckLocalDeletes();            // Things mentioned as deleted in local manifest, delete remote copy too.
                                    // Mark these as DeleteRemote. Does not make new rev.
    if DebugMode then self.DisplayNoteInfo(RemoteMetaData);
    // if TestMode then exit();
    // DoDownLoads();
    // DoUploads();
    Result := True;
end;

function TSync.LocalNoteExists(const ID : string; out CDate : string; GetDate : boolean = false) : Boolean;
var
	Doc : TXMLDocument;
	Node : TDOMNode;
    LastChange : string;
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
    if RemoteMetaData <> Nil then
        RemoteMetaData.Free;
     if ReportList <> Nil then
        ReportList.Free;
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
begin
    Result := true;
    if SkipFile then exit();
    if not FileExists(ConfigDir + 'manifest.xml') then begin
        LocalLastSyncDateSt := '';
        CurrRev := 0;
        exit(True);                 // Its not an error, just never synced before
    end;
    LocalMetaData := TNoteInfoList.Create;
    try
    	try
    		ReadXMLFile(Doc, ConfigDir + 'manifest.xml');
            Node := Doc.DocumentElement.FindNode('last-sync-date');
        	LocalLastSyncDateSt := Node.FirstChild.NodeValue;
            Node := Doc.DocumentElement.FindNode('last-sync-rev');
            try
        		CurrRev := strtoint(Node.FirstChild.NodeValue);     // Convert error ??
			except
                    on EObjectCheck do begin                        // mac does this
                        ErrorString := 'Error in local mainfest, check RevNo';
                        CurrRev := 0;
                        LocalLastSyncDateSt := '';
                        exit(False);
                    end;
                    on EAccessViolation do begin	// Lin does this
                        ErrorString := 'Error in local mainfest, check RevNo';
                        CurrRev := 0;
                        LocalLastSyncDateSt := '';
                        exit(False);
                    end;
            end;
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


end.

