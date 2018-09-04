unit sync;

{$mode objfpc}{$H+}

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

      // True says keep local, false says use remote version
      function DealWithClash(const ID, CDate, RemoteCDate: string): boolean;

      // Just a debug procedure, dumps (some) contents of a list to console
      procedure DisplayNoteInfo(meta: TNoteInfoList);

        // Checks if local note exists, optionally returning with its last change date.
      function LocalNoteExists(const ID : string; out CDate: string; GetDate: boolean=false): Boolean;

      // Reads through Local Manifest file, filling out LocalMetaData, LastSyncDateSt and CurrRev.
      function ReadLocalManifest(SkipFile: boolean=False): boolean;

  public
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
    DisplayNoteInfo(RemoteMetaData);
    Result := True;
end;

procedure TSync.DisplayNoteInfo(meta : TNoteInfoList);
var
    I : Integer;
    St : string;
begin
    for I := 0 to Meta.Count -1 do begin
        St := ' UNKNOWN ';
        case Meta.Items[i]^.Action of
            Nothing : St := ' Nothing ';
            Upload  : St := ' Upload ';
            Download: St := ' Download ';
            DeleteLocal  : St := ' DeleteLocal ';
            DeleteRemote : St := ' DeleteRemote ';
        end;
        debugln(Meta.Items[I]^.ID + ' ' + inttostr(Meta.Items[i]^.Rev) + St
            + Meta.Items[i]^.LastChange);
    end;
end;

function TSync.JoinSync(SyncID: string): boolean;
begin
    Result := True;
end;


function TSync.DealWithClash(const ID, CDate, RemoteCDate : string) : boolean;
begin
    Result := True;
end;

function TSync.CheckAgainstRemote() : boolean;
var
    I : integer;
    ID : string;    // to make it a bit easier to read souce
    PNote : PNoteInfo;
    CDate : string;
begin
    Result := True;
    for I := 0 to RemoteMetaData.Count -1 do begin
        if RemoteMetaData.Items[I]^.Rev <= self.CurrRev then continue;      // skip old notes
        ID := RemoteMetaData.Items[I]^.ID;
        if LocalNoteExists(ID, CDate) then begin    // local copy exits.
            LocalNoteExists(ID, CDate, True);
            if CDate = RemoteMetaData.Items[I]^.LastChange then begin        // We have identical note locally
                RemoteMetaData.Items[I]^.Action:=Nothing;
                continue;
            end;
            if GetGMTFromStr(CDate) > LocalLastSyncDate then begin // Ahh, its been changed since last sync !
                if DealWithClash(ID, CDate, RemoteMetaData.Items[I]^.LastChange) then
                    RemoteMetaData.Items[I]^.Action:= Upload
                else RemoteMetaData.Items[I]^.Action:= Download;
            end else RemoteMetaData.Items[I]^.Action:= Download;
        end else begin      // Not here but maybe we deleted it previously ?
            Pnote := Self.LocalMetaData.FindID(ID);
            if PNote <> Nil then begin
                if PNote^.Deleted then
                   RemoteMetaData.Items[I]^.Action:=DeleteRemote;       // I have deleted that already.
            end else RemoteMetaData.Items[I]^.Action:=Download;         // its a new note from elsewhere
        end;
    end;
end;

function TSync.StartSync(): boolean;
begin
    LocalLastSyncDate :=  GetGMTFromStr(LocalLastSyncDateSt);
    if (LocalLastSyncDate > now()) or (LocalLastSyncDate < (Now() - 36500))  then begin
        // TDateTime has integer part no. of days, fraction part is frction of day.
        // we have here in the future or more than 100years ago - Fail !
        ErrorString := 'Invalid last sync date in local manifest';
        exit(False);
    end;
    CheckAgainstRemote();
    // RemoteDeletes();
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

