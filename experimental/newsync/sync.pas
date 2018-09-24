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

  Down to detect any notes that are new since last sync, we scan over notes that exist
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



}

interface
uses
    Classes, SysUtils, SyncUtils;





type                       { ----------------- T S Y N C --------------------- }

  { TSync }

  TSync = class

  private
      //fDoFileSync: boolean;
      //fDoNetSync: boolean;

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

	  function DoDeletes(): boolean;

      // We call transport for all the notes in the list we need download. Transport does most of the work.
	  function DoDownloads(): boolean;

            { Uploads any files it finds necessary in NoteMetaData. Returns false
              if anything goes wrong (such as a file error) }
	  function DoUploads(): boolean;
	  function LoadMetaData(): boolean;

            { Searches list for any clashes, refering each one to user. Done after
              list is filled out in case we want to ask user for general instrucions }
      procedure ProcessClashes();

        // Checks if local note exists, optionally returning with its last change date.
      function LocalNoteExists(const ID : string; out CDate: string; GetDate: boolean=false): Boolean;

        // Call this when we are resolving a sync clash. Note : not possible results make sense !
      function ProceedWith(const ID, FullRemoteFileName: ANSIString): TSyncAction;

      // Reads through Local Manifest file, filling out LocalMetaData, LastSyncDateSt and CurrRev.
      function ReadLocalManifest(SkipFile: boolean=False): boolean;


            { Writes a local mainfest file. Assumes NoteMetaData contains valid data about
              new Rev numbers and for uploads, last-change-date. If WriteOK is false then
              don't rev number and don't mention new notes in local manifest.}
      function WriteLocalManifest(const WriteOK, NewRev : boolean) : boolean;

            { We write a remote manifest out localy if we have any uploads to do.
              Then call Transport to deal with it. Writing it locally is fast
              and we get to check for and isolate any data errors. This is where
              Initially written to $CONFIG/manifest.xml-remote }
	  function WriteRemoteManifest(out NewRev: boolean): boolean;

   public
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
                // A report on what happened (or we make out happened) during a sync
        //ReportList : TSyncReportList;

                { Reports on contents of a created and filled list }
	    procedure ReportMetaData(out UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing: integer);

                { True=FileSync, False=Network Sync }
        procedure SetMode(Mode : boolean);

                { Checks NoteMetaData for valid Actions }
        function CheckMetaData() : boolean;

            { Returns true if the connecton looks viable, either (fileSync) it has right files
              there and write access OR (NetSync) network answers somehow (?). This is about finding
              an existing connection. IFF ret SyncReady can proceed to StartSync}
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

uses laz2_DOM, laz2_XMLRead, Trans, TransFile, TransNet, LazLogger;

var
    Transport : TTomboyTrans;

constructor TSync.Create();
begin
    NoteMetaData := TNoteInfoList.Create;
    LocalMetaData := TNoteInfoList.Create;
    Transport := nil;
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
    if Mode then Transport := TFileSync.Create
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
        if not (NoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit, SyDownLoad, SyDeleteLocal, SyDeleteRemote]) then
           Debugln('ERROR note not assigned ' + NoteMetaData[Index]^.ID + ' '
                   + NoteMetaData.ActionName(NoteMetaData[Index]^.Action));
        result := False;
    end;
end;


function TSync.TestConnection(): TSyncAvailable;
var
    ServerID : string;
begin
    {if LocalMetaData <> Nil then
        LocalMetaData.Free;
    LocalMetaData := TNoteInfoList.Create; }
    if not ReadLocalManifest() then exit(SyncNoLocal);    // Error in local mainfest
    LocalLastSyncDate :=  GetGMTFromStr(LocalLastSyncDateSt);
    if (LocalLastSyncDate > now()) or (LocalLastSyncDate < (Now() - 36500))  then begin
        // TDateTime has integer part no. of days, fraction part is fraction of day.
        // we have here in the future or more than 100years ago - Fail !
        ErrorString := 'Invalid last sync date in local manifest';
        exit(SyncXMLError);
    end;
    if DebugMode then begin
        debugln('CurrRev=' + inttostr(CurrRev) + '   Last Sync=' + LocalLastSyncDateSt
                        + '   Local Entries=' + inttostr(LocalMetaData.Count));
        debugln('Config=' + ConfigDir + ' NotesDir=' + NotesDir);
    end;
    if LocalLastSyncDateSt = '' then exit(SyncNoLocal);   // we don't have an existing connection setup
    Result := Transport.TestTransport(ServerID);
    if Result <> SyncReady then begin
    //if not Transport.GetNewNotes(NoteMetaData, CurrRev, ServerID) then begin
      ErrorString := Transport.ErrorString;
      exit;
    end;
    if ServerID <> self.LocalLastSyncID then begin
        ErrorString := 'ServerID Mismatch';
        exit(SyncXMLError);
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
        // if NoteMetaData.Items[I]^.Rev <= self.CurrRev then continue;  // skip old notes - why ???? better to check if they have changed locally
        ID := NoteMetaData.Items[I]^.ID;
        // if ID ='b53b2b13-2e4d-4eaf-833c-6f117c024bcb' then debugln('---- got it -----');
        if LocalNoteExists(ID, LocCDate) then begin                        // local copy exits.
            LocalNoteExists(ID, LocCDate, True);
            if LocCDate = NoteMetaData.Items[I]^.LastChange then begin   // We have identical note locally
                NoteMetaData.Items[I]^.Action:=SyNothing;
                // Maybe, check that revision numbers also match ?
                continue;
            end;
            // Dates don't match, must do something !
            if GetGMTFromStr(LocCDate) > LocalLastSyncDate then begin      // Ahh, local version changed since last sync !
                if NoteMetaData.Items[I]^.Rev <= self.CurrRev then    // Remote version unchanged, easy - Flow chart does NOT show this test !!!!
                    NoteMetaData.Items[I]^.Action:= SyUpLoadEdit
                else NoteMetaData.Items[I]^.Action := SyClash;               // resolve later
                {if DealWithClash(ID, CDate, NoteMetaData.Items[I]^.LastChange) then
                    NoteMetaData.Items[I]^.Action:= Upload
                else NoteMetaData.Items[I]^.Action:= Download;}
            end else NoteMetaData.Items[I]^.Action:= SyDownload;
        end else begin      // Not here but maybe we deleted it previously ?
            Pnote := Self.LocalMetaData.FindID(ID);
            if PNote <> Nil then begin
                if PNote^.Deleted then
                   NoteMetaData.Items[I]^.Action:=SyDeleteRemote;       // I have deleted that already.
            end else NoteMetaData.Items[I]^.Action:=SyDownload;         // its a new note from elsewhere
        end;
        if NoteMetaData.Items[I]^.Action = SyUnset then debugln('---- missed one -----');
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
    try
        Uploads := TstringList.Create;
        for Index := 0 to NoteMetaData.Count -1 do begin
            if NoteMetaData.Items[Index]^.Action in [SyUploadEdit, SyUploadNew] then
               Uploads.Add(NoteMetaData.Items[Index]^.ID);
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
	{if NoteMetaData <> Nil then
	    NoteMetaData.Free;
    NoteMetaData := TNoteInfoList.Create;  }
    if not Transport.GetNewNotes(NoteMetaData, CurrRev) then begin
          ErrorString := Transport.ErrorString;
          exit(False);
    end;
    Result := CheckAgainstRemote(); // don't do anything at this stage, just assign actions.
    CheckRemoteDeletes();           // if we have notes prev synced but not now in NoteMetaData, it was deleted
                                    // by another client, must (backup and) delete here. Mark these as DeleteLocal.
    CheckLocalDeletes();            // Things mentioned as deleted in local manifest, delete remote copy too.
                                    // Mark these as DeleteRemote. Does not make new rev.
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
    if not DoDownLoads() then exit(False);      // DoDownloads() will tell us what troubled it.
    if WriteRemoteManifest(NewRev) then begin;       // false is an ERROR !
        if (DoDeletes() and DoUploads()) then begin
	        WriteLocalManifest(true, NewRev);
	    end else begin
            WriteLocalManifest(false, false);       // write a recovery local manifest. Downloads only noted.

	end;
    end;
    Result := True;
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
begin
    Result := true;
    if SkipFile then exit();
    // LocalMetaData := TNoteInfoList.Create;      // created before called
    if not FileExists(ConfigDir + 'manifest.xml') then begin
        LocalLastSyncDateSt := '';
        CurrRev := 0;
        exit(True);                 // Its not an error, just never synced before
    end;
    try
    	try
    		ReadXMLFile(Doc, ConfigDir + 'manifest.xml');
                Node := Doc.DocumentElement.FindNode('last-sync-date');
                LocalLastSyncDateSt := Node.FirstChild.NodeValue;
                Node := Doc.DocumentElement.FindNode('server-id');
                LocalLastSyncID := Node.FirstChild.NodeValue;
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

function TSync.WriteRemoteManifest(out NewRev : boolean): boolean;
var
    OutFile: TextFile;
    Index : integer;
begin
    result := true;
    NewRev := False;
    for Index := 0 to NoteMetaData.Count - 1 do begin
        if NoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit, SyDeleteRemote] then begin
            NewRev := True;
            break;                  // one is enough, need a new manifest file !
		end;
    end;
    if Not NewRev then exit(true);      // exit cos no need for new Man, ret true cos no file error,
    // if not WriteOK then exit(false);
    // if not NewRev then exit(false);
    AssignFile(OutFile, ConfigDir + 'manifest.xml-remote');
    try
	    try
		    Rewrite(OutFile);
	        writeln(OutFile, '<?xml version="1.0" encoding="utf-8"?>');
            write(OutFile, '<sync revision="' + inttostr(Transport.RemoteServerRev + 1));
            writeln(OutFile, '" server-id="' + Transport.RemoteServerID + '">');
	        for Index := 0 to NoteMetaData.Count - 1 do begin
                if NoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit, SyDownLoad, SyNothing] then begin
	                    write(OutFile, '  <note id="' + NoteMetaData.Items[Index]^.ID + '" rev="');
	                    writeln(OutFile, inttostr(NoteMetaData.Items[Index]^.Rev) + '" />');
				end;
			end;
            writeln(OutFile, '</sync>');
		finally
            CloseFile(OutFile);
		end;
        except
          on E: EInOutError do begin
              Debugln('File handling error occurred. Details: ' + E.Message);
              exit(false);      // file error !
    	  end;
    	end;
        // do a safe version of this -
        if not TestRun then
           result := Transport.DoRemoteManifest(ConfigDir + 'manifest.xml-remote');
end;

function TSync.WriteLocalManifest(const WriteOK, NewRev : boolean ): boolean;
{ We try and provide some recovery from a fail to write to remote repo. It should
  not happen but ... If WriteOk is false we write back local manifest that still
  mentions the previous deleted files and does not list locally new and changed
  files. Such files retain their thier previous rev numbers. Test ! }
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
                        case NoteMetaData[Index]^.Action of
                            SyUpLoadEdit, SyUpLoadNew : write(OutFile, inttostr(NoteMetaData[Index]^.Rev +IncRev));
                            SyDownload, SyNothing : write(OutFile, inttostr(NoteMetaData[Index]^.Rev));
						end;
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
end;


end.

