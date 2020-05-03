unit sync;
{
    A Unit to manage tomboy-ng sync behaviour.
    Copyright (C) 2018 David Bannon
    See attached licence file.
}

{$mode objfpc}{$H+}

{

HISTORY
    2020/05/04  Complete re-structure
    2018/10/18  Memory leak in CheckUsingLCD(), StartSync() should not call processClashes()
                during a TestRun. Only during real thing.
    2018/10/22  CheckMetaData() was returning wrong value.
    2018/10/25  Much testing, support for Tomdroid.
    2018/10/28  Much tweaking and bug fixing.
    2018/10/29  Tell TB_Sdiff about note title before showing it.
    2018/11/03  Call checkmetadata before resolving clashes.
    2018/11/04  No longer call MarkNoteReadOnly as we now rely on searchForm.ProcessSyncUpdates
    2018/11/05  Now set Notemeatdata LCD to LCD of local note when Clash handler sets SyUpLoadEdit
    2018/11/25  Added DeleteFromLocalManifest(), called from search unit, TEST !
    2018/06/05  Change to doing Tomboy's sync dir names, rev 431 is in ~/4/341
    2019/07/19  Escape ' and " when using Title as an attribute in local manifest.
    2020/02/27  Better detect when we try to sync and don't have a local manifest, useful for Tomdroid, check normal !
}

interface
uses
    Classes, SysUtils, SyncUtils;


type                       { ----------------- T S Y N C --------------------- }

  { TSync }

  TSync = class

  private

      TransportMode : TSyncTransport;

      // Data about notes in remote manifest that may need be copied down
      NoteMetaData : TNoteInfoList;
      // Data obtained from Local Manifest. Might be empty.....
      LocalMetaData : TNoteInfoList;

      { Default action to take in case of clash }
      ClashAction : TSyncAction;

      function getManifestName() : String;

      {   Asks Transport for a list of the notes remote server knows about. }
      function LoadRemoteNotes() : boolean;

      {   Load local notes in memory }
      function LoadLocalNotes() : boolean;

      { Find Remotes notes not in local repo }
      procedure FindNewRemoteNotes();

      { Find Local notes not in remote repo }
      procedure FindNewLocalNotes();

      { Find Server notes that have been deleted locally }
      procedure FindDeletedLocalNotes();

      { Find Local notes that have been deleted remotely }
      procedure FindDeletedServerNotes();

      { Set actions to be done that are systemic }
      procedure SetSystemicActions();

      { Searches list for any clashes, refering each one to user. Done after
        list is filled out in case we want to ask user for general instrucions }
      procedure ProcessClashes();

      { Looks at a clash and determines if its an up or down depending on
        possible higher order TSyncActions such as newer, older }
      function ResolveAction(const SNote, LNote : PNoteInfo) : TSyncAction;

      { Returns true if the passed dates are pretty close, see code for just how close }
      function DatesCompare(const DS1, DS2: TDateTime): double;

      { We call transport for all the notes in the list we need download.
        Transport does most of the work. In TestMode, does nothing }
      function DoDownloads(): boolean;

      { Backs up and then removes Notes marked as SyDeleteLocal }
      function DoDeleteLocal(): boolean;

      { Select and call Transport to push changes }
      function PushChanges(): boolean;

      { We write a remote manifest out localy if we have any uploads or to handle
              the delete from server a note that was deleted locally to do. Then, if
              TestMode is false, call Transport to deal with it. Writing it locally is fast
              and we get to check for and isolate any data errors. Initially
              written to $CONFIG/manifest.xml-remote and copied (moved ?).}
      function DoManifest(): boolean;

      // Just a debug procedure, dumps (some) contents of a list to console
      procedure DisplayNoteInfo(const meta: TNoteInfoList; const ListTitle : string);


   public

      ErrorString : string;
      NotesDir : String;
      ConfigDir : String;

      DeletedList, DownList, GridReportList : TStringList;

      { It will be called if the sync process finds notes
           that are not systematically resolved }
      ClashFunction : TClashFunction;



       { Reports on contents of a created and filled list }
       procedure ReportMetaData(out UpNew, UpEdit, Down, DelLoc, DelRem, CreateCopy, DoNothing, Undecided: integer);

       { Selects a Trans layer, adjusts config dir, }
       function SetTransport(Mode : TSyncTransport) : TSyncAvailable;

       { May return : SyncXMLError, SyncNoRemoteDir, SyncNoRemoteWrite,
              SyncNoRemoteRepo, SyncBadRemote, SyncMismatch. Checks if the connecton
              looks viable, either (fileSync) it has right files there and write access
              OR (NetSync) network answers somehow (?). Reads local manifest if
              RepoAction=RepoUse and compares ts serverID with one found by
              Trans.testConnection. SyncReady means we can proceed to StartSync, else must
              do something first, setup new connect, consult user etc.}
       function TestConnection() : TSyncAvailable;

       { Do actual sync, but if TestRun=True just report on what you'd do.
              Assumes a Transport has been selected and remote address is set.
              We must already be a member of this sync, ie, its remote ID is recorded
              in our local manifest. }
       function StartSync(isTest : boolean) : boolean;

       function GetNoteTitle(const ID : ANSIString) : ANSIString;

       constructor Create();
       destructor Destroy(); override;
  end;

implementation

{ TSync }

uses laz2_DOM, laz2_XMLRead, Trans, TransFile, TransAndroid, TransNext, LazLogger, LazFileUtils,
    FileUtil, Settings, Tomdroid;

var
    Transport : TTomboyTrans;
    //SyncOnce : boolean;

constructor TSync.Create();
begin
    NoteMetaData := TNoteInfoList.Create;
    LocalMetaData := TNoteInfoList.Create;
    DeletedList := TStringList.Create;
    DownList := TStringList.Create;
    GridReportList := TStringList.Create;
    Transport := nil;
end;

destructor TSync.Destroy();
begin
    FreeandNil(LocalMetaData);
    FreeandNil(NoteMetaData);
    FreeandNil(Transport);
    FreeandNil(DeletedList);
    FreeandNil(DownList);
    inherited Destroy();
end;

function TSync.getManifestName() : String;
begin
    Result := Format('%s%s-%s-manifest.xml',[NotesDir, Transport.getPrefix() , Transport.ServerID ]);
end;


{ =================   E X T E R N A L   C A L L   O U T S ===========================}


procedure TSync.ReportMetaData(out UpNew, UpEdit, Down, DelLoc, DelRem, CreateCopy, DoNothing, Undecided : integer);
var
    Index : integer;
begin
    UpNew := 0; UpEdit := 0; Down := 0; Undecided := 0;
    DelLoc := 0; DelRem := 0; DoNothing := 0; CreateCopy :=0;

    for Index := 0 to NoteMetaData.Count -1 do
    begin
        case NoteMetaData.Items[Index]^.Action of
            SynUpLoadNew : inc(UpNew);
            SynCopy : inc(CreateCopy);
            SynUpLoadEdit : inc(UpEdit);
            SynDownLoad : inc(Down);
            SynDeleteLocal : inc(DelLoc);
            SynDeleteRemote : inc(DelRem);
            SynNothing : inc(DoNothing);
            SynUnset : inc(Undecided);
	end;
    end;
end;


function TSync.GetNoteTitle(const ID : ANSIString) : ANSIString;
var
    note : PNoteInfo;
begin
    Result := '--Error--';

    note := NoteMetaData.FindID(ID);
    if(note <> nil) then exit(note^.Title);

    note := LocalMetaData.FindID(ID);
    if(note <> nil) then exit(note^.Title);
end;


procedure TSync.DisplayNoteInfo(const meta : TNoteInfoList; const ListTitle : string);
var
    I : Integer;
    St : string;
begin
    debugln('-----------list dump for ' + ListTitle);
    for I := 0 to Meta.Count -1 do begin
        St := ' ' + inttostr(Meta.Items[i]^.Rev);
        while length(St) < 5 do St := St + ' ';
        // St := Meta.ActionName(Meta.Items[i]^.Action);
        debugln(Meta.Items[I]^.ID + St + Meta.ActionName(Meta.Items[i]^.Action)
            + Meta.Items[i]^.LastChange + '   ' + Meta.Items[I]^.Title);
    end;
end;


{ =====================   D A T A    C H E C K I N G    M E T H O D S  ============= }

function TSync.DatesCompare(const DS1, DS2 : TDateTime) : double;
var
    Margin : TDateTime = 0.00001;      // a second

begin
    debugln('DS1 '+DateTimeToStr(DS1));
    debugln('DS1+ '+DateTimeToStr(DS1 + Margin));
    debugln('DS1- '+DateTimeToStr(DS1 - Margin));
    debugln('DS2 '+DateTimeToStr(DS2));
    debugln('DS2+ '+DateTimeToStr(DS2 + Margin));
    debugln('DS2- '+DateTimeToStr(DS2 - Margin));

    if (DS1 > DS2 + Margin) then exit(DS1-DS2);
    if (DS2 > DS1 + Margin) then exit(DS1-DS2);

    Result := 0;
end;


procedure TSync.FindDeletedServerNotes();
var
    PNote : PNoteInfo;
    ID : String;
    i,c : integer;
begin
    c := 0;
    for i := 0 to LocalMetaData.Count -1 do
    begin
        if(LocalMetaData.Items[i]^.Deleted) then continue; // Do not consider deleted
        if(LocalMetaData.Items[i]^.Rev<0) then continue; // Do not consider never sync

        ID := LocalMetaData.Items[i]^.ID;
        PNote := NoteMetaData.FindID(ID);
        if(PNote <> Nil) then continue;

        new(PNote);
        PNote^.Action:=SynDeleteLocal;
        PNote^.ID := ID;
                  // Whatever the content, we will backup/delete it
        NoteMetaData.Add(PNote);
        inc(c);
    end;
    debugln('Found '+IntToStr(c)+' deleted server notes');
end;

procedure TSync.FindDeletedLocalNotes();
var
    ID : String;
    PNote : PNoteInfo;
    c,i : integer;
begin
    c:=0;
    for i := 0 to LocalMetaData.Count -1 do
    begin
        if(not LocalMetaData.Items[i]^.Deleted) then continue; // Consider only deleted

        ID := LocalMetaData.Items[i]^.ID;
        PNote := NoteMetaData.FindID(ID);
        if(PNote = Nil) then continue;
        inc(c);
        PNote^.Action:=SynDeleteRemote;
    end;
    debugln('Found '+IntToStr(c)+' deleted local notes');
end;

procedure TSync.FindNewLocalNotes();
var
    ID : String;
    c,i : integer;
    PNote : PNoteInfo;
begin
    c :=0;
    for i := 0 to LocalMetaData.Count -1 do
    begin
        if(LocalMetaData.Items[i]^.Deleted) then continue; // Do not consider deleted
        ID := LocalMetaData.Items[i]^.ID;
        PNote := NoteMetaData.FindID(ID);
        if(PNote <> Nil) then continue;

        new(PNote);
        PNote^ := LocalMetaData.Items[i]^;
        PNote^.Action:=SynUploadNew;
        NoteMetaData.Add(PNote);
        inc(c);
    end;
    debugln('Found '+IntToStr(c)+' new local notes');
end;

procedure TSync.FindNewRemoteNotes();
var
    ID : String;
    c,i : integer;
    PNote : PNoteInfo;
begin
    c := 0;
    for i := 0 to NoteMetaData.Count -1 do
    begin
         if(NoteMetaData.Items[i]^.Action <> SynUnset) then continue; // Don't look at notes already scrutinized

         ID := NoteMetaData.Items[i]^.ID;
         PNote := LocalMetaData.FindID(ID);
         if(PNote = Nil) then continue;

         NoteMetaData.Items[i]^.Action := SynDownLoad;
         inc(c);
    end;
    debugln('Found '+IntToStr(c)+' new remote notes');
end;

procedure TSync.SetSystemicActions();
var
    ID : String;
    i : integer;
    LNote, SNote : PNoteInfo;
begin
    for i := 0 to NoteMetaData.Count -1 do
    begin
        SNote := NoteMetaData.Items[i];
        if(SNote^.Action <> SynUnset) then continue; // Don't look at notes already scrutinized

        ID := SNote^.ID;
        LNote := LocalMetaData.FindID(ID); // Must be not null !

        if(LNote^.Rev > SNote^.Rev) then continue; // This is abnormal -> Must check manually

        if((DatesCompare(LNote^.LastSyncGMT,SNote^.LastChangeGMT)>0) and (DatesCompare(LNote^.LastSyncGMT,LNote^.LastChangeGMT)>0)) then
        begin
           if((DatesCompare(LNote^.LastSyncGMT,SNote^.LastMetaChangeGMT)>0 ) and (DatesCompare(LNote^.LastSyncGMT, LNote^.LastMetaChangeGMT)>0 ))
           then SNote^.Action := SynNothing
           else if(DatesCompare(LNote^.LastSyncGMT, SNote^.LastMetaChangeGMT)>0 )
                then SNote^.Action := SynUploadEdit
                else if(DatesCompare(LNote^.LastSyncGMT, LNote^.LastMetaChangeGMT)>0 )
                     then SNote^.Action := SynDownload
                     else SNote^.Action := SynNothing;
        end;

        if(DatesCompare(LNote^.LastSyncGMT, SNote^.LastChangeGMT)>0 ) // i.e. not (LNote^.LastSyncGMT > LNote^.LastChangeGMT ))
        then SNote^.Action := SynUploadEdit
        else if(DatesCompare(LNote^.LastSyncGMT, LNote^.LastChangeGMT)>0 ) // i.e. not (LNote^.LastSyncGMT > SNote^.LastChangeGMT )
             then SNote^.Action := SynDownload;

        if((LNote^.Rev < SNote^.Rev) and (SNote^.Action <> SynDownload))  then // This is abnormal -> Must check manually
        begin
           SNote^.Action := SynUnset;
           continue;
        end;
    end;
end;


function TSync.ResolveAction(const SNote, LNote : PNoteInfo) : TSyncAction;
begin
    Result := ClashAction;

    case ClashAction of
        SynAllLocal : exit(SynUpLoadEdit);
        SynAllRemote : exit(SynDownLoad);
        SynAllCopy : exit(SynCopy);

        SynAllNewest : if (DatesCompare(LNote^.LastChangeGMT,SNote^.LastChangeGMT)>0)
                     then exit(SynUploadEdit)
                     else exit(SynDownLoad);

        SynAllOldest : if (DatesCompare(LNote^.LastChangeGMT,SNote^.LastChangeGMT)<0)
                     then exit(SynUploadEdit)
                     else exit(SynDownLoad);

        SynDownLoad : ClashAction := SynUnset;
        SynUpLoadEdit : ClashAction := SynUnset;
    end;
end;


procedure TSync.ProcessClashes();
var
    ClashRec : TClashRecord;
    Index : integer;
    remote,local : PNoteInfo;
begin

    ClashAction := SynUnset;

    case Sett.SyncOption of
         UseLocal : ClashAction := SynAllLocal;
         UseServer : ClashAction := SynAllRemote;
         MakeCopy : ClashAction := SynAllCopy;
    end;

    for Index := 0 to NoteMetaData.Count -1 do
    begin
        remote := NoteMetaData.Items[Index];
        if remote^.Action = SynUnset then
        begin
           local := LocalMetaData.FindID(remote^.ID);
           if ClashAction = SynUnset then
           begin
                ClashRec.RemoteNote := remote;
                ClashRec.LocalNote := local;
                ClashAction := ClashFunction(Clashrec);
           end;
           remote^.Action := ResolveAction(remote, local);
        end;
    end;
end;


{ ========================  N O T E   M O V E M E N T    M E T H O D S ================}

function TSync.DoDownloads() : boolean;
var
    i : integer;
    dest,backupdir,backup,ID : String;
    f : TextFile;
    note : PNoteInfo;
    GUID : TGUID;
begin
    debugln('DoDownloads');

    backupdir := NotesDir + 'Backup' + PathDelim;

    if not DirectoryExists(backupdir) then
        if not ForceDirectory(backupdir) then
        begin
            ErrorString := 'Failed to create Backup directory.';
            exit(False);
        end;

    for i := 0 to NoteMetaData.Count-1 do
    begin
        if NoteMetaData.Items[i]^.Action <> SynCopy then continue;
        new(note);
        note^ := NoteMetaData.Items[i]^;
        note^.Title := note^.Title + ' (Server)';
        CreateGUID(GUID);
        note^.ID := copy(GUIDToString(GUID), 2, 36);
        note^.Action := SynDownload;
        NoteMetaData.Add(note);
        NoteMetaData.Items[i]^.Action := SynUploadEdit;
    end;

    DownList.Clear;

    for i := 0 to NoteMetaData.Count-1 do
    begin
        if NoteMetaData.Items[i]^.Action <> SynDownLoad then continue;
        ID := NoteMetaData.Items[i]^.ID;

        DownList.Add(ID);
        dest := GetLocalNotePath(NotesDir, ID);
        backup := GetLocalNotePath(NotesDir +  'Backup');
        if FileExists(dest) then
        begin
           ForceDirectories(backup);
           if not CopyFile(dest, GetLocalNotePath(NotesDir +  'Backup',ID)) then
                begin
                    ErrorString := 'Failed to copy file '+ dest + ' to Backup ' + backup;
                    debugln(ErrorString);
                    exit(False);
                end;
        end;
        Assign(f,dest);
        write(f, NoteMetaData.Items[i]^.Content);
        Close(f);
    end;

    result := True;
end;


function TSync.DoDeleteLocal() : boolean;
var
    I : integer;
    s,d,ID : String;
begin
    debugln('DoDeleteLocal');

    DeletedList.Clear;

    for I := 0 to NoteMetaData.Count -1 do
    begin
        if NoteMetaData.Items[i]^.Action <> SynDeleteLocal then continue;
        ID := NoteMetaData.Items[i]^.ID;
        DeletedList.Add(ID);

        s:= GetLocalNotePath(NotesDir,ID);
        d:= GetLocalNotePath(NotesDir,'Backup' + Pathdelim + ID);

        if FileExists(s) then
        begin
            ForceDirectories(s+'Backup');

            if CopyFile(s,d)
            then DeleteFile(s);
        end;
    end;
    result := true;
end;


function TSync.PushChanges() : boolean;
var
    notes : TNoteInfoList;
    i : integer;
    note,l : PNoteInfo;
begin
    debugln('PushChanges . ServerRev = ' + inttostr(Transport.ServerRev));

    notes := TNoteInfoList.Create;

    for i := 0 to NoteMetaData.Count -1 do
    begin
        note := NoteMetaData.Items[i];

        if(note^.Action  = SynUploadNew) then
        begin
            note^.Rev := Transport.ServerRev + 1;
            notes.Add(note);
        end;

        if(note^.Action  = SynUploadEdit) then
        begin
            l := LocalMetaData.FindID(note^.ID);
            l^.Action := SynUploadEdit;
            l^.Rev := Transport.ServerRev + 1;
            notes.Add(l);
        end;

        if note^.Action = SynDeleteRemote then
        begin
            notes.Add(l);
        end;
    end;

    if(notes.Count = 0) then Result:= false
    else Result := Transport.PushChanges(notes);

    FreeAndNil(notes);

end;


function TSync.DoManifest(): boolean;
var
    OutFile: String;
    Index : integer;
    d,globalrev,rev : string;
    needRev : boolean;
    f : TextFile;
begin

    needRev := False;
    for Index := 0 to NoteMetaData.Count - 1 do
    begin
        if NoteMetaData[Index]^.Action in [SynUploadNew, SynUpLoadEdit, SynDeleteRemote] then needRev := True;
    end;

    if(needRev) then globalrev := inttostr(Transport.ServerRev + 1)
    else globalrev := inttostr(Transport.ServerRev);

    OutFile := '<?xml version="1.0" encoding="utf-8"?>';
    OutFile := OutFile + '<sync revision="' + globalrev;
    OutFile := OutFile + '" server-id="' + Transport.ServerID + '">';

    for Index := 0 to NoteMetaData.Count - 1 do
    begin
         if NoteMetaData[Index]^.Deleted then continue;

         if NoteMetaData.Items[Index]^.LastChangeGMT = 0
         then d := GetCurrentTimeStr()
         else d := GetTimeFromGMT(NoteMetaData.Items[Index]^.LastChangeGMT);

         if NoteMetaData[Index]^.Action in [SynUploadNew, SynUpLoadEdit]
         then begin
              rev := IntToStr(Transport.ServerRev+1);
              d := GetCurrentTimeStr();
         end else rev := IntToStr(NoteMetaData[Index]^.Rev);


         if NoteMetaData[Index]^.Action in [SynUploadNew, SynUpLoadEdit, SynDownLoad, SynNothing] then // Filter bugs (?)
         begin
	      OutFile := OutFile +  '  <note guid="' + NoteMetaData.Items[Index]^.ID + '" latest-revision="'
              + rev + '" last-change-date="' + d + '" />';
         end
         else debugln('Skipping '+NoteMetaData[Index]^.ID + ' because Action=' + NoteMetaData.ActionName(NoteMetaData[Index]^.Action));
    end;
    OutFile := OutFile + '</sync>';

    result := Transport.DoRemoteManifest(OutFile);

    try
       AssignFile(f,getManifestName());
       Rewrite(f);
       Write(f,Outfile);
       Close(f)
    except on E:Exception do begin
       ErrorString := E.message;
       debugln(ErrorString);
       exit(false);
       end;
    end;

end;


{ =================  S T A R T   U P   M E T H O D S ============== }

function TSync.SetTransport(Mode: TSyncTransport) : TSyncAvailable;
begin
    TransportMode := Mode;
    NotesDir := AppendPathDelim(ChompPathDelim(NotesDir));
    ConfigDir := AppendPathDelim(ConfigDir);
    ErrorString := '';
    FreeAndNil(Transport);
    case Mode of
        SyncFile : begin
        	Transport := TFileSync.Create;
	        Transport.setParam('RemoteAddress',AppendPathDelim(Sett.LabelFileSync.Caption));
                end;
	SyncNextCloud : begin
                Transport := TNextSync.Create;
	        Transport.setParam('URL', Sett.NCUrl);
                Transport.setParam('KEY', Sett.NCKey);
                Transport.setParam('TOKEN', Sett.NCToken);
                Transport.setParam('SECRET', Sett.NCSecret);
                end;
        SyncAndroid : begin
            Transport := TAndSync.Create;
                Transport.setParam('RemoteAddress', FormTomdroid.EditIPAddress.Text);
                Transport.setParam('Password', FormTomdroid.EditPassword.Text);
                end;
    end;
    Transport.NotesDir := NotesDir;

    if TransportMode = SyncAndroid then
    begin
        ConfigDir := ConfigDir + 'android' + PathDelim;
        ForceDirectory(ConfigDir);
    end;
    Transport.ConfigDir := ConfigDir;

    Result := Transport.SetTransport();

    ErrorString := Transport.ErrorString;

end;

function TSync.TestConnection(): TSyncAvailable;
var
    res : TSyncAvailable;
begin
    Transport.ServerRev:=-1;

    res := Transport.TestTransport();
    if res <> SyncReady
    then begin
         ErrorString := Transport.ErrorString;
         exit;
    end;

    Result := res;
end;

function TSync.LoadLocalNotes(): boolean;
var
   ID,s : String;
   c,j : integer;
   Info : TSearchRec;
   PNote : PNoteInfo;
   Doc : TXMLDocument;
   manifest : String;
   NodeList : TDOMNodeList;
   Node : TDOMNode;
begin

    FreeAndNil(LocalMetaData);
    LocalMetaData := TNoteInfoList.Create;
    c :=0;
    if FindFirst(GetLocalNotePath(NotesDir,'*'), faAnyFile, Info)=0 then
    repeat
        ID := copy(Info.Name, 1, 36);
        new(PNote);
        PNote^.Action:=SynUnset;
        PNote^.ID := ID;
        PNote^.Rev := -1;
        PNote^.LastSyncGMT := 0;
        PNote^.LastSync := '';

        s := GetLocalNotePath(NotesDir, ID);
        if(FileToNote(s, PNote ))
        then begin
             LocalMetaData.Add(PNote);
             inc(c);
        end
        else begin
             FreeAndNil(PNote);
             debugln('Local note '+s+' not readable');
        end;
    until FindNext(Info) <> 0;
    FindClose(Info);
    debugln('Found '+IntToStr(c)+' total local notes');

    manifest:= getManifestName();
    if not FileExists(manifest) then exit();

    try
         ReadXMLFile(Doc, manifest);
    except on E:Exception do begin debugln(E.message); exit(false); end;
    end;

    NodeList := Doc.DocumentElement.ChildNodes;

    if not assigned(NodeList) then
    begin
         Doc.Free;
         debugln('We failed to read XML children in the remote manifest file '+manifest);
         exit();
    end;

    for j := 0 to NodeList.Count-1 do
    begin
        Node := NodeList.Item[j].Attributes.GetNamedItem('guid');
        ID := Node.NodeValue;

        PNote := LocalMetaData.FindID(ID);
        if(PNote = Nil) then begin
            new(PNote);
            PNote^.Action:=SynUnset;
            PNote^.ID := ID;
            PNote^.Deleted := true;
            PNote^.LastSyncGMT := 0;
            PNote^.LastSync := '';
            LocalMetaData.Add(PNote);
        end;

        Node := NodeList.Item[j].Attributes.GetNamedItem('latest-revision');
        PNote^.Rev := StrToint(Node.NodeValue);
        Node := NodeList.Item[j].Attributes.GetNamedItem('latest-sync-date');
        PNote^.LastSync := Node.NodeValue;
        if PNote^.LastSync <> ''
        then PNote^.LastSyncGMT := GetGMTFromStr(PNote^.LastSync)
        else PNote^.LastSyncGMT := 0;
    end;
end;

function TSync.LoadRemoteNotes(): boolean;
begin
    Result := True;
    FreeAndNil(NoteMetaData);
    NoteMetaData := TNoteInfoList.Create;

    Result := Transport.GetNotes(NoteMetaData);

    debugln('LoadRemoteNotes found ' + inttostr(NoteMetaData.Count) + ' remote notes');
end;

                        { ---------- The Lets Do it Function ------------- }

function TSync.StartSync(isTest : boolean): boolean;
var
   i : integer;
begin
    Result := True;

    if(isTest) then debugln('StartSync(isTest = true)') else debugln('StartSync(isTest = false)');
    //if(SyncOnce) then debugln('SyncOnce = true') else debugln('SyncOnce = false');

    if not LoadRemoteNotes() then begin ErrorString := Transport.ErrorString; exit(False); end; // Goes first to get ServerID
    if not LoadLocalNotes() then begin ErrorString := 'Can not load local notes'; exit(false); end;

    FindDeletedServerNotes(); // Step 1 : compare local manifest and server status for locally existing notes
    FindDeletedLocalNotes();  // Step 2 : compare local manifest and server status for none locally existing notes
    FindNewLocalNotes();      // Step 3 : Add newly created local notes
    FindNewRemoteNotes();     // Step 4 : Add newly created remote notes
    // Now, NoteMetaData contains all notes to be dealt with
    SetSystemicActions();     // Step 5 : Set systemic actions

    ProcessClashes();         // Step 6 : Process unresolved cases

    // ====================== Set an exit here to do no-write tests
    if isTest then exit();

    DisplayNoteInfo(NoteMetaData, 'Note Meta Data');

    // ========= Proceed with real actions

    // Add Remove notes (whatever the rev)
    if not DoDownLoads() then exit(false);
    if not DoDeleteLocal() then exit(false);

    GridReportList.Clear;
    for i := 0 to NoteMetaData.Count -1 do
    begin
         if NoteMetaData.Items[i]^.Action = SynNothing then continue;

         GridReportList.Add(NoteMetaData.ActionName(NoteMetaData.Items[i]^.Action));
         GridReportList.Add(NoteMetaData.Items[i]^.Title);
         GridReportList.Add(NoteMetaData.Items[i]^.ID);
    end;

    // Write remote manifest (only applicable for SyncFile)
    if not PushChanges() then exit(false);

    Result := DoManifest();
end;

end.

