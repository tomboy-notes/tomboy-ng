unit sync;
{   Copyright (C) 2017-2024 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

    ------------------

}

{$mode objfpc}{$H+}


{ How to use this Unit -

    This unit is used by : SearchUnit but only for access to local manifest file;
    Settings where its run, in a thread, to do auto sync; SyncGUI where it runs in
    min thread interactivly. At all levels, msgs might be dropped to console
    but only in SyncGUI can we show the user a msg.

    Syncutils will probably be needed in Interface Uses
    Sync in implementation of Uses.

  if we believe we have an existing Repo accessible -

  	ASync := TSync.Create();
    ASync.SetTransport(TransP);         // possible value defined in SyncUtils, SyncFile, SyncMisty (!SyncNextCloud, !Sync
                                        // test for IPOK() with GitHubSync
    ASync.DebugMode:=True;
	ASync.TestRun := ? ;
	ASync.ProceedFunction:=@Proceed;    // A higher level function that can resolve clashes
	ASync.NotesDir:= ?;
	ASync.ConfigDir := ?;
	ASync.SyncAddress := ?;
    ASync.RepoAction:= RepoUse;         // RepoUse says its all there, ready to go.
	Async.SetMode(SyncFile);            // SyncFile, SyncGithub, SyncMisty....
    SyncReady <> ASync.TestConnection() then    // something bad happened, SyncReady only
                                                // acceptable answer. Check ErrorString

  If joining an existing or making a new repo, set RepoAction to RepoJoin, if TestConnection
  returns SyncNoRemoteRepo then ask user if they want to create a new repo, try again with
  RepoNew.

  Other errors to be dealt with include -
  SyncXMLError, SyncNoRemoteDir, SyncBadRemote, SyncNoRemoteWrite ....

  -----------------------------------------------------------------------------


Operation of this unit -

Fistly, this unit depends on on the Trans unit, a virtual unit of which the TransFile
and TransGitHub have been implemented. Further Transport layers should be easily
made.

Two seperate approaches are needed. In both cases we build a list of all the notes we
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

The Model as implemented in tomboy-ng, October 2018

We call TestTransportEarly() and/or TestTransport()
Its tests and may, or may not populate NoteMetaData with remote notes. It may, or may not
put the remote LCD in NoteMetaData.

We call StartSync() (not talking about Android mode here.)
---------------------
RepoAction = RepoUse (that is, it all should be setup, just go and do it)
CheckUsingRev() Assigned a action to each existing entry in NoteMetaData based on the
    current revision number and notes present in NotesDir. Note this method is an alternative to CheckUsingLCD().
CheckRemoteDeletes() Marks remote deletes, that is notes that are mentioned in
    LocalManifets as having been synced in the past (but not deleted section) but
    were not listed in NoteMetaData at this stage because the Remote Server does not
    know about them. Note, this will override a local note that has been edited
    since last sync - should this be a clash ?
CheckLocalDeletes() looks at notes listed in LocalManifest as Deletes, it markes
    them, in NoteMetaData as DeleteRemote to be deleted from remote system (maybe
    just not mentioned in remote manifest any more).
CheckNewNotes() looks in Notes dir for any notes there that are not yet listed in
    NoteMetaData. These notes are UploadNew.

Call the General Write Behavour Block if not a TestRun.

RepoAction = RepoJoin
---------------------
CheckUsingLCD(True) Assignes an Action to each entry in NoteMetaData (which only
    contains entries from remote repo at this stage) based on last-change-date.
    If it finds a potential clash, aborts if it does not have last-change-date for
    the remote note. In that case, we recall LoadReopData(True) this time demanding
    last change date. And then run CheckUsingLCD again.  Note this method is an
    alternative to CheckUsingRev(). Any notes thats determined here to be an UpLoad
    has its LCD (in NoteMetaData) set to the local note's LCD.
Because a join cannot use local manifest, we do not honour remote or local deletes.
CheckNewNotes() looks in Notes dir for any notes there that are not yet listed
    in NoteMetaData. These notes are UploadNew.

Call the General Write Behavour Block if not a TestRun.

RepoAction = RepoNew (if we determine, during a RepoJoin, that the dir we are
            pointing to does not contain a repo, we create one in this mode. Must
            always call RepoJoin first)
-----------------------
CheckNewNotes() looks in Notes dir for any notes there that are not yet listed in
            NoteMetaData (which, is, of course empty at this stage). These notes are
            UploadNew. Call the General Write Behavour Block if not a TestRun.

General Write Behavour
----------------------
applies for all three cases above. Some methods are not relevent in some modes,
they just return true when they detect that themselves.)
ProcessClashes()
DoDownLoads()
WriteRemoteManifest()   - writes out a local copy of remote manifest to be used later.
    We always write it (except in TestRun) but only call Transport to deal with it
    if we have notes changing.
DoDeletes() - Deletes from remote server. In current file implementation, does nothing.
DoUploads()
DoDeleteLocal()
WriteLocalManifest()

Note, we want to write a new local manifest even if there are no note changes taking place,
that way, we get an updated Last-Sync-Date and possibly updated revnumber. If we have not
changed any files, then RevNo is the one we found in RemoteManifest. So, we must step
through the write stack.

We make a half hearted attempt to restore normality if we get to local manifest stage and somehow fail to write it out.
-----------------------------------------------------------------------------------

HISTORY
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
    2021/01/04  Support TomdroidFile sync mode.
    2021/08/31  Added sha to TNoteInfo
    2021/09/08  Added progress indicator
    2021/09/27  Selective Sync, possible to have both configured and in use.
    2022/10/18  When renaming a file, delete target if its exists first, its a windows problem
    2023/10/28  Restructure some of Auto Sync to allow multithreading, does not use SyncGUI
    2023/12/16  Nasty bug where ProcessClashes() return value was not set.
    2024/05/08  extensive rework of debugging code, esp GitHub related
}

interface
uses
    Classes, SysUtils, SyncUtils, Trans, LCLIntf;


type                       { ----------------- T S Y N C --------------------- }

  { TSync }

  TSync = class

  private
            // Indicates that use of this class, currently, is multithreaded.
            // In particular, StartSync() will call Synchronize() and tell it to
            // check upcoming downloads, marking any matching notes as readonly.
            // Its set in AutoSync() only.
        Threaded : boolean;

            // Generally an empty string but in Android/Tomdroid something we prefix
            // to local manifest file name to indicate which connection it relates to.
        ManPrefix : string;

            // Set by SetTransport(), indicates what sort of Sync we are trying to do.
        TransportMode : TSyncTransport;

            { Indicates an action to take to be taken for each (and every) note during
              a sync. }
        ProceedAction : TSyncAction;

	         // Where we find Tomboy style notes
	    FNotesDir : string;

	         // Where we find config and local manifest files
        FConfigDir : string;

            { Scans Notes dir looking for each note in NoteMetaData. Any it finds
              are either clashes or SyNothing, anything left are downloads.
              If AssumeNoClash and we find a clash, unresolable 'cos LCD missing, ret False,
              (and expect LoadRepoData to be called again. Used when JOINING an existing repo.}
      function CheckUsingLCD(AssumeNoClash : boolean) : boolean;

            { Returns true if the passed dates are pretty close, see code for just how close }
      function DatesClose(const DS1, DS2: TDateTime): boolean;


            { Backs up and then removes any local local notes listed NoteMetaData as SyDeleteLocal }
      function DoDeleteLocal(): boolean;

            { Returns the title of given note, prefers the local version but if
              it does not exist, then "downloads" remote one }
      function GetNoteTitle(const ID : ANSIString; const Rev : integer): ANSIString;



      // function IDLooksOK(const ID: string): boolean;

            { Looks at a clash and determines if its an up or down depending on
              possible higher order TSyncActions such as newer, older }
      function ResolveAllClash(const Act : TSyncAction; const ID, FullRemoteFileName : ANSIString): TSyncAction;

          { Goes over NoteMetaData (which has only remote notes at this stage) assigning
            an Action to each. Only called when using an established sync connection. }
      function CheckUsingRev(): boolean;

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
      procedure DisplayNoteInfo(const meta: TNoteInfoList; const ListTitle : string);
            // Based on NoteMetaData, calls transport to delete notes from Server.
	  function DoDeletes(): boolean;

          { We call transport for all the notes in the list we need download.
          Transport does most of the work. In TestMode, does nothing }
	  function DoDownloads(): boolean;

            { Uploads any files it finds necessary in NoteMetaData. Returns false
              if anything goes wrong (such as a file error) }
	  function DoUploads(): boolean;

            {   Asks Transport for a list of the notes remote server knows about.
                If ForceLCD then make heroic efforts to get last-change-dates }
	  function LoadRepoData(ForceLCD : boolean): boolean;

            { Searches list for any clashes, refering each one to user. Done after
              list is filled out in case we want to ask user for general instrucions }
      function ProcessClashes: boolean;

        // Checks if local note exists, optionally returning with its last change date.
      function LocalNoteExists(const ID : string; out CDate: string; GetDate: boolean=false): Boolean;

        // Call this when we are resolving a sync clash. Note : not possible results make sense !
      function ProceedWith(const ID, FullRemoteFileName, NTitle: ANSIString): TSyncAction;

            { Reads through Local Manifest file, filling out LocalMetaData,
                LastSyncDateSt and CurrRev. If local manifest does not exist, still
                returns True but CurrRev=0 and LocalLastSyncDateSt=''
                If FullFileName parameter is missing, uses default version.}
      function ReadLocalManifest(const FullFileName : string = ''): boolean;

            { Writes a local mainfest file. Assumes NoteMetaData contains valid data about
              new Rev numbers and for uploads, last-change-date. If WriteOK is false then
              don't rev number and don't mention new notes in local manifest. Should
              never write entries in the DeletedNotes section.  This function is called at
              the end of a normal file sync, another one is used when updating (with deletes)
              an existing manifest. Key diff is ability to inc rev no and
              writing of a failsafe version if things go wrong.  Merge at some stage ?
              }
      function WriteLocalManifest(const WriteOK, NewRev : boolean) : boolean;

            { We write a remote manifest out localy if we have any uploads or to handle
              the delete from server a note that was deleted locally to do. Then, if
              TestMode is false, call Transport to deal with it. Writing it locally is fast
              and we get to check for and isolate any data errors. Initially
              written to $CONFIG/manifest.xml-remote and copied (moved ?).}
	  function WriteRemoteManifest(out NewRev: boolean): boolean;

                      { Only used with DeleteFromLocalManifest(), iff it finds a matching entry in
                        indicated manifest main sesction moves it to the deleted Notes section }
      function DeleteFromThisManifest(const FullFileName, ID: string): boolean;

                      { Only used with DeleteFromLocalManifest(), writes LocalMetaData back to disk.
                        Honours TestRun. }
      function ReWriteLocalManifest(const FullFileName : string) : boolean;

            { Applies only to Github, returns the token expire data or 'Expired' }
      function FGetTokenExpire() : string;

      function FGetTransRemoteAddress() : string;

   public

            // A passord, passed on to Trans for those Transports that need it.
            // Must be set (if needed) before SetTransport is called.
        UserName, Password : string;

            // Indicates what we want this unit to do. Set it and call TestConnection() repeatly...
       RepoAction : TRepoAction;

            { Records local manifests view of serverID, after succefull test, it
              should be the remote manifest's one too. In Android mode, we preload
              it from the config file at class creation and its overwritten (with
              same data) when local manifest is read. But in RepoJoin, we set it,
              after TestConnection to the Transport's view of what remote ServerID is.
              This is for Android mode to be able to record the new ID in its own
              config file. }
        LocalServerID : string;

          { The calling process must pass a function address to this var except in
          Auto Sync mode (when its nil). It will be called if the sync process finds
          a sync class where both copies of a note have changed since last sync.}
        ProceedFunction : TProceedFunction;

            { A method to call when we can advise a GUI of progress through sync }
        ProgressProcedure : TProgressProcedure;

                // A URL or directory with trailing delim. Get from Settings.
        SyncAddress : string;
                // Revision number the client is currently on
        CurrRev : integer;
                // A string of local last sync date. Empty if we have not synced before
                // Available iff we are in RepoUse mode after TestConnection()
        LocalLastSyncDateSt : string;
                // Last time this client synced (not this run), set and tested in call to StartSync()
                // Available iff we are in RepoUse mode after TestConnection()
        LocalLastSyncDate : TDateTime;
                // Write debug messages as we do things.
        DebugMode : boolean;

                // Determine what we'd do and write ref ver of manifests but don't move files around.
                // The first run during a Join is a TestRun, then, if user says Save, its not.
        TestRun : boolean;
                // A reason why something failed.
        ErrorString : string;
                    { Data about notes in remote manifest, this list ultimatly holds the actions
                      to be taked when the the actual sync process really runs. Is used to create
                      manifests and generate user report after a sync.}
        RemoteMetaData : TNoteInfoList;
                // Data obtained from Local Manifest. Represents notes previously synced. Might be empty.....
        LocalMetaData : TNoteInfoList;
                // Used in Threaded mode to lock any open notes that are about to be overwritten by a sync download.
                // Is called from the Thread.Execute method in Settings.
        Procedure AdjustNoteList();
                { returns the number of changes this sync run made. }
        function ReportChanges(): integer;
        procedure FSetConfigDir(Dir : string);
        property ConfigDir : string read FConfigDir write FSetConfigDir;

        property TransMode : TSyncTransport read TransportMode;
        property TokenExpire : string read FGetTokenExpire;

        procedure FSetNotesDir(Dir : string);
        Property NotesDir : string read FNotesDir write FSetNotesDir;

                                { Returns the set Transport's RemoteAddress, for GithubSync }
        property GetTransRemoteAddress : string read fGetTransRemoteAddress;

                            { IFF its there, delete the indicated ID from main section of local manifest (and
                            any Tomdroid  and SyncGithub manifests) and list it in the deleted section instead.
                            Its really stand alone, create a sync object, set config and notes dir, call
                            this method and free. }
        function DeleteFromLocalManifest(ID: ANSIString) : boolean;

                            { Reports on contents of a created and filled list in SyncGUI }
	    procedure ReportMetaData(out UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing, Errors: integer);

                            { Selects a Trans layer, adjusts config dir,
                            TransFileAnd : checks for the expected remote dir, may return SyncNoRemoteRepo,
                            SyncNoServerID (not an error, just not used previously) or SyncReady. }
        function SetTransport(Mode : TSyncTransport) : TSyncAvailable;

                { Checks NoteMetaData for valid Actions, writes error to console.
                  Always returns True and does mark bad lines with Action=SyError
                  Also fills in note Title for notes we will do something with.}
        function CheckMetaData() : boolean;

            { May return : SyncXMLError, SyncNoRemoteDir, SyncNoRemoteWrite,
              SyncNoRemoteRepo, SyncBadRemote, SyncMismatch. Checks if the connecton
              looks viable, either (fileSync) it has right files there and write access
              OR (NetSync) network answers somehow (?). Reads local manifest if
              RepoAction=RepoUse and compares its serverID with one found by
              Trans.testConnection. SyncReady means we can proceed to StartSync, else must
              do something first, setup new connect, consult user etc.}
        function TestConnection() : TSyncAvailable;

            { Does the actual moving of files around, depends on a successful GetSyncData()
              before hand. TestRun=True just report on what you'd do.
              Assumes a Transport has been selected and remote address is set.  }
      function UseSyncData(DoClash: boolean = false): boolean;
            { Populates the Sync data structures with info about what files need to be
              moved where. Does no data moving (thats done by UseSyncData())
              We must already be a member of this sync, ie, its remote ID is recorded
              in our local manifest. TestRun=True just report on what you'd do.}
      function GetSyncData(): boolean;
            { Shortcut setup for background run, still needs GetSyncData() and
              UseSyncData() to be called aftwards. Able to be run in a background thread. }
      function AutoSetUp(Mode: TSyncTransport): boolean;
      constructor Create();

      destructor Destroy(); override;

  end;

implementation

{ TSync }

uses laz2_DOM, laz2_XMLRead,
    TransFile,
    {$ifndef TESTRIG}       // too hard to make this work needing note_lister ! For now ...
    TransGithub,
    {$endif}
    transmisty,
    LazLogger, LazFileUtils, FileUtil,
    {$ifndef TESTRIG}
    Settings,
    SearchUnit,
    {$endif}
    tb_utils;                    // here because we call SearchForm.ProcessSyncUpdates()

{$ifdef TESTRIG}
const TestRigSyncFileRepo = '/run/user/1000/gvfs/smb-share:server=greybox.local,share=store/TB_Sync-alt/';
{$endif}

var
    Transport : TTomboyTrans;

constructor TSync.Create();
begin
    ProgressProcedure := Nil;
    ProceedFunction := Nil;
    RemoteMetaData := TNoteInfoList.Create;
    LocalMetaData := TNoteInfoList.Create;
    Transport := nil;
end;

destructor TSync.Destroy();
begin
    FreeandNil(LocalMetaData);
    FreeandNil(RemoteMetaData);
    FreeandNil(Transport);
    inherited Destroy();
end;

function TSync.DeleteFromLocalManifest(ID: ANSIString) : boolean;
var
    FullFileName : string;
    i : integer;
begin
    for i := 0 to high(Sett.SyncInfo) do
        if Sett.SyncInfo[i].RemoteAddress <> '' then begin                      // empty RemoteAddress says not configured
           FullFileName := ConfigDir;
           if i = ord(SyncFile) then
               FullFileName :=  FullFileName + 'manifest.xml'
           else FullFileName :=  FullFileName + Sett.SyncInfo[i].DisplayName + PathDelim + 'manifest.xml';
           if FileExists(FullFileName) then
               if not DeleteFromThisManifest(FullFileName, ID) then begin
                          debugln('TSync.DeleteFromLocalManifest - ERROR - failed to delete ' + ID + ' from ' + FullFileName);
                          exit(False);
               end
           else if DebugMode then
                debugln('TSync.DeleteFromLocalManifest - ERROR - Sync appears configured but cannot find ' + FullFileName);
        end;                                                                    // Else that sync not configured, thats OK
    result := True;
end;


(*        if not DeleteFromThisManifest(FullFileName, ID) then begin
           debugln('ERROR - failed to delete ' + ID + ' from ' + FullFileName);
            // Note - not finding the manifest file is not an error, just unsynced.
            exit(False);
        end
    else if DebugMode then
       debugln('DeleteFromLocalManifest - cannot find ' + FullFileName + ' not in use ?');

    FullFileName := ConfigDir + SyncTransportName(SyncGithub) + PathDelim + 'manifest.xml';
    if FileExists(FullFileName) then
        if not DeleteFromThisManifest(FullFileName, ID) then begin
           debugln('ERROR - failed to delete ' + ID + ' from ' + FullFileName);
           exit(False);
        end
    else if DebugMode then
       debugln('DeleteFromLocalManifest - cannot find ' + FullFileName + ' not in use ?');

    if DirectoryExists(ConfigDir + 'android') then begin
        if FindFirst(ConfigDir + 'android' + pathdelim + '*.xml', faAnyFile, Info)=0 then
           try
               repeat
                   // Info.Name is just the file name, no path prepended.
                   // debugln('DeleteFromLocalManifest-------- Found xml file ' + Info.Name);
                   DeleteFromThisManifest(ConfigDir + 'android' + pathdelim + Info.Name, ID);
               until FindNext(Info) <> 0;
           finally
               FindClose(Info);
           end;
    end else if DebugMode then
       debugln('DeleteFromLocalManifest - cannot find ' + ConfigDir + 'android');
    exit(True);
end;              *)


function TSync.LocalNoteExists(const ID : string; out CDate : string; GetDate : boolean = false) : Boolean;
var
	Doc : TXMLDocument;
	Node : TDOMNode;
    //LastChange : string;
begin
    if not FileExists(NotesDir + ID + '.note') then exit(False);
    if not GetDate then exit(True);
    Result := True;
	try
		ReadXMLFile(Doc, NotesDir + ID + '.note');
		Node := Doc.DocumentElement.FindNode('last-change-date');
        if assigned(node) then
            CDate := Node.FirstChild.NodeValue
        else begin
            CDate := '';
            Result := False;
        end;
    finally
        Doc.free;
    end;
end;


{ =================   E X T E R N A L   C A L L   O U T S ===========================}


function TSync.ProceedWith(const ID, FullRemoteFileName, NTitle : ANSIString) : TSyncAction;
var
    ClashRec : TClashRecord;
    //ChangeDate : ANSIString;
begin
    // Note - we no longer fill in all of clash record, let sdiff unit work it out.
    ClashRec.NoteID := ID;
    ClashRec.Title:= NTitle;
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
    Result := SyDownload;               // just in case ....
    case Act of
        SyAllLocal : exit(SyUpLoadEdit);
        SyAllRemote : exit(SyDownLoad);
        SyAllNewest : if TB_GetGMTFromStr(GetNoteLastChangeSt(NotesDir + ID + '.note', Temp))
                            >  TB_GetGMTFromStr(GetNoteLastChangeSt(FullRemoteFileName, Temp)) then
                        exit(SyUploadEdit)
                      else
                        exit(SyDownLoad);
        SyAllOldest :  if TB_GetGMTFromStr(GetNoteLastChangeSt(NotesDir + ID + '.note', Temp))
                            <  TB_GetGMTFromStr(GetNoteLastChangeSt(FullRemoteFileName, Temp)) then
                        exit(SyUploadEdit)
                      else
                        exit(SyDownLoad);
        else;
    end;
end;

function TSync.ProcessClashes() : boolean;
var
    Index : integer;
    RemoteNote : string;
begin
    Result := True;         // Will be false if we have a Sync Clash and in AutoSync Mode
    for Index := 0 to RemoteMetaData.Count -1 do begin
        // debugln('TSync.ProcessClashes checking note number ' + inttostr(Index) + ' id=' + RemoteMetaData.Items[Index]^.ID);
        with RemoteMetaData.Items[Index]^ do begin
            if Action = SyClash then begin
                if ProceedFunction = Nil then exit(False);                      // In autosync, we cannot have a ProceedFunction !
                RemoteNote := Transport.DownLoadNote(ID, Rev);
                // debugln('TSync.ProcessClashes - Resolving clash with ' + RemoteNote);
                if ProceedAction = SyUnSet then begin       // let user decide
                    Action := ProceedWith(ID, RemoteNote, RemoteMetaData.Items[Index]^.Title);
                    if Action in [SyAllOldest, SyAllNewest, SyAllLocal, SyAllRemote] then
                        Action := ResolveAllClash(Action, ID, RemoteNote);
                end else
                    Action := ResolveAllClash(ProceedAction, ID, RemoteNote);  // user has already said "all something"
            end;
            //if its now become an upload, we update the LCD because ......
            if Action = SyUpLoadEdit then begin
                LastChange := GetNoteLastChangeSt(NotesDir + ID + '.note', ErrorString);
                if  LastChange <> '' then
                    LastChangeGMT := TB_GetGMTFromStr(LastChange)
                else
                    debugln('TSync.ProcessClashes - ERROR, Failed to get LCD from local ' + ID + ' --- ' + ErrorString);
            end;
        end;
    end;
end;

procedure TSync.ReportMetaData(out UpNew, UpEdit, Down, DelLoc, DelRem, Clash, DoNothing, Errors : integer);
var
    Index : integer;
begin
    UpNew := 0; UpEdit := 0; Down := 0; Errors := 0;
    DelLoc := 0; DelRem := 0; DoNothing := 0; Clash := 0;
    for Index := 0 to RemoteMetaData.Count -1 do begin
        case RemoteMetaData.Items[Index]^.Action of
            SyUpLoadNew : inc(UpNew);
            SyUpLoadEdit : inc(UpEdit);
            SyDownLoad : inc(Down);
            SyDeleteLocal : inc(DelLoc);
            SyDeleteRemote : inc(DelRem);
            SyClash : inc(Clash);
            SyNothing : inc(DoNothing);
            SyError : inc(Errors);
		end;
    end;
end;

{x$define DEBUGAUTOSYNC}
function TSync.ReportChanges() : integer;
var
    Index : integer;
//    St : string;
begin
    result := 0;
    if (RemoteMetaData =  nil) then begin
        debugln({$I %CURRENTROUTINE%}, '() ', {$I %FILE%}, ', ', 'line:', {$I %LINE%}, ' : ', 'BUT REMOTEMETADATA is NIL.');
        exit(0);
    end;
    {$ifdef DEBUGAUTOSYNC}
    debugln('------- AutoSync Run ' +  FormatDateTime('YYYY-MM-DD hh:mm', now()) + '  TSync.ReportChanges  -------');
    {$endif}
    for Index := 0 to RemoteMetaData.Count -1 do
        if RemoteMetaData.Items[Index]^.Action in [SyUpLoadNew, SyUpLoadEdit, SyDownLoad, SyDeleteLocal, SyDeleteRemote]
            then begin
               inc(result);
               {$ifdef DEBUGAUTOSYNC}
               debugln(RemoteMetaData.Items[Index]^.Title + '  ' + RemoteMetaData.ActionName(RemoteMetaData.Items[Index]^.Action));
               {$endif}
            end;
end;

function TSync.CheckMetaData(): boolean;
var
    Index : integer;
begin
    Result := True;
    ErrorString := '';
    for Index := 0 to RemoteMetaData.Count -1 do begin
        if RemoteMetaData[Index]^.Action = SyUnSet then begin
            Debugln('TSync.CheckMetaData - ERROR note not assigned ' + RemoteMetaData[Index]^.ID + ' '
                   + RemoteMetaData.ActionName(RemoteMetaData[Index]^.Action) + '  '
                   + RemoteMetaData[Index]^.LastChange + '  '
                   + RemoteMetaData[Index]^.Title );
            result := False;
        end;
        if IDLooksOK(RemoteMetaData[Index]^.ID) then begin
            if (RemoteMetaData[Index]^.Action in [SyNothing, SyUploadNew, SyUpLoadEdit, SyDownLoad,
                    SyDeleteLocal, SyDeleteRemote, SyClash] )
                and  (RemoteMetaData[Index]^.Title = '') then
                       RemoteMetaData[Index]^.Title := GetNoteTitle(RemoteMetaData[Index]^.ID, RemoteMetaData[Index]^.Rev);
        end else begin
            if RemoteMetaData[Index]^.Title = '' then
                RemoteMetaData[Index]^.Title := GetNoteTitle(RemoteMetaData[Index]^.ID, RemoteMetaData[Index]^.Rev);
            Debugln('TSync.CheckMetaData - ERROR - invalid ID detected : [' + RemoteMetaData[Index]^.ID + ']');
            RemoteMetaData[Index]^.Action := SyError;
            ErrorString := 'ERROR - invalid ID detected when CheckMetaData [' + RemoteMetaData[Index]^.ID + ']';
        end;
    end;
    if debugmode then
       debugln('CheckMetaData - NoteMetaData has ' + inttostr(RemoteMetaData.Count) + ' entries.');
end;

function TSync.GetNoteTitle(const ID : ANSIString; const Rev : integer) : ANSIString;
var
        Doc : TXMLDocument;
        Node : TDOMNode;
        FileName : string;
begin
    Result := 'File Not Found';
    FileName := NotesDir + ID + '.note';
    if not FileExistsUTF8(FileName) then
        if assigned(Transport) then FileName := Transport.DownLoadNote(ID, Rev);
    if FileExistsUTF8(FileName) then begin
        try
            Result := 'Unknown Title';
            try
                ReadXMLFile(Doc, FileName);
                Node := Doc.DocumentElement.FindNode('title');
                Result := Node.FirstChild.NodeValue;
            except on EXMLReadError do
                Result := 'Note has no Title ' + FileName;
                on EAccessViolation do
                    Result := 'Access Violation ' + FileName;
            end;
        finally
            Doc.free;
        end;
    end else begin
        debugln('ERROR - cannot get title for ' + FileName);
        result := 'ERROR getting Title';
    end;
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
        // debugln('ID=' + copy(Meta.Items[I]^.ID, 1, 9)  + St + Meta.ActionName(Meta.Items[i]^.Action)
        debugln('ID=' + Meta.Items[I]^.ID  + St + Meta.ActionName(Meta.Items[i]^.Action)
                + '   ' + Meta.Items[I]^.Title + '  sha=' + copy(Meta.Items[I]^.Sha, 1, 9));
        debugln('          CDate=' + Meta.Items[i]^.CreateDate + ' LCDate=' + Meta.Items[i]^.LastChange);
    end;
end;


{ =====================   D A T A    C H E C K I N G    M E T H O D S  ============= }

function TSync.DatesClose(const DS1, DS2 : TDateTime) : boolean;
var
    Margin : TDateTime = 0.000001;      // a tenth of a second is about one millionth of a day !
begin
    if DS1 > DS2 then result := (DS1 < (DS2 + Margin))
    // 1 greater than 2, if it changes when we  increase 2 result is true

    else Result := (DS1 > (DS2 - Margin));
    // 1 is less than 2, if it changes when we decrease 2, result is true
end;

function TSync.CheckUsingLCD(AssumeNoClash : boolean) : boolean;
var
    Index : integer;
    Count : integer = 0;
    Info : TSearchRec;
    PNote : PNoteInfo;
    LocLCD : string;        // The local note's last change date
begin
    { We declare a clash if both notes exist and LCDs are not identical (or nearly identical)
      However, iff we have a local last sync date (LLSD) then if the
      earlier note pre-dates it, its not a clash. So, not a 'pure' LCD model,
    }
    if FindFirst(NotesDir + '*.note', faAnyFile, Info)=0 then begin
        try
        repeat
            inc(Count);
            PNote := RemoteMetaData.FindID(copy(Info.Name, 1, 36));
            LocLCD := GetNoteLastChangeSt(NotesDir + Info.Name, ErrorString);
            // hmm, not checking for errors there .....
            if PNote <> nil then begin                      // ie, note exists on both sides
                if AssumeNoClash and (PNote^.LastChange = '')
                    then begin
                        if Debugmode then debugln('CheckUsingLCD exiting because if unresolved clash');
                        exit(false);                        // might be a clash, go fill out LCD in remote data
                    end;
                    // Next line new, we now accept an idetical string or a datestring thats pretty close
                if ((PNote^.LastChange = LocLCD) or (DatesClose(PNote^.LastChangeGMT, TB_GetGMTFromStr(LocLCD)))) then          // its the same note
                    PNote^.Action := SyNothing
                else  begin
                    PNote^.Action := SyClash;             // Best we can do if last sync date not available.
                    if LocalLastSyncDateSt <> '' then begin          // We can override that iff we have a LLSD
                        if TB_GetGMTFromStr(LocLCD) < LocalLastSyncDate then  PNote^.Action := SyDownload
                        else if  PNote^.LastChangeGMT < LocalLastSyncDate then PNote^.Action := SyUploadEdit;
                        if debugmode then
                            debugln('GMTimes - loc=' + FormatDateTime( 'yyyy-mm-dd hh:mm:ss', TB_GetGMTFromStr(LocLCD))
                                     + '  rem=' + FormatDateTime( 'yyyy-mm-dd hh:mm:ss', PNote^.LastChangeGMT)
                                     + '  LLSD=' + FormatDateTime( 'yyyy-mm-dd hh:mm:ss', LocalLastSyncDate)
                                     + '  rem-st=' + PNote^.LastChange);
                    end;
                end;
            end else begin                                  // this note is a new upload, add it to list.
                new(PNote);
                PNote^.ID := copy(Info.Name, 1, 36);
                PNote^.LastChange:=LocLCD;
                PNote^.Action:= SyUpLoadNew;                // Note, we may overrule that in CheckRemoteDeletes()
                PNote^.Sha := '';
                PNote^.ForceUpload := false;
                RemoteMetaData.Add(PNote);
            end;
        until FindNext(Info) <> 0;
        finally
            FindClose(Info);
        end;
    end;
    for Index := 0 to RemoteMetaData.Count -1 do
        if RemoteMetaData.Items[Index]^.Action = SyUnSet then
           RemoteMetaData.Items[Index]^.Action := SyDownLoad;
    if DebugMode then
       Debugln('CheckUsingLCD checked against ' + inttostr(Count) + ' local notes');
    exit(True);
end;

procedure TSync.CheckRemoteDeletes();
var
    Index : integer;
    PNote : PNoteInfo;
    Count : integer = 0;
begin
    // Must find a created LocalMetaData but an empty one is normal.
    // Iterate over LocalMetaData looking for notes listed as prev synced
    // but are not listed in RemoteMetaData. Or are listed as SyUploadNew !!
    // CheckUsingLCD puts all notes it finds locally but not in Remote as SyUploadNew
    // We'll add a entry (or change entry) in NoteMetaData for any we find.
    for Index := 0 to LocalMetaData.Count -1 do begin
        if not LocalMetaData.Items[Index]^.Deleted then begin
            PNote := RemoteMetaData.FindID(LocalMetaData.Items[Index]^.ID);
            if PNote = nil then begin   // That is, we did not find it
                new(PNote);
                PNote^.ID:= LocalMetaData.Items[Index]^.ID;
                PNote^.Title := LocalMetaData.Items[Index]^.Title;                    // I think we know title, useful debug info here....
                PNote^.Action := SyDeleteLocal;                                       // Was deleted elsewhere, do same here.
                PNote^.Sha := '';
                PNote^.ForceUpload := False;
                RemoteMetaData.Add(PNote);
                inc(Count);
            end else begin
                if PNote^.Action = SyUploadNew then         // if it is mentioned in Local Man but Load decided it was '~New', its
                    PNote^.Action := SyDeleteLocal;         // really a note that was deleted remotely and should be deleted locally now
            end;
        end;
    end;
    if debugmode then debugln('CheckRemoteDeletes checked ' + inttostr(LocalMetaData.Count)
            + ' and found ' + inttostr(Count) + ' notes ');
end;

procedure TSync.CheckNewNotes();
var
    Info : TSearchRec;
    PNote : PNoteInfo;
    ID, CDate : string;
    Count : integer = 0;
    CountNew : integer = 0;
begin
    if FindFirst(NotesDir + '*.note', faAnyFile, Info)=0 then begin
        repeat
            ID := copy(Info.Name, 1, 36);
            inc(Count);
            //Debugln('Found [' + NotesDir+ Info.Name + ']');
            PNote := RemoteMetaData.FindID(ID);
            if PNote = nil then begin
                if LocalNoteExists(ID, CDate, True) then begin
	                    new(PNote);
	                    Pnote^.ID:=ID;
	                    Pnote^.LastChange:=CDate;
	                    PNote^.Action:=SyUploadNew;
                        PNote^.sha := '';
                        PNote^.ForceUpload := false;
                        RemoteMetaData.Add(PNote);
                        inc(CountNew);
				end else Debugln('Failed to find lastchangedate in ' + Info.Name);
			end;
		until FindNext(Info) <> 0;
    end;
    FindClose(Info);
    if debugMode then debugln('CheckNewNotes found ' + inttostr(Count) + ' notes in local dir and ' + inttostr(CountNew) + ' new ones.');
end;

    // Iterate over LocalMetaData looking for notes that have been deleted
    // locally and put them in RemoteMetaData to be deleted from the server.
procedure TSync.CheckLocalDeletes();
var
    I : integer;
    Count : integer = 0;
    PNote : PNoteInfo;
begin
    for I := 0 to LocalMetaData.Count -1 do begin
        if LocalMetaData.Items[i]^.Deleted then begin
            inc(Count);
            PNote := RemoteMetaData.FindID(LocalMetaData.Items[i]^.ID);
            if PNote <> nil then
                PNote^.Action := SyDeleteRemote;
        end;
    end;
    if DebugMode then debugln('CheckLocalDeletes found ' + inttostr(Count) + ' deleted notes in local manifest');
end;

function TSync.CheckUsingRev() : boolean;
var
    I : integer;
    ID : string;    // to make it a bit easier to read souce
    PNote : PNoteInfo;
    LocCDate : string;
    LocChange, RemChange : boolean;
begin
    Result := True;
    for I := 0 to RemoteMetaData.Count -1 do begin
        ID := RemoteMetaData.Items[I]^.ID;
        if LocalNoteExists(ID, LocCDate) then begin
            LocalNoteExists(ID, LocCDate, True);
            LocChange := TB_GetGMTFromStr(LocCDate) > LocalLastSyncDate;       // TDateTime is a float

            RemChange := RemoteMetaData.Items[I]^.Rev > CurrRev;              // This is not valid for Tomdroid

            if LocChange and RemChange then
                RemoteMetaData.Items[I]^.Action := SyClash
            else if LocChange then
                    RemoteMetaData.Items[I]^.Action := SyUpLoadEdit
                else if RemChange then
                        RemoteMetaData.Items[I]^.Action := SyDownLoad
                    else  RemoteMetaData.Items[I]^.Action := SyNothing;
        end else begin      // OK, not here but maybe we deleted it previously ?
            Pnote := LocalMetaData.FindID(ID);
            if PNote <> Nil then begin
                if PNote^.Deleted then
                   RemoteMetaData.Items[I]^.Action:=SyDeleteRemote;       // I have deleted that already.
            end else RemoteMetaData.Items[I]^.Action:=SyDownload;         // its a new note from elsewhere
        end;
        if RemoteMetaData.Items[I]^.Action = SyUnset then begin
            debugln('---- Note on Sync List with unassigned action ----');
            debugln('ID=' + RemoteMetaData.Items[I]^.ID);
            debugln('sync.pas CheckUsingRev() - please report this message');
        end;
        if RemoteMetaData.Items[I]^.Action = SyUpLoadEdit then begin
            RemoteMetaData.Items[I]^.LastChange := GetNoteLastChangeSt(NotesDir + ID + '.note', ErrorString);
            // debugln('=========== LCD is [' + RemoteMetaData.Items[I]^.CreateDate + ']');
        end;
    end;
end;


{ ========================  N O T E   M O V E M E N T    M E T H O D S ================}

function TSync.DoDownloads() : boolean;
{var
    I : integer; }
begin
    Result := Transport.DownloadNotes(RemoteMetaData);
	if Result = false then begin
       self.ErrorString:= Transport.ErrorString;
       debugln('ERROR - Download Notes reported ' + ErrorString);
	end;
    if DebugMode then debugln('Downloaded notes.');
end;

function TSync.DoDeleteLocal() : boolean;
var
    I : integer;
begin
    for I := 0 to RemoteMetaData.Count -1 do begin
        if RemoteMetaData.Items[i]^.Action = SyDeleteLocal then begin
            if FileExists(NotesDir + RemoteMetaData.Items[i]^.ID + '.note') then
                if CopyFile(NotesDir + RemoteMetaData.Items[i]^.ID + '.note', NotesDir + PathDelim
                        + 'Backup' + Pathdelim + RemoteMetaData.Items[i]^.ID + '.note') then
                    DeleteFile(NotesDir + RemoteMetaData.Items[i]^.ID + '.note');
        end;
    end;
    result := true;
end;

function TSync.DoDeletes() : boolean;
var
    Index : integer;
    //Cnt : integer = 0;
begin
    if DebugMode then
       Debugln('DoDeletes Count = ' + inttostr(RemoteMetaData.Count));
    for Index := 0 to RemoteMetaData.Count - 1 do begin
        if RemoteMetaData[Index]^.Action = SyDeleteRemote then begin
            if DebugMode then
               Debugln('Delete remote note : ' + RemoteMetaData.Items[Index]^.ID);
            if not TestRun then begin
               if not Transport.DeleteNote(RemoteMetaData.Items[Index]^.ID,
                            RemoteMetaData.Items[Index]^.Rev) then begin
                               debugln('ERROR, TSync.DoDeletes got back false from transport');
                               Exit(False);
							end;
			end;
        end;
	end;
	Result := true;
end;

function TSync.DoUploads() : boolean;
var
    Uploads : TstringList;
    Index : integer;
begin
    if DebugMode then begin
        debugln('TSync.DoUploads() - Doing uploads and Remote ServerRev is ' + inttostr(Transport.RemoteServerRev));
        debugln('TSync.DoUploads() - We have ' + RemoteMetaData.Count.ToString + ' notes to consider');
    end;
    try
        Uploads := TstringList.Create;
        for Index := 0 to RemoteMetaData.Count -1 do begin
            if RemoteMetaData.Items[Index]^.Action in [SyUploadEdit, SyUploadNew] then begin
               Uploads.Add(RemoteMetaData.Items[Index]^.ID);
               RemoteMetaData.Items[Index]^.Rev := Transport.RemoteServerRev + 1;
			end;
		end;
        if DebugMode then debugln('TSync.DoUploads() - We have ' + Uploads.Count.ToString + ' notes to upload');
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

function TSync.FGetTokenExpire(): string;
begin
    {$ifndef TESTRIG}                              // github sync not working in test rig yet
    if assigned(Transport)
        and (Transport is TGitHubSync) then
        Result := TGitHubSync(Transport).TokenExpires
    else {$endif}Result := 'not applicable';                       // should never happen
end;

function TSync.FGetTransRemoteAddress(): string;
begin
    Result := Transport.RemoteAddress;
end;




{ =================  S T A R T   U P   M E T H O D S ============== }

function TSync.SetTransport(Mode: TSyncTransport) : TSyncAvailable;
begin
    if ProgressProcedure <> nil then
       ProgressProcedure('Set Transport');
    TransportMode := Mode;
    NotesDir := AppendPathDelim(NotesDir);
    ConfigDir := AppendPathDelim(ConfigDir);
    ErrorString := '';
    FreeAndNil(Transport);
    case Mode of
        SyncFile : begin
                        {$ifndef TESTRIG}
                        //SyncAddress := AppendPathDelim(Sett.GetSyncFileRepo());
                        if SyncAddress = '' then                                // It may have been provided by settings in a new setup
                            SyncAddress := Sett.SyncInfo[ord(SyncFile)].RemoteAddress;
                        {$endif}
                        Transport := TFileSync.Create(ProgressProcedure);
	               end;

        SyncMisty : begin
                        if SyncAddress = '' then
                            SyncAddress := Sett.SyncInfo[ord(SyncMisty)].RemoteAddress;
                        Transport := TMistySync.Create(ProgressProcedure);
                        ConfigDir := ConfigDir + Sett.SyncInfo[ord(SyncMisty)].DisplayName + PathDelim;
                        ForceDirectory(ConfigDir);
                    end;

        {$ifndef TESTRIG}
        SyncGitHub :  begin
                        Transport := TGithubSync.Create(ProgressProcedure);
                        if TGitHubSync(Transport).FailedToResolveIPs then begin
                            ErrorString := Transport.ErrorString;
                            debugln('TSync.SetTransport ErrorString is '+ ErrorString);
                            exit(SyncNetworkError);
                        end;
                        Transport.Password := Password;
                        Transport.Username := UserName;
                        ConfigDir := ConfigDir + SyncGithub.ToString + PathDelim;
                        ForceDirectory(ConfigDir);
                      end;
        {$endif}
    end;
//    Transport.ProgressProcedure := ProgressProcedure;
    Transport.Password := Password;
    Transport.NotesDir := NotesDir;
    Transport.DebugMode := DebugMode;
    Transport.ConfigDir := ConfigDir;                               // unneeded I think ??
    Transport.RemoteAddress:= SyncAddress;                          // happens _before_ Trans.SetTransport
    Result := Transport.SetTransport();                             // in github, this will (re)set Transport.RemoteAddress
    ErrorString := Transport.ErrorString;
    if DebugMode then begin
        debugln('Remote address is ' + SyncAddress);
        debugln('Local Config ' + ConfigDir);
        debugln('Notes dir ' + NotesDir);
        debugln('SetTransport ErrorString : ' + ErrorString);
	end;
end;

function TSync.TestConnection(): TSyncAvailable;
{var
    XServerID : string;}
begin
    if ProgressProcedure <> nil then ProgressProcedure('Test Transport');
    if RepoAction = RepoNew then begin
        LocalLastSyncDate := 0;
        LocalLastSyncDateSt := '';
        Transport.RemoteServerRev:=-1;
        Transport.ANewRepo:= True;
        // means we should prepare for a new repo (but not make it yet), don't check for files.
    end;
    if RepoAction = RepoUse then begin
	    if not ReadLocalManifest() then exit(SyncXMLError);    // Error in local mainfest, OK or no manifest=true
        if LocalLastSyncDateSt = '' then begin
            ErrorString := 'Failed to read local manifest, is this an existing sync ?';
            debugln('ReadLocalManifest set an empty LocalLastSyncDateSt, probably local manifest does not exist.');
            exit(SyncNoLocal);
        end;
	    LocalLastSyncDate :=  TB_GetGMTFromStr(LocalLastSyncDateSt);
	    if LocalLastSyncDate < 1.0 then begin
		    ErrorString := 'Invalid last sync date in local manifest [' + LocalLastSyncDateSt + ']';
            debugln('Invalid last sync date in ' + ConfigDir + ManPrefix + 'manifest.xml');
		    exit(SyncXMLError);
        end;
    end;
    if RepoAction = RepoJoin then begin
        LocalLastSyncDate := 0;
        LocalLastSyncDateSt := '';
    end;
    if TransMode = SyncMisty then
       TMistySync(Transport).RMetaData := RemoteMetaData;     // Misty needs access to this mid TestTransport
    Result := Transport.TestTransport(not TestRun);                                   // In Misty, this will also do ReadRemoteManifest

    if Result <> SyncReady then begin
      ErrorString := Transport.ErrorString;
      // We get the next line every time we start a new sync repo because we always try a join first.  Not helpful.
      // debugln('TSync.TestConnection() : failed Transport.TestTransport, maybe a new connection ? ' + SyncAvailableString(Result));
      exit;
    end;
    if DebugMode then begin
        debugln('CurrRev=' + inttostr(CurrRev) + '   Last Sync=' + LocalLastSyncDateSt
                        + '   Local Entries=' + inttostr(LocalMetaData.Count));
        debugln('Config=' + ConfigDir + ' NotesDir=' + NotesDir);
    end;
    if RepoAction = RepoUse then
	    if Transport.ServerID <> LocalServerID then begin
	        ErrorString := 'ServerID Mismatch';
            if DebugMode then
                debugln('ERROR Server ID Mismatch Remote ' + Transport.ServerID + ' and local ' + LocalServerID);
	        exit(SyncMismatch);
		end;
    if RepoAction = RepoJoin then begin
        LocalServerID := Transport.ServerID;
    end;
    if Result = SyncReady then begin
        if not IDLooksOK(Transport.ServerID) then begin
            ErrorString := 'An invalid serverID detected [' + Transport.ServerID + ']';
            debugln('ERROR - completed TestConnection but ServerID is invalid ['
                    + Transport.ServerID + ']');
            Result :=  SyncBadError;
        end;
    end  else
        debugln('TSync.TestConnection() : do not have SyncReady at end of method, ' + SyncAvailableString(Result));
end;

function TSync.LoadRepoData(ForceLCD : boolean): boolean;
begin
    if TransportMode = SyncMisty then exit(true);         // dont need this in Misty, TestTransport does it. In Misty, always have LCD
    if ProgressProcedure <> nil then ProgressProcedure('Load Remote Repo');
    Result := True;
    FreeAndNil(RemoteMetaData);
    RemoteMetaData := TNoteInfoList.Create;
    if not assigned(Transport) then
       debugln('Transport is not assigned');
    if not assigned(RemoteMetaData) then
       debugln('RemoteMetaData is not assigned');
    if TransportMode = SyncGitHub then exit();
    case RepoAction of
        RepoUse  : Result := Transport.GetRemoteNotes(RemoteMetaData, False);
        RepoJoin : Result := Transport.GetRemoteNotes(RemoteMetaData, ForceLCD);
        // Note, RepoNew does not apply here, if we are making a new one, we assume its empty.
    end;
    if DebugMode then begin
        debugln('LoadRepoData found ' + inttostr(RemoteMetaData.Count) + ' remote notes');
        // DisplayNoteInfo(RemoteMetaData, 'NoteMetaData just after Loading');
    end;
    // We do not load remote metadata when creating a new repo !
end;

                        { ---------- The Lets Do it Functions ------------- }

//function TSync.StartSync(): boolean;
function TSync.GetSyncData() : boolean;
    // Tick1, Tick2, Tick3, Tick4 : Dword;
begin

    // DisplayNoteInfo(RemoteMetaData, 'Remote - At start of GetSyncData');

    Result := True;
    if ProgressProcedure <> nil then ProgressProcedure('Starting Sync');
    // TestRun := True;
    if not LoadRepoData(False) then exit(False);     // don't get LCD until we know we need it.
    case RepoAction of
        RepoUse : begin
                        {$ifndef TESTRIG}
                        if TransportMode = SyncGithub then
                            TGithubSync(Transport).AssignActions(RemoteMetaData, LocalMetaData, TestRun)
                        else {$endif} CheckUsingRev();
                        // note that the Android sync used to use CheckUsingLCD(False) do we still need it ?
                        CheckRemoteDeletes();
                        CheckLocalDeletes();
                  end;
        RepoJoin :  {$ifndef TESTRIG}
                    if TransportMode = SyncGithub then
                        TGithubSync(Transport).AssignActions(RemoteMetaData, LocalMetaData, TestRun)
                    else {$endif}                                           // Github will always have a LCD but not usable for this purpose.
                        if not CheckUsingLCD(True) then begin               // at least 1 possible clash
                            LoadRepoData(True);                             // start again, getting LCD this time
                            CheckUsingLCD(False);
                        end;
        RepoNew : begin
                        freeandNil(RemoteMetaData);
                        RemoteMetaData := TNoteInfoList.Create;             // clean out the list and start again
                        {$ifndef TESTRIG}
                        if TransportMode = SyncGithub then
                            TGithubSync(Transport).AssignActions(RemoteMetaData, LocalMetaData, TestRun)
                        {$endif}
                  end
    end;

    //DisplayNoteInfo(RemoteMetaData, 'RemoteMetaData before CheckNewNotes()');
    if TransportMode <> SyncGithub
        then CheckNewNotes();

    //DisplayNoteInfo(RemoteMetaData, 'RemoteMetaData before  CheckMetaData()');
    CheckMetaData();

//    if DebugMode then DisplayNoteInfo(RemoteMetaData, 'RemoteMetaData after CheckMetaData()');

// DisplayNoteInfo(RemoteMetaData, 'Remote - At end of TSync.GetSyncData');


    if TestRun then begin
        if ProgressProcedure <> nil then ProgressProcedure('Finished Test Run');
        exit();
    end;
    result := ProcessClashes();       // Will be false if in AutoSync mode AND we have a clash.
end;

function TSync.UseSyncData(DoClash : boolean=false) : boolean;
var
    NewRev : boolean = false;
begin
    if DoClash then ProcessClashes();                    // When called during a Join that ran in TestOnly mode GetSyncData skps this
    if DebugMode then debugln('TSync.UseSyncData - started actual sync, is github=' + booltostr(TransPortMode=SyncGitHub, true));
    if not DoDownLoads() then exit(SayDebugSafe('TSync.StartSync - failed DoDownLoads'));
    if TransPortMode <> SyncGitHub then
       if not WriteRemoteManifest(NewRev) then exit(SayDebugSafe('TSync.StartSync - failed early WriteRemoteManifest'));
    if not DoDeletes() then exit(SayDebugSafe('TSync.StartSync - failed DoDeletes'));

    //DisplayNoteInfo(RemoteMetaData, 'Remote - Before DoUpLoads in UseSyncData');

    if not DoUploads() then exit(SayDebugSafe('TSync.StartSync - failed DoUploads'));

    // If DoUpLoads fails mid list, then some notes will have been uploaded and some not.
    // So, we SHOULD continue on to update manifests.
    // So, when DoUpLoads fails to upload a note (eg a Markdown failure, temp network error)
    // it should continue to try the reminder and remove the failed entry from its list

    if not DoDeleteLocal() then exit(SayDebugSafe('TSync.StartSync - failed DoDeleteLocal'));
    if TransportMode = SyncGithub then
        if not Transport.DoRemoteManifest('', RemoteMetaData) then exit(SayDebugSafe('TSync.StartSync - failed late WriteRemoteManifest'));
    if not WriteLocalManifest(true, NewRev) then
        WriteLocalManifest(false, false);   // write a recovery local manifest. Downloads only noted.

(*    if DoDownLoads() then
        if TransPortMode <> SyncGitHub then
            if WriteRemoteManifest(NewRev) then
                if DoDeletes() then
                    if DoUploads() then
                        if DoDeleteLocal() then
                            if TransportMode = SyncGithub then

                                if not WriteLocalManifest(true, NewRev) then
                                    WriteLocalManifest(false, false);   // write a recovery local manifest. Downloads only noted.   *)
    if ProgressProcedure <> nil then ProgressProcedure('Sync Complete');
    Result := True;
end;

procedure TSync.AdjustNoteList();
// Another version of this exists in SyncGUI, maybe merge ?
var
    DeletedList, DownList : TStringList;
    Index : integer;
begin
    DeletedList := TStringList.Create;
    DownList := TStringList.Create;
 	with RemoteMetaData do begin
		for Index := 0 to Count -1 do begin
            if Items[Index]^.Action = SyDeleteLocal then
                DeletedList.Add(Items[Index]^.ID);
            if Items[Index]^.Action = SyDownload then
                DownList.Add(Items[Index]^.ID);
        end;
    end;
    {$ifndef TESTRIG}                                         // this relates to recording in note_lister. NOT in testRig
    if (DeletedList.Count > 0) or (DownList.Count > 0) then
        SearchForm.ProcessSyncUpdates(DeletedList, DownList);
    {$endif}
    FreeandNil(DeletedList);
    FreeandNil(DownList);
end;


function TSync.AutoSetUp(Mode : TSyncTransport) : boolean;
{ we must have set, after creation and before calling this -
    debugmode
    NotesDir
    ConfigDir
    Password
    UserName  }
begin
    Threaded := True;
    RepoAction:= RepoUse;
    if SyncReady <> SetTransport(Mode) then
        exit(False);
    //debugln({$I %FILE%}, ', ', {$I %CURRENTROUTINE%}, '(), line:', {$I %LINE%}, ' : Testing Connection.');
    if TestConnection() <> SyncReady then begin
        //debugln({$I %FILE%}, ', ', {$I %CURRENTROUTINE%}, '(), line:', {$I %LINE%}, ' : ', 'Test Transport Failed.');
        exit(false);
    end;
    TestRun := False;
    Result := True;
    // PostMessage(sett.Handle, WM_SYNCBLOCKFINISHED,  2, 0);    // ThreadTest
end;

// ------------------  M A N I F E S T    R E L A T E D  -----------------------



function TSync.WriteRemoteManifest(out NewRev : boolean): boolean;
var
    OutFile: TextFile;
    Index : integer;
    NewRevString : string;
begin
    if DebugMode then debugln('Ready to do remote Manifest');
    if not IDLooksOK(Transport.ServerID) then exit(false);        // already checked but ....
    result := true;
    NewRev := False;
    for Index := 0 to RemoteMetaData.Count - 1 do begin
        if RemoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit, SyDeleteRemote] then begin
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
            writeln(OutFile, '" server-id="' + Transport.ServerID + '">');
	        for Index := 0 to RemoteMetaData.Count - 1 do begin
                if RemoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit, SyDownLoad, SyNothing] then begin
	                write(OutFile, '  <note id="' + RemoteMetaData.Items[Index]^.ID + '" rev="');
                    if RemoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit] then
	                    write(OutFile, NewRevString + '"')
	                else write(OutFile, inttostr(RemoteMetaData.Items[Index]^.Rev) + '"');
                    if RemoteMetaData.Items[Index]^.LastChange = '' then
                       writeln(OutFile, ' />')
                    else writeln(OutFile, ' last-change-date="'
                            + RemoteMetaData.Items[Index]^.LastChange + '" />');
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
    if debugmode then
       debugln('Have written remote manifest to ' + ConfigDir + ManPrefix + 'manifest.xml-remote');
end;

//OK, this needs to call its code for each valid android manifest file.

function TSync.DeleteFromThisManifest(const FullFileName, ID : string): boolean;
var
    i : integer;
    Found : boolean = false;
begin
    // if debugmode then debugln('DeleteFromThisManifest, searching for ' + ID);
	if not ReadLocalManifest(FullFileName) then exit(false); 	// read a local manifest
    if debugmode then debugln('DeleteFromThisManifest lines = ' + inttostr(LocalMetaData.count));
    if LocalMetaData.count = 0 then exit(True);
    //if debugmode then debugln('DeleteFromThisManifest searcing for ' + ID);
    for I := 0 to LocalMetaData.count -1 do  begin
        // debugln('DeleteFrom.. Testing ' +  LocalMetaData.Items[i]^.ID);
    	if LocalMetaData.Items[i]^.ID  = ID then begin
        	LocalMetaData.Items[i]^.Deleted:= True;
            LocalMetaData.Items[i]^.Title :=
            		GetNoteTitle(ID, -1);       // -1 says don't try and download if its not local
            Found := True;
            if debugmode then debugln('DeleteFromThisManifest deleted ' + ID + ' from ' + FullFileName);
            break;
        end;
	end;
    if Found then ReWriteLocalManifest(FullFileName);
    Result := True;
end;

function TSync.ReWriteLocalManifest(const FullFileName : string) : boolean;
var
    OutFile: TextFile;
    Index : integer;
begin
    AssignFile(OutFile, FullFileName + '-local');
    try
	    try
		    Rewrite(OutFile);
            writeln(OutFile, '<?xml version="1.0" encoding="utf-8"?>');
            writeln(Outfile, '<manifest xmlns="http://beatniksoftware.com/tomboy">');
            writeln(OutFile, '  <last-sync-date>' + LocalMetaData.LastSyncDateSt + '</last-sync-date>');
            write(OutFile, '  <last-sync-rev>"' +  inttostr(LocalMetaData.LastRev));
            writeln(OutFile, '"</last-sync-rev>');
            writeln(OutFile, '  <server-id>"' + LocalMetaData.ServerID + '"</server-id>');
            writeln(OutFile, '  <note-revisions>');
		    for Index := 0 to LocalMetaData.Count - 1 do begin
                if not LocalMetaData[Index]^.Deleted then begin
                    write(Outfile, '    <note guid="' + LocalMetaData[Index]^.ID + '" latest-revision="');
                    write(OutFile, inttostr(LocalMetaData[Index]^.Rev) + '" ');
                    if LocalMetaData[Index]^.Sha <> '' then
                        write(Outfile, 'sha="' + LocalMetaData[Index]^.Sha + '" ');





                    writeln(Outfile, '/>');
				end;
			end;
            writeln(OutFile, '  </note-revisions>'#10'  <note-deletions>');
 		    for Index := 0 to LocalMetaData.Count - 1 do begin
                if LocalMetaData[Index]^.Deleted then begin
                    write(Outfile, '    <note guid="' + LocalMetaData[Index]^.ID + '" title="');
                    write(OutFile, RemoveBadXMLCharacters(LocalMetaData[Index]^.Title, True));      // cannot allow " or ' in an xml attribute
                    writeln(Outfile, '" />');
				end;
			end;
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
    { if debugmode then
       debugln('Have written local manifest to ' + FullFileName + '-local'); }
	if not TestRun then begin
        if FileExistsUTF8(FullFileName + '-old') then
           DeleteFileUTF8(FullFileName + '-old');
       renamefileutf8(FullFileName, FullFileName + '-old');
       renamefileutf8(FullFileName + '-local', FullFileName);      // ToDo : check for errors, Windows rename will not overwrite !
    end;
    result := True;
end;

function TSync.WriteLocalManifest(const WriteOK, NewRev : boolean ): boolean;
{ We try and provide some recovery from a fail to write to remote repo. It should
  not happen but ... If WriteOk is false we write back local manifest that still
  mentions the previous deleted files and does not list locally new and changed
  files. Such files retain their thier previous rev numbers. Test !      }
var
    OutFile: TextFile;
    Index : integer;
    IncRev : integer = 0;
begin
    if not IDLooksOK(Transport.ServerID) then exit(false);        // already checked but ....
    result := true;
    if WriteOK and NewRev then IncRev := 1 else IncRev := 0;
    AssignFile(OutFile, ConfigDir + ManPrefix + 'manifest.xml-local');  // ManPrefix is '' for most Modes.
    try
	    try
		    Rewrite(OutFile);
            writeln(OutFile, '<?xml version="1.0" encoding="utf-8"?>');
            writeln(Outfile, '<manifest xmlns="http://beatniksoftware.com/tomboy">');
            writeln(OutFile, '  <last-sync-date>' + TB_GetLocalTime + '</last-sync-date>');
            write(OutFile, '  <last-sync-rev>"' + inttostr(Transport.RemoteServerRev + IncRev));
            writeln(OutFile, '"</last-sync-rev>');
            writeln(OutFile, '  <server-id>"' + Transport.ServerID + '"</server-id>');
            writeln(OutFile, '  <note-revisions>');
		    for Index := 0 to RemoteMetaData.Count - 1 do begin
                if RemoteMetaData[Index]^.Action in [SyUploadNew, SyUpLoadEdit, SyDownLoad, SyNothing] then begin
                    if (not WriteOK) and (RemoteMetaData[Index]^.Action = SyUpLoadNew) then continue;
                    write(Outfile, '    <note guid="' + RemoteMetaData[Index]^.ID + '" latest-revision="');
                    write(OutFile, inttostr(RemoteMetaData[Index]^.Rev) + '" ');
                    if RemoteMetaData[Index]^.Sha <> '' then
                       write(Outfile, 'sha="' + RemoteMetaData[Index]^.Sha + '" ');
                    if RemoteMetaData[Index]^.ForceUpload then
                       write(Outfile, 'force-upload="yes" ');               // we failed to upload this note this time, next time better ?
                    writeln(Outfile, '/>');
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
	if not TestRun then begin
        if fileexists(ConfigDir + ManPrefix + 'manifest.xml-old') then
           deletefile(ConfigDir + ManPrefix + 'manifest.xml-old');
        renamefile(ConfigDir + ManPrefix + 'manifest.xml', ConfigDir + ManPrefix + 'manifest.xml-old');
        copyfile(ConfigDir + ManPrefix + 'manifest.xml-local', ConfigDir + ManPrefix + 'manifest.xml');
    end;
    if debugmode then
       debugln('Have written local manifest to [' + ConfigDir + '] [' + ManPrefix + '] [' + 'manifest.xml]');
end;

function TSync.ReadLocalManifest(const FullFileName : string = '') : boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node, ANode : TDOMNode;
    j : integer;
    NoteInfoP : PNoteInfo;                  // pointer to the record we will shove into LocalMetaDAta
    RevStr, ServerID, ManifestFile : string;
begin
    Result := true;
    ErrorString := '';
    freeandNil(LocalMetaData);
    LocalMetaData := TNoteInfoList.Create;
    if FullFileName = '' then begin         // get a FFN when using this unit to edit local man after a file delete
        ManifestFile := ConfigDir + ManPrefix + 'manifest.xml';
        if not FileExists(ManifestFile) then begin
            if DebugMode then debugln('TSync.ReadLocalManifest did not find Local Manifest ['
                + ManifestFile + '], assuming new sync ' + TransMode.ToString + ' ?');
                // + ManifestFile + '], assuming new sync ' + SyncTransportName(TransMode) + ' ?');
            LocalLastSyncDateSt := '';
            CurrRev := 0;
            exit(True);                 // Its not an error, just never synced before
        end;
    end else ManifestFile := FullFileName;      // existance is checked before calling.
    if DebugMode then debugln('Reading local mainfest ' + ManifestFile);
    try
    	try
    		ReadXMLFile(Doc, ManifestFile);
            Node := Doc.DocumentElement.FindNode('last-sync-date');
            if assigned(Node) then begin
                LocalLastSyncDateSt := Node.FirstChild.NodeValue;
                LocalMetaData.LastSyncDateSt := Node.FirstChild.NodeValue;
                if not MyTryISO8601ToDate(LocalLastSyncDateSt, LocalMetaData.LastSyncDate, True) then
                    LocalMetaData.LastSyncDate := 0.0;
            end else begin
                LocalLastSyncDateSt := '';
                LocalMetaData.LastSyncDateSt := '';
                LocalMetaData.LastSyncDate := 0.0;
                debugln('ERROR, cannot find LSD in ' + ManifestFile);
            end;
            Node := Doc.DocumentElement.FindNode('server-id');
            if Assigned(Node) then
                ServerID := Node.FirstChild.NodeValue
            else  ServerID := '';                                   // Thats untested but should never happen
            if ServerID[1] = '"' then
               ServerID := copy(ServerID, 2, 36);
            if not IDLooksOK(ServerID) then begin
                ErrorString := 'Local manifest contains an invalid serverID [' + ServerID +']';
                debugln('ERROR - local manifest contains an invalid serverID [' + ServerID +']');
                debugln('Maybe you should delete it and rejoin the Repo. ?');
                exit(false);
            end;
            LocalServerID := ServerID;
            LocalMetaData.ServerID:= ServerID;
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
            LocalMetaData.LastRev := CurrRev;
            if ErrorString <> '' then begin
                CurrRev := 0;
                LocalLastSyncDateSt := '';
                LocalMetaData.LastSyncDateSt:='';
                exit(False);
			end;
			NodeList := Doc.DocumentElement.FindNode('note-revisions').ChildNodes;
            if assigned(NodeList) then
               for j := 0 to NodeList.Count-1 do begin
                   new(NoteInfoP);
                   NoteInfoP^.ID := NodeList.Item[j].Attributes.GetNamedItem('guid').NodeValue;
                   NoteInfoP^.Rev := strtoint(NodeList.Item[j].Attributes.GetNamedItem('latest-revision').NodeValue);
                   ANode := NodeList.Item[j].Attributes.GetNamedItem('sha');
                   if ANode = nil then                                                  // only possible in Github sync
                        NoteInfoP^.Sha := ''
                   else  NoteInfoP^.Sha := ANode.NodeValue;
                   ANode := NodeList.Item[j].Attributes.GetNamedItem('force-upload');
                   NoteInfoP^.ForceUpload := (ANode <> Nil);                            // only possible in Github sync
                   NoteInfoP^.Deleted := False;
                   //debugln('TSync.ReadLocalManifest ID=' + NoteInfoP^.ID + ' is forceupload ' + booltostr(NoteInfoP^.ForceUpload, true));
                   LocalMetaData.Add(NoteInfoP);
			   end;
            NodeList := Doc.DocumentElement.FindNode('note-deletions').ChildNodes;
            if assigned(NodeList) then
               for j := 0 to NodeList.Count-1 do begin
                   new(NoteInfoP);
                   NoteInfoP^.ID := NodeList.Item[j].Attributes.GetNamedItem('guid').NodeValue;
                   NoteInfoP^.Title := NodeList.Item[j].Attributes.GetNamedItem('title').NodeValue;
                   NoteInfoP^.Deleted := True;
                   NoteInfoP^.Sha := '';
                   NoteInfoP^.ForceUpload := false;
                   LocalMetaData.Add(NoteInfoP);
   			   end;
		finally
            Doc.Free;
		end;
	except
      on EAccessViolation do begin         // probably means we did not find an expected attribute
              ErrorString := 'Error in local mainfest';
              CurrRev := 0;
              LocalMetaData.LastRev:=0;
              LocalLastSyncDateSt := '';
              LocalMetaData.LastSyncDateSt:='';
              Result := false;
          end;
	end;
    // if Debugmode then DisplayNoteInfo(LocalMetaData, 'LocalMetaData');
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

end.

