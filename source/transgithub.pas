unit transgithub;
{
    Copyright (C) 2017-2024 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT
    ------------------
}


{ A class that provides transport for a tomboy-ng to github sync.



  -------------- P E R S O N A L    A C C E S S   T O K E N S ------------------

  https://github.com/settings/tokens to generate a Person Access Token

  From a logged in to github page, click my pretty picture, top right. Then 'Settings'.
  on left sidebar, click 'Developer",  "Personal Access Token".
}


{  Normal Sync Cycle for GitHub Sync eg, as driven by SyncGui.ManualSync
   ---------------------------------------------------------------------

Create ASync
Assign Proceed and Progress methods
Assign various config data
Async.RepoAction := RepoUse
Async.SetTransport
    Assigns and configures Transport
ASync.TestConnection
    Reads Local Manifest, setting LocalServerID and LocalLastSyncDates
    Calls Transport.TestTransport
        Might make  new serverID
        Creates RemoteNotes data structure
        Contacts github, confirming UID and Token work, a ServerID can be found.
        Scans remote dir indexing all files found in /, /Notes and /Meta
        Reads Remote Manifest, filling RemoteNotes out with details of all remote notes.
Compares ServerIDs
ASync.StartSync
    Calls LoadRemoteRepo which calls GitHubSync.GetRemoteNotes which does nothing.
    Calls GitHubSync.AssignActions which
        Iterates over RemoteNotes adding every entry into RemoteMetaData making some initial Action guesses.
        Iterates over NoteLister (both Note List and Notebook List) adding all entries
        not already in RemoteMetaData, firming up Actions as it goes.
Calls CheckRemoteDeletes and CheckLocalDeletes that further refines Actions in RemoteMetaData
based on information from LocalManifest.
Asks the user to resolve any clashes
Calls DoDownloads, DoDeletes and DoUpLoads which call Transport to do actual remote file work.
Calls DoDeletesLocal.
Calls Transport.DoRemoteManifest to write and upload a remote manifest, based on data from TSync.RemoteMetaData list.
Calls WriteLocalManifest

Easy Peasy !

The above glosses over error handling and, importantly, how data such as last change dates
is shuffled around to ensure its in RemoteNotes when we write out remote manifests.

AssignActions()
--------------
We decide a note needs uploading because its main-notelister entry is later than the last
sync date as recorded in local manifest. OR because the new attribute, force-upload="yes"
is present.

We decide a note needs downloading because its remote SHA (obtained from gitlab) does
not match the SHA in the local manifest as recorded at previous sync.
If both, we have a clash, if neither, its a do nothing.
(Also have to deal with new notes and deleted notes...)

Downloading a Note.
-------------------
RemoteNotes will already know CData and Notebooks from reading remote manifest.
DownLoadANote will write a temp file in Note format. It first calls Downloader() which
returns with a string containing note content as JSON Base64 encoded. We decode and
drop into s stringlist. We pass that to Importer thats converts to xml.
Download (and uploads) will be retried up to 5 times if necessary.

Uploading a Note, that fails
----------------------------
Must ensure any note in repo is mentioned in both manifests (but ensure uploading next time).
The remote manifest entry should have original LCD and the SHA we got from github and
put in RemoteNotes.
The local manifest needs its SHA left as is and a new attribute, force-upload="yes".

What happens is RemoteNotes holds the existing SHA of a note, when a new one is uploaded
to overwrite it, the SHA in RemoteNotes is updated (down in SendData()) to the new github
iff upload is succssful. If unsucessful, leave it as it is (the note on github has not
changed) but set RemoteNotes item.SyncState to FAILEDUPLOAD, a str constant.

Next action is in DoRemoteManifest, it will spot the FAILEDUPLOAD msg, and set the
TSync.RemoteMetaData item.forceupload to true. Then, WriteLocalManifest will see that
and put the neecessary extra attribute in, force-upload="yes" in the local mainfest.

Manifests
---------
There are two manifests, local and remote.  The local has a global LastSyncDate and a SHA
for each Note under its control. The remote lists a title, CreateData, LastChangeDate, SHA,
format and a list of notebooks for each note in the repo.

Local Manifest is built, mainly, from TSync.RemoteMetaData relying on appropriate
data being put in there by TGithubSync.

Remote Manifest is made with data from TSync.TRemoteMetaData (from remote manifest) and
TGithubSync.RemoteNotes (based on data from github). SHA and LCD from RemoteNotes.


HISTORY :
    2021/09/20 - Changed to using JSONTools, tidier, safer, easier to read code.
    2021/09/25 - fixed how notebook lists are stored in RemoteNotes, as JSON array, not JSON data
    2021/09/27 - Implement selective sync.
    2022/10/18 - Fix markup around readme warning
    2024/05/08 - extensive rework of debugging code, retry download if it fails.
    2024/09/15 - After noting a catistrophic fail to sync that appeared to be related to a fail to
                 resolve api.github.com and observing my network is currently very flakey, a
                 number of tweeks to make more resiliant. 1) Before starting aGHSync, I try to
                 resolve (as an exercise) the hostnames. 2) Extend the downloader timeouts,
                 Client.ConnectTimeout := 8000; was 3000; Client.IOTimeout := 4000; was 0;
    2024/09/19   Ensure, after a transit network failure, that a note that failed to upload
                 is still mentioned in the manifests, else it looks like a delete.
}


{x$define DEBUG}

{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
    Classes, SysUtils, {fpjson, jsonparser, }syncutils {$ifndef TESTRIG}, trans{$endif};


type TFileFormat = (
                ffNone,               // Format not, at this stage specified
                ffEncrypt,            // File is encrypted
                ffMarkDown);          // File is in MarkDown, CommonMark

type
      PGitNote=^TGitNote;
      TGitNote = record
            FName : string;     // eg README.txt, Meta/serverid, Notes/<guid>.md
            Sha   : string;     // The SHA, we always have this.
            Title : string;     // Last known note title.
            LCDate: string;     // The Last Change Date of the note. Always can get that. (In RemoteNotes blank indicates an upload failure)
            CDate : string;     // The create Date, we may not know it if its a new note.
            Format : TFileFormat;       // How the file is saved at github, md only at present
            Notebooks : string; // empty OK, else [] or ["notebook1", "notebook2"] etc. Only needed for SyDownLoad
            SyncState : string; // Generally '' but might be 'failedupload' a failed upload, set in UpLoadNotes()
      end;

type
    { TGitNoteList }
TGitNoteList = class(TFPList)
    private
        procedure DumpList(wherefrom : string);
        function Get(Index: integer): PGitNote;
                            { Inserts just one field into GitNoteList. FName should
                            include remote path and extension (ie Notes/ABC.md). Does
                            not do format but might need to ...}
        procedure InsertData(const FName, Field, Data: string);

     public

         constructor Create();
         destructor Destroy; override;
                            // Adds an item, found in the remote dir scan JSON, to the
                            // RemoteNotes list, does NOT check for duplicates. Only gets
                            // Name (that is, FileName maybe with dir prepended) and sha.
         //function AddJItem(jItm: TJSONData; Prefix: string): boolean;


                            // Adds a new item to List, Notes and files in Meta require prefix added before being passed here
         procedure AddNewItem(const FName, Sha: string);

         function Add(AGitNote : PGitNote) : integer;
                            // Adds or updates record, FName being key.
         procedure Add(const FName, Sha: string);

                            // Selectivly inserts the supplied data into GitNoteList.
                            // Always use most fields but don't use LCD if the sha do not match.
         procedure InsertData(const GitRec : TGitNote);
                            // Returns true and sets sha if note is in List. Ignores trailing / in FName.
                            // If the Note exists in the list but its sha is not set, ret True but sha is empty
         function FNameExists(FName: string; out Sha: string): boolean;
                             { Tries to find the list item, first tries RNotesDir+ID+.md and then tries as
                             if ID is FFname. Returns a pointer to a GitNote record or nil if not found. }
         function Find(const ID: string): PGitNote;
         property Items[Index: integer]: PGitNote read Get; default;
     end;




type

{ TGitHub }

 { TGitHubSync }

  {$ifdef TESTRIG}
 TGitHubSync = class
 {$else}
 TGithubSync = class(TTomboyTrans)
 {$endif}
  private
                            { Private : Initialised in TestConnection which calls ScanRemoteRepo
                            to put an ID and sha entry in, then ReadRemoteManifest to fill in
                            cdate, format, title and notebooks.
                            AssignAction will add locally know notes and and actions.}
        RemoteNotes : TGitNoteList;

                             { Hold name of SelectiveSync Notebook, eg SyncGithub, set during a Join (depending on
                             remote repo, a New (depending on if we have a Notebook by that name) or in a Use its
                             loaded from RemoteManifest and cannot be changed. The actual name to trigger all
                             this is a constant in implementation section of transgithub. }
        SelectiveSync : string;

        SelectiveNotebookIDs : TstringList;  // May contain FNames of notes that are members of SelectiveSync notebook. Do not create or free.

        HeaderOut : string;             // Very ugly global to get optional value back from Downloader


                            // A general purpose downloader, results in JSON, if downloading a file we need
                            // to pass the Strings through ExtractContent() to do JSON and base64 stuff.
                            // This method may set ErrorMessage, it might need resetting afterwards.
                            // The two optional parameter must be used together and extract one header value.
        function Downloader(URL: string; out SomeString: String; const Header: string =''): boolean;
        function DownloaderSafe(URL: string; out SomeString: String; const Header: string=''): boolean;

        //procedure DumpJSON(const St: string; WhereFrom: string = '');

                            { Returns the LCDate string from the commit history. We ask for just the most recent
                            commit and find the datestring in the json. Expects a full file name, something
                            like 'Notes/6EC64290-4348-4716-A892-41C9DE4AEC4C.md' - should work for any format.}
        function GetNoteLCD(FFName: string): string;

                            { Used only by AssignActions, scans over NoteLister's notes adding any it finds
                            present in NoteLister but not yet in RMData. We might exclude notes not members
                            of the SyncGithub notebook if SelectiveSync contains that name. The list of
                            local notes who are members of SyncGithub might be nil is no local SyncGithub.
                            Note : we don't copy records from RMetaData to RemoteNotes is TestRun.  }
        function MergeNotesFromNoteLister(RMData: TNoteInfoList; TestRun: boolean ): boolean;

                            // Reads the (json) remote manifest, adding cdate, format and Notebooks to RemoteNotes.
                            // Assumes RemoteNotes is created, comms tested. Ret false if cannot find remote
                            // manifest.  All a best effort approach, a github created note will not be listed.
                            // The LCD is also set if the remote note has not been altered since last sync.
        function ReadRemoteManifest(): boolean;
                            // Generic Putting / Posting Method. Put = False means Post. If an FName is provided
                            // then its a file upload, record the sha in RemoteNotes.
        function SendData(const URL, BodyJSt: String; Put: boolean; FName: string = ''): boolean;
                            // Returns URL like https://api.github.com/repos/davidbannon/tb_test/contents (true)
                            // Or like          https://github.com/davidbannon/tb_test/blob/main/        (false)
                            // Requires UserName, RemoteRepoName, API version does not have trailing /
        function ContentsURL(API: boolean): string;
                            // Returns content asociated with Key at either toplevel (no third parameter) or
                            // one level down under the ItemNo indexed toplevel Key. First top level key is zero.
        function ExtractJSONField(const data, Field: string; Level1: string = ''; Level2: string = ''): string;

        function GetServerId() : string;

                            // Creates a file at remote using contents of List. RemoteFName may be
                            // something like Meta/serverid for example. Checks RemoteNotes to see if
                            // file already exists and does update node if it does. No dir is defaulted to.
                            // Makes multiple attempts to send if we get a network fail.
        function SendFile(RemoteFName: string; STL: TstringList): boolean;

                            // Makes a new remote repo if it does not exist, if called when repo
                            // does exist, will check that a serverID is present and just return false
                            // If the ServerID is NOT present, will make one and return true
        function MakeRemoteRepo() : boolean;
                            // Gets just an ID, uses NotesDir to load that into commonmark and then passes
                            // the resulting string to SendFile.  Also works for NoteBooks ??
        function SendNote(ID: string): boolean;
                            // Scans the top level of the repo and then Notes and Meta recording in me and
                            // RemoteNotes the filename and sha for every remote file it finds.
                            // Calls SendFile() which makes multiple attemps to send if network fails.
        function ScanRemoteRepo() : boolean;

                            // Returns true if it has written temp file named ID.note-temp in Note format in the
                            // NotesDir. Assumes it can write in NotesDir. If FFName provided, saves there instead.
                            // FFName, if provided, must include path and extension and must be writable.
        function DownloadANote(const NoteID: string; FFName: string = ''): boolean;


  public
        //UserName : string;
        //RemoteRepoName : string;                // eg tb_test, tb_notes
        FailedToResolveIPs : boolean;      // says we failed to resolve (unused) IPs for github. Don't proceed.
        TokenExpires : string;                  // Will have token expire date after TestTransport()
        {$ifdef TESTRIG}                        // These are all defined in trans, need to provide them is in TestRig mode
        RemoteServerRev : integer;
        ServerID : string;
        ErrorString : string;
        Password    : string;
        ANewRepo : boolean;
        ProgressProcedure : TProgressProcedure;
        RemoteAddress : string;
        {$endif}

        (* ------------- Defined in parent class ----------------
        Password : string;          // A password for those Transports that need one.
        DebugMode : boolean;
        ANewRepo : Boolean;         // Indicates its a new repo, don't look for remote manifest.
        ErrorString : string;       // Set to '' is no errors.
        NotesDir, ConfigDir : string;     // Local notes directory
        RemoteAddress : string;     // A url to network server or 'remote' file directory for FileSync
        ServerID : string;          { The current server ID. Is set with a successful TestTransport call. }
        RemoteServerRev : integer;  { The current Server Rev, before we upload. Is set with a successful  TestTransport call. }
        *)

        constructor Create(PP : TProgressProcedure = nil);
        destructor Destroy; override;

        // --------------- Methods required to be here by Trans ----------------

                                { GitHub - tries to contact Github, testing UserName, Token and tries to scan the
                                remote files putting ID and SHA into RemoteNotes.  If WriteNewID is true, tries
                                to create repo first. Does no fill in LCD, will need to be done, note by note later.
                                Might return SyncNetworkError, SyncNoRemoteMan, SyncReady, SyncCredentialError  }
        function TestTransport(const WriteNewServerID: boolean = False): TSyncAvailable;   {$ifndef TESTRIG} override;{$endif}

                                { Github : Checks Temp dir, should empty it, ret TSyncReady or SyncBadError
                                Sets RemoteAddress (so must have username available) }
        function SetTransport() : TSyncAvailable;  {$ifndef TESTRIG} override;{$endif}


                                 { Github : This is just a stub here, does nothing. We populate RemoteNotes in AssignAction()}
        function GetRemoteNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean;  {$ifndef TESTRIG} override;{$endif}

                                { GitHub : after downloading a note we record its LCDate in RemoteNotes
                                because it will be needed to write the remote manifest a bit later }
        function DownloadNotes(const DownLoads: TNoteInfoList): boolean;   {$ifndef TESTRIG} override;{$endif}

        function DeleteNote(const ID : string; const ExistRev : integer) : boolean;  {$ifndef TESTRIG} override;{$endif}

                                 {Github : we expect a list of plain IDs, we update the GUI Progress indicator
                                 here. Pass SendNote an ID, downstream look after updating RemoteNotes sha data
                                 but we set the LCDate (in RemoteNotes) here.  }
        function UploadNotes(const Uploads: TStringList): boolean;  {$ifndef TESTRIG} override;{$endif}


                                {GitHub : Has to make Meta/mainfest.json and README.md. ignores RevNo
                                and the passed manifest, We must have the RemoteMetaData which tells us
                                which notes apply, CDate, LCD etc. Notebooks comes from NoteLister. Must
                                be called after syncing done so it can copy SHA data to RemoteMetaData.
                                Make just a flat index but in future, some sort of notebook organisation.
                                Generate suitable content, upload it, README.md to Github. }
        function DoRemoteManifest(const RemoteManifest : string; MetaData : TNoteInfoList = nil) : boolean; {$ifndef TESTRIG} override;{$endif}

                                { Github - Downloads indicated note to temp dir, returns FFname is OK
                                The downloaded file should be reusable in this session if its later needed }
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; {$ifndef TESTRIG} override;{$endif}

                                { Public - but not defined in Trans.
                                Gets passed both Remote and Local MetaData lists, makes changes to only Remote.
                                Relies on RemoteNotes, LocalMeta and NoteLister to fill in all details  (inc
                                Action) of all notes that are currently on remote server. Then  scans  over
                                NoteLister adding any notes it finds that are not already in RemoteMetaData and
                                marks them as SyUploads. Also adds the new NoteLister notes to RemoteNotes if
                                NOT a TestRun.}
        function AssignActions(RMData, LMData: TNoteInfoList; TestRun: boolean): boolean;

        procedure Test();

end;


// =============================================================================

implementation

uses
    {$if (FPC_FULLVERSION>=30200)}  opensslsockets, {$endif}  // only available in FPC320 and later
    {$ifdef LCL}  lazlogger, {$endif}                        // trying to not be dependent on LCL
    fphttpclient, httpprotocol, base64,
    LazUTF8, LazFileUtils, fpopenssl, ssockets, {ssockets,} DateUtils, fileutil,
    CommonMark, import_notes,
    Note_Lister, TB_Utils, jsontools, ResourceStr,
    resolve;                // because I am playing with eg gethostbyname()

const
//  UPLOADNOTE = 'PleaseUploadThisNote';  // Left in the local manfest item.sha when this note is still waiting to be uploaded.
  FAILEDUPLOAD='failedupload';          // left in TGitNote item.SyncState to indicate UploadNotes() failed on this note
  MaxNetTries = 5;                      // number of re-tries when a net operation fails
  GitBaseURL='https://github.com/';
  BaseURL='https://api.github.com/';
  RNotesDir='Notes/';
  RMetaDir='Meta/';
  RemoteRepoName : string ='tb_notes';         // Set EnvVar, eg TB_GITHUB_REPO="tb_alt" to change
  {$ifdef TESTRIG}
  NotesDir='/home/dbannon/Pascal/GithubAPI/notes/';
  UserName='davidbannon';
  DebugMode=true;
  {$endif}
  TempDir='Temp/';
  // see also UserName (eg davidbannon) and RemoteRepoName (eg tb_test)



// ================================ TGitNoteList ===============================

function TGitNoteList.Get(Index: integer): PGitNote;
begin
    Result := PGitNote(inherited get(Index));
end;

procedure TGitNoteList.DumpList(wherefrom: string);
var
    i : integer = 0;
    {Notebooks,} Format : string = '';
begin
    SayDebugSafe('');
    if Wherefrom <> '' then
        SayDebugSafe('-------- TransGithub RemoteNotes ' + Wherefrom + '----------');
    while i < count do begin
        case Items[i]^.Format of
            ffNone : Format := 'not set';
            ffEncrypt : Format := 'Encrypt';
            ffMarkDown : format := 'Markdown';
        end;
        SayDebugSafe('List - FFName=[' + Items[i]^.FName + '] Sha=' + Items[i]^.Sha + ' LCDate=' + Items[i]^.LCDate);
        SaydebugSafe('       CDate=' + Items[i]^.CDate + '  Format=' + Format + ' Title=' + Items[i]^.Title + ' Notebooks=' + Items[i]^.Notebooks);
        inc(i);
    end;
end;

constructor TGitNoteList.Create();   { uses resolve unit }
begin
     inherited Create;
end;

destructor TGitNoteList.Destroy;
var
    i : integer;
begin
    for I := 0 to Count-1 do begin
    	dispose(Items[I]);
	end;
    inherited Destroy;
end;

procedure TGitNoteList.AddNewItem(const FName, Sha: string);
var
    PNote : PGitNote;
begin
    new(PNote);
    PNote^.FName := Fname;
    PNote^.Title := '';
    PNote^.LCDate := '';
    PNote^.CDate := '';
    PNote^.Format := ffNone;
    PNote^.Notebooks := '';
    PNote^.sha := Sha;
    PNote^.syncstate := '';
    add(PNote);
end;

function TGitNoteList.Add(AGitNote: PGitNote): integer;
begin
    result := inherited Add(AGitNote);
end;

procedure TGitNoteList.Add(const FName, Sha: string);
var
    PNote : PGitNote;
    i : integer = 0;
begin
    while i < count do begin
        if Items[i]^.FName = FName then begin
            Items[i]^.Sha := Sha;
            exit;
        end;
        inc(i);
    end;                    // OK, must be a new entry
    new(PNote);
    PNote^.SyncState := '';
    PNote^.FName := FName;
    PNote^.Sha := Sha;
    PNote^.Title := '';
    PNote^.LCDate := '';
    PNote^.CDate := '';
    PNote^.Format := ffNone;
    Add(PNote);
end;

procedure TGitNoteList.InsertData(const FName, Field, Data : string);
var
  //i : integer = 0;
  P : PGitNote;
begin
    for p in self do begin
        if p^.FName = FName then begin
            case Field of
                'title'     : p^.Title     := Data;
                'lcdate'    : p^.LCDate    := Data;
                'sha'       : p^.Sha       := Data;
                'cdate'     : p^.CDate     := Data;
//              'format'    : p^.Format    := Data;
                'notebooks' : p^.Notebooks := Data;
                'syncstate' : p^.SyncState := Data;
            otherwise SayDebugSafe('TGitNoteList.InserData(s,s,s) asked to insert into nonexisting field : ' + Field);
            end;
            exit;
        end;
    end;
    SayDebugSafe('GitHub.InsertData(s,s,s) : Failed to find ' + FName + ' to insert data');
end;

procedure TGitNoteList.InsertData(const GitRec : TGitNote);
var
  i : integer = 0;
begin
    if GitRec.FName = '' then begin
        SayDebugSafe('TGitNoteList.InsertData ERROR received a record with blank FName, Title is ' + GitRec.Title);
        exit;
    end;

    while i < count do begin
        if Items[i]^.FName = GitRec.FName then begin
            Items[i]^.Title      := GitRec.Title;
            Items[i]^.CDate      := GitRec.CDate;
            Items[i]^.Format     := GitRec.Format;
            Items[i]^.Notebooks  := GitRec.Notebooks;
            if Items[i]^.Sha = GitRec.Sha then
                Items[i]^.LCDate := GitRec.LCDate
            else Items[i]^.LCDate := '';           // note has been edited in Github web interface
            if Items[i]^.CDate = '' then
                Items[i]^.CDate := TheMainNoteLister.GetLastChangeDate(extractFileName(GitRec.FName));
            // Leave LCDate as it is, we may fix it with a download later. For now, its not useful.
            // We could get the github commit date (a zulu date) but not sure its worthwhile at this stage.
            Items[i]^.SyncState := '';
            exit;
        end;
        inc(i);
    end;
    SayDebugSafe('GitHub.InsertData : Failed to find ' + GitRec.FName + ' to insert data');
end;

function TGitNoteList.FNameExists(FName: string; out Sha: string): boolean;
var
  i : integer = 0;
begin
    Sha := '';
    if FName[length(FName)] = '/' then
        FName := FName.remove(length(FName)-1);   // its part of a URL so don't reverse for windows !
    while i < count do begin
        if Items[i]^.FName = FName then begin
            Sha := Items[i]^.Sha;
            exit(True);
        end;
        inc(i);
    end;
    //debugln('TGitNoteList.FNameExists WARNING ? did not find ID=[' + FName +']');
    result := False;
end;

function TGitNoteList.Find(const ID : string): PGitNote;
var
    i : integer = 0;
begin
    while i < count do begin
        if Items[i]^.FName = RNotesDir + ID + '.md' then        // first, assume its a note
            exit(Items[i]);
        if Items[i]^.FName = ID then                            // but also try as if its a FFName
            exit(Items[i]);
        inc(i);
    end;
    Result := Nil;
end;



// =========================== T G i t   H u b  ================================

// ---------------- P U B L I C   M E T H O D S  ie from Trans -----------------

{$define DEBUGghs}

function TGithubSync.TestTransport(const WriteNewServerID : boolean
    ) : TSyncAvailable;
{  If we initially fail to find offered user account, try defunkt so we can tell if
   its a network error or username one.}
var
   St : string;
begin
    Result := SyncNotYet;
    ErrorString := '';
    if DebugMode then
        debugln('TGithubSync.TestTransport - WriteNewServerID is ', booltostr(WriteNewServerID, true));
    if ANewRepo and WriteNewServerID then           // Will fail ? if repo already exists.
        MakeRemoteRepo();
    if RemoteNotes <> Nil then RemoteNotes.Free;
    RemoteNotes := TGitNoteList.Create();
    if ProgressProcedure <> nil then ProgressProcedure(rsTestingCredentials);
    if DebugMode then begin
        debugln('TGithubSync.TestTransport - about to get auth-token-expire');
        debugln('URL=' + BaseURL + 'users/' + UserName);
    end;
    if DownLoaderSafe(BaseURL + 'users/' + UserName, ST,
                        'github-authentication-token-expiration') then begin
        // So, does nominated user account exist ?
        if DebugMode then debugln('TGithubSync.TestTransport - Downloader got token-exp data, good');
        ErrorString := ExtractJSONField(ST, 'login');
        if ErrorString = UserName then begin     // "A" valid username
            ErrorString := '';
            TokenExpires := HeaderOut;           // this was set in DownLoader to returned header item matching "github-authentication-token-expiration"
            if DebugMode then debugln('TGithubSync.TestTransport - Confirmed account exists');
            if TokenExpires = '' then begin
                ErrorString := 'Username exists but Token Failure';             // we did not get back an expiry date from DownLoader()
                exit(SyncCredentialError);              // Token failure
            end;
            // If to here, we have a valid username and a valid Password but don't know if they work together
            if ProgressProcedure <> nil then progressProcedure(rsLookingServerID);
            ServerID := GetServerId();
            if DebugMode and (not ANewRepo) then
                debugln('TGithubSync.TestTransport : serverID is ' + ServerID);
            if ServerID = '' then begin
                ErrorString := 'Failed to get a ServerID, does Token have Repo Scope ?';
                exit(SyncNoRemoteRepo)
            end else begin
                if ProgressProcedure <> nil then progressProcedure(rsScanRemote);
                if not ScanRemoteRepo() then begin
                    debugln('TGithubSync.TestTransport - Failed to Scan the Remote Repo');
                    exit(SyncBadRemote);               // puts only remote filenames and sha in RemoteNotes
                end;
                if (not ReadRemoteManifest()) then begin
                        if (not ANewRepo) and (not WriteNewServerID) then begin      // do not expect a remote manifest in ANewRepo mode.
                            // But if we have had an aborted New process, might have serverid but no manifest
                            if DebugMode then
                                debugln('TGithubSync.TestTransport - Remote Repo does not have a Remote Manifest');
                            exit(SyncNoRemoteMan)
                        end else ANewRepo := True;
                end;
                // we MUST detect here where a user is trying to add SelectiveSync to an existing non-selective repo = ERROR.
                // If remote is already selective, thats OK, it stays that way. If the remote is not selective but readable,
                // and local system is selective, ERROR.  But if we appear to be building a new repo, we will go with
                // whatever the local system does.

                {  I have two vars, SelectiveSync will hold name of selective NB RemoteManifest has one set. Even if
                   local system does not have that NB (and therefore has no notes suitable). Else its empty.
                   Secondly, we have SelectiveNotebookIDs, a pointer to the local selective NB's list of notes.
                   If there is no local selective NB, then SelectiveNotebookIDs is nil (so don't poke it). And we should skip
                   all local files.
                   If the remote repo is not selective but local one is, thats an ERROR.  The exception being if
                   the remote repo is being constructed, has serverID perhaps but no remotemanifest, ANewRepo is true,
                   then we follow local policy.
                   REMEMBER -  SelectiveNotebookIDs might be nil !
               }

               if TheMainNoteLister.GetNotesInNoteBook(SelectiveNotebookIDs, SyncTransportName(SyncGithub))
               and (SelectiveSync = '') and (not ANewRepo) then begin
                   ErrorString := 'Local is Selective, remote is NOT';
                   SayDebugSafe(ErrorString + ' probably need build a new remote repo, please read documentation');
                   exit(SyncMismatch)
               end;
               if (SelectiveSync = '') and  assigned(SelectiveNotebookIDs) then begin     // Use local 'cos its a new repo.
                   SelectiveSync := SyncTransportName(SyncGithub);
                   // debugln('TGitHubSync.TestTransport - setting SelectiveSync to : ' + SelectiveSync);
               end;

               if DebugMode then begin
                    debugln('TGitHubSync.TestTransport SelectiveSync=' + SelectiveSync);
                    if not assigned(SelectiveNotebookIDs) then
                        debugln('TGitHubSync.TestTransport SelectiveNotebookIDs not assigned.');
                end;
                Result := SyncReady;
                if ProgressProcedure <> nil then ProgressProcedure('TestTransport Happy, SelectiveSync=' + SelectiveSync);
                if DebugMode then begin
                    debugln('TGitHubSync.TestTransport SelectiveSync is OK');
                    if not assigned(SelectiveNotebookIDs) then
                        debugln('TGitHubSync.TestTransport SelectiveNotebookIDs not assigned.');
                end;
            end;
        end else begin
            if DebugMode then begin
                DebugLn('TGithubSync.TestTransport - JSON : Github did not confirm "login"');
                debugln('Start bad JSON [' + St + '] End bad JSON');
                Result := SyncBadError;
            end;
        end;
    end
    else begin                                                              // OK, we are not getting through, why not ?
        debugln('TGitHubSync.TestTransport : Failed to speak to github as the user ' + UserName
                + ', network or credentials error ?, ' + St);
        if St = 'Fatal' then exit(SyncNetworkError);                        // Downloader has said it all !
        if DownLoaderSafe(BaseURL + 'users/defunkt', ST) then begin                 // try a known good user
            ErrorString := ErrorString + ' Username is not valid : ' + UserName;
            debugln('TGitHubSync.TestTransport : But can speak to github as user defunkt.');
            exit(SyncCredentialError);
        end
        else begin                                                              // but known good user worked, must be the token we are using
            debugln('TGitHubSync.TestTransport : But can speak to github, probably bad token, '
                    + SyncAvailableString(SyncCredentialError));
            // here probably because of a bad token, lets rewrite the error message
            if pos('401', ErrorString) > 0 then
                ErrorString := ErrorString + ' ' + rsGithubTokenExpired;
            exit(SyncNetworkError);                                             // Hmm, NetWorkError ? more like credentials  ???
        end;
    end;
    {$ifdef DEBUGghs} if DebugMode then
        debugln('TGitHubSync.TestTransport : at end of method returning : '
            + SyncAvailableString(Result));
    {$endif}
end;

function TGithubSync.SetTransport : TSyncAvailable;
begin
    if DebugMode then saydebugSafe('TGithubSync.SetTransport - called');
    if not directoryexists(NotesDir + TempDir) then
        ForceDirectory(NotesDir + TempDir);
    if directoryexists(NotesDir + TempDir)
            and DirectoryIsWritable(NotesDir + TempDir) then begin
        result := SyncReady;
        if not DeleteDirectory(NotesDir + TempDir, True) then
            Saydebugsafe('TGithubSync.SetTransport ERROR, failed to clear out Temp dir');
        // by clearing the Temp dir, we know any files in there later on arrived in
        // this session and can be safely re-used eg, ones pulled down for clash handling.
    end
    else begin
        SayDebugSafe('Cannot use dir : ' + NotesDir + TempDir);
        exit(SyncBadError);
    end;
    RemoteAddress := GitBaseURL + UserName + '/' + RemoteRepoName;
end;

function TGithubSync.DownloadNotes(const DownLoads : TNoteInfoList) : boolean;
var
    I : integer;
    DownCount : integer = 0;
    FullFileName : string;
begin
    Result := True;
    if not DirectoryExists(NotesDir + TempDir) then
        exit(SayDebugSafe('TGithubSync.DownloadNotes - ERROR, no temp dir  ' + NotesDir + TempDir));
    if ProgressProcedure <> nil then ProgressProcedure(rsDownloadNotes);
    if not DirectoryExists(NotesDir + 'Backup') then
        if not ForceDirectory(NotesDir + 'Backup') then begin
            ErrorString := 'Failed to create Backup directory.';
            exit(False);
        end;
    for I := 0 to DownLoads.Count-1 do begin
        if DownLoads.Items[I]^.Action = SyDownLoad then begin
            if FileExists(NotesDir + Downloads.Items[I]^.ID + '.note') then
                // First make a Backup copy
                if not CopyFile(NotesDir + Downloads.Items[I]^.ID + '.note',
                        NotesDir + 'Backup' + PathDelim + Downloads.Items[I]^.ID + '.note') then begin
                    ErrorString := 'GitHub.DownloadNotes Failed to copy file to Backup ' + NotesDir + Downloads.Items[I]^.ID + '.note';
                    exit(False);
                end;
            FullFileName := NotesDir + TempDir + Downloads.Items[I]^.ID + '.note';
            if not FileExists(FullFileName) then
                Result := DownloadANote(Downloads.Items[I]^.ID, FullFileName)   // OK, now download the file,
            else Result := True;                                                // we must have downloaded it to resolve clash
            if Result and fileexists(FullFileName)  then begin                  // to be sure, to be sure
                    deletefile(NotesDir + Downloads.Items[I]^.ID + '.note');
                    renamefile(FullFileName, NotesDir + Downloads.Items[I]^.ID + '.note');
            end else begin
                ErrorString := 'GitHub.DownloadNotes Failed to download ' + FullFileName;
                exit(SayDebugSafe('TGithubSync.DownloadNotes - ERROR, failed to down to ' + FullFileName));
            end;
            inc(DownCount);
            if (DownCount mod 5 = 0) then
                if ProgressProcedure <> nil then ProgressProcedure(rsDownLoaded + ' ' + inttostr(DownCount) + ' notes');
        end;
    end;
end;

function TGithubSync.DeleteNote(const ID : string; const ExistRev : integer
    ) : boolean;
//   https://docs.github.com/en/rest/reference/repos#delete-a-file
var
    Response : TStringList;
    Client: TFPHttpClient;
    BodyStr, Sha : string;
    RFName : string;
begin
    Result := false;
    RFName := RNotesDir + ID + '.md';
    if not (RemoteNotes.FNameExists(RFName, Sha) and (Sha <> '')) then begin   // Try for an ID first.
        RFName := ID;
        if not (RemoteNotes.FNameExists(RFName, Sha) and (Sha <> '')) then     // Failing an ID, we try "as is".
            exit(SayDebugSafe('TGitHubSync.DeleteNote ERROR did not find sha for ' + ID));
    end;
    BodyStr :=  '{ "message": "update upload", "sha" : "' + Sha + '" }';
    Client := TFPHttpClient.create(nil);
    Response := TStringList.create;
    try
        Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
        Client.AddHeader('Content-Type','application/json; charset=UTF-8');
        Client.AddHeader('Accept', 'application/json');
        Client.AllowRedirect := true;
        Client.UserName:=UserName;
        Client.Password:=Password;
        client.RequestBody := TRawByteStringStream.Create(BodyStr);
        Client.Delete(ContentsURL(True) + '/' + RFName,  Response);
        Result := (Client.ResponseStatusCode = 200);
        if not Result then begin
                saydebugsafe('TGitHubSync.DeleteNote : Delete returned ' + inttostr(Client.ResponseStatusCode));
                saydebugsafe('URL=' + ContentsURL(true) + '/' + RFName);
                saydebugsafe(' ------------- Delete Response  ------------');
                saydebugsafe(Response.text);
                saydebugsafe(' ------------- Delete Response End ------------');
        end;
    finally
        Response.free;
        Client.RequestBody.Free;
        Client.Free;
    end;
end;

function TGithubSync.GetRemoteNotes(const NoteMeta : TNoteInfoList;
    const GetLCD : boolean) : boolean;
begin
    if (RemoteNotes = Nil) or (NoteMeta = Nil) then exit(SayDebugSafe('TGitHubSync.GetRemoteNotes ERROR getRemoteNotes called with nil list'));
    result := True;
end;

function TGithubSync.UploadNotes(const Uploads : TStringList) : boolean;
var
    St : string;
    NoteCount : integer = 0;
begin
    {$ifdef DEBUG}
    RemoteNotes.DumpList('TGitHubSync.UploadNotes : About to upload ' + inttostr(UpLoads.Count) + ' notes');
    {$endif}
    if ProgressProcedure <> nil then
                ProgressProcedure(rsUpLoading + ' ' + inttostr(Uploads.Count));
    for St in Uploads do begin
        if not SendNote(St) then begin                                          // that will make multiple tries at sending if necessary
            RemoteNotes.InsertData(RNotesDir + St + '.md', 'syncstate', FAILEDUPLOAD);
            debugln('TGithubSync.UploadNotes - An upload failed, we will mark it to try again next sync');
            continue;                                                           // continue so we don't update LCD
        end else
            RemoteNotes.InsertData(RNotesDir + St + '.md', 'syncstate', '');
        inc(NoteCount);
        if NoteCount mod 5 = 0 then
            if ProgressProcedure <> nil then
                ProgressProcedure(rsUpLoaded + ' ' + inttostr(NoteCount) + ' notes');
        RemoteNotes.InsertData(RNotesDir + St + '.md', 'lcdate', TheMainNoteLister.GetLastChangeDate(St));
        // ToDo : that has hardwired assumption about markdown
    end;
    result := true;
end;

function TGithubSync.DoRemoteManifest(const RemoteManifest : string; MetaData : TNoteInfoList) : boolean;
// Note that MetsData is the RemoteMetaData data structure from sync, changes here reflected in local manifest, report
var
    P : PNoteInfo;      // an item from RemoteMetaData
    PGit : PGitNote;    // an item from local data structure, RemoteNotes
    Readme, manifest : TStringList;
    St, Notebooks : string;
begin
    // Note : we do not use the supplied XML RemoteManifest, we build our own json one.
    Result := false;
    Readme := TstringList.Create;
    Manifest := TstringList.Create;
    Readme.Append('## My tomboy-ng Notes');
    // * [Note Title](https://github.com/davidbannon/tb_demo/blob/main/Notes/287CAB9C-A75F-4FAF-A3A4-058DDB1BA982.md)
    Manifest.Append('{' + #10'  "selectivesync" : "' + EscapeJSON(SelectiveSync) + '",'#10' "notes" : {');
    try
        if MetaData = nil then exit(SayDebugSafe('TGithubSync.DoRemoteManifest ERROR, passed a nil metadata list'));
        for P in MetaData do begin                                              // iterate over TSync.RemoteMetaData
            if P^.Action in [ SyNothing, SyUploadNew, SyUploadEdit, SyDownload, SyClash ] then begin  // SyClash ? I don't think so .....
                // These notes will be the ones that are now up on GitHub.
                PGit := RemoteNotes.Find(P^.ID);
                if PGit = nil then
                    exit(SayDebugSafe('TGitHubSync.DoRemoteManifest - ERROR, failed to find ID from RemoteMetaData in RemoteNotes'));
                // if PGit^.LCDate = '' then begin                     // That means the note failed to upload, was set by UploadNote
                Readme.Append('* [' + P^.Title + '](' + ContentsURL(False) + PGit^.FName + ')');
                if P^.Action = SyDownload then                                  // P^ comes from MetaData,  TSync.TRemoteMetaData from remote manifest.
                    NoteBooks := PGit^.Notebooks
                else
                    NoteBooks := TheMainNoteLister.NotebookJArray(P^.ID + '.note');
                Manifest.Append('    "' + P^.ID + '" : {'#10                    // this is remote manifest in the making
                        + '      "title" : "'  + EscapeJSON(P^.Title) + '",'#10
                        + '      "cdate" : "'  + P^.CreateDate + '",'#10
                        + '      "lcdate" : "' + PGit^.LCDate + '",'#10         // PGit^ comes from TGithubSync.RemoteNotes, based on data from github
                        + '      "sha" : "'    + PGit^.Sha + '", '#10
                        + '      "format" : "md",'#10
                        + '      "notebooks" : '+ NoteBooks + #10 + '    },');  // should be empty string or eg ["one", "two"]
                P^.ForceUpload := (PGit^.SyncState = FAILEDUPLOAD);             // so local manifest knows to signal next sync force-upload
            end;
        end;
        // Remove that annoying trailing comma from last block
        if manifest.count > 0 then begin
            St := manifest[manifest.count-1];
            if St[St.Length] = ',' then begin
                delete(St, St.Length, 1);
                manifest.Delete(manifest.count-1);
                manifest.append(St);
            end;
        end;
        Readme.append('');
        Readme.append('***' + rsMetaDirWarning + '***');
        Manifest.Append('  }'#10 + '}'#10);
        for PGit in RemoteNotes do                      // Put all the SHAs we know about into RemoteMetaData (for local manifest);
            if PGit^.Sha <> '' then begin
                P := MetaData.FindID(extractFileNameOnly(PGit^.FName));
                if P <> nil then
                    P^.Sha := PGit^.Sha;
            end;
        if not SendFile(RMetaDir + 'manifest.json', Manifest) then SayDebugSafe('TGitHubSync.DoRemoteManifest ERROR, failed to write remote manifest');
        if not SendFile('README.md', Readme) then SayDebugSafe('TGitHubSync.DoRemoteManifest ERROR, failed to write remote README.md');
        result := true;
    finally
        Manifest.Free;
        Readme.Free;
    end;
end;

function TGithubSync.DownLoadNote(const ID : string; const RevNo : Integer
    ) : string;
begin
   if DownloadANote(ID, NotesDir + TempDir + ID + '.note') then
       Result := NotesDir + TempDir + ID + '.note'
   else Result := '';
end;



// ToDo : work through this better, are we risking race conditions here ?

const Seconds5 = 0.00005;          // Very roughly, 5 seconds


function TGithubSync.MergeNotesFromNoteLister(RMData : TNoteInfoList;
    TestRun : boolean) : boolean;
var
    PGit : PGitNote;
    RemRec: PNoteInfo;
    NLister : PNote;
    i : integer;
begin
   if (SelectiveSync <> '') and (not Assigned(SelectiveNotebookIDs)) then     // Something in SelectiveSync but no local NB, no uploads possible
       exit(false);
   for i := 0 to TheMainNoteLister.GetNoteCount() -1 do begin                     // OK, now whats in NoteLister but not RemoteNotes ?
       NLister := TheMainNoteLister.GetNote(i);
       // debugln('TGitHubSync.MergeNotesFromNoteLister  considering Title=' + NLister^.Title);
       if NLister = nil then exit(SayDebugSafe('TGitHubSync.AssignActions ERROR - not finding NoteLister Content'));

       // if I can be sure it really does sort the (NoteLister's) list with SelectiveNotebookIDs.sorted := true.
       // Calling SelectiveNotebookIDs.Sort does not seem to work ??
        // we know we can safely poke at SelectiveNotebookIDs if SelectiveSync is not empty.

       // debugln('TGitHubSync.MergeNotesFromNoteLister looking at ' + NLister^.Title);

       if (SelectiveSync <> '') and (FindInStringList(SelectiveNotebookIDs, NLister^.ID) < 0)
           then continue;
       // Look for items in NoteLister that do not currently exist in RemoteMetaData. If we find one,
       // we will add it to both RemoteMetaData and RemoteNodes (because its needed to store sha on upload)

       // debugln('TGitHubSync.MergeNotesFromNoteLister  Adding Title=' + NLister^.Title);
       if RMData.FindID(extractfilenameonly(NLister^.ID)) = nil then begin
           if not TestRun then begin
               new(PGit);
               PGit^.FName := RNotesDir + extractfilenameonly(NLister^.ID) + '.md';                     // ToDo : Careful, assumes markdown
               PGit^.Sha := '';
               PGit^.Notebooks := '';
               PGit^.CDate := NLister^.CreateDate;
               PGit^.LCDate := NLister^.LastChange;
               PGit^.Format := ffMarkDown;
               PGit^.SyncState := '';
               RemoteNotes.Add(PGit);
           end;
           new(RemRec);
           RemRec^.ID := extractfilenameonly(NLister^.ID);
           RemRec^.LastChange := NLister^.LastChange;
           RemRec^.CreateDate := NLister^.CreateDate;
           RemRec^.Sha := '';
           RemRec^.Title := NLister^.Title;
           RemRec^.Action := SyUploadNew;
           RemRec^.ForceUpload := False;
           RemRec^.Deleted := False;
           RemRec^.Rev := 0;
           RemRec^.SID := 0;
           RMData.Add(RemRec);
       end { else debugln('TGithubSync.AssignActions - skiping because its already in RemoteNotes')};
   end;
   Result := true;
end;

function TGithubSync.AssignActions(RMData, LMData : TNoteInfoList; TestRun : boolean) : boolean;
var
    PGit : PGitNote;               // A pointer to an item in TGithubSync.RemoteNotes, initially filled from Github file data
    RemRec : PNoteInfo;            // A pointer to an item in TSync.RemoteMetaData, initially filled from remote manifest
    LocRec : PNoteInfo;            // A pointer to an item in TSync.LocalMetaData, initially filled from local manifest
    I : integer;
    //NLister : PNote;
    pNBook: PNoteBook;
    LCDate, CDate : string;
begin
    // RMData should be empty, LMData will be empty if its a Join, LMData derived from local manifest.
    Result := True;
    {$ifdef DEBUG}
    debugln('==================================================================');
    debugln('                  A S S I G N    A C T I O N S ');
    debugln('==================================================================');
    RMData.DumpList('TGithubSync.AssignActions.start RemoteMD');
    LMData.DumpList('TGithubSync.AssignActions.start LocalMD');
    RemoteNotes.DumpList('TGithubSync.AssignActions.start RemoteNotes');
    {$endif}
    for PGit in RemoteNotes do begin                                            // First, put an entry in RemoteMetaData for every remote note.
        if copy(PGit^.FName, 1, length(RNotesDir)) <> RNotesDir then continue;
        // Every note we see in this loop exists remotely. But may not exist locally.
        new(RemRec);
        RemRec^.ID := extractFileNameOnly(PGit^.FName);
        LocRec := LMData.FindID(RemRec^.ID);                                    // Nil is OK, just means the note is not in LocalMetaData
        RemRec^.CreateDate := PGit^.CDate;
        RemRec^.LastChange := PGit^.LCDate;                                     // We may not have this, if note was edited in github
        RemRec^.Deleted := false;
        RemRec^.Rev := 0;
        RemRec^.SID := 0;
        RemRec^.Title := TheMainNoteLister.GetTitle(RemRec^.ID);                    // We 'prefer' the local title, remote one may be different
        if RemRec^.Title = '' then
            RemRec^.Title := TheMainNoteLister.GetNotebookName(RemRec^.ID);         // Maybe its a template ?
        RemRec^.Action := SyUnset;
        RemRec^.ForceUpload := False;                                           // becomes True if a subsquent up fails
        if RemRec^.Title = '' then begin                                        // Not in Notelister, must be new or locally deleted
            //debugln('TGithubSync.AssignActions setting ' + RemRec^.ID + ' to Download #1');
            RemRec^.Action := SyDownLoad;                                       // May get changed to SyDeleteRemote
            RemRec^.Title := PGit^.Title;                                       // One we prepared earlier, from remote manifest
            {$ifdef DEBUG}
            //SayDebugSafe('TGithubSync.AssignActions RemRec^.Title = ' + RemRec^.Title);
            //SayDebugSafe('TGithubSync.AssignActions PGit^.Title = ' + PGit^.Title);
            {$endif}
        end
        else begin                                                              // OK, it exists at both ends, now we need to look closely.
            //debugln('TGithubSync.AssignActions - Possibe clash LMData.LastSyncDate UTC= ' +  FormatDateTime('YYYY MM DD : hh:mm', LMData.LastSyncDate ));
            if  LMData.LastSyncDate < 1.0 then begin                                 // Not valid
                // if LastSyncDate is 0.0, a Join. An ID that exists at both ends is a clash. No local manifest to help here.
                // But we have one more trick.  If the remote note has a valid LCDate in RemoteNotes, it came from a -ng
                // (ie, not a Github edit). We can compare the date string to the local one and if they match, all good.
                if (PGit^.LCDate <> '') and (TheMainNoteLister.GetLastChangeDate(RemRec^.ID)
                            = PGit^.LCDate) then
                    RemRec^.Action := SyNothing
                    else RemRec^.Action := SyClash;                             // OK, we tried but this one goes through to keeper.
                //debugln('TGithubSync.AssignActions - found possible JOIN clash, now its ' + RMData.ActionName(RemRec^.Action));
                //debugln('PGit^.LCDate = ' + PGit^.LCDate + ' and  TheNoteLister.GetLastChangeDate = ' + LocalLastChangeDate(RemRec^.ID + '.note'));
            end
            else begin                                                          // Normal sync, we have a local manifest.
                if  (TB_GetGMTFromStr(TheMainNoteLister.GetLastChangeDate(RemRec^.ID)) - Seconds5)
                            > LMData.LastSyncDate then
                    RemRec^.Action := SyUploadEdit;                             // changed since last sync ? Upload it !
                if  LocRec = Nil then begin                                     // ?? If it exists at both ends we must have uploaded it ??
                    dispose(RemRec);
                    ErrorString := 'TGitHubSync.AssignActions ERROR, ID not found in LocalMetaData, might need to force a Join : ' + PGIT^.FName;
                    exit(SayDebugSafe(ErrorString));
                end;
                if LocRec^.ForceUpload then begin
                    RemRec^.Action := SyUploadEdit;                             // upload failed in previous sync, we'll try again
                    debugln('TGitHubSync.AssignActions ' + PGit^.FName + ' is forceupload ! A previous sync failed, we will try again.');
                end;
                if PGit^.Sha <> LocRec^.Sha then begin                          // Ah, its been changed remotely
                    if RemRec^.Action = SyUnset then
                         RemRec^.Action := SyDownLoad                           // Good, only remotely
                    else                                                        // Oh, already set to SyUploadEdit about 20 lines up
                        RemRec^.Action := SyClash;                              // There is a problem to solve elsewhere.
                end;
            end;
             //debugln('TGithubSync.AssignActions - Possibe clash becomes ' + RMData.ActionName(RemRec^.Action));
        end;
        if RemRec^.Action = SyUnset then
             RemRec^.Action := SyNothing;
        RMData.Add(RemRec);
    end;

    {$ifdef DEBUG}
    RMData.DumpList('TGithubSync.AssignActions RemoteMD - Before scanning NoteLister');
    {$endif}

    MergeNotesFromNoteLister(RMData, TestRun);

    // OK, just need to check over the Notebooks now, notebooks are NOT listed in NoteLister.Notelist !
    for i := 0 to TheMainNoteLister.NotebookCount() -1 do begin
        pNBook := TheMainNoteLister.GetNoteBook(i);
        if (SelectiveSync <> '') and (SelectiveSync <> pNBook^.Name) then      // only interested in SyncGithub template here....
            continue;
        if RMData.FindID(extractfilenameonly(pNBook^.Template)) = nil then begin
            ErrorString := '';
            CDate := GetNoteLastChangeSt(NotesDir + pNBook^.Template, ErrorString, True);
            LCDate := GetNoteLastChangeSt(NotesDir + pNBook^.Template, ErrorString, False);
            if ErrorString <> '' then
                exit(SayDebugSafe('Failed to find dates in template ' + pNBook^.Template));
            if not TestRun then begin
                new(PGit);
                PGit^.FName := RNotesDir + extractfilenameonly(pNBook^.Template) + '.md';               // ToDo : Careful, assumes markdown
                PGit^.Sha := '';
                PGit^.Notebooks := '';
                PGit^.CDate := CDate;
                PGit^.LCDate := LCDate;
                PGit^.Format := ffMarkDown;
                PGit^.SyncState := '';
                RemoteNotes.Add(PGit);
            end;
            new(RemRec);
            RemRec^.ID := extractfilenameonly(pNBook^.Template);
            RemRec^.LastChange := LCDate;
            RemRec^.CreateDate := CDate;
            RemRec^.Sha := '';
            RemRec^.Title := pNBook^.Name;
            RemRec^.Action := SyUploadNew;
            RemRec^.ForceUpload := false;
            RemRec^.Deleted := False;
            RemRec^.Rev := 0;
            RemRec^.SID := 0;
            RMData.Add(RemRec);
        end {else debugln('TGithubSync.AssignActions - skiping because its already in RemoteNotes')};
    end;
    {$ifdef DEBUG}
    RMData.DumpList('TGithubSync.AssignActions.End RemoteMD');
    LMData.DumpList('TGithubSync.AssignActions.End LocalMD');
    RemoteNotes.DumpList('TGithubSync.AssignActions.End RemoteNotes');
    {$endif}
end;

procedure TGithubSync.Test;
var ST : string;
begin
   if Downloader(ContentsURL(True)+'/'+RNotesDir, ST) then begin   // Github appears to be happy with Notes and Notes/   ?
        debugln('---------------------------------------------');
        debugln(St);
        debugln('---------------------------------------------');
   end else debugln('Downloader Failed');

end;

// ====================  P R I V A T E   M E T H O D S =========================

function TGithubSync.GetNoteLCD(FFName : string) : string;
var
   St : string;
   URL : string;
begin
    Result := '';
    // This call brings back an array, '[one-record]', note its not the Contents URL !
    URL := BaseURL + 'repos/' + UserName + '/' + RemoteRepoName + '/';
    if DownLoaderSafe(URL + 'commits?path=' + FFName + '&per_page=1&page=1', ST) then begin
        if St[1] = '[' then begin
            delete(St, 1, 1);
        end else SayDebugSafe('GitHub.GetNoteLCD - Error, failed to remove  from array');
        if St[St.Length] = ']' then begin
            delete(St, St.Length, 1);
        end else SayDebugSafe('GetNoteLCD - Error, failed to remove [ from array');
        Result := ExtractJSONField(St, 'date', 'commit', 'author');
        if (Result = '') or (Result[1] = 'E') then                  // E for ERROR
            SayDebugSafe('TGitHubSync.GetNoteLCD ERROR failed to find commit date in JSON ' + St
                + ' for file ' + FFName);
     end else SayDebugSafe('GitHub.GetNoteLCD - Error, failed to get LCD for ' + FFName);
end;

function TGithubSync.SendNote(ID : string) : boolean;
var
    STL : TStringList;
    CM  : TExportCommon;
begin
    if DebugMode then
                debugln('TGitHubSync.SendNote - going to upload note, ID=' + ID
                    + ' : [' + TheMainNoteLister.GetTitle(ID) + ']');
    STL := TStringList.Create;
    CM := TExportCommon.Create;
    try
        CM.NotesDir := NotesDir;
        CM.GetMDcontent(ID, STL);
        if STL.Count < 1 then begin
            // if DebugMode then
            debugln('TGitHubSync.SendNote - ERROR, failed to convert to markdown, ID=' + ID);
            debugln(CM.ErrorMsg);
            exit(False);
        end;
        Result := SendFile(RNotesDir + ID + '.md', STL);
        if (not Result) and DebugMode then
                debugln('TGitHubSync.SendNote - ERROR, failed to upload note, ID=' + ID);
    finally
        CM.Free;
        STL.Free;
    end;
end;

function TGithubSync.SendFile(RemoteFName : string; STL : TstringList) : boolean;      // Public only in test mode
var
    Sha : string = '';
    BodyStr : string;
    Cnt : integer = 0;
begin
    if RemoteNotes = nil then exit(false);
    if RemoteNotes.FNameExists(RemoteFName, Sha) and (Sha <> '') then begin         // Existing file mode
        BodyStr :=  '{ "message": "update upload", "sha" : "' + Sha
                    + '", "content": "' + EncodeStringBase64(STL.Text) + '" }';
        if Sha = '' then exit(False);                                           // Hmm, why and what happens if it does ?
    end else begin                                      // New file mode
        BodyStr :=  '{ "message": "initial upload", "content": "'
                    + EncodeStringBase64(STL.Text) + '" }';
    end;
    Result := False;
    repeat

{        if pos('1D65A2EC-3C63-4732-8F9E-73E2A4544BC5', RemoteFName) > 0 then
            ErrorString := '  ***** fakeing upload failure *****'
        else    }

        Result := SendData(ContentsURL(True) + '/' + RemoteFName, BodyStr, true, RemoteFName);
        if Result then break;
        if ProgressProcedure <> nil then
                ProgressProcedure('Upload problem retrying ' + inttostr(cnt));
        sleep(100*Cnt*Cnt*Cnt);         // 0, 100, 800, 2700, 6400 mS
        inc(Cnt);
        DebugLn('TGitHubSync.SendFile - NOTICE, retry no.', Cnt.ToString
                , ' : Failed to send file filename=' + RemoteFName + ' Error=' + ErrorString);
    until Cnt = MaxNetTries;
    if DebugMode and (not Result) then
        DebugLn('TGitHubSync.SendFile - ERROR - Failed to send file filename=' + RemoteFName + ' Error=' + ErrorString);
end;

function TGithubSync.MakeRemoteRepo : boolean;
var
    GUID : TGUID;
    STL: TstringList;
    Cnt : integer = 0;
begin
    // https://docs.github.com/en/rest/reference/repos#create-a-repository-for-the-authenticated-user
    {$ifdef DEBUG}
    SayDebugSafe('TGitHubSync.MakeRemoteRepo called');
    {$endif}

    repeat
        // eg { "name": "tb_notes", "auto_init": true, "private": true" }    ????  excess " there ?  // ToDo : check double inverted comma use
        Result := SendData(BaseURL + 'user/repos',
            '{ "name": "' + RemoteRepoName + '", "auto_init": true, "private": true" }', False);
        if Result then break;
        sleep(100*Cnt*Cnt*Cnt);     // 0, 100, 800, 2700 mS   (not doing 6400 because user is staring at screen here.
        inc(Cnt);
        if ProgressProcedure <> nil then
                ProgressProcedure('Network problem retrying ' + inttostr(cnt));
        SayDebugSafe('TGitHubSync.MakeRemoteRepo NOTICE retry = No.' + Cnt.ToString + ' failed to create github repo.');
    until Cnt = (MaxNetTries-1);

//    Result := SendData(BaseURL + 'user/repos',
//        '{ "name": "' + RemoteRepoName + '", "auto_init": true, "private": true" }', False);
    if (not Result) and (GetServerId() <> '') then exit(false);

    {$ifdef DEBUG}
    SayDebugSafe('TGitHubSync.MakeRemoteRepo creating new ServerID');
    {$endif}

    CreateGUID(GUID);
    STL := TstringList.Create;
    try
        ServerID := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
        STL.Add(ServerID);
        Result := SendFile(RMetaDir + 'serverid', STL);         // Now, RemoteNotes does not exist at this stage !!
        // eg URL=https://api.github.com/repos/davidbannon/tb_test/contents/Meta/serverid
    finally
        STL.Free;
    end;
    {$ifdef DEBUG}
    SayDebugSafe('TGitHubSync.MakeRemoteRepo returning ' + booltostr(Result, True));
    {$endif}

    //RemoteServerRev := -1;
end;


function TGithubSync.ScanRemoteRepo : boolean;
var
    Node, ANode : TJsonNode;
    St, Sha : string;

    function ReadDir(Dir : string) : boolean;
    begin
        Result := True;
        if (Dir <> '') and (not RemoteNotes.FNameExists(Dir, Sha)) then exit;
        if DownloaderSafe(ContentsURL(True) + Dir, ST) then begin  // St now contains a full dir listing as JSON array
            Node := TJsonNode.Create;
            try
                if Node.TryParse(St) then begin
                    for ANode in Node do
                        if ANode.Exists('name') and ANode.Exists('sha') then
                            RemoteNotes.AddNewItem(Dir + ANode.Find('name').asString, ANode.Find('sha').asString)
                        else exit(SayDebugSafe('TGitHubSync.ScanRemoteRepo - ERROR Invalid J data = ' + St));
                end else
                    exit(SayDebugSafe('TGitHubSync.ScanRemoteRepo - ERROR Invalid J data = ' + St));
            finally
                Node.Free;
            end;
        end else exit(SayDebugSafe('TGitHubSync.ScanRemoteRepo - Download ERROR ' + Dir));
    end;

begin
    Result := ReadDir('') and ReadDir(RNotesDir) and ReadDir(RMetaDir);

    {$ifdef DEBUG}
    RemoteNotes.DumpList('TGitHubSync.ScanRemoteRepo RemoteNotes after Scan.');
    {$endif}

end;

(*
procedure TForm1.FormShow(Sender : TObject);
var
    Host : THostResolver;
begin
    Host := THostResolver.Create(nil);
    if Host.NameLookup('api.github.com') then
        writeln(Host.AddressAsString)
    else writeln('not found');
    host.Free;
end;  *)



constructor TGithubSync.Create(PP : TProgressProcedure = nil);
var
    Host : THostResolver;
    Cnt : integer = 0;
begin
    ProgressProcedure := PP;           // It gets passed after create.    WHY ?
    RemoteNotes := Nil;
    SelectiveNotebookIDs := nil;
    if GetEnvironmentVariableUTF8('TB_GITHUB_REPO') <> '' then begin
        RemoteRepoName := GetEnvironmentVariableUTF8('TB_GITHUB_REPO');
        debugln('TGitHubSync.Create() - Github repo renamed as ' + RemoteRepoName);
    end;

    // here we poke the dns wrt github in hope of clearing a path.

    Host := THostResolver.Create(nil);
    try
        repeat                                                    // check api.github.com first
            if Host.NameLookup('api.github.com') then break;      // discard result, just want to know its possible
            sleep(100*Cnt*Cnt*Cnt);                               // 0, 100, 800, 2700, 6400 mS
            inc(Cnt);
            if ProgressProcedure <> nil then
                ProgressProcedure('Network problem retrying ' + inttostr(cnt));
            debugln('TGithubSync.Create - retesting IP for api.github.com retry no.'+Cnt.Tostring);
        until Cnt = MaxNetTries;
        if Cnt = MaxNetTries then
            FailedToResolveIPs := true;
        if not FailedToResolveIPs then begin                      // if still ok, try for github.com
            Cnt := 0;
            repeat
                if Host.NameLookup('github.com') then break;      // ToDo : no progress report here, it is necessary !
                sleep(100*Cnt*Cnt*Cnt);                           // 0, 100, 800, 2700, 6400 mS
                inc(Cnt);
                if ProgressProcedure <> nil then
                    ProgressProcedure('Network problem retrying ' + inttostr(cnt));
                debugln('TGithubSync.Create - retesting IP for github.com retry no.'+Cnt.Tostring);
            until Cnt = MaxNetTries;
            if Cnt = MaxNetTries then
                FailedToResolveIPs := true;
        end;
    finally
        Host.Free;
        if FailedToResolveIPs then begin
            ErrorString := rsNetworkNotAvailable;
            debugln('TGithubSync.Create ---- ERROR failed when testing IP address');
        end;
    end;
end;

destructor TGithubSync.Destroy;
begin
    if RemoteNotes <> Nil then RemoteNotes.Free;
    inherited Destroy;
end;

function TGithubSync.DownloadANote(const NoteID : string; FFName : string
    ) : boolean;
var
    NoteSTL : TStringList;
    St  : string;
    Importer : TImportNotes;
    PGit : PGitNote;
begin
    Importer := Nil;
    NoteSTL := Nil;
    Result := True;
    {$ifdef DEBUG}Saydebugsafe('TGithubSync.DownloadANote');{$endif}
    try
        PGit := RemoteNotes.Find(RNotesDir + NoteID + '.md');      // ToDo : assumes markdown
        if PGit = nil then exit(SayDebugSafe('TGithubSync.DownloadANote - ERROR, cannot find ID in RemoteNotes = ' + RNotesDir + NoteID + '.md'));
        if PGit^.LCDate = '' then begin                                 // maybe because note was edited in github and remote manifest LCD was unusable
            PGit^.LCDate := GetNoteLCD(PGit^.FName);
           // debugln(' TGithubSync.DownloadANote gethub edited note has date of ' + PGit^.LCDate);
// TEST THIS !!!!
        end;

        if not DownloaderSafe(ContentsURL(True) + '/' + RNotesDir + NoteID + '.md', ST) then
            exit(SayDebugSafe('TGithubSync.DownloadANote ERROR, failed to download note : ' + NoteID));
        {$ifdef DEBUG}Saydebugsafe('TGithubSync.DownloadANote downloaded OK ' + NoteID);{$endif}
        NoteSTL := TStringList.Create;
        NoteSTL.Text := DecodeStringBase64(ExtractJSONField(ST, 'content'));
        {$ifdef DEBUG}Saydebugsafe('TGithubSync.DownloadANote decoded');{$endif}
        if NoteSTL.Count > 0 then begin
                Importer := TImportNotes.Create;

                { if length(PGit^.Notebooks) > 5 then
                    Saydebugsafe('TGithubSync.DownloadANote Using Notebook = ' + PGit^.Notebooks); }

                Importer.NoteBook := PGit^.Notebooks;
                {$ifdef DEBUG}Saydebugsafe('TGithubSync.DownloadANote about to import');{$endif}
                Importer.MDtoNote(NoteSTL, PGit^.LCDate, PGit^.CDate);
                {$ifdef DEBUG}Saydebugsafe('TGithubSync.DownloadANote imported');{$endif}
                // writeln(NoteSTL.TEXT);
                if FFName = '' then
                    NoteSTL.SaveToFile(NotesDir + NoteID + '.note-temp')
                else NoteSTL.SaveToFile(FFname);
                {$ifdef DEBUG}Saydebugsafe('TGithubSync.DownloadANote file saved');{$endif}
        end else Result := false;
    finally
        if Importer <> Nil then Importer.Free;
        if NoteSTL <> Nil then NoteSTL.Free;
        //STL.Free;
    end;
    {$ifdef DEBUG}Saydebugsafe('TGithubSync.DownloadANote finished');{$endif}
end;

function TGithubSync.ReadRemoteManifest : boolean;
var
    St : string;
    Node, ANode, NotesNode : TJsonNode;
    PGit : PGitNote;
begin
    if RemoteNotes.Find(RMetaDir + 'manifest.json') = nil then
        exit(SayDebugSafe('TGitHubSync.ReadRemoteManifest : Remote manifest not present, maybe a new repo ?'));
    if not DownloaderSafe(ContentsURL(True) + '/' + RMetaDir + 'manifest.json', ST) then
        exit(SayDebugSafe('GitHub.ReadRemoteMainfest : Failed to read the remote manifest file'));
    {$ifdef DEBUG}
    RemoteNotes.DumpList('TGithubSync.ReadRemoteManifest - Before ReadRemoteManifest');
    {$endif}
    Node := TJsonNode.Create;
    try
        // content is in the "content" field, Base64 encoded.
        if not Node.TryParse(DecodeStringBase64(ExtractJSONField(ST, 'content'))) then
            exit(SayDebugSafe('TGitHubSync.ReadRemoteManifest ERROR invalid JSON : ' + ST));
        if Node.Exists('selectivesync') then begin
            SelectiveSync := Node.Find('selectivesync').AsString;
            //debugln('TGitHubSync.ReadRemoteManifest - setting SelectiveSync to : ' + SelectiveSync);
        end;
        NotesNode := Node.Find('notes');
        if NotesNode = nil then
            exit(SayDebugSafe('TGitHubSync.ReadRemoteManifest ERROR invalid JSON, notes not present : ' + ST));
        for ANode in NotesNode do begin
            if ANode.Exists('title') and ANode.exists('lcdate') and ANode.Exists('cdate')
                    and ANode.Exists('format') and ANode.Exists('sha') and ANode.Exists('notebooks') then begin
                PGit := RemoteNotes.Find(ANode.Name);                           // note, we pass an ID with out path, Find adds Path internally
                if PGit = nil then
                    exit(SayDebugSafe('TGitHubSync.ReadRemoteManifest ERROR invalid JSON, FName not present in RemoteNotes : ' + ANode.AsString));
                //PGit^.FName := ANode.Name;
                PGit^.Title := ANode.Find('title').AsString;
                PGit^.CDate := ANode.Find('lcdate').AsString;
                if ANode.Find('format').AsString = 'md' then
                    PGit^.Format := ffMarkDown
                else PGit^.Format := ffEncrypt;
                if PGit^.Sha = ANode.Find('sha').AsString then                  // That is, if note is unchanged.
                    PGit^.LCDate := ANode.Find('lcdate').AsString;              // PGit points to a RemoteNotes record. ANode looks to the downloaded manifest.
                PGit^.Notebooks := ANode.Find('notebooks').AsJSON.Remove(0,12); // "notebooks" : ["Notebook1","Notebook2", "Notebook3"]
                // PGit^.Notebooks := ANode.Find('notebooks').AsArray.AsJson;
             end;
        end;
    finally
        Node.free;
    end;
    {$ifdef DEBUG}
    RemoteNotes.DumpList('TGithubSync.ReadRemoteManifest - After ReadRemoteManifest');
    {$endif}
end;

function TGithubSync.GetServerId : string;
var
   St : string;
begin
    Result := '';
    if DownloaderSafe(ContentsURL(True) + '/' + RMetaDir + 'serverid', ST) then
        Result := DecodeStringBase64(self.ExtractJSONField(ST, 'content'))
    else DebugLn('TGithubSync.GetServerID - NOTICE - failed to download serverid file');
    Result := Result.Replace(#10, '');
    Result := Result.Replace(#13, '');
    //debugln('TGithubSync.GetServerId = [' + Result + ']');
end;


function TGithubSync.DownloaderSafe(URL : string; out SomeString : String; const Header : string) : boolean;
var Cnt : integer = 0;
begin
    repeat
        Result := Downloader(URL, SomeString, Header);
        if Result then break;

        sleep(100*Cnt*Cnt*Cnt);        // 0, 100, 800, 2700, 6400
        if ProgressProcedure <> nil then
            ProgressProcedure('Download problem retrying ' + inttostr(cnt));
        inc(Cnt);
        DebugLn(#10'TGithubSync.DownloaderSafe - NOTICE retry no.' + Cnt.tostring + ' download failed ' + URL);
    until cnt = MaxNetTries;
    if not result then
        DebugLn('TGithubSync.DownloaderSafe - ERROR - download failed ! ' + ErrorString);
end;

function TGithubSync.Downloader(URL : string; out SomeString : String;
    const Header : string) : boolean;
var
    Client: TFPHttpClient;

begin
    //InitSSLInterface;
    // curl -i -u $GH_USER https://api.github.com/repos/davidbannon/libappindicator3/contents/README.note
    Client := TFPHttpClient.Create(nil);
    Client.UserName := UserName;
    Client.Password := Password; // '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';     eg a github token
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    Client.AddHeader('Content-Type','application/json; charset=UTF-8');
    Client.AllowRedirect := true;
    Client.ConnectTimeout := 8000;      // mS ?  was 3000, I find initial response from github very slow ....
    Client.IOTimeout := 4000;           // mS ? was 0
    SomeString := '';
    try
        try
            SomeString := Client.Get(URL);
        except
            on E: ESocketError do begin
                ErrorString := 'TGithubSync.Downloader - NOTICE - SocketError ' + E.Message     // eg failed dns, timeout etc
                    + ' ResultCode ' + inttostr(Client.ResponseStatusCode);
                SomeString := 'Fatal';
                exit(SayDebugSafe(ErrorString));
                end;
            on E: EInOutError do begin
                ErrorString := 'TGithubSync Downloader - InOutError ' + E.Message;
                exit(SayDebugSafe(ErrorString));
                end;
            on E: ESSL do begin
                ErrorString := 'TGithubSync.Downloader - SSLError ' + E.Message;         // eg openssl problem, maybe FPC cannot work with libopenssl
                SomeString := 'Fatal';
                exit(SayDebugSafe(ErrorString));
                end;
            on E: Exception do begin
                ErrorString := 'TGitHubSync.Downloader Exception ' + E.Message + ' downloading ' + URL;
                case Client.ResponseStatusCode of
                    401 : ErrorString := ErrorString + ' 401 Maybe your Token has expired or password is invalid ??';
                    404 : if URL.EndsWith('serverid') then
                                ErrorString := 'TGitHubSync.Downloader : ServerID not found (OK in New Sync)'
                          else ErrorString := ErrorString + ' 404 File not found ' + URL;
                    403, 409 : ErrorString := ErrorString + ' 403 Maybe github rate exceeded ? ';
                    else ErrorString := ErrorString + ' https error no ' + inttostr(Client.ResponseStatusCode);
                end;
                exit(SayDebugSafe(ErrorString));
                end;
        end;
        with Client.ResponseHeaders do begin
            if Header <> '' then begin

                // if the required header is not found, we will look for another known one, X-RateLimit-Remaining
                // if that is there set HeaderOut to 'unavailable' (that will appear in Sync Report) but if
                // the second one fails, set HeaderOut to '' and that will abort sync

                if DebugMode then debugln('TGithubSync.Downloader - looking for a response header, ' + Header);    // added post 0.41
                if DebugMode then begin
                    debugln('-------------------------------------');
                    debugln(Client.ResponseHeaders.Text);
                    debugln('-------------------------------------');
                end;
                if IndexOfName(Header) = -1 then
                    if IndexOfname('X-RateLimit-Remaining') = -1 then begin
                        HeaderOut := '';                              // that will abort sync, we cannot have a valid header.
                        debugln('WARNING TGithubSync.Downloader failed to get valid header back from github');
                    end else HeaderOut := 'unavailable'               // Sync will proceed, token may not have an expiry date set, user's choice
                else
                    HeaderOut := ValueFromIndex[IndexOfName(Header)]; // we found a valid expiry date.
            end;
        end;
    finally
        Client.Free;
    end;
    result := true;          // debugln('TGithubSync.Downloader returned true');
end;

function TGithubSync.SendData(const URL, BodyJSt : String; Put : boolean; FName : string) : boolean;
var
    Client: TFPHttpClient;
    Response : TStringStream;
begin
    Result := false;
    //SayDebugSafe('TGitHubSync.SendData - Posting to ' + URL);
    Client := TFPHttpClient.Create(nil);
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    Client.AddHeader('Content-Type','application/json; charset=UTF-8');
    Client.AddHeader('Accept', 'application/json');
    Client.AllowRedirect := true;
    Client.UserName:=UserName;
    Client.Password:=Password;
    client.RequestBody := TRawByteStringStream.Create(BodyJSt);
    Response := TStringStream.Create('');
    try
        try
            if Put then begin
                client.Put(URL, Response);
                //DumpJSON(Response.DataString, 'SendData just after PUT');
                if (FName <> '')                                                // if FName is provided, is uploading a file
                        and (Client.ResponseStatusCode = 200) or (Client.ResponseStatusCode = 201) then
                    RemoteNotes.Add(FName, ExtractJSONField(Response.DataString, 'sha', 'content'));      // if put failed, don't update SHA
            end else
                client.Post(URL, Response);  // don't use FormPost, it messes with the Content-Type value
            if (Client.ResponseStatusCode = 200) or (Client.ResponseStatusCode = 201) then
                Result := True
            else begin
                SayDebugSafe('GitHub.SendData : Post ret ' + inttostr(Client.ResponseStatusCode));
                SayDebugSafe(Client.ResponseStatusText);
            end;
        except on E:Exception do begin
                ErrorString := 'GitHub.SendData - bad things happened : ' + E.Message;
                exit(SayDebugSafe(ErrorString));
                end;
        end;
    finally
        Client.RequestBody.Free;
        Client.Free;
        Response.Free;
    end;
end;
// ToDo : investigate
{GitHub.SendData : Post ret 409
Conflict
TGitHubSync.SendFile - NOTICE, retry no.1 : Failed to send file filename=README.md Error=
}

function TGithubSync.ContentsURL(API : boolean) : string;
begin
    if API then
        Result := BaseURL + 'repos/' + UserName + '/' + RemoteRepoName + '/contents'
    else  Result := GITBaseURL + UserName + '/' + RemoteRepoName + '/blob/main/';
end;


// -------------------- J S O N   T O O L S ------------------------------------


// Returns content asociated with Field at either toplevel or if there are up to
// two level names, that field down up to two fields down. Level1 is upper ....
function TGithubSync.ExtractJSONField(const data, Field : string;
    Level1 : string; Level2 : string) : string;
var
    Node, ANode : TJsonNode;
begin
    result := '';
    Node := TJsonNode.Create;
    ANode := Node;                  // Don't change Node, free will not be able to find it.
    try
        if not Node.TryParse(data) then exit('Failed to parse JSON data');
        if Level1 <> '' then
            ANode := ANode.Find(Level1);
            if ANode = nil then
                exit('JSON ERROR - field not found 1 : ' + Level1);
        if Level2 <> '' then
            ANode := ANode.Find(Level2);
            if ANode = nil then
                exit('JSON ERROR - field not found 2 : ' + Level2);
        ANode := ANode.Find(Field);
        if ANode = nil then
            result := 'JSON ERROR - field not found 3 : ' + Field
        else result := ANode.AsString;
    finally
        Node.Free;
    end;
end;


(*    WARNING this uses FPjson, needs to be rewritten before use.
procedure TGitHubSync.DumpJSON(const St: string; WhereFrom: string);
var
    jData : TJSONData;
begin
    if Wherefrom <> '' then
        SayDebugSafe('------------ Dump from ' + Wherefrom + '-------------');
    JData := GetJSON(St);
    SayDebugSafe('---------- JSON ------------');
    SayDebugSafe(jData.FormatJSON);
    SayDebugSafe('----------------------------');
    JData.Free;
end;                     *)


end.


// =============================================================================
// =============================================================================
// =============================================================================


{ Notebook Syntax -
  ---------------
A note not in any notebooks -
<tags>
</tags>

A notebook template -
<tags>
   <tag>system:template</tag>
   <tag>system:notebook:MacStuff</tag>
 </tags>

A note in a notebook -
<tags>
  <tag>system:notebook:MacStuff</tag>
</tags>
}

// =========================  J S O N   R E S P O N S E S ======================

(*  ----------- Commit response  -----------------

   {    "sha"     : "fb1dfecbc50329c7bedfc9ae00ba522bc3bd8536",
        "node_id" : "MDY6Q29tbWl0Mzk0NTAzNTYxOmZiMWRmZWNiYzUwMzI5YzdiZWRmYzlhZTAwYmE1MjJiYzNiZDg1MzY=",
        "commit"  : {
                "author" : {
                        "name"  : "blar",
                        "email" : "blar",
                        "date"  : "2021-08-11T04:09:43Z"
    ....
    quite a lot more follows ...
    .....
*)

(*
    ---------- This is the directory listing, in this case, of Notes.  Note its  ----------
               an array, wrapped in [], one array element per file or dir found.
               We need only Name and sha.
[{
    "name"    :"06c8c753-77df-4b0f-a855-3d1416ef0260.md",
    "path"    :"Notes/06c8c753-77df-4b0f-a855-3d1416ef0260.md",
    "sha"     :"f11b48e4204e442759d1250a39c6d4a683f634f3",
    "size"    :6323,
    "url"     :"https://api.github.com/repos/davidbannon/tb_test/contents/Notes/06c8c753-77df-4b0f-a855-3d1416ef0260.md?ref=main",
    "html_url":"https://github.com/davidbannon/tb_test/blob/main/Notes/06c8c753-77df-4b0f-a855-3d1416ef0260.md",
    "git_url" :"https://api.github.com/repos/davidbannon/tb_test/git/blobs/f11b48e4204e442759d1250a39c6d4a683f634f3",
    "download_url":"https://raw.githubusercontent.com/davidbannon/tb_test/main/Notes/06c8c753-77df-4b0f-a855-3d1416ef0260.md?token=ABP76MGKOA3EL4QAG5VQSVLBIRS3Q",
    "type":"file",
    "_links" : {
        "self":"https://api.github.com/repos/davidbannon/tb_test/contents/Notes/06c8c753-77df-4b0f-a855-3d1416ef0260.md?ref=main",
        "git" :"https://api.github.com/repos/davidbannon/tb_test/git/blobs/f11b48e4204e442759d1250a39c6d4a683f634f3",
        "html":"https://github.com/davidbannon/tb_test/blob/main/Notes/06c8c753-77df-4b0f-a855-3d1416ef0260.md"
    }
},
....
....
]
*)

(* ----------------- Remote Manifest ------------------

{
  "notes" : {
       "903C31B1-229E-44E6-8FA3-EAAD53ED3E77" : {
           "title" : "Man Pages",
           "cdate" : "2021-08-01T19:13:24.7238449+10:00",
           "lcdate" : "2021-08-04T20:13:10.1153643+10:00",
           "sha" : "1c3b3dd579f8e46481fd5b5c93044d1c50448f4c",
           "format" : "md",
           "notebooks" : ["template", "Man Pages"]
    },
       "B4B75FFE-996B-4D9A-B978-71D55427C7F1" : {
           "title" : "Installing LazarusCross Compiler",
           "cdate" : "2018-01-29T11:21:07.8490000+11:00",
           "lcdate" : "2021-09-15T11:42:00.4447428+10:00",
           "sha" : "96d4a70f065a92e099e4129f8bc9d3f3e568e4bc",
           "format" : "md",
           "notebooks" : []
    },
.....
.....
}                       *)












