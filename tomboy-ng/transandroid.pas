unit transandroid;

{ A unit that does the file transfer side of a very limited one to one Tomdroid Sync
  *  Copyright (C) 2018 David Bannon
  *  See attached licence file.

  HISTORY
  2018/10/28    Improve error checking on SetServerID(), needs to be applied to
                all similar methods.
  2018/11/20    Try Finally around SetsrverID() stuff to stop memory leak. Check
                if that still handles a bad password ???





  Sync.SetTransport -
        Selects a Trans layer, adjusts config dir,  Ping device ? That would
        indicate its there.

  Sync.TestTransport
        In repoUse mode - Reads Local Manifest (if exists), calls Trans.TestTransport,
        compares localServerID (from config and local manifest).
        If serverID problem, consult user,  rets SyncMismatch
        In RepoNew mode, we ignore any local manifest and both local and remote
        serverIDs. A fresh start.

  Trans.TestTransport (here, for android)
        If not JoinRepo, grabs the devices serverID.
        If JoinRepo, generates a new ServerID and puts it on device.
        Checks for remote (Tomdroid made) directory. If there is no remote
        serverID present, one is immedialy made (Thats not really as it should be !)
        We should now have a valid Trans.ServerID, either the existing one or a new one.

  Note that because of how the file based Tomdroid sync works, we set action to
        either RepoUse or RepoJoin, not RepoNew ! Join here is effectivly a combination
        of Join and New. A Join overwrites an existing ServerID with a new one.

        Tomdroid seems to need to be stopped after each (internal) sync to be sure of
        reliable notice of deleted notes. Otherwise, it sometimes seems to not notice that
        a previously synced note has now dissapeared from its sync dir (as a result of
        -ng syncing there) and therefore does not remove that note from its dbase
        when syncing.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, process, trans, SyncUtils;

type

  { TAndSync }

  TAndSync = Class(TTomboyTrans)
    private
        function ChangeNoteDateUTC(const ID: string): string;
        function CheckRemoteDir: TSyncAvailable;
        function DownLoad(const ID, FullNoteName: string): boolean;
        function GetDroidMetaData(AStringList: TStringList): boolean;
        function GetNoteLastChange(const FullFileName: string): string;
        procedure InsertNoteBookTags(const FullSourceFile, FullDestFile,
            TagString: string);
        function RemoteFileExists(const ID: string): boolean;
        function RunFSSync(): boolean;
            // May return SyncReady, SyncNoRemoteRepo (if unable to find a remote ServerID), SyncNetworkError
        function SetServerID(): TSyncAvailable;
        function Ping(const Count : integer): boolean;
        function StampServerID(const ID: string): boolean;
            // Reads the (filesync) remote Manifest for synced note details. It gets ID, RevNo
            // and, if its there the LastChangeDate. If LCD is not in manifest and GetLCD
            // is True, gets it from the file.

        function UpLoad(const ID: string): boolean;

    public
        //RemoteDir : string; // where the remote filesync repo lives.
        function TestTransport(const WriteNewServerID : boolean = False) : TSyncAvailable; override;
        function SetTransport() : TSyncAvailable; override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
            { ToDo : transandroid version - deletes the indicated note from remote device
              returns False if the note was not found there to be deleted.  Other error
              are possible.}
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
        // function SetRemoteRepo(ManFile : string = '') : boolean; override;
  end;


implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil, LazLogger, searchUnit;

const // Must become config things eventually.
  //Password = 'admin';
  DevDir = '/storage/emulated/0/tomdroid/';

{ TAndSync }

function TAndSync.Ping(const Count : integer) : boolean;
    // Ping returns 0 or one or more packets came back, 1 if none, 2 for other error
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'ping';
    AProcess.Parameters.Add('-qc' + inttostr(Count));
    AProcess.Parameters.Add(RemoteAddress);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        AProcess.Execute;
        Result := (AProcess.ExitStatus = 0);        // says at least one packet got back
    except on
        E: EProcess do ErrorString := 'EProcess Error during Ping';
    end;
    if not Result then
        ErrorString := 'Not able to ping device, check its awake and IP is correct. ';
    List := TStringList.Create;
    List.LoadFromStream(AProcess.Output);       // just to clear it away.
    List.Free;
    AProcess.Free;
end;

function TAndSync.StampServerID(const ID : string) : boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    result := true;
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('ssh');
    AProcess.Parameters.Add('-p2222');
    // AProcess.Parameters.Add('-o');
    // AProcess.Parameters.Add('StrictHostKeyChecking=no');
    AProcess.Parameters.Add('root@' + self.RemoteAddress);
    AProcess.Parameters.Add('echo "' + ID + '" > tomboy.serverid');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        try
            AProcess.Execute;
            List := TStringList.Create;
            List.LoadFromStream(AProcess.Output);
        except on
            E: EProcess do ErrorString := 'EProcess Error ' + E.Message;
            on E: EExternal do ErrorString := 'Some process error ' + E.Message;
        end;
        if debugmode then debugln('StampServerID [' + ID + ']  [' + List.Text + ']');
        Result := (AProcess.ExitStatus = 0);
    finally
        FreeandNil(List);
        AProcess.Free;
    end;
end;

function TAndSync.RunFSSync() : boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    Result := True;
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('ssh');
    AProcess.Parameters.Add('-p2222');
    AProcess.Parameters.Add('-o');
    AProcess.Parameters.Add('StrictHostKeyChecking=no');    // probably first ssh call to device !
    AProcess.Parameters.Add('root@' + self.RemoteAddress);
    AProcess.Parameters.Add('sync');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        try
            AProcess.Execute;
            List := TStringList.Create;
            List.LoadFromStream(AProcess.Output);           // Output should be empty, StdErr contains ssh prompts
            if Length(List.Text) <> 0 then begin
                result := False;
                debugln('Tomdroid Sync, error when sending FSSync :');
                debugln('[' + List.Text + ']');
            end;
        except on
            E: EProcess do begin
                ErrorString := 'EProcess Error ' + E.Message;
                debugln('RunFSSync ' + ErrorString);
                Result := False;
            end
        end;
    finally
        FreeandNil(List);
        AProcess.Free;
    end;
end;

    // May return SyncNetworkError, SyncNoRemoteDir, SyncReady
function TAndSync.CheckRemoteDir : TSyncAvailable;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('ssh');
    AProcess.Parameters.Add('-p2222');
    AProcess.Parameters.Add('-o');
    AProcess.Parameters.Add('StrictHostKeyChecking=no');    // probably first ssh call to device !
    AProcess.Parameters.Add('root@' + self.RemoteAddress);
    AProcess.Parameters.Add('ls');
    AProcess.Parameters.Add('-d');
    AProcess.Parameters.Add(DevDir);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    // CL (eg) sshpass -p admin ssh -p2222 -o StrictHostKeyChecking=no root@192.168.1.174 ls /storage/emulated/0/tomdroid/
    try
        try
            AProcess.Execute;
            //if debugmode then debugln('SetServerID - Executed');
            List := TStringList.Create;
            List.LoadFromStream(AProcess.Output);
            //if debugmode then debugln('SetServerID - Loadfromstream');
            if length(List.Text) = 0 then begin
                if debugmode then debugln('CheckRemoteDir - Length was zero');
                List.LoadFromStream(AProcess.Stderr);
                if length(List.Text) = 0 then
                    ErrorString := 'Unable to connect, unknown error'
                else if pos('Connection refused', List.Text) > 0 then
                    ErrorString := 'Unable to connect, is ssh server running ?'
                else if pos('Permission denied', List.Text) > 0 then
                    ErrorString := 'Unable to connect, check password'
                else ErrorString := List.Text;
                if Debugmode then debugln('CheckRemoteDir returning SyncNetworkError ' + ErrorString);
                exit(SyncNetworkError);
            end;
        except on
            E: EProcess do begin
                ErrorString := 'EProcess Error ' + E.Message;
                debugln('SetServerID ' + ErrorString);
                Result := SyncNetworkError;
                exit;
            end
        end;
        if pos('No such file or directory', List.Text) > 0 then exit(SyncNoRemoteDir);  // no ID present, uninitialized ?
    finally
        FreeandNil(List);
        AProcess.Free;
    end;
    Result := SyncReady;
end;

function TAndSync.SetServerID() : TSyncAvailable;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    ServerID := '';
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('ssh');
    AProcess.Parameters.Add('-p2222');
    AProcess.Parameters.Add('-o');
    AProcess.Parameters.Add('StrictHostKeyChecking=no');
    AProcess.Parameters.Add('root@' + self.RemoteAddress);
    AProcess.Parameters.Add('cat tomboy.serverid');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    // CL (eg) sshpass -p admin ssh -p2222 root@192.168.1.174 cat tomboy.serverid
    try
        try
            AProcess.Execute;
            //if debugmode then debugln('SetServerID - Executed');
            List := TStringList.Create;
            List.LoadFromStream(AProcess.Output);
            //if debugmode then debugln('SetServerID - Loadfromstream');
            if length(List.Text) = 0 then begin
                if debugmode then debugln('SetServerID - Length was zero');
                List.LoadFromStream(AProcess.Stderr);
                if length(List.Text) = 0 then
                    ErrorString := 'Unable to connect, unknown error'
                else if pos('Connection refused', List.Text) > 0 then
                    ErrorString := 'Unable to connect, is ssh server running ?'
                else if pos('Permission denied', List.Text) > 0 then
                    ErrorString := 'Unable to connect, check password'
                else ErrorString := List.Text;
                exit(SyncNetworkError);
            end;
        except on
            E: EProcess do begin
                ErrorString := 'EProcess Error ' + E.Message;
                debugln('SetServerID ' + ErrorString);
                Result :=  SyncNetworkError;
                exit();
            end
        end;
        if pos('No such file or directory', List.Text) > 0 then exit(SyncNoRemoteRepo); // no ID present, uninitialized ?
        if List.Count > 0 then
            ServerID := copy(List.Strings[List.Count-1], 1, 36);                        // Thats, perhaps, a serverID
        if debugmode then debugln('SetServerID [' + ServerID + ']' + List.Text);
    finally
        FreeandNil(List);
        AProcess.Free;
    end;
    if not IDLooksOK(ServerID) then begin
        Debugln('SetServerID unable to read tomboy.serverid, we got [' + List.Text + ']');
        exit(SyncNoRemoteRepo);     // No really NoRemoteRepo but a currupted ID ?
    end;
    Result := SyncReady;
end;

function TAndSync.TestTransport(const WriteNewServerID : boolean = False): TSyncAvailable;
// OK, droping TestTransportEarly and merging most back here.
var
    GUID : TGUID;
    T1, T2, T3, T4 : DWord;
begin
    { ssh in, read serverID file, its in <landing place> tomboy.serverid
    makes sure the expected Sync Dir exists
    }
    ErrorString := '';
    T1 := GetTickCount64();
    Result := CheckRemoteDir();
    if Result <> SyncReady then exit;
    RunFSSync();
    if ANewRepo then Result := SyncNoRemoteRepo
    else Result := SetServerID();
    T2 := GetTickCount64();
    T3 := T2;
    T4 := T2;
    if Result <> SyncReady then begin
        if Result = SyncNoRemoteRepo then begin
            T3 := GetTickCount64();
            CreateGUID(GUID);
            ServerID := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
            if WriteNewServerID and StampServerID(ServerID) then Result := SyncReady
            else Result := SyncReady;
            T3 := GetTickCount64();
            if DebugMode then debugln('Made a new serverID ' + ServerID );
        end else begin
            debugln(ErrorString);
            exit;
        end;
    end;
    if debugmode then
        debugln('TestTransport ID=' + ServerID + ' SetServerID took ' + inttostr(T2 - T1)
            + 'mS and StampID took ' + inttostr(T4 - T3));
end;

function TAndSync.SetTransport(): TSyncAvailable;
var
    T1, T2 : DWord;
begin
    T1 := GetTickCount64();
    if not Ping(1) then
        if not Ping(2) then
            if not Ping(5) then begin
                debugln('Failed to ping ' + RemoteAddress);
                exit(SyncNetworkError);
            end;
    T2 := GetTickCount64();
    if debugmode then
        debugln('SetTransport Ping took ' + inttostr(T2 - T1));
    result := SyncReady;
end;


function TAndSync.GetDroidMetaData(AStringList : TStringList) : boolean;
var
    AProcess: TProcess;
begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('ssh');
    AProcess.Parameters.Add('-p2222');
    AProcess.Parameters.Add('root@' + self.RemoteAddress);
    AProcess.Parameters.Add('cd ' + DevDir + '; grep -H "<last-change-date>" *.note');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        AProcess.Execute;
        AStringList.LoadFromStream(AProcess.Output);
        // AStringList.SaveToFile('output.txt');
        Result := (AProcess.ExitStatus = 0);
    except on
        E: EProcess do ErrorString := 'EProcess Error ' + E.Message;
    end;
    if not Result then
        ErrorString := 'something bad happened';
    AProcess.Free;
end;

function TAndSync.GetNewNotes(const NoteMeta: TNoteInfoList; const GetLCD : boolean): boolean;
var
        StList: TStringList = nil;
        I : integer;
        NoteInfo : PNoteInfo;
begin
    // by nature of how we get remote note date, always get LCD
    if NoteMeta = Nil then begin
        ErrorString := 'Passed an uncreated list to GetNewNotes()';
        exit(False);
    end;
    StList := TStringList.Create;
    GetDroidMetaData(StList);
    if StList.Count > 0 then begin
        for I := 0 to StList.Count -1 do begin
            // if Debugmode then debugln('RET - [' + StList.Strings[I] + ']');
            new(NoteInfo);
            NoteInfo^.Action:=SyUnset;
            NoteInfo^.ID := copy(StList.Strings[I], 1, 36);
            NoteInfo^.Rev := -1;
            NoteInfo^.LastChange := copy(StList.Strings[I], pos('>', StList.Strings[I])+1, 33);
            NoteInfo^.LastChangeGMT := GetGMTFromStr(NoteInfo^.LastChange);
            NoteMeta.Add(NoteInfo);
        end;
    end;
    freeandNil(StList);
    result := True;
end;


function TAndSync.GetNoteLastChange(const FullFileName : string) : string;
begin
    Result := GetNoteLastChangeSt(FullFileName, ErrorString);   // syncutils function
end;

            // Puts back the tag string into a temp note downloaded from dev and puts it in note dir, overwrites
procedure TAndSync.InsertNoteBookTags(const FullSourceFile, FullDestFile, TagString : string);
var
    InFile, OutFile: TextFile;
    InString : string;
begin
    AssignFile(InFile, FullSourceFile);
    AssignFile(OutFile, FullDestFile);
    try
        try
            Reset(InFile);
            Rewrite(OutFile);
            while not eof(InFile) do begin
                readln(InFile, InString);
                if (Pos('</y>', InString) > 0) then begin
                    writeln(OutFile, InString);
                    writeln(outFile, TagString);
                end else writeln(OutFile, InString);
            end;
        finally
            CloseFile(OutFile);
            CloseFile(InFile);
        end;
    except
        on E: EInOutError do
            debugln('File handling error occurred. Details: ' + E.Message);
    end;
end;

function TAndSync.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
var
    I : integer;
begin
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
                    ErrorString := 'Failed to copy file to Backup ' + NotesDir + Downloads.Items[I]^.ID + '.note';
                    exit(False);
                end;
            // OK, now pull down the file.
            if not DownLoad(Downloads.Items[I]^.ID, NotesDir + Downloads.Items[I]^.ID + '.note') then begin
                Debugln('ERROR - in TAndSync.DownloadNotes ' + ErrorString);
                exit(false);
            end;
        end;
    end;
    result := True;
{var                                      // trash this, turned out completely unnecessary !
    I : integer;
    NoteBookTags, DownloadTo : string;
begin
    if not DirectoryExists(NotesDir + 'Backup') then
        if not ForceDirectory(NotesDir + 'Backup') then begin
            ErrorString := 'Failed to create Backup directory.';
            exit(False);
        end;
    for I := 0 to DownLoads.Count-1 do begin
        if DownLoads.Items[I]^.Action = SyDownLoad then begin
            DownLoadTo := NotesDir + Downloads.Items[I]^.ID + '.note';
            if FileExists(NotesDir + Downloads.Items[I]^.ID + '.note') then begin
                NoteBookTags := SearchForm.NoteLister.NotebookTags(Downloads.Items[I]^.ID + '.note');
                if NoteBookTags <> '' then begin
                    DownLoadTo := ConfigDir + 'downFromDroid.note';
                    if debugmode then debugln('Note has tags, download to '+ DownloadTo);
                end;
                // First make a Backup copy
                if not CopyFile(NotesDir + Downloads.Items[I]^.ID + '.note',
                        NotesDir + 'Backup' + PathDelim + Downloads.Items[I]^.ID + '.note') then begin
                    ErrorString := 'Failed to copy file to Backup ' + NotesDir + Downloads.Items[I]^.ID + '.note';
                    debugln('Failed to copy [' + NotesDir + Downloads.Items[I]^.ID + '.note]');
                    debugln('to [' + NotesDir + 'Backup' + PathDelim + Downloads.Items[I]^.ID + '.note]');
                    exit(False);
                end;
            end;
            // OK, now pull down the file.
            if not DownLoad(Downloads.Items[I]^.ID, DownLoadTo) then begin
                Debugln('ERROR - in TAndSync.DownloadNotes ' + ErrorString);
                exit(false);
            end;
            if NoteBookTags <> '' then
                InsertNoteBookTags(DownLoadTo, NotesDir + Downloads.Items[I]^.ID + '.note', NoteBookTags);
        end;
    end;
    result := True;     }
end;

function TAndSync.RemoteFileExists(const ID: string): boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('ssh');
    AProcess.Parameters.Add('-p2222');
    AProcess.Parameters.Add('-o');
    AProcess.Parameters.Add('StrictHostKeyChecking=no');    // probably first ssh call to device !
    AProcess.Parameters.Add('root@' + RemoteAddress);
    AProcess.Parameters.Add('ls');
    AProcess.Parameters.Add(DevDir + ID + '.note');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    { If we found the indicated file, its name will appear in stdout. There
      will still be something in stderr, ssh prompts etc.
      If we have NOT found it, then -
      * Stdout is empty.
      * The name will appear in stderror
      * The phrase "No such file or directory" will appear in stderr
    }
    Result := False;
    try
        try
            AProcess.Execute;
            List := TStringList.Create;
            List.LoadFromStream(AProcess.Output);
            if length(List.text) > 0 then
                if pos(ID, List.Text) > 0 then
                    Result := True;
          except on
            E: EProcess do begin
                ErrorString := 'EProcess Error ' + E.Message;
                debugln('RemoteFileExists ' + ErrorString);
                Result := False;
            end
        end;
    finally
        FreeandNil(List);
        AProcess.Free;
    end;
end;

function TAndSync.DeleteNote(const ID: string; const ExistRev : integer ): boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('ssh');
    AProcess.Parameters.Add('-p2222');
    AProcess.Parameters.Add('-o');
    AProcess.Parameters.Add('StrictHostKeyChecking=no');    // probably first ssh call to device !
    AProcess.Parameters.Add('root@' + self.RemoteAddress);
    AProcess.Parameters.Add('rm');
    AProcess.Parameters.Add(DevDir + ID + '.note');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        try
            result := True;                             // hope for the best
            AProcess.Execute;
            List := TStringList.Create;
            List.LoadFromStream(AProcess.Output);
            if length(List.Text) = 0 then begin         // thats good
                List.LoadFromStream(AProcess.Stderr);
                if length(List.Text) <> 0 then          // thats bad
                    if pos('No such file or directory', List.Text) > 0 then exit(False);
            end;
        except on
            E: EProcess do begin
                ErrorString := 'EProcess Error ' + E.Message;
                debugln('ERROR in DeleteNote ' + ErrorString);
                Result := False;
            end
        end;
        if Debugmode then debugln('Transandroid DeleteNote removed ' + ID +' from device');
    finally
        FreeandNil(List);
        AProcess.Free;
    end;
end;

function TAndSync.UploadNotes(const Uploads: TStringList): boolean;
var
    Index : integer;
begin
    for Index := 0 to Uploads.Count -1 do begin
        if DebugMode then debugln('Uploading ' + Uploads.Strings[Index] + '.note');
        if not UpLoad(Uploads.Strings[Index]) then begin
            debugln('ERROR in TAndSync.UploadNotes' + ErrorString);
            exit(False);
        end;
    end;
    RunFSSync();
    result := True;
end;

function TAndSync.DoRemoteManifest(const RemoteManifest: string): boolean;
begin
    // The Tomdroid sync model does not use a remote manifest.
  result := True;
end;

function TAndSync.ChangeNoteDateUTC(const ID : string) : string;
var
    InFile, OutFile: TextFile;
    NoteDateSt, InString : string;
begin
    NoteDateSt := GetNoteLastChangeSt(NotesDir + ID + '.note', ErrorString);
    // debugln('Upload note date was ' + NoteDateSt);
    NoteDateSt := ConvertDateStrAbsolute(NoteDateSt);
    // debugln('Upload note date is  ' + NoteDateSt);
    AssignFile(InFile, NotesDir + ID + '.note');
    AssignFile(OutFile, ConfigDir + 'remote.note');
    try
        try
            Reset(InFile);
            Rewrite(OutFile);
            while not eof(InFile) do begin
                readln(InFile, InString);
                if (Pos('<last-metadata-change-date>', InString) > 0) or
                        (Pos('<last-change-date>', InString) > 0)  then begin
                    if (Pos('<last-metadata-change-date>', InString) > 0) then
                        writeln(OutFile, ' <last-metadata-change-date>' +  NoteDateSt + '</last-metadata-change-date>')
                    else writeln(OutFile, ' <last-change-date>' +  NoteDateSt + '</last-change-date>');
                end else writeln(OutFile, InString);
            end;
        finally
            CloseFile(OutFile);
            CloseFile(InFile);
        end;
    except
        on E: EInOutError do
            debugln('File handling error occurred. Details: ' + E.Message);
    end;
    Result := ConfigDir + 'remote.note';
end;

function TAndSync.UpLoad(const ID : string ) : boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    // OK, seems Tomdroid, likes its date strings in UTC with zero offset,
    // messes with sync (I suspect).  So, before uploading a file, we'll
    // stuff about with its date strings.....
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('scp');
    AProcess.Parameters.Add('-P2222');
    AProcess.Parameters.Add(ChangeNoteDateUTC(ID));
    // AProcess.Parameters.Add(NotesDir + ID + '.note');
    AProcess.Parameters.Add('root@' + RemoteAddress + ':' + DevDir + ID + '.note');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        AProcess.Execute;
        Result := (AProcess.ExitStatus = 0);
    except on
        E: EProcess do ErrorString := 'EProcess Error during upload';
    end;
    if not Result then
        ErrorString := 'something bad happened when uploading ' + ID;
    List := TStringList.Create;
    List.LoadFromStream(AProcess.Output);
    List.Free;
    AProcess.Free;
end;

function TAndSync.DownLoad(const ID, FullNoteName : string ) : boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('scp');
    AProcess.Parameters.Add('-P2222');
    AProcess.Parameters.Add('root@' + RemoteAddress + ':' + DevDir + ID + '.note');
    AProcess.Parameters.Add(FullNoteName);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        AProcess.Execute;
        Result := (AProcess.ExitStatus = 0);
    except on
        E: EProcess do ErrorString := 'EProcess Error during download';
    end;
    if not Result then begin
        ErrorString := 'something bad happened when downloading ' + ID;
        debugln('Failed to download [' + RemoteAddress + ':' + DevDir + ID + '.note]');
        debugln(' to [' + FullNoteName + ']');
    end;
    List := TStringList.Create;
    List.LoadFromStream(AProcess.Output);
    List.Free;
    AProcess.Free;
end;

function TAndSync.DownLoadNote(const ID: string; const RevNo: Integer): string;
    // Here we will pull down the indicated note and return its fullname. In 'droid
    // mode, this is only used to get note title and then only when its a new note
    // on remote device. If this proves too slow, we could capture all this data
    // in the same way we get LCD. Please consider.
begin
    if DebugMode then debugln('Download to Temp ' + ID);
    if DownLoad(ID, self.ConfigDir + 'remote.note') then
        Result := ConfigDir + 'remote.note'
    else Result := '';
end;

end.

