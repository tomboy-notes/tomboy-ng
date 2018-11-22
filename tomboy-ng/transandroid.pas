unit transandroid;

{ A unit that does the file transfer side of a very limited one to one Tomdroid Sync
  *  Copyright (C) 2018 David Bannon
  *  See attached licence file.

  HISTORY
  2018/10/28    Improve error checking on SetServerID(), needs to be applied to
                all similar methods.
  2018/11/20    Try Finally around SetsrverID() stuff to stop memory leak. Check
                if that still handles a bad password ???


  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  Must add -o StrickHostKeyChecking=no   to ssh command that initially looks for the serverID
  on device. Maybe only for first run ?  A run that was saved in cfg file ?

  So, if loaded profile is unchanged, we'll check for matching serverID and a corresponding
  local profile.  If thats what happens, we'll proceed to sync.

  A forcejoin is same as a new join (here), so, if above test fails, we'll ask user if they want to 'join' ?
  A join will replace existing server.id (if it exists) and not load a local mainifest.
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  Normal Sync assumes IP address of both PC and device have not changed.

  Profile is loaded from config file, that will have name, server ID, IP address and password (?)

  We call sync.Settransport and it calls Trans.TestTransport{early} [pings device, reads its serverID
  and if not present or bad, creates a new one and puts it in place on device]

  We then call sync.testconnection. In a RepoUse case, it reads local manifest,


  ---------------------
  All wrong !
  ---------------------

  get rid of the Trans TestTransportEarly and work the same as we do with other Trans.

  Sync.SetTransport -
        Selects a Trans layer, adjusts config dir,  Does this Ping device ? That would
        indicate its there and its ssh server is running.

  Sync.TestTransport
        In repoUse mode - Reads Local Manifest (if exists), calls Trans.TestTransport,
        currently does nothing but much of Trans.TestTransportEarly goes there.
        compares localServerID (from config and local manifest).
        If serverID problem, consult user.  rets SyncMismatch
        In RepoNew mode, we ignore any local manifest and both local and remote
        serverIDs. A fresh start.

  Trans.TestTransport (here, for android)
        If not NewRepo, grabs the devices serverID.
        If NewRepo, generates a new ServerID and puts it on device.

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, process, trans, SyncUtils;

type

  { TAndSync }

  TAndSync = Class(TTomboyTrans)
    private
        function DownLoad(const ID, FullNoteName: string): boolean;
        function GetDroidMetaData(AStringList: TStringList): boolean;
        function GetNoteLastChange(const FullFileName: string): string;
        function SetServerID(): boolean;
        function Ping(const Count : integer): boolean;
        function StampServerID(const ID: string): boolean;
            // Reads the (filesync) remote Manifest for synced note details. It gets ID, RevNo
            // and, if its there the LastChangeDate. If LCD is not in manifest and GetLCD
            // is True, gets it from the file.
        function UpLoad(const ID: string): boolean;

    public
        //RemoteDir : string; // where the remote filesync repo lives.
        function TestTransport() : TSyncAvailable; override;
        function TestTransportEarly(out ManPrefix : string) : TSyncAvailable; override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
        // function SetRemoteRepo(ManFile : string = '') : boolean; override;
  end;


implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil, LazLogger;

const // Must become config things eventually.
  //Password = 'admin';
  DevDir = '/storage/emulated/0/tomdroid/';

{ TAndSync }

function TAndSync.Ping(const Count : integer) : boolean;
    // Ping retuns 0 of one or more packets came back, 1 if none, 2 for other error
var
    AProcess: TProcess;
    List : TStringList;
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
    List : TStringList;
begin
    result := true;
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('ssh');
    AProcess.Parameters.Add('-p2222');
    AProcess.Parameters.Add('root@' + self.RemoteAddress);
    AProcess.Parameters.Add('echo "' + ID + '" > tomboy.serverid');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
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
    FreeandNil(List);
    AProcess.Free;
end;

function TAndSync.SetServerID() : boolean;       // returns true if it connected but ServerID may still be empty
var
    AProcess: TProcess;
    List : TStringList;
begin
    ServerID := '';
    result := true;
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('ssh');
    AProcess.Parameters.Add('-p2222');
    AProcess.Parameters.Add('root@' + self.RemoteAddress);
    AProcess.Parameters.Add('cat tomboy.serverid');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    // CL (eg) sshpass -p admin ssh -p2222 root@192.168.174 cat tomboy.serverid
    try
        try
            AProcess.Execute;
            if debugmode then debugln('SetServerID - Executed');
            List := TStringList.Create;
            List.LoadFromStream(AProcess.Output);
            if debugmode then debugln('SetServerID - Loadfromstream');
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
                    exit(false);
            end;
        except on
            E: EProcess do begin
                ErrorString := 'EProcess Error ' + E.Message;
                debugln('SetServerID ' + ErrorString);
            end
        end;
        if pos('No such file or directory', List.Text) > 0 then exit(True);     // no ID present, uninitialized ?
        if List.Count > 0 then
            ServerID := copy(List.Strings[List.Count-1], 1, 36);                // Thats, perhaps, a serverID
        if debugmode then debugln('GetServerID [' + ServerID + ']' + List.Text);
    finally
        FreeandNil(List);
        AProcess.Free;
    end;
end;

function TAndSync.TestTransport(): TSyncAvailable;
begin
    Result := SyncReady;
end;

function TAndSync.TestTransportEarly(out ManPrefix : string): TSyncAvailable;
var
    GUID : TGUID;
    T1, T2, T3, T4 : DWord;
begin
  { Here we must -
    ping device, see if its there.
    ssh in, read serverI file, its in <landing place> tomboy.serverid
    makes sure the expected Sync Dir exists
  }
  T1 := GetTickCount64();
  ManPrefix := '';
  ErrorString := '';
  if not Ping(1) then
    if not Ping(2) then
        if not Ping(5) then begin
            debugln('Failed to ping ' + RemoteAddress);
            exit(SyncNetworkError);
        end;
  T2 := GetTickCount64();
  if not SetServerID() then begin
      debugln(ErrorString);
      exit(SyncNetworkError);
  end;
  T3 := GetTickCount64();
  ErrorString := '';
  if not IDLooksOK(ServerID) then begin
        CreateGUID(GUID);
        ServerID := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
        StampServerID(ServerID);
        if DebugMode then debugln('Made a new serverID ' + ServerID );
  end;
  ManPrefix := copy(ServerID, 1, 8);
  Result := SyncReady;
  T4 := GetTickCount64();
  if debugmode then
      debugln('TestTransportEarly ID=' + ServerID + ' ' + inttostr(T2 - T1) + 'mS '
            + inttostr(T3 - T2) + 'mS ' + inttostr(T4 - T3) + 'mS');
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
        StList: TStringList;
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
end;

function TAndSync.DeleteNote(const ID: string; const ExistRev : integer ): boolean;
begin
    // This Sync Engine does not do deletes at either end.
  result := True;
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
    result := True;
end;

function TAndSync.DoRemoteManifest(const RemoteManifest: string): boolean;
begin
    // The Tomdroid sync model does not use a remote manifest.
  result := True;
end;

function TAndSync.UpLoad(const ID : string ) : boolean;
var
    AProcess: TProcess;
    List : TStringList;
begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'sshpass';
    AProcess.Parameters.Add('-p');
    AProcess.Parameters.Add(Password);
    AProcess.Parameters.Add('scp');
    AProcess.Parameters.Add('-P2222');
    AProcess.Parameters.Add(NotesDir + ID + '.note');
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
    List : TStringList;
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
    if not Result then
        ErrorString := 'something bad happened when downloading ' + ID;
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

