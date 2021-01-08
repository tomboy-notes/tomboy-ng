unit transfileand;


{   Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------

  A unit that does the file transfer side of a very limited one to one Tomdroid Sync
  This version uses File sync (with a locally mounted android file system) not SSH.

  HISTORY
    2020/12/29 Forked from transandroid





  Sync.SetTransport -
        Does nothing here.

  Sync.TestTransport    (parent)
        In repoUse mode - Reads Local Manifest (if exists), calls Trans.TestTransport,
        compares localServerID (from config and local manifest).
        If serverID problem, consult user,  rets SyncMismatch
        In RepoNew mode, we ignore any local manifest and both local and remote
        serverIDs. A fresh start.

  Trans.TestTransport (here, for android)
        Looks for the tomdroid dir, if it finds it, fills out RemoteDir
        If not JoinRepo, grabs the devices serverID.
        If JoinRepo, generates a new ServerID and puts it on device.
        Checks for remote (Tomdroid made) directory. We declare the dir a SyncNoRemote
        if there is no remote serverID present. We make one if user clicks 'Join'.
        So, in Join there is no transport.serverid until after TestTransport.

  Note that because of how the file based Tomdroid sync works, we set action to
        either RepoUse or RepoJoin, not RepoNew ! Join here is effectivly a combination
        of Join and New. A Join overwrites an existing ServerID with a new one.

        Tomdroid seems to need to be stopped after each (internal) sync to be sure of
        reliable notice of deleted notes. Otherwise, it sometimes seems to not notice that
        a previously synced note has now dissapeared from its sync dir (as a result of
        -ng syncing there) and therefore does not remove that note from its dbase
        when syncing.

        The "Remote Manifest" does not enter play here, we write one out because thats what Sync does
        but its ignored. Data normally obtained from a remote manifest is read directly
        from the 'remote' (but mounted on a local FS) sync files them selves.
        So, only status file we are interested in

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, process, trans, SyncUtils, LazFileUtils;

type

  { TAndSync }

  { TAndFileSync }

  TAndFileSync = Class(TTomboyTrans)
    private
        OneToOne : string;      // a dir, if present, use it to sync to instead of expected Tomdroid gvfs one
        CheckRemoteDirResult : TSyncAvailable;
				                  { Converts a note from local time plus offset to UTC with zero offset. Gets its
				                  input from std NotesDir and returns a FullFileName to to a temp file that
				                  has been converted. Temp file is overwritten. }
        function ChangeNoteDateUTC(const ID: string): string;
                            // May return SyncNoRemoteDir, SyncReady, SyncNoServerID
                            // Sets the RemoteDir to either the GVFS mountpoint plus phone/tomdroid
                            // or to the TB_ONETOONE env var (if present). Tests for a the dir
                            // and the presence of the serverID.
        function CheckRemoteDir: TSyncAvailable;
        function GetNoteLastChange(const FullFileName: string): string;
        procedure InsertNoteBookTags(const FullSourceFile, FullDestFile, TagString: string);

                            // Attempts to read remote Server ID
                            // May return SyncReady or SyncNoRemoteRepo (if unable to find a remote ServerID)
        function SetServerID(): TSyncAvailable;
                            // Writes a file called tomboy.serverid into remote dir, contains an ID
        function StampServerID(const ID: string): boolean;

                            // Uploads the nominated file to MTPDIR using gio commands.
                            // we may be able to do this directly with GVFS calls one day ....
                            // But if in OneToOne mode, just uses copyFile(
        function UploadFile(FullFileName: string; ID: string=''): boolean;

    public
                { Where the remote filesync repo lives, changes for every connection, set by
                CheckRemoteDir(). Its a mountpoint but not all file functions will work there.
                Might look like /run/user/1000/gvfs/mtp:host=%5Busb%3A001%2C053%5D/Phone/tomdroid/ }
//        RemoteDir : string;                                                           // Should use RemoteAddress ??

                { has something like mtp://%5Busb%3A001,031%5D/Phone/tomdroid/
                  It is set by CheckRemoteDir, use as gio Location, append filename. }
        MTPDir    : string;
        function TestTransport(const WriteNewServerID : boolean = False) : TSyncAvailable; override;
        function SetTransport() : TSyncAvailable; override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
                            // TransFileAnd : just returns a full path to note, '' if not found
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
        constructor Create();
  end;


implementation

uses users,      // for getUserID()
    laz2_DOM, laz2_XMLRead, FileUtil, LazLogger, forms;

{ TAndSync }

constructor TAndFileSync.Create();
begin
    inherited.create;
    DebugMode :=  Application.HasOption('s', 'debug-sync');
    CheckRemoteDirResult := CheckRemoteDir();
end;

function TAndFileSync.StampServerID(const ID : string) : boolean;
// Called by TestTransport if we require a new ServerID.
var
    OutFile: TextFile;
begin
    Result := False;
    if debugmode then debugln('TAndFileSync.StampServerID stamp at ' + RemoteAddress + 'tomboy.serverid');
    AssignFile(OutFile, ConfigDir + 'tomboy.serverid');
    try
        Rewrite(OutFile);
        writeln(OutFile, ID);
        Result := True;
	finally
        CloseFile(OutFile);
	end;
    if Debugmode then debugln('TAndFileSync.StampServerID - Local config is ' + ConfigDir);
    UploadFile(ConfigDir + 'tomboy.serverid');
    deletefile(ConfigDir + 'tomboy.serverid');
end;

function TAndFileSync.UploadFile(FullFileName: string; ID : string = ''): boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
    NewName : string;
begin
    if ID = '' then
        NewName := extractFileName(FullFileName)
    else NewName := ID + '.note';
    if OneToOne <> '' then
        exit(CopyFile(FullFileName, RemoteAddress + NewName));

    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'gio';
    AProcess.Parameters.Add('copy');
    AProcess.Parameters.Add(FullFileName);
    AProcess.Parameters.Add(MTPDir +  NewName);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    //debugln('CL = ' + 'gio ' + 'copy ' + FullFileName + ' ' + MTPDir +  NewName);
    try
        AProcess.Execute;
        Result := (AProcess.ExitStatus = 0);
    except on
        E: EProcess do ErrorString := 'TransFileSync.Upload EProcess Error during upload';
    end;
    if not Result then
        ErrorString := 'TransFileSync.Upload something bad happened when uploading ' + FullFileName;
    if Debugmode and (ErrorString <> '') then debugln('ERROR - ' + ErrorString);
    List := TStringList.Create;
    List.LoadFromStream(AProcess.Output);
    List.Free;
    AProcess.Free;
end;

function TAndFileSync.CheckRemoteDir : TSyncAvailable;
var
    Info : TSearchRec;
    InFile: TextFile;
begin
    // Hmm, GetEnvironmentVariable('UID') fails ? No idea ....
    OneToOne := GetEnvironmentVariable('TB_ONETOONE');
    if OneToOne = '' then
        RemoteAddress := '/run/user/' + GetUserId(GetEnvironmentVariable('USER')).ToString() + '/gvfs/'     // thats just the start.....
    else RemoteAddress := appendPathDelim(OneToOne);
    ServerID := '';
    MtpDir := '';
    if DirectoryExistsUTF8(RemoteAddress) then begin
        if OneToOne = '' then begin
        	if FindFirst(RemoteAddress + 'mtp*', faDirectory, Info)=0 then begin
        		repeat
                      if DirectoryExistsUTF8(RemoteAddress + Info.Name + '/Phone/tomdroid') then begin
                          MTPDir := Info.Name + '/Phone/tomdroid' + PathDelim;
                          RemoteAddress := RemoteAddress + MTPDir;
                          MtpDir := MTPDir.Remove(0, 9);
                          MTPDir := 'mtp://' + MTPDir;
                          break;
		    		  end;
                until FindNext(Info) <> 0;
    	    end;
            FindClose(Info);
            // if MTPDir = '' then RemoteDir := '';
        end;
	end else MTPDIR := 'OneToOne FAIL';           // thats really for OneToOne mode only.
    // MTRDir = '' if we failed to set Tomdroid USB dir.  RemoteDir will always show what we
    // tried. If we failed to find the OneToOne dir, MTPDIR will contain 'OneToOne FAIL'
    if DebugMode then begin
        DebugLn('TAndFileSync.CheckRemoteDir RemoteDir = ' + RemoteAddress);
        Debugln('TAndFileSync.CheckRemoteDir MTPDir = '    + MtpDir);
	end;
    if (MTPDir = '') or (MTPDIR = 'OneToOne FAIL') then
        Result := SyncNoRemoteDir
    else begin
        // If to here, we know we have a real dir in RemoteDir, maybe we should also test for writing ?
        if FileExistsUTF8(RemoteAddress + 'tomboy.serverid') then begin
            AssignFile(InFile, RemoteAddress + 'tomboy.serverid');
            try
        	    Reset(InFile);
                readln(InFile, ServerID);
        	finally
                close(InFile);
        	end;
            if not IDLooksOK(ServerID) then begin
                Debugln('SetServerID unable to read tomboy.serverid, we got [' + ServerID + ']');
                exit(SyncNoRemoteRepo);     // No really NoRemoteRepo but a currupted or empty ID ?
            end;
            Result := SyncReady;
		end else Result := SyncNoRemoteRepo;
	end;
    if DebugMode then
        DebugLn('TAndFileSync.CheckRemoteDir ServerID = ' + ServerID);
end;
{
   mtp://%5Busb%3A001,031%5D/      // what gio expects, comma does not matter ?
mtp:host=%5Busb%3A001%2C031%5D/    // the mount point
}

function TAndFileSync.SetServerID() : TSyncAvailable;
var
    InFile: TextFile;                                       // ToDo : we have already read the serverid in SetTransport ...
begin
    if not FileExistsUTF8(RemoteAddress + 'tomboy.serverid') then exit(SyncNoRemoteRepo);
    ServerID := '';
    AssignFile(InFile, RemoteAddress + 'tomboy.serverid');
    try
	    Reset(InFile);
        readln(InFile, ServerID);
	finally
        close(InFile);
	end;
    if not IDLooksOK(ServerID) then begin
        Debugln('SetServerID unable to read tomboy.serverid, we got [' + ServerID + ']');
        exit(SyncNoRemoteRepo);     // No really NoRemoteRepo but a currupted or empty ID ?
    end;
    Result := SyncReady;
end;

function TAndFileSync.TestTransport(const WriteNewServerID : boolean = False): TSyncAvailable;
var
    GUID : TGUID;
    //T1, T2, T3, T4 : DWord;
begin
    { We have already checked the sync dir and established if, or if not we have a serverid there.
      So, it seems all we do here is make a new server id if necessary (ie if WriteNewServerID is true
      or ANewrepo is true).
    }
    if RemoteAddress = '' then exit(SyncNoRemoteRepo);
    ErrorString := '';
    if  WriteNewServerID and ANewRepo then begin
        CreateGUID(GUID);
        ServerID := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
        StampServerID(ServerID);
	end;
    Result := SyncReady;
end;


function TAndFileSync.SetTransport(): TSyncAvailable;
begin
    Result := CheckRemoteDirResult;         // CheckRemoteDir is called in Create, but later we need to know result.
                                            // serverID and RemoteDir will have been set if possible
end;

function TAndFileSync.GetNewNotes(const NoteMeta: TNoteInfoList; const GetLCD : boolean): boolean;
var
        NoteInfo : PNoteInfo;
        Info : TSearchRec;
        St : string;
begin
    // by nature of how we get remote note date, always get LCD
    if NoteMeta = Nil then begin
        ErrorString := 'Passed an uncreated list to GetNewNotes()';
        exit(False);
    end;
    if FindFirst(RemoteAddress + '*.note', faAnyFile, Info)=0 then begin
        repeat
            St := GetNoteLastChangeSt(RemoteAddress + Info.Name, ErrorString);
            if St = '' then
                debugln('ERROR, TransFileAnd.GetDroidMetaData failed to find LCD in ' + RemoteAddress + Info.Name)
            else begin
                new(NoteInfo);
                NoteInfo^.Action:=SyUnset;
                NoteInfo^.ID := extractFileNameOnly(info.Name);
                NoteInfo^.Rev := -1;
                NoteInfo^.LastChange := St;
                NoteInfo^.LastChangeGMT := GetGMTFromStr(St);
                NoteMeta.Add(NoteInfo);
            end;
        until FindNext(Info) <> 0;
    end;
    result := True;
end;


function TAndFileSync.GetNoteLastChange(const FullFileName : string) : string;
begin
    Result := GetNoteLastChangeSt(FullFileName, ErrorString);   // syncutils function
end;

            // Puts back the tag string into a temp note downloaded from dev and puts it in note dir, overwrites
procedure TAndFileSync.InsertNoteBookTags(const FullSourceFile, FullDestFile, TagString : string);
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
                    writeln(OutFile, TagString);
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

function TAndFileSync.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
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
            copyfile(RemoteAddress + Downloads.Items[I]^.ID + '.note', self.NotesDir + Downloads.Items[I]^.ID + '.note', false);
        end;
    end;
    result := True;
end;


function TAndFileSync.DeleteNote(const ID: string; const ExistRev : integer ): boolean;
begin
    // MTP: seems to allow us to delete notes from there.
    if FileExistsUTF8(RemoteAddress + ID + '.note') then begin
        DeleteFileUTF8(RemoteAddress + ID + '.note')
	end	else begin
        debugln('ERROR TransFileAnd.DeleteNote cannot find note to delete ' + RemoteAddress + ID + '.note');
        exit(False);
	end;
    if FileExistsUTF8(RemoteAddress + ID + '.note') then begin
        debugln('ERROR TransFileAnd.DeleteNote Failed to delete note ' + RemoteAddress + ID + '.note');
        exit(False);
	end;
    result := False;
end;

function TAndFileSync.UploadNotes(const Uploads: TStringList): boolean;
var
    Index : integer;
begin
    // OK, seems Tomdroid likes its date strings in UTC with zero offset, (not 'zulu' time)
    // messes with sync.  So, before uploading a file, we'll
    // stuff about with its date strings. ChangeNoteDateUTC() makes a temp, edited copy and ret its full path.
    for Index := 0 to Uploads.Count -1 do begin
        if DebugMode then debugln('TransFileAnd.UploadNotes ' + Uploads.Strings[Index] + '.note');
        if FileExistsUTF8(RemoteAddress + Uploads.Strings[Index] + '.note') then
            DeleteFileUTF8(RemoteAddress + Uploads.Strings[Index] + '.note');
        if FileExistsUTF8(NotesDir + Uploads.Strings[Index] + '.note') then
            UploadFile(ChangeNoteDateUTC(Uploads.Strings[Index]), Uploads.Strings[Index])
        else  debugln('ERROR TransFileAnd.UploadNotes Failed to find ' + NotesDir + Uploads.Strings[Index] + '.note');
    end;
    result := True;    // unless, of course, we failed some how. Hmm...
end;

function TAndFileSync.DoRemoteManifest(const RemoteManifest: string): boolean;
begin
    // The Tomdroid sync model does not use a remote manifest.
  result := True;
end;

(*
{ Takes the FFN of a note (probably on a remote mounted MTP dir) and converts
  its date from UTC with zero offset to Local Time with an offset. Returns
  a FFN to the temp file that will be overwritten.  }

function TAndFileSync.ChangeNoteDateFromUTC(FFN : string) : string;
var
    InFile, OutFile: TextFile;
    NoteDateSt, InString : string;
begin

end;
*)

function TAndFileSync.ChangeNoteDateUTC(const ID : string) : string;
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

function TAndFileSync.DownLoadNote(const ID: string; const RevNo: Integer): string;
var
    St : string;
begin
    St := RemoteAddress + ID + '.note';
    if FileExistsUTF8(St) then exit(St);
    debugln('TransFileAnd.DownloadNote failed to find ' + St);
    Result := '';
end;



end.

