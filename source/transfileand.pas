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
    2020/01/13 Ready for testing, aware of two issues -
        1. An error in older version of Android MTP, a deleted note can appear to remain
           in the sync dir but be unreadable until phone is rebooted. Msg appears std out.
           Seen often in Android 5.1.1, not at all in 8.0.0
        2. When a note is deleted on one Android device, it will be restored by another.
           I need to add an entry in each current Android Local Manifest noting that it
           is a deleted file.
        3. All this and related code does not work in Windows but its it is exposed to
	   the compiler. Some ifdef's are in order here ...
    2021/01/17 Added a method to convert the HTML Entities that Tomdroid sends us back to UTF8
    2021/01/19 Restructed layout to allow ifdef to compile stubs when not Linux




  The Android File Sync Process (and the OneToOne one too)

  Create()
        Calls CheckRemoteDir that does much of the initial testing
        including setting RemoteAddress, testing for a server ID.
        CheckRemoteDir expects to find a gio 'Location' that it can use to build
        a 'path' to the tomboy-ng.serverid file.
        Alternativly, it looks in the TB_ONETOONE  env var.

  Sync.SetTransport -
        Just returns the Sync Status already established by Create.

  Sync.TestTransport    (parent)
        In repoUse mode - Reads Local Manifest (if exists), calls Trans.TestTransport,
        compares localServerID (from config and local manifest).
        If serverID problem, consult user,  rets SyncMismatch
        In RepoNew mode, we ignore any local manifest and both local and remote
        serverIDs. A fresh start.

  Trans.TestTransport (here, for android file)
        Just writes a server ID if necessary.

  StartSync   (A Sync Method, not here)
        It calls functions that build both meta data lists, acts on what it finds.




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


        It would seem worth considering converting this to use libmtp, (same project, one echo of other)
        https://github.com/libmtp/libmtp
        https://sourceforge.net/projects/libmtp/
        No formal docs but pretty clear examples in C available.
        Possibly buildable for windows but looks difficult, both building and installing for end user.

}


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, process, trans, SyncUtils, LazFileUtils;

type

  { TAndSync }

  { TAndFileTrans }

  TAndFileTrans = Class(TTomboyTrans)
    private
        OneToOne : string;      // a dir, if present, use it to sync to instead of expected Tomdroid gvfs one
        CheckRemoteDirResult : TSyncAvailable;

                            // Searches for any UTF8 char (more than 1 char long) and replaces it with the
				            // appropriate HTML Entity. Use on a note being sent to Tomdroid. eg Δ becomes &#394;
				            // NO - it turns out Tomdroid does not like us doing this so we'll send it UTF8
		                    // and let Tomdroid deal with it itself. So far, seems OK ...........
		procedure AddHTMLNumericCode(var St: string);
                            // Converts a note from local time plus offset to UTC with zero offset before
                            // sending it to Tomdroid. Gets its input from std NotesDir and returns a
                            // FullFileName to to a temp file that has been converted. Temp file is overwritten.
        function ChangeNoteDateUTC(const ID: string; AlsoEncodeEntity: boolean): string;
                            // May return SyncNoRemoteDir, SyncReady, SyncNoServerID
                            // Sets the RemoteDir to either the GVFS mountpoint plus phone/tomdroid
                            // or to the TB_ONETOONE env var (if present). Tests for a the dir
                            // and the presence of the serverID.
        function CheckRemoteDir: TSyncAvailable;
                            // Copies any file from the mtp: location to ConfigDir + remote.note.
                            // It does not, however, need to be a note. Gets overwritten all the time.
		function OS_DownLoadFile(FN: string): boolean;
                            { Used in Tomdroid but not OneToOne mode. We rely on gio to get us a list of notes
                            present in remoote dir. I experienced erratic behaviour when doing a findfirst(). }
		function GetNewNotesGIO(const NoteMeta: TNoteInfoList; const GetLCD: boolean) : boolean;
                            // Will use RemoteAddress and append the FName to it. Will check its gone.
                            // Cannot remove directories, ret false. OneToOne safe.
		function OS_DeleteFile(const FName: string): boolean;
                            // Returns True if passed file (a full filename without path) exists
                            // in the remote directory. If isDirectory, will test a compounded dir
                            // but it must not have a leading seperator. It is OneToOne Safe.   }
		function OS_FileExists(aName: string; isDirectory: boolean=false): boolean;
                            // Assumes RemoteAddress now holds the basic gio Location,
                            // just the first part. It will try and find a dir below
                            // that that itself, contains 'tomdroid'. False if fail.
		function FindTomdroidDirectory(): boolean;
                            // Fills out the passed stringlist (that must have been created) with
                            // files and directories found in RemoteDir + aDir. Does not test that it has
                            // found anything, just that the process completed.  Is NOT OneToOne safe.
		function OS_ListDirectory(var List: TstringList; const aDir: string=''): boolean;
		                    // Attempts to find the gio location, searches for exactly one 'mtp://'
		                    // entry in gio mount -l output. Sets RemoteAddress to only that first term,
                            // eg mtp://%5Busb%3A001,119%5D/  or mtp://Android_Android_ba0da805/ or
                            // mtp://SAMSUNG_SAMSUNG_Android_52004dfb47a785e5/ always with trailing delim.
		function OS_Location(): boolean;

        procedure InsertNoteBookTags(const FullSourceFile, FullDestFile, TagString: string);
                            // Looks for a tomboy.serverid file on remote FS, reads if found and makes sure
                            // it is a GUID (but does not check that it is the right one). We copy it down
                            // and read it locally as older Android's mtp seems unwilling to let me read it
                            // in place. In OneToOne, we just read it. Implies that OneToOne is not mtp: !
		function ReadServerID(): boolean;
                            // Looks for any HTML Entities in passed string, changes then to UTF8 codepoint.
                            // eg &#911; becomes the Omega char. Used when bringing a note back from Tomdroid
		function RemoveHTMLNumericCode(var St: string): boolean;
                            // Writes a file called tomboy.serverid into remote dir, contains an ID
        function StampServerID(const ID: string): boolean;
                            // Uploads the nominated file to MTPDIR using gio commands.
                            // we may be able to do this directly with GVFS calls one day ....
                            // But if in OneToOne mode, just uses copyFile(
        function OS_UploadFile(FullFileName: string; ID: string=''): boolean;

    public
                { has something like mtp://%5Busb%3A001,031%5D/Phone/tomdroid/
                  It is set by CheckRemoteDir, use as gio Location, append filename. }
//        RemoteAddress    : string;

                            // TAndFileTrans : will stamp a new serverid (if doing a join).
        function TestTransport(const WriteNewServerID : boolean = False) : TSyncAvailable; override;
                            // In transFileAnd, just returns the already established status.
        function SetTransport() : TSyncAvailable; override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;
                            // TomdroidFile : we pull a file down locally to 'remote.note' but we could
                            // speed things up a bit by making it go straight to destination. But what
                            // about putting tags back into a note before overwriting orig ?
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
                            // TAndFileTrans : does nothing, Tomdroid model does not use remote manifest.
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
                            // TransFileAnd : Pulls remote file down to 'remote.note and ret a full path to note,
                            // '' if it was not found. OneOnOne friendly.
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
                            // Calls CheckRemoteDir that does much of the initial testing
                            // including setting RemoteAddress, testing for a server ID.
        constructor Create();
  end;


implementation

uses {users, }     // for getUserID()
    laz2_DOM, laz2_XMLRead, FileUtil, LazLogger, forms, LazUTF8;

{ TAndSync }

// ========================  Public Methods =============================

constructor TAndFileTrans.Create();
begin
    inherited.create;
    DebugMode :=  Application.HasOption('s', 'debug-sync');
    CheckRemoteDirResult := CheckRemoteDir();
end;

function TAndFileTrans.TestTransport(const WriteNewServerID : boolean = False): TSyncAvailable;
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
    // ToDo : should call readServerID() here to be sure .....
    Result := SyncReady;
end;

function TAndFileTrans.SetTransport(): TSyncAvailable;
begin
    Result := CheckRemoteDirResult;         // CheckRemoteDir is called in Create, but later we need to know result.
                                            // serverID and RemoteDir will have been set if possible
end;

function TAndFileTrans.GetNewNotes(const NoteMeta: TNoteInfoList; const GetLCD : boolean): boolean;
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
    if OneToOne = '' then exit( GetNewNotesGIO(NoteMeta, GetLCD));              // ie Tomdroid mode

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

function TAndFileTrans.DeleteNote(const ID: string; const ExistRev : integer ): boolean;
begin
    // MTP: seems to allow us to delete notes from there.    But should we do it via GIO ???
    //if FileExistsUTF8(RemoteAddress + ID + '.note') then begin
    if OS_FileExists(ID + '.note') then begin
        OS_DeleteFile(ID + '.note')
        // DeleteFileUTF8(RemoteAddress + ID + '.note')
	end	else begin
        debugln('ERROR TransFileAnd.DeleteNote cannot find note to delete ' + RemoteAddress + ID + '.note');
        exit(False);
	end;
    //if FileExistsUTF8(RemoteAddress + ID + '.note') then begin
    if OS_FileExists(ID + '.note') then begin
        debugln('ERROR TransFileAnd.DeleteNote Failed to delete note ' + RemoteAddress + ID + '.note');
        exit(False);
	end else if debugmode then debugln('========= Deleted ' + RemoteAddress + ID + '.note');
    result := True;
end;

function TAndFileTrans.UploadNotes(const Uploads: TStringList): boolean;
var
    Index : integer;
begin
    // OK, seems Tomdroid likes its date strings in UTC with zero offset, (not 'zulu' time)
    // messes with sync.  So, before uploading a file, we'll
    // stuff about with its date strings. ChangeNoteDateUTC() makes a temp, edited copy and ret its full path.
    for Index := 0 to Uploads.Count -1 do begin
        if DebugMode then debugln('TransFileAnd.UploadNotes ' + Uploads.Strings[Index] + '.note');
        if OS_FileExists(Uploads.Strings[Index] + '.note') then
            OS_DeleteFile(Uploads.Strings[Index] + '.note');
        if FileExistsUTF8(NotesDir + Uploads.Strings[Index] + '.note') then
            OS_UploadFile(ChangeNoteDateUTC(Uploads.Strings[Index], True), Uploads.Strings[Index])
        else  debugln('ERROR TransFileAnd.UploadNotes Failed to find ' + NotesDir + Uploads.Strings[Index] + '.note');
    end;
    result := True;    // unless, of course, we failed some how. Hmm...
end;

function TAndFileTrans.DoRemoteManifest(const RemoteManifest: string): boolean;
begin
    // The Tomdroid sync model does not use a remote manifest.
  result := True;
end;


function TAndFileTrans.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
var
    I : integer;
    TempName : string;
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
            // copyfile(RemoteAddress + Downloads.Items[I]^.ID + '.note', self.NotesDir + Downloads.Items[I]^.ID + '.note', false);
            //copyFile(DownloadNote(Downloads.Items[I]^.ID + '.note', -1), NotesDir + Downloads.Items[I]^.ID + '.note');
            TempName := DownloadNote(Downloads.Items[I]^.ID + '.note', -1);
            copyfile(TempName, NotesDir + Downloads.Items[I]^.ID + '.note');
            if not FileExists(NotesDir + Downloads.Items[I]^.ID + '.note') then
                debugln('ERROR, did not download ' + Downloads.Items[I]^.ID);
        end;
    end;
    result := True;
end;


function TAndFileTrans.DownLoadNote(const ID: string; const RevNo: Integer): string;
var
    St : string;
    //AProcess: TProcess;
    //List : TStringList = nil;
    Extension : string = '';
    InFile, OutFile: TextFile;
    InString : string;
begin
    //debugln('===================== Called TAndFileSync.DownLoadNote( ========');
    if not ID.EndsWith('.note') then Extension := '.note';
    St := RemoteAddress + ID + Extension;
    if OneToOne = '' then
        if OS_DownLoadFile(ID + Extension) then
            St := ConfigDir + 'remote.note'
        else St := '';
    if FileExistsUTF8(St) then begin // St, by this stage, should always be a local note, so no GioFileExists()
        AssignFile(InFile, St);
        AssignFile(OutFile, St + '-decoded');
        try
            try
                Reset(InFile);
                Rewrite(OutFile);
                while not eof(InFile) do begin
                    readln(InFile, InString);
                    while RemoveHTMLNumericCode(InString) do;   // eg &#911; becomes omega
                    writeln(OutFile, InString);
				end;
			finally
                CloseFile(OutFile);
                CloseFile(InFile);
			end;
        except
            on E: EInOutError do begin
                debugln('ERROR TAndFileTrans.DownLoadNote - File handling error : ' + E.Message);
                ErrorString := 'ERROR TAndFileTrans.DownLoadNote - File handling error : ' + E.Message;
                exit('');
			end;
		end;
        DeleteFile(St);
        CopyFile(St + '-decoded', St);
        exit(St);
    end;
    debugln('TransFileAnd.DownloadNote failed to find ' + St);
    Result := '';
end;


// ======================= Private Methods that are OS Agnostic ================


function TAndFileTrans.FindTomdroidDirectory() : boolean;
var
    List : TStringList;
    St : string;
begin
    List := TStringList.Create;
    try
	    if OS_ListDirectory(List, '') then begin            // Remember, at this stage, RemoteAddress has just basic, first part.
            for St in List do
                if St <> '' then
                    if OS_FileExists(appendPathDelim(St) + 'tomdroid', True) then begin
                        RemoteAddress := RemoteAddress + appendPathDelim(St) + 'tomdroid' + PathDelim;
                        exit(True);
				    end else debugln('Tried ' + RemoteAddress + appendPathDelim(St) + 'tomdroid');
        end else debugln('TAndFileSync.GioFindDirectory - ERROR, GioListDirectory returned false.');
	finally
        List.free;
	end;
    debugln('TAndFileSync.GioFindDirectory - ERROR, could not find ' + RemoteAddress + '*/tomdroid' );
    debugln('Please install Tomdroid and set it to sync to a "SD Card" directory called "tomdroid"');
    result := False;
end;


function TAndFileTrans.StampServerID(const ID : string) : boolean;
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
    OS_UploadFile(ConfigDir + 'tomboy.serverid');
    deletefile(ConfigDir + 'tomboy.serverid');
end;

function TAndFileTrans.ReadServerID() : boolean;
var
    FFN : string;
    //AProcess: TProcess;
    //List : TStringList = nil;
    InFile: TextFile;
begin
    ServerID := '';
    if OneToOne = '' then begin                                        // ie Tomdroid mode
        if not OS_DownLoadFile('tomboy.serverid') then exit(false);
        FFN := Configdir + 'remote.note';       // I know ! Its not a note, sorry !
    end else
        FFN := RemoteAddress + 'tomboy.serverid';                      // As usual, if its OneToOne we assume not mtp:
        AssignFile(InFile, FFN);
        try
        	    Reset(InFile);
                readln(InFile, ServerID);                              // This causes a disk full error on xpedia  ???
        finally
                close(InFile);
        end;
    if not IDLooksOK(ServerID) then begin
        Debugln('TAndFileSync.ReadServerID unable to read tomboy.serverid, we got [' + ServerID + ']');
        exit(False);
    end;
    Result := True;
end;

function TAndFileTrans.CheckRemoteDir : TSyncAvailable;
begin
    // Assume : the user will create a dir called 'tomdroid'
    // Assume : we don't know the remainder of the token mtp:.....
    // Assume : we don't know the top level name presented by the device, 'phone', Internal Storage', 'tablet' ....
    // Assume : the above may be in spanish. So, unicode, spaces etc

    // Hmm, GetEnvironmentVariable('UID') fails ? No idea ....
    OneToOne := GetEnvironmentVariable('TB_ONETOONE');
    if OneToOne = '' then begin                                                     // some duplicate code here but keep it simple ....
        OS_Location();                                         // Sets basic Location and Devices top level dir.
        //debugln('MTPDir after GioLocation() ' + RemoteAddress );
        if RemoteAddress = '' then begin
            debugln('TAndFileSync.CheckRemoteDir ERROR - Have you connected Device to Computer ?');
            exit(SyncNoRemoteDir);
        end;
        if not FindTomdroidDirectory() then
            exit(SyncNoRemoteDir);                                              // else RemoteAddress now points to valid tomdroid dir.
     end else begin                                                             // OK, its OneToOne mode then !
        RemoteAddress := appendPathDelim(OneToOne);
        if not DirectoryExistsUTF8(RemoteAddress) then begin
            debugln('Failed to find OnetoOne Dir : ' + RemoteAddress);
            exit(SyncNoRemoteDir);
		end;
    end;
    // OK, if to here we have a dir that we can work in, we should test that we can write to it.
    // If to here, we know we have a real dir in RemoteDir, maybe we should also test for writing ?
    Result := SyncNoRemoteRepo;
    if OS_FileExists('tomboy.serverid') then
        if ReadServerID() then
            Result := SyncReady;
    if Debugmode then begin
        debugln('TAndFileSync.CheckRemoteDir RemoteAddress = ' + RemoteAddress);
        debugln('TAndFileSync.CheckRemoteDir the  ServerID = ' + ServerID);
	end;
end;

// Puts back the tag string into a temp note downloaded from dev and puts it in note dir, overwrites
procedure TAndFileTrans.InsertNoteBookTags(const FullSourceFile, FullDestFile, TagString : string);       // ToDo : can we make this work ?
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




procedure TAndFileTrans.AddHTMLNumericCode(var St : string);
// Seems we are not using this method.
var
    Len, Blar: Integer;
    ACodePoint: String = '';
    AUnicode: Cardinal;
    Index : integer = 0;
begin
    if St = '' then exit;
    repeat
        Len := UTF8CodepointSize(PChar(St) + Index);
        if Len = 1 then begin
            inc(Index);
            continue;                                   // its only 1 byte, keep looking ...
        end;
        ACodePoint := copy(St, Index+1, Len);           // Index is zero based
        AUnicode:=UTF8CodepointToUnicode(pchar(ACodePoint), Blar);
        ACodePoint := '&#' + AUnicode.ToString() + ';';
        delete(St, Index+1, Len);                       // delete the utf8
        insert(ACodePoint, St, Index+1);
        inc(Index, ACodePoint.length);                  // jump past new chars
	until Index >= St.Length;
end;


function TAndFileTrans.ChangeNoteDateUTC(const ID : string; AlsoEncodeEntity : boolean) : string;
var
    InFile, OutFile: TextFile;
    NoteDateSt, InString : string;
//    FirstLine : boolean = True;
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
//                if (not FirstLine) and AlsoEncodeEntity then
//                    AddHTMLNumericCode(InString)                       // eg Δ becomes &#394;
//                else FirstLine := False;
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


function TAndFileTrans.RemoveHTMLNumericCode(var St : string) : boolean;
var
    Target : integer = 1;
    Buff : string = '';
begin
    result := false;
    repeat
        Target := Pos('&#', St, Target);
        if Target = 0 then exit(False);                 // None left, lets get out of here.
        if Target + 3 > St.length then exit(false);     // No room ....
        inc(Target, 2);
        if (St[Target] in ['0'..'9']) then begin        // Looks like we have one !
            while St[Target] in ['0'..'9'] do begin
                Buff := Buff + St[Target];
                delete(St, Target, 1);
                if Target > St.Length then break;
            end;
            if St[Target] = ';' then delete(St, Target, 1);
            Dec(Target, 2);                             // Back to start of Entity
            delete(St, Target, 2);
            insert(UnicodeToUTF8(Buff.ToInteger), St, Target);  // In LazUTF8
            exit(True);
		end;                                            // Oh, well, how sad, try again ?
 	until Target > St.length;
end;

function TAndFileTrans.GetNewNotesGIO(const NoteMeta: TNoteInfoList; const GetLCD : boolean): boolean;
var
        NoteInfo : PNoteInfo;
        //Info : TSearchRec;
        St, LocalTempNote, DateSt : string;
//        AProcess: TProcess;
        List : TStringList = nil;
begin
    // by nature of how we get remote note date, always get LCD
    if NoteMeta = Nil then begin
        ErrorString := 'Passed an uncreated list to GetNewNotes()';
        exit(False);
    end;

     (*
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'gio';
    AProcess.Parameters.Add('list');
    AProcess.Parameters.Add(RemoteAddress);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        AProcess.Execute;
        Result := (AProcess.ExitStatus = 0);
    except on
        E: EProcess do ErrorString := 'TransFileSync.GetNewNotes EProcess Error during LIST';
    end;
    if not Result then
        ErrorString := 'TransFileSync.GetNewNotes something bad happened when LISTing ' + RemoteAddress;
    if Debugmode and (ErrorString <> '') then debugln('ERROR - ' + ErrorString);
    List := TStringList.Create;
    List.LoadFromStream(AProcess.Output);
    *)

    List := TStringList.Create;
    if not OS_ListDirectory(List, '') then begin
        ErrorString := 'TransFileSync.GetNewNotesGIO Error during LIST ' + RemoteAddress;
        debugln(ErrorString);
        List.free;
        exit(false);
	end;

    //debugln('========= Processing remote files ===========');
    for St in List do
            if (St <> '') and St.EndsWith('.note') then begin
                LocalTempNote := DownLoadNote(extractFileNameOnly(St), -1);
                DateSt := GetNoteLastChangeSt(LocalTempNote, ErrorString);
                if DateSt <> '' then begin
                    new(NoteInfo);
                    NoteInfo^.Action:=SyUnset;
                    NoteInfo^.ID := extractFileNameOnly(St);
                    NoteInfo^.Rev := -1;
                    NoteInfo^.LastChange := St;
                    NoteInfo^.LastChangeGMT := GetGMTFromStr(DateSt);
                    NoteMeta.Add(NoteInfo);
                end else
                    debugln('ERROR, TransFileAnd.GetNewNotesGIO failed to find LCD in ' + St + ' - ' + LocalTempNote);
		    end;
    List.Free;
//    AProcess.Free;
    result := true;
end;


// ==================== Private Methods that are OS Specific ================
// Move into an inc file at some stage

{$ifdef LINUX }

function TAndFileTrans.OS_UploadFile(FullFileName: string; ID : string = ''): boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
    NewName : string;
begin
    if ID = '' then
        NewName := extractFileName(FullFileName)
    else NewName := ID + '.note';
    if OneToOne <> '' then                                                     // ie OneToOne mode
        exit(CopyFile(FullFileName, RemoteAddress + NewName));

    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'gio';
    AProcess.Parameters.Add('copy');
    AProcess.Parameters.Add(FullFileName);
    AProcess.Parameters.Add(RemoteAddress +  NewName);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    //debugln('CL = ' + 'gio ' + 'copy ' + FullFileName + ' ' + RemoteAddress +  NewName);
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


function TAndFileTrans.OS_ListDirectory(var List : TstringList; const aDir : string = '') : boolean;
var
    AProcess: TProcess;
begin
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'gio';
    AProcess.Parameters.Add('list');
    AProcess.Parameters.Add(RemoteAddress + aDir);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        try
            AProcess.Execute;
            if (AProcess.ExitStatus <> 0) then begin
                debugln('TAndFileSync.GioListDirectory ERROR, could not list  ' + RemoteAddress + aDir);
		    end;
            //List := TStringList.Create;
            List.LoadFromStream(AProcess.Output);
            // debugln('TAndFileSync.GioListDirectory looking at ' +  RemoteAddress + aDir);
            if List.Count < 1 then begin
                debugln('TAndFileSync.GioListDirectory built an empty list when looking at ' +  RemoteAddress + aDir);
                exit(false);
			end; // else debugln('TAndFileSync.GioListDirectory List = [' + List.Text + ']');
	    except on
            E: EProcess do begin
                debugln('TransFileSync.GetNewNotes EProcess Error during LIST ' + RemoteAddress + aDir);
                exit(false);
			end;
		end;
    finally
        Aprocess.Free;
    end;
    result := True;
end;


function TAndFileTrans.OS_DownLoadFile(FN : string): boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    //if not FileExistsUTF8(RemoteAddress + FN) then exit(false);
    if not OS_FileExists(FN) then exit(false);

    DeleteFileUTF8(ConfigDir + 'remote.note');
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'gio';
    AProcess.Parameters.Add('copy');
    AProcess.Parameters.Add(RemoteAddress + FN);
    AProcess.Parameters.Add(ConfigDir + 'remote.note');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        try
            AProcess.Execute;
            if AProcess.ExitStatus <> 0 then begin
                ErrorString := 'TransFileSync.DownLoadFile something bad happened when LISTing ' + RemoteAddress;
                debugln('--------------------------------------------------------------------------');
                debugln('TAndFileSync.DownLoadFile WARNING failed to download to temp file');
                debugln('gio copy ' + RemoteAddress + FN + ' ' + ConfigDir + 'remote.note');
                debugln('A problem with mtp in your phone can happen when you delete a note in Tomdroid');
                debugln('and its sync process fails to remove the file from the mtp view of the sync');
                debugln('dir. While it does no harm, a phone reboot will suppress this message.');
                debugln('-------------------------------------------------------------------------');
                exit(False);
			end;
		except on
            E: EProcess do ErrorString := 'TransFileSync.DownLoadSpecificNote EProcess Error during LIST';
        end;
        List := TStringList.Create;
        List.LoadFromStream(AProcess.Output);
	finally
        if List <> nil then List.free;
        AProcess.Free;
	end;
    Result := true;
end;


function TAndFileTrans.OS_DeleteFile(const FName : string) : boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    if OneToOne <> '' then
        exit(DeleteFileUTF8(RemoteAddress + FName));

    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'gio';
    AProcess.Parameters.Add('remove');
    AProcess.Parameters.Add(RemoteAddress + FName);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        try
            AProcess.Execute;
            List := TStringList.Create;                 // Hmm, can I do this if we had an error ?
            List.LoadFromStream(AProcess.Output);
            if (AProcess.ExitStatus <> 0) then begin        // That is, the was not there to start with ?
                debugln('TransFileSync.OS_DeleteFile ERROR could not delete ' + RemoteAddress + FName);
                exit(False);
			end;
        except on
            E: EProcess do begin
                debugln('TransFileSync.GioFileExits EXCEPTION when asking about ' + RemoteAddress + FName);
                exit(False);
                end;
		end;
	finally
        if List <> nil then List.free;
        AProcess.Free;
	end;
    Result := not OS_FileExists(FName);
end;

const MTPSTART='mtp://';

function TAndFileTrans.OS_Location() : boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
    St : string = '';
begin
    RemoteAddress := '';
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'gio';
    AProcess.Parameters.Add('mount');
    AProcess.Parameters.Add('-l');
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        try
            AProcess.Execute;
            List := TStringList.Create;
            List.LoadFromStream(AProcess.Output);
            if (AProcess.ExitStatus <> 0) or (List.Count < 1) then begin
                debugln('TransFileSync.GioLocation ERROR ');                    // That is ??
                exit(False);
			end;
            for St in List do begin
                if pos(MTPSTART, St) > 0 then begin
                    RemoteAddress := appendPathDelim(copy(St, Pos(MTPSTART, St), 1000));
                    break;
                end;

            end;
        except on
            E: EProcess do begin
                debugln('TransFileSync.GioLocation EXCEPTION');
                exit(False);
                end;
		end;
	finally
        if List <> nil then List.free;
        AProcess.Free;
	end;
    Result := length(RemoteAddress) > length(MTPSTART);
    if Result then
        RemoteAddress := appendPathDelim(RemoteAddress)
    else
        RemoteAddress := '';
end;

function TAndFileTrans.OS_FileExists(aName : string; isDirectory : boolean = false) : boolean;
// aName (directory) could be like Phone, Phone/tomdroid, Phone/tomdroid/, tomdroid, tomdroid/ ......
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    if OneToOne <> '' then
        if isDirectory then exit(DirectoryExistsUTF8(RemoteAddress + aName))
        else exit(FileExistsUTF8(RemoteAddress + aName));

    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'gio';
    AProcess.Parameters.Add('info');
    AProcess.Parameters.Add(RemoteAddress + aName);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        try
            AProcess.Execute;
            List := TStringList.Create;                 // Hmm, can I do this if we had an error ?
            List.LoadFromStream(AProcess.Output);
            if (AProcess.ExitStatus <> 0) or (List.Count < 4) then begin    // That is, the file does not exist.
                // debugln('TransFileSync.GioFileExits ERROR when asking about ' + RemoteAddress + AName);
                exit(False);
			end;
            if IsDirectory and (aName <> '') then begin                     // better check if its compounded
                if aName[aName.Length] = PathDelim then                     // trailing / has to go.
                    aName := aName.Remove(aName.Length-1);
				while pos('/', aName) > 0 do
                    aName := aName.Remove(0, pos('/', aName));              // Any leading terms also have to go
			end;
            if not List[0].EndsWith(aName) then exit(false);                // if its last text in line, then file not found follows
            if isDirectory then begin
                if List[2].endswith('directory') then exit(true);           // its a directory
			end else if List[2].endswith('regular') then exit(true);        // its a regular file.
        except on
            E: EProcess do begin
                debugln('TransFileSync.GioFileExits EXCEPTION when asking about ' + RemoteAddress + AName);
                exit(False);
                end;
		end;
	finally
        if List <> nil then List.free;
        AProcess.Free;
	end;
    Result := false;
end;

{$else}

// these stubs will be used if not compiling under linux.  Best moved to an inc
// file if we start implementing something here.

function TAndFileTrans.OS_UploadFile(FullFileName: string; ID : string = ''): boolean;
begin
    Result := false;
end;

function TAndFileTrans.OS_DownLoadFile(FN : string): boolean;
begin
    Result := false;
end;

function TAndFileTrans.OS_DeleteFile(const FName : string) : boolean;
begin
    Result := false;
end;

function TAndFileTrans.OS_FileExists(aName : string; isDirectory : boolean = false) : boolean;
begin
    Result := false;
end;

function TAndFileTrans.OS_ListDirectory(var List : TstringList; const aDir : string) : boolean;
begin
    Result := false;
end;

function TAndFileTrans.OS_Location() : boolean;
begin
    Result := false;
end;

{$endif}


end.

