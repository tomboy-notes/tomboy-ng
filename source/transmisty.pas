// this is a comment
unit transmisty;

{  Copyright (C) 2017-2025 David Bannon

    License:
    This code is licensed under MIT License, see the file License.txt
    or https://spdx.org/licenses/MIT.html  SPDX short identifier: MIT

  ------------------

  A unit that does the file transfer side of Sync operation to the Misty Web Service
  HISTORY
  2025-06-23  First working version.
}

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
    Classes, SysUtils, trans, SyncUtils, tb_utils, ResourceStr;    // we share resources with GithubSync

type

  { TMistySync }

  TMistySync = Class(TTomboyTrans)
    private
                    // This is the downloader, pass a URL such as -
                    // For a note download, http://localhost:8080/DOWNLOAD/26D6EDFD-B518-472A-985B-AEF0B266D00F.note
                    // For the manifest, http://localhost:8080/DOWNLOAD/MANIFEST
                    // Ret false if something went wrong, sets ErrorString
        function Downloader(URL: string; out SomeString: String;
            const ConType: TContentType; const Header: string = ''): boolean;

                    // A version of DownLoader that retries
        function DownloaderSafe(URL: string; out SomeString: String;
            const ConType: TContentType; const Header: string = ''): boolean;

            // Private - Downloads the remote server Manifest for synced note details. It gets ID, RevNo
            // and the LastChangeDate. Returns False if error OR manifest does not exist (ie new repo)
        function ReadRemoteManifest: boolean;
        procedure SaveString(InString, OutFilePath: string);

                    // Will be used to upload a note or a manifest file to the server, in both cases, XML
                    // eg UpLoader(RemoteAddress, NotesDir + 'B4E48D4F-061D-420E-AC5C-6C81B1812094.note')
                    // Returns False and ErrorString wil be set if somthing goes wrong
        function Uploader(const URL, FFName: string): boolean;

    public

        HeaderOut : string;     // A very ugly way to get header back from Downloader, do better !
        //RemoteDir : string; // where the remote filesync repo lives.

        RMetaData : TNoteInfoList;   // only present in TransMisty, need to allow TestTransport to call ReadRemoteManifest()

        // ------ I n h e r i t e d   M e t h o d s  ------

                    // Misty, public inherited. Establish we can talk to Server unathenticated, might
                    // return one of SyncReady, SyncOpenSSLError, SyncNetworkError. Working server will
                    // return a human readable error page and code 200 if no command given. }
        function SetTransport(): TSyncAvailable; override;
                    // Misty, public inherited.
        function TestTransport(const WriteNewServerID : boolean = False) : TSyncAvailable; override;
                    // Just a stub, we get this data in TestTransport()
        function GetRemoteNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;
                    // Misty, public inherited.  For each note, create backup and then
                    // download frm server, put into note directory.
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
                    // Misty, public inherited.
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
                    // Misty, public inherited.
        function UploadNotes(const Uploads : TStringList) : boolean; override;
                    // Misty, public inherited.
        function DoRemoteManifest(const RemoteManifest : string; MetaData : TNoteInfoList = nil) : boolean; override;
                    // Misty, public inherited.
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
        // function SetRemoteRepo(ManFile : string = '') : boolean; override;
                    // Misty, public inherited.
        constructor Create(PP : TProgressProcedure = nil);
  end;


implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil, LazLogger, fphttpclient,
    ssockets, fpopenssl;

{ TMistySync }


// ===================== P r i v a t e    M e t h o d s ========================

const MaxNetTries = 3;
      TempDir='Temp/';  // usually off NotesDir, a dir to save temp downloaded files to.


function TMistySync.DownloaderSafe(URL : string; out SomeString : String; const ConType : TContentType; const Header : string = '') : boolean;
var Cnt : integer = 0;
begin
    // DownloaderSafe();
    repeat
        Result := Downloader(URL, SomeString, ConType, Header);
        if Result then break;

        sleep(100*Cnt*Cnt*Cnt);        // 0, 100, 800, 2700, 6400
        if ProgressProcedure <> nil then
            ProgressProcedure('Download problem retrying ' + inttostr(cnt));
        inc(Cnt);
        DebugLn(#10'TMistySync.DownloaderSafe - NOTICE retry no.' + Cnt.tostring + ' download failed ' + URL);
    until cnt = MaxNetTries;
    if not result then
        DebugLn('TMistySync.DownloaderSafe - ERROR - download failed ! ' + ErrorString);
        // ToDo : This should raise an exception.
end;

function TMistySync.Downloader(    URL : string; out SomeString : String; const ConType : TContentType; const Header : string = '') : boolean;
var
    Client: TFPHTTPClient;
begin
    if DebugMode then debugln('TMistySync.Downloader URL is ' + URL);
    //InitSSLInterface;
    // curl -i -u $GH_USER https://api.github.com/repos/davidbannon/libappindicator3/contents/README.note
    Client := TFPHttpClient.Create(nil);
//    Client.UserName := UserName;
//    Client.Password := Password; // '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';     eg a github token
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    case ConType of
        ctXML : Client.AddHeader('Content-Type','application/xml; charset=UTF-8');
        ctText : Client.AddHeader('Content-Type','application/text; charset=UTF-8');
        ctJSON : Client.AddHeader('Content-Type','application/json; charset=UTF-8');
        ctHTML : Client.AddHeader('Content-Type','application/HTML; charset=UTF-8');
    end;
    Client.AllowRedirect := true;
    Client.ConnectTimeout := 8000;      // mS ?  was 3000, I find initial response from github very slow ....
    Client.IOTimeout := 4000;           // mS ? was 0
    SomeString := '';
    try
        try
            SomeString := Client.Get(URL);
            // SayDebugSafe('TMistySync.Downloader Code:' + inttostr(Client.ResponseStatusCode) + ' - ' + SomeString);
        except
            on E: EHTTPClient do begin                                          // eg, File Not Found, we have asked for an unavailable file
                ErrorString := 'TMistySync.Downloader - EHTTPClient Error ' + E.Message
                    + ' ResultCode ' + inttostr(Client.ResponseStatusCode);
                exit(SayDebugSafe(ErrorString));
            end;
            on E: ESocketError do begin
                ErrorString := 'TMistySync.Downloader - SocketError ' + E.Message     // eg failed dns, timeout etc
                    + ' ResultCode ' + inttostr(Client.ResponseStatusCode);
                SomeString := 'Fatal, is server available ?';
                exit(SayDebugSafe(ErrorString));
                end;
            on E: EInOutError do begin
                ErrorString := 'TMistySync Downloader - InOutError ' + E.Message;
                // might generate "TGithubSync Downloader - InOutError Could not initialize OpenSSL library"
                SomeString := 'Failed to initialise OpenSSL';           // is error message translated ?
                exit(SayDebugSafe(ErrorString));
                end;
            on E: ESSL do begin
                ErrorString := 'TMistySync.Downloader - SSLError ' + E.Message;         // eg openssl problem, maybe FPC cannot work with libopenssl
                SomeString := 'Failed to work with OpenSSL';
                exit(SayDebugSafe(ErrorString));
                end;
            on E: Exception do begin
                ErrorString := 'TMistySync.Downloader Unexpected Exception ' + E.Message + ' downloading ' + URL;
                ErrorString := ErrorString + ' HTTPS error no ' + inttostr(Client.ResponseStatusCode);
                exit(SayDebugSafe(ErrorString));
                end;
        end;
        Result := Client.ResponseStatusCode = 200;
        // My version of this in GitHubSync then did some processing of Client.ResponseHeaders, apparently not necessary here.
    finally
        Client.Free;
    end;
end;


function TMistySync.Uploader(const URL, FFName: string): boolean;
var
    Client: TFPHttpClient;
    Response : TStringStream;
begin
    Result := false;
    if DebugMode then debugln('TMistySync.UpLoader - Posting ', FFName, ' to ' + URL);
    Client := TFPHttpClient.Create(nil);
    try
        try
            Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
            Response := TStringStream.Create(nil);
            Client.FileFormPost(URL, 'myfield', FFName, Response);      // ToDo : use field name for extra check ?
            case Client.ResponseStatusCode of
                200 : result := true;
                422 : ErrorString := 'TMistySync.Uploader - failed to save note on server';
                else ErrorString := 'TMistySync.Uploader - unknown error uploading note, ' + inttostr(Client.ResponseStatusCode);
            end;
            if (Client.ResponseStatusCode <> 200) and (Client.ResponseStatusCode <> 422) then
                debugln(ErrorString);

        except on E:Exception do begin                    // I have no idea what might happen here ?
                ErrorString := 'TMistySync.Uploader - unexpected Exception : ' + E.Message;
                debugln(ErrorString);
                exit(SayDebugSafe(ErrorString));
            end;
        end;
    finally
        Response.Free;
        Client.Free;
    end;
end;

// =================== I n h e r i t e d   M e t h o d s  ======================
// -----------------------------------------------------------------------------


function TMistySync.SetTransport(): TSyncAvailable;
var SomeString : string;
begin
    // SetTransport();

    if DebugMode then DebugLn('TMistySync.SetTransport RemoteAddress = ' + RemoteAddress);

   RemoteAddress := AppendPathDelim(RemoteAddress);                             // ToDo : NOT WINDOWS COMPATIBLE
   if Downloader(RemoteAddress,  SomeString, ctHTML) then    // all we care about is the 200 status Downloader found
        Result := SyncReady                                            // OR maybe create a repo code goies here ?
   else Result := SyncNetworkError;

// following are some tests used during development, clean it out !

(* if Downloader('http://localhost:8080/DOWNLOAD/26D6EDFD-B518-472A-985B-AEF0B266D00F.note',  SomeString, ctXML) then
        writeln('TMistySync.SetTransport() A Note [' + SomeString + ']');   *)

(*    if not Uploader(RemoteAddress, NotesDir + 'B4E48D4F-061D-420E-AC5C-6C81B1812094.note')
    then begin
         writeln('TMistySync.SetTransport() client ERROR testing UpLoader - ' + ErrorString);
         exit(SyncNetworkError);
     end else writeln('TMistySync.SetTransport() Seemed to work - ' + ErrorString);  *)

(*      writeln('TMistySync.SetTransport() now going to do download');
    if Downloader('http://localhost:8080/DOWNLOAD/B4E48D4F-061D-420E-AC5C-6C81B1812094.note',  SomeString, ctXML) then
        writeln('TMistySync.SetTransport() A Note [' + SomeString + ']') else
     writeln('TMistySync.SetTransport() failed to download note - ' + ErrorString);    *)
end;


function TMistySync.TestTransport(const WriteNewServerID : boolean = False): TSyncAvailable;
var
    GUID : TGUID;
    ManExists : boolean; // for readability of code only
begin
    // TestTransport();
    if ANewRepo then begin
        CreateGUID(GUID);
        ServerID := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
        RemoteServerRev := -1;
        exit(SyncReady);
    end;

    ManExists :=  ReadRemoteManifest();
    if (pos('404', ErrorString) > 0) then
        ErrorString := 'Remote dir does not contain a manifest ' + RemoteAddress;  // not always an error !

    if not ManExists then exit(SyncNoRemoteRepo);

    if 36 <> length(ServerID) then begin
        ErrorString := 'Invalid ServerID';
        exit(SyncXMLError);
    end;
    Result := SyncReady;
    if DebugMode then begin
        debugln('TMistySync.TestTransport ServerID  ', ServerID);
        debugln('TMistySync.TestTransport Revision  ', inttostr(RMetaData.LastRev));
        debugln('TMistySync.TestTransport RMetaData ', inttostr(RMetaData.Count));
    end;
end;

function TMistySync.GetRemoteNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean;
begin
    result := True;           // Just a stub, we get this data in TestTransport()
end;

function TMistySync.ReadRemoteManifest(): boolean;
var
    ManifestSt : string;
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    NoteInfo : PNoteInfo;
    j : integer;
    ManifestStream : TStringStream;
begin
    if RMetaData <> Nil then FreeAndNil(RMetaData);         // RMetaData is a copy, placed here during initialisation, of Sync's ReoteMetaData
    RMetaData := TNoteInfoList.Create;
    Result := true;
    if not Downloader(RemoteAddress + 'MANIFEST', ManifestSt, ctXML) then exit(False);       // Manifest not there, probably new repo ?
    try
    	try
    		ManifestStream := TStringStream.create(ManifestSt);
            ReadXMLFile(Doc, ManifestStream);
            // 2nd line of server looks like this <sync revision="498" server-id="C20866A3-9D6D-415C-964A-2485E031D1A4">
            ServerID := Doc.DocumentElement.GetAttribute('server-id');
            RemoteServerRev := strtoint(Doc.DocumentElement.GetAttribute('revision'));
            RMetaData.ServerID := ServerID;
            RMetaData.LastRev := RemoteServerRev;
    		NodeList := Doc.DocumentElement.ChildNodes;
    		if assigned(NodeList) then begin
        		for j := 0 to NodeList.Count-1 do begin
                    new(NoteInfo);
                    NoteInfo^.ForceUpload := false;
                    NoteInfo^.Action:=SyUnset;
                    Node := NodeList.Item[j].Attributes.GetNamedItem('id');
                    NoteInfo^.ID := Node.NodeValue;									// ID does not contain '.note';
					Node := NodeList.Item[j].Attributes.GetNamedItem('rev');
                    NoteInfo^.Rev := strtoint(Node.NodeValue);                      // what happens if its empty ?
                    Node := NodeList.Item[j].Attributes.GetNamedItem('last-change-date');
                    // if assigned(node) then                                   // We can assume all manifest entries do have a LCD here.
                    NoteInfo^.LastChange:=Node.NodeValue;
                    //if NoteInfo^.LastChange <> '' then
                    //    NoteInfo^.LastChangeGMT := TB_GetGMTFromStr(NoteInfo^.LastChange);
                    RMetaData.Add(NoteInfo);
                end;
		    end;
        except
            on E: EAccessViolation do begin
                Result := false;	// probably means we did not find an expected attribute
                ErrorString := 'Remote Manifest Error ' + E.Message;
            end;
            on E: EConvertError do begin
                ErrorString := 'Remote Manifest Error ' + E.Message;
                Result := false;	// probably means we did not find an expected attribute
            end;
	    end;
	finally
        Doc.Free;
        ManifestStream.Free;
	end;
    if Result = True then begin
    	if debugmode then Debugln('TMistySync.ReadRemoteManifest - read OK');
    end else begin
        DebugLn('TMistySync.ReadRemoteManifest - We failed to read the remote manifest file ', RemoteAddress + 'manifest.xml');
        debugln(ErrorString);
    end;
end;

procedure TMistySync.SaveString(InString, OutFilePath: string);
var
    F: TextFile;
begin
    AssignFile(F, OutFilePath);           // ToDo : some error checking please.
    try
        ReWrite(F);
        Write(F, InString);
    finally
      CloseFile(F);
    end;
end;

(* function TMistySync.GetNoteLastChange(const FullFileName : string) : string;
begin
    Result := GetNoteLastChangeSt(FullFileName, ErrorString);   // syncutils function
    // Is this something I should replace with a call to MetaData ?  LastChange ?
end;  *)

function TMistySync.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
var
    I : integer;
    DLCount : integer = 0;
    NoteString : string;
    DownCount : integer = 0;
begin
    if ProgressProcedure <> nil then progressProcedure(rsDownloadNotes);
    if not DirectoryExists(NotesDir + 'Backup') then
        if not ForceDirectory(NotesDir + 'Backup') then begin
            ErrorString := 'Failed to create Backup directory.';
            exit(False);
        end;
    ErrorString := '';
    for I := 0 to DownLoads.Count-1 do begin
        if DownLoads.Items[I]^.Action = SyDownLoad then begin
            inc(DLCount);
            if (DLCount mod 5 = 0) and (ProgressProcedure <> nil) then
                ProgressProcedure(rsDownLoaded + ' ' + inttostr(DLCount) + ' notes');
            if FileExists(NotesDir + Downloads.Items[I]^.ID + '.note') then
                // First make a Backup copy
                if not CopyFile(NotesDir + Downloads.Items[I]^.ID + '.note',
                        NotesDir + 'Backup' + PathDelim + Downloads.Items[I]^.ID + '.note') then begin
                    ErrorString := 'Failed to copy file to Backup ' + NotesDir + Downloads.Items[I]^.ID + '.note';
                    exit(False);
                end;
            // OK, now download the file.
            if DebugMode then DebugLn('DownloadNotes() - downloading ' + Downloads.Items[I]^.ID + '.note');
            if DownloaderSafe(RemoteAddress+'/DOWNLOAD/' + Downloads.Items[I]^.ID + '.note', NoteString, ctXML) then begin
                if pos('<note version=', NoteString) > 0 then
                    SaveString(NoteString, NotesDir + Downloads.Items[I]^.ID + '.note')
                else  debugln('TMistySync.DownloadNotes - Error detected in note, NOT Synced ' + NotesDir + Downloads.Items[I]^.ID + '.note');
            end else debugln('TMistySync.DownloadNotes - Failed to download a note, NOT Synced ' + NotesDir + Downloads.Items[I]^.ID + '.note');
            // In both cases above, the sync will (attempt to) proceed, hoping its an error with that note.
            // ToDo : I really should log this, perhaps set an Error in Downloads or a list the SyncGui gets to see. Probaby never happen but ....
            inc(DownCount);
            if (DownCount mod 10 = 0) then
                if ProgressProcedure <> nil then ProgressProcedure(rsDownLoaded + ' ' + inttostr(DownCount) + ' notes');
        end;
    end;
    result := True;
end;

function TMistySync.DeleteNote(const ID: string; const ExistRev : integer ): boolean;
begin
    // I think all that happens is deleted note is not listed in remote manifest
    // and that is done. But other thansport modes might need to do something here
    if DebugMode then DebugLn('TMistySync.DeleteNote() - nothing to do here');
  result := True;
end;

function TMistySync.UploadNotes(const Uploads: TStringList): boolean;
var
    Index : integer;
    UpCount : integer = 0;
begin
    if ProgressProcedure <> nil then progressProcedure('Uploading ' + inttostr(UpLoads.Count) + ' notes');
    for Index := 0 to Uploads.Count -1 do begin
        if DebugMode then debugln(rsUpLoading + ' ' + Uploads.Strings[Index] + '.note');
        if not UpLoader(RemoteAddress, NotesDir + Uploads.Strings[Index] + '.note') then
            exit(False);           // ErrorString will have been set, terminate sync (ie, dont upload new manifest)
        inc(UpCount);
        if (UpCount mod 5 = 0) and (ProgressProcedure <> nil) then
            progressProcedure(rsUpLoaded + ' ' + inttostr(UpCount) + ' notes');
    end;
    result := True;
end;

function TMistySync.DoRemoteManifest(const RemoteManifest: string; MetaData : TNoteInfoList = nil): boolean;
begin
    if DebugMode then DebugLn('TMistySync.DoRemoteManifest uploading Remote Manifest');
    result := Uploader(RemoteAddress, RemoteManifest);
end;

function TMistySync.DownLoadNote(const ID: string; const RevNo: Integer): string;
var NoteString : string;
begin
    // we don't need no stinking RevNo
    // The server will sort that out, just ask for the $ID.note
    Result := '';
    if DownloaderSafe(RemoteAddress + 'DOWNLOAD/' + ID + '.note', NoteString, ctXML) then begin
        if pos('<note version=', NoteString) < 1 then begin
            debugln('TMistySync.DownloadNote - Error detected in note, NOT Synced ' + NotesDir + ID + '.note for clash processing');
            exit('');
        end;
        if not DirectoryExists(NotesDir + TempDir) then
            ForceDirectoriesUTF8(NotesDir + TempDir);      // We already know we can write in NotesDir
        Result := NotesDir + TempDir + ID + '.note';
        SaveString(NoteString, Result);
        if FileExists(Result) then exit
        else Result := '';
    end;
    if Result  = '' then
        debugln('MistySync.DownLoadNote failed to get ' + ID + ' for clash processing');
end;

constructor TMistySync.Create(PP : TProgressProcedure = nil);
begin
    ProgressProcedure := PP;
end;

end.

