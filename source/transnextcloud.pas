unit transnextcloud;

{ A unit that provides transport for a sync to the NextCloud Notes application.

  *  Copyright (C) 2020 David Bannon
  *  See attached licence file.

  HISTORY              

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, trans, SyncUtils, fpjson, jsonparser;


type

  { TNextCloudSync }

  TNextCloudSync = class(TTomboyTrans)      
    private
	    function Downloader(URL: string; SomeStrings: TStringList): boolean;
		function ExtractJID(data: string): longint;
		                    { Adds http if necessary (must add https) and returns either SyncReady,
				              SyncCredentialError or SyncNetworkError depending on what it found.
				              Will also set ErrorString if not SyncReady.  }
		function FormatTestURL(): TSyncAvailable;
                            { Downloads a list of notes from NextCloud, not getting things mentioned
                              in Params, so, typically, '?exclude=content,title'. Results put into
                              the (must be created) TNoteInfoList. }
		function GetListOfNotes(const NoteMeta: TNoteInfoList; Params: string=''): boolean;
		function GetServerID(SID: longint): boolean;
		function JData2NoteList(const NoteMeta: TNoteInfoList; jItm: TJSONData): boolean;
		function PostNewNote(Title, Content: String): longint;
		function PushUpNote(SimpleID: string): longint;
		procedure SayDebug(st: string; Always: boolean=false);
		function TestNextCloudConnection(out MSG: string; const UsePassword: boolean): boolean;

                            { Tries to upload a KEYNOTE, might fail for a whole lot of reasons that we should
                              really have tested before we got to here. Later. What if there is already one
                              there with a bad UUID ?  TextCoonections should deal with things like that.}
	    function WriteANewServerID(): boolean;
                            { These are just a debug function. }
        procedure DumpJSON(const St: string);
        //procedure SayDebug(st : string; Always : boolean = false);
    public
        function TestTransport(const WriteNewServerID : boolean = False) : TSyncAvailable; override;  
        function SetTransport(ForceClean : boolean = False) : TSyncAvailable;  override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;   
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;  
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override; 
        function UploadNotes(const Uploads : TNoteInfoList) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override; 
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; override; 
  end;


implementation

{ TNextCloudSync }

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil,
    {$if (FPC_FULLVERSION=30200)}
	    opensslsockets,                 // only available in FPC320 and later
	{$endif}
	fphttpclient, httpprotocol, {$ifdef LCL}lazlogger, {$endif}
	fpopenssl, ssockets, DateUtils, commonmark, import_notes;

const
  URL_SUFFIX = '/index.php/apps/notes/api/v1/notes';
  USER = 'dbannon';         // not a long term solution, only works for people called dbannon !
  KEYNOTETITLE = 'NextCloud to tomboy-ng sync';
  KEYNOTETEXT  = 'Please do not alter this note, its an important part of the tomboy-ng sync\n--------\n';
  KEYNOTEUUID  = 'TOMBOY-NG UUID = ';

function TNextCloudSync.TestTransport(const WriteNewServerID: boolean) : TSyncAvailable;
            { Here we will try to contact the server, look for a note that records the ServerID,
              If we are doing a RepoJoin then ANewRepo will be set, if no KEYNOTE, generate a ServerID,
              send it to the remote NextCloud in a KEYNOTE. If there is already one there, SyncMisMatch
              Might return SyncReady, SyncCredentialsError, SyncNetworkError, SyncNoRemoteRepo, SyncMisMatch }
var
    NoteMeta : TNoteInfoList;
    PNote : PNoteInfo;
    i : integer;
begin
    Result := FormatTestURL();                              // 'adjusts' url and tries to use it.
    if Result <> SyncReady then exit;                       // Failed to find a workable form of URL
    ServerID := '';

    NoteMeta := TNoteInfoList.Create;
    try
	    GetListOfNotes(NoteMeta, '?exclude=modified,catagory,favorite,content');
	    for i := 0 to NoteMeta.count -1 do begin
	        PNote := NoteMeta[i];
	        if PNote^.Title = KEYNOTETITLE then
	            if GetServerID(PNote^.SID) then begin
                    result := SyncReady;
	                break;
			    end else ErrorString := 'TransNextCloud remote KEYNOTE does not have a GUID';
			//SayDebug(PNote^.Title);
		end;
	finally
        NoteMeta.Free;
	end;
    // OK, whether or not having a ServerID is a good thing depends on the mode we are in.
    if ANewRepo then begin
        if (ServerID = '') then
            if WriteANewServerID() then          // will have set ServerID if successful.
                Result := SyncReady
            else Result := SyncNoRemoteRepo
        else Result := SyncMismatch;            // We should not have a remote ServerID if making a new repo.
	end else begin                              // So, we are not making a new repo then.
        if (ServerID = '') then                 // Thats bad.
            Result := SyncNoRemoteRepo
        else Result := SyncReady;
	end;
    SayDebug('TestTransport is exiting with SyncReady = ' + booltostr((Result = SyncReady), True));
end;

function TNextCloudSync.SetTransport(ForceClean: boolean): TSyncAvailable;
begin
    { SyncNoLocal indicates there is no local manifest and therefore no sync setup.
      SyncReady  would indicate there is a local manifest and it may be viable
      but we cannot, at this stage, tell ?
    }
    if Fileexists(ConfigDir + 'manifest.xml') then
        result := SyncReady
    else Result := SyncNoLocal;
    if (ForceClean and (Result = Syncready)) then begin
        DeleteFile(ConfigDir + 'manifest.xml');
        DeleteFile(ConfigDir + 'manifest-remote.xml');
        Result := SyncNoLocal;
        // ToDo : consider removing the remote KEYNOTE here ?  Possible ?  No, better to make user do it, to be sure.
	end;
end;

function TNextCloudSync.GetNewNotes(const NoteMeta: TNoteInfoList; const GetLCD: boolean): boolean;
            { gets a list of notes from NextCloud, we get only SID and modified at this stage.
              As NC always gives us its weak LCD, getLCD has no effect. RevNo is also irrelevant, set to 1}
begin
    result := GetListOfNotes(NoteMeta, '?exclude=favorite,content');
    Saydebug('TNextCloudSync.GetNewNotes = ' + inttostr(NoteMeta.Count));
end;

function TNextCloudSync.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
var
    PNote : PNoteInfo;
    Strs : TStringList;
    Content : string = '';
    jData : TJSONData;
    jObj  : TJSONObject;
    jStr :  TjsonString;
    Imp : TImportNotes;
begin
    for PNote in Downloads do begin
        if pNote^.Action <> SyDownload then continue;
        Strs := TStringList.create;
        if Downloader(RemoteAddress + URL_SUFFIX + '/' + inttostr(PNote^.SID), Strs) then begin
            //DumpJSON(Strs.Text);
            jData := GetJSON(Strs.text);
            jObj := TJSONObject(jData);
            if jObj.Find('content', jStr) then
                Content := jStr.AsString;
            jData.free;
            Strs.Free;
            Strs := TStringList.Create;
            Strs.text:=StringReplace(Content,'\n',Lineending,[rfReplaceAll, rfIgnoreCase]);
		end;
        if Strs.count > 1 then begin
            Strs.SaveToFile(inttostr(PNote^.SID) + '.md');
            Imp := TImportNotes.create;
            Imp.Mode   := 'markdown';
            Imp.DestinationDir := NotesDir;
            Imp.CrDate := pNote^.LastChange;
            Imp.LCDate := pNote^.CreateDate;
            Imp.MDtoNote(Strs, pNote^.Title, pNote^.ID);
            Imp.free;
		end;
		Strs.free;
	end;
    result := true;
end;

function TNextCloudSync.DeleteNote(const ID: string; const ExistRev: integer): boolean;
var
    Client: TFPHttpClient;              { yes, definitly needed here. }
    STRL  : TStringList;
begin
    Result := False;
    Client := TFPHttpClient.Create(nil);
    STRL := TStringList.Create;
    Client.UserName := USER;
    Client.Password := Password;
    Client.AllowRedirect := true;
    try
        try
            Client.Delete(RemoteAddress + '/' + ID, STRL);
            if Client.ResponseStatusCode = 200
                then Result := True
            else
                ErrorString := 'Network Error, Response Code ' + inttostr(Client.ResponseStatusCode);
        except
            on E: Exception do begin
                    ErrorString := 'TransNexCloud DeleteNote -Something bad happened ' + E.Message;
                    SayDebug('TransNexCloud DeleteNote -Something bad happened ' + E.Message, True);
                end;
		end;
	finally
        Client.Free;
        STRL.Free;
	end;
	result := true;
end;

function TNextCloudSync.UploadNotes(const Uploads: TNoteInfoList): boolean;
var
    I : integer;
begin
    result := true;
    for I := 0 to Uploads.Count -1 do begin
        if UpLoads[i]^.Action = SyUpLoadNew then
            pNoteInfo(UpLoads[i])^.SID := PushUpNote(pNoteInfo(UpLoads[i])^.ID);
        if UpLoads[i]^.SID = 0 then
            Result := false;
    end;
end;

function TNextCloudSync.UploadNotes(const Uploads: TStringList): boolean;
// We have no need for this function here. Use UploadNotes(TNoteInfoList)
begin
        result := false;
end;

function TNextCloudSync.DoRemoteManifest(const RemoteManifest: string): boolean;
                        // Lets assume we have no need for this function here.
begin
     result := true;
end;

function TNextCloudSync.DownLoadNote(const ID: string; const RevNo: Integer): string;
                        // Lets assume we have no need for this function here.
begin
     result := '';
end;

// -----------------------  Private Functions ---------------------


function TNextCloudSync.FormatTestURL() : TSyncAvailable;
var
    MSG : string;
begin
    ErrorString := '';
    if (copy(RemoteAddress, 1, 4) <> 'http') then begin             // else user has provided the prefix
        RemoteAddress := 'http://' + RemoteAddress;                 // we try http first, must add https later
	end;
    if TestNextCloudConnection(MSG, True) then exit(SyncReady);     // All good
    if TestNextCloudConnection(MSG, False) then begin               // Just a credentials issue
            ErrorString:= 'Invalid Credentials';
            exit(SyncCredentialError);
	end;
    ErrorString := MSG;                                             // More serious network problem.
    exit(SyncNetworkError);
end;



function TNextCloudSync.WriteANewServerID() : boolean;
var
    GUID : TGUID;
begin
    CreateGUID(GUID);
    ServerID := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
    Result := PostNewNote(KEYNOTETITLE, KEYNOTETEXT + KEYNOTEUUID + ServerID) > 0;
    if not Result then
        ServerID := '';
end;




function TNextCloudSync.GetServerID(SID : longint) : boolean;
var
    Strs : TStringList;
    jData : TJSONData;
    jObj  : TJSONObject;
    jStr : TJSONString;
    Content : string;
    UUIDIndex : integer;
    {TEMPMeta : TNoteInfoList;
    PNote : PNoteInfo;}

begin
    result := false;
    Saydebug('In GetServerID looking for ' + inttostr(SID));
    Strs := TStringList.create;
    try
        if Downloader(RemoteAddress + URL_SUFFIX + '/' + inttostr(SID), Strs) then begin
            //DumpJSON(Strs.Text);
            jData := GetJSON(Strs.text);

            { This just to display, for test, the date from
            TEMPMeta := TNoteInfoList.Create;
            jData2NoteList(TEMPMeta, jData);
            PNote := TEMPMeta[0];
            Saydebug('LastChange = ' + PNote^.LastChange);
            Saydebug('LCD GMT    = ' + FormatDateTime('YYYY-MM-DD', PNote^.LastChangeGMT ) + 'T'
                        + FormatDateTime('hh:mm:ss.0000000"', pNote^.LastChangeGMT));
            SayDebug('Create     = ' + pNote^.CreateDate);
            TempMeta.free; }

            jObj := TJSONObject(jData);
            if jObj.Find('content', jStr) then begin
                Content := jStr.asstring;
                UUIDIndex := pos(KEYNOTEUUID, Content);
                if UUIDIndex > -1 then
                    ServerID := copy(Content, UUIDIndex + length(KEYNOTEUUID), 36);
                if IDLooksOK(ServerID) then begin
                    Result := True;
                    SayDebug('Remote Server ID is ' + ServerID);
				end;
			end;
            jData.free;
		end;
	finally
        Strs.free;
	end;
end;

function TNextCloudSync.TestNextCloudConnection(out MSG : string; const UsePassword : boolean) : boolean;
{   If return false             - Invalid IP address - Socket Error : Connection to 10.85.2.3:80 timed out
                    Something there but no Nextcloud - Socket Error : Connect to 192.168.1.250:80 failed.

So, call this initially with True, if it works, probably so will everything else.  If it fails with "Socket
Error ..." tell user to check address.  If it fails with "Invalid logon credentials" first try without
the credentials, if that works, then check user:password.  If both fail, its some NextCloud issue .... }

var
    Client: TFPHttpClient;
    SomeStrings : TStringList;
begin
    Result := false;
    Client := TFPHttpClient.Create(nil);
    if UsePassword then begin
        Client.UserName := USER;
        Client.Password := Password;
    end;
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    Client.AddHeader('OCS-APIRequest', 'true');
    Client.AddHeader('Accept', 'application/json');
    Client.AllowRedirect := true;
    SomeStrings := TStringList.create;
    try
        try
            Client.Get(RemoteAddress + '/ocs/v1.php/cloud/capabilities', SomeStrings);
        except
            on E: EInOutError do begin
                SayDebug('TransNexCloud Downloader - InOutError ' + E.Message, True);
                exit(false);
                end;
            on E: ESSL do begin
                SayDebug('TransNexCloud Downloader -SSLError ' + E.Message, True);
                exit(false);
                end;
            on E: ESocketError do begin
                MSG := 'Socket Error : ' + E.Message;
                exit(false);
                end;
            on E: Exception do begin
                MSG := 'Something bad happened : ' + E.Message;
                exit(false);
                end;
        end;
        if UsePassword then begin    // we will still have content but not some specific things
            if (SomeStrings.Count > 0) and (pos('password_policy', SomeStrings.Text) > 0) then begin
                Result := True;
                MSG := '';
			end else Msg := 'Invalid logon credentials'
        end else begin
            if (SomeStrings.Count > 0) and (pos('{"ocs":{"meta"', SomeStrings.Text) > 0) then begin
                    Result := True;
                    MSG := '';
            end;
        end;
    finally
        SomeStrings.free;
        Client.Free;
    end;
end;


function TNextCloudSync.Downloader(URL : string; SomeStrings : TStringList) : boolean;
var
    Client: TFPHttpClient;
begin
    // Windows can be made work with this if we push out ssl dll - see DownloaderSSL local project
    //InitSSLInterface;
    Client := TFPHttpClient.Create(nil);
    Client.UserName := USER;
    Client.Password := Password;
    Client.AllowRedirect := true;
    try
        try
            Client.Get(URL, SomeStrings);
        except
            on E: EInOutError do begin
                SayDebug('TransNexCloud Downloader - InOutError ' + E.Message, True);
                exit(false);
                end;
            on E: ESSL do begin
                SayDebug('TransNexCloud Downloader -SSLError ' + E.Message, True);
                exit(false);
                end;
            on E: Exception do begin
                SayDebug('TransNexCloud Downloader -Something bad happened ' + E.Message, True);
                SayDebug('URL was ' + URL, True);
                SayDebug('U=' + USER + 'P=' + Password);
                exit(false);
                end;
        end;
    finally
        Client.Free;
    end;
    //ErrorMsg := '';
    result := true;
end;

function TNextCloudSync.JData2NoteList(const NoteMeta: TNoteInfoList; jItm : TJSONData) : boolean;
var
    jObj : TJSONObject; jBool : TJSONBoolean; PNote : PNoteInfo; jNumb : TJSONNumber;
    jString : TJSONString;
    DTStr : string; Off : longint;
    Res : string;
begin
    Result := True;
    jObj := TJSONObject(jItm);
    //saydebug('in jobj2pnote');
    if JObj.Find('error', jBool) and (jBool.AsBoolean = false) then begin
            //saydebug('writing pnote');
            new(PNote);
            PNote^.ID := '';                                                        // ID
            if jObj.Find('modified', jNumb) then begin
                pNote^.LastChangeGMT := UnixToDateTime(jNumb.AsInteger);            // LastChangeGMT
                DTStr := FormatDateTime('YYYY-MM-DD', pNote^.LastChangeGMT) + 'T'
                    + FormatDateTime('hh:mm:ss.0000000"', pNote^.LastChangeGMT);
                Off := GetLocalTimeOffset();            // local time offset in minutes
                if (Off div -60) >= 0 then Res := '+'
	            else Res := '-';
	            if abs(Off div -60) < 10 then Res := Res + '0';
	            Res := Res + inttostr(abs(Off div -60)) + ':';
       	        if (Off mod 60) = 0 then
		            Res := res + '00'
	            else Res := Res + inttostr(abs(Off mod 60));
                pNote^.LastChange :=  DTStr + Res;                                  // LastChange
			end;
            pNote^.CreateDate := DTStr + Res;                                       // CreateDate, will update from local note if possible
            pNote^.Rev := 1;                                                        // Rev
            if JObj.Find('id', JNumb) then
                PNote^.SID := JNumb.asInteger
            else PNote^.SID := 0;                                                   // SID
            PNote^.Action:= SyUnset;                                                // Action
            if jobj.Find('title', jString) then                                     // Title
                PNote^.Title := jString.AsString
            else PNote^.Title:= '';
            NoteMeta.Add(PNote);
	end else result := false;
end;

function TNextCloudSync.GetListOfNotes(const NoteMeta: TNoteInfoList; Params : string = '') : boolean;
var
    SomeStrings : TStringList;
    jData : TJSONData;
    jItem : TJSONData;
    i : integer;
begin
    SomeStrings := TStringList.Create;
    if Downloader(RemoteAddress + URL_SUFFIX + Params,  SomeStrings) then begin
        //SayDebug('That looks good');
        //dumpJSON(SomeStrings.text);
        jData := GetJson(SomeStrings.text);
        for i := 0 to JData.Count -1 do begin
                jItem := jData.Items[i];
                JData2NoteList(NoteMeta, JItem);
		end;
        jData.free;
	end else
        SayDebug('TansNextCloud GetListOfNotes, failed', True);
    //SayDebug('transnextcloud, Called GetListOfNotes');
    SomeStrings.free;
    exit(true);
end;




// -------------------- U P L O A D -----------------------


function TNextCloudSync.PushUpNote(SimpleID : string) : longint;
var
  STL : TStringList;
  Title : string = '';
  ENotes : TExportCommon;
begin
    ENotes := TExportCommon.Create;
    STL := TStringList.Create;
    try
    	ENotes.NotesDir := NotesDir;
        ENotes.GetMDcontent(SimpleID, STL);
        if STL.Count > 1 then begin
            Title := StringToJSONString(STL[0]);
            Stl.Delete(0);
            Stl.Delete(0);
	    end;
        STL.SaveToFile(Title + '.md');
        Result := PostNewNote(Title, StringToJSONString(STL.Text));
	finally
        STL.Free;
        freeandnil(ENotes);
    end;
end;


procedure TNextCloudSync.DumpJSON(const St : string);
var
    jData : TJSONData;
begin
    SayDebug('---------- TEXT ------------');
    SayDebug(St);
    JData := GetJSON(St);
    SayDebug('---------- JSON ------------');
    SayDebug(jData.FormatJSON);
    SayDebug('----------------------------');
    JData.Free;
end;

                // Ret the (NextCloud) ID if its found in the passed (json) string, 0 else.
function TNextCloudSync.ExtractJID(data : string) : longint;
var
    JData : TJSONData;
    JObject : TJSONObject;
    JNumb : TJSONNumber;
begin
    try
	    try
            JData := GetJSON(Data);         // requires a free
            JObject := TJSONObject(jData);  // does not require a free ??
            // Result := JObject.Get('id');    // will raise exceptions if not present, better to use Find
            if JObject.Find('id', jNumb) then
                Result := jNumb.AsInteger
            else Result := 0;
	    //except on  E: EVariantTypeCastError do Result := 0;
        except on E:Exception do Result := 0;   // Invalid JSON or ID not present
	    end;
	finally
        JData.Free;
	end;
end;


// Returns the NextCloud assigned NoteID on success, 0 on failure, must set a regional ErrorMessage
function TNextCloudSync.PostNewNote(Title, Content : String) : longint;
var
    Client: TFPHttpClient;
    Response : TStringStream;
begin
    Result := 0;
    saydebug('In PostNewNote');
    Client := TFPHttpClient.Create(nil);
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    Client.AddHeader('Content-Type','application/json; charset=UTF-8');
    Client.AddHeader('Accept', 'application/json');
    Client.AllowRedirect := true;
    Client.UserName:=USER;
    Client.Password:=Password;
    client.RequestBody := TRawByteStringStream.Create('{ "title": "'
                            + Title + '", "content": "' + content + '"}');
    Response := TStringStream.Create('');
    try
        try
            saydebug('About to Post');
            client.Post(RemoteAddress + URL_SUFFIX, Response);  // don't use FormPost, it messes with the Content-Type value
            saydebug('Posted');
            if Client.ResponseStatusCode = 200 then begin
                //if DebugMode then DumpJSON(Response.DataString);
                Result := ExtractJID(Response.DataString);
                SayDebug('Posted Note ID is ' + inttostr(Result));
			end else begin
			        SayDebug('Response Code is ' + inttostr(Client.ResponseStatusCode));
                    SayDebug(Response.DataString);
			end;
		except on E:Exception do
                SayDebug('TransNext.PostNewNote bad things happened : ' + E.Message);
                // Set some sort of error message here.
        end;
    finally
        Client.RequestBody.Free;
        Client.Free;
        Response.Free;
    end;
end;


procedure TNextCloudSync.SayDebug(st: string; Always : boolean = false);
begin
    if not (DebugMode or Always) then exit;
    {$ifdef LCL}Debugln{$else}writeln{$endif}(St);
end;


end.

