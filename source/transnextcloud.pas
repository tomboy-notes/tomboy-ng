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
  TNoteRec = record
    SID : integer;
    modified : longint;
    category : string;
    content  : string;
  end;

type

  { TNextCloudSync }

  TNextCloudSync = class(TTomboyTrans)      
    private
	    function Downloader(URL: string; SomeStrings: TStringList): boolean;
		function ExtractJID(data: string; out NRec: TNoteRec): longint;
		                    { Adds http if necessary (must add https) and returns either SyncReady,
				              SyncCredentialError or SyncNetworkError depending on what it found.
				              Will also set ErrorString if not SyncReady.  }
		function FormatTestURL(): TSyncAvailable;
                            { Downloads a list of notes from NextCloud, not getting things mentioned
                              in Params, so, typically, '?exclude=content,title'. Results put into
                              the (must be created) TNoteInfoList. }
		function GetListOfNotes(const NoteMeta: TNoteInfoList; Params: string=''): boolean;
		function GetServerID(SID: longint): boolean;
				                { Returns True if passed represents a Notebook Template, if not
				                it will fill out the passed string with first notebook encounted.
				                Note : we ignore all except the first Notebook encountered. }
		function IsTemplate(const ID: string; out Notebook: String): boolean;
		function JData2NoteList(const NoteMeta: TNoteInfoList; jItm: TJSONData): boolean;
		function PostNewNote(Title, Notebook, Content: String; out NRec: TNoteRec;
				SID: integer=0): longint;
		function PushUpNote(ID, Notebook: string; out NRec: TNoteRec; SID: integer
				): longint;


                            { Reads remote manifest if it exists, adding ID and Create Date to Meta
                            If its not there, no problem, we are doing a Join. Make sure its not
                            there IF we are doing a Join !!
                            Then looks through the MMD for any remaining notes that were not matched
                            by the remote manifest, these are new remote notes, will be marked so and
                            assigned a UUID ID.   }
		procedure ReadRemoteManifest(NoteMeta: TNoteInfoList);
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

                            // Takes a NextCloud epoc second count and returns a Tomboy style ISO8601
  function ModifiedToTBDate(Modified : longint) : string;


implementation

{ TNextCloudSync }

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil,
    {$if (FPC_FULLVERSION=30200)}
	    opensslsockets,                 // only available in FPC320 and later
	{$endif}
	fphttpclient, httpprotocol, {$ifdef LCL}lazlogger, LazUTF8, {$endif}
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
        DeleteFile(ConfigDir + 'manifest.xml-remote');
        DeleteFile(ConfigDir + 'manifest.xml-local');
        Result := SyncNoLocal;
        // ToDo : consider removing the remote KEYNOTE here ?  Possible ?  No, better to make user do it, to be sure.
	end;
end;

function TNextCloudSync.GetNewNotes(const NoteMeta: TNoteInfoList; const GetLCD: boolean): boolean;
            { gets a list of notes from NextCloud, we get only SID and modified at this stage.
              As NC always gives us its weak LCD, getLCD has no effect. RevNo is also irrelevant, set to 1}
begin
    result := GetListOfNotes(NoteMeta, '?exclude=favorite,content');
    if Result then ReadRemoteManifest(NoteMeta);
    Saydebug('TNextCloudSync.GetNewNotes = ' + inttostr(NoteMeta.Count));
end;

function TNextCloudSync.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
var
    PNote   : PNoteInfo;
    Strs    : TStringList;
    NRec    : TNoteRec;
    Imp     : TImportNotes;
begin
    for PNote in Downloads do begin
        if pNote^.Action <> SyDownload then continue;
        Strs := TStringList.create;
        try
            if Downloader(RemoteAddress + URL_SUFFIX + '/' + inttostr(PNote^.SID), Strs) then begin
debugln('NextCloud Downloadnotes - SID=' + inttostr(pNote^.SID) + ' ID=' + pNote^.ID);
                if ExtractJID(Strs.Text, NRec) > 0 then begin
                    pNote^.LastChange    := ModifiedToTBDate(NRec.modified);
                    pNote^.LastChangeGMT := UnixToDateTime(NRec.modified);
                    Strs.Free;
                    Strs := TStringList.Create;
                    Strs.text := StringReplace(NRec.Content,'\n',Lineending,[rfReplaceAll, rfIgnoreCase]);
                    if Strs.count > 1 then begin
                        Strs.SaveToFile(inttostr(PNote^.SID) + '.md');
                        Imp := TImportNotes.create;
                        Imp.Mode     := 'markdown';
                        Imp.Notebook := NRec.category;                              // What happens if the 'category' does not exist as a Notebook ???
                                                                                    // And further testing, a renamed NextCloud note does not get a new timestamp ?
                        Imp.LCDate   := pNote^.LastChange;
                        Imp.CrDate   := pNote^.CreateDate;
                        Imp.DestinationDir := NotesDir;
                        if not Imp.MDtoNote(Strs, pNote^.Title, pNote^.ID) then
                                Saydebug('ERROR, Nextcloud DownloadNotes, failed to convert from MD to Note SID=' + inttostr(pNote^.SID) + ' ID=' + pNote^.ID, True);
                        Imp.free;
		            end else begin
                        Saydebug('ERROR, Nextcloud DownloadNotes, cannot convert from empty MD SID=' + inttostr(pNote^.SID) + ' ID=' + pNote^.ID, True);
                        saydebug('[' + Strs.text + ']');
					end;
debugln('Nextcloud Download - Meta LCD is ' + pNote^.LastChange);
debugLn('Nextcloud Download LCD - ' + pNote^.LastChange);
debugln(' ----- ');
                end;
            end;
            if not FileExistsUTF8(NotesDir + pNote^.ID + '.note') then begin
                SayDebug('ERROR - NextCloud.DownloadNotes did not create ' + NotesDir + pNote^.ID + '.note');
                exit(false);
			end;
	  finally
            Strs.free;
	  end;
		// ToDo : if we get an error here and the note is not saved, must prevent it from appearing in RemoteManifest
        // else it will be treated as a DeleteRemote and deleted from NextCloud next time we sync !!!!!
        // maybe just remove the entry from MMD, that way, we might get it done next time ?
	end;
    result := true;
end;

function TNextCloudSync.DeleteNote(const ID: string; const ExistRev: integer): boolean;
var
    Client: TFPHttpClient;              { yes, definitly needed here. }
    STRL  : TStringList;
begin
    Saydebug('Deleting note ' + ID);
    Result := False;
    Client := TFPHttpClient.Create(nil);
    STRL := TStringList.Create;
    Client.UserName := USER;
    Client.Password := Password;
    Client.AllowRedirect := true;
    try
        try
            Client.Delete(RemoteAddress + '/' + URL_SUFFIX + '/' + ID, STRL);
            if Client.ResponseStatusCode = 200
                then Result := True
            else begin
                ErrorString := 'Network Error, Response Code ' + inttostr(Client.ResponseStatusCode);
                Saydebug(ErrorString, True);
                Saydebug('Called ' + RemoteAddress + '/' + URL_SUFFIX + '/' + ID, True);
			end;
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
end;

function TNextCloudSync.UploadNotes(const Uploads: TNoteInfoList): boolean;
var
    I, SID : integer;
    NRec : TNoteRec;
    Notebook : string;
begin
    result := true;
    for I := 0 to Uploads.Count -1 do begin
        if UpLoads[i]^.Action in [SyUpLoadNew, SyUpLoadEdit] then begin
            if IsTemplate(UpLoads[i]^.ID, Notebook) then begin
                UpLoads[i]^.Action := SyTemplate;                   // we do not sync Templates to NextCloud
                continue;
			end;
            if Notebook <> '' then Notebook := StringToJSONString(Notebook);
			if  UpLoads[i]^.Action = SyUpLoadNew then
                SID := 0
            else SID :=  UpLoads[i]^.SID;

debugln('NextCloud Uploadnotes - Uploading  ' + UpLoads[i]^.ID + ' SID=' + inttostr(pNoteInfo(UpLoads[i])^.SID) );
debugln('Nextcloud Uploadnotes - old LCD ' + UpLoads[i]^.LastChange);

            if PushUpNote(UpLoads[i]^.ID, Notebook, NRec, SID) > 0 then begin;
                UpLoads[i]^.SID := NRec.SID;
                UpLoads[i]^.LastChange := ModifiedToTBDate(NRec.modified);

debugln('Nextcloud Uploadnotes - new LCD ' + ModifiedToTBDate(NRec.modified));
debugln(' ----- ');

			end else
                exit(false);
		end;
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




procedure TNextCloudSync.ReadRemoteManifest(NoteMeta : TNoteInfoList);
// We iterate over the remote manifest, for each item look for an entry in the MMD.
// If its present in both MMD and the RemoteManifest but not in note dir, its a
// SyDeleteRemote. Esle, if found, decide about Action, SyNothing, SyDownload.
var
    Doc : TXMLDocument;
    Node : TDOMNode;
    NodeList : TDOMNodeList;
    SID, ID, CrDate, LCDate : string;
    i : integer;
    pNote : PNoteInfo;
begin
    if FileExists(ConfigDir + 'manifest.xml-remote') then begin
        try
            try
                ReadXMLFile(Doc, ConfigDir + 'manifest.xml-remote');
                //Node := Doc.DocumentElement.FindNode('server-id');      // ToDo, check server ID matches, very bad things could happen ....
                //ServerID := Node.FirstChild.NodeValue;
                NodeList := Doc.DocumentElement.ChildNodes;
	            if assigned(NodeList) then begin
	                for i := 0 to NodeList.Count-1 do begin
	                    SID := ''; ID := ''; CrDate := '';
	                    Node := NodeList.Item[i].Attributes.GetNamedItem('sid');
	                    SID := Node.NodeValue;
	                    Node := NodeList.Item[i].Attributes.GetNamedItem('id');
	                    ID := Node.NodeValue;
	                    Node := NodeList.Item[i].Attributes.GetNamedItem('create-date');
	                    CrDate := Node.NodeValue;
	                    Node := NodeList.Item[i].Attributes.GetNamedItem('last-change-date');
	                    LCDate := Node.NodeValue;

debugln('ReadRemoteMainfest Looking at ID=' + ID + ' SID=' + SID + ' LCD=' + LCDate);

	                    if (ID <> '') and (SID <> '') and (CrDate <> '') and (LCDate <> '') then begin
	                        pNote := NoteMeta.FindSID(strtoint(SID));
	                        if pNote <> nil then begin
	                            pNote^.CreateDate := CrDate;

debugln('MainMeta Action Set is ' + copy(ID, 1, 8) + ' ' + NoteMeta.ActionName(pNote^.Action));
debugln('MainMeta   LCD is      ' + copy(ID, 1, 8) + ' ' + pNote^.LastChange);
debugln('Remote Manifest LCD is ' + copy(ID, 1, 8) + ' ' + LCDate);

                                // If here, its present in both NoteMeta and RemoteManifest
                                if FileExistsUTF8(NotesDir + ID + '.note') then
		                                if pNote^.LastChange = LCDate then
		                                    pNote^.Action := SyNothing
		                                else pNote^.Action := SyDownLoad
                                else pNote^.Action := SyDeleteRemote;

debugln('MainMeta Action Set is '  + copy(ID, 1, 8) + NoteMeta.ActionName(pNote^.Action));

	                            pNote^.LastChangeGMT := GetGMTFromStr(pNote^.LastChange);
	                            pNote^.ID := ID;
						    end;
						end else SayDebug('ERROR, bad data in remote mainfest ');
	                end;
	            end;
            except on EXMLReadError do
                ErrorString := 'Error reading the remote manifest, ' + ConfigDir + 'manifest.xml-remote';
            end;
        finally
            Doc.free;
        end;
    end;
    for I := 0 to NoteMeta.count -1 do begin            // Remaining notes (or all in a Join) are SyDownloads
        if (NoteMeta[i]^.ID = '') and (NoteMeta[i]^.SID > 0) then begin
            NoteMeta[i]^.ID := MakeGUID();
            NoteMeta[i]^.Action := SyDownload;
		end;
    end;
end;

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
    NRec : TNoteRec;
begin
    CreateGUID(GUID);
    ServerID := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
    Result := PostNewNote(KEYNOTETITLE, '', KEYNOTETEXT + KEYNOTEUUID + ServerID, NRec) > 0;
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

function ModifiedToTBDate(Modified: longint): string;
var
    DT : TDateTime;
    Off : longint;
    Res : string;
begin
    // modified time is in GMT or UTC. So, need to correct for time zone.
    Off := GetLocalTimeOffset();                    // local time offset in minutes
    DT := UnixToDateTime(Modified - Off*60);        // Conv Off minutes to seconds and add it
    Result := FormatDateTime('YYYY-MM-DD', DT) + 'T'
        + FormatDateTime('hh:mm:ss.0000000"', DT);
    if (Off div -60) >= 0 then Res := '+'
    else Res := '-';
    if abs(Off div -60) < 10 then Res := Res + '0';
    Res := Res + inttostr(abs(Off div -60)) + ':';
        if (Off mod 60) = 0 then
            Res := res + '00'
    else Res := Res + inttostr(abs(Off mod 60));
    Result :=  Result + Res;                        // LastChange
end;

function TNextCloudSync.JData2NoteList(const NoteMeta: TNoteInfoList; jItm : TJSONData) : boolean;
var
    jObj : TJSONObject; jBool : TJSONBoolean; PNote : PNoteInfo; jNumb : TJSONNumber;
    jString : TJSONString;
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
                pNote^.LastChange := ModifiedToTBDate(jNumb.AsInteger);             // LastChange
			end;
            pNote^.CreateDate := pNote^.LastChange;                                 // CreateDate, will update from local note if possible
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


{ Sends a note up to NextCloud, default is a new note and a Post.
  if SID > 0, its to replace an existing note of that SID }
function TNextCloudSync.PushUpNote(ID, Notebook : string; out NRec : TNoteRec; SID : integer) : longint;
var
  STL : TStringList;
  Title : string = '';
  ENotes : TExportCommon;
begin
    ENotes := TExportCommon.Create;
    STL := TStringList.Create;
    try
    	ENotes.NotesDir := NotesDir;
        ENotes.GetMDcontent(ID, STL);
        if STL.Count > 1 then begin
            Title := StringToJSONString(STL[0]);
            Stl.Delete(0);
            Stl.Delete(0);
	    end;
        // STL.SaveToFile(Title + '.md');
        Result := PostNewNote(Title, Notebook, StringToJSONString(STL.Text), NRec, SID);
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
    SayDebug('---------- JSON ------------', true);
    SayDebug(jData.FormatJSON, true);
    SayDebug('----------------------------', true);
    JData.Free;
end;

                // Ret the (NextCloud) ID if its found in the passed (json) string, 0 else.
function TNextCloudSync.ExtractJID(data : string; out NRec : TNoteRec) : longint;
var
    JData : TJSONData;
    JObject : TJSONObject;
    JNumb : TJSONNumber;
    jStr : TJSONString;
begin
    try
	    try
            JData := GetJSON(Data);                         // requires a free
            JObject := TJSONObject(jData);                  // does not require a free
            // Result := JObject.Get('id');                 // will raise exceptions if not present, better to use Find
            NRec.SID := 0;
            NRec.modified:=0;
            if JObject.Find('id', jNumb) then
                NRec.SID := jNumb.AsInteger;
            if JObject.Find('modified', jNumb) then
                NRec.modified := jNumb.AsInteger;
            if JObject.Find('category', jStr) then
                NRec.category := JStr.AsString
            else NRec.category := '';                       // we may or may not have a category
            if jObject.Find('content', Jstr) then
                NRec.content:= jStr.AsString
            else NRec.content := '';                        // same with content
            result := NRec.SID;
	    //except on  E: EVariantTypeCastError ....
        except on E:Exception do Result := 0;               // Invalid JSON or ID not present
	    end;
	finally
        JData.Free;
	end;
end;


// Returns the NextCloud assigned NoteID on success, 0 on failure, must set a regional ErrorMessage
function TNextCloudSync.PostNewNote(Title, Notebook, Content : String; out NRec : TNoteRec; SID : integer= 0) : longint;
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
    if Notebook = '' then
        client.RequestBody := TRawByteStringStream.Create('{ "title": "'
                            + Title + '", "content": "' + content + '"}')
    else client.RequestBody := TRawByteStringStream.Create('{ "title": "' + Title
                    + '",  "category": "' + Notebook + '", "content": "' + content + '"}');
    //Saydebug('NextCloud POST Request Body [' + client.RequestBody + ']');
    Response := TStringStream.Create('');
    try
        try
            if SID = 0 then begin
            saydebug('About to Post');
            client.Post(RemoteAddress + URL_SUFFIX, Response);  // don't use FormPost, it messes with the Content-Type value
            end else  begin
                saydebug('About to PUT');
                client.Put(RemoteAddress + URL_SUFFIX + '/' + inttostr(SID), Response);
            end;
            // saydebug('Posted');
            if Client.ResponseStatusCode = 200 then begin
                //if DebugMode then DumpJSON(Response.DataString);
                Result := ExtractJID(Response.DataString, NRec);
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

function TNextCloudSync.IsTemplate(const ID : string; out Notebook : String) : boolean;
var
    Doc : TXMLDocument;
	Node : TDOMNode;
    J : integer;
begin
    result := False;            // This is returned if there is not 'tags' present (or if the note is not present !)
    Notebook := '';
  	if FileExistsUTF8(NotesDir + ID + '.note') then begin
  	    try
	        try
	            ReadXMLFile(Doc, NotesDir + ID + '.note');
                Node := Doc.DocumentElement.FindNode('tags');
                if Assigned(Node) then begin
                    for J := 0 to Node.ChildNodes.Count-1 do        // forgetting notebooks, its a Template
                        if UTF8Pos('system:template', Node.ChildNodes.Item[J].TextContent) > 0 then
                                exit(True);
                    for J := 0 to Node.ChildNodes.Count-1 do
                        if UTF8pos('system:notebook', Node.ChildNodes.Item[J].TextContent) > 0 then begin
                            Notebook := UTF8Copy(Node.ChildNodes.Item[J].TextContent, 17, 1000);
                            exit(False);                             // We are interested in only the first one we find.
                        end;
				end;
            except 	on E: Exception do SayDebug('NextCloud IsTemplate - XML ERROR reading ' + NotesDir + ID + '.note' + ' ' + E.Message);
            end;
  	    finally
      	    Doc.free;
  	    end;
    end else SayDebug('NextCloud IsTemplate - Error, Cannot locate note ID=' + NotesDir + ID + '.note');
end;

procedure TNextCloudSync.SayDebug(st: string; Always : boolean = false);
begin
    if not (DebugMode or Always) then exit;
    {$ifdef LCL}Debugln{$else}writeln{$endif}(St);
end;


end.

