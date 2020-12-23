unit nextcloud;

{$mode objfpc}{$H+}

interface

uses
        Classes, SysUtils, export_notes, syncutils;

type

  { TNextCloudNotes }

  TNextCloudNotes = class
    private
        ENotes : TExportNote;
        procedure DumpJSON(const St: string);
        function ExportAll(): boolean;
        function ExtractJID(data: string): longint;
        function PostNewNote(Title, Content: String): longint;
        function PushUpNote(SimpleID: string): boolean;
        function Downloader(URL : string; SomeStrings : TStringList) : boolean;
        function GetNewNotes(): boolean;
    public
        Debug : boolean;                      // Writeln lots of stuff as we go.
        NoNet : boolean;                      // Don't actually do network stuff, maybe write files.
        ResponseCode : longint;                   // HTTP response code, 200 is OK
        SyncAction : string;                  // one of 'PUSHUP', 'PULLDOWN', 'DELETEALL', 'SYNC'
        TheURL : string;
        NoteDir : string;
        function Execute() : boolean;               // The go and do it function.
        constructor Create;
        destructor Destroy; override;
  end;

implementation

{ TNextCloudNotes }
uses
	    {$if (FPC_FULLVERSION=30200)}
	    opensslsockets,                 // only available in FPC320 and later
	    {$endif}
	    fphttpclient, httpprotocol, {$ifdef LCL}lazlogger, {$endif}
	    fpopenssl, TypInfo, fpjson, jsonparser;


const ID = '5D7471B6-DC00-4A03-B101-91EC476D7186';
  USER='dbannon';
  PW='DavosOldPW';


function TNextCloudNotes.Execute(): boolean;
begin
    //Result := PushUpNote(ID);
    case SyncAction of
        'PUSHUP'   : Result := ExportAll();
        'PULLDOWN' : GetNewNotes();
    else
        result := false;
	end;
end;

constructor TNextCloudNotes.Create;
begin
    ENotes := nil;
    ResponseCode := 0;
    Debug := True;
end;

destructor TNextCloudNotes.Destroy;
begin
    freeandnil(ENotes);
	inherited Destroy;
end;

// ----------------  D O W N L O A D ----------------

function TNextCloudNotes.Downloader(URL : string; SomeStrings : TStringList) : boolean;
var
    Client: TFPHttpClient;
begin
    // Windows can be made work with this if we push out ssl dll - see DownloaderSSL local project
    //InitSSLInterface;
    Client := TFPHttpClient.Create(nil);
    Client.UserName:=USER;
    Client.Password:=PW;
    Client.AllowRedirect := true;
    try
        try
            Client.Get(URL, SomeStrings);
        except
            on E: EInOutError do begin
                {$ifdef LCL}Debugln{$else}writeln{$endif}('InOutError ' + E.Message);         // danger - writeln
                exit(false);
                end;
            on E: ESSL do begin
                {$ifdef LCL}Debugln{$else}writeln{$endif}('SSLError ' + E.Message);         // danger - writeln
                exit(false);
                end;
            on E: Exception do begin
                {$ifdef LCL}Debugln{$else}writeln{$endif}('Something bad happened ' + E.Message);         // danger - writeln
                {$ifdef LCL}Debugln{$else}writeln{$endif}('URL was ' + URL);
                exit(false);
                end;
        end;
    finally
        Client.Free;
    end;
    //ErrorMsg := '';
    result := true;
end;

function TNextCloudNotes.GetNewNotes({const NoteMeta: TNoteInfoList; const GetLCD : boolean}): boolean;
var
    SomeStrings : TStringList;
    jData : TJSONData;
    jItem : TJSONData;
    AnID, ChangeDate : longint;
    jError : boolean;
    jObject : TJSONObject;

    object_name, field_name, field_value, object_type, object_items: String;
    i : integer;
begin
    if NoNet then exit(true);
    SomeStrings := TStringList.Create;
    if Downloader(TheURL+'?exclude=content,title',  SomeStrings) then begin
        {$ifdef LCL}Debugln{$else}writeln{$endif}('That looks good');
        dumpJSON(SomeStrings.text);
        jData := GetJson(SomeStrings.text);
        for i := 0 to JData.Count -1 do begin
                jItem := jData.Items[i];
                JObject := TJSONObject(jItem);
                if not JObject.Get('error') then begin
                    AnID := JObject.get('id');
                    ChangeDate := jObject.get('modified');
                    writeln('Item=' + inttostr(AnID) + ' modified=' + inttostr(ChangeDate));
				end;
		end;
        jData.free;
	end else
        {$ifdef LCL}Debugln{$else}writeln{$endif}('Nope, failed');
    {$ifdef LCL}Debugln{$else}writeln{$endif}('transnet, Called getNewNotes');
    SomeStrings.free;
    exit(true);
end;




// -------------------- U P L O A D -----------------------

function TNextCloudNotes.PushUpNote(SimpleID : string) : boolean;
var
  STL : TStringList;
  Title : string = '';
begin
    ENotes := TExportNote.Create;
    STL := TStringList.Create;
    ENotes.NoteDir := NoteDir;
    ENotes.GetMDcontent(SimpleID, STL);
    if STL.Count > 1 then begin
        Title := StringToJSONString(STL[0]);
        Stl.Delete(0);
        Stl.Delete(0);
	end;
    if NoNet then
        STL.SaveToFile('export.md')      // over writes all except last one in set.
    else
        ResponseCode := PostNewNote(Title, StringToJSONString(STL.Text));
    STL.Free;
    freeandnil(ENotes);
    Result := True;
end;

function TNextCloudNotes.ExportAll(): boolean;
var
    Info : TSearchRec;
begin
    if FindFirst(NoteDir + '*.note', faAnyFile, Info) = 0 then
        try
            repeat
	            PushUpNote(copy(Info.Name, 1, length(Info.name) - 5));
            until FindNext(Info) <> 0;
        finally
            FindClose(Info);
        end
    else begin
        // ErrorMessage := 'ERROR : No notes were found in ' + NoteDir;
        {$ifdef LCL}Debugln{$else}writeln{$endif}('ERROR : No notes were found in ' + NoteDir);
        exit(false);
	end;
	result := true;
end;

procedure TNextCloudNotes.DumpJSON(const St : string);
var
    jData : TJSONData;
begin
    JData := GetJSON(St);
    {$ifdef LCL}Debugln{$else}writeln{$endif}('---------- JSON ------------');
    {$ifdef LCL}Debugln{$else}writeln{$endif}(jData.FormatJSON);
    {$ifdef LCL}Debugln{$else}writeln{$endif}('----------------------------');
    JData.Free;
end;

                // Ret the (NextCloud) ID if its found in the passed (json) string, 0 else.
function TNextCloudNotes.ExtractJID(data : string) : longint;
var
    JData : TJSONData;
    JObject : TJSONObject;
begin
    try
	    try
            JData := GetJSON(Data);         // requires a free
            JObject := TJSONObject(jData);  // apparently does not require a free ??
            Result := JObject.Get('id');    // will raise exceptions if not present
	    //except on  E: EVariantTypeCastError do Result := 0;
        except on E:Exception do Result := 0;   // Invalid JSON or ID not present
	    end;
	finally
        JData.Free;
	end;
end;


// Returns the NextCloud assigned NoteID on success, 0 on failure, must set a regional ErrorMessage
function TNextCloudNotes.PostNewNote(Title, Content : String) : longint;
var
    Client: TFPHttpClient;
    Response : TStringStream;
    //Response : string;
    //Params : string = '{"title": "Some New Note","content": "This is awesome stuff in the new note"}';
begin
    Result := 0;
    Client := TFPHttpClient.Create(nil);
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    Client.AddHeader('Content-Type','application/json; charset=UTF-8');
    Client.AddHeader('Accept', 'application/json');
    Client.AllowRedirect := true;
    Client.UserName:=USER;
    Client.Password:=PW;
    client.RequestBody := TRawByteStringStream.Create('{ "title": "'
                            + Title + '", "content": "' + content + '"}');
    Response := TStringStream.Create('');
    try
        try
            client.Post(TheURL, Response);  // don't use FormPost, it messes with the Content-Type value
            if Client.ResponseStatusCode = 200 then begin
                if Debug then DumpJSON(Response.DataString);
                Result := ExtractJID(Response.DataString);
                if Debug then writeln('ID is ' + inttostr(Result));
			end else begin
                // Set some error code here.
                if Debug then begin
			        {$ifdef LCL}Debugln{$else}writeln{$endif}('Response Code is ' + inttostr(Client.ResponseStatusCode));
                    {$ifdef LCL}Debugln{$else}writeln{$endif}(Response.DataString);
				end;
			end;
		except on E:Exception do
                {$ifdef LCL}Debugln{$else}writeln{$endif}('TransNext.PostNewNote bad things happened : ' + E.Message);
                // Set some sort of error message here.
        end;
    finally
        Client.RequestBody.Free;
        Client.Free;
        Response.Free;
    end;
end;


end.

