unit transnet;

{ A unit that does the file transfer side of a NetSync operation.

  *
  *  See attached licence file.
}
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Trans, SyncUtils, ssockets;

type               { ----------- TNetSync ------------ }


{ TNetSync }

TNetSync = Class(TTomboyTrans)
    private
			function Downloader(URL: string; SomeStrings: TStringList): boolean;
			procedure DumpJSON(const St: string);
			procedure DumpResponse(const St: string);
			function HTTPGet(URL: String): String;
			function HTTPPost(URL: String; Params: TStringList): String;
			function HTTPPost(URL, ParamSt: String): String;
			function MakeOAuthConnection(const URL: string): boolean;

    public
        function TestTransport(const WriteNewServerID : boolean = False): TSyncAvailable; override;
                            { if we do not yet have a working sync connection, we will need to do the
                              OAuth stuff, else we will just make sure its talking to us, somehow. }
        function SetTransport(): TSyncAvailable; override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
        //function SetRemoteRepo(ManFile : string = '') : boolean; override;
 end;


implementation

{ TNetSync }



uses
    {$if (FPC_FULLVERSION=30200)}
    opensslsockets,                 // only available in FPC320 and later
    {$endif}
    fphttpclient, httpprotocol,
    fpopenssl, fpjson, jsonparser, oauth{$ifdef LCL}, LazLogger{$endif};

const URL_LEFT='http://localhost/nextcloud/index.php/apps/grauphel/';

  function TNetSync.TestTransport(const WriteNewServerID : boolean = False): TSyncAvailable;
begin
    Result := SyncReady;
end;

function TNetSync.HTTPGet(URL : String) : String;
var
    Client: TFPHttpClient;
begin
    writeln('HTTPGET sending ' + URL);
    Client := TFPHttpClient.Create(nil);
	//Client.OnGetSocketHandler := @HttpClientGetSocketHandler;
	Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
	Client.AllowRedirect := true;
    try
        try
            Result := Client.Get(URL);
        except
            on E:Exception do begin
                writeln('Error, ' + E.Message);
                Result :='';
            end;
        end;
	finally
        Client.Free;
	end;
end;

            // Returns JSON on success, '' on failure, must set a regional ErrorMessage
function TNetSync.HTTPPost(URL : String; Params : TStringList) : String;
var
  Client: TFPHttpClient;
  Response : TStringStream;
begin
    Client := TFPHttpClient.Create(nil);
    // Client.OnGetSocketHandler := @HttpClientGetSocketHandler;       // not needed with fpc320
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    Client.AllowRedirect := true;
    Response := TStringStream.Create('');
    try
	    try
            writeln('Posting to ' + URL);
	        Client.FormPost(URL, Params.text, Response);
            //Client.FormPost(URL, Params, Response);
	        Result := Response.DataString;
	        writeln('HTTPPost ['+ Result + ']');
	    except on E:Exception do begin
	        Result := '';
            writeln('Transnet.HTTPPost bad things happened');
            writeln(E.Message);
	        end;
	    end;
	finally
        Client.Free;
        Response.Free;
	end;
end;


procedure TNetSync.DumpJSON(const St : string);
var
    jData : TJSONData;
begin
    JData := GetJSON(St);
    writeln('---------- JSON ------------');
    writeln(jData.FormatJSON);
    writeln('----------------------------');
end;

procedure TNetSync.DumpResponse(const St : string);
var
    St2 : string = '';
    St3 : string;
    ch : char = ' ';
    i : integer = 0;
begin
    St3 := httpdecode(St);
    for ch in St3 do begin
        if ch = '&' then St2 := St2 + #10
        else St2 := St2 + Ch;
        inc(i);
    end;
    writeln('---------- RESPONSE ------------');
    writeln(St2);
    writeln('----------------------------' + inttostr(i));
end;


// Returns JSON on success, '' on failure, must set a regional ErrorMessage
function TNetSync.HTTPPost(URL, ParamSt : String) : String;
var
    Client: TFPHttpClient;
    Response : TStringStream;

begin
    Client := TFPHttpClient.Create(nil);
    // Client.OnGetSocketHandler := @HttpClientGetSocketHandler;       // not needed with fpc320
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    Client.AllowRedirect := true;
    Response := TStringStream.Create('');
    try
        try
            writeln('Posting to ' + URL);
            writeln('Params ' + ParamSt);
            Client.FormPost(URL,  Paramst, Response);
            Result := Response.DataString;
            DumpResponse(Result);
        except on E:Exception do begin
                Result := '';
                writeln('Transnet.HTTPPost bad things happened');
                writeln(E.Message);
            end;
        end;
    finally
        Client.Free;
        Response.Free;
    end;
end;

                // This is my generic downloader, its a GET of course, only here for reference
function TNetSync.Downloader(URL : string; SomeStrings : TStringList) : boolean;
var
    Client: TFPHttpClient;
begin
    // Windows can be made work with this if we push out ssl dll - see DownloaderSSL local project
    //InitSSLInterface;
    Client := TFPHttpClient.Create(nil);
    {$if (FPC_FULLVERSION<30200)}
    //Client.OnGetSocketHandler := @HttpClientGetSocketHandler;
    {$endif}
    //Client.OnDataReceived := @DataReceived;
    Client.AllowRedirect := true;
    try
        try
            Client.Get(URL, SomeStrings);
        except
            on E: EInOutError do begin
                writeln('InOutError ' + E.Message);         // danger - writeln
                exit(false);
                end;
            on E: ESSL do begin
                writeln('SSLError ' + E.Message);         // danger - writeln
                exit(false);
                end;
            on E: Exception do begin
                writeln('Something bad happened ' + E.Message);         // danger - writeln
                writeln('URL was ' + URL);
                exit(false);
                end;
        end;
    finally
        Client.Free;
    end;
    //ErrorMsg := '';
    result := true;
end;

function TNetSync.MakeOAuthConnection(const URL : string) : boolean;
var
    Response : string;
    jData : TJSONData;
    jObject : TJSONObject;
    OAuth : TOAuth;
begin
    Response := HTTPGet(URL_LEFT + 'api/1.0');
    if Response = '' then exit(False);
    OAuth := TOAuth.Create;
    try
        jData :=  GetJSON(Response);
        jObject := TJSONObject(jData);
        OAuth.requestTokenUrl := jObject.Get('oauth_request_token_url');
        OAuth.authorizeUrl    := jObject.Get('oauth_authorize_url');
        OAuth.accessTokenUrl  := jObject.Get('oauth_access_token_url');
        oauth.SetReqHeader();
        //Response := HTTPPost(OAuth.RequestTokenUrl,  oAuth.ParamList);
        Response := HTTPPost(OAuth.RequestTokenUrl,  oAuth.ParamReqBodyString);
        //Response := HTTPGET(OAuth.RequestTokenUrl + OAuth.ParamQueryString);   // grauphl does not accept this in query mode.
	finally
        OAuth.free;          // will need to save the access Token and secret somehow.
	end;
end;

function TNetSync.SetTransport(): TSyncAvailable;
begin
    // for now, we assume we do not have a connection, later must check.
    MakeOAuthConnection(URL_LEFT);

        Result := SyncNoRemoteMan;
end;

function TNetSync.GetNewNotes(const NoteMeta: TNoteInfoList; const GetLCD : boolean): boolean;
begin
      {$ifdef LCL}Debugln{$else}writeln{$endif}('transnet, Called getNewNotes');
      MakeOAuthConnection(URL_LEFT);
      exit(true);
end;

function TNetSync.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
begin
     result := True;
end;

function TNetSync.DeleteNote(const ID: string; const ExistRev : integer): boolean;
begin
     result := True;
end;

function TNetSync.UploadNotes(const Uploads: TStringList): boolean;
begin
    result := True;
end;

function TNetSync.DoRemoteManifest(const RemoteManifest: string): boolean;
begin
    result := True;
end;

function TNetSync.DownLoadNote(const ID: string; const RevNo: Integer): string;
begin
    // Download indicated note and call it ConfigDir + 'remote.note';
    result := ConfigDir + 'remote.note';
end;

{function TNetSync.SetRemoteRepo(ManFile: string = ''): boolean;
begin

end; }

end.

