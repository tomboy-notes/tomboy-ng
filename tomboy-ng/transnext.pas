unit transnext;

{ A unit that does the file transfer side of a NextSync operation.
  Depends on external (Ruby) modules, as yet unimplemented.
  *
  *  See attached licence file.
}
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Trans, SyncUtils, Dialogs, fpjson, jsonparser, oauth;

type               { ----------- TNextSync ------------ }


{ TNextSync }

TNextSync = Class(TTomboyTrans)
    private
        oauth: TOAuth;

    public
        constructor create;
        destructor Destroy;
        function TestTransport(const WriteNewServerID : boolean = False): TSyncAvailable; override;
        function SetTransport(): TSyncAvailable; override;
        function GetNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
        //function SetRemoteRepo(ManFile : string = '') : boolean; override;
 end;


implementation

{ TNextSync }

constructor TNextSync.create;
begin
    inherited Create;
    oauth := TOAuth.Create();
end;

destructor TNextSync.Destroy;
begin
    FreeAndNil(oauth);
    inherited Destroy;
end;


function TNextSync.TestTransport(const WriteNewServerID : boolean = False): TSyncAvailable;
var
  resturl,res : String;
  json : TJSONData;
  jObject : TJSONObject;
  p : TStrings;
  ok : boolean;
  ts : TSysCharSet;
begin
    WriteLn('Next-TestTransport');
    oauth.Token := getParam('TOKEN');
    oauth.TokenSecret := getParam('SECRET');
    oauth.Key := getParam('KEY');
    oauth.callbackUrl := '';

    // TESTING AUTH
    resturl := getParam('URL') + '/api/1.0/';
    p := TstringList.Create();
    oauth.BaseParams(p,true);
    oauth.ParamsSort(p);
    oauth.Sign(resturl, 'GET', p,oauth.TokenSecret);
    res := oauth.WebGet(resturl,p);
    writeln(res);
    FreeAndNil(p);

    if (res = '') then begin ErrorString :=  'Next-TestTransport: Unable to et initial data'; exit(SyncBadError); end;

    ok := true;
    ErrorString := '';

    try
       json := GetJSON(res);
       json := json.Items[4];
       jObject := TJSONObject(json);
       res := jObject.Get('api-ref');
       FreeAndNil(jObject);
    except on E:Exception do begin
       ErrorString := E.message;
       ok:= false;
       end;
    end;
    if (not ok) then begin ErrorString :=  'Next-TestTransport: '+ErrorString; exit(SyncBadError); end;
    if(length(res)<10) then begin ErrorString :=  'Next-TestTransport: Server returns invalid OAuth URLs '+ErrorString; exit(SyncBadError); end;

    // YEAH
    setParam('URLUSER',res);
    p := TstringList.Create();
    oauth.BaseParams(p,true);
    oauth.ParamsSort(p);
    oauth.Sign(res, 'GET', p,oauth.TokenSecret);
    res := oauth.WebGet(res,p);
    writeln(res);
    FreeAndNil(p);
    ok := true;
    ErrorString := '';
    try
       json := GetJSON(res);
       jObject := TJSONObject(json);
       setParam('REVISION',jObject.Get('latest-sync-revision'));
       setParam('SERVERID',jObject.Get('current-sync-guid'));
       FreeAndNil(jObject);
       json := GetJSON(res);
       json:= json.Items[3];
       jObject := TJSONObject(json);
       res := jObject.Get('api-ref');
       setParam('URLNOTES',res);
       FreeAndNil(jObject);
    except on E:Exception do begin
       ErrorString := E.message;
       ok:= false;
       end;
    end;
    if (not ok) then begin ErrorString :=  'Next-TestTransport: '+ErrorString; exit(SyncBadError); end;
    if(length(res)<10) then begin ErrorString :=  'Next-TestTransport: Server returns invalid OAuth URLs '+ErrorString; exit(SyncBadError); end;

    ServerID:= getParam('SERVERID');
    RemoteServerRev := StrToInt(getParam('REVISION'));

    ErrorString :=  '';
    Result := SyncReady;
end;

function TNextSync.SetTransport(): TSyncAvailable;
begin
	WriteLn('Next-SetTransport');
        Result := SyncReady;
end;

function TNextSync.GetNotes(const NoteMeta: TNoteInfoList; const GetLCD : boolean): boolean;
var
  res : String;
  json, jnote : TJSONData;
  jObject : TJSONObject;
  p : TStrings;
  ok : boolean;
begin
    WriteLn('Next-GetNotes');

    // HTTP REQUETS
    res := getParam('URLNOTES');
    p := TstringList.Create();
    oauth.BaseParams(p,true);
    p.Add('include-notes');
    p.Add('true');
    oauth.ParamsSort(p);
    oauth.Sign(res, 'GET', p,oauth.TokenSecret);
    res := oauth.WebGet(res,p);
    FreeAndNil(p);

    if (res = '') then begin ErrorString :=  'Next-GetNotes: Unable to et initial data'; exit(false); end;

    ok := true;
    ErrorString := '';
    try
       json := GetJSON(res);
       jObject := TJSONObject(json);
       setParam('REVISION',jObject.Get('latest-sync-revision'));
       FreeAndNil(jObject);
       json := GetJSON(res);
       writeln('JSON= '+json.AsString);
       json:= json.Items[1];
       writeln('JSON 2 = '+json.AsString);
    except on E:Exception do begin
       ErrorString := E.message;
       ok:= false;
       end;
    end;

    if (not ok) then begin ErrorString :=  'Next-GetNotes: '+ErrorString; exit(false); end;



    result := False;
end;

function TNextSync.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
begin
	WriteLn('Next-DownloadNotes');
     result := True;
end;

function TNextSync.DeleteNote(const ID: string; const ExistRev : integer): boolean;
begin
	WriteLn('Next-DeleteNote');
     result := True;
end;

function TNextSync.UploadNotes(const Uploads: TStringList): boolean;
begin
	WriteLn('Next-UploadNotes');
    result := True;
end;

function TNextSync.DoRemoteManifest(const RemoteManifest: string): boolean;
begin
	WriteLn('Next-DoRemoteManifest');
    result := True;
end;

function TNextSync.DownLoadNote(const ID: string; const RevNo: Integer): string;
begin
	WriteLn('Next-DownLoadNote');
    // Download indicated note and call it ConfigDir + 'remote.note';
    result := ConfigDir + 'remote.note';
end;

{function TNextSync.SetRemoteRepo(ManFile: string = ''): boolean;
begin

end; }

end.

