unit transnext;

{ A unit that does the file transfer side of a NextSync operation.
  Depends on external (Ruby) modules, as yet unimplemented.
  *
  *  See attached licence file.
}
{$mode objfpc}{$H+}

interface

uses
    Classes, LazLogger, SysUtils, Trans, SyncUtils, Dialogs, fpjson, jsonparser, oauth;

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
        function IDLooksOK() : boolean; Override;
        function getPrefix(): string; Override;
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

function TNextSync.getPrefix() : string;
begin
  Result := 'nc';
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
    debugln(res);
    FreeAndNil(p);

    if (res = '') then begin ErrorString :=  'Next-TestTransport: Unable to et initial data'; exit(SyncBadError); end;

    ok := true;
    ErrorString := '';

    try
       json := GetJSON(res);
       debugln('JSON= '+json.AsJSON);

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
    debugln(res);
    FreeAndNil(p);
    ok := true;
    ErrorString := '';
    try
       json := GetJSON(res);
       debugln('JSON OAUTH = ' + json.FormatJSON());

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

    if not IDLooksOK() then begin
        ErrorString := 'Invalid ServerID '+ServerID;
        exit(SyncMismatch);
    end;

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
  json: TJSONData;
  jObject : TJSONObject;
  jnotes : TJSONArray;
  p : TStrings;
  ok : boolean;
  nbnotes,i,j : integer;
  NoteInfo : PNoteInfo;
  d : double;
begin
    WriteLn('Next-GetNotes');

    // HTTP REQUETS
    res := getParam('URLNOTES');
    debugln(res);
    p := TstringList.Create();
    oauth.BaseParams(p,true);
    p.Add('include_notes');
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
       debugln('JSON NOTES = '+json.AsJSON);
       jObject := TJSONObject(json);
       setParam('REVISION',jObject.Get('latest-sync-revision'));
       jnotes :=  jObject.Get('notes',jnotes);
    except on E:Exception do begin
       ErrorString := E.message;
       debugln(ErrorString);
       ok:= false;
       end;
    end;

    if (not ok) then begin ErrorString :=  'Next-GetNotes: '+ErrorString; debugln(ErrorString); exit(false); end;

    nbnotes := jnotes.Count;
    debugln(Format('Nb notes %d',[nbnotes]));

    i:=0;
    while(i<nbnotes) do
    begin
       new(NoteInfo);
       ok :=true;

       try
          json := jnotes.Items[i];
          jObject := TJSONObject(json);
          debugln(Format('Note %d',[i]));
          debugln(jObject.AsJSON);
          debugln(json.FormatJSON());

          NoteInfo^.Action:=SyUnset;
          NoteInfo^.ID := jObject.Get('guid');
          NoteInfo^.Rev := jObject.Get('last-sync-revision');

          NoteInfo^.CreateDate:=jObject.Get('create-date','');
          if NoteInfo^.CreateDate <> '' then
             NoteInfo^.CreateDateGMT := GetGMTFromStr(NoteInfo^.CreateDate);
          NoteInfo^.LastChange:=jObject.Get('last-change-date','');
          if NoteInfo^.LastChange <> '' then
             NoteInfo^.LastChangeGMT := GetGMTFromStr(NoteInfo^.LastChange);
          NoteInfo^.LastMetaChange:=jObject.Get('last-metadata-change-date','');
          if NoteInfo^.LastMetaChange <> '' then
             NoteInfo^.LastMetaChangeGMT := GetGMTFromStr(NoteInfo^.LastMetaChange);

          d := StrToFloat(jObject.Get('note-content-version'));
          j := round(d*10);
          d := j * 0.1;
          NoteInfo^.Version := Format('%d',[d]);

          NoteInfo^.Deleted := false;
          NoteInfo^.Title := jObject.Get('title');
          NoteInfo^.Content := jObject.Get('note-content');

          NoteInfo^.OpenOnStartup := (jObject.Get('open-on-startup') = 'true');
          NoteInfo^.Pinned := (jObject.Get('pinned') = 'true');

          NoteInfo^.CursorPosition := StrToInt(jObject.Get('cursor-position'));
          NoteInfo^.SelectBoundPosition := StrToInt(jObject.Get('selection-bound-position'));
          NoteInfo^.Width := StrToInt(jObject.Get('width'));
          NoteInfo^.Height := StrToInt(jObject.Get('height'));
          NoteInfo^.X := StrToInt(jObject.Get('x'));
          NoteInfo^.Y := StrToInt(jObject.Get('y'));

          NoteInfo^.Source := json.AsJSON;

       except on E:Exception do begin ok := false; debugln(E.message); end;
       end;

       if(ok) then NoteMeta.Add(NoteInfo)
       else FreeAndNil(NoteInfo);

       i := i+1;
    end;

    debugln('done');
    FreeAndNil(jnotes);

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

function TNextSync.IDLooksOK() : boolean;
var
  n,m : Integer;
begin
    n := pos('-', ServerID);
    m := pos('.', ServerID);

    if((m-n) <> 15) then exit(false);
    if((length(ServerID)-m) <> 8) then exit(false);
    result := True;
end;

end.

