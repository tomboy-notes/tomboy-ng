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
        destructor Destroy; override;
        function TestTransport(): TSyncAvailable; override;
        function SetTransport(): TSyncAvailable; override;
        function GetNotes(const NoteMeta : TNoteInfoList) : boolean; override;
        function PushChanges(notes : TNoteInfoList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function IDLooksOK() : boolean; Override;
        function getPrefix(): string; Override;
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

function TNextSync.TestTransport(): TSyncAvailable;
var
  resturl,res : String;
  json : TJSONData;
  jObject : TJSONObject;
  p : TStrings;
  ok : boolean;
  sid : String;
  rev : integer;
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
       sid := jObject.Get('current-sync-guid');
       rev := StrToInt(jObject.Get('latest-sync-revision'));
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

    ServerID:= sid;
    ServerRev := rev;

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

function TNextSync.GetNotes(const NoteMeta: TNoteInfoList): boolean;
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

    if NoteMeta = Nil then begin
        ErrorString := 'Passed an uncreated list to GetNotes()';
        exit(False);
    end;

    // HTTP REQUEST
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

    if (res = '') then begin ErrorString :=  'Next-GetNotes: Unable to get initial data'; exit(false); end;

    ok := true;
    ErrorString := '';
    try
       json := GetJSON(res);
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

          NoteInfo^.Action:=SynUnset;
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

          NoteInfo^.OpenOnStartup := (jObject.Get('open-on-startup').ToLower = 'true');
          NoteInfo^.Pinned := (jObject.Get('pinned').ToLower = 'true');

          NoteInfo^.CursorPosition := StrToInt(jObject.Get('cursor-position'));
          NoteInfo^.SelectBoundPosition := StrToInt(jObject.Get('selection-bound-position'));
          NoteInfo^.Width := StrToInt(jObject.Get('width'));
          NoteInfo^.Height := StrToInt(jObject.Get('height'));
          NoteInfo^.X := StrToInt(jObject.Get('x'));
          NoteInfo^.Y := StrToInt(jObject.Get('y'));

          //NoteInfo^.Source := json.AsJSON;

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

function TNextSync.PushChanges(notes : TNoteInfoList) : boolean;
var
    i : integer;
    note : PNoteInfo;
    json, res : String;
    p : TStrings;
begin
    json := '( "latest-sync-revision": '+IntToStr(ServerRev+1)+', "note-changes": [';

    for i := 0 to notes.Count -1 do
    begin
       note := notes.Items[i];
       if note^.Action = SynDeleteRemote
       then json := json + '( "guid": "' + note^.ID + '", "command": "delete" }'
       else begin
           json := json + '( "guid": "' + note^.ID + '",';
           json := json + ' "title": "' + StringReplace(note^.Title,'"','\"',[rfReplaceAll]) + '",';
           json := json + ' "note-content": "' + StringReplace(note^.Content,'"','\"',[rfReplaceAll]) + '",';
           json := json + ' "note-content-version": "' + note^.Version + '",';
           json := json + ' "last-change-date": "' + note^.LastChange + '",';
           json := json + ' "last-metadata-change-date": "' + note^.LastMetaChange + '",';
           json := json + ' "create-date": "' + note^.CreateDate + '",';
           json := json + ' "open-on-startup": "' + BoolToStr(note^.OpenOnStartup) + '",';
           json := json + ' "pinned": "' + BoolToStr(note^.Pinned) + '",';
           json := json + ' "x": "' + IntToStr(note^.X) + '",';
           json := json + ' "y": "' + IntToStr(note^.Y) + '",';
           json := json + ' "height": "' + IntToStr(note^.Height) + '",';
           json := json + ' "width": "' + IntToStr(note^.Width) + '",';
           json := json + ' "width": "' + IntToStr(note^.Width) + '",';
           json := json + ' "selection-bound-position": "' + IntToStr(note^.SelectBoundPosition) + '",';
           json := json + ' "cursor-position": "' + IntToStr(note^.CursorPosition) + '" }';
       end;
       if(i<notes.Count -1 ) then json := json + ', ';
    end;
    json := json + ' ] } ';

    // HTTP REQUEST
    res := getParam('URLNOTES');
    debugln(res);
    p := TstringList.Create();
    oauth.BaseParams(p,true);

    oauth.ParamsSort(p);
    oauth.Sign(res, 'PUT', p,oauth.TokenSecret);
    res := oauth.WebPut(res,p,json);
    FreeAndNil(p);

    debugln('RES PUSH = '+res);

    if (res = '') then begin ErrorString :=  'Next-GetNotes: Unable to get initial data'; exit(false); end;

    result := True;
end;


function TNextSync.DoRemoteManifest(const RemoteManifest: string): boolean;
begin
	WriteLn('Next-DoRemoteManifest');
    result := True;
end;

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

