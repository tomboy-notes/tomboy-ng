unit ncsetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLIntf, StdCtrls,
  fpjson, jsonparser, oauth, strutils, ncserver, fphttpserver, LazLogger;

type    { TFormNCSetup }
  TFormNCSetup = class(TForm)
    NCAuth: TButton;
    URL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SetupStatus: TLabel;
    NCDone: TToggleBox;
    function isSuccess() : boolean;
    function getKey() : String;
    function getToken() : String;
    function getTokenSecret() : String;
    procedure setKey(s : String);
    procedure setToken(s : String);
    procedure NCAuthClick(Sender: TObject);
    procedure AuthSuccess(i : Int64);
    procedure AuthFailure(i : Int64);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NCDoneChange(Sender: TObject);
  private
    success : boolean;
    listening : boolean;
    oauth: TOAuth;
    web: WebServer;
    procedure DoHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
  end;

var
  FormNCSetup: TFormNCSetup;

implementation

{$R *.lfm}

{ TFormNCSetup }

procedure TFormNCSetup.FormCreate(Sender: TObject);
begin
     OAuth := TOAuth.Create();
     success :=false;
     listening := false;
end;

procedure TFormNCSetup.FormDestroy(Sender: TObject);
begin
   FreeAndNil(OAuth);
//   if(FServer<>nil) then FreeAndNil(FServer);
end;


function TFormNCSetup.isSuccess() : boolean;
begin
     Result := success;
end;

function TFormNCSetup.getKey() : String;
begin
        Result := oauth.Key;
end;

function TFormNCSetup.getToken() : String;
begin
        Result := oauth.Token;
end;

function TFormNCSetup.getTokenSecret() : String;
begin
        Result := oauth.TokenSecret;
end;

procedure TFormNCSetup.setKey(s : String);
begin
        oauth.Key := s;
end;

procedure TFormNCSetup.setToken(s : String);
begin
        oauth.Token :=s;
end;

procedure TFormNCSetup.NCAuthClick(Sender: TObject);
var
  res, s1, s2, s3, resturl, u : String;
  jData : TJSONData;
  jObject : TJSONObject;
  p : TStrings;
  ok : boolean;
  ts : TSysCharSet;
begin
  if(listening) then begin
     SetupStatus.Caption := 'OAuth: Prcessus canceled';
     URL.Enabled:= true;
     NCAuth.Caption := 'Authenticate';
     web.Finish();
     listening :=false;
     exit;
  end;

  success :=false;

  NCAuth.Enabled :=false;
  try
    // GET OAUTH URLS
     SetupStatus.Caption := 'OAuth: Getting URLs settings';
     resturl := Trim(URL.Text) + '/api/1.0/';
     p := TstringList.Create();
     res := oauth.WebGet(resturl,p);
     FreeAndNil(p);
     if(length(res)>0) then begin
        ok := true; s1 :=''; s2:=''; s3:='';
        try
           jData := GetJSON(res);
           debugln('JSON OAUTH = ' + jData.FormatJSON());
           jObject := TJSONObject(jData);
           s1 := jObject.Get('oauth_request_token_url');
           s2 := jObject.Get('oauth_authorize_url');
           s3 := jObject.Get('oauth_access_token_url');
           FreeAndNil(jObject);
        except on E:Exception do begin
          ShowMessage(E.message);
          ok:= false;
          end;
        end;
        if (not ok) then begin SetupStatus.Caption := 'Error JSON'; exit; end;
        if((length(s1)<10) or (length(s2)<10) or (length(s3)<10)) then
        begin SetupStatus.Caption := 'Server returns invalid OAuth URLs'; exit; end;

        oauth.requestTokenUrl := s1;
        oauth.authorizeUrl    := s2;
        oauth.accessTokenUrl  := s3;

        // REQUEST TOKEN
        SetupStatus.Caption := 'OAuth: Getting Request Token';
        p := TStringList.Create();
        oauth.BaseParams(p,false);
        oauth.ParamsSort(p);
        oauth.Sign(oauth.requestTokenUrl, 'POST', p, '');
        res := oauth.WebPost(oauth.requestTokenUrl,p);
        FreeAndNil(p);

        //Example : oauth_token=r36747eda81d3a14c&oauth_token_secret=s05507586554bb6bf&oauth_callback_confirmed=true

        ts:=['&'];
        s1 := ExtractWord(1, res, ts);
        s2 := ExtractWord(2, res, ts);
        s3 := ExtractWord(3, res, ts);

        ts:=['='];

        if( ExtractWord(1, s1, ts) <> 'oauth_token' ) then begin SetupStatus.Caption := 'OAuth: token invalid'; ShowMessage(SetupStatus.Caption); exit; end;
        if( ExtractWord(1, s2, ts) <> 'oauth_token_secret' ) then begin SetupStatus.Caption := 'OAuth token Secret invalid'; ShowMessage(SetupStatus.Caption); exit; end;
        if( ExtractWord(1, s3, ts) <> 'oauth_callback_confirmed' ) then begin SetupStatus.Caption := 'OAuth not confirmed'; ShowMessage(SetupStatus.Caption); exit; end;

        oauth.Token := ExtractWord(2, s1,ts);
        oauth.TokenSecret:= ExtractWord(2,s2,ts);

        // AUTORIZE
        SetupStatus.Caption := 'OAuth: Asking user to autorize';
        u:= oauth.authorizeUrl + '?oauth_token=' + oauth.Token + '&client=TomboyNG&oauth_callback=' + oauth.URLEncode(oauth.callbackUrl);
        NCAuth.Caption := 'Cancel OAuth';
        URL.Enabled:= false;

        web := WebServer.Create(false);
        web.Setup(8000,@DoHandleRequest);
        listening :=true;
        openUrl(u);
     end;
  except on E:Exception do
    ShowMessage(E.message);
  end;
  NCAuth.Enabled := true;

end;

procedure TFormNCSetup.DoHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  restok, resverif : String;
begin
  AResponse.Code := 200;
  AResponse.ContentType := 'text/html;charset=utf-8';


  restok := ARequest.QueryFields.Values['oauth_token'];
  resverif := ARequest.QueryFields.Values['oauth_verifier'];
  web.Finish();
web := nil;
  listening :=false;
  if(restok = oauth.Token) then begin
    oauth.Verifier := resverif;
    AResponse.Contents.Text := '<h2>Congratulation, your TomboyNG is authenticated</h2>';
    //web.Synchronize(web,@AuthSuccess);
    Application.QueueAsyncCall(@AuthSuccess,0);
  end else begin
     AResponse.Contents.Text := '<h2>A error occured : URL is '+ARequest.URL+'/h2>';
     //web.Synchronize(web,@AuthFailure);
     Application.QueueAsyncCall(@AuthFailure,0);
  end;
end;

procedure TFormNCSetup.AuthFailure(i : Int64);
begin
  SetupStatus.Caption := 'OAuth: Error occured';
  URL.Enabled:= true;
  NCAuth.Caption := 'Authenticate';
end;

procedure TFormNCSetup.AuthSuccess(i : Int64);
var
  p : TStrings;
  res, s1, s2 : String;
  ts : TSysCharSet;
begin
  NCAuth.Enabled :=false;

  // ACCESS TOKEN
  SetupStatus.Caption := 'OAuth: Getting Access Token';
  p := TStringList.Create();
  oauth.BaseParams(p,true);
  oauth.ParamsSort(p);
  oauth.Sign(oauth.accessTokenUrl, 'POST', p, oauth.TokenSecret);
  res := oauth.WebPost(oauth.accessTokenUrl,p);
  FreeAndNil(p);

  ts:=['&'];
  s1 := ExtractWord(1, res, ts);
  s2 := ExtractWord(2, res, ts);

  ts:=['='];

  NCAuth.Enabled := true;
  NCAuth.Caption := 'Re-authenticate';

  if( ExtractWord(1, s1, ts) <> 'oauth_token' ) then begin SetupStatus.Caption := 'OAuth: token invalid'; ShowMessage(SetupStatus.Caption); exit; end;
  if( ExtractWord(1, s2, ts) <> 'oauth_token_secret' ) then begin SetupStatus.Caption := 'OAuth token Secret invalid'; ShowMessage(SetupStatus.Caption); exit; end;

  oauth.Token := ExtractWord(2, s1,ts);
  oauth.TokenSecret:= ExtractWord(2,s2,ts);

  success :=true;
  SetupStatus.Caption := 'Congratulation, your TomboyNG is authenticated';

end;

procedure TFormNCSetup.NCDoneChange(Sender: TObject);
begin
    Close;
end;

end.

