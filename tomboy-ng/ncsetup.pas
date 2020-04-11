unit ncsetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLIntf, StdCtrls,
  fpjson, jsonparser, oauth, strutils;

type    { TFormNCSetup }
  TFormNCSetup = class(TForm)
    NCAuth: TButton;
    URL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Reset: TToggleBox;
    NCDone: TToggleBox;
    procedure NCAuthClick(Sender: TObject);
    procedure NCDoneChange(Sender: TObject);
    procedure ResetChange(Sender: TObject);
  private

  public
    oauth : TOauth;
  end;

var
  FormNCSetup: TFormNCSetup;

implementation

{$R *.lfm}

{ TFormNCSetup }


procedure TFormNCSetup.NCAuthClick(Sender: TObject);
var
  res, s1, s2, s3, resturl, u : String;
  jData : TJSONData;
  jObject : TJSONObject;
  p : TStrings;
  ok : boolean;
  ts : TSysCharSet;
begin

  try
    // GET OAUTH URLS
     resturl := Trim(URL.Text) + '/api/1.0/';
     p := TstringList.Create();
     res := oauth.WebGet(resturl,p);
     FreeAndNil(p);
     if(length(res)>0) then begin
        ok := true; s1 :=''; s2:=''; s3:='';
        try
           jData := GetJSON(res);
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
        if (not ok) then begin ShowMessage('Error JSON'); exit; end;
        if((length(s1)<10) or (length(s2)<10) or (length(s3)<10)) then
        begin ShowMessage('Server returns invalid OAuth URLs'); exit; end;

        oauth.requestTokenUrl := s1;
        oauth.authorizeUrl    := s2;
        oauth.accessTokenUrl  := s3;

        // REQUEST TOKEN
        p := TStringList.Create();
        oauth.BaseParams(p,false);
        oauth.ParamsSort(p);
        oauth.Sign(oauth.requestTokenUrl, 'POST', p);
        res := oauth.WebPost(oauth.requestTokenUrl,p);
        FreeAndNil(p);

        //Example : oauth_token=r36747eda81d3a14c&oauth_token_secret=s05507586554bb6bf&oauth_callback_confirmed=true

        ts:=['&'];
        s1 := ExtractWord(1, res, ts);
        s2 := ExtractWord(2, res, ts);
        s3 := ExtractWord(3, res, ts);

        ts:=['='];

        if( ExtractWord(1, s1, ts) <> 'oauth_token' ) then begin ShowMessage('OAuth token invalid'); exit; end;
        if( ExtractWord(1, s2, ts) <> 'oauth_token_secret' ) then begin ShowMessage('OAuth token Secret invalid'); exit; end;
        if( ExtractWord(1, s3, ts) <> 'oauth_callback_confirmed' ) then begin ShowMessage('OAuth not confirmed'); exit; end;

        oauth.Token := ExtractWord(2, s1,ts);
        oauth.TokenSecret:= ExtractWord(2,s2,ts);

        //writeln('Token= '+ oauth.Token);
        //writeln('TokenSecret= '+ oauth.TokenSecret);

        // AUTORIZE
        u:= oauth.authorizeUrl + '?oauth_token=' + oauth.Token + '&oauth_callback=' + oauth.URLEncode(oauth.callbackUrl);
        res := oauth.Callback(u);
     end;
  except on E:Exception do
    ShowMessage(E.message);
  end;
end;

procedure TFormNCSetup.NCDoneChange(Sender: TObject);
begin

end;

procedure TFormNCSetup.ResetChange(Sender: TObject);
begin

end;

end.

