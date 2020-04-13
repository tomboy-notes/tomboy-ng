unit ncsetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLIntf, StdCtrls,
  fpjson, jsonparser, oauth;

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
  res : String;
  resturl : String;
  jData : TJSONData;
  jObject : TJSONObject;
  p : TStrings;
begin

  try
    // GET OAUTH URLS
     resturl := Trim(URL.Text) + '/api/1.0/';
     writeln('GET0= '+resturl);
  p := TstringList.Create();
     res := oauth.WebGet(resturl,p);
     if(length(res)>0) then begin
        jData := GetJSON(res);
        writeln(jData.FormatJSON);
        jObject := TJSONObject(jData);
        oauth.requestTokenUrl := jObject.Get('oauth_request_token_url');
        oauth.authorizeUrl    := jObject.Get('oauth_authorize_url');
        oauth.accessTokenUrl  := jObject.Get('oauth_access_token_url');
        // REQUEST TOKEN
        p := TStringList.Create();
        oauth.BaseParams(p);
        p := oauth.ParamsSort(p);
        oauth.Sign(oauth.requestTokenUrl, 'POST', p);
        //writeln(oauth.WebPost('https://grosjo.net/test.php',p));
        res := oauth.WebPost(oauth.requestTokenUrl,p);
        // TO BE CONTIBUED
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

