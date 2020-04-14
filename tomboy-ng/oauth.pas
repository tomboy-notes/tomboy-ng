unit oauth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, StdCtrls, Dialogs, openssl, ssockets, sslsockets,
  fphttpclient, fpjson, jsonparser, fpopenssl, HMAC, base64, strutils;

type    { TOAuth }
  TOAuth = class(TObject)
public
    ConsumerKey : String;
    ConsumerSecret : String;
    Token : String;
    TokenSecret : String;
    Verifier : String;
    requestTokenUrl : String;
    authorizeUrl : String;
    accessTokenUrl : String;
    callbackUrl : String;
    constructor Create(); overload;
    function WebGet(u : String; params : TStrings ) : String;
    function WebPost(u : String; params : TStrings) : String;
    function URLEncode(s: String): String;
    function URLDecode(s: String): String;
    function ParamsSort(params : TStrings) : TStrings;
    procedure Sign(u : String; mode : String; params : TStrings);
    procedure BaseParams(const p : TStrings);
private
    function Timestamp() : String;
    function Nonce() : String;
    procedure HttpClientGetSocketHandler(Sender: TObject; const UseSSL: Boolean; out AHandler: TSocketHandler);

end;

implementation

{ TOAuth }

constructor TOAuth.Create();
begin
  inherited;

  InitSSLInterface;
  ConsumerKey := 'anyone';
  ConsumerSecret := 'anyone';
  Token := '';
  Verifier := '';
  callbackUrl := 'http://localhost:8000/tomboy-web-sync/';
end;
	
function TOAuth.Timestamp() : String;
begin
  Result := Format('%d',[Trunc((Now - EncodeDate(1970, 1 ,1)) * 24 * 60 * 60)]   );
end;

function TOAuth.Nonce() : String;
begin
//  Result := Timestamp() + Format('%d',[Random(1000000000)]);
  Result := Format('%d',[Random(9999999-123400)+123400]);
end;

procedure TOAuth.BaseParams(const p : TStrings);
begin
  //OAuth setup
  p.Add('oauth_version');
  p.Add('1.0');
  p.Add('oauth_signature_method');
  p.Add('HMAC-SHA1');
  // NONCE
  p.Add('oauth_nonce');
  p.Add(Nonce());
  // TIMESTAMP
  p.Add('oauth_timestamp');
  p.Add(Timestamp());
  // ConsumerKey
  p.Add('oauth_consumer_key');
  p.Add(ConsumerKey);
  // Token
  if(length(Token)>0) then begin
	p.Add('oauth_token');
  	p.Add(Token);
  end;
  // Verifier
  if(length(Verifier)>0) then begin
	p.Add('oauth_verifier');
  	p.Add(Verifier);
  end;
  // callbackUrl
  if(length(callbackUrl)>0) then begin
	p.Add('oauth_callback');
  	p.Add(callbackUrl);
  end;
end;

procedure TOAuth.HttpClientGetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  If UseSSL then begin
    AHandler:=TSSLSocketHandler.Create;
    TSSLSocketHandler(AHandler).SSLType:=stTLSv1_2;
  end else
      AHandler := TSocketHandler.Create;
end;

function TOAuth.URLDecode(s: String): String;
var
  i,lengthsource: integer;
  source: PAnsiChar;
begin
  result := '';
  source := pansichar(s);
  lengthsource := length(source);
  i:=1;
  while (i<=lengthsource) do
    begin
      if source[i-1] <> '%' then
        result := result + source[i-1]
      else if (source[i-1] = '%') and (i+1<=lengthsource) then
        try
          begin
            result := result + Chr(StrToInt('0x'+source[i]+source[i+1]));
            i:=i+2;
          end;
        except
        end
      else
        result := result + source[i-1];
      inc(i);
    end;
end;

function TOAuth.URLEncode(s: string): string;
var
  i: integer;
  source: PAnsiChar;
begin
  Result := '';
  source := pansichar(s);
  i :=0;
  while(i<length(source)) do begin
    if (source[i] = ' ') then Result := Result + '+'
    else if not (source[i] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.']) then
      Result := Result + '%' + IntToHex(ord(source[i]), 2)
    else
      Result := Result + source[i];
    i := i + 1;
  end;
end;

function TOAuth.ParamsSort(params : TStrings) : TStrings;
var
  p : TStrings;
  i : integer;
  j : integer;
  k : integer;
begin
  p := TStringList.Create();
  i:=0;
  while(i<params.Count) do
    begin
      j := 0;
      while((j<p.Count) and (CompareText(params[i],p[j])>0)) do j:= j +2;

      if(j<p.Count) then
        begin
          p.Insert(j,params[i]);
          p.Insert(j+1,params[i+1]);
        end else  begin
          p.Add(params[i]);
          p.Add(params[i+1]);
        end;
        i := i + 2;
    end;
  Result := p;
end;

function TOAuth.WebGet(u : String; params : TStrings) : String;
var
  Client: TFPHttpClient;
  i : integer;
begin
  Client := TFPHttpClient.Create(nil);
  Client.OnGetSocketHandler := @HttpClientGetSocketHandler;
  Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  Client.AllowRedirect := true;

  writeln('GET2= '+u);

  i:=0;
  while(i<params.Count) do begin
    if(i=0) then u := u + '?' else u := u + '&';
    u := u + URLEncode(params[i]) + '=' + URLEncode(params[i+1]);
    i := i +2;
  end;

  writeln('GET3= '+u);

  try
    Result := Client.Get(u);
    writeln('GET RES= '+Result);
  except on E:Exception do begin
    ShowMessage(E.message);
    Result :='';
    end;
  end;
  Client.Free;
end;


function TOAuth.WebPost(u : String; params : TStrings) : String;
var
  Client: TFPHttpClient;
  res : TStringStream;
  p : TStrings;
  i : integer;
begin

  Client := TFPHttpClient.Create(nil);
  Client.OnGetSocketHandler := @HttpClientGetSocketHandler;
  Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  Client.AllowRedirect := true;

  res := TStringStream.Create('');

  p := TStringList.Create();
  i:=0;
  while(i<params.Count) do begin
    //p.Add(Format('%s=%s',[URLEncode(params[i]),URLEncode(params[i+1])]));
    p.Add(Format('%s=%s',[params[i],params[i+1]]));
    i := i +2;
  end;

  try
    Client.FormPost(u,p,res);
    Result := res.DataString;
    writeln('POST RES= '+Result);
  except on E:Exception do begin
    ShowMessage(E.message);
    Result := '';
    end;
  end;
  Client.Free;
  res.Free;
end;

procedure TOAuth.Sign(u : String; mode : String; params : TStrings);
var
  data : String;
  p : String;
  i : integer;
  j : integer;
  key : String;
  signature : String;
  s2 : String;
  c : String;
  b64 : String;
begin
  p :='';
  i:=0;
  while(i<params.Count) do begin
    if(i>0) then p := p + '&' ;
    p := p + params.Strings[i] + '=' + URLEncode(params.Strings[i+1]);
    i := i +2;
  end;

  key := ConsumerSecret + '&' + Token;

  data := mode + '&' + URLEncode(u) + '&' + URLEncode(p);

  signature := HMACSHA1(key, data);
  writeln('KEY = '+key);
  writeln('SIGN = '+data);

  b64 := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  i:=1;
  s2 :='';
  while(i<=length(signature)) do begin
    c := '0x' + copy(signature,i,2);
    j := StrToInt(c);
    s2 := s2 + intToBin(j,8);
    i := i+2;
  end;

  i :=1;
  signature :='';
  while(i<=length(s2)) do begin
    c := '%' + copy(s2,i,6);
    p :='';
    if(length(c)<7) then begin c:= c + '00'; p:=p+'='; end;
    if(length(c)<7) then begin c:= c + '00'; p:=p+'='; end;
    j := StrToInt(c);
    signature := signature + b64[j+1] + p;
    i := i+6;
  end;

  writeln('SIGNATURE = '+signature);

  params.Add('oauth_signature');
  params.Add(signature);

end;

end.

