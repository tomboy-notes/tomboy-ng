unit oauth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, openssl, ssockets, sslsockets,
  fphttpclient, fpjson, jsonparser, fpopenssl,  strutils;

type    { TOAuth }
  TOAuth = class(TObject)
public
    ParamList : TstringList;
    ConsumerKey :   String;         // load from file
    ConsumerSecret :  String;       // load from file
    Token : String;                 // Empty, then RequestToken, then AccessToken
    TokenSecret : String;           // Empty, then RequestToken, then AccessToken ??
    Verifier : String;
    requestTokenUrl : String;
    authorizeUrl : String;
    accessTokenUrl : String;
    callbackUrl : String;
    constructor Create(); overload;
    destructor Destroy; override;
    function GetSignature(): string;
    procedure SetReqHeader();
    function ParamQueryString(): string;
    function ParamAuthHeadString(): string;
    function ParamReqBodyString(): string;
private
    function GetNonce(): string;
	function LoadConsumerData(): boolean;



	procedure PrettyHeader();
end;


implementation

uses DateUtils, HMAC, base64, httpprotocol;

{ TOAuth }

constructor TOAuth.Create();
begin
  inherited;
  ParamList := TStringList.Create;
  if not LoadConsumerData() then
    writeln('Sorry, no consumer data found');
  Token := '';
  Verifier := '';
  //callbackUrl := 'http://localhost:8000/tomboy-web-sync/';
end;

destructor TOAuth.Destroy;
begin
    ParamList.Free;
    inherited;
end;

    // here we load a consumer key (public key) and consumer secret (privare key) I generated
    // with openssl, I think its massivly overkill but cannot see a simpler approach
    // Note that Tomboy docs state that initial connect needs ConsumerKey := 'anyone' ????
function TOAuth.LoadConsumerData() : boolean;
var
    InFile : TextFile;
    InString : string;
begin
    if fileexists('appkey.pem') and fileexists('appkey.pub') then begin
        AssignFile(InFile, 'appkey.pem');
        reset(InFile);
        while not eof(InFile) do begin
            readln(InFile, InString);
            if pos('----', InString) = 0 then begin
                while (InString[length(Instring)] = #10)
                    or (InString[length(inString)] = #13) do
                        delete(InString, length(InString), 1);
                ConsumerSecret := ConsumerSecret + Instring;
			end;
		end;
        CloseFile(InFile);
        AssignFile(InFile, 'appkey.pub');
        reset(InFile);
        while not eof(InFile) do begin
            readln(InFile, InString);
            if pos('----', InString) = 0 then begin
                while (InString[length(Instring)] = #10)
                    or (InString[length(inString)] = #13) do
                        delete(InString, length(InString), 1);
                ConsumerKey := ConsumerKey + Instring;
			end;
		end;
        CloseFile(InFile);
        result := true;
	end else
        result := false;
end;

function TOAuth.GetNonce() : string;
var
    GUID : TGUID;
begin
    CreateGUID(GUID);
    Result := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
end;

        // Put double inverted commas around the values, use this only if we want
        // to pass the list, as a list, to a POST method.
procedure TOAuth.PrettyHeader();
var
    Index : integer = 0;
    St : string;
begin
    while Index < ParamList.count do begin
        St := ParamList[Index];
        {insert('"', St, pos('=', St) + 1);
        St := St + '"';}
        St := St + '&';
        ParamList.Delete(Index);
        ParamList.Insert(Index, St);
        inc(Index);
	end;
end;


    // As per the rfc5.4.1, values in double inverted, comma seperated, auth header
function TOAuth.ParamAuthHeadString() : string;
var
    St, St2 : string;
begin
    // note hardwired realm and clumbsy decision about what to encode ....
    Result := httpEncode('Authorization:') + ' OAuth realm="Snowy",';
    for St in ParamList do begin
        St2 := St;
        insert('"', St2, pos('=', St2) + 1);
        St2 := St2 + '",';
        Result := Result + St2;
	end;
    delete(Result, length(Result), 1);      // remove trailing ,
end;

// starts with ?, no inverted commas, seperate with &
function TOAuth.ParamQueryString() : string;
var
    St : string;
begin
    Result := '?';
    for St in ParamList do begin
         Result := Result + St + '&';
	end;
	delete(Result, length(Result), 1);      // remove trailing &
end;

    // This one should work. The library POST functions add the appropriate "APPLICATION...." header
    // and we have & seperated pairs with no commas.  Stuff sent is all encoded.
    // However, it says sig is bad.
    // if I send, comma seperated wrapped in double inverted, it look, in the logs, just like Tomboy
    // but still fails.  Not absolutly sure but the real Tomboy one is probably also a POST, why does it
    // send data in a non-standard (for POST) manner ?
function TOAuth.ParamReqBodyString() : string;
var
    St, St2 : string;
begin
    Result := '';
    for St in ParamList do begin
        {St2 := St;
        insert('"', St2, pos('=', St2) + 1);
        St2 := St2 + '",';
        Result := Result + St2;  }
              // or -
        Result := Result + St + '&';
	end;
	delete(Result, length(Result), 1);      // remove trailing &
    result := 'Authorization: Oauth realm="Snowy",' + result +#10#13;
end;

        // Again, guessing these don't all contain char that need to be HTTPEncoded but .......
procedure TOAuth.SetReqHeader();
begin
    ParamList.Clear;
    //ParamList.add('oauth_consumer_key=' + httpencode(ConsumerKey));
    ParamList.add('oauth_consumer_key=' + httpencode('anyone'));
    if Token <> '' then
        ParamList.Add('oauth_token=' + Token);
    ParamList.Add('oauth_signature_method=HMAC-SHA1');
    ParamList.add('oauth_timestamp=' + inttostr(SecondsBetween(Now, EncodeDate(1970, 1 ,1))));
    ParamList.add('oauth_nonce=' + GetNonce());
    ParamList.add('oauth_version=1.0');
    ParamList.add('oauth_callback=' + HTTPEncode(authorizeUrl));
    ParamList.add('oauth_signature=' + GetSignature());
    //PrettyHeader();
    writeln('------------ HEADER -------------');
    writeln(ParamList.Text);
end;


function TOAuth.GetSignature() : string;
var
  TheText, Key : string;
  St : string;
begin
    ParamList.Sort;
    if Token = '' then begin    // just starting up here.
        TheText := 'POST&' + HTTPEncode(requestTokenUrl) {+ '&'};
        // Just what is the Key ?  RFC 9.2 says  "Consumer Secret and Token Secret, separated by
        // an '&' character (ASCII code 38) even if empty."  TokenSecret is empty at this stage, so
        // maybe just consumersecret and '&'  ??
        Key := HTTPEncode(ConsumerSecret) + '&';
	end else  begin
        // must deal with later stages
        exit('');
    end;
    for St in ParamList do
        TheText := TheText + '&' + St;
    writeln('********* PARAM LIST');
    writeln(httpdecode(TheText));
    writeln('********* KEY ');
    writeln(httpdecode(Key));
    writeln('***************');
    // OK, we have what we need, lets do it.  Says the rfc9.2.1 - oauth_signature is set to the calculated digest
    // octet string,first base64-encoded per [RFC2045] section 6.8, then URL-encoded per Parameter Encoding.
    result := EncodeStringBase64(HMACSHA1(Key, TheText));
    //result := EncodeStringBase64(HMACSHA1(TheText, Key));  //Big Chimp says BaseString, Key ????
    result := HTTPEncode(result);
end;


end.

