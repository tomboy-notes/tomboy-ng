unit wmecho;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, webutil;

type

  { TEchoModule }

  TEchoModule = class(TFPWebModule)
    procedure EchoModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  EchoModule: TEchoModule;

implementation

{$R *.lfm}

{ TEchoModule }

procedure TEchoModule.EchoModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
Var
  S : TStrings;

begin
  S:=TStringList.Create;
  try
    // Analyze request.
    DumpRequest(ARequest,S);

    AResponse.Contents:=S;
    Handled:=True;
  finally
    S.Free;
  end;
end;

initialization
  RegisterHTTPModule('TEchoModule', TEchoModule);
end.

