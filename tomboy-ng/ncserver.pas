 unit ncserver;
     
 {$mode objfpc}{$H+}
    
 interface
   
 uses
      Classes, SysUtils, fphttpserver;
     
 type
      WebServer = class(TThread)
      private
        server: TFPHTTPServer;
      public
        destructor Destroy; override;
        procedure Setup(APort: word; const OnRequest: THTTPServerRequestHandler);
        procedure Execute(); override;
        procedure Finish();
      end;
     
     
 implementation
     
 procedure WebServer.Setup(APort: word; const OnRequest: THTTPServerRequestHandler);
 begin
   server := TFPHTTPServer.Create(nil);
   server.Port := APort;
   server.OnRequest := OnRequest;
   FreeOnTerminate := true;
 end;

destructor WebServer.Destroy;
begin
  server.Free;
end;

procedure WebServer.Execute;
begin
  try
     server.Active := True;
  except on E: Exception do writeln(E.Message);
  end;
end;
     
procedure WebServer.Finish();
begin
   server.Active := False;
end;
     
end.
