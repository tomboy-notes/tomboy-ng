unit autostart;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

  { TAutoStartCtrl }

  TAutoStartCtrl = class
    Private
        procedure AutoOff(AppName : string);
        procedure AutoOn(AppName: string);


    Public
      ADir : string;       // where we look for link.
      LinkName : string;      // What we put in destination
      LinkDestination : string; // Directory we put the above.
      constructor Create(AppName : string; StartIt : boolean);
      destructor Destroy;  override;
  end;

implementation

uses LazLogger, LazFileUtils, FileUtil, LazUTF8;

{ TAutoStartCtrl }

procedure TAutoStartCtrl.AutoOn(AppName : string);
begin
    {$ifdef LINUX}
        if not DirPathExists(LinkDestination) then
            ForceDirectory(LinkDestination);
        if  FileExists('/usr/share/applications/' + AppName + '.desktop') then
            CopyFile('/usr/share/applications/' + AppName + '.desktop', LinkDestination + '/' + AppName + '.desktop');
    {$endif}
end;

procedure TAutoStartCtrl.AutoOff(AppName : string);
begin
    {$ifdef LINUX}
        if FileExists(LinkDestination + '/' + AppName + '.desktop') then
            DeleteFile(LinkDestination + '/' + AppName + '.desktop');
    {$endif}
end;

constructor TAutoStartCtrl.Create(AppName: string; StartIt: boolean);
begin
    inherited create;
    {$ifdef LINUX}
        LinkDestination := AppendPathDelim(GetEnvironmentVariableUTF8('HOME')) + '.config/autostart';
    {$endif}
    if StartIt then AutoOn(AppName)
    else AutoOff(AppName);
end;

destructor TAutoStartCtrl.Destroy;
begin
    inherited Destroy;
end;

end.

