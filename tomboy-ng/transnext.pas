unit transnext;

{ A unit that does the file transfer side of a NextSync operation.
  Depends on external (Ruby) modules, as yet unimplemented.
  *
  *  See attached licence file.
}
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Trans, SyncUtils;

type               { ----------- TNextSync ------------ }


{ TNextSync }

TNextSync = Class(TTomboyTrans)
    private

    public
        function TestTransport(const WriteNewServerID : boolean = False): TSyncAvailable; override;
        function SetTransport(): TSyncAvailable; override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
        //function SetRemoteRepo(ManFile : string = '') : boolean; override;
 end;


implementation

{ TNextSync }

function TNextSync.TestTransport(const WriteNewServerID : boolean = False): TSyncAvailable;
begin
	WriteLn('Next-TestTransport');
    Result := SyncReady;
end;

function TNextSync.SetTransport(): TSyncAvailable;
begin
	WriteLn('Next-SetTransport');
        Result := SyncNoRemoteMan;
end;

function TNextSync.GetNewNotes(const NoteMeta: TNoteInfoList; const GetLCD : boolean): boolean;
begin
	WriteLn('Next-GetNewNotes');
    { Will call external ruby script that returns a list of notes. Probably does
      something like -
      Runcommand('ruby', ['sync.rb', 'newnotes', booltostr(GetLCD)]);
      we then capture the output from RunCommand(), parse it and put result in
      the NoteMeta list. Capture at least ID and RevNo and ideally LastChangeDate.
      If all worked as expected, set result to true, if not, False and put something
      into ErrorString.
      }
    result := True;
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

end.

