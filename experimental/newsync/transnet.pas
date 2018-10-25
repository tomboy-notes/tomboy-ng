unit transnet;

{ A unit that does the file transfer side of a NetSync operation.
  Depends on external (Ruby) modules, as yet unimplemented.
  *
  *  See attached licence file.
}
{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Trans, SyncUtils;

type               { ----------- TNetSync ------------ }


{ TNetSync }

TNetSync = Class(TTomboyTrans)
    private

    public
        function TestTransport(): TSyncAvailable; override;
        function TestTransportEarly(out ManPrefix: string): TSyncAvailable; override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; Override;
        //function SetRemoteRepo(ManFile : string = '') : boolean; override;
 end;


implementation

{ TNetSync }

function TNetSync.TestTransport(): TSyncAvailable;
begin
    Result := SyncReady;
end;

function TNetSync.TestTransportEarly(out ManPrefix: string): TSyncAvailable;
begin
        ServerID := '';
        Result := SyncNoRemoteMan;
end;

function TNetSync.GetNewNotes(const NoteMeta: TNoteInfoList; const GetLCD : boolean): boolean;
begin
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

function TNetSync.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
begin
     result := True;
end;

function TNetSync.DeleteNote(const ID: string; const ExistRev : integer): boolean;
begin
     result := True;
end;

function TNetSync.UploadNotes(const Uploads: TStringList): boolean;
begin
    result := True;
end;

function TNetSync.DoRemoteManifest(const RemoteManifest: string): boolean;
begin
    result := True;
end;

function TNetSync.DownLoadNote(const ID: string; const RevNo: Integer): string;
begin
    // Download indicated note and call it ConfigDir + 'remote.note';
    result := ConfigDir + 'remote.note';
end;

{function TNetSync.SetRemoteRepo(ManFile: string = ''): boolean;
begin

end; }

end.

