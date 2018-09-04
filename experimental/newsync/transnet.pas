unit transnet;

{
A unit that does the file transfer side of a NetSync operation

This is one implementation of layer 4 from here -
https://github.com/tomboy-notes/tomboy-ng/wiki/Another-Sync-Model#implementation

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
        function GetNewNotes(NoteMeta : TNoteInfoList; LocRev : integer) : boolean; override;
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;
        function DeleteNote(const ID : string{; var NewRev : integer}) : boolean; override;
        function UploadNotes(const Uploads : TStringList; var NewRev : integer) : boolean; override;
  end;


implementation

{ TNetSync }

function TNetSync.GetNewNotes(NoteMeta: TNoteInfoList; LocRev: integer): boolean;
begin
    { Will call external ruby script that returns a list of notes that are newer
      than LocRev. Probably does something like
      Runcommand('ruby', ['sync.rb', 'newnotes', inttostr(LocRev)]);
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

function TNetSync.DeleteNote(const ID: string): boolean;
begin
     result := True;
end;

function TNetSync.UploadNotes(const Uploads: TStringList; var NewRev : integer): boolean;
begin
    result := True;
end;

end.

