unit transfile;

{ A unit that does the file transfer side of a FileSync operation

This is one implementation of layer 4 from here -
https://github.com/tomboy-notes/tomboy-ng/wiki/Another-Sync-Model#implementation

}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, trans, Sync;

type

  { TFileSync }

  TFileSync = Class(TTomboyTrans)
    private

    public
        RemoteDir : string; // where the remote filesync repo lives.

        function GetNewNotes(NoteMeta : TNoteInfoList; LocRev : integer) : boolean; override;
        function DownloadNotes(const DownLoads : TStringList) : boolean; override;
        function DeleteNote(const ID : string{; var NewRev : integer}) : boolean; override;
        function UploadNotes(const Uploads : TStringList; var NewRev : integer) : boolean; override;
  end;

implementation

uses LazFileUtils;

{ TFileSync }

function TFileSync.GetNewNotes(NoteMeta: TNoteInfoList; LocRev : integer): boolean;
begin
    // Read the remote mainfest and fill in as much as we can
    // pretty much just copy from TTomboyFileSync.ReadRemoteManifest()
    // but only return with entries whose rev number is greater than LocRev
  result := True;
end;

function TFileSync.DownloadNotes(const DownLoads: TStringList): boolean;
begin
  // Just a copy operation, test if target is there already and backup if it is.
  result := True;
end;

function TFileSync.DeleteNote(const ID: string): boolean;
begin
  // need a beter understaning here, not ready to implement yet !!!!!!!!!!!!
  result := True;
end;

function TFileSync.UploadNotes(const Uploads: TStringList; var NewRev : integer): boolean;
begin
  // Create a new server revision and upload the list of notes we have to it.

  result := True;
end;



end.

