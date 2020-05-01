unit trans;

{ Contains parent, abstract class that does Transport part of tomboy-ng sync.
  It moves files around, one way or another, determined by its children.
  *  Copyright (C) 2018 David Bannon
  *  See attached licence file.

  HISTORY
    2018/10/25  Much testing, support for Tomdroid.
    2018/10/28  Added password
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, SyncUtils;


type

  { TTomboyTrans }

  TTomboyTrans = class      // An abstract class, parent of eg FileTrans and NetTrans
    private
          // Hold any parameters
        names : TStringList;
        values : TStringList;

    public
            // A password for those Transports that need one.
        //Password : string;

        DebugMode : boolean;
            // Set to '' is no errors.
        ErrorString : string;
            // Local notes directory
        NotesDir, ConfigDir : string;
            // A url to network server or 'remote' file directory for FileSync
        //RemoteAddress : string;

            { The current server ID. Is set with a successful TestTransport call. }
        ServerID : string;

            { The current Server Rev, before we upload. Is set with a successful
              TestTransport call. }
        RemoteServerRev : integer;

        constructor create;
        destructor destroy;

            { Tests availability of remote part of connection. For file sync (eg) thats
            existance of remote manifest and 0 dir, write access. Sets its own ServerID.
            This would be a good place to put lock or authenticate as  necessary}
        function TestTransport(const WriteNewServerID : boolean = False) : TSyncAvailable;     virtual; abstract;

            { May (or may not) do some early transport tests, ie, in Tomdroid sync
              it pings the remote device. Should return SyncReady or an error value
              if something failed.}
        function SetTransport() : TSyncAvailable; virtual; abstract;

            {Request a list of all notes the server knows about. Returns with Last Change
            Date (LCD) if easily available and always if GetLCD is true. We don't use all
            fields in TInfoList, the list must have been created.}
        function GetNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean
                                   ) : boolean; virtual; abstract;

            {Request that all the notes mentioned in the simple list be downloaded and,
            if necessary, any existing note be moved to Backup.  Note that the list
            contains just IDs, there is no '.note'}
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; virtual; abstract;

            { --- Check if this function does actully need implementing ------
              Advise server that a note has been deleted, a new rev has been triggered.
              Would call note by note, returns false if Transport has an error.
              ExistRev is rev number under which the note should be found in remote
              repo and deleted from iff you feel so inclinded.}
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; virtual; abstract;

           { Push a list of notes up to the server. A new revision has been made and we
             are passed its number. If it turns out to be a new Repo, we'll make the
             necessary directories first. }
        function UploadNotes(const Uploads : TStringList) : boolean; virtual; abstract;

            { Tells Trans to deal with with remote mainfest. This is the trigger
              for a new revision on the server, the server must now do whatever
              it needs to accomodate the new new revision, some new or update
              notes will be sent to it a bit later. New RevNo will be RemoteServerRev plus 1 }
        function DoRemoteManifest(const RemoteManifest : string) : boolean; virtual; abstract;
            { Returns a full file name (inc path) to a (copy?) of indicated server
              version of a note. File sync will return just full path and name to the
              'remote' file but net sync will need to download the file and return
              path and name to an overwriteable file, perhaps $CONFIG/remote.note ?
              We need this so we can compare notes when we are resolving a clash.}
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; virtual; abstract;

        function getPrefix(): string; virtual; abstract;

        function IDLooksOK() : boolean; virtual; abstract;

        procedure setParam(p : String; v : String);
        function getParam(p : String) : String;


  end;


implementation

{ TTomboyTrans }

constructor TTomboyTrans.Create;
begin
  names := TStringList.Create();
  values := TStringList.Create();
end;

destructor TTomboyTrans.Destroy;
begin
  FreeAndNil(names);
  FreeAndNil(values);
end;


procedure TTomboyTrans.setParam(p : String; v : String);
var
  i : LongInt;
begin
  writeln('SetParams( '+p+' , '+v+' )');

  i := names.IndexOf(p);
  if(i<0) then begin
     names.add(p);
     values.add(v);
  end else values.Strings[i] :=v;
  i := names.IndexOf(p);
end;

function TTomboyTrans.getParam(p : String) : String ;
var
  i : LongInt;
  v : String;
begin
  i := names.IndexOf(p);
  if(i<0) then v := ''
  else v := values.strings[i];
  writeln('GetParams( '+p+' ) => '+v);
  Result := v;
end;

end.





