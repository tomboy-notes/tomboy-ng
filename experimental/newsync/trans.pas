unit trans;

{ Contains parent, abstract class that does Transport part of sync.
  It moves files around, one way or another.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, SyncUtils;


type

  { TTomboyTrans }

  TTomboyTrans = class      // An abstract class, parent of eg FileTrans and NetTrans
    private

    public
        DebugMode : boolean;
            // Set to '' is no errors.
        ErrorString : string;
            // Local notes directory
        NotesDir, ConfigDir : string;
            // A url to network server or 'remote' file directory for FileSync
        RemoteAddress : string;

            { The current server ID. Is set with a successful TestTransport call. }
        RemoteServerID : string;

            { The current Server Rev, before we upload. Is set with a successful
              TestTransport call. }
        RemoteServerRev : integer;

            {Tests availability of remote part of connection. For file sync thats
            existance of manifest and 0 dir, write access. Returns with ServerID.
            This would be a good place to put lock or authenticate as  necessary}
        function TestTransport(out ServerID : string) : TSyncAvailable; virtual; abstract;

            {Request a list of all notes the server knows about. Ones with a rev no higher
            nominated revision should have a last-change-date. We don't use all fields
            in TInfoList and the list must have been created.
            This function gets, one way or another info that is in a RemoteManifest
            it must return with the remote server ID so client knows it can trust
            its local manifest. }
        function GetNewNotes(const NoteMeta : TNoteInfoList; const LocRev : integer
                                   ) : boolean; virtual; abstract;

            {Request that all the notes mentioned in the simple list be downloaded and,
            if necessary, any existing note be moved to Backup.  }
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; virtual; abstract;

            { Advise server that a note has been deleted, this does trigger a new rev.
              must call note by note, returns false if Transport has an error. ExistRev is
              rev number under which the note should be found in remote repo and deleted from.}
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; virtual; abstract;

           { Push a list of notes up to the server. A new revision has been made and we
             are passed its number. }
        function UploadNotes(const Uploads : TStringList) : boolean; virtual; abstract;

            { Tells Trans to deal with with remote mainfest. This is the trigger
              for a new revision on the server, the server must now do whatever
              it needs to accomodate the new new revision, some new or update
              notes will be sent to it a bit later. New RevNo will be RemoteServerRev plus 1 }
        function DoRemoteManifest(const RemoteManifest : string) : boolean; virtual; abstract;
            { Returns a full file name (inc path) to a (copy?) of indicated server
              version of a note. File sync will return just full path and name to the
              'remote' file but net sync will need to download the file and return
              path and name to an overwriteable file, perhaps $CONFIG/remote.note ? }
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; virtual; abstract;

  end;


implementation

{ TTomboyTrans }



end.

