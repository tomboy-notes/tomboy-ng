unit trans;

{ Contains parent, abstract class that does Transport part of sync.
  It moves files around, one way or another.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Sync;


type

  { TTomboyTrans }

  TTomboyTrans = class      // An abstract class, parent of eg FileTrans and NetTrans
    private

    public
        DebugMode : boolean;
            // Set to '' is no errors.
        ErrorString : string;
            // Local directories
        NotesDir, ConfigDir : string;

            {Request a list of new notes since nominated revision with, ideally,
            last-change-date for each. We don't use all fields in TInfoList }
        function GetNewNotes(NoteMeta : TNoteInfoList; LocRev : integer) : boolean; virtual; abstract;

            {Request that all the notes mentioned in the simple list be downloaded and,
            if necessary, any existing note be moved to Backup.  }
        function DownloadNotes(const DownLoads : TStringList) : boolean; virtual; abstract;

            { Advise server that a note has been deleted (does this trigger a rev ??).
              Can it be a list of notes ? Does this trigger a new revision. }
        function DeleteNote(const ID : string{; var NewRev : integer}) : boolean; virtual; abstract;

           { Push a note, or ideally, a set of notes up to server (triggering a new release). }
        function UploadNotes(const Uploads : TStringList; var NewRev : integer) : boolean; virtual; abstract;
  end;


implementation

{ TTomboyTrans }



end.

