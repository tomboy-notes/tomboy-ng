unit transnextcloud;

{ A unit that provides transport for a sync to the NextCloud Notes application.

  *  Copyright (C) 2020 David Bannon
  *  See attached licence file.

  ---- This overview ignores error testing, logging etc ----

  TFormSync.RunNextCloudSync(); calls :
    SetTransport - creates Trans, trans.SetTransport (does little)
    TestConnection - read local manifest to LocalMetaData, trans.TestTransport
                    (connects to server, looks for keynote)
    TSync.StartSync does -
      LoadRemoteRepoData(F) clears MainMetaData, calls Trans.GetNewNotes(downloads
                            list of existing notes to MMD and then uses the (local)
                            RemoteManifest to map SID->ID, correct creation date
                            for notes that come back from NC with Title, Modified.
                            The notes that don't need syncing come back with just
                            SID (because of pruneBefore) are marked SyNothing, that
                            might be overridden, later, to SyRemoteDelete.
      NextCloudCompare -    scans notes dir, comparing existing note's LCD to the
                            LastSyncDate, will decide if they are uploads
                            of clashes depending on existing state.
      CheckRemoteDeletes -  adds SyDeleteLocal Entry to MainMetaData for notes
                            previously synced (cos in LMD) but no longer at Remote.
      CheckLocalDeletes -   looks for notes in MMD that are marked as Deleted in LMD,
                            marks MMD entry as SyDeleteRemote.
      CheckNewNotes -       redundent, NextCloudCompare has this covered.
      ProcessClashes
      DoDownLoads
      DoDeletes
      DoUploads
      DoDeleteLocal

      The string, LocalLastSyncDateSt is read from localmanifest, used by CheckUsingLCD()
      and we don't use that in NextCloud.  In TSync.LoadRemoteRepoData(), in a 'Use',
      after creating MMD, we set MMD.LastSyncDateSt to LocalLastSyncDateSt before
      calling Trans.GetNewNotes. Other Sync Models don't use MMD.LastSyncDateSt so,
      no external impact. Then, when we make our initial GET call to NextCloud,
      we poke the HTTP response header Last-Modified into MainMetaData->LastSyncDateSt
      and then in  TSync.WriteLocalManifest()  if MMD.LastSyncDateSt is <> '' we use
      it instead of a call to  Sett.GetLocalTime.  That way, on the next sync,
      LocalLastSyncDateSt (when doing NextCloud) will contain a date string we
      can send to GetNotes.

        POTENTIAL PROBLEM

        We get a scan of NextCloud's current notes and record the datetime we did so,
        save that DateTime for the next run. However, we then upload a any notes that
        need uploading, this new note has a NextCloud TimeDate later than the sync time
        saved so on next sync, it will be downloaded (or, worse, trigger a clash).

        Only (and still risky) solution is to do the above and then do another
        GetNewNotes and use its retured time instead of the one obtained a minute
        or so earlier. If that GetNewNotes returns only the notes previously
        uploaded, just save the new datetime as the LastSyncDate, done.  If it
        returns more than just the previously uploaded notes, run another full
        sync and keep repeating until its 'clean'. If the the end user, on tomboy-ng
        has changed a note in that intervening period, but does not go on to change
        it after the new declared LastSyncDate, then that note will not be synced at
        the next sync.

        https://github.com/nextcloud/notes/issues/627

        Note that apart from issue above, the clash handler is not loading the remote
        note and deletes from both ends has not be truly tested.



  HISTORY
  2020/09/23  Made dowloads a bit more failsafe, remove from MMD if not able to process this time.
  2020/10/30  Changed the way GetNewNotes works, it now uses Meta.LastSyncDate for a pruneBefore call
              and if successful, sets it to the new value.  Notes NC believe don't need be downloaded
              are marked SyNothing (might be upgraded to SyRemoteDelete) else are S.
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, trans, SyncUtils, fpjson, jsonparser;

type
  TNoteRec = record
    SID : integer;
    modified : longint;
    category : string;
    content  : string;
  end;

type

  { TNextCloudSync }

  TNextCloudSync = class(TTomboyTrans)      
    private
                            { A Unix style time stamp, number of sec since epoc. Set when GetNewNotes
                            is called, used to push up a new or changed note }
        ThisSyncTime : longint;
                            { General Purpose http GET, uses passed URL and returns with results
                            in the passed SomeStrings. If GetModifiedTime is True the first line
                            of SomeStrings is the Date record of the GET in plain text from http
                            header, remainder is JSON, the content depending on passed URL. }
	    function Downloader(URL: string; SomeStrings: TStringList;  GetModifiedTime: boolean=false): boolean;
                            { Returns the Title of a note who's ID is SID, "ERROR, failed to get Title on error }
        function GetRemoteTitle(SID : longint) : string;

		function ExtractJID(data: string; out NRec: TNoteRec): longint;
		                    { Adds http if necessary (must add https) and returns either SyncReady,
				              SyncCredentialError or SyncNetworkError depending on what it found.
				              Will also set ErrorString if not SyncReady.  }
		function FormatTestURL(): TSyncAvailable;
                            { Downloads a list of notes from NextCloud, not getting things mentioned
                              in Params, so, typically, '?exclude=content,title'. Results put into
                              the (must be created) TNoteInfoList. }
		function GetListOfNotes(const NoteMeta: TNoteInfoList; Params: string=''): boolean;
		function GetServerID(SID: longint): boolean;
				                { Returns True if passed represents a Notebook Template, if not
				                it will fill out the passed string with first notebook encounted.
				                Note : we ignore all except the first Notebook encountered. }
		function IsTemplate(const ID: string; out Notebook: String): boolean;
		function JData2NoteList(const NoteMeta: TNoteInfoList; jItm: TJSONData): boolean;
                            { Returns the NextCloud assigned NoteID on success, 0 on failure
                            The passed noteRec should have modified set to a non zero numb
                            to be used as the modified value for the note. 0 is ignored. }
		function PostNewNote(Title, Notebook, Content: String; var NRec: TNoteRec): longint;
		function PushUpNote(ID, Notebook: string; var NRec: TNoteRec): longint;
                             { Reads remote manifest if it exists, adding ID and Create Date to Meta
                            If its not there, no problem, we are doing a Join. Make sure its not
                            there IF we are doing a Join !!
                            Then looks through the MMD for any remaining notes that were not matched
                            by the remote manifest, these are new remote notes, will be marked so and
                            assigned a UUID ID.   }
		procedure ReadRemoteManifest(NoteMeta: TNoteInfoList);
		procedure SayDebug(st: string; Always: boolean=false);
		function SillyDateTo8601(SillyDate: string): string;
		function TestNextCloudConnection(out MSG: string; const UsePassword: boolean): boolean;

                            { Tries to upload a KEYNOTE, might fail for a whole lot of reasons that we should
                              really have tested before we got to here. Later. What if there is already one
                              there with a bad UUID ?  TextCoonections should deal with things like that.}
	    function WriteANewServerID(): boolean;
                            { These are just a debug function. }
        procedure DumpJSON(const St: string);
        //procedure SayDebug(st : string; Always : boolean = false);
    public
        function TestTransport(const WriteNewServerID : boolean = False) : TSyncAvailable; override;  
        function SetTransport(ForceClean : boolean = False) : TSyncAvailable;  override;
        function GetNewNotes(const NoteMeta : TNoteInfoList; const GetLCD : boolean) : boolean; override;   
        function DownloadNotes(const DownLoads : TNoteInfoList) : boolean; override;  
        function DeleteNote(const ID : string; const ExistRev : integer) : boolean; override; 
        function UploadNotes(const Uploads : TNoteInfoList) : boolean; override;
        function UploadNotes(const Uploads : TStringList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override; 
        function DownLoadNote(const ID : string; const RevNo : Integer) : string; override; 
  end;

                            // Takes a NextCloud epoc second count and returns a Tomboy style ISO8601
  function ModifiedToTBDate(Modified : longint) : string;


implementation

{ TNextCloudSync }

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil,
    {$if (FPC_FULLVERSION=30200)}
	    opensslsockets,                 // only available in FPC320 and later
	{$endif}
	fphttpclient, httpprotocol, {$ifdef LCL}
    lazlogger,
    {$endif}
    LazUTF8,                            // UTF8 Pos()
	fpopenssl, ssockets, DateUtils, import_notes,
    settings,                           // just for sett.helpnotepath to load nextcloud.md
    //dateutils,                           // just for DateTimeToUnix()
    commonmark;

const
  URL_SUFFIX = '/index.php/apps/notes/api/v1/notes';
  //USER = 'dbannon';         // not a long term solution, only works for people called dbannon !
  KEYNOTETITLE = 'NextCloud to tomboy-ng sync';
  KEYNOTETEXT  = #10#10'Please do not alter this note, its an important part of the tomboy-ng sync'#10'--------';
  KEYNOTEUUID  = #10#10'TOMBOY-NG UUID = ';




function TNextCloudSync.TestTransport(const WriteNewServerID: boolean) : TSyncAvailable;
            { Here we will try to contact the server, look for a note that records the ServerID,
              If we are doing a RepoJoin then ANewRepo will be set, if no KEYNOTE, generate a ServerID,
              send it to the remote NextCloud in a KEYNOTE. If there is already one there, SyncMisMatch
              Might return SyncReady, SyncCredentialError, SyncNetworkError, SyncNoRemoteRepo, SyncMisMatch }
var
    NoteMeta : TNoteInfoList;
    PNote : PNoteInfo;
    i : integer;
begin
    ThisSyncTime := 0;                                      // Well, it got to be set somewhere !
    Result := FormatTestURL();                              // 'adjusts' url and tries to use it.
    if Result <> SyncReady then exit;                       // Failed to find a workable form of URL
    ServerID := '';

    NoteMeta := TNoteInfoList.Create;
    try
	    GetListOfNotes(NoteMeta, '?exclude=modified,catagory,favorite,content');
	    for i := 0 to NoteMeta.count -1 do begin
	        PNote := NoteMeta[i];
	        if PNote^.Title = KEYNOTETITLE then
	            if GetServerID(PNote^.SID) then begin
                    result := SyncReady;
	                break;
			    end else ErrorString := 'TransNextCloud remote KEYNOTE does not have a GUID';
			//SayDebug(PNote^.Title);
		end;
	finally
        NoteMeta.Free;
	end;
    // OK, whether or not having a ServerID is a good thing depends on the mode we are in.
    if ANewRepo then begin
        if (ServerID = '') then
            if WriteANewServerID() then          // will have set ServerID if successful.
                Result := SyncReady
            else Result := SyncNoRemoteRepo
        else Result := SyncMismatch;            // We should not have a remote ServerID if making a new repo.
	end else begin                              // So, we are not making a new repo then.
        if (ServerID = '') then                 // Thats bad.
            Result := SyncNoRemoteRepo
        else Result := SyncReady;
	end;
    SayDebug('TestTransport is exiting with SyncReady = ' + booltostr((Result = SyncReady), True));
end;

function TNextCloudSync.SetTransport(ForceClean: boolean): TSyncAvailable;
begin
    { SyncNoLocal indicates there is no local manifest and therefore no sync setup.
      SyncReady  would indicate there is a local manifest and it may be viable
      but we cannot, at this stage, tell ? }
    if Fileexists(ConfigDir + 'manifest.xml') then
        result := SyncReady
    else Result := SyncNoLocal;
    if (ForceClean and (Result = Syncready)) then begin
        DeleteFile(ConfigDir + 'manifest.xml');
        DeleteFile(ConfigDir + 'manifest.xml-remote');
        DeleteFile(ConfigDir + 'manifest.xml-local');
        Result := SyncNoLocal;
        // ToDo : consider removing the remote KEYNOTE here ?  Possible ?  No, better to make user do it, to be sure.
	end;
end;

function TNextCloudSync.GetNewNotes(const NoteMeta: TNoteInfoList; const GetLCD: boolean): boolean;
            { gets a list of notes from NextCloud, we get only SID, Title and Modified at this stage.
              As NC always gives us its weak LCD, getLCD has no effect. RevNo is also irrelevant, set to 1.
              For notes NC thinks we don't need to sync we set Action to SyNothing, else its SyUnSet.
              However, NoteMeta.LastSyncDate (TDateTime) is important, it arrives here with last time
              in there and leaves, if successfull, with the current one. }
var
    UnixTimeCount : string;
begin
    UnixTimeCount := inttostr(DateTimeToUnix(NoteMeta.LastSyncDate));
    // 1603970525
    result := GetListOfNotes(NoteMeta, '?pruneBefore=' + UnixTimeCount + '&exclude=favorite,content,category');
    if Result then ReadRemoteManifest(NoteMeta);
    //Saydebug('TNextCloudSync.GetNewNotes = ' + inttostr(NoteMeta.Count));
end;

function TNextCloudSync.DownloadNotes(const DownLoads: TNoteInfoList): boolean;
var
    //PNote   : PNoteInfo;
    Strs    : TStringList;
    NRec    : TNoteRec;
    Imp     : TImportNotes;
    I       : integer = 0;
begin
    //for PNote in Downloads do begin
    while I < Downloads.Count do begin
        if Downloads[i]^.Action <> SyDownload then begin inc(i); continue; end;
        Strs := TStringList.create;
        try
            if Downloader(RemoteAddress + URL_SUFFIX + '/' + inttostr(Downloads[i]^.SID), Strs) then begin
                if ExtractJID(Strs.Text, NRec) > 0 then begin
                    Downloads[i]^.LastChange    := ModifiedToTBDate(NRec.modified);
                    Downloads[i]^.LastChangeGMT := UnixToDateTime(NRec.modified);
                    Strs.Free;
                    Strs := TStringList.Create;
                    Strs.text := StringReplace(NRec.Content,'\n',Lineending,[rfReplaceAll, rfIgnoreCase]);
                    if Strs.count > 1 then begin                                     // here, &, < and > are unescaped.
                        Strs.SaveToFile(inttostr(Downloads[i]^.SID) + '.md');        // ToDo : remove this
                        Imp := TImportNotes.create;
                        Imp.Mode     := 'markdown';
                        Imp.Notebook := NRec.category;      // here, &, < and > are unescaped.
                                                            // bug logged, a renamed NextCloud note does not get a new timestamp ?
                        Imp.LCDate   := Downloads[i]^.LastChange;
                        Imp.CrDate   := Downloads[i]^.CreateDate;
                        Imp.DestinationDir := NotesDir;
                        if not Imp.MDtoNote(Strs, Downloads[i]^.Title, Downloads[i]^.ID) then
                            Saydebug('ERROR, Nextcloud DownloadNotes, failed to convert from MD to Note SID='
                                        + inttostr(Downloads[i]^.SID) + ' ID=' + Downloads[i]^.ID, True);
                        Imp.free;
                        if not FileExistsUTF8(NotesDir + Downloads[i]^.ID + '.note') then begin
                            SayDebug('ERROR - NextCloud.DownloadNotes did not create '
                                        + NotesDir + Downloads[i]^.ID + '.note', True);
                            Downloads.Delete(i);
                            continue;
                        end;
                        // if we recieve an empty note or fail to convert to TB, we remove the entry, can try next time.
		            end else begin
                        Saydebug('ERROR, Nextcloud DownloadNotes, cannot convert from empty MD SID='
                                        + inttostr(Downloads[i]^.SID) + ' ID=' + Downloads[i]^.ID, True);
                        saydebug('[' + Strs.text + ']', True);
                        Downloads.Delete(i);
                        continue;
					end;
                    inc(i);
                end;
            end;
	  finally
            Strs.free;
	  end;
		// ToDo : if we get an error here and the note is not saved, must prevent it from appearing in RemoteManifest
        // else it will be treated as a DeleteRemote and deleted from NextCloud next time we sync !!!!!
        // maybe just remove the entry from MMD, that way, we might get it done next time ?
	end;                // end of while loop
    result := true;
end;

function TNextCloudSync.DeleteNote(const ID: string; const ExistRev: integer): boolean;
var
    Client: TFPHttpClient;              { yes, definitly needed here. }
    STRL  : TStringList;
begin
    Saydebug('Deleting note ' + ID);
    Result := False;
    Client := TFPHttpClient.Create(nil);
    STRL := TStringList.Create;
    Client.UserName := UserName;
    Client.Password := Password;
    Client.AllowRedirect := true;
    try
        try
            Client.Delete(RemoteAddress + '/' + URL_SUFFIX + '/' + ID, STRL);
            if Client.ResponseStatusCode = 200
                then Result := True
            else begin
                ErrorString := 'Network Error, Response Code ' + inttostr(Client.ResponseStatusCode);
                Saydebug(ErrorString, True);
                Saydebug('Called ' + RemoteAddress + '/' + URL_SUFFIX + '/' + ID, True);
			end;
		except
            on E: Exception do begin
                    ErrorString := 'TransNexCloud DeleteNote -Something bad happened ' + E.Message;
                    SayDebug('TransNexCloud DeleteNote -Something bad happened ' + E.Message, True);
                end;
		end;
	finally
        Client.Free;
        STRL.Free;
	end;
end;

function TNextCloudSync.UploadNotes(const Uploads: TNoteInfoList): boolean;
var
    I, SID : integer;
    NRec : TNoteRec;
    Notebook : string;
begin
    result := true;
    NRec.SID:=0;
    for I := 0 to Uploads.Count -1 do begin
        if UpLoads[i]^.Action in [SyUpLoadNew, SyUpLoadEdit] then begin
            if IsTemplate(UpLoads[i]^.ID, Notebook) then begin
                UpLoads[i]^.Action := SyTemplate;                   // we do not sync Templates to NextCloud
                continue;
			end;
            if Notebook <> '' then Notebook := StringToJSONString(Notebook);
			if  UpLoads[i]^.Action = SyUpLoadNew then
                SID := 0
            else SID :=  UpLoads[i]^.SID;
            NRec.modified := DateTimeToUnix(Uploads[i]^.LastChangeGMT);
            NRec.SID:= SID;
            if PushUpNote(UpLoads[i]^.ID, Notebook, NRec{, SID}) > 0 then begin;
                UpLoads[i]^.SID := NRec.SID;
                UpLoads[i]^.LastChange := ModifiedToTBDate(NRec.modified);
			end else begin
                SayDebug('ERROR - NextCloud UploadNotes, failed to push ' + UpLoads[i]^.ID);
                exit(false);
			end;
		end;
    end;
end;

function TNextCloudSync.UploadNotes(const Uploads: TStringList): boolean;
// We have no need for this function here. Use UploadNotes(TNoteInfoList)
begin
        result := false;
end;

function TNextCloudSync.DoRemoteManifest(const RemoteManifest: string): boolean;
                        // Lets assume we have no need for this function here.
begin
     result := true;
end;

function TNextCloudSync.DownLoadNote(const ID: string; const RevNo: Integer): string;
                        // Lets assume we have no need for this function here.
begin
     result := '';
end;


// -----------------------  Private Functions ---------------------


procedure TNextCloudSync.ReadRemoteManifest(NoteMeta : TNoteInfoList);
// The list arrives here with notes marked SyNothing or SyDownLoad.
// We iterate over the "remote" manifest, for each item look for an entry in the MMD.
// If its present in both MMD and the RemoteManifest but not in note dir, its a
// SyDeleteRemote. Else, if found, decide about Action, SyNothing, SyDownload.
var
    Doc : TXMLDocument;
    Node : TDOMNode;
    NodeList : TDOMNodeList;
    SID, ID, CrDate, LCDate : string;
    i : integer;
    pNote : PNoteInfo;
begin
    if FileExists(ConfigDir + 'manifest.xml-remote') then begin
        try
            try
                ReadXMLFile(Doc, ConfigDir + 'manifest.xml-remote');
                //Node := Doc.DocumentElement.FindNode('server-id');      // ToDo, check server ID matches, very bad things could happen ....
                //ServerID := Node.FirstChild.NodeValue;
                NodeList := Doc.DocumentElement.ChildNodes;
	            if assigned(NodeList) then begin
	                for i := 0 to NodeList.Count-1 do begin
	                    SID := ''; ID := ''; CrDate := '';
	                    Node := NodeList.Item[i].Attributes.GetNamedItem('sid');
	                    SID := Node.NodeValue;
	                    Node := NodeList.Item[i].Attributes.GetNamedItem('id');
	                    ID := Node.NodeValue;
	                    Node := NodeList.Item[i].Attributes.GetNamedItem('create-date');
	                    CrDate := Node.NodeValue;
	                    Node := NodeList.Item[i].Attributes.GetNamedItem('last-change-date');
	                    LCDate := Node.NodeValue;
	                    if (ID <> '') and (SID <> '') {and (CrDate <> '') and (LCDate <> '')} then begin
	                        pNote := NoteMeta.FindSID(strtoint(SID));
	                        if pNote <> nil then begin
                                // If here, its present in both NoteMeta and RemoteManifest. But if we
                                // don't have a title in Meta, we don't care. Action has already been set.
                                // if pNote^.Action = SyNothing then continue;    // ie to for i in notelist loop
	                            pNote^.CreateDate := CrDate;
                                pNote^.LastChange:= LCDate;
                                pNote^.LastChangeGMT := GetGMTFromStr(pNote^.LastChange);
                                pNote^.ID := ID;
                                {
                                if pNote^.Action = SyNothing then                 // it was there, has it been locally deleted ?
                                    if not FileExistsUTF8(NotesDir + ID + '.note') then begin
                                        pNote^.Action := SyDeleteRemote;
                                        pNote^.Title := GetRemoteTitle(pNote^.SID);     // We'll ask NextCloud
									end;
                                }               // this function is done by checklocaldeletes, it reads local mainfest.

						    end {else begin               // This id is present in remote manifest but not remote server, its a SyDeleteLocal
                                new(PNote);
                                pNote^.SID     := 0;     // irrelevent
                                pNote^.Action  := SyDeleteLocal;
                                pNote^.Deleted := True;
                                pNote^.ID      := ID;
                                pNote^.Title   := 'A doomed note';      // ToDo : must get title to display in report
                                NoteMeta.Add(pNote);
							end};                                       // That code is unnecessary, the function is done by sync.checkremotedeletes
						end else SayDebug('ERROR, bad data in remote mainfest ');
	                end;
	            end;
            except on EXMLReadError do
                ErrorString := 'Error reading the remote manifest, ' + ConfigDir + 'manifest.xml-remote';
            end;
        finally
            Doc.free;
        end;
    end;
    // If the above did not assign an ID but do have a SID, must be a new note or, if we also
    // also don't have a title (SyNothing), it can only be changed to SyDeleteRemote, don't bother.
    for pNote in NoteMeta do begin
        if (pNote^.ID = '') and (pNote^.SID <> 0) and (pNote^.Action <> SyNothing) then begin
            pNote^.ID := MakeGUID();
            pNote^.Action:= SyDownLoad;
            SayDebug('--- Changing ' + inttostr(pNote^.SID), True);
	    end;
        Saydebug('--- SID=' + inttostr(pNote^.SID) + ' Title=' + pNote^.Title, True);
        Saydebug('--- ACTION=' + NoteMeta.ActionName(pNote^.Action), True);

	end;


end;

function TNextCloudSync.FormatTestURL() : TSyncAvailable;
var
    MSG : string;
begin
    ErrorString := '';
    if (copy(RemoteAddress, 1, 4) <> 'http') then begin             // else user has provided the prefix
        RemoteAddress := 'http://' + RemoteAddress;                 // we try http first, must add https later
	end;
    if TestNextCloudConnection(MSG, True) then exit(SyncReady);     // All good
    if TestNextCloudConnection(MSG, False) then begin               // Just a credentials issue
            ErrorString:= 'Invalid Credentials';
            exit(SyncCredentialError);
	end;
    ErrorString := MSG;                                             // More serious network problem.
    exit(SyncNetworkError);
end;

function TNextCloudSync.WriteANewServerID() : boolean;
var
    GUID : TGUID;
    NRec : TNoteRec;
    StrList : TStringList;
begin
    NRec.modified:=0;
    NRec.SID:=0;
    StrList := TstringList.Create;
    if fileexistsUTF8(sett.HelpNotesPath + 'nextcloud.md') then begin
        StrList.LoadFromFile(sett.HelpNotesPath + 'nextcloud.md');
        saydebug('strlist count=' + inttostr(strlist.Count));
	end
	else
        Saydebug('WARNING - TNextCloudSync.WriteANewServerID - failed to load ' + sett.HelpNotesPath + 'nextcloud.md', true);
    CreateGUID(GUID);
    ServerID := copy(GUIDToString(GUID), 2, 36);      // it arrives here wrapped in {}
    saydebug('-------------------');
    saydebug(strlist.text);
    saydebug('-------------------');
    saydebug(StringToJSONString(KEYNOTETEXT + KEYNOTEUUID + ServerID + #10#10 + strlist.text));
    saydebug('-------------------');
    Result := PostNewNote(KEYNOTETITLE, ''
            , StringToJSONString(KEYNOTETEXT + KEYNOTEUUID + ServerID + #10#10 + strlist.text), NRec) > 0;
            //, KEYNOTETEXT + KEYNOTEUUID + ServerID{ + strlist.text}, NRec) > 0;
    StrList.free;
    if not Result then begin
        ServerID := '';
        saydebug('ERROR - TNextCloudSync.WriteANewServerID, failed to post a remote server ID note', True);
	end;
end;

function TNextCloudSync.GetServerID(SID : longint) : boolean;
var
    Strs : TStringList;
    jData : TJSONData;
    jObj  : TJSONObject;
    jStr : TJSONString;
    Content : string;
    UUIDIndex : integer;
    {TEMPMeta : TNoteInfoList;
    PNote : PNoteInfo;}

begin
    result := false;
    Saydebug('In GetServerID looking for ' + inttostr(SID));
    Strs := TStringList.create;
    try
        if Downloader(RemoteAddress + URL_SUFFIX + '/' + inttostr(SID), Strs) then begin
            //DumpJSON(Strs.Text);
            jData := GetJSON(Strs.text);
            jObj := TJSONObject(jData);
            if jObj.Find('content', jStr) then begin
                Content := jStr.asstring;
                UUIDIndex := pos(KEYNOTEUUID, Content);
                if UUIDIndex > -1 then
                    ServerID := copy(Content, UUIDIndex + length(KEYNOTEUUID), 36);
                if IDLooksOK(ServerID) then begin
                    Result := True;
                    SayDebug('Remote Server ID is ' + ServerID);
				end;
			end;
            jData.free;
		end;
	finally
        Strs.free;
	end;
end;

function TNextCloudSync.TestNextCloudConnection(out MSG : string; const UsePassword : boolean) : boolean;
{   If return false             - Invalid IP address - Socket Error : Connection to 10.85.2.3:80 timed out
                    Something there but no Nextcloud - Socket Error : Connect to 192.168.1.250:80 failed.

So, call this initially with True, if it works, probably so will everything else.  If it fails with "Socket
Error ..." tell user to check address.  If it fails with "Invalid logon credentials" first try without
the credentials, if that works, then check user:password.  If both fail, its some NextCloud issue .... }

var
    Client: TFPHttpClient;
    SomeStrings : TStringList;
begin
    Result := false;
    Client := TFPHttpClient.Create(nil);
    if UsePassword then begin
        Client.UserName := UserName;
        Client.Password := Password;
    end;
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    Client.AddHeader('OCS-APIRequest', 'true');
    Client.AddHeader('Accept', 'application/json');
    Client.AllowRedirect := true;
    SomeStrings := TStringList.create;
    try
        try
            Client.Get(RemoteAddress + '/ocs/v1.php/cloud/capabilities', SomeStrings);
        except
            on E: EInOutError do begin
                SayDebug('TransNexCloud Downloader - InOutError ' + E.Message, True);
                exit(false);
                end;
            on E: ESSL do begin
                SayDebug('TransNexCloud Downloader -SSLError ' + E.Message, True);
                exit(false);
                end;
            on E: ESocketError do begin
                MSG := 'Socket Error : ' + E.Message;
                exit(false);
                end;
            on E: Exception do begin
                MSG := 'Something bad happened : ' + E.Message;
                exit(false);
                end;
        end;
        if UsePassword then begin    // we will still have content but not some specific things
            if (SomeStrings.Count > 0) and (pos('password_policy', SomeStrings.Text) > 0) then begin
                Result := True;
                MSG := '';
			end else Msg := 'Invalid logon credentials'
        end else begin
            if (SomeStrings.Count > 0) and (pos('{"ocs":{"meta"', SomeStrings.Text) > 0) then begin
                    Result := True;
                    MSG := '';
            end;
        end;
    finally
        SomeStrings.free;
        Client.Free;
    end;
end;


function TNextCloudSync.Downloader(URL : string; SomeStrings : TStringList; GetModifiedTime : boolean = false) : boolean;
var
    Client: TFPHttpClient;
begin
    // Windows can be made work with this if we push out ssl dll - see DownloaderSSL local project
    //InitSSLInterface;
    Client := TFPHttpClient.Create(nil);
    Client.UserName := UserName;
    Client.Password := Password;
    Client.AllowRedirect := true;
    try
        try
            Client.Get(URL, SomeStrings);
        except
            on E: EInOutError do begin
                SayDebug('TransNexCloud Downloader - InOutError ' + E.Message, True);
                exit(false);
                end;
            on E: ESSL do begin
                SayDebug('TransNexCloud Downloader -SSLError ' + E.Message, True);
                exit(false);
                end;
            on E: Exception do begin
                SayDebug('TransNexCloud Downloader -Something bad happened ' + E.Message, True);
                SayDebug('URL was ' + URL, True);
                SayDebug('U=' + UserName {+ 'P=' + Password});
                exit(false);
                end;
        end;
        if GetModifiedTime then
            with Client.ResponseHeaders do
	                SomeStrings.Insert(0, ValueFromIndex[IndexOfName('Last-Modified')]);
            { SayDebug('Value ' + , True);
            SayDebug('HEADER : ' + Client.ResponseHeaders.Text, True);            // ToDo : remove
            SayDebug('===== INDEX ===== : ' +  inttostr(Client.ResponseHeaders.IndexOfName('Last-Modified')), True);
            end; }
        //end;
    finally
        Client.Free;
    end;
    //ErrorMsg := '';
    result := true;
end;



function ModifiedToTBDate(Modified: longint): string;
var
    DT : TDateTime;
    Off : longint;
    Res : string;
begin
    // modified time is in GMT or UTC. So, need to correct for time zone.
    Off := GetLocalTimeOffset();                    // local time offset in minutes
    DT := UnixToDateTime(Modified - Off*60);        // Conv Off minutes to seconds and add it
    Result := FormatDateTime('YYYY-MM-DD', DT) + 'T'
        + FormatDateTime('hh:mm:ss.0000000"', DT);
    if (Off div -60) >= 0 then Res := '+'
    else Res := '-';
    if abs(Off div -60) < 10 then Res := Res + '0';
    Res := Res + inttostr(abs(Off div -60)) + ':';
        if (Off mod 60) = 0 then
            Res := res + '00'
    else Res := Res + inttostr(abs(Off mod 60));
    Result :=  Result + Res;                        // LastChange
end;

function TNextCloudSync.JData2NoteList(const NoteMeta: TNoteInfoList; jItm : TJSONData) : boolean;
var
    jObj : TJSONObject; jBool : TJSONBoolean; PNote : PNoteInfo; jNumb : TJSONNumber;
    jString : TJSONString;
begin
    Result := True;
    jObj := TJSONObject(jItm);
    // if JObj.Find('error', jBool) and (jBool.AsBoolean = false) then begin           // pruneBefore removes the error line !
    if not ((JObj.Find('error', jBool) and (jBool.AsBoolean = true))) then begin
            new(PNote);
            PNote^.ID := '';                                                        // ID
            if jObj.Find('modified', jNumb) then begin
                pNote^.LastChangeGMT := UnixToDateTime(jNumb.AsInteger);            // LastChangeGMT
                pNote^.LastChange := ModifiedToTBDate(jNumb.AsInteger);             // LastChange
                pNote^.CreateDate := pNote^.LastChange;                             // will update from local note if possible
			end else begin
                pNote^.LastChangeGMT := 0.0;
                pNote^.LastChange := '';
                pNote^.CreateDate := '';
			end;
            pNote^.Rev := 1;                                                        // Rev
            if JObj.Find('id', JNumb) then
                PNote^.SID := JNumb.asInteger
            else PNote^.SID := 0;                                                   // SID
            PNote^.Action:= SyDownLoad;                                             // a default, will change if no Title provided
            if jobj.Find('title', jString) then                                     // Title
                PNote^.Title := jString.AsString
            else begin
                PNote^.Title:= '';
                // If no title, then we must be in a pruneBefore and this pruned note is unchanged.
                // SyNothing can only be overridden by a SyDeleteRemote
                PNote^.Action:= SyNothing;
			end;
			NoteMeta.Add(PNote);
	end else result := false;
end;

// Gets a date like "Thu, 29 Oct 2020 08:48:21 GMT" and turns it into ISO 8601
function TNextCloudSync.SillyDateTo8601(SillyDate : string) : string;
var
    Bits : TStringArray;
begin
    Bits := SillyDate.Split(' ');
    Result := Bits[4] + '-';
    case Bits[3] of
        'Jan' : Result := Result + '01';
        'Feb' : Result := Result + '02';
        'Mar' : Result := Result + '03';
        'Apr' : Result := Result + '04';
        'May' : Result := Result + '05';
        'Jun' : Result := Result + '06';
        'Jul' : Result := Result + '07';
        'Aug' : Result := Result + '08';
        'Sep' : Result := Result + '09';
        'Oct' : Result := Result + '10';
        'Nov' : Result := Result + '11';
        'Dec' : Result := Result + '12';
	end;
    if length(Bits[2]) = 1 then
        Result := Result + '-0' + Bits[2] + 'T' + Bits[5] + 'Z'
    else Result := Result + '-' + Bits[2] + 'T' + Bits[5] + 'Z';
end;

function TNextCloudSync.GetListOfNotes(const NoteMeta: TNoteInfoList; Params : string = '') : boolean;
var
    SomeStrings : TStringList;
    jData : TJSONData;
    jItem : TJSONData;
    i : integer;
    ErrorMsg : string;
begin
    SomeStrings := TStringList.Create;
    if Downloader(RemoteAddress + URL_SUFFIX + Params,  SomeStrings, True) then begin
        //SayDebug('That looks good');
        if not SafeGetUTCfromStr(SillyDateTo8601(SomeStrings[0]), NoteMeta.LastSyncDate, ErrorMsg) then
            SayDebug('Failed to convert header Last-Modified [' + SomeStrings[0] + ']' + #10 + ErrorMsg);
        SomeStrings.Delete(0);              // First line hold the date, NOT JSON !!!!!!!!!!!
        //dumpJSON(SomeStrings.text);
        //SayDebug('That time was ' + inttostr(DateTimeToUnix(NoteMeta.LastSyncDate)), True);
        ThisSyncTime := DateTimeToUnix(NoteMeta.LastSyncDate);
        jData := GetJson(SomeStrings.text);
        for i := 0 to JData.Count -1 do begin
                jItem := jData.Items[i];
                JData2NoteList(NoteMeta, JItem);        // Will set the record to SyNothing if Title is not returned due to pruneBefore
		end;
        jData.free;
	end else
        SayDebug('TansNextCloud GetListOfNotes, failed', True);
    //SayDebug('transnextcloud, Called GetListOfNotes');
    SomeStrings.free;
    exit(true);
end;

function TNextCloudSync.GetRemoteTitle(SID: longint): string;
var
    SomeStrings : TStringList;
    jData : TJSONData;
    JObject : TJSONObject;
    jStr : TJSONString;
begin
    SomeStrings := TStringList.Create;
    if Downloader(RemoteAddress + URL_SUFFIX + '/' + inttostr(SID)
            + '?exclude=favorite,content,category,modified',  SomeStrings) then begin
        try
            try
                jData := GetJson(SomeStrings.text);
                jObject := TJSONObject(jData);
                if JObject.Find('title', jStr) then
                Result := jStr.AsString
                else Result := 'ERROR, failed to get title';
            except on E:Exception do Result := 'ERROR - failed to get title';    // Invalid JSON or ID not present
	    end;
	    finally
            JData.Free;
	    end;
	end;
    SomeStrings.Free;
end;


// -------------------- U P L O A D -----------------------


{ Sends a note up to NextCloud, default is a new note and a Post.
  if SID > 0, its to replace an existing note of that SID }
function TNextCloudSync.PushUpNote(ID, Notebook : string; var NRec : TNoteRec) : longint;
var
  STL : TStringList;
  Title : string = '';
  ENotes : TExportCommon;
begin
    ENotes := TExportCommon.Create;
    STL := TStringList.Create;
    try
    	ENotes.NotesDir := NotesDir;
        ENotes.GetMDcontent(ID, STL);
        if STL.Count > 1 then begin
            Title := StringToJSONString(STL[0]);
            Stl.Delete(0);
            Stl.Delete(0);
	    end;
        // STL.SaveToFile(Title + '.md');
        Result := PostNewNote(Title, Notebook, StringToJSONString(STL.Text), NRec);
	finally
        STL.Free;
        freeandnil(ENotes);
    end;
end;


procedure TNextCloudSync.DumpJSON(const St : string);
var
    jData : TJSONData;
begin
    SayDebug('---------- TEXT ------------');
    SayDebug(St);
    JData := GetJSON(St);
    SayDebug('---------- JSON ------------', true);
    SayDebug(jData.FormatJSON, true);
    SayDebug('----------------------------', true);
    JData.Free;
end;

                // Ret the (NextCloud) ID if its found in the passed (json) string, 0 else.
function TNextCloudSync.ExtractJID(data : string; out NRec : TNoteRec) : longint;
var
    JData : TJSONData;
    JObject : TJSONObject;
    JNumb : TJSONNumber;
    jStr : TJSONString;
begin
    try
	    try
            JData := GetJSON(Data);                         // requires a free
            JObject := TJSONObject(jData);                  // does not require a free
            // Result := JObject.Get('id');                 // will raise exceptions if not present, better to use Find
            NRec.SID := 0;
            NRec.modified:=0;
            if JObject.Find('id', jNumb) then
                NRec.SID := jNumb.AsInteger;
            if JObject.Find('modified', jNumb) then
                NRec.modified := jNumb.AsInteger;
            if JObject.Find('category', jStr) then
                NRec.category := JStr.AsString
            else NRec.category := '';                       // we may or may not have a category
            if jObject.Find('content', Jstr) then
                NRec.content:= jStr.AsString
            else NRec.content := '';                        // same with content
            result := NRec.SID;
	    //except on  E: EVariantTypeCastError ....
        except on E:Exception do Result := 0;               // Invalid JSON or ID not present
	    end;
	finally
        JData.Free;
	end;
end;


// Returns the NextCloud assigned NoteID on success, 0 on failure, must set a regional ErrorMessage
function TNextCloudSync.PostNewNote(Title, Notebook, Content : String; var NRec : TNoteRec) : longint;
var
    Client: TFPHttpClient;
    Response : TStringStream;
    BodySt : string = '';
begin
    Result := 0;
    saydebug('In PostNewNote');
    Client := TFPHttpClient.Create(nil);
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    Client.AddHeader('Content-Type','application/json; charset=UTF-8');
    Client.AddHeader('Accept', 'application/json');
    Client.AllowRedirect := true;
    Client.UserName:=UserName;
    Client.Password:=Password;

(*    if Notebook = '' then
        client.RequestBody := TRawByteStringStream.Create('{ "title": "'
                            + Title + '", "content": "' + content + '"}')
    else client.RequestBody := TRawByteStringStream.Create('{ "title": "' + Title
                    + '",  "category": "' + Notebook + '", "content": "' + content + '"}');     *)

    BodySt := '{ "title": "' + Title;
    if (NRec.modified <> 0) then
        BodySt := BodySt  + '", "modified": "' + inttostr(NRec.Modified);
    if (NoteBook <> '') then
        BodySt := BodySt + '", "category": "' + Notebook;
    client.RequestBody := TRawByteStringStream.Create(BodySt + '", "content": "' + content + '"}');

    // Saydebug('************** NextCloud POST Request Body [' + BodySt + '", "content": "' + content + '"}'   + ']', True);

    Response := TStringStream.Create('');
    try
        try
            if NRec.SID = 0 then begin
                saydebug('About to Post');
                client.Post(RemoteAddress + URL_SUFFIX, Response);  // don't use FormPost, it messes with the Content-Type value
            end else  begin
                saydebug('About to PUT');
                client.Put(RemoteAddress + URL_SUFFIX + '/' + inttostr(NRec.SID), Response);
            end;
            // saydebug('Posted');
            if Client.ResponseStatusCode = 200 then begin
                //if DebugMode then DumpJSON(Response.DataString);
                Result := ExtractJID(Response.DataString, NRec);
                SayDebug('Posted Note ID is ' + inttostr(Result));
			end else begin
			        SayDebug('ERROR - TNextCloudSync.PostNewNote - Failed PUT/POST, Response Code is '
                        + inttostr(Client.ResponseStatusCode), True);
                    SayDebug(Response.DataString, True);
			end;
		except on E:Exception do
                SayDebug('TransNext.PostNewNote bad things happened : ' + E.Message);
                // Set some sort of error message here.
        end;
    finally
        Client.RequestBody.Free;
        Client.Free;
        Response.Free;
    end;
end;

function TNextCloudSync.IsTemplate(const ID : string; out Notebook : String) : boolean;
var
    Doc : TXMLDocument;
	Node : TDOMNode;
    J : integer;
begin
    result := False;            // This is returned if there is not 'tags' present (or if the note is not present !)
    Notebook := '';
  	if FileExistsUTF8(NotesDir + ID + '.note') then begin
  	    try
	        try
	            ReadXMLFile(Doc, NotesDir + ID + '.note');
                Node := Doc.DocumentElement.FindNode('tags');
                if Assigned(Node) then begin
                    for J := 0 to Node.ChildNodes.Count-1 do        // forgetting notebooks, its a Template
                        if UTF8Pos('system:template', Node.ChildNodes.Item[J].TextContent) > 0 then
                                exit(True);
                    for J := 0 to Node.ChildNodes.Count-1 do
                        if UTF8pos('system:notebook', Node.ChildNodes.Item[J].TextContent) > 0 then begin
                            Notebook := UTF8Copy(Node.ChildNodes.Item[J].TextContent, 17, 1000);
                            exit(False);                             // We are interested in only the first one we find.
                        end;
				end;
            except 	on E: Exception do SayDebug('NextCloud IsTemplate - XML ERROR reading ' + NotesDir + ID + '.note' + ' ' + E.Message);
            end;
  	    finally
      	    Doc.free;
  	    end;
    end else SayDebug('NextCloud IsTemplate - Error, Cannot locate note ID=' + NotesDir + ID + '.note');
end;

procedure TNextCloudSync.SayDebug(st: string; Always : boolean = false);
begin
    if not (DebugMode or Always) then exit;
    {$ifdef LCL}Debugln{$else}writeln{$endif}(St);
end;


end.

