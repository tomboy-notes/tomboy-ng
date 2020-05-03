unit SyncGUI;
{
 * Copyright (C) 2017 David Bannon
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

{	History
	2017/12/06	Marked FileSync debug mode off to quieten console output a little
	2017/12/30  Changed above DebugMode to VerboseMode
	2017/12/30  We now call IndexNotes() after a sync. Potentially slow.
	2017/12/30  Added a seperate procedure to do manual Sync, its called
				by a timer to ensure we can see dialog before it starts.
	2018/01/01  Added ID in sync report to make it easier to track errors.
	2018/01/01  Set goThumbTracking true so contents of scroll box glide past as
    			you move the "Thumb Slide".
	2018/01/01  Changed ModalResult for cancel button to mrCancel
	2018/01/08  Tidied up message box text displayed when a sync conflict happens.
	2018/01/25  Changes to support Notebooks
    2018/01/04  Forced a screen update before manual sync so user knows whats happening.
    2018/04/12  Added ability to call MarkNoteReadOnly() to cover case where user has unchanged
                note open while sync process downloads or deletes that note from disk.
    2018/04/13  Taught MarkNoteReadOnly() to also delete ref in NoteLister to a sync deleted note
    2018/05/12  Extensive changes - MainUnit is now just that. Only change here relates
                to naming of MainUnit and SearchUnit.
    2018/05/21  Show any sync errors as hints in the StringGrid.
    2018/06/02  Honor a cli --debug-sync
    2018/06/14  Update labels when transitioning from Testing Sync to Manual Sync
    2018/08/14  Added SDiff to replace clumbsy dialog when sync clash happens.
    2018/08/18  Improved test/reporting of file access during sync
    2018/10/25  New sync model. Much testing, support for Tomdroid.
    2018/10/28  Much tweaking and bug fixing.
    2018/10/29  Tell TB_Sdiff about note title before showing it.
    2018/10/30  Don't show SyNothing in sync report
    2018/11/04  Added support to update in memory NoteList after a sync.
    2019/05/19  Display strings all (?) moved to resourcestrings
    2020/02/20  Added capability to sync without showing GUI.
}

{$mode objfpc}{$H+}




interface

uses
		Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
		StdCtrls, Grids, Syncutils, LazFileUtils, settings;

type

		{ TFormSync }

  TFormSync = class(TForm)
		ButtonSave: TButton;
		ButtonCancel: TButton;
		ButtonClose: TButton;
		Label1: TLabel;
		Label2: TLabel;
		Memo1: TMemo;
		Panel1: TPanel;
		Panel2: TPanel;
		Panel3: TPanel;
		Splitter3: TSplitter;
		StringGridReport: TStringGrid;
                                        { Runs a sync without showing form. Ret False if error or its not setup.
                                          Caller must ensure that Sync is config and that the Sync dir is available.
                                          If clash, user will see dialog. }
                function RunSyncHidden() : boolean;
		procedure ButtonCancelClick(Sender: TObject);
		procedure ButtonCloseClick(Sender: TObject);
    		procedure ButtonSaveClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
                procedure FormHide(Sender: TObject);
                { At Show, depending on SetUpSync, we'll either go ahead and do it, any
                  error is fatal or, if True, walk user through process. }
				procedure FormShow(Sender: TObject);
                procedure StringGridReportGetCellHint(Sender: TObject; ACol, ARow: Integer;
                                  var HintText: String);
		private
                FormShown : boolean;
                LocalTimer : TTimer;
                procedure AfterShown(Sender : TObject);
                    // Display a summary of sync actions to user.
                function DisplaySync(): string;
                    { Called when user wants to join a (possibly uninitialised) Repo,
                      will handle some problems with user's help. }
                procedure DrySync();
                    { Called to do a sync assuming its all setup. Any problem is fatal }
                function DoSync(): boolean;
                    { Populates the string grid with details of notes to be actioned }
                procedure ShowReport;
            	//procedure TestRepo();
        		//procedure DoSetUp();

		public
                Busy : boolean; // indicates that there is some sort of sync in process now.

                LocalConfig, NoteDirectory : ANSIString;

                procedure MarkNoteReadOnly(const Filename : string; const WasDeleted : Boolean = False);
                    { we will pass address of this function to Sync }
                function DefineDefaultAction(const ClashRec : TClashRecord) : TSyncAction;
		end;

var
		FormSync: TFormSync;

implementation

{ In SetupFileSync mode, does a superficial, non writing, test OnShow() user
  can then click 'OK' and we'd do a real sync, exit and settings saved in calling
  process.
}

uses LazLogger, SearchUnit, TB_SDiff, Sync,  LCLType, SyncError, ResourceStr;

{$R *.lfm}

var
        ASync : TSync;
{ TFormSync }

procedure TFormSync.MarkNoteReadOnly(const Filename : string; const WasDeleted : Boolean = False);
begin
    SearchForm.MarkNoteReadOnly(FileName, WasDeleted);
end;

function TFormSync.DefineDefaultAction(const ClashRec : TClashRecord) : TSyncAction;
var
    SDiff : TFormSDiff;
begin

    SDiff := TFormSDiff.Create(self);

    SDiff.NoteID.Caption := 'Note ID ; '+ ClashRec.LocalNote^.ID;
    SDiff.TitleLocal.Caption := ClashRec.LocalNote^.Title;
    SDiff.ChangeLocal.Caption := ClashRec.LocalNote^.LastChange;
    SDiff.TitleRemote.Caption := ClashRec.RemoteNote^.Title;
    SDiff.ChangeRemote.Caption := ClashRec.RemoteNote^.LastChange;

    SDiff.MemoLocal.ReadOnly := true;
    SDiff.MemoLocal.Text := SDiff.RemoveXml(ClashRec.LocalNote^.Content);
    SDiff.MemoRemote.ReadOnly := true;
    SDiff.MemoRemote.Text := SDiff.RemoveXml(ClashRec.RemoteNote^.Content);

    case SDiff.ShowModal of
            mrYes      : Result := SynDownLoad;
            mrNo       : Result := SynUpLoadEdit;
            mrNoToAll  : Result := SynAllLocal;
            mrYesToAll : Result := SynAllRemote;
            mrAll      : Result := SynAllNewest;
            mrRetry    : Result := SynAllCopy;
            mrClose    : Result := SynAllOldest;
    else
            Result := SynUnSet;   // Should not get there
    end;
    SDiff.Free;
    Application.ProcessMessages;
end;

procedure TFormSync.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    FreeandNil(ASync);
    Busy := False;
end;

procedure TFormSync.FormHide(Sender: TObject);
begin
    if LocalTimer = Nil then exit();
    LocalTimer.Free;
    LocalTimer := nil;
end;


// Following resourcestrings defined in syncUtils.pas

function TFormSync.DisplaySync(): string;
var
    UpNew, UpEdit, Down, DelLoc, DelRem, CreateCopy, DoNothing, Undecided : integer;
begin
    ASync.ReportMetaData(UpNew, UpEdit, Down, DelLoc, DelRem, CreateCopy, DoNothing, Undecided);
    Memo1.Append(rsNewUploads + inttostr(UpNew));
    Memo1.Append(rsEditUploads + inttostr(UpEdit));
    Memo1.Append(rsDownloads + inttostr(Down));
    Memo1.Append(rsLocalDeletes + inttostr(DelLoc));
    Memo1.Append(rsRemoteDeletes + inttostr(DelRem));
    Memo1.Append(rsSynCopies + inttostr(CreateCopy));
    Memo1.Append(rsDoNothing + inttostr(DoNothing));
    Memo1.Append(rsUndecided + inttostr(Undecided));
    result := 'Uploads=' + inttostr(UpNew+UpEdit) + ' downloads=' + inttostr(Down) + ' deletes=' + inttostr(DelLoc + DelRem);
    // debugln('Display Sync called, DoNothings is ' + inttostr(DoNothing));
end;

    // User is only allowed to press Cancel or Save when this is finished.
procedure TFormSync.DrySync();
var
    SyncAvail : TSyncAvailable;
begin
    debugln('DrySync');

    freeandnil(ASync);

    NoteDirectory := Sett.NoteDirectory;
    LocalConfig := AppendPathDelim(Sett.LocalConfig);

    ASync := TSync.Create;
    Label1.Caption:= rsTestingRepo;
    Application.ProcessMessages;
    ASync.ClashFunction:= @DefineDefaultAction;
    ASync.NotesDir:= NoteDirectory;
    ASync.ConfigDir := LocalConfig;
    Async.SetTransport(Sett.getSyncType());
    SyncAvail := ASync.TestConnection();
    if SyncAvail <> SyncReady then begin
        showmessage(rsUnableToProceed + ' ' + ASync.ErrorString);
        ModalResult := mrCancel;
        exit;
    end;
    Label1.Caption:=rsLookingatNotes;
    Application.ProcessMessages;
    if ASync.StartSync(true) then begin
        DisplaySync();
        ShowReport();
        Label1.Caption:=rsLookingatNotes;
        Label2.Caption := rsSaveAndSync;
        ButtonSave.Enabled := True;
    end  else
        Showmessage(rsSyncError + ' ' + ASync.ErrorString);
    ButtonCancel.Enabled := True;
end;

procedure TFormSync.AfterShown(Sender : TObject);
begin
    LocalTimer.Enabled := False;             // Dont want to hear from you again
    if not Sett.getSyncTested() then begin
        DrySync();
    end else
        DoSync();
end;

//RESOURCESTRING
//  rsPleaseWait = 'Please wait a minute or two ...';

procedure TFormSync.FormShow(Sender: TObject);
begin
    Busy := True;
    Left := 55 + random(55);
    Top := 55 + random(55);
    FormShown := False;
    Label2.Caption := rsNextBitSlow;
    Memo1.Clear;
    StringGridReport.Clear;
    ButtonSave.Enabled := False;
    ButtonClose.Enabled := False;
    ButtonCancel.Enabled := False;
    // We call a timer to get out of OnShow so ProcessMessages works as expected
    LocalTimer := TTimer.Create(Nil);
    LocalTimer.OnTimer:= @AfterShown;
    LocalTimer.Interval:=500;
    LocalTimer.Enabled := True;
end;

function TFormSync.RunSyncHidden(): boolean;
begin
    //debugln('In RunSyncHidden');
    if not Sett.getSyncTested() then exit(False);      // should never call this in setup mode but to be sure ...

    busy := true;
    StringGridReport.Clear;

    Result := DoSync();
end;

        // User is only allowed to press Close when this is finished.
function TFormSync.DoSync() : boolean;
var
    SyncState : TSyncAvailable = SyncNotYet;
begin
    debugln('DoSync');

    Label1.Caption := rsTestingSync;
    Application.ProcessMessages;

    NoteDirectory := Sett.NoteDirectory;
    LocalConfig := AppendPathDelim(Sett.LocalConfig);

    ASync := TSync.Create;


    try
        ASync.ClashFunction:= @DefineDefaultAction;
        ASync.NotesDir:= NoteDirectory;
	ASync.ConfigDir := LocalConfig;
        Async.SetTransport(Sett.getSyncType());
        SyncState := ASync.TestConnection();
        while SyncState <> SyncReady do begin
            debugln('Failed testConnection');

            FormSyncError.Label1.caption := rsUnableToSync + ':';
            FormSyncError.label3.caption := ASync.ErrorString;
            FormSyncError.ButtRetry.Visible := not Visible;
            ModalResult := FormSyncError.ShowModal;
            if ModalResult = mrCancel then exit(false);

            SyncState := ASync.TestConnection();
        end;
        Label1.Caption:= rsRunningSync;
        Application.ProcessMessages;
        //ASync.TestRun := False;
        ASync.StartSync(false);
        SearchForm.UpdateSyncStatus(rsLastSync + ' ' + FormatDateTime('YYYY-MM-DD hh:mm', now)  + ' ' + DisplaySync());
        ShowReport();
        SearchForm.ProcessSyncUpdates(Async.DeletedList, Async.DownList);
        Label1.Caption:=rsAllDone;
        Label2.Caption := rsPressClose;
        ButtonClose.Enabled := True;
        Result := True;
    finally
        FreeandNil(ASync);
        Busy := False;
    end;
end;

procedure TFormSync.ShowReport;

var
    Rows,i : integer;
begin
    StringGridReport.Clean;
    i := 0;
    Rows :=0;
    while (i<Async.GridReportList.Count) do
    begin
        StringGridReport.InsertRowWithValues(
            Rows,
            [Async.GridReportList.Strings[i],
            Async.GridReportList.Strings[i+1],
            Async.GridReportList.Strings[i+2]]);
        inc(Rows);
        i := i+3;
    end;

    StringGridReport.AutoSizeColumn(0);
    StringGridReport.AutoSizeColumn(1);

    if  Rows = 0
    then Memo1.Append(rsNoNotesNeededSync)
    else Memo1.Append(inttostr(Rows) + rsNotesWereDealt);
end;

procedure TFormSync.StringGridReportGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
begin
// HintText := FileSync.ReportList.Items[ARow]^.Message;
end;

procedure TFormSync.ButtonCancelClick(Sender: TObject);
begin
    ModalResult := mrCancel;
end;

procedure TFormSync.ButtonCloseClick(Sender: TObject);
begin
	ModalResult := mrOK;
end;

procedure TFormSync.ButtonSaveClick(Sender: TObject);
begin
    Label2.Caption:=rsNextBitSlow;
    Label1.Caption:='First Time Sync';
    Memo1.Clear;
    Application.ProcessMessages;
    ButtonCancel.Enabled := False;
    ButtonSave.Enabled := False;
    //ASync.TestRun := False;
    if ASync.StartSync(false) then begin
        SearchForm.UpdateSyncStatus(rsLastSync + ' ' + FormatDateTime('YYYY-MM-DD hh:mm', now)  + ' ' + DisplaySync());
        ShowReport();
        SearchForm.ProcessSyncUpdates(Async.DeletedList, Async.DownList);
        Label1.Caption:=rsAllDone;
        Label2.Caption := rsPressClose;
	Sett.setSyncTested(true);
    end  else
        Showmessage(rsSyncError + ASync.ErrorString);
    ButtonClose.Enabled := True;
end;

end.

