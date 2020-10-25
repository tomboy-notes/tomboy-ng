program Tomboy_NG;

{    Copyright (C) 2017-2020 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------
    History
	27/12/2017 - Altered order to make the settings form the main one instead of RTSearch
}

{$mode objfpc}{$H+}

uses
    {$DEFINE UseCThreads}
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cmem, cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    LCLProc, Forms, Dialogs, printer4lazarus, SearchUnit, settings, SyncGUI,
    Notebook, Spelling, Mainunit, BackupView, recover, tomdroid, markdown,
    Index, autostart, hunspell, sync, syncutils, transandroid,
    ResourceStr, SyncError, colours, cli, notifier, RollBack;

{$R *.res}

begin
    Application.Scaled:=True;
    Application.Title:='tomboy-ng';
    RequireDerivedFormResource:=True;
    Application.Initialize;

    if ContinueToGUI then begin
        Application.CreateForm(TMainForm, MainForm);
        Application.CreateForm(TSett, Sett);
        Application.CreateForm(TSearchForm, SearchForm);
        Application.CreateForm(TFormSync, FormSync);
        Application.CreateForm(TFormTomdroid, FormTomdroid);
        Application.CreateForm(TFormMarkdown, FormMarkdown);
        Application.CreateForm(TFormSyncError, FormSyncError);
        Application.CreateForm(TFormColours, FormColours);
		Application.CreateForm(TFormRollBack, FormRollBack);
        // Application.CreateForm(TNoteBookPick, NoteBookPick);
        // Application.CreateForm(TFormSpell, FormSpell);
        // Application.CreateForm(TEditBoxForm, EditBoxForm);
        Application.Run;
    end;
end.

