program Tomboy_NG;

{    Copyright (C) 2017-2022 David Bannon

    License:
    This code is licensed under BSD 3-Clause Clear License, see file License.txt
    or https://spdx.org/licenses/BSD-3-Clause-Clear.html

    ------------------
    History
	27/12/2017 - Altered order to make the settings form the main one instead of RTSearch
}

{$mode objfpc}{$H+}

{$define TOMBOY_NG}

uses
    {$DEFINE UseCThreads}
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cmem, cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    LCLProc, Forms, Dialogs, printer4lazarus, SearchUnit, settings, SyncGUI,
    Notebook, Spelling, Mainunit, BackupView, recover, Index,
    autostart, hunspell, sync, syncutils, ResourceStr, colours,
    cli, RollBack, commonmark, notenormal, transgithub,
    import_notes, JsonTools, kmemo2pdf, tb_symbol, fpTTF, uQt_Colors;

{$R *.res}


begin
    Application.Scaled := True;
    Application.Title := 'tomboy-ng';
    RequireDerivedFormResource:=True;
    Application.Initialize;

    if ContinueToGUI then begin
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TFormSymbol, FormSymbol);
    Application.CreateForm(TSett, Sett);
    Application.CreateForm(TSearchForm, SearchForm);
    Application.CreateForm(TFormSync, FormSync);
    Application.CreateForm(TFormColours, FormColours);
    Application.CreateForm(TFormRollBack, FormRollBack);
        Application.Run;
    end;

end.

