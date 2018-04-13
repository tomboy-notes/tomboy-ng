program Tomboy_NG;

{ History
	27/12/2017 - Altered order to make the settings form the main one instead of RTSearch
}

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, MainUnit, settings, SyncGUI, Notebook, Spelling
    { you can add units after this };

{$R *.res}

begin
	Application.Title:='tomboy-ng';
    RequireDerivedFormResource:=True;
    Application.Initialize;
    Application.ShowMainForm := False;
    Application.CreateForm(TSett, Sett);
    Application.CreateForm(TRTSearch, RTSearch);
	Application.CreateForm(TFormSync, FormSync);
	// Application.CreateForm(TNoteBookPick, NoteBookPick);
    // Application.CreateForm(TFormSpell, FormSpell);
    // Application.CreateForm(TEditBoxForm, EditBoxForm);
    Application.Run;
end.

