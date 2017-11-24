program Tomboy_NG;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, MainUnit, { EditBox, SaveNote, LoadNote,} settings, SyncGUI
    { you can add units after this };

{$R *.res}

begin
	Application.Title:='tomboy-ng';
    RequireDerivedFormResource:=True;
    Application.Initialize;
    Application.CreateForm(TRTSearch, RTSearch);
    Application.CreateForm(TSett, Sett);
	Application.CreateForm(TFormSync, FormSync);
    // Application.CreateForm(TEditBoxForm, EditBoxForm);
    Application.Run;
end.

