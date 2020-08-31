program TomboyTools;

{ License - see tomboy-ng license information }

{$mode objfpc}{$H+}

uses
        {$IFDEF UNIX}{$IFDEF UseCThreads}
        cthreads,
        {$ENDIF}{$ENDIF}
        Interfaces, // this includes the LCL widgetset
        Forms, main, cmdline, export_notes, ttutils, import_notes, nextcloud
        { you can add units after this };

{$R *.res}

begin
        if Finished() then exit;
        RequireDerivedFormResource:=True;
		Application.Scaled:=True;
        Application.Initialize;
		Application.CreateForm(TFormMain, FormMain);
        Application.Run;
end.

