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
    import_notes, JsonTools, kmemo2pdf, tb_symbol, fpTTF, uQt_Colors
    , LazVersion, Backlinks
    {$ifdef LCLGTK2}
    , unitywsctrls          // only safe to use in gtk2, use it if we need it or not
    {$endif};

{$R *.res}

{$if defined(LCLGTK2) and (laz_major = 3) }        // defined in LazVersion unit
{$define APPINDPATCH}                               // Note: IDE greys incorrectly
{$endif}


// ----------------------------------------------------------------------------
//   This about reading the command line and env for instructions re TrayIcon
//   It applies to ONLY gtk2 and Lazarus >= 3.0.0 (confirm that) !
// ----------------------------------------------------------------------------
{$ifdef APPINDPATCH}
 function GetUseAppInd() : UnityWSCtrls.TUseAppIndInstruction;
 var
    EnvVar : string;
 begin
 Result := UnityWSCtrls.GlobalUseAppInd;
 if Application.HasOption('useappind') then begin              // Command line
    if Application.GetOptionValue('useappind') = 'yes' then    // if not set, leave it alone.
        Result := UseAppIndYes
    else if Application.GetOptionValue('useappind') = 'no' then
        Result := UseAppIndNo;                            // Anything other than yes or no is ignored
 end else begin
    EnvVar := Application.EnvironmentVariable['LAZUSEAPPIND'];   // EnvironmentVariable
    if EnvVar = 'YES' then
        Result := UseAppIndYes
    else if EnvVar = 'NO' then
        Result := UseAppIndNo;
    end;
end;
{$endif}


begin
    Application.Scaled:=True;
    Application.Title:='tomboy-ng';
    RequireDerivedFormResource:=True;
    Application.Initialize;

    if ContinueToGUI then begin

        {$ifdef APPINDPATCH}
        {$ifdef CPUi386}             // Note: unless Ayatana fix their problem, no option for Gnome users
                                    // https://github.com/AyatanaIndicators/libayatana-appindicator/issues/76
        UnityWSCtrls.GlobalUseAppInd := UnityWSCtrls.UseAppIndNo;     // 32bit must be a 'no'.
        debugln('Tomboy_NG.lpr : Deciding to set UseAppInd to no to prevent AV.');
        debugln('You may over rule that with --useappind=yes to see what happens.');
        {$endif}
        UnityWSCtrls.GlobalUseAppInd := GetUseAppInd();   // Set before creating TrayIcon
        {$endif}
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

