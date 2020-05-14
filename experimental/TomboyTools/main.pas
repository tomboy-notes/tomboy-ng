unit main;

{$mode objfpc}{$H+} {$assertions on}

{ License - see tomboy-ng license information }

interface

uses
        Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
		ExtCtrls, Buttons, StdCtrls, CheckLst;

type

		{ TFormMain }

        TFormMain = class(TForm)
				CheckListBox1: TCheckListBox;
				ComboExportMode: TComboBox;
				ComboExport: TComboBox;
				ComboSource: TComboBox;
				Label1: TLabel;
				Label2: TLabel;
				Label3: TLabel;
				Label4: TLabel;
				Label5: TLabel;
				Label6: TLabel;
                LabelVersion: TLabel;
                LabelErrorMessage: TLabel;
				LabelSourcePrompt: TLabel;
				LabelSource: TLabel;
				LabelDestinationPrompt: TLabel;
				LabelDestination: TLabel;
				PageControl1: TPageControl;
				PanelTop: TPanel;
				SelectDirectoryDialog1: TSelectDirectoryDialog;
				SpeedExit: TSpeedButton;
				SpeedProceed: TSpeedButton;
				SpeedSetSource: TSpeedButton;
				SpeedSetDestination: TSpeedButton;
				StatusBar1: TStatusBar;
				TabExport: TTabSheet;
				TabImport: TTabSheet;
				procedure CheckListBox1ItemClick(Sender: TObject; Index: integer);
                procedure ComboExportChange(Sender: TObject);
				procedure ComboExportModeChange(Sender: TObject);
                procedure ComboSourceChange(Sender: TObject);
                procedure FormShow(Sender: TObject);
				procedure SpeedExitClick(Sender: TObject);
				procedure SpeedProceedClick(Sender: TObject);
    			procedure SpeedSetDestinationClick(Sender: TObject);
				procedure SpeedSetSourceClick(Sender: TObject);
        private
				procedure SetUpSource(Mode: integer);
				procedure SetUpNoteBook();
				function GetNoteBooks(): integer;
				function ReadyToGo(): boolean;
				function SetExportSource(SDir: string): boolean;

        public

        end;


var
        FormMain: TFormMain;

implementation

uses cmdline, FileUtil, LazFileUtils, ttutils, export_notes;

const

    cbNG = 0;            // The selected index ComboSource, must agree with order of strings in Combo, set in ObjectInspector
    cbTB = 1;
    cbManual = 2;

    cbDirectory = 0;    // Export Mode combobox, must agree with order of strings in Combo, set in ObjectInspector
    cbBook = 1;


{$R *.lfm}

{ TFormMain }


procedure TFormMain.SetUpSource(Mode : integer);
begin
    ComboSource.Enabled := true;
    case Mode of
        cbNG : if SetExportSource(GetDefaultNoteDir()) then
                        ComboSource.ItemIndex := cbNG
                    else if SetExportSource(GetDefaultNoteDir(True)) then
                        ComboSource.ItemIndex := cbTB
                        else ComboSource.ItemIndex := cbManual;
        cbTB :  if SetExportSource(GetDefaultNoteDir(True)) then
                        ComboSource.ItemIndex := cbTB
                        else ComboSource.ItemIndex := cbManual;
        cbManual : ComboSource.ItemIndex := cbManual;               // which it is anyway
	end;

    SpeedSetSource.Enabled := not (comboSource.ItemIndex = cbNG) or (comboSource.ItemIndex = cbTB);

    //LabelSource.caption := '';
    CheckListBox1.enabled := false;
    if ComboExportMode.ItemIndex = cbBook then
           SetUpNoteBook();
    ReadyToGo();
end;

procedure TFormMain.SetUpNoteBook() ;
begin
    CheckListBox1.Enabled := true;
    if GetNoteBooks() = 0 then
        showmessage('That dir has not notes in notebooks');
end;

function TFormMain.ReadyToGo() : boolean;
begin
    Result :=   (LabelSource.Caption <> '') and
                (LabelDestination.Caption <> '') and
                ( (ComboExportMode.ItemIndex <> cbBook) or (CheckListBox1.ItemIndex <> -1) )
                ;

    SpeedProceed.Enabled := Result;
end;


procedure TFormMain.FormShow(Sender: TObject);
begin
    LabelErrorMessage.Caption := '';
    LabelDestination.caption := '';
    SetUpSource(cbNG);   // will try NG first, then TB, then fall back to manual
    ReadyToGo();
end;

procedure TFormMain.SpeedExitClick(Sender: TObject);
begin
        close;
end;

procedure TFormMain.SpeedProceedClick(Sender: TObject);
var
    Exporter : TExportNote;
begin
    StatusBar1.SimpleText:= 'processing notes, please wait ....';
    LabelErrorMessage.Caption := '';
    Application.ProcessMessages;
    Exporter := TExportNote.Create;
    Exporter.DestDir := LabelDestination.Caption;
    Exporter.NoteDir := LabelSource.caption;
    Exporter.OutFormat := ComboExport.Text;
    try
        case comboExportMode.itemIndex of
            cbDirectory : Exporter.AllNotes := True;
			cbBook : Exporter.Notebook := CheckListBox1.Items[CheckListBox1.ItemIndex];
		end;
        Exporter.Execute();
        if Exporter.ErrorMessage <> '' then begin
            showmessage(Exporter.ErrorMessage);
            LabelErrorMessage.Caption := Exporter.ErrorMessage;
        end;
        StatusBar1.SimpleText:= inttostr(Exporter.NotesProcessed) + ' notes processed.';
	finally
        Exporter.Free;
	end;
end;

procedure TFormMain.ComboSourceChange(Sender: TObject);
begin
        SetUpSource(ComboSource.ItemIndex);
        ReadyToGo();
end;

procedure TFormMain.ComboExportChange(Sender: TObject);
begin
    ReadyToGo();
end;

procedure TFormMain.CheckListBox1ItemClick(Sender: TObject; Index: integer);
begin
    ReadyToGo();
end;

procedure TFormMain.ComboExportModeChange(Sender: TObject);
begin
    case ComboExportMode.ItemIndex of
        cbDirectory : SetUpSource(cbNG);
        cbBook      : SetUpNoteBook();
	end;
    ReadyToGo();
end;

        // Tests indicated directory, sets LabelSouce and ret T if it finds notes there.
function TFormMain.SetExportSource(SDir : string) : boolean;
var
    SL : TStringList;
begin
    SL := nil;
    Result := false;
    if directoryExists(SDir) then begin
	        SL := FindAllFiles(SDir, '*.note', false);
	        if SL.Count > 0 then begin
	            result := true;
                LabelSource.caption := SDir;
                StatusBar1.SimpleText:= inttostr(SL.Count) + ' notes found';
		    end;
     end;
    if SL <> nil then SL.Free;
    ReadyToGo();
end;

                            // Loads all the notebooks found in current source dir into the TListBox
function TFormMain.GetNoteBooks() : integer;
var
    SLFiles, SLContent, SLBooks : TStringList;
    ST : string;
    Index, NBStart, NBEnd : integer;
    Buff : string;
begin
    Result := 0;
    CheckListBox1.Clear;
    if LabelSource.Caption = '' then exit(0);
    if directoryExists(LabelSource.Caption) then begin
	    SLFiles := FindAllFiles(LabelSource.Caption, '*.note', false);
        SLBooks := TStringList.Create;
        SLBooks.Sorted := True;
        SLBooks.Duplicates:= dupIgnore;
        try
            for St in SLFiles do begin
                SLContent := TStringList.create;        // ToDo : reuse this list
                SLContent.LoadFromFile(St);
                Index := FindInStringList(SLContent, '<tag>system:notebook:');
                if Index > -1 then begin
                    Buff := SLContent.Strings[Index];
                    NBStart := pos('<tag>system:notebook:', Buff) +21;
                    NBEnd   := pos('</tag>', Buff);
                    SLBooks.Add(RestoreBadXMLChar((copy(Buff, NBStart, NBEnd - NBStart))));
                    inc(Result);
				end;
                SLContent.free;
			end;
            CheckListBox1.Items := SLBooks;
		finally
          SLBooks.Free;
          SLFiles.free;
		end;
	end;
    StatusBar1.SimpleText:= inttostr(CheckListBox1.Count) + ' notebooks found';
end;

procedure TFormMain.SpeedSetDestinationClick(Sender: TObject);
begin
    if SelectDirectoryDialog1.Execute then
            LabelDestination.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
    ReadyToGo();
end;

procedure TFormMain.SpeedSetSourceClick(Sender: TObject);
begin
    if SelectDirectoryDialog1.Execute then begin
        LabelSource.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
        if  ComboExportMode.ItemIndex = cbBook then
            SetUpNoteBook();
	end;

    ReadyToGo();
end;



end.

