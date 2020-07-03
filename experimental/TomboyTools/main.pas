unit main;

{$mode objfpc}{$H+} {$assertions on}

{ License - see tomboy-ng license information }

interface

uses
        Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
		ExtCtrls, Buttons, StdCtrls, CheckLst, LCLProc;

type

		{ TFormMain }

        TFormMain = class(TForm)
				CheckListBox1: TCheckListBox;
                CheckListImportFiles: TCheckListBox;
                ComboImportDest: TComboBox;
                ComboSourceFormat: TComboBox;
				ComboExportMode: TComboBox;
				ComboExport: TComboBox;
				ComboSource: TComboBox;
                GroupBox1: TGroupBox;
                GroupBox2: TGroupBox;
				Label1: TLabel;
				Label2: TLabel;
				Label3: TLabel;
				Label4: TLabel;
				Label5: TLabel;
				Label6: TLabel;
                Label7: TLabel;
                Label8: TLabel;
                LabelImportDestination: TLabel;
                LabelErrorMessage: TLabel;
                LabelImportSource: TLabel;
                LabelVersion: TLabel;
				LabelSourcePrompt: TLabel;
				LabelSource: TLabel;
				LabelDestinationPrompt: TLabel;
				LabelDestination: TLabel;
				PageControl1: TPageControl;
                PanelLower: TPanel;
				PanelTop: TPanel;
                RadioFileNameTitle: TRadioButton;
                RadioFileNameID: TRadioButton;
                RadioTitleFirstLine: TRadioButton;
                RadioTitleFilename: TRadioButton;
				SelectDirectoryDialog1: TSelectDirectoryDialog;
                SpeedExit: TSpeedButton;
                SpeedImportSourceDir: TSpeedButton;
                SpeedProceed: TSpeedButton;
				SpeedSetSource: TSpeedButton;
				SpeedSetDestination: TSpeedButton;
				StatusBar1: TStatusBar;
				TabExport: TTabSheet;
				TabImport: TTabSheet;
                procedure CheckListBox1Click(Sender: TObject);
                procedure CheckListImportFilesClick(Sender: TObject);
                procedure CheckListImportFilesClickCheck(Sender: TObject);
                procedure ComboImportDestChange(Sender: TObject);
                procedure ComboExportChange(Sender: TObject);
				procedure ComboExportModeChange(Sender: TObject);
                procedure ComboSourceChange(Sender: TObject);
                procedure ComboSourceFormatChange(Sender: TObject);
                procedure FormCreate(Sender: TObject);
                procedure FormShow(Sender: TObject);
				procedure SpeedExitClick(Sender: TObject);

				procedure SpeedProceedClick(Sender: TObject);
    			procedure SpeedSetDestinationClick(Sender: TObject);
				procedure SpeedSetSourceClick(Sender: TObject);
                procedure SpeedImportSourceDirClick(Sender: TObject);
                procedure TabExportShow(Sender: TObject);
                procedure TabImportShow(Sender: TObject);
        private
            procedure ImportProceed();
            procedure ImportReadyToGo();
            function NumberChecked(CLB: TCheckListBox): integer;
            procedure ProcessDirectory;
            procedure ProcessNotebooks;
				procedure SetUpSource(Mode: integer);
				procedure SetUpNoteBook();
				function GetNoteBooks(): integer;
				function ExportReadyToGo(): boolean;
				function SetExportSource(SDir: string): boolean;

        public

        end;


var
        FormMain: TFormMain;

implementation

uses cmdline, FileUtil, LazFileUtils, ttutils, export_notes, import_notes;

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
    ExportReadyToGo();
end;

procedure TFormMain.SetUpNoteBook() ;
begin
    CheckListBox1.Enabled := true;
    if GetNoteBooks() = 0 then
        showmessage('That dir has not notes in notebooks');
end;

function TFormMain.ExportReadyToGo() : boolean;
begin
    Result :=   (LabelSource.Caption <> '') and
                (LabelDestination.Caption <> '') and
                ( (ComboExportMode.ItemIndex <> cbBook) or (NumberChecked(CheckListBox1) > 0))
                ;

    SpeedProceed.Enabled := Result;
end;


procedure TFormMain.FormShow(Sender: TObject);
begin
    //CheckListBox1.AllowGrayed:=true;

end;



procedure TFormMain.ProcessDirectory;
var
    Exporter : TExportNote;
begin
    Exporter := TExportNote.Create;
    try
        Exporter.DestDir := LabelDestination.Caption;
        Exporter.NoteDir := LabelSource.caption;
        Exporter.OutFormat := ComboExport.Text;
        Exporter.FileNameIsTitle := RadioFileNameTitle.checked;
        //Exporter.AllNotes := True;
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

procedure TFormMain.ProcessNotebooks;
var
    Exporter : TExportNote;
    Index : integer = 0;
    UsingChecked : boolean = false;
    NotesProcessed : integer = 0;
begin
    while Index < CheckListBox1.Items.Count do begin
        if CheckListBox1.Checked[Index] then begin
            UsingChecked := True;
            debugln('Checked [' + CheckListBox1.Items[Index] + ']');
            break;
        end;
        inc(Index);
    end;
    Exporter := TExportNote.Create;
    try
        Exporter.DestDir := LabelDestination.Caption;
        Exporter.NoteDir := LabelSource.caption;
        Exporter.OutFormat := ComboExport.Text;

            DebugLn('checked notebooks');
            Index := 0;
            while Index < CheckListBox1.Items.Count do begin
                if CheckListBox1.Checked[Index] then begin
                    debugln('Checked [' + CheckListBox1.Items[Index] + ']');
                    Exporter.Notebook := CheckListBox1.Items[Index];
                    Exporter.Execute();
                    if Exporter.ErrorMessage <> '' then begin
                        debugln(Exporter.ErrorMessage);
                        showmessage(Exporter.ErrorMessage);
                        LabelErrorMessage.Caption := Exporter.ErrorMessage;
                    end else begin
                        StatusBar1.SimpleText:=  CheckListBox1.items[Index] + ' '
                                + inttostr(Exporter.NotesProcessed) + ' notes processed.';
                        NotesProcessed := NotesProcessed + Exporter.NotesProcessed;
                    end;
                    Application.ProcessMessages;
                end;
                inc(Index);
            end;
            StatusBar1.SimpleText:=  'Total of '
                    + inttostr(NotesProcessed) + ' notes processed.';
	finally
        Exporter.Free;
	end;
end;




procedure TFormMain.ComboSourceChange(Sender: TObject);
begin
        SetUpSource(ComboSource.ItemIndex);
        ExportReadyToGo();
end;



procedure TFormMain.ComboExportChange(Sender: TObject);
begin
    ExportReadyToGo();
end;

procedure TFormMain.CheckListBox1Click(Sender: TObject);
begin
    CheckListBox1.ItemIndex := -1;
    ExportReadyToGo();
end;

procedure TFormMain.CheckListImportFilesClick(Sender: TObject);
begin
    CheckListImportFiles.ItemIndex := -1;
end;



procedure TFormMain.ComboExportModeChange(Sender: TObject);
begin
    case ComboExportMode.ItemIndex of
        cbDirectory : SetUpSource(cbNG);
        cbBook      : SetUpNoteBook();
	end;
    ExportReadyToGo();
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
    ExportReadyToGo();
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
    if DirectoryIsWritable(LabelDestination.Caption) then
        ExportReadyToGo()
    else
        Showmessage('Cannot write to that dir' + #10 + LabelDestination.Caption);
end;

procedure TFormMain.SpeedSetSourceClick(Sender: TObject);
begin
    if SelectDirectoryDialog1.Execute then begin
        LabelSource.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
        if  ComboExportMode.ItemIndex = cbBook then
            SetUpNoteBook();
	end;
    ExportReadyToGo();
end;

procedure TFormMain.TabExportShow(Sender: TObject);
begin
    StatusBar1.SimpleText:= '';
    LabelErrorMessage.Caption := '';
    SetUpSource(cbNG);   // will try NG first, then TB, then fall back to manual
    ExportReadyToGo();
end;



// -------------------------- S H A R E D -------------------------------------


RESOURCESTRING
rsTomboyngDefault = 'tomboy-ng default';
rsTomboyDefault   = 'Tomboy Default';
rsLetMeChoose     = 'Let Me Choose';
rsDirOfNotes      = 'A Directory of Notes';
rsNotesInNotebook = 'Notes in a Notebook';
rsPlainText       = 'Plain Text';
rsMarkDown        = 'Mark Down (git style)';




procedure TFormMain.FormCreate(Sender: TObject);
begin
    LabelImportSource.Caption := '';
    LabelImportDestination.Caption := '';
    LabelErrorMessage.Caption := '';
    LabelDestination.caption := '';
    ComboExportMode.Items.add(rsDirOfNotes);
    ComboExportMode.Items.add(rsNotesInNoteBook);
    ComboImportDest.Items.add(rsTomboyngDefault);
    ComboImportDest.Items.add(rsTomboyDefault);
    ComboImportDest.Items.add(rsLetMeChoose);
    ComboImportDest.ItemIndex := 0;
    ComboSourceFormat.Items.Add(rsPlainText);
    ComboSourceFormat.Items.Add(rsMarkDown);
    ComboSourceFormat.ItemIndex:=1;
end;

procedure TFormMain.SpeedExitClick(Sender: TObject);
begin
    close;
end;

function TFormMain.NumberChecked(CLB : TCheckListBox) : integer;
var
    Index : integer = 0;
begin
    result := 0;
    while Index < CLB.Count do begin
        if CLB.Checked[Index] then
            inc(Result);
        inc(Index);
    end;
end;

procedure TFormMain.SpeedProceedClick(Sender: TObject);
{var
    Exporter : TExportNote;  }
begin
    if PageControl1.ActivePage.TabIndex = 0 then begin      // Thats Export
        StatusBar1.SimpleText:= 'processing notes, please wait ....';
        LabelErrorMessage.Caption := '';
        Application.ProcessMessages;
        case comboExportMode.itemIndex of
            cbDirectory : ProcessDirectory;
		    cbBook      : ProcessNotebooks;
	    end;
    end else                                           // Unless we add more, that Import
        ImportProceed();
end;




// -------------------------- I M P O R T ------------------------------------


procedure TFormMain.ComboSourceFormatChange(Sender: TObject);
begin
    ImportReadyToGo();
end;

procedure TFormMain.CheckListImportFilesClickCheck(Sender: TObject);
begin
    ImportReadyToGo();
end;

procedure TFormMain.ComboImportDestChange(Sender: TObject);
begin
    if ComboImportDest.Items[ComboImportDest.ItemIndex] = rsTomboyngDefault then
        LabelImportDestination.Caption := GetDefaultNoteDir()
    else if ComboImportDest.Items[ComboImportDest.ItemIndex] = rsTomboyDefault then
        LabelImportDestination.Caption := GetDefaultNoteDir(True)
        else begin
            if SelectDirectoryDialog1.Execute then
                    LabelImportDestination.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
        end;
    if not DirectoryIsWritable(LabelImportDestination.Caption) then begin
        Showmessage('Cannot write to that dir' + #10 + LabelImportDestination.Caption);
        LabelImportDestination.Caption := ''
    end;
    ImportReadyToGo();
end;


procedure TFormMain.ImportReadyToGo();
begin
    SpeedProceed.Enabled :=  ((NumberChecked(CheckListImportFiles) > 0)
                                and (LabelImportSource.Caption <> '')
                                and (LabelImportDestination.Caption <> ''));
end;

procedure TFormMain.ImportProceed();
var
    NameList : TStringList;
    Import : TImportNotes;
    Index : integer = 0;
begin
    if NumberChecked(CheckListImportFiles) > 0 then begin
        try
            NameList := TStringList.Create;
            Import :=  TImportNotes.Create;
            while Index < CheckListImportFiles.Count do begin
                if CheckListImportFiles.Checked[Index] then
                    NameList.Add(CheckListImportFiles.Items[Index]);
                inc(Index);
            end;
            Import.ImportNames := NameList;
            Import.DestinationDir := LabelImportDestination.Caption;
            Import.FirstLineTitle := RadioTitleFirstLine.Checked;
            if ComboSourceFormat.Items[ComboSourceFormat.ItemIndex] = rsPlainText then
                Import.Mode := 'plaintext'
            else if ComboSourceFormat.Items[ComboSourceFormat.ItemIndex] = rsMarkDown then
                Import.Mode := 'markdown';
            StatusBar1.SimpleText:= inttostr(Import.Execute) + ' files imported';
            LabelErrorMessage.Caption := Import.ErrorMsg;
        finally
            freeandnil(Import);
            freeandnil(NameList);
        end;
    end else showmessage('No files selected');
end;

procedure TFormMain.SpeedImportSourceDirClick(Sender: TObject);
var
    SrcFiles : TstringList;
begin
    if SelectDirectoryDialog1.Execute then begin
        CheckListImportFiles.Clear;
        LabelImportSource.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
        SrcFiles := FindAllFiles(LabelImportSource.Caption, '*.txt;*.text;*.md', false);
        CheckListImportFiles.items := SrcFiles;
	end;
    freeandnil(SrcFiles);
    ImportReadyToGo();
end;

procedure TFormMain.TabImportShow(Sender: TObject);
begin
    StatusBar1.SimpleText:= '';
    LabelErrorMessage.Caption := '';
    ImportReadyToGo();
end;


end.

