unit Notebook;

{$mode objfpc}{$H+}

interface

uses
		Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
		ExtCtrls, StdCtrls, Buttons, ComCtrls;

type

		{ TNoteBookPick }

  TNoteBookPick = class(TForm)
				Button1: TButton;
				ButtonOK: TButton;
				CheckListBox1: TCheckListBox;
				EditNewNotebook: TEdit;
				Label1: TLabel;
				Label2: TLabel;
				Label3: TLabel;
				Label4: TLabel;
				Label5: TLabel;
				PageControl1: TPageControl;
				Panel1: TPanel;
				TabExisting: TTabSheet;
				TabNewNoteBook: TTabSheet;
				procedure ButtonOKClick(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
		private

		public
	FullFileName : ANSIString;
    Title : ANSIString;
		end;

var
		NoteBookPick: TNoteBookPick;

implementation

{$R *.lfm}

{ TNoteBookPick }

uses MainUnit, LazFileUtils, Settings, SaveNote;

procedure TNoteBookPick.FormShow(Sender: TObject);
var
        SL : TStringList;
        Index, I : Integer;
begin
    Label1.Caption := Title;
    if Sett.CheckManyNotebooks.Checked then
        Label2.Caption := 'Settings allow multiple Notebooks'
    else Label2.Caption := 'Settings allow only one Notebook';
    Label3.Caption := 'Set the notebooks this note is a member of';
    SL := TStringList.Create;
    RTSearch.NoteLister.GetNotebooks(SL);
    CheckListBox1.Items.Assign(SL);
    SL.Free;
    SL := TStringList.Create;
    RTSearch.NoteLister.GetNotebooks(SL, ExtractFileNameOnly(FullFileName) + '.note');
    for Index := 0 to SL.Count -1 do
    	for I := 0 to CheckListBox1.Count-1 do
			if SL[Index] = CheckListBox1.Items[I] then
            	CheckListBox1.Checked[I] := True;
    SL.Free;
end;

procedure TNoteBookPick.CheckListBox1ClickCheck(Sender: TObject);
var
	Index : integer;
begin
    if Sett.CheckManyNotebooks.Checked then exit;
    if CheckListBox1.Checked[CheckListBox1.ItemIndex] then begin
        for Index := 0 to CheckListBox1.Count -1 do
        	CheckListBox1.Checked[Index] := False;
        CheckListBox1.Checked[CheckListBox1.ItemIndex] := True;
	end;
end;

procedure TNoteBookPick.ButtonOKClick(Sender: TObject);
var
        SL : TStringList;
        Index : Integer;
        Saver : TBSaveNote;
begin
    SL := TStringList.Create;
    try
        if PageControl1.ActivePage = TabExisting then begin
			for Index := 0 to CheckListBox1.Count -1 do
        		if CheckListBox1.Checked[Index] then SL.Add(CheckListBox1.Items[Index]);
    		RTSearch.NoteLister.SetNotebookMembership(ExtractFileNameOnly(FullFileName) + '.note', SL);
		end else begin
          	if EditNewNotebook.Text <> '' then begin
            	Saver := TBSaveNote.Create();
                try
                    Saver.SaveNewTemplate(EditNewNotebook.Text);
                    // OK, now add current note to the new Notebook
                    RTSearch.NoteLister.AddNoteBook(ExtractFileNameOnly(FullFileName) + '.note', EditNewNotebook.Text, False);
				finally
                	Saver.Destroy;
				end;
            end else showmessage('Enter a new Notebook Name please');
		end;
	finally
    	Sl.Free;
	end;
	ModalResult := mrOK;
end;

end.

