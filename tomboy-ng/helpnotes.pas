unit helpnotes;


{ We allow user to download (from github) an alternate set of help notes.
  These notes are dropped into mainunit->HelpNotesPath/alt-help/*, on Windows and Mac
  but on Linux ('cos HelpNotesPath is readonly) we put them [config-dir]/alt-help/*
  The help engine will always prefer alt-help files if they exist, if user wants to revert
  back to English, we just delete the files in alt-help.

  Windows users will need to have some ssl lib installed, what happens if they dont ....
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ssockets;

type

    { TFormHelpNotes }

    TFormHelpNotes = class(TForm)
        ButtonClose: TButton;
        ButtonRestore: TButton;
        Label1: TLabel;
        Label2: TLabel;
        ListBox1: TListBox;
        procedure ButtonCloseClick(Sender: TObject);
        procedure ButtonRestoreClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
        procedure ListBox1DblClick(Sender: TObject);
    private
        function DownloadFile(URL, FullFileName: string; out ErrorMsg: string): boolean;
        // This proc replaces the RTL version to enable v1.1 of ssl instead of v23
        // note we set it in DownLoadFile() after the object is created.
        procedure HttpClientGetSocketHandler(Sender: TObject;
            const UseSSL: Boolean; out AHandler: TSocketHandler);

    public

    end;

var
    FormHelpNotes: TFormHelpNotes;

implementation

{$R *.lfm}

{ TFormHelpNotes }
uses
   mainunit, Settings, // HelpNotesPath, Config dir
   LazFileUtils, zipper,
  fphttpclient,
  fpopenssl,
  openssl,
  sslsockets;      // for TSSLSocketHandler etc


procedure TFormHelpNotes.HttpClientGetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  If UseSSL then begin
    AHandler := TSSLSocketHandler.Create;
    TSSLSocketHandler(AHandler).SSLType:=stTLSv1_2;  // <--
  end else
      AHandler := TSocketHandler.Create;
end;

function TFormHelpNotes.DownloadFile(URL, FullFileName : string; out ErrorMsg : string) : boolean;
var
    Client: TFPHttpClient;
    FS: TStream;
begin
    result := false;
    InitSSLInterface;
    Client := TFPHttpClient.Create(nil);
    FS := TFileStream.Create(FullFilename,fmCreate or fmOpenWrite);
    // Comment out next line to revert to fpc3.0.4 behaviour, generates a ESSL
    // exception (that I dont seem to be able to catch) if we try for any https site.
    Client.OnGetSocketHandler := @HttpClientGetSocketHandler;
    ErrorMsg := 'Created';
    try
        try
            Client.AllowRedirect := true;
            Client.Get(URL, FS);
        except
            on E: ESSL do begin ErrorMsg := E.Message; exit; end;    // does not catch it !
            on E: Exception do begin
                ErrorMsg := E.Message;
                exit;
            end;
        end;
    finally
        FS.Free;
        Client.Free;
    end;
    ErrorMsg := '';
    result := true;
end;

procedure TFormHelpNotes.ListBox1Click(Sender: TObject);
begin

end;

procedure TFormHelpNotes.FormCreate(Sender: TObject);
begin
    ListBox1.AddItem('es Spanish', Nil);
end;

procedure TFormHelpNotes.ButtonCloseClick(Sender: TObject);
begin
    close;
end;

procedure TFormHelpNotes.ButtonRestoreClick(Sender: TObject);
var
    Info : TSearchRec;
begin
    if FindFirst(MainForm.AltHelpPath + '*.*', faAnyFile, Info)=0 then begin
        repeat
            DeleteFileUTF8(MainForm.AltHelpPath() + Info.Name);                // should we test return value ?
	    until FindNext(Info) <> 0;
	end;
    FindClose(Info);
    if not RemoveDirUTF8(MainForm.AltHelpPath) then
        showmessage('Remove Dir Error in HelpNotes Unit');
end;

procedure TFormHelpNotes.ListBox1DblClick(Sender: TObject);
var
    EMsg : string;   ZipFile:
    TUnZipper;
    FileName : string;
    DownLoadPath : string = 'http://bannons.id.au/downloads/';
begin
    if not DirectoryExistsUTF8(MainForm.AltHelpPath) then
        CreateDirUTF8(MainForm.AltHelpPath);

    showmessage(ListBox1.Items[ListBox1.ItemIndex]);
    if ListBox1.Items[ListBox1.ItemIndex] = 'es Spanish' then begin
        FileName := 'es_notes.zip';
        if DownLoadFile(DownLoadPath + FileName, MainForm.AltHelpPath + FileName, EMsg) then begin
            ZipFile := TUnZipper.Create;
            try
                ZipFile.FileName := MainForm.AltHelpPath + FileName;
                ZipFile.OutputPath := MainForm.AltHelpPath;
                ZipFile.Examine;
                ZipFile.UnZipAllFiles;
            finally
                ZipFile.Free;
            end;
        end else
            showmessage('ERROR - ' + EMsg);
    end;
end;

end.

