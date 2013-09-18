unit FmTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ExtCtrls, ComCtrls,

  CS.Database.IO.Native,
  CS.Database.Main,
  CS.Database.Types,
  CS.SourceCode.Languages,
  CS.Utils.Dates;

type
  TForm1 = class(TForm)
    pcMain: TPageControl;
    tsSnippetEdit: TTabSheet;
    edTitle: TLabeledEdit;
    lblDesc: TLabel;
    edDesc: TMemo;
    lblLanguage: TLabel;
    cbLanguage: TComboBox;
    lblSnippets: TLabel;
    lvSnippets: TListView;
    btnAdd: TButton;
    btnUpdate: TButton;
    btnDelete: TButton;
    lblIDDesc: TLabel;
    stID: TStaticText;
    btnNew: TButton;
    lblSource: TLabel;
    edSource: TMemo;
    btnSave: TButton;
    procedure btnNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lvSnippetsDblClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    fCurrentSnippet: ISnippet;
    procedure SelectLanguage(const Lang: TSourceCodeLanguageID);
    procedure UpdateEditControls;
    procedure ClearEditControls;
    procedure UpdateCurrentSnuppet;
    procedure PopulateLV;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  CS.Markup,
  UStrUtils;

{$R *.dfm}

procedure TForm1.btnAddClick(Sender: TObject);
begin
  UpdateCurrentSnuppet;
  TDatabase.InsertSnippet(fCurrentSnippet);
  PopulateLV;
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
var
  LI: TLIstItem;
begin
  LI := lvSnippets.Selected;
  if LI = nil then
    Exit;
  TDatabase.DeleteSnippet(
    TDBSnippetID.Create(LI.Caption)
  );
  PopulateLV;
  fCurrentSnippet := nil;
  ClearEditControls;
end;

procedure TForm1.btnNewClick(Sender: TObject);
begin
  fCurrentSnippet := TDatabase.NewSnippet;
  UpdateEditControls;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  TDatabase.Save;
end;

procedure TForm1.btnUpdateClick(Sender: TObject);
begin
  UpdateCurrentSnuppet;
  TDatabase.UpdateSnippet(fCurrentSnippet);
  PopulateLV;
end;

procedure TForm1.ClearEditControls;
begin
  stID.Caption := '';
  edTitle.Text := '';
  edDesc.Text := '';
  edSource.Text := '';
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  D: TUTCDateTime;
begin
  D := TUTCDateTime.CreateFromISO8601String('2000-01-01T12:34:57Z');
  edSource.Lines.Add('2000-01-01T12:34:57Z');
  edSource.Lines.Add(D.ToISO8601String(False));
  edSource.Lines.Add(D.ToISO8601String(True));
  edSource.Lines.Add('');
  D := TUTCDateTime.CreateFromISO8601String('2000-01-01T12:34:57.567Z');
  edSource.Lines.Add('2000-01-01T12:34:57.567Z');
  edSource.Lines.Add(D.ToISO8601String);
  edSource.Lines.Add(D.ToISO8601String(True));
  edSource.Lines.Add('');
  edSource.Lines.Add(BoolToStr(D.IsNull, True));
  D := TUTCDateTime.CreateNull;
  edSource.Lines.Add(BoolToStr(D.IsNull, True));
  D := TUTCDateTime.Now;
  edSource.Lines.Add(D.ToISO8601String(False));
  edSource.Lines.Add(D.ToISO8601String(True));
  D := TUTCDateTime.Create(Now);
  edSource.Lines.Add(D.ToISO8601String);
  edSource.Lines.Add('');
  D := TUTCDateTime.CreateNull;
  edSource.Lines.Add('NULL');
  edSource.Lines.Add(D.ToISO8601String);
end;

procedure TForm1.lvSnippetsDblClick(Sender: TObject);
var
  LI: TLIstItem;
begin
  LI := lvSnippets.Selected;
  if LI = nil then
    Exit;
  fCurrentSnippet := TDatabase.GetSnippet(
    TDBSnippetID.Create(LI.Caption)
  );
  UpdateEditControls;
end;

procedure TForm1.PopulateLV;
var
  SnippetIDs: IDBSnippetIDList;
  ID: TDBSnippetID;
  Snippet: IReadOnlySnippet;
  LI: TListItem;
begin
  lvSnippets.Items.BeginUpdate;
  try
    lvSnippets.Clear;
    SnippetIDs := TDatabase.GetSnippetIDs(
      TDelegatedDBFilter.Create(
        function (ASnippet: IReadOnlySnippet): Boolean
        begin
          Result := True;
        end,
        [spID]
      )
    );
    for ID in SnippetIDs do
    begin
      Snippet := TDatabase.GetReadOnlySnippet(ID, [spTitle, spModified]);
      LI := lvSnippets.Items.Add;
      LI.Caption := Snippet.ID.ToString;
      LI.SubItems.Add(Snippet.Title);
      LI.SubItems.Add(Snippet.Modified.ToString('c'));
    end;
  finally
    lvSnippets.Items.EndUpdate;
  end;
end;

procedure TForm1.SelectLanguage(const Lang: TSourceCodeLanguageID);
var
  Idx: Integer;
begin
  Idx := cbLanguage.Items.IndexOf(Lang.ToString);
  cbLanguage.ItemIndex := Idx;
end;

procedure TForm1.UpdateCurrentSnuppet;
begin
  fCurrentSnippet.Title := StrTrim(edTitle.Text);
  fCurrentSnippet.Description := TMarkup.Create(edDesc.Text, mkPlainText);
  fCurrentSnippet.SourceCode := StrTrimRight(edSource.Text);
  fCurrentSnippet.LanguageID := TSourceCodeLanguageID.Create(
    cbLanguage.Items[cbLanguage.ItemIndex]
  );
end;

procedure TForm1.UpdateEditControls;
begin
  stID.Caption := fCurrentSnippet.ID.ToString;
  edTitle.Text := fCurrentSnippet.Title;
  edDesc.Text := fCurrentSnippet.Description.Source;
  edSource.Text := fCurrentSnippet.SourceCode;
  SelectLanguage(fCurrentSnippet.LanguageID);
end;

end.
