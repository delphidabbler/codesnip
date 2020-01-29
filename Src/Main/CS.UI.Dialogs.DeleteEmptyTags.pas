unit CS.UI.Dialogs.DeleteEmptyTags;

interface

// TODO -cHelp: Make Help button visible once help topic available
// TODO -cRefactor: Pull out check list handling into frame - code similar to CS.UI.Dialogs.TagsSearch

uses
  // VCL
  StdCtrls,
  Controls,
  ExtCtrls,
  Classes,
  CheckLst,
  // Project
  CS.Database.Types,
  CS.UI.Helper.CollectionCtrlKVMgr,
  FmGenericOKDlg,
  UBaseObjects,
  UCtrlArranger,
  UStrUtils {for inlining};

type
  TDeleteUnusedTagsDlg = class(TGenericOKDlg, INoPublicConstruct)
    clbTags: TCheckListBox;
    lblDesc: TLabel;
    btnUncheckAll: TButton;
    btnCheckAll: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure clbTagsClickCheck(Sender: TObject);
    procedure btnUncheckAllClick(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
  strict private
    fTags: ITagSet;
    fTagListMgr: TSortedCollectionCtrlKVMgr<TTag>;
    procedure SetAllChecks(const State: Boolean);
    function HaveTagsSelected: Boolean;
    procedure UpdateOKBtn;
  strict protected
    procedure InitForm; override;
    procedure ArrangeForm; override;
  public
    class function Execute(AOwner: TComponent; EmptyTags: ITagSet): Boolean;
  end;

implementation

{$R *.dfm}

{ TDeleteUnusedTagsDlg }

procedure TDeleteUnusedTagsDlg.ArrangeForm;
begin
  TCtrlArranger.AlignLefts([lblDesc, clbTags, btnUncheckAll], 0);
  TCtrlArranger.AlignRights([clbTags, btnCheckAll]);
  lblDesc.Top := 0;
  TCtrlArranger.MoveBelow(lblDesc, clbTags, 4);
  TCtrlArranger.MoveBelow(clbTags, btnUncheckAll);
  TCtrlArranger.MoveBelow(clbTags, btnCheckAll);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 4;
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);
  inherited;
end;

procedure TDeleteUnusedTagsDlg.btnCheckAllClick(Sender: TObject);
begin
  SetAllChecks(True);
end;

procedure TDeleteUnusedTagsDlg.btnOKClick(Sender: TObject);
var
  I: Integer;
begin
  // Remove any unchecked tags from unused tags to be deleted
  for I := 0 to Pred(clbTags.Count) do
    if not clbTags.Checked[I] then
      fTags.Remove(fTagListMgr.GetKeyAt(I));
end;

procedure TDeleteUnusedTagsDlg.btnUncheckAllClick(Sender: TObject);
begin
  SetAllChecks(False);
end;

procedure TDeleteUnusedTagsDlg.clbTagsClickCheck(Sender: TObject);
begin
  UpdateOKBtn;
end;

class function TDeleteUnusedTagsDlg.Execute(AOwner: TComponent;
  EmptyTags: ITagSet): Boolean;
begin
  with TDeleteUnusedTagsDlg.InternalCreate(AOwner) do
    try
      fTags := EmptyTags;
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

procedure TDeleteUnusedTagsDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fTagListMgr := TSortedCollectionCtrlKVMgr<TTag>.Create(
    TListBoxAdapter.Create(clbTags),
    True,
    function (const Left, Right: TTag): Boolean
    begin
      Result := Left = Right;
    end,
    stIgnoreCase
  );
end;

procedure TDeleteUnusedTagsDlg.FormDestroy(Sender: TObject);
begin
  fTagListMgr.Free;
  inherited;
end;

function TDeleteUnusedTagsDlg.HaveTagsSelected: Boolean;
var
  I: Integer;
begin
  for I := 0 to Pred(clbTags.Count) do
    if clbTags.Checked[I] then
      Exit(True);
  Result := False;
end;

procedure TDeleteUnusedTagsDlg.InitForm;
var
  Tag: TTag;
begin
  inherited;
  // Populate tags check list box
  for Tag in fTags do
    fTagListMgr.Add(Tag, Tag.ToString);
  // Check all tags
  SetAllChecks(True);
end;

procedure TDeleteUnusedTagsDlg.SetAllChecks(const State: Boolean);
var
  I: Integer;
begin
  for I := 0 to Pred(clbTags.Count) do
    clbTags.Checked[I] := State;
  UpdateOKBtn;
end;

procedure TDeleteUnusedTagsDlg.UpdateOKBtn;
begin
  // OK button enabled if at least one tag is checked
  btnOK.Enabled := HaveTagsSelected;
end;

end.
