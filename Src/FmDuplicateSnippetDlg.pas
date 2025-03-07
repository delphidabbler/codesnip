{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box which can create a duplicate copy of a snippet.
}


unit FmDuplicateSnippetDlg;


interface


uses
  // Delphi
  SysUtils,
  Controls,
  StdCtrls,
  ExtCtrls,
  Classes,
  // Project
  DB.Snippets,
  DB.Vaults,
  FmGenericOKDlg,
  UBaseObjects,
  UI.Adapters.CategoryList,
  UI.Adapters.VaultList,
  UIStringList;


type
  TDuplicateSnippetDlg = class(TGenericOKDlg, INoPublicConstruct)
    cbCategory: TComboBox;
    chkEdit: TCheckBox;
    edDisplayName: TEdit;
    lblCategory: TLabel;
    lblDisplayName: TLabel;
    lblVaults: TLabel;
    cbVaults: TComboBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    type
      TPersistentOptions = class(TObject)
      strict private
        var
          fEditSnippetOnClose: Boolean;
      public
        constructor Create;
        destructor Destroy; override;
        property EditSnippetOnClose: Boolean
          read fEditSnippetOnClose write fEditSnippetOnClose;
      end;
  strict private
    var
      fSnippet: TSnippet;
      fCatList: TCategoryListAdapter;
      fVaultList: TVaultListAdapter;
      fOptions: TPersistentOptions;
      fSnippetKey: string;
    ///  <summary>Returns the ID of the vault selected in the vaults drop down
    ///  list, or the null vault ID if no vault is selected.</summary>
    function SelectedVaultID: TVaultID;
    function SelectedCategoryID: string;
    procedure ValidateData;
    procedure HandleException(const E: Exception);
    procedure UpdateDatabase;
  strict protected
    ///  <summary>Initialises form fields and controls.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure InitForm; override;
    ///  <summary>Aligns and arranges controls.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure ArrangeForm; override;
  public
    class function Execute(const AOwner: TComponent;
      const ASnippet: TSnippet): Boolean;
  end;


implementation


uses
  // Delphi
  Math,
  // Project
  DB.Categories,
  DB.Main,
  DB.SnippetIDs,
  UCtrlArranger,
  UExceptions,
  UMessageBox,
  USettings,
  USnippetValidator,
  UStructs,
  UStrUtils,
  UUserDBMgr;

{$R *.dfm}

{TODO -cTidy: Re-order methods alphabetically}
{TODO -cRefactor: Add method to get display name from ctrl and strip whitespace}

{ TDuplicateSnippetDlg }

procedure TDuplicateSnippetDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);

  TCtrlArranger.AlignLefts(
    [
      lblDisplayName, edDisplayName,
      lblCategory, cbCategory,
      lblVaults, cbVaults,
      chkEdit
    ],
    0
  );

  lblDisplayName.Top := 0;
  TCtrlArranger.MoveBelow(lblDisplayName, edDisplayName, 4);
  TCtrlArranger.MoveBelow(edDisplayName, lblCategory, 8);
  TCtrlArranger.MoveBelow(lblCategory, cbCategory, 4);
  TCtrlArranger.MoveBelow(cbCategory, lblVaults, 8);
  TCtrlArranger.MoveBelow(lblVaults, cbVaults, 4);
  TCtrlArranger.MoveBelow(cbVaults, chkEdit, 20);

  pnlBody.ClientWidth := Max(
    TCtrlArranger.TotalControlWidth(pnlBody) + 8,
    TCtrlArranger.RightOf(btnHelp) - btnOK.Left
  );

  // Arrange inherited controls and size the form
  inherited;
end;

procedure TDuplicateSnippetDlg.btnOKClick(Sender: TObject);
begin
  try
    ValidateData;
    fSnippetKey := (Database as IDatabaseEdit).GetUniqueSnippetKey(
      SelectedVaultID
    );
    UpdateDatabase;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

class function TDuplicateSnippetDlg.Execute(const AOwner: TComponent;
  const ASnippet: TSnippet): Boolean;
var
  Dlg: TDuplicateSnippetDlg;
resourcestring
  sCaption = 'Duplicate "%s"';   // dialogue box caption
begin
  Assert(Assigned(ASnippet), ClassName + '.Execute: ASnippet is nil');
  Dlg := InternalCreate(AOwner);
  try
    Dlg.Caption := Format(sCaption, [ASnippet.DisplayName]);
    Dlg.fSnippet := ASnippet;
    Result := Dlg.ShowModal = mrOK;
  finally
    Dlg.Free;
  end;
end;

procedure TDuplicateSnippetDlg.HandleException(const E: Exception);
var
  Error: EDataEntry;
begin
  ModalResult := mrNone;
  if E is EDataEntry then
  begin
    Error := E as EDataEntry;
    TMessageBox.Error(Self, Error.Message);
    Error.Ctrl.SetFocus;
    if Error.HasSelection and (Error.Ctrl is TCustomEdit) then
    begin
      (Error.Ctrl as TCustomEdit).SelStart := Error.Selection.StartPos;
      (Error.Ctrl as TCustomEdit).SelLength := Error.Selection.Length;
    end;
  end
  else
    raise E;
end;

procedure TDuplicateSnippetDlg.InitForm;
var
  SnippetCat: TCategory;
  SnippetVault: TVault;
begin
  inherited;
  edDisplayName.Text := fSnippet.DisplayName;

  fCatList.ToStrings(cbCategory.Items);
  fVaultList.ToStrings(cbVaults.Items);
  Assert(cbCategory.Items.Count > 0, ClassName + '.InitForm: no categories');
  Assert(cbVaults.Items.Count > 0, ClassName + '.InitForm: no vaults');

  SnippetCat := Database.Categories.Find(fSnippet.Category);
  Assert(Assigned(SnippetCat), ClassName + '.InitForm: invalid category');
  cbCategory.ItemIndex := cbCategory.Items.IndexOf(SnippetCat.Description);

  SnippetVault := TVaults.Instance.GetVault(fSnippet.VaultID);
  cbVaults.ItemIndex := cbVaults.Items.IndexOf(SnippetVault.Name);

  chkEdit.Checked := fOptions.EditSnippetOnClose;
end;

function TDuplicateSnippetDlg.SelectedCategoryID: string;
begin
  Assert(cbCategory.ItemIndex >= 0,
    ClassName + '.SelectedCategoryID: no category selected');
  Result := fCatList.CatID(cbCategory.ItemIndex);
end;

function TDuplicateSnippetDlg.SelectedVaultID: TVaultID;
begin
  Assert(cbVaults.ItemIndex >= 0,
    ClassName + '.SelectedVaultID: no vault selected');
  Result := fVaultList.Vault(cbVaults.ItemIndex).UID;
end;

procedure TDuplicateSnippetDlg.UpdateDatabase;
begin
  (Database as IDatabaseEdit).DuplicateSnippet(
    fSnippet,
    fSnippetKey,
    SelectedVaultID,
    StrTrim(edDisplayName.Text),
    SelectedCategoryID
  );
end;

procedure TDuplicateSnippetDlg.ValidateData;
resourcestring
  sNoCategory = 'You must choose a category';
  sNoVault = 'You must choose a vault';
  sNoDisplayName = 'You must provide a display name';
begin
  if StrTrim(edDisplayName.Text) = '' then
    raise EDataEntry.Create(sNoDisplayName, edDisplayName);
  if cbCategory.ItemIndex = -1 then
    raise EDataEntry.Create(sNoCategory, cbCategory);
  if cbVaults.ItemIndex = -1 then
    raise EDataEntry.Create(sNoVault, cbVaults);
end;

procedure TDuplicateSnippetDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fCatList := TCategoryListAdapter.Create(Database.Categories);
  fVaultList := TVaultListAdapter.Create;
  fOptions := TPersistentOptions.Create;
end;

procedure TDuplicateSnippetDlg.FormDestroy(Sender: TObject);
begin
  if (ModalResult = mrOK) and chkEdit.Checked then
    TUserDBMgr.EditSnippet(TSnippetID.Create(fSnippetKey, SelectedVaultID));
  fOptions.EditSnippetOnClose := chkEdit.Checked;
  inherited;
  fOptions.Free;
  fVaultList.Free;
  fCatList.Free;
end;

{ TDuplicateSnippetDlg.TPersistentOptions }

constructor TDuplicateSnippetDlg.TPersistentOptions.Create;
var
  Section: ISettingsSection;
begin
  inherited Create;
  Section := Settings.ReadSection(ssDuplicateSnippet);
  fEditSnippetOnClose := Section.GetBoolean('EditSnippetOnClose', False);
end;

destructor TDuplicateSnippetDlg.TPersistentOptions.Destroy;
var
  Section: ISettingsSection;
begin
  Section := Settings.EmptySection(ssDuplicateSnippet);
  Section.SetBoolean('EditSnippetOnClose', fEditSnippetOnClose);
  Section.Save;
  inherited;
end;

end.

