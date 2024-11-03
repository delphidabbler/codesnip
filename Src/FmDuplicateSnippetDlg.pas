{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box which can create a duplicate copy of asnippet.
}


unit FmDuplicateSnippetDlg;


interface


uses
  // Delphi
  SysUtils, Controls, StdCtrls, ExtCtrls, Classes,
  // Project
  DB.USnippet, FmGenericOKDlg, UBaseObjects, UCategoryListAdapter,
  UIStringList;


type
  TDuplicateSnippetDlg = class(TGenericOKDlg, INoPublicConstruct)
    cbCategory: TComboBox;
    chkEdit: TCheckBox;
    edDisplayName: TEdit;
    edUniqueName: TEdit;
    lblCategory: TLabel;
    lblDisplayName: TLabel;
    lblUniqueName: TLabel;
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
      fOptions: TPersistentOptions;
    function DisallowedNames: IStringList;
    function UniqueSnippetName(const BaseName: string): string;
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
  DB.UCollections,
  DB.UCategory, DB.UMain, UCtrlArranger, UExceptions, UMessageBox, USettings,
  USnippetValidator, UStructs, UStrUtils, UUserDBMgr;

{$R *.dfm}

{ TDuplicateSnippetDlg }

procedure TDuplicateSnippetDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);

  TCtrlArranger.AlignLefts(
    [
      lblUniqueName, lblDisplayName, lblCategory, edUniqueName, edDisplayName,
      cbCategory, chkEdit
    ],
    0
  );

  lblUniqueName.Top := 0;
  TCtrlArranger.MoveBelow(lblUniqueName, edUniqueName, 4);
  TCtrlArranger.MoveBelow(edUniqueName, lblDisplayName, 8);
  TCtrlArranger.MoveBelow(lblDisplayName, edDisplayName, 4);
  TCtrlArranger.MoveBelow(edDisplayName, lblCategory, 8);
  TCtrlArranger.MoveBelow(lblCategory, cbCategory, 4);
  TCtrlArranger.MoveBelow(cbCategory, chkEdit, 20);

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
    UpdateDatabase;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

function TDuplicateSnippetDlg.DisallowedNames: IStringList;
var
  Snippet: TSnippet;
begin
  Result := TIStringList.Create;
  Result.CaseSensitive := False;
  for Snippet in Database.Snippets do
//    if Snippet.UserDefined then
    if Snippet.CollectionID <> TCollectionID.__TMP__MainDBCollectionID then
      Result.Add(Snippet.Name);
end;

class function TDuplicateSnippetDlg.Execute(const AOwner: TComponent;
  const ASnippet: TSnippet): Boolean;
var
  Dlg: TDuplicateSnippetDlg;
resourcestring
  sCaption = 'Duplicate %s';   // dialog box caption
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
begin
  inherited;
  edUniqueName.Text := UniqueSnippetName(fSnippet.Name);
  edDisplayName.Text := StrIf(
    StrSameStr(fSnippet.Name, fSnippet.DisplayName), '', fSnippet.DisplayName
  );
  fCatList.ToStrings(cbCategory.Items);
  SnippetCat := Database.Categories.Find(fSnippet.Category);
  if Assigned(SnippetCat) then
    cbCategory.ItemIndex := cbCategory.Items.IndexOf(SnippetCat.Description)
  else
    cbCategory.ItemIndex := -1;
  chkEdit.Checked := fOptions.EditSnippetOnClose;
end;

function TDuplicateSnippetDlg.UniqueSnippetName(const BaseName: string): string;
var
  ExistingNames: IStringList;
  Postfix: Cardinal;
begin
  ExistingNames := DisallowedNames;
  if not ExistingNames.Contains(BaseName) then
    Exit(BaseName);
  // BaseName exists: find number to append to it to make name unique
  Postfix := 1;
  repeat
    Inc(PostFix);
    Result := BaseName + IntToStr(PostFix);
  until not ExistingNames.Contains(Result);
end;

procedure TDuplicateSnippetDlg.UpdateDatabase;
var
  UniqueName: string;
  DisplayName: string;
begin
  UniqueName := StrTrim(edUniqueName.Text);
  DisplayName := StrTrim(edDisplayName.Text);
  (Database as IDatabaseEdit).DuplicateSnippet(
    fSnippet,
    UniqueName,
    StrIf(StrSameStr(UniqueName, DisplayName), '', DisplayName),
    fCatList.CatID(cbCategory.ItemIndex)
  );
end;

procedure TDuplicateSnippetDlg.ValidateData;
var
  ErrMsg: string;
  ErrSel: TSelection;
resourcestring
  sNoCategory = 'You must choose a category';
begin
  if not TSnippetValidator.ValidateName(
    StrTrim(edUniqueName.Text), True, ErrMsg, ErrSel
  ) then
    raise EDataEntry.Create(ErrMsg, edUniqueName, ErrSel);
  if cbCategory.ItemIndex = -1 then
    raise EDataEntry.Create(sNoCategory, cbCategory);
end;

procedure TDuplicateSnippetDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fCatList := TCategoryListAdapter.Create(Database.Categories);
  fOptions := TPersistentOptions.Create;
end;

procedure TDuplicateSnippetDlg.FormDestroy(Sender: TObject);
begin
  if (ModalResult = mrOK) and chkEdit.Checked then
    TUserDBMgr.EditSnippet(StrTrim(edUniqueName.Text));
  fOptions.EditSnippetOnClose := chkEdit.Checked;
  inherited;
  fOptions.Free;
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

