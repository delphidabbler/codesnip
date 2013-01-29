{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box which can create a duplicate copy of asnippet.
}


unit FmDuplicateSnippetDlg;


interface


uses
  // Delphi
  SysUtils, Controls, StdCtrls, ExtCtrls, Classes,
  // Project
  DB.UCategory, DB.USnippet, FmGenericOKDlg, UBaseObjects, UCategoryListAdapter,
  UIStringList;


type
  TDuplicateSnippetDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblNewName: TLabel;
    edNewName: TEdit;
    lblCategory: TLabel;
    cbCategory: TComboBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    var
      fSnippet: TSnippet;
      fCatList: TCategoryListAdapter;
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
  DB.UMain, UCtrlArranger, UExceptions, UMessageBox, USnippetValidator,
  UStructs, UStrUtils;

{$R *.dfm}

{ TDuplicateSnippetDlg }

procedure TDuplicateSnippetDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);
  edNewName.Top := TCtrlArranger.BottomOf(lblNewName, 4);
  lblCategory.Top := TCtrlArranger.BottomOf(edNewName, 8);
  cbCategory.Top := TCtrlArranger.BottomOf(lblCategory, 4);

  pnlBody.ClientWidth := Max(
    Max(
      TCtrlArranger.RightOf(lblNewName),
      TCtrlArranger.RightOf(lblCategory)
    ),
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
    if Snippet.UserDefined then
      Result.Add(Snippet.Name);
end;

class function TDuplicateSnippetDlg.Execute(const AOwner: TComponent;
  const ASnippet: TSnippet): Boolean;
resourcestring
  sCaption = 'Duplicate %s';   // dialog box caption
begin
  Assert(Assigned(ASnippet), ClassName + '.Execute: ASnippet is nil');
  with InternalCreate(AOwner) do
    try
      Caption := Format(sCaption, [ASnippet.DisplayName]);
      fSnippet := ASnippet;
      Result := ShowModal = mrOK;
    finally
      Free;
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
  edNewName.Text := UniqueSnippetName(fSnippet.Name);
  fCatList.ToStrings(cbCategory.Items);
  SnippetCat := Database.Categories.Find(fSnippet.Category);
  if Assigned(SnippetCat) then
    cbCategory.ItemIndex := cbCategory.Items.IndexOf(SnippetCat.Description)
  else
    cbCategory.ItemIndex := -1;
end;

function TDuplicateSnippetDlg.UniqueSnippetName(const BaseName: string): string;
var
  ExistingNames: IStringList;
  Postfix: Cardinal;      // number to be appended to name to make unique
begin
  ExistingNames := DisallowedNames;
  if not ExistingNames.Contains(BaseName) then
    Exit(BaseName);
  Postfix := 1;
  repeat
    Inc(PostFix);
    Result := BaseName + IntToStr(PostFix);
  until not ExistingNames.Contains(Result);
end;

procedure TDuplicateSnippetDlg.UpdateDatabase;
begin
  (Database as IDatabaseEdit).DuplicateSnippet(
    fSnippet,
    StrTrim(edNewName.Text),
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
    StrTrim(edNewName.Text), True, ErrMsg, ErrSel
  ) then
    raise EDataEntry.Create(ErrMsg, edNewName, ErrSel);
  if cbCategory.ItemIndex = -1 then
    raise EDataEntry.Create(sNoCategory, cbCategory);
end;

procedure TDuplicateSnippetDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fCatList := TCategoryListAdapter.Create(Database.Categories);
end;

procedure TDuplicateSnippetDlg.FormDestroy(Sender: TObject);
begin
  inherited;
  fCatList.Free;
end;

end.