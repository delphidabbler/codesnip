{
 * FmDuplicateSnippetDlg.pas
 *
 * Implements a dialog box which can create a duplicate copy of asnippet.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is FmDuplicateSnippetDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmDuplicateSnippetDlg;


interface


uses
  // Delphi
  SysUtils, Controls, StdCtrls, ExtCtrls, Classes,
  // Project
  DB.UCategory, DB.USnippet, FmGenericOKDlg, UBaseObjects, UIStringList;


type
  TDuplicateSnippetDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblNewName: TLabel;
    edNewName: TEdit;
    lblCategory: TLabel;
    cbCategory: TComboBox;
    procedure btnOKClick(Sender: TObject);
  strict private
    var
      fSnippet: TSnippet;
    function DisallowedNames: IStringList;
    function UniqueSnippetName(const BaseName: string): string;
    procedure ValidateData;
    procedure HandleException(const E: Exception);
    function SelectedCategory: TCategory;
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
  DB.UMain, UCategoryListAdapter, UCtrlArranger, UExceptions,
  UMessageBox, USnippetValidator, UStructs, UStrUtils;

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
      Caption := Format(sCaption, [ASnippet.Name]);
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
  CatList: TCategoryListAdapter;
  SnippetCat: TCategory;
begin
  inherited;
  edNewName.Text := UniqueSnippetName(fSnippet.Name);
  CatList := TCategoryListAdapter.Create(Database.Categories);
  try
    CatList.ToStrings(cbCategory.Items);
  finally
    CatList.Free;
  end;
  SnippetCat := Database.Categories.Find(fSnippet.Category);
  if Assigned(SnippetCat) then
    cbCategory.ItemIndex := cbCategory.Items.IndexOf(SnippetCat.Description)
  else
    cbCategory.ItemIndex := -1;
end;

function TDuplicateSnippetDlg.SelectedCategory: TCategory;
begin
  Result := cbCategory.Items.Objects[cbCategory.ItemIndex] as TCategory;
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
    SelectedCategory.ID
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

end.
