{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2013, Peter Johnson (www.delphidabbler.com).
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
  DB.USnippet, FmGenericOKDlg, UBaseObjects, UCategoryListAdapter,
  UIStringList;


type
  TDuplicateSnippetDlg = class(TGenericOKDlg, INoPublicConstruct)
    cbCategory: TComboBox;
    chkEdit: TCheckBox;
    edTitle: TEdit;
    lblCategory: TLabel;
    lblTitle: TLabel;
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
      fNewSnippet: TSnippet;
      fCatList: TCategoryListAdapter;
      fOptions: TPersistentOptions;
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
  DB.UCategory, DB.UMain, UCtrlArranger, UExceptions, UMessageBox, USettings,
  USnippetValidator, UStructs, UStrUtils, UUserDBMgr;

{$R *.dfm}

{ TDuplicateSnippetDlg }

procedure TDuplicateSnippetDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);

  TCtrlArranger.AlignLefts(
    [lblTitle, lblCategory, edTitle, cbCategory, chkEdit], 0
  );

  lblTitle.Top := 0;
  TCtrlArranger.MoveBelow(lblTitle, edTitle, 4);
  TCtrlArranger.MoveBelow(edTitle, lblCategory, 8);
  TCtrlArranger.MoveBelow(lblCategory, cbCategory, 4);
  TCtrlArranger.MoveBelow(cbCategory, chkEdit, 20);

  pnlBody.ClientWidth := Max(
    TCtrlArranger.TotalControlWidth(pnlBody) + 8,
    TCtrlArranger.RightOf(btnHelp) - btnOK.Left
  );

  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;

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

class function TDuplicateSnippetDlg.Execute(const AOwner: TComponent;
  const ASnippet: TSnippet): Boolean;
resourcestring
  sCaption = 'Duplicate %s';   // dialog box caption
begin
  Assert(Assigned(ASnippet), ClassName + '.Execute: ASnippet is nil');
  with InternalCreate(AOwner) do
    try
      Caption := Format(sCaption, [ASnippet.Title]);
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
  edTitle.Text := fSnippet.Title;
  fCatList.ToStrings(cbCategory.Items);
  SnippetCat := _Database.Categories.Find(fSnippet.Category);
  if Assigned(SnippetCat) then
    cbCategory.ItemIndex := cbCategory.Items.IndexOf(SnippetCat.Description)
  else
    cbCategory.ItemIndex := -1;
  chkEdit.Checked := fOptions.EditSnippetOnClose;
end;

procedure TDuplicateSnippetDlg.UpdateDatabase;
var
  DisplayName: string;
begin
  DisplayName := StrTrim(edTitle.Text);
  fNewSnippet := (_Database as IDatabaseEdit).DuplicateSnippet(
    fSnippet,
    DisplayName,
    fCatList.CatID(cbCategory.ItemIndex)
  );
end;

procedure TDuplicateSnippetDlg.ValidateData;
var
  ErrMsg: string;
resourcestring
  sNoCategory = 'You must choose a category';
begin
  if not TSnippetValidator.ValidateTitle(edTitle.Text, ErrMsg) then
    raise EDataEntry.Create(ErrMsg, edTitle);
  if cbCategory.ItemIndex = -1 then
    raise EDataEntry.Create(sNoCategory, cbCategory);
end;

procedure TDuplicateSnippetDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fCatList := TCategoryListAdapter.Create(_Database.Categories);
  fOptions := TPersistentOptions.Create;
end;

procedure TDuplicateSnippetDlg.FormDestroy(Sender: TObject);
begin
  if (ModalResult = mrOK) and chkEdit.Checked then
    TUserDBMgr.EditSnippet(fNewSnippet.ID);
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

