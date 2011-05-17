{
 * FmCodeImportDlg.pas
 *
 * Implements a wizrd dialog box that handles the import of user defined
 * snippets into the database. Permits snippets from the import file to be
 * renamed or rejected.
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
 * The Original Code is FmCodeImportDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmCodeImportDlg;

interface

uses
  // Delphi
  Classes, ActnList, Controls, ComCtrls, StdCtrls, ExtCtrls, Forms,
  // Project
  FmWizardDlg, UBaseObjects, UCodeImportMgr;

type
  TCodeImportDlg = class(TWizardDlg, INoPublicConstruct)
    tsInfo: TTabSheet;
    tsFile: TTabSheet;
    tsUserInfo: TTabSheet;
    tsUpdate: TTabSheet;
    lblIntro: TLabel;
    lblFile: TLabel;
    edFile: TEdit;
    btnBrowse: TButton;
    tsFinish: TTabSheet;
    lblName: TLabel;
    lblEmail: TLabel;
    lblComments: TLabel;
    edComments: TMemo;
    lvImports: TListView;
    lblImportList: TLabel;
    edName: TEdit;
    edEmail: TEdit;
    lblLoadFile: TLabel;
    btnRename: TButton;
    edRename: TEdit;
    lblSelectedSnippet: TLabel;
    alMain: TActionList;
    actRename: TAction;
    actBrowse: TAction;
    lblModifyInstructions: TLabel;
    lblFinish: TLabel;
    sbFinish: TScrollBox;
    procedure lvImportsItemChecked(Sender: TObject; Item: TListItem);
    procedure lvImportsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure actRenameUpdate(Sender: TObject);
    procedure actRenameExecute(Sender: TObject);
    procedure actBrowseExecute(Sender: TObject);
  strict private
    const
      // Indices of wizard pages
      cIntroPage = 0;
      cFilePage = 1;
      cUserInfoPage = 2;  // displayed only there is user info
      cUpdatePage = 3;
      cFinishPage = 4;
      // Index of subitems in list view
      cLVActionIdx = 1;
      cLVImportName = 0;
    var
      fImportMgr: TCodeImportMgr;
    procedure ValidatePage(const PageIdx: Integer);
    function ValidateSnippetName(const Name: string; const Item: TListItem;
      out ErrMsg: string): Boolean;
    procedure ReadImportFile;
    function GetFileNameFromEditCtrl: string;
    class procedure CanOpenDialogClose(Sender: TObject;
      var CanClose: Boolean);
    procedure InitUserInfo;
    procedure InitImportInfo;
    function CountImportSnippets: Integer;
    function GetImportAsNameFromLV(const Item: TListItem): string;
    procedure SetImportNameInLV(const Item: TListItem; const Value: string);
    procedure SetActionInLV(const Item: TListItem; const Value: string);
    procedure UpdateImportData(const Item: TListItem);
    procedure UpdateActionDisplay(const Item: TListItem);
    procedure UpdateDatabase;
    procedure PresentResults;
  strict protected
    constructor InternalCreate(AOwner: TComponent;
      const ImportMgr: TCodeImportMgr); reintroduce;
    procedure ArrangeForm; override;
    function HeadingText(const PageIdx: Integer): string; override;
    procedure UpdateButtons(const PageIdx: Integer); override;
    procedure BeginPage(const PageIdx: Integer); override;
    procedure MoveForward(const PageIdx: Integer; var CanMove: Boolean);
      override;
    function NextPage(const PageIdx: Integer): Integer; override;
    function PrevPage(const PageIdx: Integer): Integer; override;
  public
    class function Execute(AOwner: TComponent; const ImportMgr: TCodeImportMgr):
      Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils, Dialogs,
  // Project
  UCtrlArranger, UExceptions, UMessageBox, UOpenDialogEx, UOpenDialogHelper,
  USnippetValidator;

{$R *.dfm}

{ TCodeImportDlg }

procedure TCodeImportDlg.actBrowseExecute(Sender: TObject);
var
  OpenDlg: TOpenDialogEx; // self-aligning enhanced open dialog box
resourcestring
  sFilter = 'CodeSnip export files (*.csexp)|*.csexp|'  // file filter
    + 'All files (*.*)|*.*';
  sTitle = 'Import File';                               // dialog box title
begin
  // Create and initialise
  OpenDlg := TOpenDialogEx.Create(nil);
  try
    OpenDlg.OnCanClose := CanOpenDialogClose;
    OpenDlg.Filter := sFilter;
    OpenDlg.FilterIndex := 1;
    OpenDlg.InitialDir := '';
    // we don't include ofFileMustExist in Options below since we handle
    // non-existant files ourselves
    OpenDlg.Options := [ofHideReadOnly, ofEnableSizing];
    OpenDlg.OptionsEx := [];
    OpenDlg.Title := sTitle;
    OpenDlg.HelpKeyword := 'ImportFileDlg';
    if OpenDlg.Execute then
      // User OKd: record entered file name
      edFile.Text := OpenDlg.FileName;
  finally
    OpenDlg.Free;
  end;
end;

procedure TCodeImportDlg.actRenameExecute(Sender: TObject);
var
  ErrMsg: string;
begin
  if not ValidateSnippetName(edRename.Text, lvImports.Selected, ErrMsg) then
    raise EDataEntry.Create(ErrMsg, edRename);
  SetImportNameInLV(lvImports.Selected, edRename.Text);
  UpdateImportData(lvImports.Selected);
end;

procedure TCodeImportDlg.actRenameUpdate(Sender: TObject);
begin
  actRename.Enabled := Assigned(lvImports.Selected);
  edRename.Enabled := Assigned(lvImports.Selected);
end;

procedure TCodeImportDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);
  // Arrange controls on tab sheets
  // tsInfo
  { nothing to do }
  // tsFile
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblFile, 6), [edFile, btnBrowse]
  );
  lblLoadFile.Top := TCtrlArranger.BottomOf([edFile, btnBrowse], 12);
  // tsUserInfo
  TCtrlArranger.AlignVCentres(8, [lblName, edName]);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblName, edName], 8), [lblEmail, edEmail]
  );
  lblComments.Top := TCtrlArranger.BottomOf([lblEmail, edEmail], 8);
  edComments.Top := lblComments.Top;
  // tsUpdate
  lblImportList.Top := TCtrlArranger.BottomOf(lblModifyInstructions, 8);
  lvImports.Top := TCtrlArranger.BottomOf(lblImportList, 6);
  lblSelectedSnippet.Top := TCtrlArranger.BottomOf(lvImports, 8);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblSelectedSnippet, 6), [edRename, btnRename]
  );
  // tsFinish
  sbFinish.Top := TCtrlArranger.BottomOf(lblFinish, 6);

  // Size body
  pnlBody.ClientHeight := TCtrlArranger.MaxContainerHeight(
    [tsInfo, tsFile, tsUserInfo, tsUpdate, tsFinish]
  ) + pnlBody.ClientHeight - tsInfo.Height;

  // Arrange inherited controls and size the form
  inherited;
end;

procedure TCodeImportDlg.BeginPage(const PageIdx: Integer);
begin
  case PageIdx of
    cUserInfoPage: InitUserInfo;
    cUpdatePage: InitImportInfo;
    cFinishPage: PresentResults;
  end;
end;

class procedure TCodeImportDlg.CanOpenDialogClose(Sender: TObject;
  var CanClose: Boolean);
var
  Dlg: TOpenDialogEx; // dialog box instance triggering this event
  FileSpec: string;   // full path to entered or selected file name
resourcestring
  // Error messages
  sFileDoesNotExist = '"%s" does not exist.';
begin
  Dlg := Sender as TOpenDialogEx;
  FileSpec := FileOpenEditedFileName(Dlg);
  CanClose := FileExists(FileSpec);
  if not CanClose then
    TMessageBox.Error(Dlg, Format(sFileDoesNotExist, [FileSpec]));
end;

function TCodeImportDlg.CountImportSnippets: Integer;
var
  DataItem: TImportInfo;
begin
  Result := 0;
  for DataItem in fImportMgr.ImportInfo do
    if not DataItem.Skip then
      Inc(Result);
end;

class function TCodeImportDlg.Execute(AOwner: TComponent;
  const ImportMgr: TCodeImportMgr): Boolean;
begin
  with InternalCreate(AOwner, ImportMgr) do
    try
      Result := ShowModal = mrOK;
    finally
      Free;
    end;
end;

function TCodeImportDlg.GetFileNameFromEditCtrl: string;
begin
  Result := Trim(edFile.Text);
end;

function TCodeImportDlg.GetImportAsNameFromLV(const Item: TListItem): string;
begin
  if Item.SubItems.Count <= cLVImportName then
    Exit('');
  Result := Item.SubItems[cLVImportName];
end;

function TCodeImportDlg.HeadingText(const PageIdx: Integer): string;
resourcestring
  sIntroPageheading = 'Import snippets from a file';
  sFilePage = 'Choose import file';
  sUserInfoPage = 'User information';
  sUpdatePage = 'Edit import and update database';
  sFinishPage = 'Import complete';
begin
  case PageIdx of
    cIntroPage:     Result := sIntroPageheading;
    cFilePage:      Result := sFilePage;
    cUserInfoPage:  Result := sUserInfoPage;
    cUpdatePage:    Result := sUpdatePage;
    cFinishPage:    Result := sFinishPage;
  end;
end;

procedure TCodeImportDlg.InitImportInfo;

  procedure AddListItem(const Info: TImportInfo);
  var
    LI: TListItem;
  begin
    LI := lvImports.Items.Add;
    LI.SubItems.Add('');
    LI.SubItems.Add('');
    LI.Caption := Info.OrigName;
    SetImportNameInLV(LI, Info.ImportAsName);
    LI.Checked := not Info.Skip;
    UpdateActionDisplay(LI);
  end;

var
  InfoItem: TImportInfo;
begin
  lvImports.Items.BeginUpdate;
  try
    lvImports.OnItemChecked := nil;
    lvImports.Clear;
    for InfoItem in fImportMgr.ImportInfo do
      AddListItem(InfoItem);
    if lvImports.Items.Count > 0 then
      lvImports.Selected := lvImports.Items[0];
    lvImports.OnItemChecked := lvImportsItemChecked;
  finally
    lvImports.Items.EndUpdate;
  end;
end;

procedure TCodeImportDlg.InitUserInfo;
begin
  edName.Text := fImportMgr.UserInfo.Details.Name;
  edEmail.Text := fImportMgr.UserInfo.Details.Email;
  edComments.Text := fImportMgr.UserInfo.Comments;
end;

constructor TCodeImportDlg.InternalCreate(AOwner: TComponent;
  const ImportMgr: TCodeImportMgr);
begin
  inherited InternalCreate(AOwner);
  fImportMgr := ImportMgr;
end;

procedure TCodeImportDlg.lvImportsItemChecked(Sender: TObject; Item: TListItem);
begin
  UpdateActionDisplay(Item);
  UpdateImportData(Item);
end;

procedure TCodeImportDlg.lvImportsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Assigned(Item) then
    edRename.Text := GetImportAsNameFromLV(Item);
end;

procedure TCodeImportDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
begin
  CanMove := False;
  try
    ValidatePage(PageIdx);
    case PageIdx of
      cFilePage: ReadImportFile;
      cUpdatePage: UpdateDatabase;
    end;
    CanMove := True;
  except
    on E: EDataEntry do
    begin
      TMessageBox.Error(Self, E.Message);
      if Assigned(E.Ctrl) then
        E.Ctrl.SetFocus;
    end;
    on E: ECodeImportMgr do
      TMessageBox.Error(Self, E.Message);
  end;
end;

function TCodeImportDlg.NextPage(const PageIdx: Integer): Integer;
begin
  case PageIdx of
    cFilePage:
      if fImportMgr.UserInfo.IsNul then
        Exit(cUpdatePage);
  end;
  Result := inherited NextPage(PageIdx);
end;

procedure TCodeImportDlg.PresentResults;

  procedure AddLabel(var Top: Integer; const SnippetName: string);
  var
    Lbl: TLabel;
  begin
    Lbl := TLabel.Create(Self);
    Lbl.Parent := sbFinish;
    Lbl.Left := 0;
    Lbl.Top := Top;
    Lbl.Caption := '» ' + SnippetName;
    Top := TCtrlArranger.BottomOf(Lbl, 2);
  end;

var
  DataItem: TImportInfo;
  LblTop: Integer;
begin
  LblTop := 0;
  for DataItem in fImportMgr.ImportInfo do
  begin
    if DataItem.Skip then
      Continue;
    AddLabel(LblTop, DataItem.ImportAsName);
  end;
end;

function TCodeImportDlg.PrevPage(const PageIdx: Integer): Integer;
begin
  case PageIdx of
    cUpdatePage:
      if fImportMgr.UserInfo.IsNul then
        Exit(cFilePage);
  end;
  Result := inherited PrevPage(PageIdx);
end;

procedure TCodeImportDlg.ReadImportFile;
begin
  fImportMgr.Import(GetFileNameFromEditCtrl);
end;

procedure TCodeImportDlg.SetActionInLV(const Item: TListItem;
  const Value: string);
begin
  if Item.SubItems.Count <= cLVActionIdx then
    Exit;
  Item.SubItems[cLVActionIdx] := Value;
end;

procedure TCodeImportDlg.SetImportNameInLV(const Item: TListItem;
  const Value: string);
begin
  if Item.SubItems.Count <= cLVImportName then
    Exit;
  Item.SubItems[cLVImportName] := Value;
end;

procedure TCodeImportDlg.UpdateActionDisplay(const Item: TListItem);
resourcestring
  sSkip = 'Skip';
  sImport = 'Import';
begin
  if not Assigned(Item) then
    Exit;
  if Item.Checked then
    SetActionInLV(Item, sImport)
  else
    SetActionInLV(Item, sSkip);
end;

procedure TCodeImportDlg.UpdateButtons(const PageIdx: Integer);
resourcestring
  sUpdateCaption = 'Update';
begin
  inherited;
  case PageIdx of
    cUpdatePage: btnNext.Caption := sUpdateCaption;
    cFinishPage: btnBack.Enabled := False;
  end;
end;

procedure TCodeImportDlg.UpdateDatabase;
begin
  Screen.Cursor := crHourglass;
  try
    Enabled := False;
    Application.ProcessMessages;
    fImportMgr.UpdateDatabase;
  finally
    Enabled := True;
    Screen.Cursor := crDefault;
  end;
end;

procedure TCodeImportDlg.UpdateImportData(const Item: TListItem);
var
  DataItem: TImportInfo;
  Idx: Integer;
begin
  if not Assigned(Item) then
    Exit;
  Idx := fImportMgr.ImportInfo.IndexOfName(Item.Caption);
  if Idx = -1 then
    raise EBug.Create(
      ClassName + '.UpdateImportData: Can''t find import data item.'
    );
  DataItem := TImportInfo.Create(
    Item.Caption, GetImportAsNameFromLV(Item), not Item.Checked
  );
  fImportMgr.ImportInfo.Items[Idx] := DataItem;
end;

procedure TCodeImportDlg.ValidatePage(const PageIdx: Integer);
resourcestring
  sNoFileNameError = 'No file name specified. Please enter one.';
  sBadFileNameError = 'File "%s" does not exist.';
  sNoSnippetsSelected = 'No snippets are selected for import. Either select '
    + 'one or more or cancel the import.';
begin
  case PageIdx of
    cFilePage:
    begin
      if GetFileNameFromEditCtrl = '' then
        raise EDataEntry.Create(sNoFileNameError, edFile);
      if not FileExists(GetFileNameFromEditCtrl) then
        raise EDataEntry.CreateFmt(
          sBadFileNameError, [GetFileNameFromEditCtrl], edFile
        );
    end;
    cUpdatePage:
    begin
      if CountImportSnippets = 0 then
        raise EDataEntry.Create(sNoSnippetsSelected, lvImports);
    end;
  end;
end;

function TCodeImportDlg.ValidateSnippetName(const Name: string;
  const Item: TListItem; out ErrMsg: string): Boolean;
resourcestring
  sDuplicateName = '"%s" duplicates a name in the import list.';
var
  LI: TListItem;
begin
  Result := TSnippetValidator.ValidateName(Name, True, ErrMsg);
  if not Result then
    Exit;
  for LI in lvImports.Items do
  begin
    if LI = Item then
      Continue;     // this is item we're about to change: ignore its name
    if AnsiSameText(Name, GetImportAsNameFromLV(LI)) then
    begin
      ErrMsg := Format(sDuplicateName, [Name]);
      Exit(False);
    end;
  end;
end;

end.
