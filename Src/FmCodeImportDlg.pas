{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a wizard dialogue box that handles the import of user defined
 * snippets into the database. Permits snippets from the import file to be
 * renamed or rejected.
}


unit FmCodeImportDlg;

interface

uses
  // Delphi
  Classes,
  ActnList,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  Forms,
  // Project
  DB.UCollections,
  FmWizardDlg,
  UBaseObjects,
  UCodeImportMgr;

type
  ///  <summary>
  ///  Wizard dialog box that handles the import of user defined snippets into
  ///  the user database. Permits snippets from the import file to be renamed or
  ///  rejected.
  ///  </summary>
  TCodeImportDlg = class(TWizardDlg, INoPublicConstruct)
    tsInfo: TTabSheet;
    tsFile: TTabSheet;
    tsUpdate: TTabSheet;
    lblIntro: TLabel;
    lblFile: TLabel;
    edFile: TEdit;
    btnBrowse: TButton;
    tsFinish: TTabSheet;
    lvImports: TListView;
    lblImportList: TLabel;
    lblLoadFile: TLabel;
    alMain: TActionList;
    actBrowse: TAction;
    lblModifyInstructions: TLabel;
    lblFinish: TLabel;
    sbFinish: TScrollBox;
    ///  <summary>Handles clicks on list view check boxes.</summary>
    procedure lvImportsItemChecked(Sender: TObject; Item: TListItem);
    ///  <summary>Handles request to display open file dialog box to get import
    ///  file name.</summary>
    procedure actBrowseExecute(Sender: TObject);
    ///  <summary>Frees objects stored in list view items' Data properties.
    ///  </summary>
    procedure FormDestroy(Sender: TObject);
  strict private
    const
      {TODO -cCollections: Insert a new page to get collection to receive
              imports from user after cFilePage and before cUpdatePage. That way
              we can be sure that the collection won't change after selecting
              snippets.}
      // Indices of wizard pages
      cIntroPage    = 0;
      cFilePage     = 1;
      cUpdatePage   = 2;
      cFinishPage   = 3;
      // Index of subitems in list view
      cLVActionIdx  = 0;
    var
      ///  <summary>Reference to import manager object used to perform import
      ///  operations.</summary>
      fImportMgr: TCodeImportMgr;
    ///  <summary>Validates entries on wizard pages indetified by the page
    ///  index.</summary>
    procedure ValidatePage(const PageIdx: Integer);
    ///  <summary>Reads input file from disk.</summary>
    procedure ReadImportFile;
    ///  <summary>Retrieves import file name from edit control where it is
    ///  entered.</summary>
    function GetFileNameFromEditCtrl: string;
    ///  <summary>Checks if an open file open dialog box can close. Displays an
    ///  error message if not.</summary>
    class procedure CanOpenDialogClose(Sender: TObject;
      var CanClose: Boolean);
    ///  <summary>Displays current details of all snippets in import file in
    ///  list view on update page.</summary>
    procedure InitImportInfo;
    ///  <summary>Counts snippets that will be / have been added to database.
    ///  Excludes any snippets skipped by user.</summary>
    function CountImportSnippets: Integer;
    ///  <summary>Updates given list item with description of current import
    ///  action for the associated snippet.</summary>
    procedure SetActionInLV(const Item: TListItem; const Value: string);
    ///  <summary>Updates data that describes snippets to import from values
    ///  stored in given list item.</summary>
    procedure UpdateImportData(const Item: TListItem);
    ///  <summary>Updates description of import action displayed in given list
    ///  item depending on state of list item's check box.</summary>
    procedure UpdateActionDisplay(const Item: TListItem);
    ///  <summary>Updates database with imported snippets.</summary>
    ///  <remarks>UI is disabled during this process.</remarks>
    procedure UpdateDatabase;
    ///  <summary>Displays names of imported snippets on finish page.</summary>
    procedure PresentResults;
    ///  <summary>Gets the ID of the collection into which all snippets are to
    ///  be imported.</summary>
    function GetCollectionID: TCollectionID;
  strict protected
    ///  <summary>Protected constructor that sets up object to use given import
    ///  manager object.</summary>
    constructor InternalCreate(AOwner: TComponent;
      const ImportMgr: TCodeImportMgr); reintroduce;
    ///  <summary>Aligns and arranges controls in each tab sheet and sizes
    ///  dialog box to accomodate controls.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure ArrangeForm; override;
    ///  <summary>Returns text of heading on page indexed by PageIdx.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    function HeadingText(const PageIdx: Integer): string; override;
    ///  <summary>Updates state and caption of buttons on page index by
    ///  PageIdx.</summary>
    ///  <remarks>Implementation of abstract method called from ancestor class.
    ///  </remarks>
    procedure UpdateButtons(const PageIdx: Integer); override;
    ///  <summary>Initialises page indexed by PageIdx.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure BeginPage(const PageIdx: Integer); override;
    ///  <summary>Validates page specified by PageIdx then performs any action
    ///  required before next page is displayed.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure MoveForward(const PageIdx: Integer; var CanMove: Boolean);
      override;
  public
    ///  <summary>Displays wizard, passing a reference to import manager object
    ///  to be used for import operations. Returns True if wizard finishes or
    ///  False if wizard cancelled.</summary>
    class function Execute(AOwner: TComponent; const ImportMgr: TCodeImportMgr):
      Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Dialogs,
  // Project
  UBox,
  UCtrlArranger,
  UExceptions,
  UMessageBox,
  UOpenDialogEx,
  UOpenDialogHelper,
  USnippetValidator,
  UStrUtils;

{$R *.dfm}


{ TCodeImportDlg }

procedure TCodeImportDlg.actBrowseExecute(Sender: TObject);
var
  OpenDlg: TOpenDialogEx; // self-aligning enhanced open dialog box
resourcestring
  sFilter = 'CodeSnip export files (*.csexp)|*.csexp|'  // file filter
    + 'All files (*.*)|*.*';
  sTitle = 'Import File';                               // dialogue box title
begin
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
      edFile.Text := OpenDlg.FileName;
  finally
    OpenDlg.Free;
  end;
end;

procedure TCodeImportDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);

  // Arrange controls on tab sheets

  // tsInfo
  // nothing to do

  // tsFile
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblFile, 6), [edFile, btnBrowse]
  );
  lblLoadFile.Top := TCtrlArranger.BottomOf([edFile, btnBrowse], 12);

  // tsUpdate
  lblImportList.Top := TCtrlArranger.BottomOf(lblModifyInstructions, 8);
  lvImports.Top := TCtrlArranger.BottomOf(lblImportList, 6);

  // tsFinish
  sbFinish.Top := TCtrlArranger.BottomOf(lblFinish, 6);

  // Size body
  pnlBody.ClientHeight := TCtrlArranger.MaxContainerHeight(
    [tsInfo, tsFile, tsUpdate, tsFinish]
  ) + pnlBody.ClientHeight - tsInfo.Height;

  // Arrange inherited controls and size the form
  inherited;
end;

procedure TCodeImportDlg.BeginPage(const PageIdx: Integer);
begin
  case PageIdx of
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
  DataItem: TImportInfo;  // each item of import information
begin
  Result := 0;
  for DataItem in fImportMgr.ImportInfo do
    if not DataItem.Skip then
      Inc(Result);
end;

class function TCodeImportDlg.Execute(AOwner: TComponent;
  const ImportMgr: TCodeImportMgr): Boolean;
var
  Dlg: TCodeImportDlg;
begin
  Dlg := InternalCreate(AOwner, ImportMgr);
  try
    Result := Dlg.ShowModal = mrOK;
  finally
    Dlg.Free;
  end;
end;

procedure TCodeImportDlg.FormDestroy(Sender: TObject);
var
  Idx: Integer;
begin
  inherited;
  // Free the TBox<> objects stored in list item data pointer
  for Idx := Pred(lvImports.Items.Count) downto 0 do
    TObject(lvImports.Items[Idx].Data).Free;
end;

function TCodeImportDlg.GetCollectionID: TCollectionID;
begin
  {TODO -cCollections: Add code to get user's choice of collection into which
          snippets are to be imported. Will need a drop down list of available
          collections. At present, only the "user" collection is permitted.
  }
  Result := TCollectionID.__TMP__UserDBCollectionID;
end;

function TCodeImportDlg.GetFileNameFromEditCtrl: string;
begin
  Result := StrTrim(edFile.Text);
end;

function TCodeImportDlg.HeadingText(const PageIdx: Integer): string;
resourcestring
  // Page headings
  sIntroPageheading = 'Import snippets from a file';
  sFilePage = 'Choose import file';
  sUpdatePage = 'Edit import and update database';
  sFinishPage = 'Import complete';
begin
  case PageIdx of
    cIntroPage:     Result := sIntroPageheading;
    cFilePage:      Result := sFilePage;
    cUpdatePage:    Result := sUpdatePage;
    cFinishPage:    Result := sFinishPage;
  end;
end;

procedure TCodeImportDlg.InitImportInfo;

  ///  Creates a new list view items containing given information.
  procedure AddListItem(const Info: TImportInfo);
  var
    LI: TListItem;  // new list item
  begin
    LI := lvImports.Items.Add;
    LI.SubItems.Add('');
    LI.Caption := Info.DisplayName;
    LI.Checked := not Info.Skip;
    LI.Data := TBox<TImportInfo>.Create(Info);
    UpdateActionDisplay(LI);
  end;

var
  InfoItem: TImportInfo;  // import info item describing an imported snippet
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

constructor TCodeImportDlg.InternalCreate(AOwner: TComponent;
  const ImportMgr: TCodeImportMgr);
begin
  inherited InternalCreate(AOwner);
  fImportMgr := ImportMgr;
  fImportMgr.RequestCollectionCallback := GetCollectionID;
end;

procedure TCodeImportDlg.lvImportsItemChecked(Sender: TObject; Item: TListItem);
begin
  UpdateActionDisplay(Item);
  UpdateImportData(Item);
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

procedure TCodeImportDlg.PresentResults;

  ///  Creates a label containing name of an imported snippet and adds it to
  ///  scroll box with top at given position.
  procedure AddLabel(var Top: Integer; const SnippetName: string);
  var
    Lbl: TLabel;
  begin
    Lbl := TLabel.Create(Self);
    Lbl.Parent := sbFinish;
    Lbl.Left := 0;
    Lbl.Top := Top;
    Lbl.Caption := '� ' + SnippetName;
    Top := TCtrlArranger.BottomOf(Lbl, 2);
  end;

var
  DataItem: TImportInfo;  // description of each snippet from import file
  LblTop: Integer;        // vertical position of top of next label in scrollbox
begin
  LblTop := 0;
  for DataItem in fImportMgr.ImportInfo do
  begin
    if DataItem.Skip then
      Continue;
    AddLabel(LblTop, DataItem.DisplayName);
  end;
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

procedure TCodeImportDlg.UpdateActionDisplay(const Item: TListItem);
resourcestring
  // description of actions
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
  // button caption for update page
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
begin
  if not Assigned(Item) then
    Exit;
  fImportMgr.ImportInfo.SetSkip(
    TBox<TImportInfo>(Item.Data).Value.OrigKey,
    not Item.Checked
  );
end;

procedure TCodeImportDlg.ValidatePage(const PageIdx: Integer);
resourcestring
  // Error messages
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

end.
