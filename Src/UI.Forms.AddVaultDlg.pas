{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2026, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a wizard dialogue box that lets the user add an existing snippet
 * data directory as a new vault.
}


unit UI.Forms.AddVaultDlg;

interface

uses
  // Delphi
  Classes,
  ActnList,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  // Project
  FmWizardDlg,
  DB.Vaults,
  UBaseObjects;

type
  ///  <summary>Wizard dialogue box that enables the user to add an existing
  ///  snippet data directory as a new vault.</summary>
  TAddVaultDlg = class(TWizardDlg, INoPublicConstruct)
    tsIntro: TTabSheet;
    tsName: TTabSheet;
    tsSourcePath: TTabSheet;
    tsConfirm: TTabSheet;
    lblIntroDesc1: TLabel;
    lblIntroDesc2: TLabel;
    lblIntroDesc3: TLabel;
    lblIntroDesc4: TLabel;
    lblName: TLabel;
    edName: TEdit;
    lblSourcePath: TLabel;
    lblSourcePathDesc: TLabel;
    edSourcePath: TEdit;
    btnSourcePath: TButton;
    lblConfirmDesc1: TLabel;
    lvConfirm: TListView;
    lblConfirmDesc2: TLabel;
    alMain: TActionList;
    actBrowseSourcePath: TAction;
    tsFinish: TTabSheet;
    lblFinishMsg1: TLabel;
    lblFinishMsg2: TLabel;
    lblFinishMsg3: TLabel;
    procedure actBrowseSourcePathExecute(Sender: TObject);
  strict private
    const
      // Page indices
      IntroPageIdx = 0;
      NamePageIdx = 1;
      SourcePathPageIdx = 2;
      ConfirmPageIdx = 3;
      FinishPageIdx = 4;

  strict private

    ///  <summary>Gets the text from the given control, trimmed of leading and
    ///  trailing spaces.</summary>
    function GetCtrlText(const ACtrl: TControl): string;

    ///  <summary>Displays the Windows standard browse for folder dialogue box
    ///  configured according to the given parameters. Returns the selected
    ///  folder or an empty string if the user cancels.</summary>
    function BrowseForFolder(const ATitle, AHeading, ADefaultFolder: string;
      const AShowNewFolderButton: Boolean): string;
      {TODO -cRefactor: similar or same method occurs in UI.Forms.AddVaultDlg}

    ///  <summary>Gets a folder name from the user using the Windows standard
    ///  browse for folder dialogue box. If the user accepts the entry the
    ///  selected folder is copied into the given edit control and the text is
    ///  selected.</summary>
    ///  <remarks>The final four parameters are used to configure the dialogue
    ///  box.</remarks>
    procedure SetPathEditTextFromFolderDlg(const AEdit: TEdit;
      const ADlgTitle, ADlgHeading, ADlgDefaultFolder: string;
      const ADlgShowNewFolderButton: Boolean);
      {TODO -cRefactor: similar or same method occurs in UI.Forms.AddVaultDlg}

    ///  <summary>Initialises the controls on the confirmation page. Called when
    ///  the page is displayed.</summary>
    procedure InitConfirmPage;

    ///  <summary>Creates and returns a new vault object that references the
    ///  chosen snippet data.</summary>
    function CreateVault: TVault;

    ///  <summary>Validates entries on the wizard page identified by the given
    ///  page index.</summary>
    ///  <exception>An <c>EDataEntry</c> exception is raised if validation
    ///  fails.</exception>
    procedure ValidatePage(const PageIdx: Integer);

  strict protected

    ///  <summary>Positions controls and sets the form size according to the
    ///  body panel dimensions.</summary>
    procedure ArrangeForm; override;

    ///  <summary>Gets the heading text of a wizard page.</summary>
    ///  <param name="PageIdx"><c>Integer</c> [in] Index of the wizard page for
    ///  which a heading is required.</param>
    ///  <returns><c>string</c>. Required heading text.</returns>
    function HeadingText(const PageIdx: Integer): string; override;

    ///  <summary>Updates the wizard buttons for a given page.</summary>
    ///  <param name="PageIdx"><c>Integer</c> [in] Index of the wizard page for
    ///  which button state is to be updated.</param>
    procedure UpdateButtons(const PageIdx: Integer); override;

    ///  <summary>Updates a given wizard page when it is displayed.</summary>
    ///  <param name="PageIdx"><c>Integer</c> [in] Index of the wizard page to
    ///  be updated.</param>
    procedure BeginPage(const PageIdx: Integer); override;

    ///  <summary>Called before moving forwards in the wizard.</summary>
    ///  <param name="PageIdx"><c>Integer</c> [in] Index of the current wizard
    ///  page.</param>
    ///  <param name="CanMove"><c>Boolean</c> [in/out] Flag that
    ///  indicates whether the page change is allowed. <c>True</c> is always
    ///  passed in. Set to <c>False</c> to inhibit the page change.</param>
    procedure MoveForward(const PageIdx: Integer; var CanMove: Boolean);
      override;

  public

    ///  <summary>Displays the Add Vault wizard dialogue box to allow the user
    ///  to create a new vault that contains existing snippet data.</summary>
    ///  <param name="AOwner"><c>TComponent</c> [in] Reference to the component
    ///  that owns this dialogue box.</param>
    ///  <param name="AVault"><c>TVault</c> [out] Stores details of the new
    ///  vault created by the user. Set to <c>nil</c> if no vault was created.
    ///  </param>
    ///  <returns><c>Boolean</c>. <c>True</c> if a new vault was created or
    ///  <c>False</c> otherwise.</returns>
    ///  <remarks>If the user confirms then the caller is responsible for
    ///  freeing the object referenced by <c>AVault</c>.</remarks>
    class function Execute(const AOwner: TComponent; out AVault: TVault):
      Boolean;

  end;


implementation

uses
  // Delphi
  SysUtils,
  // Project
  ClassHelpers.UControls,
  UBrowseForFolderDlg,
  UCtrlArranger,
  DB.DataFormats,
  DB.IO.DataFormatSniffer,
  DB.Categories,
  DB.Snippets,
  DB.Main,
  DB.MetaData,
  UExceptions,
  UMessageBox,
  UStrUtils,
  UUtils;

{$R *.dfm}

{ TAddVaultDlg }

procedure TAddVaultDlg.actBrowseSourcePathExecute(Sender: TObject);
resourcestring
  sDlgCaption = 'Add vault directory';
  sDlgHeading = 'Choose the directory containing the vault to be added';
var
  DefFolder: string;
begin
  DefFolder := GetCtrlText(edSourcePath);
  SetPathEditTextFromFolderDlg(
    edSourcePath, sDlgCaption, sDlgHeading, DefFolder, False
  );
end;

procedure TAddVaultDlg.ArrangeForm;
const
  TopMargin = 4;
  BottomMargin = 16;
begin
  TCtrlArranger.SetLabelHeights(Self);

  // Intro tab

  TCtrlArranger.AlignLefts(
    [lblIntroDesc1, lblIntroDesc2, lblIntroDesc3, lblIntroDesc4],
    0
  );
  lblIntroDesc1.Width := tsIntro.ClientWidth;
  lblIntroDesc2.Width := tsIntro.ClientWidth;
  lblIntroDesc3.Width := tsIntro.ClientWidth;
  lblIntroDesc4.Width := tsIntro.ClientWidth;
  lblIntroDesc1.Top := TopMargin;
  TCtrlArranger.MoveBelow(lblIntroDesc1, lblIntroDesc2, 12);
  TCtrlArranger.MoveBelow(lblIntroDesc2, lblIntroDesc3, 6);
  TCtrlArranger.MoveBelow(lblIntroDesc3, lblIntroDesc4, 12);

  // Vault name tab

  lblName.Top := TopMargin;
  lblName.Width := tsName.ClientWidth;
  edName.Width := tsName.ClientWidth;
  TCtrlArranger.AlignLefts([lblName, edName], 0);
  TCtrlArranger.MoveBelow(lblName, edName, 8);

  // Source path tab

  lblSourcePath.Top := TopMargin;
  lblSourcePath.Width := tsSourcePath.ClientWidth;
  TCtrlArranger.AlignLefts([lblSourcePath, edSourcePath, lblSourcePathDesc], 0);
  TCtrlArranger.AlignRights([btnSourcePath], tsSourcePath.ClientWidth);
  edSourcePath.Width := tsSourcePath.ClientWidth - btnSourcePath.Width - 8;
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblSourcePath, 8),
    [edSourcePath, btnSourcePath]
  );
  TCtrlArranger.MoveBelow([edSourcePath, btnSourcePath], lblSourcePathDesc, 12);

  // Confirm tab

  TCtrlArranger.AlignLefts([lblConfirmDesc1, lblConfirmDesc2, lvConfirm], 0);
  lblConfirmDesc1.Width := tsConfirm.ClientWidth;
  lblConfirmDesc2.Width := tsConfirm.ClientWidth;
  lvConfirm.Width := tsConfirm.ClientWidth;
  lblConfirmDesc1.Top := TopMargin;
  TCtrlArranger.MoveBelow(lblConfirmDesc1, lvConfirm, 6);
  TCtrlArranger.MoveBelow(lvConfirm, lblConfirmDesc2, 6);

  // Finish tab

  TCtrlArranger.AlignLefts([lblFinishMsg1, lblFinishMsg2, lblFinishMsg3], 0);
  lblFinishMsg1.Width := tsIntro.ClientWidth;
  lblFinishMsg2.Width := tsIntro.ClientWidth;
  lblFinishMsg3.Width := tsIntro.ClientWidth;
  lblFinishMsg1.Top := TopMargin;
  TCtrlArranger.MoveBelow(lblFinishMsg1, lblFinishMsg2, 12);
  TCtrlArranger.MoveBelow(lblFinishMsg2, lblFinishMsg3, 12);

  // Set container dimensions

  pnlBody.ClientHeight := TCtrlArranger.MaxContainerHeight(
    [
      tsIntro, tsName, tsSourcePath, tsConfirm, tsFinish
    ]
  ) + pnlBody.ClientHeight - tsConfirm.Height;
  pnlBody.ClientWidth := TCtrlArranger.MaxContainerWidth(
    [
      tsIntro, tsName, tsSourcePath, tsConfirm, tsFinish
    ]
  ) + pnlBody.ClientWidth - tsIntro.Width;

  // Confirm tab revisited: adjust heights

  lvConfirm.ClientHeight := tsConfirm.ClientHeight - lvConfirm.Top
    - BottomMargin;

  inherited;
end;

procedure TAddVaultDlg.BeginPage(const PageIdx: Integer);
begin
  inherited;
  case PageIdx of
    ConfirmPageIdx: InitConfirmPage;
  end;
end;

function TAddVaultDlg.BrowseForFolder(const ATitle, AHeading,
  ADefaultFolder: string; const AShowNewFolderButton: Boolean): string;
var
  Dlg: TBrowseForFolderDlg; // browse for folder standard dialogue box
begin
  Dlg := TBrowseForFolderDlg.Create(nil);
  try
    Dlg.Title := ATitle;
    Dlg.Headline := AHeading;
    Dlg.FolderName := NearestExistingDirectory(ADefaultFolder);
    Dlg.MakeFolderBtnVisible := AShowNewFolderButton;
    if Dlg.Execute then
      Result := Dlg.FolderName
    else
      Result := '';
  finally
    Dlg.Free;
  end;
end;

function TAddVaultDlg.CreateVault: TVault;
begin
  Result := TVault.Create(
    TVaults.Instance.GetUniqueUID,
    GetCtrlText(edName),
    TDataStorageDetails.Create(
      TDataFormatSniffer.DataFormatFromDir(GetCtrlText(edSourcePath)),
      GetCtrlText(edSourcePath)
    )
  );
end;

class function TAddVaultDlg.Execute(const AOwner: TComponent;
  out AVault: TVault): Boolean;
var
  Dlg: TAddVaultDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Result := Dlg.ShowModal = mrOK;
    if Result then
      AVault := Dlg.CreateVault
    else
      AVault := nil;
  finally
    Dlg.Free;
  end;
end;

function TAddVaultDlg.GetCtrlText(const ACtrl: TControl): string;
begin
  Result := StrTrim(ACtrl.GetCtrlText);
end;

function TAddVaultDlg.HeadingText(const PageIdx: Integer): string;
resourcestring
  sIntro = 'Add a snippet vault';
  sName = 'Vault name';
  sSourcePath = 'Directory containing vault';
  sConfirm = 'Ready to add vault';
  sFinished = 'Finished';
const
  Headings: array[IntroPageIdx..FinishPageIdx] of string = (
    sIntro, sName, sSourcePath, sConfirm, sFinished
  );
begin
  Result := Headings[PageIdx];
end;

procedure TAddVaultDlg.InitConfirmPage;

  procedure AddToLV(const ACaption, AValue: string); overload;
  var
    LI: TListItem;
  begin
    LI := lvConfirm.Items.Add;
    LI.Caption := ACaption;
    LI.SubItems.Add(AValue);
  end;

resourcestring
  sName = 'Name';
  sDirectory = 'Directory';
  sDataFormat = 'Data format';
  sSnippetCount = '# of Snippets';
  sLicenseSPDX = 'License SPDX';
  sLicenseName = 'License name';
  sLicenseURL = 'License URL';
  sCopyrightHolder = 'Copyright holder';
  sCopyrightURL = 'Copyright URL';
  sCopyrightDate = 'Copyright Date';
  sContributors = 'Contributors';
  sAcknowledgements = 'Acknowledgements';
  sVersion = 'Version';
  sMetadata = 'MetaData';
  sNoMetadata = 'None';
const
  ListSeparator = ';';
var
  Vault: TVault;
  Snippets: TSnippetList;
  Categories: TCategoryList;
  DataFormat: TDataFormatKind;
  FormatCaps: TMetaDataCaps;
  MetaData: TMetaData;
begin
  // create & load a temporary vault to get info to display in confirm page
  Snippets := nil;
  Categories := nil;
  Vault := CreateVault;
  try
    Snippets := TSnippetList.Create(True);
    Categories := TCategoryList.Create(True);
    Database.LoadVault(Vault, Snippets, Categories);

    DataFormat := Vault.Storage.Format;
    MetaData := Vault.MetaData;
    FormatCaps := MetaData.Capabilities;

    lvConfirm.Clear;
    // Name
    AddToLV(sName, Vault.Name);
    // Directory
    AddToLV(sDirectory, Vault.Storage.Directory);
    // Data format
    AddToLV(sDataFormat, TDataFormatInfo.GetName(DataFormat));
    // Snippet count
    AddToLV(sSnippetCount, IntToStr(Snippets.Count));
    // Meta data
    FormatCaps := TDataFormatInfo.GetMetaDataCaps(DataFormat);
    if TMetaDataCap.License in FormatCaps then
    begin
      AddToLV(sLicenseSPDX, MetaData.LicenseInfo.SPDX);
      AddToLV(sLicenseName, MetaData.LicenseInfo.Name);
      AddToLV(sLicenseURL, MetaData.LicenseInfo.URL);
    end;
    if TMetaDataCap.Copyright in FormatCaps then
    begin
      AddToLV(sCopyrightHolder, MetaData.CopyrightInfo.Holder);
      AddToLV(sCopyrightURL, MetaData.CopyrightInfo.HolderURL);
      AddToLV(sCopyrightDate, MetaData.CopyrightInfo.Date);
      AddToLV(
        sContributors,
        MetaData.CopyrightInfo.Contributors.GetText(ListSeparator, False)
      );
    end;
    if TMetaDataCap.Acknowledgements in FormatCaps then
      AddToLV(
        sAcknowledgements,
        MetaData.Acknowledgements.GetText(ListSeparator, False)
      );
    if TMetaDataCap.Version in FormatCaps then
      AddToLV(sVersion, MetaData.Version);
    if FormatCaps = [] then
      AddToLV(sMetaData, sNoMetadata);
  finally
    Categories.Free;
    Snippets.Free;
    Vault.Free;
  end;
end;

procedure TAddVaultDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
begin
  CanMove := False;
  try
    ValidatePage(PageIdx);
    CanMove := True;
  except
    on E: EDataEntry do
    begin
      TMessageBox.Error(pcWizard, E.Message);
      if Assigned(E.Ctrl) then
        E.Ctrl.SetFocus;
    end;
  end;
end;

procedure TAddVaultDlg.SetPathEditTextFromFolderDlg(const AEdit: TEdit;
  const ADlgTitle, ADlgHeading, ADlgDefaultFolder: string;
  const ADlgShowNewFolderButton: Boolean);
var
  Folder: string;
begin
  Folder := StrTrim
    (BrowseForFolder(
      ADlgTitle, ADlgHeading, ADlgDefaultFolder, ADlgShowNewFolderButton
    )
  );
  if not StrIsEmpty(Folder) then
  begin
    AEdit.Text := Folder;
    AEdit.SelStart := Length(Folder);
  end;
end;

procedure TAddVaultDlg.UpdateButtons(const PageIdx: Integer);
resourcestring
  sInstall = '&Install';
begin
  inherited;
  case PageIdx of
    ConfirmPageIdx:
      btnNext.Caption := sInstall;
    FinishPageIdx:
      btnNext.ModalResult := mrOk;
  end;
end;

procedure TAddVaultDlg.ValidatePage(const PageIdx: Integer);

  procedure ValidateNamePage;
  resourcestring
    sEmptyNameError = 'You must provide a name';
    sDefNameError = '"%s" can''t be used - it is the default vault name';
    sDupNameError = 'A vault named "%s" already exists';
  var
    Name: string;
  begin
    Name := GetCtrlText(edName);
    if StrIsEmpty(Name) then
      raise EDataEntry.Create(sEmptyNameError, edName);
    if StrSameText(Name, TVaults.DefaultVaultName) then
      raise EDataEntry.CreateFmt(
        sDefNameError, [TVaults.DefaultVaultName], edName
      );
    if TVaults.Instance.ContainsName(Name) then
      raise EDataEntry.CreateFmt(sDupNameError, [Name], edName);
  end;

  procedure ValidateSourcePathPage;
  resourcestring
    sNoVault = 'The specified directory doesn''t contain valid vault data';
    sExistingVault = 'The specified directory is already in use by vault "%s"';
  var
    Vault: TVault;
    SourcePath: string;
  begin
    try
      SourcePath := GetCtrlText(edSourcePath);
      if not TDataFormatSniffer.DirContainsVault(SourcePath) then
        raise EDataEntry.Create(sNoVault, edSourcePath);
      for Vault in TVaults.Instance do
      begin
        if IsSameDirectory(SourcePath, Vault.Storage.Directory) then
          raise EDataEntry.CreateFmt(
            sExistingVault, [Vault.Name], edSourcePath
          );
      end;
    except
      on EDataEntry do
        raise;
      on E: Exception do
        raise EDataEntry.Create(E.Message, edSourcePath);
    end;
  end;

begin
  case PageIdx of
    NamePageIdx:
      ValidateNamePage;
    SourcePathPageIdx:
      ValidateSourcePathPage;
  end;
end;

end.

