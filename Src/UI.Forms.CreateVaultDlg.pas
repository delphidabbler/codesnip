{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2026, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a wizard dialogue box that lets the user create a new snippets
 * vault.
}


unit UI.Forms.CreateVaultDlg;

interface

uses
  // Delphi
  ActnList,
  Classes,
  ExtActns,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  // Project
  DB.DataFormats,
  DB.Vaults,
  FmWizardDlg,
  UBaseObjects,
  UIStringList;

type
  ///  <summary>Wizard dialogue box that enables the user to create a new vault.
  ///  </summary>
  TCreateVaultDlg = class(TWizardDlg, INoPublicConstruct)
    tsIntro: TTabSheet;
    tsPath: TTabSheet;
    tsName: TTabSheet;
    lblIntroDesc1: TLabel;
    lblIntroDesc4: TLabel;
    edPath: TEdit;
    btnPath: TButton;
    tsDataFormat: TTabSheet;
    edName: TEdit;
    lblName: TLabel;
    lblPath: TLabel;
    tsMetaData: TTabSheet;
    lblDataFormat: TLabel;
    cbDataFormat: TComboBox;
    lblDataFormatDesc: TLabel;
    lblMetaData: TLabel;
    tsConfirm: TTabSheet;
    tsFinish: TTabSheet;
    lblNoMetaData: TLabel;
    gbCopyright: TGroupBox;
    edCopyrightHolder: TEdit;
    edCopyrightURL: TEdit;
    btnCopyrightURL: TButton;
    gbLicense: TGroupBox;
    cbLicenseSPDX: TComboBox;
    edLicenseName: TEdit;
    edLicenseURL: TEdit;
    btnLicenseURL: TButton;
    lblPathDesc: TLabel;
    lblIntroDesc2: TLabel;
    alMain: TActionList;
    actLicenseURL: TBrowseURL;
    actCopyrightURL: TBrowseURL;
    actPath: TAction;
    lblConfirmDesc1: TLabel;
    lblConfirmDesc2: TLabel;
    lvConfirm: TListView;
    edCopyrightDate: TEdit;
    edContributors: TEdit;
    btnEditContributors: TButton;
    actEditContributors: TAction;
    gbAcknowledgements: TGroupBox;
    edAcknowledgements: TEdit;
    btnEditAcknowledgements: TButton;
    actEditAcknowledgements: TAction;
    lblIntroDesc3: TLabel;
    lblFinishMsg1: TLabel;
    lblFinishMsg2: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure cbLicenseSPDXChange(Sender: TObject);
    procedure actLicenseURLBeforeBrowse(Sender: TObject);
    procedure actCopyrightURLBeforeBrowse(Sender: TObject);
    procedure actCopyrightURLUpdate(Sender: TObject);
    procedure actLicenseURLUpdate(Sender: TObject);
    procedure actPathExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actEditAcknowledgementsExecute(Sender: TObject);
    procedure actEditContributorsExecute(Sender: TObject);
  strict private
    const
      // Wizard pages indices
      IntroPageIdx = 0;
      NamePageIdx = 1;
      PathPageIdx = 2;
      DataFormatPageIdx = 3;
      MetaDataPageIdx = 4;
      ConfirmPageIdx = 5;
      FinishPageIdx = 6;
    var
      ///  <summary>Saves the last suggested directory name.</summary>
      fSavedSuggestedDirName: string;

    ///  <summary>Returns the list separator character for the user's locale.
    ///  </summary>
    function ListSeparator: Char;

    ///  <summary>Gets the text from the given control, trimmed of leading and
    ///  trailing spaces.</summary>
    function GetTextFromCtrl(const ACtrl: TControl): string;

    ///  <summary>Gets the vault's name from its associated control, trimmed of
    ///  leading and trailing spaces.</summary>
    function GetNameFromCtrl: string;

    ///  <summary>Gets the path where the new, empty, vault will be stored from
    ///  its associated control, trimmed of leading and trailing spaces.
    ///  </summary>
    function GetPathFromCtrl: string;

    ///  <summary>Gets a license SPDX code from its associated control, trimmed
    ///  of leading and trailing spaces.</summary>
    function GetLicenseSPDXFromCtrl: string;

    ///  <summary>Gets a license name from its associated control, trimmed of
    ///  leading and trailing spaces.</summary>
    function GetLicenseNameFromCtrl: string;

    ///  <summary>Gets a license URL from its associated control, trimmed of
    ///  leading and trailing spaces.</summary>
    function GetLicenseURLFromCtrl: string;

    ///  <summary>Gets a copyright holder's name from the associated control,
    ///  trimmed of leading and trailing spaces.</summary>
    function GetCopyrightHolderFromCtrl: string;

    ///  <summary>Gets a copyright date from the associated control, trimmed of
    ///  leading and trailing spaces.</summary>
    function GetCopyrightDateFromCtrl: string;

    ///  <summary>Gets a copyright URL from its associated control, trimmed of
    ///  leading and trailing spaces.</summary>
    function GetCopyrightURLFromCtrl: string;

    ///  <summary>Creates and returns a list from list separator delimited text
    ///  entered into a control.</summary>
    function GetStringListFromCtrl(const ACtrl: TControl): IStringList;

    ///  <summary>Gets a list of contributors from its associated control, and
    ///  parses the content from a list separator delimited list.</summary>
    function GetContributorsFromCtrl: IStringList;

    ///  <summary>Gets a list of acknowledgements from its associated control,
    ///  and parses the content from a list separator delimited list.</summary>
    function GetAcknowledgementsFromCtrl: IStringList;

    ///  <summary>Renders a given string list as a single string and displays
    ///  that in the given control.</summary>
    procedure StoreStringListInCtrl(const ACtrl: TControl; List: IStringList);

    ///  <summary>Populates the list of available vault date formats.</summary>
    procedure PopulateDataFormatList;

    ///  <summary>Returns a suggested directory name for the vault being
    ///  created.</summary>
    function SuggestedDirName: string;

    ///  <summary>Method called just before the 'path' page is about to be.
    ///  displayed, when moving forward in the wizard. Updates the page's
    ///  controls.</summary>
    procedure BeforePathPage;

    ///  <summary>Method called just before the 'meta data' page is about to be
    ///  displayed, when moving forward in the wizard. Updates the page's
    ///  controls.</summary>
    procedure BeforeMetaDataPage;

    ///  <summary>Initialises the controls on the confirmation page. Called when
    ///  the page is displayed.</summary>
    procedure InitConfirmPage;

    ///  <summary>Enables or disables all the controls in <c>ACtrls</c>. The
    ///  enabled state is specified by <c>AState</c>.</summary>
    procedure SetEnabledState(const ACtrls: array of TControl;
      const AState: Boolean);

    ///  <summary>Checks if given text starts with the http or https protocol.
    ///  </summary>
    function ContainsHTTPURL(const AText: string): Boolean;

    ///  <summary>Displays the Windows standard browse for folder dialogue box
    ///  configured according to the given parameters. Returns the selected
    ///  folder or an empty string if the user cancels.</summary>
    function BrowseForFolder(const ATitle, AHeading, ADefaultFolder: string;
      const AShowNewFolderButton: Boolean): string;

    ///  <summary>Gets a folder name from the user using the Windows standard
    ///  browse for folder dialogue box. If the user accepts the entry the
    ///  selected folder is copied into the given edit control and the text is
    ///  selected.</summary>
    ///  <remarks>The final four parameters are used to configure the dialogue
    ///  box.</remarks>
    procedure SetPathEditTextFromFolderDlg(const AEdit: TEdit;
      const ADlgTitle, ADlgHeading, ADlgDefaultFolder: string;
      const ADlgShowNewFolderButton: Boolean);

    ///  <summary>Validates entries on the wizard page identified by the given
    ///  page index.</summary>
    ///  <exception>An <c>EDataEntry</c> exception is raised if validation
    ///  fails.</exception>
    procedure ValidatePage(const PageIdx: Integer);

    ///  <summary>Gets the data format to be used for the new vault from the
    ///  selected item in a combo box.</summary>
    function SelectedDataFormat: TDataFormatKind;

    ///  <summary>Creates and returns a new vault object with the properties
    ///  specified in the dialogue box.</summary>
    function CreateVault: TVault;

  strict protected

    ///  <summary>Positions controls and sets form size according to body panel
    ///   dimensions.</summary>
    procedure ArrangeForm; override;

    ///  <summary>Initialises controls.</summary>
    procedure ConfigForm; override;

    ///  <summary>Gets text of heading of a wizard page.</summary>
    ///  <param name="PageIdx"><c>Integer</c> [in] Index of wizard page for
    ///  which heading is required.</param>
    ///  <returns><c>string</c>. Required heading text.</returns>
    function HeadingText(const PageIdx: Integer): string; override;

    ///  <summary>Updates wizard buttons depending on page and state.</summary>
    ///  <param name="PageIdx"><c>Integer</c> [in] Index of wizard page for
    ///  which button state is to be updated.</param>
    procedure UpdateButtons(const PageIdx: Integer); override;

    ///  <summary>Updates a wizard page when it is displayed depending on page
    ///  and state.</summary>
    ///  <param name="PageIdx"><c>Integer</c> [in] Index of wizard page to be
    ///  updated.</param>
    procedure BeginPage(const PageIdx: Integer); override;

    ///  <summary>Called before moving forwards in the wizard.</summary>
    ///  <param name="PageIdx"><c>Integer</c> [in] Index of current wizard page.
    ///  </param>
    ///  <param name="CanMove"><c>Boolean</c> [in/out] Flag that
    ///  indicates whether the page change is allowed. <c>True</c> is always
    ///  passed in. Set to <c>False</c> to inhibit the page change.</param>
    procedure MoveForward(const PageIdx: Integer;
      var CanMove: Boolean); override;

  public

    ///  <summary>Displays the Create Vault wizard dialogue box to allow the
    ///  user to create a new, empty, vault.</summary>
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
  Character,
  IOUtils,
  // Project
  ClassHelpers.UControls,
  DB.IO.DataFormatSniffer,
  DB.Main,
  DB.MetaData,
  DB.MetaData.Licenses,
  UAppInfo,
  UBox,
  UBrowseForFolderDlg,
  UCtrlArranger,
  UExceptions,
  UI.Forms.ListEditorDlg,
  UIOUtils,
  UMessageBox,
  UStrUtils,
  UUtils,
  UVersionInfo;

{$R *.dfm}

{ TCreateVaultDlg }

procedure TCreateVaultDlg.actCopyrightURLBeforeBrowse(Sender: TObject);
begin
  (Sender as TBrowseURL).URL := GetCopyrightURLFromCtrl;
end;

procedure TCreateVaultDlg.actCopyrightURLUpdate(Sender: TObject);
begin
  (Sender as TBrowseURL).Enabled := ContainsHTTPURL(GetCopyrightURLFromCtrl);
end;

procedure TCreateVaultDlg.actEditAcknowledgementsExecute(Sender: TObject);
resourcestring
  sCaption = 'Edit Acknowledgements';
var
  List: IStringList;
begin
  List := GetAcknowledgementsFromCtrl;
  if TListEditorDlg.Execute(Self, sCaption, List) then
    StoreStringListInCtrl(edAcknowledgements, List);
end;

procedure TCreateVaultDlg.actEditContributorsExecute(Sender: TObject);
resourcestring
  sCaption = 'Edit Contributors';
var
  List: IStringList;
begin
  List := GetContributorsFromCtrl;
  if TListEditorDlg.Execute(Self, sCaption, List) then
    StoreStringListInCtrl(edContributors, List);
end;

procedure TCreateVaultDlg.actLicenseURLBeforeBrowse(Sender: TObject);
begin
  (Sender as TBrowseURL).URL := GetLicenseURLFromCtrl;
end;

procedure TCreateVaultDlg.actLicenseURLUpdate(Sender: TObject);
begin
  (Sender as TBrowseURL).Enabled := ContainsHTTPURL(GetLicenseURLFromCtrl);
end;

procedure TCreateVaultDlg.actPathExecute(Sender: TObject);
resourcestring
  sDlgCaption = 'New vault directory';
  sDlgHeading = 'Choose an empty directory or create a new one';
var
  DefFolder: string;
begin
  DefFolder := StrIf(
    GetPathFromCtrl <> '', GetPathFromCtrl, TAppInfo.UserVaultsDir
  );
  SetPathEditTextFromFolderDlg(
    edPath, sDlgCaption, sDlgHeading, DefFolder, True
  );
end;

procedure TCreateVaultDlg.ArrangeForm;
const
  TopMargin = 4;
  GroupBoxTopMargin = 24;
  GroupBoxBottomMargin = 16;
  GroupBoxHMargin = 8;
begin
  TCtrlArranger.SetLabelHeights(Self);

  // Intro tab

  TCtrlArranger.AlignLefts(
    [lblIntroDesc1, lblIntroDesc2, lblIntroDesc3{, lblIntroDesc4}],
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

  // Path tab

  lblPath.Top := TopMargin;
  lblPath.Width := tsPath.ClientWidth;
  TCtrlArranger.AlignLefts([lblPath, edPath, lblPathDesc], 0);
  TCtrlArranger.AlignRights([btnPath], tsPath.ClientWidth);
  edPath.Width := tsPath.ClientWidth - btnPath.Width - 8;
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblPath, 8),
    [edPath, btnPath]
  );
  TCtrlArranger.MoveBelow([edPath, btnPath], lblPathDesc, 12);

  // Data format tab

  TCtrlArranger.AlignLefts([lblDataFormat, lblDataFormatDesc, cbDataFormat], 0);
  lblDataFormat.Width := tsDataFormat.ClientWidth;
  lblDataFormatDesc.Width := tsDataFormat.ClientWidth;
  cbDataFormat.Width := tsDataFormat.ClientWidth;
  lblDataFormat.Top := TopMargin;
  TCtrlArranger.MoveBelow(lblDataFormat, cbDataFormat, 8);
  TCtrlArranger.MoveBelow(cbDataFormat, lblDataFormatDesc, 12);

  // Meta data tab

  // license group box
  TCtrlArranger.AlignLefts([cbLicenseSPDX, edLicenseURL], GroupBoxHMargin);
  TCtrlArranger.AlignVCentres(
    GroupBoxTopMargin, [cbLicenseSPDX, edLicenseName]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([cbLicenseSPDX, edLicenseName], 8),
    [edLicenseURL, btnLicenseURL]
  );
  edLicenseName.Width := gbLicense.ClientWidth - 2 * GroupBoxHMargin
    - cbLicenseSPDX.Width - 12;
  edLicenseURL.Width := gbLicense.ClientWidth - 2 * GroupBoxHMargin
    - btnLicenseURL.Width - 12;
  TCtrlArranger.AlignRights(
    [edLicenseName, btnLicenseURL], gbLicense.ClientWidth - 2 * GroupBoxHMargin
  );
  gbLicense.ClientHeight := TCtrlArranger.TotalControlHeight(gbLicense)
    + GroupBoxBottomMargin;

  // copyright group box
  TCtrlArranger.AlignLefts(
    [edCopyrightHolder, edCopyrightURL, edContributors], GroupBoxHMargin
  );
  TCtrlArranger.AlignRights(
    [edCopyrightDate, btnCopyrightURL, btnEditContributors],
    gbCopyright.ClientWidth - GroupBoxHMargin
  );
  TCtrlArranger.AlignVCentres(
    GroupBoxTopMargin, [edCopyrightHolder, edCopyrightDate]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([edCopyrightHolder, edCopyrightDate], 8),
    [edCopyrightURL, btnCopyrightURL]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([edCopyrightURL, btnCopyrightURL], 8),
    [edContributors, btnEditContributors]
  );
  edCopyrightHolder.Width := gbCopyright.ClientWidth - 2 * GroupBoxHMargin
    - edCopyrightDate.Width - 12;
  edCopyrightURL.Width := gbCopyright.ClientWidth - 2 * GroupBoxHMargin
    - btnCopyrightURL.Width - 12;
  edContributors.Width := gbCopyright.ClientWidth - 2 * GroupBoxHMargin
    - btnEditContributors.Width - 12;
  gbCopyright.ClientHeight := TCtrlArranger.TotalControlHeight(gbCopyright)
    + GroupBoxBottomMargin;

  // acknowledgements group box
  edAcknowledgements.Left := GroupBoxHMargin;
  TCtrlArranger.AlignVCentres(
    GroupBoxTopMargin, [edAcknowledgements, btnEditAcknowledgements]
  );
  TCtrlArranger.AlignRights(
    [btnEditAcknowledgements],
    gbAcknowledgements.ClientWidth - GroupBoxHMargin
  );
  edAcknowledgements.Width := gbAcknowledgements.ClientWidth
    - 2 * GroupBoxHMargin - btnEditAcknowledgements.Width - 12;
  gbAcknowledgements.ClientHeight :=
    TCtrlArranger.TotalControlHeight(gbAcknowledgements) + GroupBoxBottomMargin;

  // main
  TCtrlArranger.AlignLefts(
    [lblMetaData, lblNoMetaData, gbLicense, gbCopyright, gbAcknowledgements], 0
  );
  TCtrlArranger.AlignTops([lblMetaData, lblNoMetaData], TopMargin);
  TCtrlArranger.MoveBelow(lblMetaData, gbLicense, 8);
  TCtrlArranger.MoveBelow(gbLicense, gbCopyright, 8);
  TCtrlArranger.MoveBelow(gbCopyright, gbAcknowledgements, 8);

  // Confirm tab

  TCtrlArranger.AlignLefts([lblConfirmDesc1, lblConfirmDesc2, lvConfirm], 0);
  lblConfirmDesc1.Width := tsConfirm.ClientWidth;
  lblConfirmDesc2.Width := tsConfirm.ClientWidth;
  lvConfirm.Width := tsConfirm.ClientWidth;
  lblConfirmDesc1.Top := TopMargin;
  TCtrlArranger.MoveBelow(lblConfirmDesc1, lvConfirm, 6);
  TCtrlArranger.MoveBelow(lvConfirm, lblConfirmDesc2, 6);

  // Finish tab

  TCtrlArranger.AlignLefts([lblFinishMsg1, lblFinishMsg2], 0);
  lblFinishMsg1.Width := tsIntro.ClientWidth;
  lblFinishMsg2.Width := tsIntro.ClientWidth;
  lblFinishMsg1.Top := TopMargin;
  TCtrlArranger.MoveBelow(lblFinishMsg1, lblFinishMsg2, 12);

  // nothing

  // Set container dimensions

  pnlBody.ClientHeight := TCtrlArranger.MaxContainerHeight(
    [
      tsIntro, tsPath, tsDataFormat, tsName, tsMetaData, tsConfirm, tsFinish
    ]
  ) + pnlBody.ClientHeight - tsFinish.Height;
  pnlBody.ClientWidth := TCtrlArranger.MaxContainerWidth(
    [
      tsIntro, tsPath, tsDataFormat, tsName, tsMetaData, tsConfirm, tsFinish
    ]
  ) + pnlBody.ClientWidth - tsIntro.Width;

  // Confirm tab revisited: adjust heights

  lvConfirm.ClientHeight := tsConfirm.ClientHeight - lvConfirm.Top
    - GroupBoxBottomMargin;

  inherited;
end;

procedure TCreateVaultDlg.BeforeMetaDataPage;
var
  FormatCaps: TMetaDataCaps;
resourcestring
  sLicenseSupported = 'License';
  sLicenseNotSupported = 'License not supported';
  sCopyrightSupported = 'Copyright';
  sCopyrightNotSupported = 'Copyright not supported';
  sAcknowledgementsSupported = 'Acknowledgements';
  sAcknowledgementsNotSupported = 'Acknowledgements not supported';
begin
  FormatCaps := TDataFormatInfo.GetMetaDataCaps(SelectedDataFormat);

  lblMetaData.Visible := FormatCaps <> [];
  lblNoMetaData.Visible := FormatCaps = [];

  gbLicense.Visible := FormatCaps <> [];
  gbLicense.Caption := StrIf(
    TMetaDataCap.License in FormatCaps,
    sLicenseSupported,
    sLicenseNotSupported
  );

  gbCopyright.Visible := FormatCaps <> [];
  gbCopyright.Caption := StrIf(
    TMetaDataCap.Copyright in FormatCaps,
    sCopyrightSupported,
    sCopyrightNotSupported
  );

  gbAcknowledgements.Visible := FormatCaps <> [];
  gbAcknowledgements.Caption := StrIf(
    TMetaDataCap.Acknowledgements in FormatCaps,
    sAcknowledgementsSupported,
    sAcknowledgementsNotSupported
  );

  SetEnabledState(
    [gbLicense, cbLicenseSPDX, edLicenseName, edLicenseURL, btnLicenseURL],
    TMetaDataCap.License in FormatCaps
  );
  SetEnabledState(
    [
      gbCopyright, edCopyrightHolder, edCopyrightURL, edCopyrightDate,
      edContributors, btnEditContributors, btnCopyrightURL
    ],
    TMetaDataCap.Copyright in FormatCaps
  );
  SetEnabledState(
    [gbAcknowledgements, edAcknowledgements, btnEditAcknowledgements],
    TMetaDataCap.Acknowledgements in FormatCaps
  );
end;

procedure TCreateVaultDlg.BeforePathPage;
begin
  if fSavedSuggestedDirName = GetPathFromCtrl then
  begin
    fSavedSuggestedDirName := SuggestedDirName;
    edPath.Text := fSavedSuggestedDirName;
  end;
  edPath.SelStart := Length(GetPathFromCtrl);
end;

procedure TCreateVaultDlg.BeginPage(const PageIdx: Integer);
begin
  inherited;
  case PageIdx of
    ConfirmPageIdx: InitConfirmPage;
  end;
end;

function TCreateVaultDlg.BrowseForFolder(const ATitle, AHeading,
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

procedure TCreateVaultDlg.cbLicenseSPDXChange(Sender: TObject);
var
  SPDX: string;
begin
  SPDX := GetLicenseSPDXFromCtrl;
  if TSupportedLicenses.ContainsSPDX(SPDX) then
  begin
    edLicenseName.Text := TSupportedLicenses.NameFromSPDX(SPDX);
    edLicenseURL.Text := TSupportedLicenses.URLFromSPDX(SPDX);
    edLicenseName.SelStart := Length(GetLicenseNameFromCtrl);
    edLicenseURL.SelStart := Length(GetLicenseURLFromCtrl);
  end;
end;

procedure TCreateVaultDlg.ConfigForm;
begin
  inherited;
  TSupportedLicenses.SPDXList(cbLicenseSPDX.Items);
  PopulateDataFormatList;
end;

function TCreateVaultDlg.ContainsHTTPURL(const AText: string): Boolean;

  function IsValidHTTP(const AProtocol: string): Boolean;
  {TODO -cImprove: The IsValidHTTP function below is grossly over simplified.
        To improve read through this Gist of a ChatGPT conversion re URI
        validation:
        https://gist.github.com/delphidabbler/040f7092ef3b74ccdfa47ce0ffa625eb
  }
  begin
    Result := StrStartsStr(AProtocol, AText)
      and (Length(AText) >= Length(AProtocol) + 3);
  end;

const
  HTTP = 'http://';
  HTTPS = 'https://';
begin
  Result := IsValidHTTP(HTTP) or IsValidHTTP(HTTPS);
end;

function TCreateVaultDlg.CreateVault: TVault;
var
  MetaData: TMetaData;
begin
  Result := TVault.Create(
    TVaults.Instance.GetUniqueUID,
    GetNameFromCtrl,
    TDataStorageDetails.Create(SelectedDataFormat, GetPathFromCtrl)
  );
  MetaData := TMetaData.Create(
    TDataFormatInfo.GetMetaDataCaps(Result.Storage.Format)
  );
  if TMetaDataCap.License in MetaData.Capabilities then
    MetaData.LicenseInfo := TLicenseInfo.Create(
      GetLicenseNameFromCtrl, GetLicenseSPDXFromCtrl, GetLicenseURLFromCtrl, ''
    );
  {TODO -cVault: Need to add support for getting License Text}
  {TODO -cVault: Add support for requiring some meta data fields to be
          completed, per data structure specs.}
  if TMetaDataCap.Copyright in MetaData.Capabilities then
    MetaData.CopyrightInfo := TCopyrightInfo.Create(
      GetCopyrightDateFromCtrl,
      GetCopyrightHolderFromCtrl,
      GetCopyrightURLFromCtrl,
      GetContributorsFromCtrl
    );
  if TMetaDataCap.Acknowledgements in MetaData.Capabilities then
    MetaData.Acknowledgements := GetAcknowledgementsFromCtrl;
  Result.MetaData := MetaData;
end;

class function TCreateVaultDlg.Execute(const AOwner: TComponent;
  out AVault: TVault): Boolean;
var
  Dlg: TCreateVaultDlg;
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

procedure TCreateVaultDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fSavedSuggestedDirName := '';
end;

procedure TCreateVaultDlg.FormDestroy(Sender: TObject);
var
  Idx: Integer;
begin
  for Idx := Pred(cbDataFormat.Items.Count) downto 0 do
    cbDataFormat.Items.Objects[Idx].Free;
  inherited;
end;

function TCreateVaultDlg.GetAcknowledgementsFromCtrl: IStringList;
begin
  Result := GetStringListFromCtrl(edAcknowledgements);
end;

function TCreateVaultDlg.GetContributorsFromCtrl: IStringList;
begin
  Result := GetStringListFromCtrl(edContributors);
end;

function TCreateVaultDlg.GetCopyrightDateFromCtrl: string;
begin
  Result := GetTextFromCtrl(edCopyrightDate);
end;

function TCreateVaultDlg.GetCopyrightHolderFromCtrl: string;
begin
  Result := GetTextFromCtrl(edCopyrightHolder);
end;

function TCreateVaultDlg.GetCopyrightURLFromCtrl: string;
begin
  Result := GetTextFromCtrl(edCopyrightURL);
end;

function TCreateVaultDlg.GetLicenseNameFromCtrl: string;
begin
  Result := GetTextFromCtrl(edLicenseName);
end;

function TCreateVaultDlg.GetLicenseSPDXFromCtrl: string;
begin
  Result := GetTextFromCtrl(cbLicenseSPDX);
end;

function TCreateVaultDlg.GetLicenseURLFromCtrl: string;
begin
  Result := GetTextFromCtrl(edLicenseURL);
end;

function TCreateVaultDlg.GetNameFromCtrl: string;
begin
  Result := GetTextFromCtrl(edName);
end;

function TCreateVaultDlg.GetPathFromCtrl: string;
begin
  Result := GetTextFromCtrl(edPath);
end;

function TCreateVaultDlg.GetStringListFromCtrl(const ACtrl: TControl): IStringList;
begin
  Result := TIStringList.Create(
    GetTextFromCtrl(ACtrl), ListSeparator, False, True
  );
end;

function TCreateVaultDlg.GetTextFromCtrl(const ACtrl: TControl): string;
begin
  Result := StrTrim(ACtrl.GetCtrlText);
end;

function TCreateVaultDlg.HeadingText(const PageIdx: Integer): string;
resourcestring
  sIntro = 'Create snippet vault';
  sName = 'Vault name';
  sPath = 'Vault directory';
  sDataFormat = 'Vault data format';
  sMetaData = 'Vault metadata';
  sConfirm = 'Ready to create vault';
  sFinish = 'Vault create successfully';
const
  Headings: array[IntroPageIdx..FinishPageIdx] of string = (
    sIntro, sName, sPath, sDataFormat, sMetaData, sConfirm, sFinish
  );
begin
  Result := Headings[PageIdx];
end;

procedure TCreateVaultDlg.InitConfirmPage;

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
  sLicenseSPDX = 'License SPDX';
  sLicenseName = 'License name';
  sLicenseURL = 'License URL';
  sCopyrightHolder = 'Copyright holder';
  sCopyrightURL = 'Copyright URL';
  sCopyrightDate = 'Copyright Date';
  sContributors = 'Contributors';
  sAcknowledgements = 'Acknowledgements';
const
  ListSeparator = ';';
var
  DataFormat: TDataFormatKind;
  FormatCaps: TMetaDataCaps;
begin
  DataFormat := SelectedDataFormat;
  lvConfirm.Clear;
  // Name
  AddToLV(sName, GetNameFromCtrl);
  // Directory
  AddToLV(sDirectory, GetPathFromCtrl);
  // Data format
  AddToLV(sDataFormat, TDataFormatInfo.GetName(DataFormat));
  // Meta data
  FormatCaps := TDataFormatInfo.GetMetaDataCaps(DataFormat);
  if TMetaDataCap.License in FormatCaps then
  begin
    AddToLV(sLicenseSPDX, GetLicenseSPDXFromCtrl);
    AddToLV(sLicenseName, GetLicenseNameFromCtrl);
    AddToLV(sLicenseURL, GetLicenseURLFromCtrl);
  end;
  if TMetaDataCap.Copyright in FormatCaps then
  begin
    AddToLV(sCopyrightHolder, GetCopyrightHolderFromCtrl);
    AddToLV(sCopyrightURL, GetCopyrightURLFromCtrl);
    AddToLV(sCopyrightDate, GetCopyrightDateFromCtrl);
    AddToLV(
      sContributors, GetContributorsFromCtrl.GetText(ListSeparator, False)
    );
  end;
  if TMetaDataCap.Acknowledgements in FormatCaps then
  begin
    AddToLV(
      sAcknowledgements,
      GetAcknowledgementsFromCtrl.GetText(ListSeparator, False)
    );
  end;
end;

function TCreateVaultDlg.ListSeparator: Char;
begin
  Result := TFormatSettings.Create.ListSeparator;
end;

procedure TCreateVaultDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
begin
  CanMove := False;
  try
    ValidatePage(PageIdx);
    case PageIdx of
      NamePageIdx:
        BeforePathPage;
      DataFormatPageIdx:
        BeforeMetaDataPage;
    end;
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

procedure TCreateVaultDlg.PopulateDataFormatList;
var
  FormatKind: TDataFormatKind;
  Idx: Integer;
begin
  for FormatKind in TDataFormatInfo.GetSupportedFormats do
  begin
    cbDataFormat.Items.AddObject(
      TDataFormatInfo.GetName(FormatKind),
      TBox<TDataFormatKind>.Create(FormatKind)
    );
  end;
  cbDataFormat.ItemIndex := 0;
  for Idx := 0 to Pred(cbDataFormat.Items.Count) do
  begin
    if TBox<TDataFormatKind>(cbDataFormat.Items.Objects[Idx]).Value =
      TDataFormatInfo.DefaultFormat then
    begin
      cbDataFormat.ItemIndex := Idx;
      Break;
    end;
  end;
end;

function TCreateVaultDlg.SelectedDataFormat: TDataFormatKind;
begin
  Result := TBox<TDataFormatKind>(
    cbDataFormat.Items.Objects[cbDataFormat.ItemIndex]
  ).Value;
end;

procedure TCreateVaultDlg.SetEnabledState(const ACtrls: array of TControl;
  const AState: Boolean);
  {TODO -cRefactor: duplicates of this routine have appeared in other UI units.
        It would be better moved to its own unit, say UI.Utils or maybe add to
        UControlStateMgr.}
var
  Ctrl: TControl;
begin
  for Ctrl in ACtrls do
    Ctrl.Enabled := AState;
end;

procedure TCreateVaultDlg.SetPathEditTextFromFolderDlg(const AEdit: TEdit;
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

procedure TCreateVaultDlg.StoreStringListInCtrl(const ACtrl: TControl;
  List: IStringList);
begin
  Actrl.SetCtrlText(List.GetText(ListSeparator + ' ', False));
end;

function TCreateVaultDlg.SuggestedDirName: string;
var
  VaultName, ResultDir: string;
  Ch: Char;
begin
  VaultName := GetNameFromCtrl;
  if VaultName = '' then
    Exit('');
  ResultDir := '';
  for Ch in VaultName do
  begin
    if TCharacter.IsWhiteSpace(Ch) then
      Continue;
    if TPath.IsValidFileNameChar(Ch) and TPath.IsValidPathChar(Ch) then
      ResultDir := ResultDir + Ch
    else
      ResultDir := ResultDir + '_';
  end;
  if ResultDir = '' then
    Exit('');
  Result := TPath.Combine(TAppInfo.UserVaultsDir, ResultDir);
end;

procedure TCreateVaultDlg.UpdateButtons(const PageIdx: Integer);
resourcestring
  sInstall = 'Install';
begin
  inherited;
  case PageIdx of
    ConfirmPageIdx:
    begin
      btnNext.Caption := sInstall;
    end;
    FinishPageIdx:
    begin
      btnNext.ModalResult := mrOK;
    end;
  end;
end;

procedure TCreateVaultDlg.ValidatePage(const PageIdx: Integer);

  procedure ValidateNamePage;
  resourcestring
    sEmptyNameError = 'You must provide a name';
    sDefNameError = '"%s" can''t be used - it is the default vault name';
    sDupNameError = 'A vault named "%s" already exists';
  begin
    if GetNameFromCtrl = '' then
      raise EDataEntry.Create(sEmptyNameError, edName);
    if StrSameText(GetNameFromCtrl, TVaults.DefaultVaultName) then
      raise EDataEntry.CreateFmt(
        sDefNameError, [TVaults.DefaultVaultName], edName
      );
    if TVaults.Instance.ContainsName(GetNameFromCtrl) then
      raise EDataEntry.CreateFmt(sDupNameError, [GetNameFromCtrl], edName);
  end;

  procedure ValidatePathPage;
  var
    Vault: TVault;
    Path: string;
  resourcestring
    sNoDir = 'You must specify a directory.';
    sDirUnrooted = 'A full path to the directory must be specified.';
    sBadRoot = 'The root path does not exist.';
    sCantBeRoot = 'A directory below the root must be specified.';
    sDirNotEmpty = 'The directory is not empty.';
    sDirIsSubDir = 'The directory can''t be a sub-directory of an existing '
      + 'vault.';
    sParentHasVaultData = 'A parent directory already contains vault data.';
  begin
    try
      Path := GetPathFromCtrl;
      if Path = '' then
        raise EDataEntry.Create(sNoDir, edPath);
      if not TPath.IsPathRooted(Path) then
        raise EDataEntry.Create(sDirUnrooted, edPath);
      if not TDirectory.Exists(TPath.GetPathRoot(Path)) then
        raise EDataEntry.Create(sBadRoot, edPath);
      if IsSameDirectory(TPath.GetPathRoot(Path), Path) then
        raise EDataEntry.Create(sCantBeRoot, edPath);
      // Don't use TDirectory.IsRelativePath(APath) instead of following: causes
      // a crash when path root is a server name
      if not IsSubDirectory(TPath.GetPathRoot(Path), Path, False) then
        raise EDataEntry.Create(sDirUnrooted, edPath);
      if TDirectory.Exists(Path, False) and not TDirectory.IsEmpty(Path) then
        raise EDataEntry.Create(sDirNotEmpty, edPath);
      // Check if any installed vaults are parent directories
      for Vault in TVaults.Instance do
      begin
        if IsSubDirectory(Vault.Storage.Directory, Path, False) then
          raise EDataEntry.Create(sDirIsSubDir, edPath);
      end;
      // Check for any non-installed vaults in parent directories
      // NOTE: perform this check after the above check for installed vaults
      // otherwise this check will catch them & we want different error
      // messages for installed and non-installed vaults.
      if TDataFormatSniffer.PathContainsVault(Path) then
        raise EDataEntry.Create(sParentHasVaultData, edPath);
    except
      on EDataEntry do
        raise;
      on E: Exception do
        raise EDataEntry.Create(E.Message, edPath);
    end;
  end;

begin
  case PageIdx of
    NamePageIdx:
      ValidateNamePage;
    PathPageIdx:
      ValidatePathPage;
  end;
end;

end.

