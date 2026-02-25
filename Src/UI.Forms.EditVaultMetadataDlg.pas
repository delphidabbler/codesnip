{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2026, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that lets the user edit the metadata of any vault
 * that supports it.
}


unit UI.Forms.EditVaultMetadataDlg;

interface

uses
  // Delphi
  ActnList,
  Classes,
  ExtActns,
  StdCtrls,
  Controls,
  ExtCtrls,
  Generics.Defaults,
  Generics.Collections,
  // Project
  DB.MetaData,
  DB.Vaults,
  FmGenericOKDlg,
  UI.Adapters.VaultList,
  UIStringList;

type
  ///  <summary>Dialogue box that enables the user edit the meta data of any
  ///  vault that supports it.</summary>
  TEditVaultMetadataDlg = class(TGenericOKDlg)
    lblVaults: TLabel;
    cbVaults: TComboBox;
    lblNoMetaData: TLabel;
    gbLicense: TGroupBox;
    cbLicenseSPDX: TComboBox;
    edLicenseName: TEdit;
    edLicenseURL: TEdit;
    btnLicenseURL: TButton;
    gbCopyright: TGroupBox;
    edCopyrightHolder: TEdit;
    edCopyrightURL: TEdit;
    btnCopyrightURL: TButton;
    edCopyrightDate: TEdit;
    edContributors: TEdit;
    btnEditContributors: TButton;
    gbAcknowledgements: TGroupBox;
    edAcknowledgements: TEdit;
    btnEditAcknowledgements: TButton;
    alMain: TActionList;
    actLicenseURL: TBrowseURL;
    actCopyrightURL: TBrowseURL;
    actEditContributors: TAction;
    actEditAcknowledgements: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbVaultsChange(Sender: TObject);
    procedure cbLicenseSPDXChange(Sender: TObject);
    procedure actEditContributorsExecute(Sender: TObject);
    procedure actEditAcknowledgementsExecute(Sender: TObject);
    procedure actLicenseURLUpdate(Sender: TObject);
    procedure actCopyrightURLUpdate(Sender: TObject);
    procedure edCopyrightURLChange(Sender: TObject);
    procedure actLicenseURLBeforeBrowse(Sender: TObject);
    procedure actCopyrightURLBeforeBrowse(Sender: TObject);
    procedure edLicenseURLChange(Sender: TObject);
  strict private
    var
      ///  <summary>Adapter to make it easier to map between the drop down list
      ///  of vault names and the associated vault objects.</summary>
      fVaultList: TVaultListAdapter;
      ///  <summary>Map from a vault ID to the edited metadata.</summary>
      fMetaDataMap: TDictionary<TVaultID,TMetaData>;
      ///  <summary>Keeps track of the ID of the vault selected before the
      ///  current one.</summary>
      fPreviouslySelectedVaultID: TVaultID;
    ///  <summary>Creates and returns a list from list separator delimited text
    ///  entered into an edit control.</summary>
    function GetStringListFromCtrl(const ACtrl: TEdit): IStringList;
    ///  <summary>Enables or disables all the controls in <c>ACtrls</c>. The
    ///  enabled state is specified by <c>AState</c>.</summary>
    procedure SetEnabledState(const ACtrls: array of TControl;
      const AState: Boolean);
    ///  <summary>Checks if given text starts with the http or https protocol.
    ///  </summary>
    function ContainsHTTPURL(const AText: string): Boolean;
    ///  <summary>Renders a given list as a single string with each string
    ///  separated by a list separator character..</summary>
    function ListToSeparatedText(AList: IStringList): string;
    ///  <summary>Returns a reference to the vault that is currently selected
    ///  in the Vaults drop down list.</summary>
    function SelectedVault: TVault;
    ///  <summary>Performs the updates needed when the selected vault changes.
    ///  </summary>
    procedure DoVaultChange;
    ///  <summary>Updates the local copy of metadata associated with the given
    ///  vault ID according to the entries in the UI.</summary>
    procedure RecordMetaData(const AVaultID: TVaultID);
    ///  <summary>Updates the state of UI controls according to the capabilities
    ///  of the vault associated with the given vault ID.</summary>
    procedure UpdateControlState(const AVaultID: TVaultID);
    ///  <summary>Updates UI controls with the metadata associated with the
    ///  given vault ID.</summary>
    procedure PopulateMetadata(const AVaultID: TVaultID);
  strict protected
    ///  <summary>Positions controls and sets form size according to body panel
    ///  dimensions.</summary>
    procedure ArrangeForm; override;
    ///  <summary>Initialises controls.</summary>
    procedure ConfigForm; override;
  public
    ///  <summary>Displays the Edit Vault Metadata dialogue box that enables the
    ///  metadata associated with any and all vaults to be edited.</summary>
    ///  <param name="AOwner"><c>TComponent</c> [in] Reference to the component
    ///  that owns this dialogue box.</param>
    ///  <param name="AMetaData">
    ///  <c>TArray&lt;TPair&lt;TVaultID,TMetaData&gt;&gt;</c> [out] Set to an
    ///  array of key value pairs where key is a vault id and the value the
    ///  metadata associated with the identified vault. If the user cancels the
    ///  dialogue box then this array is empty.</param>
    ///  <returns><c>Boolean</c>. <c>True</c> if the user accepted the new vault
    ///  or <c>False</c> if the user cancelled.</returns>
    class function Execute(AOwner: TComponent;
      out AMetaData: TArray<TPair<TVaultID,TMetaData>>): Boolean;
  end;

implementation

{$R *.dfm}

uses
  // Delphi
  SysUtils,
  // Project
  ClassHelpers.UControls,
  DB.MetaData.Licenses,
  UCtrlArranger,
  UI.Forms.ListEditorDlg,
  UStrUtils;

procedure TEditVaultMetadataDlg.actCopyrightURLBeforeBrowse(Sender: TObject);
begin
  (Sender as TBrowseURL).URL := StrTrim(edCopyrightURL.Text);
end;

procedure TEditVaultMetadataDlg.actCopyrightURLUpdate(Sender: TObject);
begin
  (Sender as TBrowseURL).Enabled := ContainsHTTPURL(edCopyrightURL.Text);
end;

procedure TEditVaultMetadataDlg.actEditAcknowledgementsExecute(Sender: TObject);
resourcestring
  sCaption = 'Edit Acknowledgements';
var
  List: IStringList;
begin
  List := GetStringListFromCtrl(edAcknowledgements);
  if TListEditorDlg.Execute(Self, sCaption, List) then
    edAcknowledgements.Text := ListToSeparatedText(List);
end;

procedure TEditVaultMetadataDlg.actEditContributorsExecute(Sender: TObject);
resourcestring
  sCaption = 'Edit Contributors';
var
  List: IStringList;
begin
  List := GetStringListFromCtrl(edContributors);
  if TListEditorDlg.Execute(Self, sCaption, List) then
    edContributors.Text := ListToSeparatedText(List);
end;

procedure TEditVaultMetadataDlg.actLicenseURLBeforeBrowse(Sender: TObject);
begin
  (Sender as TBrowseURL).URL := StrTrim(edLicenseURL.Text);
end;

procedure TEditVaultMetadataDlg.actLicenseURLUpdate(Sender: TObject);
begin
  (Sender as TBrowseURL).Enabled := ContainsHTTPURL(edLicenseURL.Text);
end;

procedure TEditVaultMetadataDlg.ArrangeForm;
const
  TopMargin = 4;
  GroupBoxTopMargin = 24;
  GroupBoxBottomMargin = 16;
  GroupBoxHMargin = 8;
begin
  TCtrlArranger.SetLabelHeights(Self);

  TCtrlArranger.AlignVCentres(0, [lblVaults, cbVaults]);
  TCtrlArranger.MoveToRightOf(lblVaults, cbVaults, 8);

  TCtrlArranger.MoveBelow([lblVaults, cbVaults], lblNoMetaData, 60);

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
    [{lblNoMetaData, }gbLicense, gbCopyright, gbAcknowledgements], 0
  );
  TCtrlArranger.MoveBelow([lblVaults, cbVaults], gbLicense, 12);
  TCtrlArranger.MoveBelow(gbLicense, gbCopyright, 8);
  TCtrlArranger.MoveBelow(gbCopyright, gbAcknowledgements, 8);

  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;

  lblNoMetaData.Left := (pnlBody.ClientWidth - lblNoMetaData.Width) div 2;
  cbVaults.Width := pnlBody.ClientWidth - cbVaults.Left;

  inherited;
end;

procedure TEditVaultMetadataDlg.btnOKClick(Sender: TObject);
begin
  inherited;
  RecordMetaData(SelectedVault.UID.Clone);
end;

procedure TEditVaultMetadataDlg.cbLicenseSPDXChange(Sender: TObject);
var
  SPDX: string;
begin
  SPDX := StrTrim(cbLicenseSPDX.Text);
  if TSupportedLicenses.ContainsSPDX(SPDX) then
  begin
    edLicenseName.Text := TSupportedLicenses.NameFromSPDX(SPDX);
    edLicenseURL.Text := TSupportedLicenses.URLFromSPDX(SPDX);
    edLicenseName.SelStart := Length(edLicenseName.Text);
    edLicenseURL.SelStart := Length(edLicenseURL.Text);
  end;
end;

procedure TEditVaultMetadataDlg.cbVaultsChange(Sender: TObject);
begin
  DoVaultChange;
  fPreviouslySelectedVaultID := SelectedVault.UID.Clone;
end;

procedure TEditVaultMetadataDlg.ConfigForm;
begin
  inherited;
  lblNoMetaData.Visible := False;
  fVaultList.ToStrings(cbVaults.Items);
  cbVaults.ItemIndex := fVaultList.IndexOfUID(TVaultID.Default);
  DoVaultChange;
  TSupportedLicenses.SPDXList(cbLicenseSPDX.Items);
end;

function TEditVaultMetadataDlg.ContainsHTTPURL(const AText: string): Boolean;
  {TODO -cRefactor: This method is exactly the same as the method of the same
        name in UI.Forms.AddVaultDlg. The code needs to be extracted to a
        common unit}

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

procedure TEditVaultMetadataDlg.DoVaultChange;
var
  VaultID: TVaultID;
begin
  if not fPreviouslySelectedVaultID.IsNull then
    RecordMetaData(fPreviouslySelectedVaultID);
  VaultID := SelectedVault.UID.Clone;
  UpdateControlState(VaultID);
  PopulateMetadata(VaultID);
end;

procedure TEditVaultMetadataDlg.edCopyrightURLChange(Sender: TObject);
begin
  actCopyrightURL.URL := StrTrim(edCopyrightURL.Text);
end;

procedure TEditVaultMetadataDlg.edLicenseURLChange(Sender: TObject);
begin
  actLicenseURL.URL := StrTrim(edLicenseURL.Text);
end;

class function TEditVaultMetadataDlg.Execute(AOwner: TComponent;
  out AMetaData: TArray<TPair<TVaultID,TMetaData>>): Boolean;
var
  Dlg: TEditVaultMetadataDlg;
begin
  Dlg := TEditVaultMetadataDlg.Create(AOwner);
  try
    Result := Dlg.ShowModal = mrOK;
    if Result then
      AMetaData := Dlg.fMetaDataMap.ToArray
    else
      SetLength(AMetaData, 0);
  finally
    Dlg.Free;
  end;
end;

procedure TEditVaultMetadataDlg.FormCreate(Sender: TObject);
var
  Vault: TVault;
begin
  inherited;
  fVaultList := TVaultListAdapter.Create;
  // create a meta data store where Vy
  fMetaDataMap := TDictionary<TVaultID,TMetaData>.Create(
    TVaultID.TComparer.Create
  );
  for Vault in TVaults.Instance do
  begin
    fMetaDataMap.Add(Vault.UID.Clone, Vault.MetaData.Clone);
  end;
  // This is no previously selected vault, so we represent this by storing a
  // null vault id. This is safe because no actual vault can have a null ID.
  fPreviouslySelectedVaultID := TVaultID.CreateNull;
end;

procedure TEditVaultMetadataDlg.FormDestroy(Sender: TObject);
begin
  fMetaDataMap.Free;
  fVaultList.Free;
  inherited;
end;

function TEditVaultMetadataDlg.GetStringListFromCtrl(
  const ACtrl: TEdit): IStringList;
begin
  Result := TIStringList.Create(
    ACtrl.Text, TFormatSettings.Create.ListSeparator, False, True
  );
end;

function TEditVaultMetadataDlg.ListToSeparatedText(AList: IStringList): string;
begin
  Result := AList.GetText(TFormatSettings.Create.ListSeparator + ' ', False);
end;

procedure TEditVaultMetadataDlg.PopulateMetadata(const AVaultID: TVaultID);

  procedure ClearAllEditControlsOf(const Parents: array of TWinControl);
  var
    Idx: Integer;
    ParentCtrl: TWinControl;
    Control: TControl;
  begin
    for ParentCtrl in Parents do
    begin
      for Idx := 0 to Pred(ParentCtrl.ControlCount) do
      begin
        Control := ParentCtrl.Controls[Idx];
        if (Control is TEdit) or (Control is TComboBox) then
          Control.SetCtrlText('');
      end;
    end;
  end;

var
  FormatCaps: TMetaDataCaps;
  MetaData: TMetaData;
begin
  MetaData := fMetaDataMap[AVaultID];
  FormatCaps := MetaData.Capabilities;

  ClearAllEditControlsOf([gbLicense, gbCopyright, gbAcknowledgements]);

  if TMetaDataCap.License in FormatCaps then
  begin
    cbLicenseSPDX.Text := MetaData.LicenseInfo.SPDX;
    edLicenseName.Text := MetaData.LicenseInfo.Name;
    edLicenseURL.Text := MetaData.LicenseInfo.URL;
    {TODO -cVault: enable MetaData.LicenseInfo.Text to be edited}
  end;

  if TMetaDataCap.Copyright in FormatCaps then
  begin
    edCopyrightHolder.Text := MetaData.CopyrightInfo.Holder;
    edCopyrightURL.Text := MetaData.CopyrightInfo.HolderURL;
    edCopyrightDate.Text := MetaData.CopyrightInfo.Date;
    edContributors.Text := ListToSeparatedText(
      MetaData.CopyrightInfo.Contributors
    );
  end;

  if TMetaDataCap.Acknowledgements in FormatCaps then
    edAcknowledgements.Text := ListToSeparatedText(MetaData.Acknowledgements)
end;

procedure TEditVaultMetadataDlg.RecordMetaData(const AVaultID: TVaultID);
var
  MetaData: TMetaData;
begin
  MetaData := fMetaDataMap[AVaultID].Clone;

  if TMetaDataCap.License in MetaData.Capabilities then
    {TODO -cVault: Need to be able to get license text & record it.}
    MetaData.LicenseInfo := TLicenseInfo.Create(
      StrTrim(edLicenseName.Text),
      StrTrim(cbLicenseSPDX.Text),
      StrTrim(edLicenseURL.Text),
      ''
    )
  else
    MetaData.LicenseInfo := TLicenseInfo.CreateNull;

  if TMetaDataCap.Copyright in MetaData.Capabilities then
    MetaData.CopyrightInfo := TCopyrightInfo.Create(
      StrTrim(edCopyrightDate.Text),
      StrTrim(edCopyrightHolder.Text),
      StrTrim(edCopyrightURL.Text),
      GetStringListFromCtrl(edContributors)
    )
  else
    MetaData.CopyrightInfo := TCopyrightInfo.CreateNull;

  if TMetaDataCap.Acknowledgements in MetaData.Capabilities then
    MetaData.Acknowledgements := GetStringListFromCtrl(edAcknowledgements)
  else
    MetaData.Acknowledgements := nil;

  fMetaDataMap[AVaultID] := MetaData.Clone;
end;

function TEditVaultMetadataDlg.SelectedVault: TVault;
begin
  Result := fVaultList.Vault(cbVaults.ItemIndex);
end;

procedure TEditVaultMetadataDlg.SetEnabledState(const ACtrls: array of TControl;
  const AState: Boolean);
var
  Ctrl: TControl;
begin
  for Ctrl in ACtrls do
    Ctrl.Enabled := AState;
end;

procedure TEditVaultMetadataDlg.UpdateControlState(const AVaultID: TVaultID);
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
  FormatCaps := fMetaDataMap[AVaultID].Capabilities;

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

end.

