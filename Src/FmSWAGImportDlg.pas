{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a wizard dialogue box that lets the user select and import
 * packets from the DelphiDabbler implementation of the SWAG Pascal archive as
 * new CodeSnip snippets.
}


unit FmSWAGImportDlg;

interface

uses
  // Delphi
  SysUtils,
  ComCtrls,
  CheckLst,
  Controls,
  StdCtrls,
  Forms,
  ExtCtrls,
  ActnList,
  Classes,
  Generics.Collections,
  // Project
  DB.Vaults,
  FmWizardDlg,
  FrBrowserBase,
  FrFixedHTMLDlg,
  FrHTMLDlg,
  FrHTMLTpltDlg,
  UBaseObjects,
  UContainers,
  UCSSBuilder,
  SWAG.UCommon,
  SWAG.UImporter,
  SWAG.UReader,
  UI.Adapters.VaultList;


type
  ///  <summary>Class that implements a wizard dialogue box that lets the user
  ///  select and import packets from the DelphiDabbler implementation of the
  ///  SWAG Pascal archive as new snippets.</summary>
  TSWAGImportDlg = class(TWizardDlg, INoPublicConstruct)
    tsIntro: TTabSheet;
    tsCategories: TTabSheet;
    lblCategories: TLabel;
    lbCategories: TListBox;
    lblCategoriesDesc: TLabel;
    lblSelectPackets: TLabel;
    clbSelectPackets: TCheckListBox;
    tsUpdate: TTabSheet;
    lvImports: TListView;
    lblUpdateDesc1: TLabel;
    tsFinish: TTabSheet;
    frmOutro: THTMLTpltDlgFrame;
    btnDisplayCategory: TButton;
    alWizard: TActionList;
    actDisplayCategory: TAction;
    actDisplayPacket: TAction;
    btnDisplayPacket: TButton;
    tsFolder: TTabSheet;
    lblFolder: TLabel;
    edPath: TEdit;
    lblFolderPageInfo2: TLabel;
    btnBrowse: TButton;
    actBrowse: TAction;
    frmIntro: THTMLTpltDlgFrame;
    lblVersionNumber: TLabel;
    lblFolderPageInfo1: TLabel;
    lblUpdateDesc2: TLabel;
    lblVaults: TLabel;
    cbVaults: TComboBox;
    ///  <summary>Handles clicks on the check boxes next to packets in the
    ///  packet selection list box by selecting and deselecting packets for
    ///  inclusion in the import.</summary>
    procedure clbSelectPacketsClickCheck(Sender: TObject);
    ///  <summary>Handles double clicks on packets in the packet selection
    ///  list box by causing the selected packet to be previewed.</summary>
    procedure clbSelectPacketsDblClick(Sender: TObject);
    ///  <summary>Handles key down events on the packet selection list box by
    ///  causing the selected packet to be previewed when the user presses
    ///  Enter.</summary>
    procedure clbSelectPacketsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    ///  <summary>Handles double clicks on categories in the SWAG categories
    ///  list box by displaying the category's packets in the packet selection
    ///  list box.</summary>
    procedure lbCategoriesDblClick(Sender: TObject);
    ///  <summary>Handles key down events on categories in the SWAG categories
    ///  list box by displaying the category's packets in the packet selection
    ///  list box when the user presses enter.</summary>
    procedure lbCategoriesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    ///  <summary>Executes action to display a Browse for Folders dialogue box
    ///  and store the chosen folder in an edit box.</summary>
    procedure actBrowseExecute(Sender: TObject);
    ///  <summary>Executes action to display the packets in the selected
    ///  category.</summary>
    procedure actDisplayCategoryExecute(Sender: TObject);
    ///  <summary>Updates enabled state of display category action.</summary>
    procedure actDisplayCategoryUpdate(Sender: TObject);
    ///  <summary>Executes action to preview the selected packet.</summary>
    procedure actDisplayPacketExecute(Sender: TObject);
    ///  <summary>Updates enabled state of display packet category.</summary>
    procedure actDisplayPacketUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    const
      ///  <summary>Index of introductory page in wizard.</summary>
      cIntroPage = 0;
      ///  <summary>Index of SWAG database folder selection page in wizard.
      ///  </summary>
      cChooseFolderPage = 1;
      ///  <summary>Index of packet selection page in wizard.</summary>
      cSelectionPage = 2;
      ///  <summary>Index of import page in wizard.</summary>
      cUpdatePage = 3;
      ///  <summary>Index of finish page in wizard.</summary>
      cFinishPage = 4;
    var
      fPrevSWAGDir: string;
      ///  <summary>Object that provides cached access to the SWAG database.
      ///  </summary>
      fSWAGReader: TSWAGReader;
      ///  <summary>List of all categories in SWAG database, sorted by title.
      ///  </summary>
      fSortedCategories: TSortedList<TSWAGCategory>;
      ///  <summary>List of packets in the current category, sorted by title.
      ///  </summary>
      fCurrentCatPackets: TSortedList<TSWAGPacket>;
      ///  <summary>List of packets selected for import, sorted by ID.
      ///  </summary>
      fSelectedPackets: TSortedList<TSWAGPacket>;
      ///  <summary>Object that imports selected SWAG packets into a specified
      ///  vault.</summary>
      fImporter: TSWAGImporter;
      ///  <summary>ID of currently selected category.</summary>
      ///  <remarks>Set to zero if no category is selected.</remarks>
      fCurrentCatID: Cardinal;
      ///  <summary>Object that populates <c>cbVaults</c> with an alphabetical
      ///  list of vault names and manages interaction with it.</summary>
      fVaultList: TVaultListAdapter;
    ///  <summary>Retrieves import directory name from edit control where it is
    ///  entered.</summary>
    function GetDirNameFromEditCtrl: string;
    ///  <summary>Retrieves vault specified by user that applies to imported
    ///  snippets.</summary>
    ///  <returns><c>TVaultID</c>. The required vault ID.</returns>
    function SelectedVaultID: TVaultID;
    ///  <summary>Validates entries on the wizard page identified by the given
    ///  page index.</summary>
    procedure ValidatePage(const PageIdx: Integer);
    ///  <summary>Handles HTML template frame's OnBuildCSS event. Adds
    ///  additional CSS required by HTML in this form.</summary>
    ///  <param name="Sender">TObject [in] Reference to object triggering event.
    ///  </param>
    ///  <param name="CSSBuilder">TCSSBuilder [in] Object used to construct the
    ///  CSS.</param>
    procedure BuildCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
    ///  <summary>Displays packets selected for import in list view on Update
    ///  page.</summary>
    procedure PopulateImportsLV;
    ///  <summary>Initialises SWAG database XML file for reading before
    ///  Selection page.</summary>
    ///  <remarks>May display a wait dialogue box while initialising the
    ///  database.</remarks>
    procedure BeforeSelectionPage;
    ///  <summary>Initialises Selection page by populating its list of SWAG
    ///  categories, if necessary.</summary>
    ///  <remarks>May display a wait dialogue box while loading the categories.
    ///  </remarks>
    procedure InitSelectionPage;
    ///  <summary>Initialises Update page by retrieving all the selected
    ///  packets, preparing them for import and displaying them in the page's
    ///  list view.</summary>
    ///  <remarks>May display a wait dialogue box while loading the packets.
    ///  </remarks>
    procedure InitUpdatePage;
    ///  <summary>Gets the packets contained in any selected category and
    ///  displays them in the packet selection list box on the Selection page.
    ///  </summary>
    ///  <remarks>May display a wait dialogue box while the packets are being
    ///  retrieved.</remarks>
    procedure DisplayPacketsForCategory;
    ///  <summary>Creates and displays a preview of the currently selected
    ///  packet in the Selection page's packet selection list box.</summary>
    ///  <remarks>May display a wait dialogue box while the selected packet is
    ///  retrieved.</remarks>
    procedure PreviewSelectedPacket;
    ///  <summary>Gets the complete information for each packet selected for
    ///  import and stores in the given list.</summary>
    procedure GetImportPackets(const PacketList: TList<TSWAGPacket>);
    ///  <summary>Performs the import of the selected packets into a specified
    ///  vault.</summary>
    ///  <remarks>Displays a wait dialogue box while the import is proceeding.
    ///  </remarks>
    procedure UpdateDatabase;
    ///  <summary>Executes a given callback while displaying a wait dialogue
    ///  box.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the wait
    ///  dialogue box over which the dialogue is aligned.</param>
    ///  <param name="CallProc">TProc [in] Callback closure to be executed while
    ///  wait dialogue box is displayed.</param>
    ///  <param name="WaitMsg">string [in] Message to be displayed in wait
    ///  dialogue box.</param>
    procedure WaitWrapper(AOwner: TComponent; const CallProc: TProc;
      const WaitMsg: string);
  strict protected
    ///  <summary>Constructs and intialises a wizard instance.</summary>
    constructor InternalCreate(AOwner: TComponent); override;
    ///  <summary>Aligns and arranges controls in each tab sheet and sizes
    ///  dialog box to accomodate controls.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure ArrangeForm; override;
    ///  <summary>Initialises wizard pages that display HTML content.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure ConfigForm; override;
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
    ///  <summary>Destroys wizard dialogue box instance.</summary>
    destructor Destroy; override;
    ///  <summary>Displays SWAG import wizard aligned over given owner control
    ///  and returns True if the user performs an import or False if the user
    ///  cancels.</summary>
    class function Execute(const AOwner: TComponent): Boolean;
  end;


implementation


uses
  // Delphi
  Generics.Defaults,
  Windows,
  IOUtils,
  // Project
  FmPreviewDlg,
  FmWaitDlg,
  SWAG.UVersion,
  UBrowseForFolderDlg,
  UColours,
  UConsts,
  UCSSUtils,
  UCtrlArranger,
  UEncodings,
  UExceptions,
  UHTMLTemplate,
  UMessageBox,
  UStrUtils,
  UUrl,
  UVersionInfo,
  UWaitForThreadUI;

{$R *.dfm}


{ TSWAGImportDlg }

procedure TSWAGImportDlg.actBrowseExecute(Sender: TObject);
var
  Dlg: TBrowseForFolderDlg; // browse for folder standard dialogue box
resourcestring
  sDlgTitle = 'Choose SWAG database download directory';
  sDlgHeading = 'Choose an empty directory or create a new one';
begin
  Dlg := TBrowseForFolderDlg.Create(nil);
  try
    Dlg.Title := sDlgTitle;
    Dlg.Headline := sDlgHeading;
    Dlg.MakeFolderBtnVisible := True;
    if Dlg.Execute then
      edPath.Text := Dlg.FolderName;
  finally
    Dlg.Free;
  end;
end;

procedure TSWAGImportDlg.actDisplayCategoryExecute(Sender: TObject);
begin
  DisplayPacketsForCategory;
end;

procedure TSWAGImportDlg.actDisplayCategoryUpdate(Sender: TObject);
begin
  actDisplayCategory.Enabled := lbCategories.ItemIndex >= 0;
end;

procedure TSWAGImportDlg.actDisplayPacketExecute(Sender: TObject);
begin
  PreviewSelectedPacket;
end;

procedure TSWAGImportDlg.actDisplayPacketUpdate(Sender: TObject);
begin
  actDisplayPacket.Enabled := clbSelectPackets.ItemIndex >= 0;
end;

procedure TSWAGImportDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);
  // Arrange controls on tab sheets

  // tsIntro
  frmIntro.Height := frmIntro.DocHeight;

  // tsFolder
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblFolder, 6), [edPath, btnBrowse]
  );
  TCtrlArranger.AlignLefts(
    [lblFolder, edPath, lblFolderPageInfo1, lblFolderPageInfo2], 0
  );
  TCtrlArranger.MoveToRightOf(edPath, btnBrowse, 8);
  lblFolderPageInfo1.Top := TCtrlArranger.BottomOf([edPath, btnBrowse], 12);
  lblFolderPageInfo2.Top := TCtrlArranger.BottomOf(lblFolderPageInfo1, 8);

  // tsCategories
  lblCategoriesDesc.Width := tsCategories.ClientWidth;
  lblCategoriesDesc.Top := 3;
  TCtrlArranger.AlignLefts(
    [lblCategoriesDesc, lblCategories, lbCategories, lblVersionNumber], 0
  );
  TCtrlArranger.AlignTops(
    [lblCategories, lblSelectPackets],
    TCtrlArranger.BottomOf(lblCategoriesDesc, 12)
  );
  TCtrlArranger.AlignTops(
    [lbCategories, clbSelectPackets],
    TCtrlArranger.BottomOf([lblCategories, lblSelectPackets], 6)
  );
  TCtrlArranger.AlignTops(
    [btnDisplayCategory, btnDisplayPacket],
    TCtrlArranger.BottomOf([lbCategories, clbSelectPackets], 8)
  );
  TCtrlArranger.MoveBelow(
    [btnDisplayCategory, btnDisplayPacket],
    lblVersionNumber,
    8
  );
  TCtrlArranger.AlignHCentresTo([lbCategories], [btnDisplayCategory]);
  TCtrlArranger.AlignHCentresTo([clbSelectPackets], [btnDisplayPacket]);

  // tsUpdate
  TCtrlArranger.AlignLefts(
    [lblUpdateDesc1, lblUpdateDesc2, lblVaults, lvImports], 0
  );
  lblUpdateDesc1.Width := tsUpdate.ClientWidth;
  lblUpdateDesc2.Width := tsUpdate.ClientWidth;
  lvImports.Width := tsUpdate.ClientWidth;
  lblUpdateDesc1.Top := 3;
  TCtrlArranger.MoveBelow(lblUpdateDesc1, lblUpdateDesc2, 4);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblUpdateDesc2, 8), [lblVaults, cbVaults]
  );
  TCtrlArranger.MoveToRightOf(lblVaults, cbVaults, 4);
  TCtrlArranger.MoveBelow([lblVaults, cbVaults], lvImports, 12);

  // tsFinish
  frmOutro.Height := frmOutro.DocHeight;

  // set required height
  pnlBody.ClientHeight := TCtrlArranger.MaxContainerHeight(
    [tsIntro, tsFolder, tsCategories, tsUpdate, tsFinish]
  ) + pnlBody.ClientHeight - tsFinish.Height;
  pnlBody.ClientWidth := TCtrlArranger.MaxContainerWidth(
    [tsIntro, tsFolder, tsCategories, tsUpdate, tsFinish]
  ) + pnlBody.ClientWidth - tsIntro.Width;

  // re-size controls to fit height
  lvImports.Height := tsUpdate.ClientHeight - lvImports.Top;

  inherited;
end;

procedure TSWAGImportDlg.BeforeSelectionPage;
resourcestring
  sDefaultWaitMsg = 'Accessing database...';
  sWaitMsg = 'Initialising SWAG database...';
var
  WaitProc: TProc;
begin
  if StrSameText(fPrevSWAGDir, GetDirNameFromEditCtrl) then
    Exit;

  lbCategories.Clear;
  clbSelectPackets.Clear;

  WaitProc := procedure
  begin
    Application.ProcessMessages;
    fPrevSWAGDir := GetDirNameFromEditCtrl;
    FreeAndNil(fSWAGReader);
    fSWAGReader := TSWAGReader.Create(
      GetDirNameFromEditCtrl,
      procedure (CallProc: TProc)
      begin
        WaitWrapper(Self, CallProc, sDefaultWaitMsg);
      end
    );
  end;

  TWaitForThreadUI.Run( // this blocks until thread completes
    WaitProc,
    False,
    TWaitDlg.CreateAutoFree(Self, sWaitMsg),
    0,
    500
  );
end;

procedure TSWAGImportDlg.BeginPage(const PageIdx: Integer);
begin
  case PageIdx of
    cSelectionPage: InitSelectionPage;
    cUpdatePage: InitUpdatePage;
  end;
end;

procedure TSWAGImportDlg.BuildCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
begin
  inherited;
  // Set body text spacing
  CSSBuilder.Selectors['body']
    .AddProperty(TCSS.LineHeightProp(120));
  // Create .framed border style
  CSSBuilder.AddSelector('.framed')
    .AddProperty(TCSS.BorderProp(cssAll, 1, cbsSolid, clBorder))
    .AddProperty(TCSS.PaddingProp(0, 4, 4, 4))
    .AddProperty(TCSS.MarginProp(cssTop, 4));
end;

procedure TSWAGImportDlg.clbSelectPacketsClickCheck(Sender: TObject);
var
  SelIdx: Integer;
  DelIdx: Integer;
begin
  SelIdx := clbSelectPackets.ItemIndex;
  if SelIdx = -1 then
    Exit;
  if clbSelectPackets.Checked[SelIdx] then
  begin
    if not fSelectedPackets.Contains(fCurrentCatPackets[SelIdx]) then
      fSelectedPackets.Add(fCurrentCatPackets[SelIdx]);
  end
  else
  begin
    DelIdx := fSelectedPackets.IndexOf(fCurrentCatPackets[SelIdx]);
    if DelIdx >= 0 then
      fSelectedPackets.Delete(DelIdx);
  end;
end;

procedure TSWAGImportDlg.clbSelectPacketsDblClick(Sender: TObject);
begin
  PreviewSelectedPacket;
end;

procedure TSWAGImportDlg.clbSelectPacketsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    PreviewSelectedPacket;
end;

procedure TSWAGImportDlg.ConfigForm;
resourcestring
  sVersions = 'v%0:s to v%1:s';
begin
  inherited;
  pcWizard.ActivePage := tsFinish;
  frmOutro.OnBuildCSS := BuildCSS;
  frmOutro.Initialise(
    'dlg-swag-import-outro-tplt.html',
    procedure (Tplt: THTMLTemplate)
    begin
      Tplt.ResolvePlaceholderText(
        'SWAGCategory',
        TSWAGImporter.SWAGCategoryDesc
      );
    end
  );
  pcWizard.ActivePage := tsIntro;
  frmIntro.OnBuildCSS := BuildCSS;
  frmIntro.Initialise(
    'dlg-swag-import-intro-tplt.html',
    procedure (Tplt: THTMLTemplate)
    begin
      Tplt.ResolvePlaceholderText(
        'SWAGReleaseURL',
        TURL.SWAGReleases
      );
      Tplt.ResolvePlaceholderText(
        'SupportedSWAGVersions',
        Format(
          sVersions,
          [
            string(TSWAGVersion.LowestSupportedVersion),
            string(TSWAGVersion.LowestUnSupportedVersion)
          ]
        )
      );
    end
  );

  // Set up vaults list
  fVaultList.ToStrings(cbVaults.Items);
  Assert(cbVaults.Items.Count > 0,
    ClassName + '.ConfigForm: no vaults');
  Assert(TVaults.Instance.ContainsID(TVaultID.Default),
    ClassName + '.ConfigForm: default vault not found');
  cbVaults.ItemIndex := fVaultList.IndexOfUID(TVaultID.Default);
  Assert(cbVaults.ItemIndex >= 0,
    ClassName + '.ConfigForm: default vault not in cbVaults');
end;

destructor TSWAGImportDlg.Destroy;
begin
  fSWAGReader.Free;
  fImporter.Free;
  fSelectedPackets.Free;
  fCurrentCatPackets.Free;
  fSortedCategories.Free;
  inherited;
end;

procedure TSWAGImportDlg.DisplayPacketsForCategory;
resourcestring
  sPacketListCaption = '&Select packets from "%s"';
var
  CatIdx: Integer;
  Idx: Integer;
  N: Integer;
  Packets: TList<TSWAGPacket>;
begin
  CatIdx := lbCategories.ItemIndex;
  if CatIdx = -1 then
  begin
    fCurrentCatID := 0;
    Exit;
  end;
  if fCurrentCatID = fSortedCategories[CatIdx].ID then
    // nothing to do if current category selected again
    Exit;
  fCurrentCatID := fSortedCategories[CatIdx].ID;
  lblSelectPackets.Caption := Format(
    sPacketListCaption,
    // double up ampersands to avoid being treated as accelerator characters
    [StrReplace(fSortedCategories[CatIdx].Title, '&', '&&')]
  );
  Packets := TList<TSWAGPacket>.Create;
  try
    fSWAGReader.GetPartialPackets(fCurrentCatID, Packets);
    clbSelectPackets.Items.BeginUpdate;
    try
      fCurrentCatPackets.Clear;
      clbSelectPackets.Clear;
      // We set fCurrentCatPackets first because it is a sorted list which
      // means indices of new items added are not sequential, and we must have
      // displayed title at same index in clbSelectPackets as its packet is in
      // fCurrentCatPackets.
      fCurrentCatPackets.AddRange(Packets);
      for Idx := 0 to Pred(fCurrentCatPackets.Count) do
      begin
        N := clbSelectPackets.Items.Add(fCurrentCatPackets[Idx].Title);
        Assert(Idx = N, 'Idx <> N');
        clbSelectPackets.Checked[Idx] := fSelectedPackets.Contains(
          fCurrentCatPackets[Idx]
        );
      end;
    finally
      clbSelectPackets.Items.EndUpdate;
    end;
  finally
    Packets.Free;
  end;
end;

class function TSWAGImportDlg.Execute(const AOwner: TComponent): Boolean;
var
  Dlg: TSWAGImportDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Result := Dlg.ShowModal = mrOK;
  finally
    Dlg.Free;
  end;
end;

procedure TSWAGImportDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fVaultList := TVaultListAdapter.Create;
end;

procedure TSWAGImportDlg.FormDestroy(Sender: TObject);
begin
  fVaultList.Free;
  inherited;
end;

function TSWAGImportDlg.GetDirNameFromEditCtrl: string;
begin
  Result := StrTrim(edPath.Text);
end;

procedure TSWAGImportDlg.GetImportPackets(const PacketList: TList<TSWAGPacket>);
var
  PacketIDs: TList<Cardinal>;
  PartialPacket: TSWAGPacket;
resourcestring
  sWaitMsg = 'Retrieving packets...';
begin
  PacketIDs := TList<Cardinal>.Create;
  try
    for PartialPacket in fSelectedPackets do
      PacketIDs.Add(PartialPacket.ID);
    fSWAGReader.GetCompletePackets(
      PacketIDs,
      PacketList,
      procedure
      begin
        Application.ProcessMessages;
      end,
      procedure (CallProc: TProc)
      begin
        WaitWrapper(Self, CallProc, sWaitMsg);
      end
    );
  finally
    PacketIDs.Free;
  end;
end;

function TSWAGImportDlg.HeadingText(const PageIdx: Integer): string;
resourcestring
  sIntroPageHeading = 'Import packets from SWAG as new snippets';
  sFolderPage = 'Select SWAG database download folder';
  sSelectionPageHeading = 'Select required packets';
  sUpdatePage = 'Ready to import';
  sFinishPage = 'Import complete';
begin
  case PageIdx of
    cIntroPage:     Result := sIntroPageHeading;
    cChooseFolderPage:    Result := sFolderPage;
    cSelectionPage: Result := sSelectionPageHeading;
    cUpdatePage:    Result := sUpdatePage;
    cFinishPage:    Result := sFinishPage;
  end;
end;

procedure TSWAGImportDlg.InitSelectionPage;
var
  Cats: TList<TSWAGCategory>;
  Idx: Integer;
resourcestring
  sLblVersionNumberCaption = 'SWAG version %s';
begin
  lblVersionNumber.Caption := Format(
    sLblVersionNumberCaption,
    [string(TSWAGVersion.GetVersion(GetDirNameFromEditCtrl))]
  );

  Application.ProcessMessages;

  if (lbCategories.Count > 0) then
    Exit;

  Cats := TList<TSWAGCategory>.Create;
  try
    fSWAGReader.GetCategories(Cats);
    lbCategories.Items.BeginUpdate;
    try
      // We set fSortedCategories first because it is a sorted list which means
      // indices of new items added are not sequential, and we must have
      // displayed title at same index in lbCategories as its category is in
      // fSortedCategories.
      fSortedCategories.AddRange(Cats);
      for Idx := 0 to Pred(fSortedCategories.Count) do
        lbCategories.Items.Add(fSortedCategories[Idx].Title);
      lbCategories.ItemIndex := -1;
    finally
      lbCategories.Items.EndUpdate;
    end;
  finally
    Cats.Free;
  end;
end;

procedure TSWAGImportDlg.InitUpdatePage;
var
  FullPackets: TList<TSWAGPacket>;
  Packet: TSWAGPacket;
resourcestring
  sWaitMsg = 'Retrieving packets...';
begin
  Application.ProcessMessages;
  FullPackets := TList<TSWAGPacket>.Create;
  try
    GetImportPackets(FullPackets);
    fImporter.Reset;
    for Packet in FullPackets do
      fImporter.IncludePacket(Packet);
  finally
    FullPackets.Free;
  end;
  PopulateImportsLV;
end;

constructor TSWAGImportDlg.InternalCreate(AOwner: TComponent);
resourcestring
  sDefaultWaitMsg = 'Accessing SWAG...';
begin
  inherited;
  fSortedCategories := TSortedList<TSWAGCategory>.Create(
    TDelegatedComparer<TSWAGCategory>.Create(
      function (const Left, Right: TSWAGCategory): Integer
      begin
        Result := StrCompareStr(Left.Title, Right.Title);
      end
    )
  );
  fSortedCategories.PermitDuplicates := True;

  fCurrentCatPackets := TSortedList<TSWAGPacket>.Create(
    TDelegatedComparer<TSWAGPacket>.Create(
      function (const Left, Right: TSWAGPacket): Integer
      begin
        Result := StrCompareStr(Left.Title, Right.Title);
      end
    )
  );
  fCurrentCatPackets.PermitDuplicates := True;

  fSelectedPackets := TSortedList<TSWAGPacket>.Create(
    TDelegatedComparer<TSWAGPacket>.Create(
      function (const Left, Right: TSWAGPacket): Integer
      begin
        Result := Left.ID - Right.ID;
      end
    )
  );
  fSelectedPackets.PermitDuplicates := False;

  fImporter := TSWAGImporter.Create;

end;

procedure TSWAGImportDlg.lbCategoriesDblClick(Sender: TObject);
begin
  DisplayPacketsForCategory;
end;

procedure TSWAGImportDlg.lbCategoriesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    DisplayPacketsForCategory;
end;

procedure TSWAGImportDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
begin
  CanMove := False;
  try
    ValidatePage(PageIdx);
    case PageIdx of
      cChooseFolderPage: BeforeSelectionPage;
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
  end;
end;

procedure TSWAGImportDlg.PopulateImportsLV;
var
  Packet: TSWAGPacket;
  LI: TListItem;
begin
  lvImports.Items.BeginUpdate;
  try
    lvImports.Clear;
    for Packet in fSelectedPackets do
    begin
      LI := lvImports.Items.Add;
      LI.Caption := Packet.Title;
    end;
  finally
    lvImports.Items.EndUpdate;
  end;
end;

procedure TSWAGImportDlg.PreviewSelectedPacket;
var
  PartialPacket: TSWAGPacket;
  FullPacket: TSWAGPacket;
  SelIdx: Integer;
  Content: string;
resourcestring
  sWaitMsg = 'Retrieving packet...';
  sContentTplt = 'ID: %0:d' + EOL +
    'Category ID: %1:d' + EOL +
    'File Name: "%2:s"' + EOL +
    'Title: "%3:s"' + EOL +
    'Author: "%4:s"' + EOL2 +
    'Source Code:' + EOL + '%5:s' + EOL + '%6:s';
begin
  SelIdx := clbSelectPackets.ItemIndex;
  if SelIdx = -1 then
    Exit;
  PartialPacket := fCurrentCatPackets[SelIdx];
  FullPacket := fSWAGReader.GetCompletePacket(
    PartialPacket.ID,
    procedure (CallProc: TProc)
    begin
      WaitWrapper(Self, CallProc, sWaitMsg);
    end
  );
  Content := Format(
    sContentTplt,
    [
      FullPacket.ID,
      FullPacket.Category,
      FullPacket.FileName,
      FullPacket.Title,
      FullPacket.Author,
      StrOfChar('-', 80),
      StrWindowsLineBreaks(FullPacket.SourceCode)
    ]
  );
  TPreviewDlg.Execute(
    Self,
    TEncodedData.Create(Content, etUTF8),
    dtPlainText
  );
end;

function TSWAGImportDlg.SelectedVaultID: TVaultID;
begin
  Assert(cbVaults.ItemIndex >= 0,
    ClassName + '.SelectedVaultID: no vault selected');
  Result := fVaultList.Vault(cbVaults.ItemIndex).UID;
end;

procedure TSWAGImportDlg.UpdateButtons(const PageIdx: Integer);
resourcestring
  // button caption for update page
  sUpdateCaption = 'Import';
begin
  inherited;
  case PageIdx of
    cUpdatePage: btnNext.Caption := sUpdateCaption;
    cFinishPage: btnBack.Enabled := False;
  end;
end;

procedure TSWAGImportDlg.UpdateDatabase;

  procedure SetBtnVisibility(const Show: Boolean);
  begin
    btnNext.Visible := Show;
    btnBack.Visible := Show;
    btnCancel.Visible := Show;
    btnHelp.Visible := Show;
    Application.ProcessMessages;
  end;

resourcestring
  sWaitMsg = 'Importing packets into database as new snippets...';
begin
  SetBtnVisibility(False);
  try
    WaitWrapper(
      Self,
      procedure
      begin
        fImporter.Import(
          SelectedVaultID,
          procedure (const Packet: TSWAGPacket)
          begin
            Application.ProcessMessages;
          end
        );
      end,
      sWaitMsg
    );
  finally
    SetBtnVisibility(True);
  end;
end;

procedure TSWAGImportDlg.ValidatePage(const PageIdx: Integer);

  procedure ValidateChooseFolderPage;
  resourcestring
    sNoFolder = 'Please enter the directory where you downloaded the SWAG '
      + 'database.';
    sBadFolder = 'Directory "%s" does not exist. Please specify a valid one.';
    sBadVersion = '%s.' + EOL2
      + 'Please specify a directory containing a supported version.';
    sCorrupt = 'Not a valid SWAG database (%s). ' + EOL2
      + 'Please specify a different directory.';
  begin
    if GetDirNameFromEditCtrl = '' then
      raise EDataEntry.Create(sNoFolder, edPath);
    if not TDirectory.Exists(GetDirNameFromEditCtrl, False) then
      raise EDataEntry.CreateFmt(sBadFolder, [GetDirNameFromEditCtrl], edPath);
    try
      TSWAGVersion.ValidateVersionFile(GetDirNameFromEditCtrl);
    except
      on E: ECorruptSWAGVersion do
        raise EDataEntry.CreateFmt(sCorrupt, [E.Message], edPath);
      on E: EUnsupportedSWAGVersion do
        raise EDataEntry.CreateFmt(sBadVersion, [E.Message], edPath);
    end;
  end;

  procedure ValidateSelectionPage;
  resourcestring
    sEmptySelection = 'You must select one or more packets to import.';
  begin
    if fSelectedPackets.Count = 0 then
      raise EDataEntry.Create(sEmptySelection);
  end;

begin
  case PageIdx of
    cChooseFolderPage:
      ValidateChooseFolderPage;
    cSelectionPage:
      ValidateSelectionPage;
  end;
end;

procedure TSWAGImportDlg.WaitWrapper(AOwner: TComponent; const CallProc: TProc;
  const WaitMsg: string);
const
  PauseBeforeDisplay = 0;
  MinDisplayTime = 1000;
begin
  // Perform the operation
  TWaitForThreadUI.Run( // this blocks until thread completes
    CallProc,
    False,
    TWaitDlg.CreateAutoFree(AOwner, WaitMsg),
    PauseBeforeDisplay,
    MinDisplayTime
  );
end;

end.

