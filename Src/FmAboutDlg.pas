{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements the program's About dialogue box.
}


unit FmAboutDlg;


interface


uses
  // VCL
  Forms,
  ComCtrls,
  StdCtrls,
  Controls,
  ExtCtrls,
  Classes,
  Messages,
  Generics.Collections,
  // Project
  Browser.UHTMLEvents,
  DB.MetaData,
  DB.Vaults,
  FmGenericViewDlg,
  FrBrowserBase,
  FrHTMLDlg,
  FrHTMLTpltDlg,
  UCollectionListAdapter,
  UCSSBuilder,
  UIStringList;


type

  ///  <summary>Custom component that displays a path in a group box with an
  ///  associated button to display the path in Windows Explorer.</summary>
  TPathInfoBox = class(TCustomGroupBox)
  strict private
    var
      ///  <summary>Label that displays path.</summary>
      fPathLbl: TLabel;
      ///  <summary>Button that displays path in explorer.</summary>
      fViewBtn: TButton;
    ///  <summary>Read accessor for Path property. Gets and returns value from
    ///  path label.</summary>
    function GetPath: string;
    ///  <summary>Write accessor for Path property. Stores given value in path
    ///  label and updates state of button.</summary>
    procedure SetPath(const Value: string);
    ///  <summary>Button click event handler. Displays folder stored in Path
    ///  property in Windows Explorer.</summary>
    procedure BtnClick(Sender: TObject);
    ///  <summary>Handles font changes by resizing control to allow for new font
    ///  size.</summary>
    ///  <param name="Msg">TMessage [in/out] Not used.</param>
    procedure FontChange(var Msg: TMessage); message CM_FONTCHANGED;
    ///  <summary>Resizes and re-arranges control and its sub-components.
    ///  </summary>
    procedure ReArrange;
  strict protected
    ///  <summary>Handles control resizing. Re-arranges control's
    ///  sub-components.</summary>
    procedure Resize; override;
  public
    ///  <summary>Component constructor. Creates sub-components and arranges
    ///  them.</summary>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Path displayed in group box and displayed by view button.
    ///  </summary>
    property Path: string read GetPath write SetPath;
  end;

  ///  <summary>Implements program's about dialogue box.</summary>
  ///  <remarks>Displays information about the program, the collections in use
  ///  and the program's user and application folders and config files. Also
  ///  provides access to the program's easter egg.</remarks>
  TAboutDlg = class(TGenericViewDlg)
    bvlSeparator: TBevel;
    frmProgram: THTMLTpltDlgFrame;
    pcDetail: TPageControl;
    tsCollections: TTabSheet;
    tsProgram: TTabSheet;
    pnlTitle: TPanel;
    frmTitle: THTMLTpltDlgFrame;
    tsPaths: TTabSheet;
    btnViewAppConfig: TButton;
    btnViewUserConfig: TButton;
    cbCollection: TComboBox;
    lblCollection: TLabel;
    tvCollectionInfo: TTreeView;
    sbPaths: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    ///  <summary>Handles event triggered when user clicks on one of the page
    ///  control tabs. Ensures the page control has focus.</summary>
    ///  <remarks>Without this fix, the page control does not always get focus #
    ///  when a tab is clicked.</remarks>
    procedure pcDetailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    ///  <summary>Handles button click event to display application config file.
    ///  </summary>
    procedure btnViewAppConfigClick(Sender: TObject);
    ///  <summary>Handles button click event to display per-user config file.
    ///  </summary>
    procedure btnViewUserConfigClick(Sender: TObject);
    ///  <summary>Handles the change event triggered when the user selects a
    ///  collection in the collections combo box. Updates the display of
    ///  information about the selected collection.</summary>
    procedure cbCollectionChange(Sender: TObject);
  strict private
    var
      ///  <summary>List of dynamically created path information group boxes.
      ///  </summary>
      fPathInfoBoxes: TList<TPathInfoBox>;
      ///  <summary>Provides a sorted list of collection names for display in
      ///  the collections combo box.</summary>
      fCollList: TVaultListAdapter;
    ///  <summary>Handles title frame's OnHTMLEvent event. Checks for mouse
    ///  events relating to display of the easter egg and acts accordingly.
    ///  </summary>
    ///  <param name="Sender">TObject [in] Not used.</param>
    ///  <param name="EventInfo">THTMLEventInfo [in] Object providing
    ///  information about the event.</param>
    procedure HTMLEventHandler(Sender: TObject;
      const EventInfo: THTMLEventInfo);
    ///  <summary>Displays any meta data associated with a vault.</summary>
    ///  <param name="ACollection"><c>TVault</c> [in] Vault for which meta data
    ///  is to be displayed.</param>
    procedure DisplayCollectionInfo(ACollection: TVault);
    ///  <summary>Displays content of a config file in a dialogue box or an
    ///  error message if the file does not exist.</summary>
    ///  <param name="FileName">string [in] Name of config file to display.
    ///  </param>
    ///  <param name="DlgTitle">string [in] Title of dialogue box.</param>
    procedure ViewConfigFile(const FileName, DlgTitle: string);
  strict protected
    ///  <summary>Configures form by creating owned object and custom controls
    ///  and initialising HTML frames.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure ConfigForm; override;
    ///  <summary>Arranges controls on form.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure ArrangeForm; override;
    ///  <summary>Initialises HTML frames to use required HTML templates and
    ///  resolves all template placeholders.</summary>
    procedure InitHTMLFrames;
    ///  <summary>Updates CSS used for HTML displayed in title frame.</summary>
    ///  <param name="Sender">TObject [in] Not used.</param>
    ///  <param name="CSSBuilder">TCSSBuilder [in] Object used to update CSS.
    ///  </param>
    procedure UpdateTitleCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
    ///  <summary>Updates CSS used for HTML displayed in About The Program
    ///  tab frame.</summary>
    ///  <param name="Sender">TObject [in] Not used.</param>
    ///  <param name="CSSBuilder">TCSSBuilder [in] Object used to update CSS.
    ///  </param>
    procedure UpdateProgramTabCSS(Sender: TObject;
      const CSSBuilder: TCSSBuilder);
  public
    ///  <summary>Displays dialog box.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns this dialogue
    ///   box.</param>
    class procedure Execute(AOwner: TComponent);
  end;


implementation


uses
  // Delphi
  SysUtils,
  Graphics,
  Math,
  Windows,
  ShellAPI,
  IOUtils,
  // Project
  DB.DataFormats,
  FmEasterEgg,
  FmPreviewDlg,
  UAppInfo,
  UConsts,
  UCSSUtils,
  UCtrlArranger,
  UEncodings,
  UFontHelper,
  UGraphicUtils,
  UHTMLTemplate,
  UIOUtils,
  UMessageBox,
  UResourceUtils,
  UStrUtils,
  UThemesEx;

{$R *.dfm}

///  <summary>Displays Windows Explorer showing a specific folder.</summary>
///  <param name="Folder">string [in] Folder to explore.</param>
///  <returns>Boolean. True if explorer displayed, False if not.</returns>
function ExploreFolder(const Folder: string): Boolean;
begin
  if TDirectory.Exists(Folder) then
    Result := ShellExecute(
      0, 'explore', PChar(Folder), nil, nil, SW_SHOWNORMAL
    ) > 32
  else
    Result := False;
end;

{ TAboutDlg }

procedure TAboutDlg.ArrangeForm;
var
  PathInfoBox: TPathInfoBox;
  NextPathInfoBoxTop: Integer;
begin
  // Collections tab
  TCtrlArranger.AlignVCentres(8, [lblCollection, cbCollection]);
  TCtrlArranger.MoveToRightOf(lblCollection, cbCollection, 12);
  TCtrlArranger.MoveBelow([lblCollection, cbCollection], tvCollectionInfo, 8);

  // Paths tab
  TCtrlArranger.AlignTops([btnViewAppConfig, btnViewUserConfig], 8);
  TCtrlArranger.MoveBelow([btnViewAppConfig, btnViewUserConfig], sbPaths, 8);
  // stack path group boxes vertically
  NextPathInfoBoxTop := 8;
  for PathInfoBox in fPathInfoBoxes do
  begin
    PathInfoBox.Top := NextPathInfoBoxTop;
    Inc(NextPathInfoBoxTop, PathInfoBox.Height + 8);
  end;
  // align ctrl left & right sides
  TCtrlArranger.AlignLefts([sbPaths, btnViewAppConfig]);
  TCtrlArranger.AlignRights([sbPaths, btnViewUserConfig]);

  // Set height of title frame and page control
  pnlTitle.Height := frmTitle.DocHeight;
  pcDetail.ClientHeight := pcDetail.Height - tsProgram.ClientHeight
    + frmProgram.DocHeight + 8;
  pnlBody.ClientHeight := pnlTitle.Height + bvlSeparator.Height +
    pcDetail.Height;

  // Set path scroll box height
  sbPaths.ClientHeight := tsPaths.ClientHeight - sbPaths.Top - 28;

  // Set path controls' widths: must do this after all path box tops are set
  // and after scroll box height is set so any vertical scroll bar will have
  // been created.
  for PathInfoBox in fPathInfoBoxes do
    PathInfoBox.Width := sbPaths.ClientWidth - 2 * PathInfoBox.Left;

  inherited;
end;

procedure TAboutDlg.btnViewAppConfigClick(Sender: TObject);
resourcestring
  sTitle = 'Application Config File';
begin
  ViewConfigFile(TAppInfo.AppConfigFileName, sTitle);
end;

procedure TAboutDlg.btnViewUserConfigClick(Sender: TObject);
resourcestring
  sTitle = 'Per-User Config File';
begin
  ViewConfigFile(TAppInfo.UserConfigFileName, sTitle);
end;

procedure TAboutDlg.cbCollectionChange(Sender: TObject);
begin
  DisplayCollectionInfo(fCollList.Vault(cbCollection.ItemIndex));
end;

procedure TAboutDlg.ConfigForm;

  //  Creates and initialises a custom path information control with given
  //  caption, path and tab order.</summary>
  function CreatePathInfoBox(const Caption, Path: string;
    const TabOrder: Integer): TPathInfoBox;
  const
    MarginWidth = 8;
  begin
    Result := TPathInfoBox.CreateParented(sbPaths.Handle);
    Result.Parent := sbPaths;
    Result.SetBounds(MarginWidth, MarginWidth, sbPaths.ClientWidth - 2 * MarginWidth, 0);
    Result.Caption := Caption;
    Result.Path := Path;
    Result.TabOrder := TabOrder;
  end;

var
  Collection: TVault;
  TabIdx: Integer;
resourcestring
  // Captions for custom controls
  sInstallPathGpCaption = 'Install Directory';
  sCollectionPathGpCaption = '%s Collection Directory';
begin
  inherited;
  // Creates required custom controls
  TabIdx := 0;
  fPathInfoBoxes.Add(
    CreatePathInfoBox(sInstallPathGpCaption, TAppInfo.AppExeDir, 1)
  );
  for Collection in TVaults.Instance do
  begin
    Inc(TabIdx);
    fPathInfoBoxes.Add(
      CreatePathInfoBox(
        Format(sCollectionPathGpCaption, [Collection.Name]),
        Collection.Storage.Directory,
        TabIdx
      )
    );
  end;
  // Load collections into combo box & select default collection
  fCollList.ToStrings(cbCollection.Items);
  cbCollection.ItemIndex := fCollList.IndexOfUID(TVaultID.Default);
  DisplayCollectionInfo(fCollList.Vault(cbCollection.ItemIndex));
  // Set collections treeview and paths scrollbox background colours
  tvCollectionInfo.Color := ThemeServicesEx.GetTabBodyColour;
  sbPaths.Color := ThemeServicesEx.GetTabBodyColour;
  // Load content into HTML frames
  InitHTMLFrames;
end;

procedure TAboutDlg.DisplayCollectionInfo(ACollection: TVault);
var
  HasEntries: Boolean;

  function AddChild(const AParentNode: TTreeNode; const AData: string):
    TTreeNode;
  begin
    Result := tvCollectionInfo.Items.AddChild(AParentNode, AData);
    HasEntries := True;
  end;

  procedure AddChildren(const AParentNode: TTreeNode; const AData: IStringList);
  var
    DataItem: string;
  begin
    for DataItem in AData do
      AddChild(AParentNode, DataItem);
  end;

var
  MetaData: TMetaData;
  Capabilities: TMetaDataCaps;
  HeadingNode: TTreeNode;
  SubheadingNode: TTreeNode;
resourcestring
  sVersionHeading = 'Version';
  sLicenseHeading = 'License';
  sCopyrightHeading = 'Copyright';
  sContributorsHeading = 'Contributors';
  sAcknowledgementsHeading = 'Acknowledgements';
  sNoMetaData = 'No information available for this collection.';
  sNotAvailable = 'Not specified';
  sNone = 'None';
begin
  tvCollectionInfo.Items.BeginUpdate;
  try
    tvCollectionInfo.Items.Clear;
    HasEntries := False;
    MetaData := ACollection.MetaData;
    Capabilities := MetaData.Capabilities;

    if Capabilities <> [] then
    begin

      if TMetaDataCap.Version in Capabilities then
      begin
        HeadingNode := AddChild(nil, sVersionHeading);
        if not MetaData.Version.IsNull then
          AddChild(HeadingNode, MetaData.Version)
        else
          AddChild(HeadingNode, sNotAvailable);
      end;

      if (TMetaDataCap.License in Capabilities) then
      begin
        HeadingNode := AddChild(nil, sLicenseHeading);
        if not MetaData.LicenseInfo.IsNull then
        begin
          if not StrIsEmpty(MetaData.LicenseInfo.Name)
            and not StrIsEmpty(MetaData.LicenseInfo.SPDX) then
            AddChild(
              HeadingNode,
              StrIf(
                MetaData.LicenseInfo.Name <> '',
                MetaData.LicenseInfo.Name,
                MetaData.LicenseInfo.SPDX
              )
            );
          if not StrIsEmpty(MetaData.LicenseInfo.URL) then
            AddChild(HeadingNode, MetaData.LicenseInfo.URL);
        end
        else
          AddChild(HeadingNode, sNone);
      end;

      if TMetaDataCap.Copyright in Capabilities then
      begin
        HeadingNode := AddChild(nil, sCopyrightHeading);
        if not MetaData.CopyrightInfo.IsNull then
        begin
          if not StrIsEmpty(MetaData.CopyrightInfo.Date) then
            AddChild(HeadingNode, MetaData.CopyrightInfo.Date);
          if not StrIsEmpty(MetaData.CopyrightInfo.Holder) then
            AddChild(HeadingNode, MetaData.CopyrightInfo.Holder);
          if not StrIsEmpty(MetaData.CopyrightInfo.HolderURL) then
            AddChild(HeadingNode, MetaData.CopyrightInfo.HolderURL);
          if MetaData.CopyrightInfo.Contributors.Count > 0 then
          begin
            SubheadingNode := AddChild(HeadingNode, sContributorsHeading);
            AddChildren(SubheadingNode, MetaData.CopyrightInfo.Contributors);
          end;
        end
        else
          AddChild(HeadingNode, sNone);
      end;

      if TMetaDataCap.Acknowledgements in Capabilities then
      begin
        HeadingNode := AddChild(nil, sAcknowledgementsHeading);
        if MetaData.Acknowledgements.Count > 0 then
          AddChildren(HeadingNode, MetaData.Acknowledgements)
        else
          AddChild(HeadingNode, sNone);
      end;
    end

    else
      AddChild(nil, sNoMetaData);

    if HasEntries then
    begin
      tvCollectionInfo.FullExpand;
      tvCollectionInfo.Items[0].MakeVisible;
    end;

  finally
    tvCollectionInfo.Items.EndUpdate;
  end;
end;

class procedure TAboutDlg.Execute(AOwner: TComponent);
var
  Dlg: TAboutDlg;
begin
  Dlg := Create(AOwner);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

procedure TAboutDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fCollList := TVaultListAdapter.Create;
  fPathInfoBoxes := TList<TPathInfoBox>.Create;
  frmTitle.OnBuildCSS := UpdateTitleCSS;
  frmProgram.OnBuildCSS := UpdateProgramTabCSS;
end;

procedure TAboutDlg.FormDestroy(Sender: TObject);
begin
  inherited;
  fPathInfoBoxes.Free;
  fCollList.Free;
end;

procedure TAboutDlg.HTMLEventHandler(Sender: TObject;
  const EventInfo: THTMLEventInfo);
const
  cIconImgId = 'icon';  // id of icon image
begin
  // Check for onclick event on icon tag: display easter egg if ctrl key
  // pressed. Such an event is cancelled.
  if EventInfo.IsEvent(
      THTMLDocumentEvents2Sink.EventIntf,
      THTMLDocumentEvents2Sink.DISPID_OnClick
    )
    and EventInfo.Args.ctrlKey
    and EventInfo.ElemHasId(cIconImgId) then
  begin
    EventInfo.Cancelled := True;
    TEasterEggForm.Execute(Self);
  end;
  // Check for mouse move over icon tag: change cursor to hand if ctrl key
  // pressed to indicate clickable. Event permitted to bubble up.
  if EventInfo.IsEvent(
      THTMLDocumentEvents2Sink.EventIntf,
      THTMLDocumentEvents2Sink.DISPID_OnMouseMove
    )
    and EventInfo.ElemHasId(cIconImgId) then
  begin
    if EventInfo.Args.ctrlKey then
      EventInfo.Args.srcElement.style.cursor := 'hand'
    else
      EventInfo.Args.srcElement.style.cursor := 'auto';
  end;
end;

procedure TAboutDlg.InitHTMLFrames;

  // Initialises and loads HTML into title frame.
  procedure InitTitleFrame;
  begin
    frmTitle.Initialise(
      'dlg-about-head-tplt.html',
      procedure(Tplt: THTMLTemplate)
      begin
        Tplt.ResolvePlaceholderText('Release', TAppInfo.ProgramReleaseInfo);
        // MakeResourceURL('') provides just URL part before resource name
        Tplt.ResolvePlaceholderHTML('ResURL', MakeResourceURL(''));
      end
    );
    frmTitle.OnHTMLEvent := HTMLEventHandler;
  end;

  // Initialises and loads HTML into program frame.
  procedure InitProgramFrame;
  begin
    pcDetail.ActivePage := tsProgram;   // display page to let browser load OK
    frmProgram.Initialise(
      'dlg-about-program-tplt.html',
      procedure(Tplt: THTMLTemplate)
      begin
        // Do nothing: no template placeholders now registration removed
      end
    );
  end;

begin
  InitTitleFrame;
  InitProgramFrame;
end;

procedure TAboutDlg.pcDetailMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if htOnItem in pcDetail.GetHitTestInfoAt(X, Y) then
    pcDetail.SetFocus;
end;

procedure TAboutDlg.UpdateProgramTabCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
var
  ContentFont: TFont; // font used for content
begin
  // Modify body's margin and, for themed windows, background colour
  ContentFont := TFont.Create;
  try
    TFontHelper.SetContentFont(ContentFont);
    CSSBuilder.Selectors['body']
      .AddProperty(TCSS.FontProps(ContentFont))
      .AddProperty(UCSSUtils.TCSS.MarginProp(0, 2, 6, 2))
      .AddPropertyIf(
        ThemeServicesEx.ThemesEnabled,
        TCSS.BackgroundColorProp(ThemeServicesEx.GetTabBodyColour)
      );
  finally
    FreeAndNil(ContentFont);
  end;
  // Put border round scroll box
  CSSBuilder.AddSelector('.scrollbox')
    .AddProperty(UCSSUtils.TCSS.BorderProp(cssAll, 1, cbsSolid, clBtnShadow));
end;

procedure TAboutDlg.UpdateTitleCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
begin
  CSSBuilder.Selectors['body']
    .AddProperty(TCSS.BackgroundColorProp(clWindow))
    .AddProperty(TCSS.PaddingProp(4));
end;

procedure TAboutDlg.ViewConfigFile(const FileName, DlgTitle: string);
var
  Data: TEncodedData;
resourcestring
  sErrorMsg = 'Sorry, this config file does not (yet) exist.';
begin
  if not TFile.Exists(FileName) then
  begin
    TMessageBox.Error(Self, sErrorMsg);
    Exit;
  end;
  Data := TEncodedData.Create(
    TFileIO.ReadAllText(FileName, TEncoding.Unicode, True),
    etUTF16LE
  );
  TPreviewDlg.Execute(Self, Data, dtPlainText, DlgTitle);
end;

{ TPathInfoBox }

procedure TPathInfoBox.BtnClick(Sender: TObject);
begin
  if Assigned(fPathLbl) and (fPathLbl.Caption <> '') then
    ExploreFolder(fPathLbl.Caption);
end;

constructor TPathInfoBox.Create(AOwner: TComponent);
resourcestring
  // Hint attached to view button
  sViewBtnHint = 'Explore...|Display the path in Windows Explorer';
begin
  inherited;
  // Create and setup path label
  fPathLbl := TLabel.Create(Self);
  fPathLbl.Parent := Self;
  fPathLbl.Left := 8;
  fPathLbl.Top := 8;
  fPathLbl.Width := 120;
  fPathLbl.AutoSize := False;
  fPathLbl.EllipsisPosition := epPathEllipsis;
  fPathLbl.Width := Self.Width - 16;
  fPathLbl.Caption := ' ';
  fPathLbl.Transparent := True;
  fPathLbl.ShowHint := True;
  // Create and setup view button
  fViewBtn := TButton.Create(Self);
  fViewBtn.Parent := Self;
  fViewBtn.OnClick := BtnClick;
  fViewBtn.Height := 19;
  fViewBtn.Width := 26;
  fViewBtn.Caption := '...';
  fViewBtn.Hint := sViewBtnHint;
  fViewBtn.ShowHint := True;
  // Ensure correct default font is used
  TFontHelper.SetDefaultBaseFont(Font);
  // Size and arrange controls
  ReArrange;
end;

procedure TPathInfoBox.FontChange(var Msg: TMessage);
begin
  inherited;
  ReArrange;
end;

function TPathInfoBox.GetPath: string;
begin
  Result := fPathLbl.Caption;
end;

procedure TPathInfoBox.ReArrange;
begin
  TCtrlArranger.SetLabelHeight(fPathLbl);
  Height := Max(fPathLbl.Height, fViewBtn.Height) + 24;
  TCtrlArranger.AlignVCentres(
    (ClientHeight - Max(fPathLbl.Height, fViewBtn.Height)) div 3 * 2,
    [fPathLbl, fViewBtn]
  );
  fViewBtn.Left := ClientWidth - fViewBtn.Width - 8;
  fPathLbl.Left := 8;
  fPathLbl.Width := fViewBtn.Left - fPathLbl.Left - 8;
end;

procedure TPathInfoBox.Resize;
begin
  inherited;
  ReArrange;
end;

procedure TPathInfoBox.SetPath(const Value: string);
resourcestring
  // hints used when path doesn't exist
  sShortPathDoesNotExist = 'Path does not exist';
  sLongPathDoesNotExist = 'Path "%s"' + EOL + 'does not exist';
var
  TextW: Integer; // width of full path name in label in pixels
begin
  fPathLbl.Caption := Value;
  TextW := StringExtent(Value, fPathLbl.Font).cx;
  if TDirectory.Exists(Value) then
  begin
    if TextW > fPathLbl.Width then
      // path will contain ellipsis in label: display full path as hint
      fPathLbl.Hint := Value + '|'  // pipe char makes this short (pop-up) hint
    else
      // path fully displayed in label: no hint
      fPathLbl.Hint := '';
    fViewBtn.Enabled := True;
  end
  else
  begin
    if TextW > fPathLbl.Width then
      // path will contain ellipsis: display full path with message as hint
      fPathLbl.Hint := Format(sLongPathDoesNotExist, [Value]) + '|'
    else
      // path fully displayed in label: don't include full path in hint
      fPathLbl.Hint := sShortPathDoesNotExist;
    fViewBtn.Enabled :=  False;
  end;
end;

end.

