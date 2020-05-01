{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
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
  // Project
  Browser.UHTMLEvents,
  DB.UMetaData,
  FmGenericViewDlg,
  FrBrowserBase,
  FrHTMLDlg,
  FrHTMLTpltDlg,
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

  ///  <summary>Implements program&#39; about dialogue box.</summary>
  ///  <remarks>Displays information about the program, the main database and
  ///  the program's user and application folders and config files. Also
  ///  provides access to the program's easter egg.</remarks>
  TAboutDlg = class(TGenericViewDlg)
    bvlSeparator: TBevel;
    frmDatabase: THTMLTpltDlgFrame;
    frmProgram: THTMLTpltDlgFrame;
    pcDetail: TPageControl;
    tsDatabase: TTabSheet;
    tsProgram: TTabSheet;
    pnlTitle: TPanel;
    frmTitle: THTMLTpltDlgFrame;
    tsPaths: TTabSheet;
    btnViewAppConfig: TButton;
    btnViewUserConfig: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    ///  <summary>Handles event triggered when user clicks on one of page
    ///  control tabs. Ensures page control has focus.</summary>
    ///  <remarks>Without this fix, page control does not always get focus when
    ///  a tab is clicked.</remarks>
    procedure pcDetailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    ///  <summary>Handles button click event to display application config file.
    ///  </summary>
    procedure btnViewAppConfigClick(Sender: TObject);
    ///  <summary>Handles button click event to display per-user config file.
    ///  </summary>
    procedure btnViewUserConfigClick(Sender: TObject);
  strict private
    var
      ///  <summary>Control that displays main database folder.</summary>
      fMainDBPathGp: TPathInfoBox;
      ///  <summary>Control that displays user database folder.</summary>
      fUserDBPathGp: TPathInfoBox;
      ///  <summary>Control that displays program install path.</summary>
      fInstallPathGp: TPathInfoBox;
      ///  <summary>Provides access to main database meta data.</summary>
      fMetaData: IDBMetaData;
    ///  <summary>Handles title frame's OnHTMLEvent event. Checks for mouse
    ///  events relating to display of the easter egg and acts accordingly.
    ///  </summary>
    ///  <param name="Sender">TObject [in] Not used.</param>
    ///  <param name="EventInfo">THTMLEventInfo [in] Object providing
    ///  information about the event.</param>
    procedure HTMLEventHandler(Sender: TObject;
      const EventInfo: THTMLEventInfo);
    ///  <summary>Builds HTML used to display list of contributors or an error
    ///  message if the list is empty.</summary>
    ///  <param name="ContribList">IStringList [in] List of contributors to
    ///  display.</param>
    ///  <returns>string. Required HTML.</returns>
    function ContribListHTML(ContribList: IStringList): string;
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
    ///  <summary>Updates CSS used for HTML displayed in detail frames.
    ///  </summary>
    ///  <param name="Sender">TObject [in] Not used.</param>
    ///  <param name="CSSBuilder">TCSSBuilder [in] Object used to update CSS.
    ///  </param>
    ///  <remarks>Details frames form the body of the About Box on the Program
    ///  and Database tabs.</remarks>
    procedure UpdateDetailCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
  public
    ///  <summary>Displays dialog box.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns this dialogus
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
  DB.UMain,
  FmEasterEgg,
  FmPreviewDlg,
  UAppInfo,
  UColours,
  UConsts,
  UCSSUtils,
  UCtrlArranger,
  UEncodings,
  UFontHelper,
  UGraphicUtils,
  UHTMLUtils,
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
  PathTabHeight: Integer;
begin
  fMainDBPathGp.Top := TCtrlArranger.BottomOf(fInstallPathGp, 8);
  fUserDBPathGp.Top := TCtrlArranger.BottomOf(fMainDBPathGp, 8);
  TCtrlArranger.AlignTops(
    [btnViewAppConfig, btnViewUserConfig],
    TCtrlArranger.BottomOf(fUserDBPathGp, 8)
  );
  PathTabHeight := TCtrlArranger.BottomOf(
    [btnViewUserConfig, btnViewAppConfig]
  );
  TCtrlArranger.AlignLefts([fUserDBPathGp, btnViewAppConfig]);
  TCtrlArranger.AlignRights([fUserDBPathGp, btnViewUserConfig]);
  // Set height of title frame and page control
  pnlTitle.Height := frmTitle.DocHeight;
  pcDetail.ClientHeight :=
    pcDetail.Height - tsProgram.ClientHeight +
    Max(
      PathTabHeight,
      Max(frmProgram.DocHeight, frmDatabase.DocHeight)
    ) + 8;
  pnlBody.ClientHeight := pnlTitle.Height + bvlSeparator.Height +
    pcDetail.Height;
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

procedure TAboutDlg.ConfigForm;

  //  Creates and initialises a custom path information control with given
  //  caption, path and tab order.</summary>
  function CreatePathInfoBox(const Caption, Path: string;
    const TabOrder: Integer): TPathInfoBox;
  begin
    Result := TPathInfoBox.CreateParented(tsPaths.Handle);
    Result.Parent := tsPaths;
    Result.SetBounds(8, 8, tsPaths.ClientWidth - 16, 0);
    Result.Caption := Caption;
    Result.Path := Path;
    Result.TabOrder := TabOrder;
  end;

resourcestring
  // Captions for custom controls
  sInstallPathGpCaption = 'Install Directory';
  sMainDBPathGpCaption = 'Main Database Directory';
  sUserDBPathGpCaption = 'User Database Directory';
begin
  inherited;
  // Create meta data object for main database
  fMetaData := TMainDBMetaDataFactory.MainDBMetaDataInstance;
  // Creates required custom controls
  fInstallPathGp := CreatePathInfoBox(
    sInstallPathGpCaption, TAppInfo.AppExeDir, 0
  );
  fMainDBPathGp := CreatePathInfoBox(
    sMainDBPathGpCaption, TAppInfo.AppDataDir, 1
  );
  fUserDBPathGp := CreatePathInfoBox(
    sUserDBPathGpCaption, TAppInfo.UserDataDir, 2
  );
  btnViewAppConfig.TabOrder := fUserDBPathGp.TabOrder + 1;
  btnViewUserConfig.TabOrder := btnViewAppConfig.TabOrder + 1;
  // Load content into HTML frames
  InitHTMLFrames;
end;

function TAboutDlg.ContribListHTML(ContribList: IStringList):
  string;
resourcestring
  // Error string used when contributor file not available
  sNoContributors = 'No contributors list available. Database may be corrupt';
var
  Contributor: string;          // name of a contributor
  DivAttrs: IHTMLAttributes;    // attributes of div tag
begin
  Result := '';
  if ContribList.Count > 0 then
  begin
    for Contributor in ContribList do
      Result := Result
        + THTML.CompoundTag('div', THTML.Entities(Contributor))
        + EOL;
  end
  else
  begin
    // List couldn't be found: display warning message
    DivAttrs := THTMLAttributes.Create('class', 'warning');
    Result := THTML.CompoundTag(
      'div', DivAttrs, THTML.Entities(sNoContributors)
    );
  end;
end;

class procedure TAboutDlg.Execute(AOwner: TComponent);
begin
  with Create(AOwner) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TAboutDlg.FormCreate(Sender: TObject);
begin
  inherited;
  frmTitle.OnBuildCSS := UpdateTitleCSS;
  frmProgram.OnBuildCSS := UpdateDetailCSS;
  frmDatabase.OnBuildCSS := UpdateDetailCSS;
end;

procedure TAboutDlg.FormDestroy(Sender: TObject);
begin
  inherited;
  fInstallPathGp.Free;
  fMainDBPathGp.Free;
  fUserDBPathGp.Free;
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

  // Initialises and loads HTML into database frame.
  procedure InitDatabaseFrame;
  begin
    // Ensure browser loads page so we can process it
    pcDetail.ActivePage := tsDatabase;

    frmDatabase.Initialise(
      'dlg-about-database-tplt.html',
      procedure(Tplt: THTMLTemplate)
      var
        IsDBAvalable: Boolean;
        IsMetaDataAvailable: Boolean;
        IsLicenseInfoAvailable: Boolean;
      begin
        // Resolve conditionally displayed block placeholders
        IsDBAvalable := Database.Snippets.Count(False) > 0;
        IsMetaDataAvailable := fMetaData.IsSupportedVersion
          and not fMetaData.IsCorrupt;
        IsLicenseInfoAvailable := IsMetaDataAvailable
          and (fMetaData.GetLicenseInfo.Name <> '')
          and (fMetaData.GetCopyrightInfo.Date <> '')
          and (fMetaData.GetCopyrightInfo.Holder <> '');
        Tplt.ResolvePlaceholderHTML(
          'DBAvailable', TCSS.BlockDisplayProp(IsDBAvalable)
        );
        Tplt.ResolvePlaceholderHTML(
          'DBNotAvailable', TCSS.BlockDisplayProp(not IsDBAvalable)
        );
        Tplt.ResolvePlaceholderHTML(
          'MetaDataAvailable', TCSS.BlockDisplayProp(IsMetaDataAvailable)
        );
        Tplt.ResolvePlaceholderHTML(
          'MetaDataNotAvailable', TCSS.BlockDisplayProp(not IsMetaDataAvailable)
        );
        Tplt.ResolvePlaceholderHTML(
          'LicenseInfoAvailable', TCSS.BlockDisplayProp(IsLicenseInfoAvailable)
        );
        Tplt.ResolvePlaceholderHTML(
          'LicenseInfoNotAvailable',
          TCSS.BlockDisplayProp(not IsLicenseInfoAvailable)
        );

        // Resolve content placeholders
        Tplt.ResolvePlaceholderText(
          'CopyrightYear', fMetaData.GetCopyrightInfo.Date
        );
        Tplt.ResolvePlaceholderText(
          'CopyrightHolders', fMetaData.GetCopyrightInfo.Holder
        );
        Tplt.ResolvePlaceholderHTML(
          'DBLicense',
          StrIf(
            fMetaData.GetLicenseInfo.URL <> '',
            THTML.CompoundTag(
              'a',
              THTMLAttributes.Create([
                THTMLAttribute.Create('href', fMetaData.GetLicenseInfo.URL),
                THTMLAttribute.Create('class', 'external-link')
              ]),
              THTML.Entities(fMetaData.GetLicenseInfo.Name)
            ),
            THTML.Entities(fMetaData.GetLicenseInfo.Name)
          )
        );
        Tplt.ResolvePlaceholderHTML(
          'ContribList', ContribListHTML(fMetaData.GetContributors)
        );
        Tplt.ResolvePlaceholderHTML(
          'TesterList', ContribListHTML(fMetaData.GetTesters)
        );
        Tplt.ResolvePlaceholderText('Version', fMetaData.GetVersion);
      end
    );
  end;
  // ---------------------------------------------------------------------------

begin
  InitTitleFrame;
  InitDatabaseFrame;
  InitProgramFrame;
end;

procedure TAboutDlg.pcDetailMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if htOnItem in pcDetail.GetHitTestInfoAt(X, Y) then
    pcDetail.SetFocus;
end;

procedure TAboutDlg.UpdateDetailCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
var
  ContentFont: TFont; // font used for content
begin
  // Modify body's margin and, for themed windows, background colour
  with CSSBuilder.Selectors['body'] do
  begin
    ContentFont := TFont.Create;
    try
      TFontHelper.SetContentFont(ContentFont);
      AddProperty(TCSS.FontProps(ContentFont));
      if ThemeServicesEx.ThemesEnabled then
        AddProperty(TCSS.BackgroundColorProp(ThemeServicesEx.GetTabBodyColour));
      AddProperty(UCSSUtils.TCSS.MarginProp(0, 2, 6, 2));
    finally
      FreeAndNil(ContentFont);
    end;
  end;
  // Put border round scroll box
  with CSSBuilder.AddSelector('.scrollbox') do
    AddProperty(UCSSUtils.TCSS.BorderProp(cssAll, 1, cbsSolid, clBorder));
  // Set colours and font style of contributors and testers headings
  with CSSBuilder.AddSelector('.contrib-head, .tester-head') do
  begin
    AddProperty(TCSS.BackgroundColorProp(clBtnFace));
    AddProperty(TCSS.ColorProp(clBtnText));
    AddProperty(TCSS.FontWeightProp(cfwBold));
  end;
end;

procedure TAboutDlg.UpdateTitleCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
begin
  // Set body colour, and put border round it
  with CSSBuilder.Selectors['body'] do
  begin
    AddProperty(TCSS.BackgroundColorProp(clWindow));
    AddProperty(TCSS.PaddingProp(4));
  end;
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

