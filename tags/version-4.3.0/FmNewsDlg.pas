{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box that displays news items from CodeSnip's RSS news
 * feed.
}


unit FmNewsDlg;


interface


uses
  // Delphi
  Buttons, StdCtrls, Forms, Controls, ExtCtrls, Classes,
  // Project
  FmGenericViewDlg, FrBrowserBase, FrRSSNews, UBaseObjects, UExceptions, URSS20,
  UXMLDocumentEx, ActnList, ImgList;


type

  {
  TNewsDlg:
    Dialog box that displays news items from CodeSnip's RSS news feed.
  }
  TNewsDlg = class(TGenericViewDlg, INoPublicConstruct)
    actConfig: TAction;
    actRSSFeed: TAction;
    alMain: TActionList;
    btnConfig: TButton;
    btnRSSFeed: TBitBtn;
    frmHTML: TRSSNewsFrame;
    ilActions: TImageList;
    lblDays: TLabel;
    pnlTop: TPanel;
    ///  <summary>Configure action handler. Displays news prefs page of
    ///  Preferences dialog box to enable number of days to display in news
    procedure actConfigExecute(Sender: TObject);
    ///  <summary>Action handler that displays RSS feed in default web browser.
    ///  </summary>
    procedure actRSSFeedExecute(Sender: TObject);
    ///  <summary>Form creation handler. Loads image list from resources.
    ///  </summary>
    procedure FormCreate(Sender: TObject);
  strict private
    ///  <summary>Loads news from RSS feed, converts to HTML and displays in
    ///  browser control.</summary>
    procedure LoadNews;
    ///  <summary>Displays brief message Msg as an HTML paragraph in browser
    ///  control.</summary>
    procedure DisplayMessage(const Msg: string);
    ///  <summary>Displays a message in browser that informs that content is
    ///  loading.</summary>
    procedure DisplayLoadingMsg;
    ///  <summary>Renders RSS news feed in HTML and displays in browser control.
    ///  </summary>
    procedure DisplayNews(const RSS: TRSS20);
    ///  <summary>Displays a message in browser control informing that there are
    ///  no news items in RSS feed.</summary>
    procedure DisplayNoNewsMsg;
    ///  <summary>Renders HTML describing exception E and displays it in browser
    ///  control.</summary>
    procedure DisplayErrorMsg(E: ECodeSnip);
    ///  <summary>Gets and returns interface to XML document containing details
    ///  of RSS news feed from web.</summary>
    function GetRSSDocument: IXMLDocumentEx;
    ///  <summary>Gets maximum number of days of news to be displayed.</summary>
    function GetMaxNewsAge: Integer;
    ///  <summary>Updates label with current maximum news age.</summary>
    procedure UpdateNewsAgeLbl;
  strict protected
    ///  <summary>Arranges controls on form.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure ArrangeForm; override;
    ///  <summary>Initialises HTML frame.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure ConfigForm; override;
    ///  <summary>Initialises form's controls.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure InitForm; override;
    ///  <summary>Loads news from RSS feed after form is displayed.</summary>
    ///  <remarks>Called from ancestor class.</remarks>
    procedure AfterShowForm; override;
  public
    ///  <summary>Displays dialog box.</summary>
    ///  <param name="AOwner">TComponent [in] Control that owns dialog box.
    ///  </param>
    class procedure Execute(AOwner: TComponent);
  end;


implementation


uses
  // Delphi
  SysUtils, ExtActns, Windows, Graphics,
  // Project
  FmPreferencesDlg, FrNewsPrefs, UClassHelpers, UCtrlArranger, UHTMLUtils,
  UIStringList, UPreferences, UStrUtils, Web.UInfo, Web.UXMLRequestor;

{$R *.dfm}


{ TNewsDlg }

procedure TNewsDlg.actConfigExecute(Sender: TObject);
var
  CurrentNewsAge: Integer;
begin
  CurrentNewsAge := GetMaxNewsAge;
  if TPreferencesDlg.Execute(Self, [TNewsPrefsFrame]) then
  begin
    if CurrentNewsAge <> GetMaxNewsAge then
      LoadNews;
  end;
end;

procedure TNewsDlg.actRSSFeedExecute(Sender: TObject);
var
  BrowseAction: TBrowseURL; // action that displays RSS feed URL in browser
begin
  BrowseAction := TBrowseURL.Create(nil);
  try
    BrowseAction.URL := TWebInfo.NewsFeedURL(GetMaxNewsAge);
    BrowseAction.Execute;
  finally
    BrowseAction.Free;
  end;
end;

procedure TNewsDlg.AfterShowForm;
begin
  LoadNews;
end;

procedure TNewsDlg.ArrangeForm;
begin
  inherited;
  TCtrlArranger.AlignVCentres(2, [lblDays, btnConfig]);
  lblDays.Left := 0;
  pnlTop.ClientHeight := TCtrlArranger.TotalControlHeight(pnlTop) + 8;
  btnRSSFeed.Top := btnClose.Top;
end;

procedure TNewsDlg.ConfigForm;
begin
  inherited;
  frmHTML.Initialise;
end;

procedure TNewsDlg.DisplayErrorMsg(E: ECodeSnip);
resourcestring
  sErrorHeading = 'Error loading news:';  // fixed heading text
var
  ErrHeadingAttrs: IHTMLAttributes; // HTML attributes of heading
  ErrMessageAttrs: IHTMLAttributes; // HTML attributes of error message
begin
  ErrHeadingAttrs := THTMLAttributes.Create('class', 'error-heading');
  ErrMessageAttrs := THTMLAttributes.Create('class', 'error-message');
  frmHTML.DisplayContent(
    THTML.CompoundTag('p', ErrHeadingAttrs, THTML.Entities(sErrorHeading)) +
    THTML.CompoundTag('p', ErrMessageAttrs, THTML.Entities(E.Message))
  );
end;

procedure TNewsDlg.DisplayLoadingMsg;
resourcestring
  sLoadingMsg = 'Loading...'; // message text
begin
  DisplayMessage(sLoadingMsg);
end;

procedure TNewsDlg.DisplayMessage(const Msg: string);
var
  HTMLAttrs: IHTMLAttributes; // HTML attributes
begin
  HTMLAttrs := THTMLAttributes.Create('class', 'message');
  frmHTML.DisplayContent(
    THTML.CompoundTag('p', HTMLAttrs, THTML.Entities(Msg))
  );
end;

procedure TNewsDlg.DisplayNews(const RSS: TRSS20);

  ///  Renders the given RSS news item title as HTML. Rendered as a link if item
  ///  specifies a URL.
  function TitleHTML(const Item: TRSS20Item): string;
  resourcestring
    sNoTitle = 'Untitled';  // text used when no title
  var
    TitleHTML: string;  // title text
    URL: string;        // item's URL used for link
  begin
    TitleHTML := THTML.Entities(StrTrim(Item.Title));
    if TitleHTML = '' then
      TitleHTML := THTML.Entities(sNoTitle);
    URL := StrTrim(Item.Link);
    if URL = '' then
      Result := TitleHTML
    else
      Result := THTML.CompoundTag(
        'a',
        THTMLAttributes.Create([
          THTMLAttribute.Create('href', URL),
          THTMLAttribute.Create('class', 'external-link')
        ]),
        TitleHTML
      );
    Result := THTML.CompoundTag('strong', Result);
  end;

  ///  Renders given RSS new item's description as HTML.
  function DescriptionHTML(const Item: TRSS20Item): string;
  resourcestring
    sNoDescription = 'No description.'; // text used when no description
  var
    Description: string;  // description text
  begin
    Description := StrTrim(Item.Description);
    if Description = '' then
      Description := sNoDescription;
    Result := THTML.Entities(Description);
  end;

var
  SB: TStringBuilder; // object used to construct HTML
  Item: TRSS20Item;   // references each RSS item in feed
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine(THTML.OpeningTag('dl'));
    for Item in RSS do
    begin
      SB.AppendLine(THTML.OpeningTag('dt'));
      SB.AppendLine(THTML.CompoundTag('div', TitleHTML(Item)));
      if Item.PubDateAsText <> '' then
        SB.AppendLine(
          THTML.CompoundTag('div', THTML.Entities(DateTimeToStr(Item.PubDate)))
        );
      SB.AppendLine(THTML.ClosingTag('dt'));
      SB.AppendLine(THTML.CompoundTag('dd', DescriptionHTML(Item)));
    end;
    SB.AppendLine(THTML.ClosingTag('dl'));
    frmHTML.DisplayContent(SB.ToString);
  finally
    SB.Free;
  end;
end;

procedure TNewsDlg.DisplayNoNewsMsg;
resourcestring
  sNoNews = 'There are no news items to display.';  // message text
begin
  DisplayMessage(sNoNews);
end;

class procedure TNewsDlg.Execute(AOwner: TComponent);
begin
  with InternalCreate(AOwner) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TNewsDlg.FormCreate(Sender: TObject);
begin
  inherited;
  ilActions.LoadFromResource(RT_RCDATA, 'ACTIONIMAGES', 16, clFuchsia);
  RefreshActions; // ensure control glyphs are updated with loaded images
end;

function TNewsDlg.GetMaxNewsAge: Integer;
begin
  Result := Preferences.NewsAge;
end;

function TNewsDlg.GetRSSDocument: IXMLDocumentEx;
var
  Requestor: TXMLRequestor; // object that makes XML request
begin
  Requestor := TXMLRequestor.Create;
  try
    Result := Requestor.GetDocument(TWebInfo.NewsFeedURL(GetMaxNewsAge));
  finally
    Requestor.Free;
  end;
end;

procedure TNewsDlg.InitForm;
begin
  inherited;
  lblDays.Caption := '';
end;

procedure TNewsDlg.LoadNews;
var
  RSSFeed: TRSS20;  // object used to interpret RSS feed XML
begin
  Screen.Cursor := crHourGlass;
  try
    DisplayLoadingMsg;
    try
      RSSFeed := TRSS20.Create;
      try
        RSSFeed.Load(GetRSSDocument);
        if RSSFeed.Count > 0 then
          DisplayNews(RSSFeed)
        else
          DisplayNoNewsMsg;
        UpdateNewsAgeLbl;
      finally
        RSSFeed.Free;
      end;
    except
      on E: ECodeSnip do
      begin
        lblDays.Caption := '';
        DisplayErrorMsg(E);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TNewsDlg.UpdateNewsAgeLbl;
resourcestring
  // message displayed in label at
  sNewsDays = 'CodeSnip news from the last %d days.';
begin
  lblDays.Caption := Format(sNewsDays, [GetMaxNewsAge]);
end;

end.

