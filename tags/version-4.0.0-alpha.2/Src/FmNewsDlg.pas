{
 * FmNewsDlg.pas
 *
 * Implements a dialog box that displays news items from CodeSnip's RSS news
 * feed.
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
 * The Original Code is FmNewsDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmNewsDlg;


interface


uses
  // Delphi
  Buttons, StdCtrls, Forms, Controls, ExtCtrls, Classes,
  // Project
  FmGenericViewDlg, FrBrowserBase, FrRSSNews, UBaseObjects, UExceptions, URSS20,
  UXMLDocumentEx;


type

  {
  TNewsDlg:
    Dialog box that displays news items from CodeSnip's RSS news feed.
  }
  TNewsDlg = class(TGenericViewDlg, INoPublicConstruct)
    btnRSSFeed: TBitBtn;
    frmHTML: TRSSNewsFrame;
    lblDays: TLabel;
    pnlTop: TPanel;
    btnConfig: TButton;
    ///  <summary>RSS Feed button click handler. Displays RSS feed in default
    ///  web browser.</summary>
    procedure btnRSSFeedClick(Sender: TObject);
    ///  <summary>Change button click handler. Displays news prefs page of
    ///  Preferences dialog box to enable number of days to display in news
    ///  feed to be changed.</summary>
    procedure btnConfigClick(Sender: TObject);
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
    ///  <summary>Loads new from RSS feed after form is displayed.</summary>
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
  SysUtils, ExtActns,
  // Project
  FmPreferencesDlg, FrNewsPrefs, UCtrlArranger, UHTMLDetailUtils, UHTMLUtils,
  UPreferences, UStrUtils, Web.UInfo, Web.UXMLRequestor;

{$R *.dfm}


{ TNewsDlg }

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

procedure TNewsDlg.btnConfigClick(Sender: TObject);
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

procedure TNewsDlg.btnRSSFeedClick(Sender: TObject);
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
    MakeCompoundTag('p', ErrHeadingAttrs, MakeSafeHTMLText(sErrorHeading)) +
    MakeCompoundTag('p', ErrMessageAttrs, MakeSafeHTMLText(E.Message))
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
    MakeCompoundTag('p', HTMLAttrs, MakeSafeHTMLText(Msg))
  );
end;

procedure TNewsDlg.DisplayNews(const RSS: TRSS20);

  ///  Renders the given RSS news item title as HTML. Rendered as a link if item
  ///  specifies a URL.
  function TitleHTML(const Item: TRSS20Item): string;
  resourcestring
    sNoTitle = 'Untitled';  // text used when no title
  var
    Title: string;  // title text
    Link: string;   // item's URL used for link
  begin
    Title := StrTrim(Item.Title);
    if Title = '' then
      Title := sNoTitle;
    Link := StrTrim(Item.Link);
    if Link = '' then
      Result := MakeSafeHTMLText(Title)
    else
      Result := TextLink(Link, '', '', nil, Title);
    Result := MakeCompoundTag('strong', Result);
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
    Result := MakeSafeHTMLText(Description);
  end;

var
  SB: TStringBuilder; // object used to construct HTML
  Item: TRSS20Item;   // references each RSS item in feed
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine(MakeTag('dl', ttOpen));
    for Item in RSS do
    begin
      SB.AppendLine(MakeTag('dt', ttOpen));
      SB.AppendLine(MakeCompoundTag('div', TitleHTML(Item)));
      if Item.PubDateAsText <> '' then
        SB.AppendLine(
          MakeCompoundTag('div', MakeSafeHTMLText(DateTimeToStr(Item.PubDate)))
        );
      SB.AppendLine(MakeTag('dt', ttClose));
      SB.AppendLine(MakeCompoundTag('dd', DescriptionHTML(Item)));
    end;
    SB.AppendLine(MakeTag('dl', ttClose));
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
