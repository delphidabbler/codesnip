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
  FmHTMLViewDlg, FrBrowserBase, FrRSSNews, UBaseObjects, UExceptions, URSS20,
  UXMLDocumentEx;


type

  {
  TNewsDlg:
    Dialog box that displays news items from CodeSnip's RSS news feed.
  }
  TNewsDlg = class(THTMLViewDlg, INoPublicConstruct)
    btnRSSFeed: TBitBtn;
    frmHTML: TRSSNewsFrame;
    lblDays: TLabel;
    pnlTop: TPanel;
    btnConfig: TButton;
    procedure btnRSSFeedClick(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
  strict private
    procedure LoadNews;
      {Loads news from RSS feed, converts to HTML and displays in browser
      control.
      }
    procedure DisplayMessage(const Msg: string);
      {Displays a brief message as an HTML paragraph in browser control.
        @param Msg [in] Message to display.
      }
    procedure DisplayLoadingMsg;
      {Displays a message in browser that informs that content is loading.
      }
    procedure DisplayNews(const RSS: TRSS20);
      {Renders the RSS news feed in HTML and displays in browser control.
        @param RSS [in] Object that provides access to RSS feed.
      }
    procedure DisplayNoNewsMsg;
      {Displays a message in browser control informing that there are no news
      items in RSS feed.
      }
    procedure DisplayErrorMsg(E: ECodeSnip);
      {Renders HTML describing a trapped exception and displays it in browser
      control.
        @param E [in] Exception whose message is to be displayed.
      }
    function GetRSSDocument: IXMLDocumentEx;
      {Gets XML document containing details of RSS news feed from web.
        @return Interface to required XML document.
      }
    function GetMaxNewsAge: Integer;
      {Gets maximum number of days of news to be displayed.
        @return Required number of days.
      }
    procedure UpdateNewsAgeLbl;
      {Updates maximum news age label with current value.
      }
  strict protected
    procedure ArrangeForm; override;
      {Arranges controls on form. Called from ancestor class.
      }
    procedure InitHTMLFrame; override;
      {Initialises HTML frame. Called from ancestor class.
      }
    procedure InitForm; override;
      {Initialises form's controls.
      }
    procedure AfterShowForm; override;
      {Override of method called from ancestor class after form is displayed.
      Loads news from RSS feed.
      }
  public
    class procedure Execute(AOwner: TComponent);
      {Displays dialog box.
        @param AOwner [in] Component that owns this dialog box.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, ExtActns,
  // Project
  FmPreferencesDlg, FrNewsPrefs, UCtrlArranger, UHTMLDetailUtils, UHTMLUtils,
  UPreferences, Web.UInfo, Web.UXMLRequestor;

{$R *.dfm}


{ TNewsDlg }

procedure TNewsDlg.AfterShowForm;
  {Override of method called from ancestor class after form is displayed. Loads
  news from RSS feed.
  }
begin
  LoadNews;
end;

procedure TNewsDlg.ArrangeForm;
  {Arranges controls on form. Called from ancestor class.
  }
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
  {Displays RSS feed in default web browser.
    @param Sender [in] Not used.
  }
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

procedure TNewsDlg.DisplayErrorMsg(E: ECodeSnip);
  {Renders HTML describing a trapped exception and displays it in browser
  control.
    @param E [in] Exception whose message is to be displayed.
  }
resourcestring
  sErrorHeading = 'Error loading news:';  // fixed heading text
var
  ErrHeadingAttrs: IHTMLAttributes; // HTML attributes of heading
  ErrMessageAttrs: IHTMLAttributes; // HTML attributes of error message
begin
  ErrHeadingAttrs := THTMLAttributes.Create;
  ErrHeadingAttrs.Add('class', 'error-heading');
  ErrMessageAttrs := THTMLAttributes.Create;
  ErrMessageAttrs.Add('class', 'error-message');
  frmHTML.DisplayContent(
    MakeCompoundTag('p', ErrHeadingAttrs, MakeSafeHTMLText(sErrorHeading)) +
    MakeCompoundTag('p', ErrMessageAttrs, MakeSafeHTMLText(E.Message))
  );
end;

procedure TNewsDlg.DisplayLoadingMsg;
  {Displays a message in browser that informs that content is loading.
  }
resourcestring
  sLoadingMsg = 'Loading...'; // message text
begin
  DisplayMessage(sLoadingMsg);
end;

procedure TNewsDlg.DisplayMessage(const Msg: string);
  {Displays a brief message as an HTML paragraph in browser control.
    @param Msg [in] Message to display.
  }
var
  HTMLAttrs: IHTMLAttributes; // HTML attributes
begin
  HTMLAttrs := THTMLAttributes.Create;
  HTMLAttrs.Add('class', 'message');
  frmHTML.DisplayContent(
    MakeCompoundTag('p', HTMLAttrs, MakeSafeHTMLText(Msg))
  );
end;

procedure TNewsDlg.DisplayNews(const RSS: TRSS20);
  {Renders the RSS news feed in HTML and displays in browser control.
    @param RSS [in] Object that provides access to RSS feed.
  }

  function TitleHTML(const Item: TRSS20Item): string;
    {Renders RSS news item title as HTML. Rendered as a link if item specifies
    a URL.
      @param Item [in] RSS item.
    }
  resourcestring
    sNoTitle = 'Untitled';  // text used when no title
  var
    Title: string;  // title text
    Link: string;   // item's URL used for link
  begin
    Title := Trim(Item.Title);
    if Title = '' then
      Title := sNoTitle;
    Link := Trim(Item.Link);
    if Link = '' then
      Result := MakeSafeHTMLText(Title)
    else
      Result := TextLink(Link, '', '', nil, Title);
    Result := MakeCompoundTag('strong', Result);
  end;

  function DescriptionHTML(const Item: TRSS20Item): string;
    {Renders RSS news item's desciption as HTML.
      @param Item [in] RSS item.
    }
  resourcestring
    sNoDescription = 'No description.'; // text used when no description
  var
    Description: string;  // description text
  begin
    Description := Trim(Item.Description);
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
          MakeCompoundTag(
            'div',
            MakeSafeHTMLText(DateTimeToStr(Item.PubDate))
          )
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
  {Displays a message in browser control informing that there are no news items
  in RSS feed.
  }
resourcestring
  sNoNews = 'There are no news items to display.';  // message text
begin
  DisplayMessage(sNoNews);
end;

class procedure TNewsDlg.Execute(AOwner: TComponent);
  {Displays dialog box.
    @param AOwner [in] Component that owns this dialog box.
  }
begin
  with InternalCreate(AOwner) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

function TNewsDlg.GetMaxNewsAge: Integer;
  {Gets maximum number of days of news to be displayed.
    @return Required number of days.
  }
begin
  Result := Preferences.NewsAge;
end;

function TNewsDlg.GetRSSDocument: IXMLDocumentEx;
  {Gets XML document containing details of RSS news feed from web.
    @return Interface to required XML document.
  }
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
  {Initialises form's controls.
  }
begin
  inherited;
  lblDays.Caption := '';
end;

procedure TNewsDlg.InitHTMLFrame;
  {Initialises HTML frame. Called from ancestor class.
  }
begin
  frmHTML.Initialise;
end;

procedure TNewsDlg.LoadNews;
  {Loads news from RSS feed, converts to HTML and displays in browser control.
  }
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

