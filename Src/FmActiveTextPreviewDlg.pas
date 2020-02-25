{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that displays active text rendered from REML markup
 * or plain text.
}


unit FmActiveTextPreviewDlg;


interface


uses
  // Delphi
  Classes, Forms, StdCtrls, Controls, ExtCtrls,
  // Project
  ActiveText.UMain, Browser.UHTMLEvents, FmGenericViewDlg, FrBrowserBase,
  FrHTMLDlg, FrHTMLTpltDlg, UBaseObjects, UCSSBuilder;


type
  {
  TActiveTextPreviewDlg:
    Dialog box that displays active text rendered from REML markup or plain
    text.
    Active text is rendered as HTML. Link URLs can be viewed and tested.
  }
  TActiveTextPreviewDlg = class(TGenericViewDlg, INoPublicConstruct)
    frmPreview: THTMLTpltDlgFrame;
  strict private
    fActiveText: IActiveText; // Active text to be displayed.
    procedure UpdateCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
      {Modifies CSS used to display dialog box body to achieve required
      appearance.
        @param Sender [in] Not used.
        @param CSSBuilder [in] Object used to modify CSS.
      }
    procedure HTMLEventHandler(Sender: TObject;
      const EventInfo: THTMLEventInfo);
      {Handles HTML frame's OnHTMLEvent event. Cancels any clicks on links and
      handles these by displaying dialog and acting on user's response.
        @param Sender [in] Not used.
        @param EventInfo [in] Object providing information about the event.
      }
    function ContainsLinks: Boolean;
      {Checks if the active text contains any links.
        @return True if links found, False if not.
      }
  strict protected
    procedure ArrangeForm; override;
      {Sizes dialogue box to fit content.
      }
    procedure ConfigForm; override;
      {Initialises HTML frame, loads HTML template and inserts HTML
      representation of the active text.
      }
  public
    class procedure Execute(const AOwner: TComponent;
      const ActiveText: IActiveText);
      {Displays dialog box to display HTML representation of some active text.
        @param AOwner [in] Component that owns this dialog box.
        @param ActiveText [in] Active text to be displayed as HTML.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  ActiveText.UHTMLRenderer, UAnchors, UColours, UConsts, UCSSUtils, UFontHelper,
  UHTMLTemplate, UMessageBox, UProtocols, USystemInfo;

{$R *.dfm}

{ TActiveTextPreviewDlg }

procedure TActiveTextPreviewDlg.ArrangeForm;
  {Sizes dialog box to fit content.
  }
begin
  pnlBody.Height := frmPreview.DocHeight;
  inherited;
end;

procedure TActiveTextPreviewDlg.ConfigForm;
  {Initialises HTML frame, loads HTML template and inserts HTML representation
  of the active text.
  }
var
  Renderer: TActiveTextHTML;
begin
  inherited;
  frmPreview.OnBuildCSS := UpdateCSS;
  frmPreview.OnHTMLEvent := HTMLEventHandler;

  Renderer := TActiveTextHTML.Create;
  try
    frmPreview.Initialise(
      'dlg-activetext-preview-tplt.html',
      procedure(Tplt: THTMLTemplate)
      begin
        Tplt.ResolvePlaceholderHTML(
          'Content', Renderer.Render(fActiveText)
        );
      end
    );
  finally
    Renderer.Free;
  end;
end;

function TActiveTextPreviewDlg.ContainsLinks: Boolean;
  {Checks if the active text contains any links.
    @return True if links found, False if not.
  }
var
  Elem: IActiveTextElem;              // each element in active text
  ActionElem: IActiveTextActionElem;  // ref to am action element
begin
  Result := False;
  for Elem in fActiveText do
    if Supports(Elem, IActiveTextActionElem, ActionElem) and
      (ActionElem.Kind = ekLink) then
    begin
      Result := True;
      Exit;
    end;
end;

class procedure TActiveTextPreviewDlg.Execute(const AOwner: TComponent;
  const ActiveText: IActiveText);
  {Displays dialog box to display HTML representation of some active text.
    @param AOwner [in] Component that owns this dialog box.
    @param ActiveText [in] Active text to be displayed as HTML.
  }
begin
  with InternalCreate(AOwner) do
    try
      fActiveText := ActiveText;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TActiveTextPreviewDlg.HTMLEventHandler(Sender: TObject;
  const EventInfo: THTMLEventInfo);
  {Handles HTML frame's OnHTMLEvent event. Cancels any clicks on links and
  handles these by displaying dialog and acting on user's response.
    @param Sender [in] Not used.
    @param EventInfo [in] Object providing information about the event.
  }
var
  ALink: IDispatch;   // reference to the any link that was clicked
resourcestring
  // Button captions for choice dialog box
  sClose = 'Close';
  sViewLink = '&View Link';
  // Title & text to display in choice dialog box
  sDlgTitle = 'Link Information';
  sDlgText = 'Link URL:' + EOL2 + '%s';
const
  cCloseRes = mrCancel; // modal result of choice dialog's close button
  cViewLinkRes = $FF;   // modal result of choice dialog's view button
begin
  if EventInfo.IsEvent(
    THTMLDocumentEvents2Sink.EventIntf, THTMLDocumentEvents2Sink.DISPID_OnClick
  ) then
  begin
    // Mouse click: check for click on a link and handle it
    ALink := TAnchors.FindEnclosingAnchor(EventInfo.Args.srcElement);
    if not Assigned(ALink) then
      Exit;
    // Cancel event: no further action needed
    EventInfo.Cancelled := True;
    // Give user info about link and option to display it
    if TMessageBox.Custom(
      Self,
      Format(sDlgText, [TAnchors.GetURL(ALink)]),
      [
        TMessageBoxButton.Create(sClose, cCloseRes, True, True),
        TMessageBoxButton.Create(sViewLink, cViewLinkRes)
      ],
      sDlgTitle
    ) = cViewLinkRes then
    begin
      // User wants to view link: use protocol handler to display it
      with TProtocolFactory.CreateHandler(TAnchors.GetURL(ALink)) do
        try
          Execute;
        finally
          Free;
        end;
    end;
  end;
end;

procedure TActiveTextPreviewDlg.UpdateCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
  {Modifies CSS used to display dialog box body to achieve required appearance.
    @param Sender [in] Not used.
    @param CSSBuilder [in] Object used to modify CSS.
  }
const
  MaxHTMLHeight = 240;  // max height of rendered HTML
var
  ContentFont: TFont;   // font used for #content tab
begin
  ContentFont := TFont.Create;
  try
    TFontHelper.SetContentFont(ContentFont);
    // Set rendered REML container
    with CSSBuilder.AddSelector('#content') do
    begin
      AddProperty(TCSS.FontProps(ContentFont));
      AddProperty(TCSS.BackgroundColorProp(clWindow));
      AddProperty(TCSS.PaddingProp(0, 6, 6, 6));
      AddProperty(TCSS.MarginProp(cssTop, 6));
      AddProperty(TCSS.BorderProp(cssAll, 1, cbsSolid, clBorder));
      AddProperty(TCSS.OverflowProp(covAuto));
      AddProperty(TCSS.WidthProp(cluAuto, 0));
      // Use height instead of maxheight if IE 6 or lower
      if TIEInfo.SupportsCSSMaxHeight then
        AddProperty(TCSS.MaxHeightProp(MaxHTMLHeight))
      else
        AddProperty(TCSS.HeightProp(MaxHTMLHeight));
    end;
    with CSSBuilder.AddSelector('.active-text h2') do
    begin
      AddProperty(TCSS.MarginProp(4, 0, 0, 0));
      AddProperty(TCSS.FontWeightProp(cfwBold));
      AddProperty(TCSS.FontSizeProp(ContentFont.Size + 1));
    end;
    with CSSBuilder.AddSelector('.active-text p') do
      AddProperty(TCSS.MarginProp(4, 0, 0, 0));
    // Show or hide text about links depending on if links are present
    with CSSBuilder.AddSelector('#linktext') do
    begin
      if ContainsLinks then
        AddProperty(TCSS.DisplayProp(cdsInline))
      else
        AddProperty(TCSS.DisplayProp(cdsNone));
    end;
  finally
    ContentFont.Free;
  end;
end;

end.

