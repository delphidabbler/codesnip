{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2023, Peter Johnson (gravatar.com/delphidabbler).
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
var
  Dlg: TActiveTextPreviewDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Dlg.fActiveText := ActiveText;
    Dlg.ShowModal;
  finally
    Dlg.Free;
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
  ProtocolHander: TProtocol;
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
      ProtocolHander := TProtocolFactory.CreateHandler(TAnchors.GetURL(ALink));
      try
        ProtocolHander.Execute;
      finally
        ProtocolHander.Free;
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
  ContentFont: TFont;     // font used for #content tab
begin
  ContentFont := TFont.Create;
  try
    TFontHelper.SetContentFont(ContentFont);
    // Set rendered REML container
    CSSBuilder.EnsureSelector('#content')
      .AddProperty(TCSS.FontProps(ContentFont))
      .AddProperty(TCSS.BackgroundColorProp(clWindow))
      .AddProperty(TCSS.PaddingProp(0, 6, 6, 6))
      .AddProperty(TCSS.MarginProp(cssTop, 6))
      .AddProperty(TCSS.BorderProp(cssAll, 1, cbsSolid, clBorder))
      .AddProperty(TCSS.OverflowProp(covAuto))
      .AddProperty(TCSS.WidthProp(cluAuto, 0))
      .AddPropertyIf(
        TIEInfo.SupportsCSSMaxHeight,
        TCSS.MaxHeightProp(MaxHTMLHeight),
        TCSS.HeightProp(MaxHTMLHeight)
      );
    CSSBuilder.EnsureSelector('.active-text h2')
      .AddProperty(TCSS.MarginProp(4, 0, 0, 0))
      .AddProperty(TCSS.FontWeightProp(cfwBold))
      .AddProperty(TCSS.FontSizeProp(ContentFont.Size + 1));
    CSSBuilder.EnsureSelector('.active-text p')
      .AddProperty(TCSS.MarginProp(4, 0, 0, 0));
    // Show or hide text about links depending on if links are present
    CSSBuilder.EnsureSelector('#linktext')
      .AddPropertyIf(
        ContainsLinks, TCSS.DisplayProp(cdsInline), TCSS.DisplayProp(cdsNone)
      );
    // Set up lists
    CSSBuilder.EnsureSelector('.active-text ul')
      .AddProperty(TCSS.MarginProp(cssAll, 0))
      .AddProperty(TCSS.MarginProp(cssTop, 4))
      .AddProperty(TCSS.PaddingProp(cssAll, 0))
      .AddProperty(TCSS.PaddingProp(cssLeft, 24))
      .AddProperty(TCSS.ListStylePositionProp(clspOutside))
      .AddProperty(TCSS.ListStyleTypeProp(clstDisc));
    CSSBuilder.EnsureSelector('.active-text ol')
      .AddProperty(TCSS.MarginProp(cssAll, 0))
      .AddProperty(TCSS.MarginProp(cssTop, 4))
      .AddProperty(TCSS.PaddingProp(cssAll, 0))
      .AddProperty(TCSS.PaddingProp(cssLeft, 32))
      .AddProperty(TCSS.ListStylePositionProp(clspOutside))
      .AddProperty(TCSS.ListStyleTypeProp(clstDecimal));
    CSSBuilder.EnsureSelector('.active-text li')
      .AddProperty(TCSS.PaddingProp(cssAll, 0))
      .AddProperty(TCSS.MarginProp(cssAll, 0));
    CSSBuilder.EnsureSelector('.active-text li ol')
      .AddProperty(TCSS.MarginProp(cssTop, 0));
    CSSBuilder.EnsureSelector('.active-text li ul')
      .AddProperty(TCSS.MarginProp(cssTop, 0));
    CSSBuilder.EnsureSelector('.active-text ul li')
      .AddProperty(TCSS.PaddingProp(cssLeft, 8));
    CSSBuilder.EnsureSelector('.active-text ul li ol li')
      .AddProperty(TCSS.PaddingProp(cssLeft, 0));
  finally
    ContentFont.Free;
  end;
end;

end.

