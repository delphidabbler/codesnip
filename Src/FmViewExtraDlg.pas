{
 * FmViewExtraDlg.pas
 *
 * Implements a dialog box that displays active text rendered from REML markup
 * entered in snippets editor. Active text is rendered as HTML. Purpose is to
 * allow users to test the extra information markup before saving. Can also
 * display urls of any links and allow them to be displayed.
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
 * The Original Code is FmViewExtraDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmViewExtraDlg;


interface


uses
  // Delphi
  Classes, Forms, StdCtrls, Controls, ExtCtrls,
  // Project
  Browser.UHTMLEvents, FmGenericViewDlg, FrBrowserBase, FrHTMLDlg,
  FrHTMLTpltDlg, UActiveText, UBaseObjects, UCSSBuilder;


type
  {
  TViewExtraDlg:
    Dialog box that displays active text rendered from REML markup entered in
    snippets editor. Active text is rendered as HTML.
  }
  TViewExtraDlg = class(TGenericViewDlg, INoPublicConstruct)
    frmExtraInfo: THTMLTpltDlgFrame;
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
    function ExtraContainsLinks: Boolean;
      {Checks if the "extra" active text contains any link (<a>) tags.
        @return True if links found, False if not.
      }
  strict protected
    procedure ArrangeForm; override;
      {Sizes dialog box to fit content.
      }
    procedure ConfigForm; override;
      {Initialises HTML frame, loads HTML template and inserts HTML
      representation of Extra Text REML.
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
  UActiveTextHTML, UAnchors, UColours, UConsts, UCSSUtils, UFontHelper,
  UHTMLTemplate, UMessageBox, UProtocols, USystemInfo;

{$R *.dfm}

{ TViewExtraDlg }

procedure TViewExtraDlg.ArrangeForm;
  {Sizes dialog box to fit content.
  }
begin
  pnlBody.Height := frmExtraInfo.DocHeight;
  inherited;
end;

procedure TViewExtraDlg.ConfigForm;
  {Initialises HTML frame, loads HTML template and inserts HTML representation
  of Extra Text REML.
  }
var
  Renderer: TActiveTextHTML;
begin
  inherited;
  frmExtraInfo.OnBuildCSS := UpdateCSS;
  frmExtraInfo.OnHTMLEvent := HTMLEventHandler;

  Renderer := TActiveTextHTML.Create;
  try
    frmExtraInfo.Initialise(
      'dlg-viewextra-tplt.html',
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

class procedure TViewExtraDlg.Execute(const AOwner: TComponent;
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

function TViewExtraDlg.ExtraContainsLinks: Boolean;
  {Checks if the "extra" active text contains any link (<a>) tags.
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

procedure TViewExtraDlg.HTMLEventHandler(Sender: TObject;
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
  sViewLink = 'View Link';
  // Text to display in choice dialog box
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
      ]
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

procedure TViewExtraDlg.UpdateCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
  {Modifies CSS used to display dialog box body to achieve required appearance.
    @param Sender [in] Not used.
    @param CSSBuilder [in] Object used to modify CSS.
  }
const
  cMaxExtraHTMLHeight = 240;  // max height of Extra HTML
var
  ContentFont: TFont;             // font used for #content tab
begin
  ContentFont := TFont.Create;
  try
    TFontHelper.SetContentFont(ContentFont, True);
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
      if TOSInfo.BrowserVer > 6 then
        AddProperty(TCSS.MaxHeightProp(cMaxExtraHTMLHeight))
      else
        AddProperty(TCSS.HeightProp(cMaxExtraHTMLHeight));
    end;
    with CSSBuilder.AddSelector('.active-text h2') do
    begin
      AddProperty(TCSS.MarginProp(4, 0, 0, 0));
      AddProperty(TCSS.FontWeightProp(cfwBold));
      AddProperty(TCSS.FontSizeProp(ContentFont.Size + 1));
    end;
    with CSSBuilder.AddSelector('.active-text p') do
      AddProperty(TCSS.MarginProp(4, 0, 0, 0));
    // Show or hide text about links depending on if links in Extra HTML
    with CSSBuilder.AddSelector('#linktext') do
    begin
      if ExtraContainsLinks then
        AddProperty(TCSS.DisplayProp(cdsInline))
      else
        AddProperty(TCSS.DisplayProp(cdsNone));
    end;
  finally
    ContentFont.Free;
  end;
end;

end.

