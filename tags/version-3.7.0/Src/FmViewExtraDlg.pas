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
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
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
  Forms, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmHTMLViewDlg, FrBrowserBase, FrHTMLDlg, FrHTMLTpltDlg, UActiveText,
  UBaseObjects, UCSSBuilder, UHTMLEvents;


type
  {
  TViewExtraDlg:
    Dialog box that displays active text rendered from REML markup entered in
    snippets editor. Active text is rendered as HTML.
  }
  TViewExtraDlg = class(THTMLViewDlg, INoPublicConstruct)
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
    procedure InitHTMLFrame; override;
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
  UActiveTextHTML, UAnchors, UColours, UConsts, UCSSUtils, UMessageBox,
  UProtocols, USystemInfo;

{$R *.dfm}

{ TViewExtraDlg }

procedure TViewExtraDlg.ArrangeForm;
  {Sizes dialog box to fit content.
  }
begin
  pnlBody.Height := frmExtraInfo.DocHeight;
  inherited;
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
  case EventInfo.DispatchId of
    cDocEventOnClick:
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
end;

procedure TViewExtraDlg.InitHTMLFrame;
  {Initialises HTML frame, loads HTML template and inserts HTML
  representation of Extra Text REML.
  }
var
  Values: TStringList;  // values to insert in HTML template
begin
  Values := TStringList.Create;
  try
    frmExtraInfo.OnBuildCSS := UpdateCSS;
    frmExtraInfo.OnHTMLEvent := HTMLEventHandler;
    Values.Values['Content'] := TActiveTextHTML.Render(fActiveText);
    frmExtraInfo.Initialise('dlg-viewextra-tplt.html', Values);
  finally
    FreeAndNil(Values);
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
begin
  // Set rendered REML container
  with CSSBuilder.AddSelector('#content') do
  begin
    AddProperty(CSSBackgroundColorProp(clWindow));
    AddProperty(CSSPaddingProp(0, 6, 6, 6));
    AddProperty(CSSMarginProp(cssTop, 6));
    AddProperty(CSSBorderProp(cssAll, 1, cbsSolid, clBorder));
    AddProperty(CSSOverflowProp(covAuto));
    AddProperty(CSSWidthProp(cltAuto, 0));
    // Use height instead of maxheight if IE 6 or lower
    if TOSInfo.BrowserVer > 6 then
      AddProperty(CSSMaxHeightProp(cMaxExtraHTMLHeight))
    else
      AddProperty(CSSHeightProp(cMaxExtraHTMLHeight));
  end;
  // Show or hide text about links depending on if links in Extra HTML
  with CSSBuilder.AddSelector('#linktext') do
  begin
    if ExtraContainsLinks then
      AddProperty(CSSDisplayProp(cdsInline))
    else
      AddProperty(CSSDisplayProp(cdsNone));
  end;
  // Style the REML itself
  TActiveTextHTML.Styles(Font, CSSBuilder);
end;

end.

