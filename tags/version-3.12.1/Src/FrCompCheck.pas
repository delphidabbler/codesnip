{
 * FrCompCheck.pas
 *
 * Frame that displays and manages user interaction with the compiler check
 * pane.
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
 * The Original Code is FrCompCheck.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrCompCheck;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  Compilers.UGlobals, FrDetailView,IntfFrameMgrs, IntfHTMLDocHostInfo,
  UCSSBuilder, UDetailPageLoader;


type

  {
  TCompCheckFrame:
    Frame that displays and manages user interaction with the compiler check
    pane. It implements display manager, clipboard manager and selection manager
    interfaces. It also manages the contained web browser control and provides
    an extension to the browser's "external" object via its ISetWBExternal
    interface.
  }
  TCompCheckFrame = class(TDetailViewFrame,
    IWBDisplayMgr,                         // support for hosted browser control
    IViewItemDisplayMgr,                                 // displays a view item
    IPaneInfo,                                // provides information about pane
    ICompCheckDisplayMgr,              // displays compile results and test unit
    IWBCustomiser,                             // customises web browser control
    IClipboardMgr,                                          // clipboard manager
    ISelectionMgr,                                          // selection manager
    IHTMLDocHostInfo                        // info for use in HTML manipulation
  )
  protected // do not make strict
    { IWBDisplayMgr: Implemented in ancestor class }
    { IViewItemDisplayMgr: Implemented in ancestor class }
    { IPaneInfo: Implemented in ancestor class }
    { ICompCheckDisplayMgr }
    procedure DisplayCompileResults(const ACompilers: ICompilers);
      {Displays results of test compilation in pane.
        @param ACompilers [in] Compilers object containing required results.
      }
    { IWBCustomiser: Implemented in ancestor class }
    { IClipboardMgr: Implemented in ancestor class }
    { ISelectionMgr: Implemented in ancestor class }
    { IHTMLDocHostInfo: Implemented in ancestor class }
  strict protected
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
      {Generates CSS classes specific to HTML displayed in compiler check pane.
      This CSS is added to that provided by parent class.
        @param CSSBuilder [in] Object used to build the CSS code.
      }
    function GetPageKind: TDetailPageKind; override;
      {Gets page kind and provides to base class.
        @return Page kind.
      }
  end;


implementation


uses
  // Delphi
  Graphics,
  // Project
  UColours, UCSSUtils, UDHTML;


{$R *.dfm}


{ TCompCheckFrame }

procedure TCompCheckFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Generates CSS classes specific to HTML displayed in compiler check pane. This
  CSS is added to that provided by parent class.
    @param CSSBuilder [in] Object used to build the CSS code.
  }
begin
  // NOTE:
  // We only set CSS properties that may need to use system colours or fonts
  // that may be changed by user or changing program defaults. CSS that controls
  // layout remains in a CSS file embedded in resources.
  inherited;
  // Add CSS relating to compiler table
  with CSSBuilder.AddSelector('.testcomp th, .testcomp td') do
  begin
    AddProperty(CSSBorderProp(cssBottom, 1, cbsSolid, clBorder));
    AddProperty(CSSBorderProp(cssRight, 1, cbsSolid, clBorder));
  end;
  with CSSBuilder.AddSelector('.testcomp th') do
  begin
    AddProperty(CSSBackgroundColorProp(clWindow));
    AddProperty(CSSFontWeightProp(cfwNormal));
  end;
  with CSSBuilder.AddSelector(
    '.testcomp th.compiler, .testcomp th.dbres, .testcomp th.testres, '
    + '.testcomp th.compres'
  ) do
    AddProperty(CSSBackgroundColorProp(clCompTblHeadBg));
  with CSSBuilder.AddSelector('.testcomp .testerr') do
    AddProperty(CSSBorderProp(cssAll, 0, cbsNone, clNone));
  with CSSBuilder.AddSelector('.testcomp .compiler') do
    AddProperty(CSSBorderProp(cssLeft, 1, cbsSolid, clBorder));
  with CSSBuilder.AddSelector('.testcomp th.compiler') do
    AddProperty(CSSBorderProp(cssTop, 1, cbsSolid, clBorder));
  with CSSBuilder.AddSelector('.testcomp th.compres') do
    AddProperty(CSSBorderProp(cssTop, 1, cbsSolid, clBorder));
  with CSSBuilder.AddSelector('.testcomp .disabled') do
    AddProperty(CSSColorProp(clGrayText));
end;

procedure TCompCheckFrame.DisplayCompileResults(const ACompilers: ICompilers);
  {Displays results of test compilation in pane.
    @param ACompilers [in] Compilers object containing required results.
  }
var
  CompCheckDHTML: ICompCheckRoutineDHTML; // object manipulates comp check pane
begin
  CompCheckDHTML := TDHTMLFactory.CreateCompCheckRoutineDHTML(Self);
  CompCheckDHTML.DisplayCompileResults(ACompilers);
end;

function TCompCheckFrame.GetPageKind: TDetailPageKind;
  {Gets page kind and provides to base class.
    @return Page kind.
  }
begin
  Result := pkComp;
end;

end.

