{
 * FrCompCheck.pas
 *
 * Frame that displays and manages user interaction with the compiler check
 * pane.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 18 Feb 2005  - Deleted unused units from uses clauses.
 * v0.3 of 19 Feb 2005  - Refactoring: changed frame to implement the renamed
 *                        IFrameActionMgr and ICompCheckActionMgr interfaces.
 * v0.4 of 21 Feb 2005  - Significantly revised method to generating and
 *                        displaying HTML pages by using new TDetailPageLoader
 *                        classes.
 * v0.5 of 22 Feb 2005  - Modified to use new browser external object extender.
 *                      - A single extender object now supplied to frame by
 *                        calling its new ISetWBExternal interface) rather than
 *                        frame owning its own custom extender object.
 *                      - All support for IFrameActionMgr interfaces deleted
 *                        since the new extender object handles notification of
 *                        user-initiated events to the application.
 * v0.6 of 23 Feb 2005  - Significantly revised to descend from other frames
 *                        that implement functionality that is common to all
 *                        detail view frames. This frame now only implements
 *                        functions specific to the compiler check frame.
 * v0.7 of 28 Jan 2006  - Added new parameter to TCompCheckFrame.Display to
 *                        force redisplay of a view item even if it is already
 *                        displayed. This was done to enable the pane to adjust
 *                        to updates in number of available compilers.
 * v0.8 of 16 Apr 2006  - Removed ISetWBExternal interface and replaced with
 *                        new IWBCustomiser interface to perform web browser
 *                        customisation. IWBCustomiser methods implemented in an
 *                        ancestor class.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 24 Nov 2006  - Added support for ICompCheckDisplayMgr's new
 *                        TestUnitVisible property accessors. This code shows
 *                        and hides test units when routines are displayed.
 * v1.2 of 26 Nov 2006  - Changed JavaScript call used to toggle visibility of
 *                        test unit. Required routine renamed from ShowSrc() to
 *                        showTestUnit().
 * v1.3 of 02 Dec 2006  - Changed to use only a single HTML document that is
 *                        updated dynamically to show test compilation results.
 *                        Uses a DHTML object to perform these updates.
 *                      - The routine HTML page is now only reloaded if previous
 *                        page wasn't a routine, otherwise details are displayed
 *                        dynamically by DHTML object.
 *                      - Replaced hard-wired code that toggles test unit
 *                        visibility with a call into DHTML object.
 *                      - Added code to set CSS specific to compiler check pane.
 *                      - Added implementation of IDHTMLHostInfo and
 *                        ICompCheckRoutineHostInfo interfaces to provide
 *                        information about the frame to DHTML object.
 * v1.4 of 03 Dec 2006  - Changed to use implementations of IDHTMLHostInfo and
 *                        new IDetailViewHostInfo from base class.
 *                      - Removed DHTML handling code to base class.
 *                      - Removed DisplayCurViewItem to base class.
 *                      - Added GetPageKind method override to return page kind
 *                        to base class to enable it to display correct view.
 * v1.5 of 04 Feb 2007  - Replaced redundant TDetailView class references with
 *                        TViewItem.
 * v1.6 of 16 Feb 2007  - Revised to work with redefined display interfaces from
 *                        IntfFrameMgrs.
 *                      - Pushed Display method back into base class.
 *                      - Prevented ShowTestUnit method from setting test unit
 *                        visibility flag.
 * v1.7 of 04 Mar 2007  - Fixed bug that was causing showing and hiding of test
 *                        unit to get out of sync with main menu.
 *                      - Renamed and removed unecessary parameter from method
 *                        that updates visibility of test unit.
 * v1.8 of 05 Nov 2007  - Changed to use revised CSS builder classes.
 * v1.9 of 09 Jan 2009  - Changed DisplayCompileResults to take compilers object
 *                        containing results as a parameter.
 * v1.10 of 19 Jan 2009 - Changed to support interfaces from IntfHTMLDocHostInfo
 *                        to replace those deleted from UDHTML.
 *                      - Methods that need to dynamically update document in
 *                        browser control now construct an IDHTML object
 *                        explicitly now that DHTML property delected.
 *                      - Made private and protected sections strict.
 * v1.11 of 16 May 2009 - Removed frame's IDetailViewHostInfo and
 *                        ICompCheckRoutineHostInfo support.
 *                      - Stripped out code that supported showing/hiding of
 *                        test unit in compiler check pane.
 *
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
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FrCompCheck;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  FrDetailView, IntfFrameMgrs, IntfCompilers, IntfHTMLDocHostInfo, UCSSBuilder,
  UDetailPageLoader;


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

