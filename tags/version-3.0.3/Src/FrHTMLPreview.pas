{
 * FrHTMLPreview.pas
 *
 * Frame used to display previews of HTML documents.
 *
 * v0.1 of 17 Feb 2005  - Original version.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 07 Nov 2006  - Changed to use TCSSBuilder to build body CSS rather
 *                        than using a hard-wired literal string.
 * v1.2 of 13 May 2007  - Changed to use an 8px rather than 1px margin (old
 *                        BodyCSS method was replaced by BuildCSS override since
 *                        BodyCSS failed to set old 1px margin).
 *                      - Removed redundant constructor.
 *                      - Added support for selection and copying and assigning
 *                        pop-up menu.
 * v1.3 of 05 Nov 2007  - Changed to use revised CSS builder classes.
 * v1.4 of 17 Jun 2009  - Changed to implement revised IPreview.Display method
 *                        that gets HTML document title.
 *                      - Made a protected section strict.
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
 * The Original Code is FrHTMLPreview.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FrHTMLPreview;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls, Menus,
  // Project
  FrBrowserBase, IntfFrameMgrs, IntfPreview, UCSSBuilder;


type

  {
  THTMLPreviewFrame:
    Frame used to display previews of HTML documents.
  }
  THTMLPreviewFrame = class(TBrowserBaseFrame,
    IPreview, IClipboardMgr, ISelectionMgr
  )
  strict protected
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
      {Adds body margin to default style sheet.
        @param CSSBuilder [in] Object used to construct the CSS.
      }
  protected // do not make strict
    { IPreview }
    procedure Display(const DocContent: string; out Title: string);
      {Displays document in preview dialog box.
        @param DocContent [in] Content of document to be displayed.
        @param Title [out] Title of document, if any.
      }
    procedure SetPopupMenu(const Menu: TPopupMenu);
      {Sets pop up menu to be displayed when browser control is right clicked.
        @param Menu [in] Required popup menu.
      }
    { IClipboardMgr: Implemented in base class }
    { ISelectionMgr: Implemented in base class }
  end;


implementation


uses
  // Project
  UCSSUtils, UHTMLDocHelper;


{$R *.dfm}


{ THTMLPreviewFrame }

procedure THTMLPreviewFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Adds body margin to default style sheet.
    @param CSSBuilder [in] Object used to construct the CSS.
  }
begin
  inherited;
  with CSSBuilder.AddSelector('body') do
    AddProperty(CSSMarginProp(8));
end;

procedure THTMLPreviewFrame.Display(const DocContent: string;
  out Title: string);
  {Displays document in preview dialog box.
    @param DocContent [in] Content of document to be displayed.
    @param Title [out] Title of document, if any.
  }
begin
  WBController.IOMgr.LoadFromString(DocContent);  
  Title := THTMLDocHelper.GetDocTitle(wbBrowser.Document);
end;

procedure THTMLPreviewFrame.SetPopupMenu(const Menu: TPopupMenu);
  {Sets pop up menu to be displayed when browser control is right clicked.
    @param Menu [in] Required popup menu.
  }
begin
  WBController.UIMgr.PopupMenu := Menu;
end;

end.

