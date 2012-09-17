{
 * FrHTMLPreview.pas
 *
 * Frame used to display previews of HTML documents.
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
 * The Original Code is FrHTMLPreview.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
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
    AddProperty(CSSMarginProp(cPreviewMargin));
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

