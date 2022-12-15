{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame used to display previews of HTML documents.
}


unit FrHTMLPreview;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls, Menus,
  // Project
  FrBrowserBase, IntfFrameMgrs, IntfPreview, UCSSBuilder, UEncodings;


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
  public
    { IPreview }
    procedure Display(const DocContent: TEncodedData);
      {Displays document in preview dialog box.
        @param DocContent [in] Content of document to be displayed.
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
  UCSSUtils;


{$R *.dfm}


{ THTMLPreviewFrame }

procedure THTMLPreviewFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Adds body margin to default style sheet.
    @param CSSBuilder [in] Object used to construct the CSS.
  }
begin
  inherited;
  CSSBuilder.AddSelector('body')
    .AddProperty(TCSS.MarginProp(cPreviewMargin));
end;

procedure THTMLPreviewFrame.Display(const DocContent: TEncodedData);
  {Displays document in preview dialog box.
    @param DocContent [in] Content of document to be displayed.
  }
begin
  WBController.IOMgr.LoadFromString(DocContent.ToString);
end;

procedure THTMLPreviewFrame.SetPopupMenu(const Menu: TPopupMenu);
  {Sets pop up menu to be displayed when browser control is right clicked.
    @param Menu [in] Required popup menu.
  }
begin
  WBController.UIMgr.PopupMenu := Menu;
end;

end.

