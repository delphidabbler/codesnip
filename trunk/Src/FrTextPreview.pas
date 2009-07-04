{
 * FrTextPreview.pas
 *
 * Frame used to display previews of plain text documents, including source
 * code.
 *
 * v0.1 of 17 Mar 2005  - Original version.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v2.0 of 13 May 2007  - Revised to descend from TMemoPreviewFrame. Parent
 *                        class sets margin and provides selection and clipboard
 *                        copy behaviour.
 *                      - Revised to support pop-up menu via extended IPreview
 *                        interface.
 * v2.1 of 17 Jun 2009  - Added new GetTitle override method
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
 * The Original Code is FrTextPreview.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FrTextPreview;


interface


uses
  // Delphi
  StdCtrls, Classes, Controls, ExtCtrls, Menus,
  // Project
  FrMemoPreview, IntfFrameMgrs, IntfPreview;


type

  {
  TTextPreviewFrame:
    Frame used to display previews of plain text documents, including source
    code.
  }
  TTextPreviewFrame = class(TMemoPreviewFrame,
    IPreview, IClipboardMgr, ISelectionMgr
  )
    edDisplay: TMemo;
  strict protected
    { Implementation of base class' abstract methods }
    function GetMemoCtrl: TCustomMemo; override;
      {Gets reference to memo control used to display plain text.
        @return Required TMemo reference.
      }
    procedure LoadContent(const DocContent: string); override;
      {Loads document into memo control.
        @param DocContent [in] Plain text document to be displayed.
      }

    function GetTitle(const DocContent: string): string; override;
      {Extracts a document title from a document if possible.
        @param DocContent [in] Document content.
        @return '' (plain text does not support an embedded title).
      }
  protected
    { IPreview: Partially implemented in base class }
    procedure SetPopupMenu(const Menu: TPopupMenu);
      {Sets pop up menu to be displayed when memo control right clicked.
        @param Menu [in] Required popup menu.
      }
    { IClipboardMgr: Implemented in base class }
    { ISelectionMgr: Implemented in base class }
  end;


implementation


{$R *.dfm}


{ TTextPreviewFrame }

function TTextPreviewFrame.GetMemoCtrl: TCustomMemo;
  {Gets reference to memo control used to display plain text.
    @return Required TMemo reference.
  }
begin
  Result := edDisplay;
end;

function TTextPreviewFrame.GetTitle(const DocContent: string): string;
  {Extracts a document title from a document if possible.
    @param DocContent [in] Document content.
    @return '' (plain text does not support an embedded title).
  }
begin
  Result := '';   // can't get a title from plain text objects
end;

procedure TTextPreviewFrame.LoadContent(const DocContent: string);
  {Loads document into memo control.
    @param DocContent [in] Plain text document to be displayed.
  }
begin
  edDisplay.Text := DocContent;
end;

procedure TTextPreviewFrame.SetPopupMenu(const Menu: TPopupMenu);
  {Sets pop up menu to be displayed when memo control right clicked.
    @param Menu [in] Required popup menu.
  }
begin
  edDisplay.PopupMenu := Menu;
end;

end.

