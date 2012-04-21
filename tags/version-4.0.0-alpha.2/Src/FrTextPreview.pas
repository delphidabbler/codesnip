{
 * FrTextPreview.pas
 *
 * Frame used to display previews of plain text documents, including source
 * code.
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
 * The Original Code is FrTextPreview.pas
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


unit FrTextPreview;


interface


uses
  // Delphi
  StdCtrls, Classes, Controls, ExtCtrls, Menus,
  // Project
  FrMemoPreview, IntfFrameMgrs, IntfPreview, UEncodings;


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
    procedure LoadContent(const DocContent: TEncodedData); override;
      {Loads document into memo control.
        @param DocContent [in] Plain text document to be displayed.
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

procedure TTextPreviewFrame.LoadContent(const DocContent: TEncodedData);
  {Loads document into memo control.
    @param DocContent [in] Plain text document to be displayed.
  }
begin
  edDisplay.Text := DocContent.ToString;
end;

procedure TTextPreviewFrame.SetPopupMenu(const Menu: TPopupMenu);
  {Sets pop up menu to be displayed when memo control right clicked.
    @param Menu [in] Required popup menu.
  }
begin
  edDisplay.PopupMenu := Menu;
end;

end.

