{
 * IntfPreview.pas
 *
 * Interface implemented by objects that can display a document of a certain
 * type in a preview dialog box.
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
 * The Original Code is IntfPreview.pas
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


unit IntfPreview;


interface


uses
  // Delphi
  Menus;


type
  {
  IPreview:
    Interface implemented by objects that can display a document of a certain
    type in a preview dialog box.
  }
  IPreview = interface(IInterface)
    ['{85070D46-EB65-4C59-BA60-AE6144037C83}']
    procedure Display(const DocContent: string; out Title: string);
      {Displays document in preview dialog box.
        @param DocContent [in] Content of document to be displayed.
        @param Title [out] Title of document, if any.
      }
    procedure SetPopupMenu(const Menu: TPopupMenu);
      {Provides a popup menu to be displayed when control that is displaying
      preview is right-clicked.
        @param Menu [in] Popup menu to be displayed.
      }
  end;

const
  // Size of margin to use in previews
  cPreviewMargin = 8;


implementation

end.

