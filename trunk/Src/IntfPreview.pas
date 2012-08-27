{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines the interface implemented by objects that can display a document of a
 * certain type in a preview dialog box.
}


unit IntfPreview;


interface


uses
  // Delphi
  Menus,
  // Project
  UEncodings;


type
  {
  IPreview:
    Interface implemented by objects that can display a document of a certain
    type in a preview dialog box.
  }
  IPreview = interface(IInterface)
    ['{85070D46-EB65-4C59-BA60-AE6144037C83}']
    procedure Display(const DocContent: TEncodedData);
      {Displays document in preview dialog box.
        @param DocContent [in] Content of document to be displayed.
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

