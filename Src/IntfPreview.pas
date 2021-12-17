{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines the interface implemented by objects that can display a document of a
 * certain type in a preview dialogue box.
}


unit IntfPreview;


interface


uses
  // Delphi
  Menus,
  // Project
  UEncodings;


type
  ///  <summary>Interface implemented by objects that can display a document of
  ///  a certain type in a preview dialogue box.</summary>
  ///  <remarks>The supported document types are implementation dependant.
  ///  </remarks>
  IPreview = interface(IInterface)
    ['{85070D46-EB65-4C59-BA60-AE6144037C83}']
    ///  <summary>Displays a document in a preview dialogue box.</summary>
    ///  <param name="DocContent">TEncodedData [in] Content of document to be
    ///  displayed.</param>
    procedure Display(const DocContent: TEncodedData);
    ///  <summary>Provides a context menu that can be displayed by the control
    ///  that is displaying the preview.</summary>
    ///  <param name="Menu">TPopupMenu [in] Context menu.</param>
    procedure SetPopupMenu(const Menu: TPopupMenu);
  end;

const
  ///  <summary>Size of margin to use in previews, in pixels.</summary>
  cPreviewMargin = 8;


implementation

end.

