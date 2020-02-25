{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Custom action used to request display of a view item.
}


unit UViewItemAction;


interface


uses
  // Delphi
  Classes,
  // Project
  UView;


type
  ///  <summary>
  ///  Custom action used to request display of a view item.
  ///  </summary>
  TViewItemAction = class(TBasicAction)
  strict private
    ///  <summary>Value of ViewItem property.</summary>
    fViewItem: IView;
    ///  <summary>Value of NewTab property.</summary>
    fNewTab: Boolean;
  public
    ///  <summary>View item to be displayed.</summary>
    property ViewItem: IView read fViewItem write fViewItem;
    ///  <summary>Flags whether view item is to be displayed in a new tab (True)
    ///  or in existing tab (False).</summary>
    property NewTab: Boolean read fNewTab write fNewTab;
  end;


implementation

end.

