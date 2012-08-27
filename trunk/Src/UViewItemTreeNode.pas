{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements class that extends TTreeNode by adding a property that references
 * a view item.
}


unit UViewItemTreeNode;


interface


uses
  // Delphi
  ComCtrls,
  // Project
  UView;


type
  {
  TViewItemTreeNode:
    Custom tree node class that adds ability to store reference to a view item
    in a tree node.
  }
  TViewItemTreeNode = class(TTreeNode)
  strict private
    var fViewItem: IView; // Value of ViewItem property
  public
    property ViewItem: IView read fViewItem write fViewItem;
      {View item associated with tree node}
  end;


implementation

end.

