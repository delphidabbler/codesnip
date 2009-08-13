{
 * UViewItemTreeNode.pas
 *
 * Implements class that extends TTreeNode by adding a property that references
 * a view item.
 *
 * This code was formerly in the FrOverview unit.
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
 * The Original Code is UViewItemTreeNode.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
    fViewItem: TViewItem; // Value of ViewItem property
  public
    property ViewItem: TViewItem read fViewItem write fViewItem;
      {View item associated with tree node}
  end;


implementation

end.

