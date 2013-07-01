{
 * UViewItemAction.pas
 *
 * Custom action used to request display of a view item.
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
 * The Original Code is UViewItemAction.pas
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


unit UViewItemAction;


interface


uses
  // Delphi
  Classes,
  // Project
  UView;


type

  {
  TViewItemAction:
    Custom action used to request display of a view item. Stores reference to
    required view item.
  }
  TViewItemAction = class(TBasicAction)
  private
    fViewItem: TViewItem;
      {Reference to view item object}
  public
    property ViewItem: TViewItem
      read fViewItem write fViewItem;
      {View item object to be displayed}
  end;


implementation

end.

