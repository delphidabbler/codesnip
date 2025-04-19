{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements class that extends TTreeNode by adding a property that references
 * a view item.
 *
 * ACKNOWLEDGEMENT: GetViewItem & SetViewItem property accessors implemented by
 * @SirRufo (GitHub PR #160 & Issue #158).
}


unit UViewItemTreeNode;


interface


uses
  // Delphi
  ComCtrls,
  // Project
  UView;


type
  ///  <summary>Custom tree node class that adds a property to store a weak
  ///  reference to an <c>IView</c> instance in a tree node.</summary>
  TViewItemTreeNode = class(TTreeNode)
  strict private
    function GetViewItem: IView;
    procedure SetViewItem(const Value: IView);
  public
    ///  <summary>View item associated with tree node.</summary>
    ///  <remarks>NOTE: This view item is stored as a weak reference via a
    ///  pointer so the reference count is not updated.</remarks>
    property ViewItem: IView read GetViewItem write SetViewItem;
  end;


implementation

{ TViewItemTreeNode }

function TViewItemTreeNode.GetViewItem: IView;
begin
  Result := IView(Data);
end;

procedure TViewItemTreeNode.SetViewItem(const Value: IView);
begin
  Data := Pointer(Value);
end;

end.

