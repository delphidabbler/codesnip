{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
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
    function GetViewItem: IView;
    procedure SetViewItem(const Value: IView);
  published // Value of ViewItem property
  public
    property ViewItem: IView read GetViewItem write SetViewItem;
      {View item associated with tree node}
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

