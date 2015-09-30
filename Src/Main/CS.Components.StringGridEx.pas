{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * An extended TStringGrid component that replaces the grid'd standard in-place
 * editor with the TInplaceEditList editor supplied in the Delphi VCL Grids
 * unit.
 *
 * This implementation is derived from the tip "Replacing the TStringGrid's
 * standard InplaceEditor" at http://delphi.longzu.net/viewthread.php?tid=47965
}


unit CS.Components.StringGridEx;


interface


uses
  Classes,
  Grids;


type
  TGetEditStyleEvent = procedure (Sender: TObject; ACol, ARow: Integer;
    var EditStyle: TEditStyle) of object;

  TStringGridEx = class(TStringGrid)
  strict private
    var
      fDropDownRowCount: Integer;
      fOnEditButtonClick: TNotifyEvent;
      fOnGetEditStyle: TGetEditStyleEvent;
      fOnGetPickListItems: TOnGetPickListItems;
    procedure SetDropDownRowCount(Value: Integer);
    procedure SetOnEditButtonClick(Value: TNotifyEvent);
    procedure SetOnGetPickListItems(Value: TOnGetPickListItems);
  strict protected
    function CreateEditor: TInplaceEdit; override;
    function GetEditStyle(ACol, ARow: Integer): TEditStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DropDownRowCount: Integer
      read fDropDownRowCount write SetDropDownRowCount default 8;
    property OnEditButtonClick: TNotifyEvent
      read fOnEditButtonClick write SetOnEditButtonClick;
    property OnGetEditStyle: TGetEditStyleEvent
      read fOnGetEditStyle write fOnGetEditStyle;
    property OnGetPickListItems: TOnGetPickListItems
      read fOnGetPickListItems write SetOnGetPickListItems;
  end;


implementation


{ TStringGridEx }

constructor TStringGridEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDropDownRowCount := 8;
end;

function TStringGridEx.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEditList.Create(self);
  with TInplaceEditList(Result) do
  begin
    DropDownRows := fDropDownRowCount;
    OnGetPickListItems := fOnGetPickListItems;
    OnEditButtonClick := fOnEditButtonClick;
  end;
end;

function TStringGridEx.GetEditStyle(ACol, ARow: Integer): TEditStyle;
begin
  Result := esSimple;
  if Assigned(fOnGetEditStyle)
    then fOnGetEditStyle(Self, ACol, ARow, Result);
end;

procedure TStringGridEx.SetDropDownRowCount(Value: Integer);
begin
  fDropDownRowCount := Value;
  if Assigned(InplaceEditor) then
    TInplaceEditList(InplaceEditor).DropDownRows := Value;
end;

procedure TStringGridEx.SetOnEditButtonClick(Value: TNotifyEvent);
begin
  fOnEditButtonClick := Value;
  if Assigned(InplaceEditor) then
    TInplaceEditList(InplaceEditor).OnEditButtonClick := Value;
end;

procedure TStringGridEx.SetOnGetPickListItems(Value: TOnGetPickListItems);
begin
  fOnGetPickListItems := Value;
  if Assigned(InplaceEditor) then
    TInplaceEditList(InplaceEditor).OnGetPickListitems := Value;
end;

end.
