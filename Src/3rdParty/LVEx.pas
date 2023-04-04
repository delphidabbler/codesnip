{*******************************************************}
{                                                       }
{             TListViewEx Version 1.0                   }
{                                                       }
{       Copyright (c) 1999-2009 Vadim Crits             }
{                                                       }
{*******************************************************}

// NOTE: Modified by DelphiDabbler 2023 - see //! comments for changes

unit LVEx;

interface

uses
  Windows, Messages, Classes, CommCtrl, ComCtrls;
  
type
  TSortOrder = (soNone, soUp, soDown);

  TListViewEx = class(TListView)
  private
    FHeaderHandle: HWND;
    FSortColumn: Integer;
    FSortOrder: TSortOrder;
    FSortImmediately: Boolean;
    procedure UpdateColumn(Index: Integer);
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure LVMDeleteColumn(var Message: TMessage); message LVM_DELETECOLUMN;
    procedure LVMSetItemText(var Message: TMessage); message LVM_SETITEMTEXT;
  public
    constructor Create(AOwner: TComponent); override;
    property SortColumn: Integer read FSortColumn default -1;
    property SortOrder: TSortOrder read FSortOrder default soNone;
    property SortImmediately: Boolean read FSortImmediately write
      FSortImmediately default True;
  end;

procedure Register;

implementation

{$R LVEx.res}

procedure Register;
begin
  RegisterComponents('Win32', [TListViewEx]);
end;

constructor TListViewEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortColumn := -1;
  FSortOrder := soNone;
  FSortImmediately := True;
end;

procedure TListViewEx.UpdateColumn(Index: Integer);
const
  HDF_SORTUP = $0400;
  HDF_SORTDOWN = $0200;
  SortOrder: array[TSortOrder] of Word = (0, HDF_SORTUP, HDF_SORTDOWN);
var
  Item: THDItem;
begin
  FillChar(Item, SizeOf(THDItem), 0);
  Item.Mask := HDI_FORMAT;
  if (FSortColumn <> -1) and (FSortColumn <> Index) then
  begin
    Header_GetItem(FHeaderHandle, FSortColumn, Item);
    Item.fmt := Item.fmt and not (HDF_SORTDOWN or HDF_SORTUP);
    Header_SetItem(FHeaderHandle, FSortColumn, Item);
  end;
  Header_GetItem(FHeaderHandle, Index, Item);
  if (FSortColumn <> -1) and (FSortColumn = Index) then
    Item.fmt := Item.fmt and not (HDF_SORTDOWN or HDF_SORTUP);
  Item.fmt := Item.fmt or SortOrder[FSortOrder];
  Header_SetItem(FHeaderHandle, Index, Item);
end;

procedure TListViewEx.WMNotify(var Message: TWMNotify);
begin
  inherited;
  FHeaderHandle := ListView_GetHeader(Handle);
  if (FHeaderHandle <> 0) and (Message.NMHdr^.hWndFrom = FHeaderHandle) then
    with Message, NMHdr^, PHDNotify(Pointer(NMHdr))^ do
      case code of
        HDN_ITEMCLICK:
          begin
            if (FSortColumn = Item) and (FSortOrder = soUp) then
              FSortOrder := soDown
            else
              FSortOrder := soUp;
            UpdateColumn(Item);
            FSortColumn := Item;
            CustomSort(nil, FSortColumn - 1);
          end;
        HDN_ITEMCHANGED:
          if (FSortColumn <> -1) and (FSortColumn = Item) and
            (PItem^.Mask and HDI_TEXT <> 0) then
            UpdateColumn(Item);
      end;
end;

procedure TListViewEx.LVMDeleteColumn(var Message: TMessage);
begin
  inherited;
  //! Add cast to FSortColumn to avoid widened operands warnings in Delphi 11.3
  if Message.WParam = NativeUInt(FSortColumn) then
  begin
    FSortColumn := -1;
    FSortOrder := soNone;
  end;
end;

procedure TListViewEx.LVMSetItemText(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) and
    FSortImmediately and (FSortColumn <> -1) then
    with PLVItem(Message.LParam)^ do
      if (FSortColumn = iSubItem) and
        ((iSubItem = 0) or (stateMask and LVIS_FOCUSED = 0)) then
        CustomSort(nil, FSortColumn - 1);
end;

end.
