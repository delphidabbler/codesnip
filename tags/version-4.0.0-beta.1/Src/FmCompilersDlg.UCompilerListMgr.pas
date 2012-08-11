{
 * FmCompilersDlg.UCompilerListMgr.pas
 *
 * Implements a class that manages display of compiler names and glyphs in an
 * owner draw list box. This is a helper class for TCompilersDlg.
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
 * The Original Code is FmCompilersDlg.UCompilerListMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmCompilersDlg.UCompilerListMgr;


interface


uses
  // Delphi
  Classes, Controls, StdCtrls, Windows,
  // Project
  Compilers.UGlobals;


type
  ///  <summary>
  ///  Manages display of compiler names and glyphs in an owner draw list box.
  ///  </summary>
  ///  <remarks>
  ///  This is a helper class for TCompilersDlg.
  ///  </remarks>
  TCompilerListMgr = class(TObject)
  strict private
    ///  <summary>Reference to managed list box.</summary>
    ///  <remarks>Must be owenr draw.</remarks>
    fLB: TListBox;
    ///  <summary>List of compilers to be displayed in list box.</summary>
    fCompilers: ICompilers;
    ///  <summary>Reference to OnSelect event handler.</summary>
    fOnSelect: TNotifyEvent;
    ///  <summary>OnClick event handler for list box. Triggers OnSelect event.
    ///  </summary>
    procedure LBClickHandler(Sender: TObject);
    ///  <summary>OnDrawItem event handler for list box. Performs custom
    ///  drawing.</summary>
    procedure LBDrawItemHandler(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    ///  <summary>Triggers OnSelect event.</summary>
    procedure DoSelect;
    ///  <summary>Read accessor for Selected property. Gets compiler associated
    ///  with selected list item.</summary>
    function GetSelected: ICompiler;
  public
    ///  <summary>Object constructor. Sets up object for given list box control
    ///  and list of compilers.</summary>
    constructor Create(const LB: TListBox; const Compilers: ICompilers);
    ///  <summary>Initialises list box to display required compilers.</summary>
    ///  <remarks>This initialisation has to be performed when host for is
    ///  shown and not before.</remarks>
    procedure Initialise;
    ///  <summary>Refreshes display of entire list.</summary>
    procedure Refresh; overload;
    ///  <summary>Refreshes display of list item associated with given compiler.
    ///  </summary>
    procedure Refresh(Compiler: ICompiler); overload;
    ///  <summary>Event triggered when selected item in list changes.</summary>
    ///  <remarks>Read Selected property to get newly selected compiler.
    ///  </remarks>
    property OnSelect: TNotifyEvent read fOnSelect write fOnSelect;
    ///  <summary>Reference to compiler associated with currently selected list
    ///  item.</summary>
    property Selected: ICompiler read GetSelected;
  end;


implementation


uses
  // Delphi
  Graphics, GraphUtil,
  // Project
  UStructs, UThemesEx;


{ TCompilerListMgr }

constructor TCompilerListMgr.Create(const LB: TListBox;
  const Compilers: ICompilers);
begin
  inherited Create;
  fLB := LB;
  fLB.OnClick := LBClickHandler;
  fLB.OnDrawItem := LBDrawItemHandler;
  fCompilers := Compilers;
end;

procedure TCompilerListMgr.DoSelect;
begin
  if Assigned(fOnSelect) then
    fOnSelect(Self);
end;

function TCompilerListMgr.GetSelected: ICompiler;
begin
  Result := fCompilers[TCompilerID(fLB.ItemIndex)];
end;

procedure TCompilerListMgr.Initialise;
var
  CompID: TCompilerID;  // loops thru supported compilers
begin
  inherited;
  // Add empty list items: one per supported compiler
  // (note we don't need item text since we handle drawing of list items
  // ourselves and get display details from compiler objects
  for CompID := Low(TCompilerID) to High(TCompilerID) do
    fLB.Items.Add('');
  // Select first compiler in list and trigger selection event for it
  fLB.ItemIndex := 0;
  DoSelect;
end;

procedure TCompilerListMgr.LBClickHandler(Sender: TObject);
begin
  DoSelect;
end;

procedure TCompilerListMgr.LBDrawItemHandler(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  LB: TListBox;         // list box being drawn
  TxtExtent: TSize;     // extent of text to be displayed
  ItemRect: TRectEx;    // rectangle bounding the item being drawn
  ImgRect: TRectEx;     // bounding rectangle of any glyph
  TxtRect: TRectEx;     // bounding rectangle of text to be drawn
  Bmp: TBitmap;         // reference to bitmap to be displayed (or nil if none)
  DrawHeight: Integer;  // total height of drawing (text below bitmap)
  Compiler: ICompiler;  // reference to compiler associated with list item
begin
  // Copy item rectangle as extended rect
  ItemRect := Rect;

  // Get reference to list box control
  LB := Control as TListBox;

  // Get reference to compiler object associated with list item and its bitmap
  Compiler := fCompilers[TCompilerID(Index)];
  Bmp := Compiler.GetGlyph;

  // Set font style: bold if compiler available
  if Compiler.IsAvailable then
    LB.Canvas.Font.Style := [fsBold]
  else
    LB.Canvas.Font.Style := [];

  // Calculate display rectangles for text and any bitmap
  TxtExtent := LB.Canvas.TextExtent(Compiler.GetName);
  if Assigned(Bmp) then
  begin
    // Bitmap included: bitmap drawn above text and bounding box or both centred
    // horizontally and vertically
    DrawHeight := TxtExtent.cy + 2 + Bmp.Height;
    ImgRect := TRectEx.CreateBounds(
      (ItemRect.Left + ItemRect.Right - Bmp.Width) div 2,
      (ItemRect.Top + ItemRect.Bottom - DrawHeight) div 2,
      Bmp.Width,
      Bmp.Height
    );
    TxtRect := TRectEx.CreateBounds(
      (ItemRect.Left + ItemRect.Right - TxtExtent.cx) div 2,
      ImgRect.Bottom + 2,
      TxtExtent
    );
  end
  else
  begin
    // No bitmap: text centred vertically and horizontally
    TxtRect := TRectEx.CreateBounds(
      (ItemRect.Left + ItemRect.Right - TxtExtent.cx) div 2,
      (ItemRect.Top + ItemRect.Bottom - TxtExtent.cy) div 2,
      TxtExtent
    );
  end;

  // Erase background
  LB.Canvas.Pen.Color := LB.Color;
  LB.Canvas.Brush.Color := LB.Color;
  LB.Canvas.FillRect(ItemRect);

  if (odFocused in State) or (odSelected in State) then
  begin
    // Draw highlighting (smaller than item's rectangle)
    if odFocused in State then
    begin
      if ThemeServicesEx.ThemesEnabled then
        LB.Canvas.Brush.Color := GetHighLightColor(clHighlight)
      else
        LB.Canvas.Brush.Color := clHighlight;
      LB.Canvas.Font.Color := clHighlightText;
    end
    else
    begin
      LB.Canvas.Brush.Color := clBtnFace;
      LB.Canvas.Font.Color := LB.Font.Color;
    end;
    LB.Canvas.Pen.Color := GetShadowColor(LB.Canvas.Brush.Color);
    LB.Canvas.Rectangle(ItemRect.Inflate(-4, -4));
  end
  else
    // No highlighting: just ensure font colour correct
    LB.Canvas.Font.Color := LB.Font.Color;

  // Draw text and any bitamp
  LB.Canvas.TextOut(TxtRect.Left, TxtRect.Top, Compiler.GetName);
  if Assigned(Bmp) then
    LB.Canvas.BrushCopy(ImgRect, Bmp, Bmp.Canvas.ClipRect, clFuchsia);

  // Draw separator line if item not last one
  if Index < Pred(LB.Count) then
  begin
    LB.Canvas.Pen.Color := clBtnShadow;
    LB.Canvas.MoveTo(ItemRect.Left + 4, ItemRect.Bottom - 1);
    LB.Canvas.LineTo(ItemRect.Right - 4, ItemRect.Bottom - 1);
  end;

  // Remove any focus rectangle
  if odFocused in State then
    LB.Canvas.DrawFocusRect(ItemRect);
end;

procedure TCompilerListMgr.Refresh;
begin
  fLB.Invalidate;
end;

procedure TCompilerListMgr.Refresh(Compiler: ICompiler);
var
  InvalidRect: TRectEx;
begin
  InvalidRect := fLB.ItemRect(Ord(Compiler.GetID));
  InvalidateRect(fLB.Handle, @InvalidRect, False);
end;

end.
