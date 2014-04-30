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
 * Defines classes that manages display and interaction with a list box that
 * displays compiler results.
}


unit UCompileResultsLBMgr;


interface


uses
  // Delphi
  Classes, Controls, StdCtrls, Types,
  // Project
  Compilers.UGlobals, UDropDownButtons, ULEDImageList, UStructs;


type

  {
  TCompileResultsLBMgr:
    Class that manages display and interaction with a list box that displays
    compiler results. Provides a means of editing the results for individual
    compilers.
  }
  TCompileResultsLBMgr = class(TObject)
  strict private
    type
      {
      TCompilerInfo:
        Class that records information about a snippet's compilation results.
        Resulting objects are stored in list box item's Objects[] array.
      }
      TCompilerInfo = class(TObject)
      strict private
        fCompilerID: TCompilerID;       // Value of Compiler property
        fCompileResult: TCompileResult; // Value of CompileResult property
      public
        constructor Create(const CompilerID: TCompilerID;
          const CompileResult: TCompileResult);
          {Object constructor. Sets up and initialises object.
            @param CompilerID [in] Id of compiler that result applies to.
            @param CompileResult [in] Compiler result for compiler.
          }
        property CompilerID: TCompilerID read fCompilerID;
          {Id of compiler to which CompileResult applies}
        property CompileResult: TCompileResult
          read fCompileResult write fCompileResult;
          {Result of compilation with associated compiler}
      end;
    var
      fLB: TListBox;                    // Managed list box
      fCompilers: ICompilers;           // Compilers to be displayed in list box
      fLEDImages: TLEDImageList;        // Image list containing LEDs
      fLastHotDropDown: TRectEx;        // Bounds of last drop down highlighted
      fDropDownBtns: TDropDownButtons;  // Provides drop down button glyphs
    procedure PopulateListBox;
      {Adds details of each compiler to list box. Compile result is set to
      'unknown'.
      }
    function IndexOf(const CompID: TCompilerID): Integer;
      {Finds index of list item corresponding to a compiler.
        @param CompID [in] ID of compiler.
        @return Index of corresponding list item or -1 if compiler not found.
      }
    procedure SetCompileResult(const Index: Integer; const Res: TCompileResult);
      {Sets compile result of a list item.
        @param Index [in] Index of list item.
        @param Res [in] Required compiler result.
      }
    function GetCompilerInfo(const Index: Integer): TCompilerInfo;
      overload;
      {Retrieves the compiler info associated with a specified list item.
        @param Index [in] Index of list item.
        @return Required compiler info.
      }
    function GetCompilerInfo(const CompID: TCompilerID): TCompilerInfo;
      overload;
      {Retrieves the compiler info associated with a specified compiler ID.
        @param CompID [in] Compiler ID.
        @return Required compiler info.
      }
    procedure MenuSelectHandler(Sender: TObject);
      {Handles menu selection on popup menu activated from drop down button.
      Sets compile result of selected list item per menu selection.
        @param Sender [in] Menu item that was clicked.
      }
    procedure DrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
      {OnDrawItem event handler for managed list box. Custom draws compiler list
      item to name of compiler, current compile result 'LED' and a drop-down
      button.
        @param Control [in] Reference to list box that triggered event. Must be
          the managed control.
        @param Index [in] Index of list item being drawn.
        @param Rect [in] Rectangle in list box canvas where item being drawn.
        @param State [in] State of list item.
      }
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
      {Handles list box's OnMouseDown event. Displays compile result menu if
      user left clicked on drop down button with no modifier keys pressed.
        @param Sender [in] Not used.
        @param Button [in] Mouse button pressed.
        @param Shift [in] Modifier keys etc.
        @param X [in] X coordinate of mouse pointer in list box.
        @param Y [in] Y coordinate of mouse pointer in list box.
      }
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      {Handles list box's OnMouseMove event. Updates display of hot and normal
      drop down buttons depending on whether mouse is over such a button.
        @param Sender [in] List box that triggered event. Not used.
        @param Shift [in] State of shift keys. Not used.
        @param X [in] X coordinate of mouse pointer in list box.
        @param Y [in] Y coordinate of mouse pointer in list box.
      }
    procedure MouseLeave(Sender: TObject);
      {Handles list box's OnMouseLeave event. Redisplays any drop down button
      that was displayed as hot.
        @param Sender [in] Not used.
      }
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      {Handles list box's OnKeyUp event. Displays popup menu over any currently
      selected list item if user pressed space bar with no modifier keys.
        @param Sender [in] Not used.
        @param Key [in/out] Code of key pressed. Left unchanged.
        @param Shift [in] State of modifier keys.
      }
    function GetLEDBounds(const ItemRect: TRect): TRect;
      {Gets bounds of a LED glyph within an item rectangle in list box.
        @param ItemRect [in] Rectangle containing LED glyph.
        @return Bounding rectangle of glyph.
      }
    function GetDropDownBmpBounds(const ItemRect: TRect): TRectEx;
      {Gets bounds of a drop down button glyph within an item rectangle in list
      box.
        @param ItemRect [in] Rectangle containing drop down button glyph.
        @return Bounding rectangle of glyph.
      }
    procedure InvalidateLBRect(const R: TRect);
      {Invalidates a specified rectangle in list box for redrawing.
        @param R [in] Rectangle to be invalidated.
      }
    function IsMouseInRect(const Rect: TRectEx): Boolean;
      {Checks if the mouse pointer is within a specified rectangle in list box.
        @param Rect [in] Rectangle to be queried.
        @return True if mouse pointer in rectangle, False if not.
      }
    procedure ShowPopupMenu(const ItemIdx: Integer);
      {Displays popup menu over a list item that is used to select a compile
      result.
        @param ItemIdx [in] Index of list item for which menu is required.
      }
    procedure ThemesChangeHandler(Sender: TObject);
      {Handles themes change event. Redisplays list box to use new themes.
        @param Sender [in] Not used.
      }
  public
    constructor Create(LB: TListBox; const Compilers: ICompilers);
      {Object constructor. Sets up manager for a list box.
        @param LB [in] Listbox to be managed.
        @param Compilers [in] Compilers to be displayed in listbox.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    procedure SetCompileResults(const Res: TCompileResults); overload;
      {Sets the compile results displayed by the list box. All items are set.
      Each may have different value.
        @param Res [in] Array of compiler results addressed by compiler ID.
      }
    procedure SetCompileResults(const Res: TCompileResult); overload;
      {Sets all the compile results displayed by the list box to same value.
        @param Res [in] Required compiler result.
      }
    procedure SetCurrentCompileResult(const Res: TCompileResult);
      {Sets compile result of currently selected compiler.
        @param Res [in] Required compile result.
      }
    function GetCompileResults: TCompileResults;
      {Retrieves an array of all compile results per list box.
        @return Results array, indexed by compiler ID.
      }
    function GetCurrentCompileResult: TCompileResult;
      {Retrieves the compile result for the currently selected list item.
        @return Require compile result.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Menus, Windows, Graphics,
  // Project
  UKeysHelper;


type

  {
  TCompResMenuItem:
    Enhanced menu item that stores a compile result along with menu item.
    Displayed as part of menu that is popped up from compile results list box.
    Selecting the menu item updates current list box item to display the stored
    compile result.
  }
  TCompResMenuItem = class(TMenuItem)
  strict private
    var fCompileResult: TCompileResult; // Value of CompileResult property
  public
    constructor Create(AOwner: TComponent; const ACompRes: TCompileResult;
      const AClickEventHandler: TNotifyEvent); reintroduce;
      {Object constructor. Creates menu item for a compile result.
        @param AOwner [in] Component that owns this menu item.
        @param ACompRes [in] Associated compile result. Determines caption and
          displayed image.
        @param AClickEventHandler [in] Event handler trigerred when menu item
          clicked.
      }
    property CompileResult: TCompileResult read fCompileResult;
      {Compile result associated with menu item}
  end;

  {
  TCompResSelectMenu:
    Special popup menu that is displayed by the compile results list box that
    enables the compile result associated with a menu item to be edited.
  }
  TCompResSelectMenu = class(TPopupMenu)
  strict private
    var
      fCompileResult: TCompileResult; // Value of CompileResult property
      fOnSelect: TNotifyEvent;        // OnSelect event handler
    procedure MenuItemClickHandler(Sender: TObject);
      {Handles clicks on all menu items. Records compile result associated with
      selected menu item in CompileResult property and triggers OnSelect event.
        @param Sender [in] Menu item that triggered event.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Object constructor. Creates menu and populates it with items for all
      possible compile results.
        @param AOwner [in] Component that owns the menu.
      }
    property CompileResult: TCompileResult read fCompileResult default crQuery;
      {Last selected compiler result}
    property OnSelect: TNotifyEvent read fOnSelect write fOnSelect;
      {Event triggered when user selects a compile result}
  end;

{ TCompileResultsLBMgr }

constructor TCompileResultsLBMgr.Create(LB: TListBox;
  const Compilers: ICompilers);
  {Object constructor. Sets up manager for a list box.
    @param LB [in] Listbox to be managed.
    @param Compilers [in] Compilers to be displayed in listbox.
  }
begin
  Assert(Assigned(LB), ClassName + '.Create: LB is nil');
  Assert(Assigned(Compilers), ClassName + '.Create: Compilers is nil');
  inherited Create;
  // record compilers
  fCompilers := Compilers;
  // customise list box
  fLB := LB;
  fLB.Style := lbOwnerDrawFixed;
  fLB.PopupMenu := TCompResSelectMenu.Create(fLB);
  (fLB.PopupMenu as TCompResSelectMenu).OnSelect := MenuSelectHandler;
  fLB.PopupMenu.Alignment := paRight;
  // assign event handlers
  fLB.OnDrawItem := DrawItem;
  fLB.OnMouseDown := MouseDown;
  fLB.OnMouseMove := MouseMove;
  fLB.OnMouseLeave := MouseLeave;
  fLB.OnKeyUp := KeyUp;
  // create list box items and select first one
  PopulateListBox;
  fLB.ItemIndex := 0; // select first item
  // create owned objects
  fDropDownBtns := TDropDownButtons.Create(LB);
  fDropDownBtns.OnChange := ThemesChangeHandler;
  fLEDImages := TLEDImageList.Create(LB);
  // record that no drop down button is under mouse
  fLastHotDropDown.MakeEmpty;
end;

destructor TCompileResultsLBMgr.Destroy;
  {Object destructor. Tears down object.
  }
var
  Idx: Integer;     // loops through all list items
  Tmp: TPopupMenu;  // temporary menu reference used for thread safe freeing
begin
  FreeAndNil(fDropDownBtns);
  for Idx := Pred(fLB.Items.Count) downto 0 do
    fLB.Items.Objects[Idx].Free;  // free all compiler info records
  // free list box's popup menu in thread safe way
  Tmp := fLB.PopupMenu;
  fLB.PopupMenu := nil;
  FreeAndNil(Tmp);
  inherited;
end;

procedure TCompileResultsLBMgr.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
  {OnDrawItem event handler for managed list box. Custom draws compiler list
  item to name of compiler, current compile result 'LED' and a drop-down button.
    @param Control [in] Reference to list box that triggered event. Must be the
      managed control.
    @param Index [in] Index of list item being drawn.
    @param Rect [in] Rectangle in list box canvas where item being drawn.
    @param State [in] State of list item.
  }
var
  Cvs: TCanvas;             // list box's canvas
  CompInfo: TCompilerInfo;  // info about compile result associated with item
  Text: string;             // text to be displayed
  TextRect: TRect;          // rectangle in which to display text
  DropDownRect: TRect;      // rectangle in which to display drop-down "button"
  LEDRect: TRectEx;         // rectangle in which to display LED glyph
begin
  Assert(Control = fLB, ClassName + '.DrawItem: Control <> fLB');
  // Store canvas and associated compiler info
  Cvs := fLB.Canvas;
  CompInfo := GetCompilerInfo(Index);
  // Clear item's rectangle
  Cvs.FillRect(Rect);
  // Display text
  Text := fLB.Items[Index];
  TextRect := TRectEx.Create(
    Rect.Left + 2,
    (Rect.Bottom + Rect.Top - Cvs.TextHeight(Text)) div 2,
    Rect.Right - 24,
    Rect.Bottom
  );
  if odDisabled in State then
    Cvs.Font.Color := clGrayText
  else
    Cvs.Font.Color := fLB.Font.Color;
  Cvs.TextRect(TextRect, Text, [tfLeft, tfNoPrefix, tfEndEllipsis, tfTop]);
  // Display compile result "LED"
  LEDRect := GetLEDBounds(Rect);
  fLEDImages.Draw(Cvs, LEDRect.TopLeft, CompInfo.CompileResult);
  // Display drop-down glyph
  DropDownRect := GetDropDownBmpBounds(Rect);
  fDropDownBtns.Draw(
    Cvs,
    DropDownRect.TopLeft,
    IsMouseInRect(DropDownRect),                    // hot
    (odSelected in State) and (odFocused in State)  // focussed
  );
end;

function TCompileResultsLBMgr.GetCompileResults: TCompileResults;
  {Retrieves an array of all compile results per list box.
    @return Results array, indexed by compiler ID.
  }
var
  CompID: TCompilerID;  // loops through all elements of result array
begin
  for CompID := Low(Result) to High(Result) do
    Result[CompID] := GetCompilerInfo(CompID).CompileResult;
end;

function TCompileResultsLBMgr.GetCompilerInfo(
  const Index: Integer): TCompilerInfo;
  {Retrieves the compiler info associated with a specified list item.
    @param Index [in] Index of list item.
    @return Required compiler info.
  }
begin
  Result := fLB.Items.Objects[Index] as TCompilerInfo;
end;

function TCompileResultsLBMgr.GetCompilerInfo(
  const CompID: TCompilerID): TCompilerInfo;
  {Retrieves the compiler info associated with a specified compiler ID.
    @param CompID [in] Compiler ID.
    @return Required compiler info.
  }
begin
  Result := GetCompilerInfo(IndexOf(CompID));
end;

function TCompileResultsLBMgr.GetCurrentCompileResult: TCompileResult;
  {Retrieves the compile result for the currently selected list item.
    @return Require compile result.
  }
begin
  Result := GetCompilerInfo(fLB.ItemIndex).CompileResult;
end;

function TCompileResultsLBMgr.GetDropDownBmpBounds(
  const ItemRect: TRect): TRectEx;
  {Gets bounds of a drop down button glyph within an item rectangle in list box.
    @param ItemRect [in] Rectangle containing drop down button glyph.
    @return Bounding rectangle of glyph.
  }
begin
  Result :=  TRectEx.CreateBounds(
    ItemRect.Right - 2 - fDropDownBtns.Images.Width,
    (ItemRect.Bottom + ItemRect.Top - fDropDownBtns.Images.Height) div 2,
    fDropDownBtns.Images.Width,
    fDropDownBtns.Images.Height
  );
end;

function TCompileResultsLBMgr.GetLEDBounds(const ItemRect: TRect): TRect;
  {Gets bounds of a LED glyph within an item rectangle in list box.
    @param ItemRect [in] Rectangle containing LED glyph.
    @return Bounding rectangle of glyph.
  }
begin
  Result :=  TRectEx.CreateBounds(
    ItemRect.Right - 2 - fLEDImages.Width - 2 - fDropDownBtns.Images.Width,
    (ItemRect.Bottom + ItemRect.Top - fLEDImages.Height) div 2,
    fLEDImages.Width,
    fLEDImages.Height
  );
end;

function TCompileResultsLBMgr.IndexOf(const CompID: TCompilerID): Integer;
  {Finds index of list item corresponding to a compiler.
    @param CompID [in] ID of compiler.
    @return Index of corresponding list item or -1 if compiler not found.
  }
var
  Idx: Integer; // loops thru list box items
begin
  Result := -1;
  for Idx := 0 to Pred(fLB.Items.Count) do
    if GetCompilerInfo(Idx).CompilerID = CompID then
    begin
      Result := Idx;
      Break;
    end;
  Assert(Result >= 0, ClassName + '.IndexOf: CompID not found');
end;

procedure TCompileResultsLBMgr.InvalidateLBRect(const R: TRect);
  {Invalidates a specified rectangle in list box for redrawing.
    @param R [in] Rectangle to be invalidated.
  }
begin
  Windows.InvalidateRect(fLB.Handle, @R, False);
end;

function TCompileResultsLBMgr.IsMouseInRect(const Rect: TRectEx): Boolean;
  {Checks if the mouse pointer is within a specified rectangle in list box.
    @param Rect [in] Rectangle to be queried.
    @return True if mouse pointer in rectangle, False if not.
  }
begin
  Result := Rect.ContainsPoint(fLB.ScreenToClient(Mouse.CursorPos));
end;

procedure TCompileResultsLBMgr.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Handles list box's OnKeyUp event. Displays popup menu over any currently
  selected list item if user pressed space bar with no modifier keys.
    @param Sender [in] Not used.
    @param Key [in/out] Code of key pressed. Left unchanged.
    @param Shift [in] State of modifier keys.
  }
begin
  if fLB.ItemIndex = -1 then
    Exit;
  // only space with no shift keys is processed
  if HasShiftKeys(Shift) or (Key <> VK_SPACE) then
    Exit;
  // space key pressed: display menu
  ShowPopupMenu(fLB.ItemIndex);
end;

procedure TCompileResultsLBMgr.MenuSelectHandler(Sender: TObject);
  {Handles menu selection on popup menu activated from drop down button. Sets
  compile result of selected list item per menu selection.
    @param Sender [in] Menu item that was clicked.
  }
begin
  SetCurrentCompileResult((Sender as TCompResSelectMenu).CompileResult);
end;

procedure TCompileResultsLBMgr.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  {Handles list box's OnMouseDown event. Displays compile result menu if user
  left clicked on drop down button with no modifier keys pressed.
    @param Sender [in] Not used.
    @param Button [in] Mouse button pressed.
    @param Shift [in] Modifier keys etc.
    @param X [in] X coordinate of mouse pointer in list box.
    @param Y [in] Y coordinate of mouse pointer in list box.
  }
var
  ItemIdx: Integer;   // list item under mouse (-1 if mouse not on list item)
  MousePos: TPoint;   // position of mouse
  DDBounds: TRectEx;  // bounds of drop down button
begin
  // left mouse button must have been pressed with no modifier keys
  if HasShiftKeys(Shift) or (Button <> mbLeft) then
    Exit;
  // find list item from modifier keys
  MousePos := Point(X, Y);
  ItemIdx := fLB.ItemAtPos(MousePos, True);
  if ItemIdx = -1 then
    Exit;
  // find if drop down button pressed in list item and display menu if so
  DDBounds := GetDropDownBmpBounds(fLB.ItemRect(ItemIdx));
  if not DDBounds.ContainsPoint(MousePos) then
    Exit;
  ShowPopupMenu(ItemIdx);
end;

procedure TCompileResultsLBMgr.MouseLeave(Sender: TObject);
  {Handles list box's OnMouseLeave event. Redisplays any drop down button that
  was displayed as hot.
    @param Sender [in] Not used.
  }
begin
  if not fLastHotDropDown.IsEmpty then
  begin
    InvalidateLBRect(fLastHotDropDown);
    fLastHotDropDown.MakeEmpty;
  end;
end;

procedure TCompileResultsLBMgr.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  {Handles list box's OnMouseMove event. Updates display of hot and normal drop
  down buttons depending on whether mouse is over such a button.
    @param Sender [in] List box that triggered event. Not used.
    @param Shift [in] State of shift keys. Not used.
    @param X [in] X coordinate of mouse pointer in list box.
    @param Y [in] Y coordinate of mouse pointer in list box.
  }
var
  ItemIdx: Integer;         // index of list item under mouse (-1 if none)
  DropDownRect: TRectEx;    // bounds list item's drop down button
begin
  // find list item under mouse, if any
  ItemIdx := fLB.ItemAtPos(Point(X, Y), True);
  if ItemIdx >= 0 then
  begin
    // there is a list item under mouse: find bounds of item's drop down button
    DropDownRect := GetDropDownBmpBounds(fLB.ItemRect(ItemIdx));
    if DropDownRect.ContainsPoint(Point(X, Y)) then
    begin
      // we are inside drop down button
      if DropDownRect <> fLastHotDropDown then
      begin
        // button has changed: need to redraw
        if not fLastHotDropDown.IsEmpty then
        begin
          // redraw old drop down: no longer hot
          InvalidateLBRect(fLastHotDropDown);
        end;
        // redraw new drop down: now hot
        InvalidateLBRect(DropDownRect);
        // record current drop down
        fLastHotDropDown := DropDownRect;
      end;
    end
    else
    begin
      // not inside drop down button
      if not fLastHotDropDown.IsEmpty then
      begin
        // redraw old drop down: no longer hot
        InvalidateLBRect(fLastHotDropDown);
        fLastHotDropDown.MakeEmpty;
      end;
    end;
  end
  else
  begin
    // not inside any item
    if not fLastHotDropDown.IsEmpty then
    begin
      // redraw old drop down: no longer hot
      InvalidateLBRect(fLastHotDropDown);
      fLastHotDropDown.MakeEmpty;
    end;
  end;
end;

procedure TCompileResultsLBMgr.PopulateListBox;
  {Adds details of each compiler to list box. Compile result is set to
  'unknown'.
  }
var
  Compiler: ICompiler;  // each supported compiler
begin
  for Compiler in fCompilers do
    fLB.Items.AddObject(
      Compiler.GetName,
      TCompilerInfo.Create(Compiler.GetID, crQuery)
    );
end;

procedure TCompileResultsLBMgr.SetCompileResult(const Index: Integer;
  const Res: TCompileResult);
  {Sets compile result of a list item.
    @param Index [in] Index of list item.
    @param Res [in] Required compiler result.
  }
begin
  GetCompilerInfo(Index).CompileResult := Res;
  InvalidateLBRect(GetLEDBounds(fLB.ItemRect(Index)));
end;

procedure TCompileResultsLBMgr.SetCompileResults(const Res: TCompileResult);
  {Sets all the compile results displayed by the list box to same value.
    @param Res [in] Required compiler result.
  }
var
  Idx: Integer; // loops through all list items
begin
  for Idx := 0 to Pred(fLB.Items.Count) do
    SetCompileResult(Idx, Res);
end;

procedure TCompileResultsLBMgr.SetCompileResults(const Res: TCompileResults);
  {Sets the compile results displayed by the list box. All items are set. Each
  may have different value.
    @param Res [in] Array of compiler results addressed by compiler ID.
  }
var
  CompID: TCompilerID;  // loops through all compiler ids
begin
  for CompID := Low(Res) to High(Res) do
    SetCompileResult(IndexOf(CompID), Res[CompID]);
end;

procedure TCompileResultsLBMgr.SetCurrentCompileResult(
  const Res: TCompileResult);
  {Sets compile result of currently selected compiler.
    @param Res [in] Required compile result.
  }
begin
  SetCompileResult(fLB.ItemIndex, Res);
end;

procedure TCompileResultsLBMgr.ShowPopupMenu(const ItemIdx: Integer);
  {Displays popup menu over a list item that is used to select a compile result.
    @param ItemIdx [in] Index of list item for which menu is required.
  }
var
  DropDownBmpBounds: TRectEx; // bounds of list item's drop down button
  PopupPos: TPoint;           // point on screen where menu pops up
begin
  // display right aligned menu below drop down button
  DropDownBmpBounds := GetDropDownBmpBounds(fLB.ItemRect(ItemIdx));
  PopupPos := fLB.ClientToScreen(DropDownBmpBounds.BottomRight);
  fLB.PopupMenu.Popup(PopupPos.X, PopupPos.Y);
end;

procedure TCompileResultsLBMgr.ThemesChangeHandler(Sender: TObject);
  {Handles themes change event. Redisplays list box to use new themes.
    @param Sender [in] Not used.
  }
begin
  fLB.Invalidate;
end;

{ TCompileResultsLBMgr.TCompilerInfo }

constructor TCompileResultsLBMgr.TCompilerInfo.Create(
  const CompilerID: TCompilerID; const CompileResult: TCompileResult);
  {Object constructor. Sets up and initialises object.
    @param CompilerID [in] Id of compiler that result applies to.
    @param CompileResult [in] Compiler result for compiler.
  }
begin
  inherited Create;
  fCompilerID := CompilerID;
  fCompileResult := CompileResult;
end;

{ TCompResMenuItem }

constructor TCompResMenuItem.Create(AOwner: TComponent;
  const ACompRes: TCompileResult; const AClickEventHandler: TNotifyEvent);
  {Object constructor. Creates menu item for a compile result.
    @param AOwner [in] Component that owns this menu item.
    @param ACompRes [in] Associated compile result. Determines caption and
      displayed image.
    @param AClickEventHandler [in] Event handler trigerred when menu item
      clicked.
  }
resourcestring
  // Text for list items in Compiler Result list box
  sSuccess = 'Success';
  sWarning = 'Warning';
  sError = 'Error';
  sQuery = 'Unknown';
const
  // Map of compiler results onto descriptions
  cCompResCaptions: array[TCompileResult] of string = (
    sSuccess, sWarning, sError, sQuery
  );
begin
  Assert(Assigned(AClickEventHandler),
    ClassName + '.Create: AClickEventHandler not assigned');
  inherited Create(AOwner);
  Caption := cCompResCaptions[ACompRes];
  fCompileResult := ACompRes;
  ImageIndex := Ord(ACompRes);  // image list index must map to compile result
  OnClick := AClickEventHandler;
end;

{ TCompResSelectMenu }

constructor TCompResSelectMenu.Create(AOwner: TComponent);
  {Object constructor. Creates menu and populates it with items for all possible
  compile results.
    @param AOwner [in] Component that owns the menu.
  }
var
  CompRes: TCompileResult;  // loops thru all possible compile results
begin
  inherited;
  AutoPopup := False;
  // Create image list containing compiler result LEDs
  Images := TLEDImageList.Create(AOwner);
  // Populate menu
  for CompRes := Low(TCompileResult) to High(TCompileResult) do
    Items.Add(TCompResMenuItem.Create(Self, CompRes, MenuItemClickHandler));
  // Set default "unknown" compiler result
  fCompileResult := crQuery;
end;

procedure TCompResSelectMenu.MenuItemClickHandler(Sender: TObject);
  {Handles clicks on all menu items. Records compile result associated with
  selected menu item in CompileResult property and triggers OnSelect event.
    @param Sender [in] Menu item that triggered event.
  }
begin
  fCompileResult := (Sender as TCompResMenuItem).CompileResult;
  if Assigned(fOnSelect) then
    fOnSelect(Self);
end;

end.

