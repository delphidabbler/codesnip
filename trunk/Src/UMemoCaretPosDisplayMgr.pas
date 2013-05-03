{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Displays the caret position of one or more memo controls in associated label
 * controls. Labels are automatically updated whenever the caret position
 * changes.
}


unit UMemoCaretPosDisplayMgr;


interface


uses
  // Delphi
  Generics.Collections, Classes, Controls, StdCtrls, Messages;

type

  {
  TMemoCaretPosDisplayMgr:
    Class that displays the caret position of one or more memo controls in
    associated label controls. Labels are automatically updated whenever the
    caret position changes.
  }
  TMemoCaretPosDisplayMgr = class(TObject)
  strict private
    type
      {
      TMemoHook:
        Class used to hook into a memo control's message loop and detect
        selection changes, triggering an event when detected.
      }
      TMemoHook = class(TObject)
      strict private
        var
          fMemo: TMemo;               // Memo control to be hooked
          fMemoWndProc: Pointer;      // Memo's original
          fWndProcHook: Pointer;      // New hook window procedure
          fOnSelChange: TNotifyEvent; // OnSelChange event handler
        procedure WndProcHook(var Msg: TMessage);
          {Window procedure that replaces and calls into memo control's own
          window procedure. Detects selection changes and triggers OnSelChange
          event.
            @param Msg [in/out] Contains information about message. Result field
              updated with message return value.
          }
        function SetWndProc(WndProc: Pointer): Pointer;
          {Assigns a new window procedure to memo control.
            @param WndProc [in] Pointer to new window procedure.
            @return Pointer to old window procedure.
          }
      public
        constructor Create(const AMemo: TMemo);
          {Object constructor. Creates hook object for a specified memo.
            @param AMemo [in] Memo control to be hooked.
          }
        destructor Destroy; override;
          {Object destructor. Restores memo's original window procedure.
          }
        property OnSelChange: TNotifyEvent read fOnSelChange write fOnSelChange;
          {Event triggered when a selection change in memo control is detected}
      end;
    type
      // Record of values associated with memo control
      TAssociations = record
        OnKeyUp: TKeyEvent;     // Original OnKeyUp event handler
        OnMouseUp: TMouseEvent; // Original OnMouseUp event handler
        OnEnter: TNotifyEvent;  // Original OnEnter event handler
        Hook: TMemoHook;        // Object that hooks memo's window proc
        DisplayCtrl: TLabel;    // Label in which to display caret info
      end;
    var fMap: TDictionary<TMemo,TAssociations>;
      {Maps memo to associated display label and saved event handlers}
    procedure OnKeyUpHandler(Sender: TObject; var Key: Word;
      Shift: TShiftState);
      {OnKeyUp event handler for managed memo controls. Calls any original event
      handler then updates caret position display.
        @param Sender [in] Memo control that triggered event.
        @param Key [in/out] Not used. Passed to any original event handler.
        @param Shift [in] Not used. Passed to any original event handler.
      }
    procedure OnMouseUpHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      {OnMouseUp event handler for managed memo controls. Calls any original
      event handler then updates caret position display.
        @param Sender [in] Memo control that triggered event.
        @param Button [in] Not used. Passed to any original event handler.
        @param Shift [in] Not used. Passed to any original event handler.
        @param X [in] Not used. Passed to any original event handler.
        @param Y [in] Not used. Passed to any original event handler.
      }
    procedure OnEnterHandler(Sender: TObject);
      {OnEnter event handler for managed memo controls. Calls any original event
      handler then updates caret position display.
        @param Sender [in] Memo control that triggered event.
      }
    procedure OnSelChangeHandler(Sender: TObject);
      {Handles events triggered when selection changes are reported be memo
      hook. Updates caret position display.
        @param Sender [in] Memo control that triggered event.
      }
    procedure UpdateCaretPos(const SourceCtrl: TMemo);
      {Updates display of a memo control's caret position.
        @param SourceCtrl [in] Memo whose caret position to be displayed.
      }
    function FindFirstMemoControl(const ParentCtrl: TWinControl): TMemo;
      {Finds first memo child control of given parent control.
        @param ParentCtrl [in] Parent containing required memo control.
        @return Reference to memo control or nil if ParentCtrl has no memo
          child control.
      }
  public
    constructor Create;
      {Object constructor. Sets up object.
      }
    destructor Destroy; override;
      {Object destructor. Restores original event handlers to managed memo
      controls then clears up object.
      }
    procedure Manage(const SourceCtrl: TMemo; const DisplayCtrl: TLabel);
      overload;
      {Registers a memo control to have caret position displayed in an
      associated label. NOTE: This method must only be called once the memo
      control has initialised otherwise selection change events will not be
      detected. Calling during a form's OnShow event is recommended.
        @param SourceCtrl [in] Memo control whose caret position is to be
          displayed.
        @param DisplayCtrl [in] Label used to display caret position.
      }
    procedure Manage(const ParentCtrl: TWinControl; const DisplayCtrl: TLabel);
      overload;
      {Registers first memo child control of given parent control to have caret
      position displayed in an associated label. NOTE 1: This method must only
      be called once the memo control has initialised otherwise selection change
      events will not be detected. Calling during a form's OnShow event is
      recommended. NOTE 2: Parent control MUST have at least one memo control as
      a child.
        @param ParentCtrl [in] Parent of memo control whose caret position is to
          be displayed.
        @param DisplayCtrl [in] Label used to display caret position.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows;


{ TMemoCaretPosDisplayMgr }

constructor TMemoCaretPosDisplayMgr.Create;
  {Object constructor. Sets up object.
  }
begin
  inherited Create;
  fMap := TDictionary<TMemo,TAssociations>.Create;
end;

destructor TMemoCaretPosDisplayMgr.Destroy;
  {Object destructor. Restores original event handlers to managed memo controls
  then clears up object.
  }
var
  DataPair: TPair<TMemo,TAssociations>; // each key and value pair from map
  SourceCtrl: TMemo;                    // each memo control in map
  Associations: TAssociations;          // associated data for each SourceCtrl
begin
  for DataPair in fMap do
  begin
    // restore saved event handlers
    SourceCtrl := DataPair.Key;
    Associations := DataPair.Value;
    SourceCtrl.OnKeyUp := Associations.OnKeyUp;
    SourceCtrl.OnMouseUp := Associations.OnMouseUp;
    SourceCtrl.OnEnter := Associations.OnEnter;
    Associations.Hook.Free;
  end;
  fMap.Free;
  inherited;
end;

function TMemoCaretPosDisplayMgr.FindFirstMemoControl(
  const ParentCtrl: TWinControl): TMemo;
  {Finds first memo child control of given parent control.
    @param ParentCtrl [in] Parent containing required memo control.
    @return Reference to memo control or nil if ParentCtrl has no memo child
      control.
  }
var
  Ctrl: TControl;
  Idx: Integer;
begin
  for Idx := 0 to Pred(ParentCtrl.ControlCount) do
  begin
    Ctrl := ParentCtrl.Controls[Idx];
    if Ctrl is TMemo then
      Exit(Ctrl as TMemo);
  end;
  Result := nil;
end;

procedure TMemoCaretPosDisplayMgr.Manage(const ParentCtrl: TWinControl;
  const DisplayCtrl: TLabel);
  {Registers first memo child control of given parent control to have caret
  position displayed in an associated label. NOTE 1: This method must only be
  called once the memo control has initialised otherwise selection change events
  will not be detected. Calling during a form's OnShow event is recommended.
  NOTE 2: Parent control MUST have at least one memo control as a child.
    @param ParentCtrl [in] Parent of memo control whose caret position is to be
      displayed.
    @param DisplayCtrl [in] Label used to display caret position.
  }
begin
  Manage(FindFirstMemoControl(ParentCtrl), DisplayCtrl);
end;

procedure TMemoCaretPosDisplayMgr.Manage(const SourceCtrl: TMemo;
  const DisplayCtrl: TLabel);
  {Registers a memo control to have caret position displayed in an associated
  label.
    @param SourceCtrl [in] Memo control whose caret position is to be displayed.
    @param DisplayCtrl [in] Label used to display caret position.
  }
var
  Associations: TAssociations;  // data to be associated with memo control
begin
  Assert(Assigned(SourceCtrl), ClassName + '.Manage: SourceCtrl is nil');
  Assert(Assigned(DisplayCtrl), ClassName + '.Manage: DisplayCtrl is nil');
  Assert(not fMap.ContainsKey(SourceCtrl),
    ClassName + '.Manage: Source memo already managed');
  // save old event handlers
  Associations.OnKeyUp := SourceCtrl.OnKeyUp;
  Associations.OnMouseUp := SourceCtrl.OnMouseUp;
  Associations.OnEnter := SourceCtrl.OnEnter;
  // record display label
  Associations.DisplayCtrl := DisplayCtrl;
  // add menu hook object
  Associations.Hook := TMemoHook.Create(SourceCtrl);
  Associations.Hook.OnSelChange := OnSelChangeHandler;
  // hook required event handlers (each of these calls any saved handler)
  SourceCtrl.OnKeyUp := OnKeyUpHandler;
  SourceCtrl.OnMouseUp := OnMouseUpHandler;
  SourceCtrl.OnEnter := OnEnterHandler;
  // add control to list of managed controls
  fMap.Add(SourceCtrl, Associations);
  // initialise caret position display
  UpdateCaretPos(SourceCtrl);
end;

procedure TMemoCaretPosDisplayMgr.OnEnterHandler(Sender: TObject);
  {OnEnter event handler for managed memo controls. Calls any original event
  handler then updates caret position display.
    @param Sender [in] Memo control that triggered event.
  }
var
  SavedOnEnter: TNotifyEvent; // original event handler
begin
  // call any original event hander
  SavedOnEnter := fMap[Sender as TMemo].OnEnter;
  if Assigned(SavedOnEnter) then
    SavedOnEnter(Sender);
  UpdateCaretPos(Sender as TMemo);
end;

procedure TMemoCaretPosDisplayMgr.OnKeyUpHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {OnKeyUp event handler for managed memo controls. Calls any original event
  handler then updates caret position display.
    @param Sender [in] Memo control that triggered event.
    @param Key [in/out] Not used. Passed to any original event handler.
    @param Shift [in] Not used. Passed to any original event handler.
  }
var
  SavedOnKeyUp: TKeyEvent;  // original event handler
begin
  // call any original event hander
  SavedOnKeyUp := fMap[Sender as TMemo].OnKeyUp;
  if Assigned(SavedOnKeyUp) then
    SavedOnKeyUp(Sender, Key, Shift);
  UpdateCaretPos(Sender as TMemo);
end;

procedure TMemoCaretPosDisplayMgr.OnMouseUpHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  {OnMouseUp event handler for managed memo controls. Calls any original event
  handler then updates caret position display.
    @param Sender [in] Memo control that triggered event.
    @param Button [in] Not used. Passed to any original event handler.
    @param Shift [in] Not used. Passed to any original event handler.
    @param X [in] Not used. Passed to any original event handler.
    @param Y [in] Not used. Passed to any original event handler.
  }
var
  SavedOnMouseUp: TMouseEvent;  // original event handler
begin
  // call any original event hander
  SavedOnMouseUp := fMap[Sender as TMemo].OnMouseUp;
  if Assigned(SavedOnMouseUp) then
    SavedOnMouseUp(Sender, Button, Shift, X, Y);
  UpdateCaretPos(Sender as TMemo);
end;

procedure TMemoCaretPosDisplayMgr.OnSelChangeHandler(Sender: TObject);
  {Handles events triggered when selection changes are reported be memo hook.
  Updates caret position display.
    @param Sender [in] Memo control that triggered event.
  }
begin
  UpdateCaretPos(Sender as TMemo);
end;

procedure TMemoCaretPosDisplayMgr.UpdateCaretPos(const SourceCtrl: TMemo);
  {Updates display of a memo control's caret position.
    @param SourceCtrl [in] Memo whose caret position to be displayed.
  }
var
  DisplayCtrl: TLabel;  // label in which caret position to be displayed
begin
  DisplayCtrl := fMap[SourceCtrl].DisplayCtrl;
  DisplayCtrl.Caption := Format(
    '%d: %d', [SourceCtrl.CaretPos.Y, SourceCtrl.CaretPos.X]
  );
end;

{ TMemoCaretPosDisplayMgr.TMemoHook }

constructor TMemoCaretPosDisplayMgr.TMemoHook.Create(const AMemo: TMemo);
  {Object constructor. Creates hook object for a specified memo.
    @param AMemo [in] Memo control to be hooked.
  }
begin
  inherited Create;
  fMemo := AMemo;
  // hook memo's window procedure
  fWndProcHook := Classes.MakeObjectInstance(WndProcHook);
  fMemoWndProc := SetWndProc(fWndProcHook);
end;

destructor TMemoCaretPosDisplayMgr.TMemoHook.Destroy;
  {Object destructor. Restores memo's original window procedure.
  }
begin
  fOnSelChange := nil;
  if Assigned(fWndProcHook) then
  begin
    // restore original window procedure
    SetWndProc(fMemoWndProc);
    Classes.FreeObjectInstance(fWndProcHook);
  end;
  inherited;
end;

function TMemoCaretPosDisplayMgr.TMemoHook.SetWndProc(
  WndProc: Pointer): Pointer;
  {Assigns a new window procedure to memo control.
    @param WndProc [in] Pointer to new window procedure.
    @return Pointer to old window procedure.
  }
begin
  Result := Pointer(
    SetWindowLongPtr(fMemo.Handle, GWL_WNDPROC, LONG_PTR(WndProc))
  );
end;

procedure TMemoCaretPosDisplayMgr.TMemoHook.WndProcHook(var Msg: TMessage);
  {Window procedure that replaces and calls into memo control's own window
  procedure. Detects selection changes and triggers OnSelChange event.
    @param Msg [in/out] Contains information about message. Result field updated
      with message return value.
  }
begin
  Msg.Result := CallWindowProc(
    fMemoWndProc, fMemo.Handle, Msg.Msg, Msg.WParam, Msg.LParam
  );
  if (Msg.Msg = EM_SETSEL) and Assigned(fOnSelChange) then
    fOnSelChange(fMemo);
end;

end.

