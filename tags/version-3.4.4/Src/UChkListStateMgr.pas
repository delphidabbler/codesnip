{
 * UChkListStateMgr.pas
 *
 * Implements a class that modifies the behaviour of an associated check list
 * box so that clicks anywhere on a list item will toggle the state of the check
 * box. Default behaviour is that only clicks on check box change the state.
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
 * The Original Code is UChkListStateMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UChkListStateMgr;


interface


uses
  // Delphi
  Classes, Controls, CheckLst;


type

  {
  TChkListStateMgr:
    Class that modifies the behaviour of an associated check list box so that
    clicks anywhere on a list item will toggle the state of the check box.
    This overrides standard behaviour which is that only clicks on the check
    box toggle the check state.
  }
  TChkListStateMgr = class(TObject)
  private
    fCLB: TCheckListBox;              // check list box (CLB) being managed
    fOnClickCheckSave: TNotifyEvent;  // check list's orig OnClickSave handler
    fOnMouseDownSave: TMouseEvent;    // check list's orig OnMouseDown handler
    fOnMouseUpSave: TMouseEvent;      // check list's orig OnMouseUp handler
    fOnStateChange: TNotifyEvent;     // ref to OnStateChange event handler
    fCheckClicked: Boolean;           // flag indicating if check box clicked
    fMouseDownItemIdx: Integer;       // index of elem where mouse went down
    procedure ToggleState(const Index: Integer);
      {Toggles state of check box for a list item. Takes account of greyed state
      when AllowGrayed is true. Triggers OnStateChange event.
        @param Index [in] Index of list item for which state should be changed.
      }
    procedure ClickCheckHandler(Sender: TObject);
      {Handles check list box's OnClickCheck event. Sets flag to prevent MouseUp
      event changing state and triggers state change event. Event is passed list
      box's original handler, if any.
        @param Sender [in] List box that triggered event.
      }
    procedure MouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      {Handles check list box's OnMouseDown event. Records list box item where
      mouse went down. Event is passed list box's original handler, if any.
        @param Sender [in] List box that triggered event.
      }
    procedure MouseUpHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      {Handles check list box's OnMouseUp event. Toggles state of check box if
      click was not on checkbox and mouse is on same list item as mouse down.
      Event is passed list box's original handler, if any.
        @param Sender [in] List box that triggered event.
      }
    procedure StateChange;
      {Triggers OnStateChange event.
      }
  public
    constructor Create(const CLB: TCheckListBox);
      {Class constructor. Sets up object.
        @param CLB [in] Check list box control to be managed.
      }
    destructor Destroy; override;
      {Class desctructor. Restores check list box's event handlers.
      }
    property OnStateChange: TNotifyEvent
      read fOnStateChange write fOnStateChange;
      {Event triggered when check box state changes as a result of user changing
      it}
  end;


implementation


uses
  // Delphi
  StdCtrls;


{ TChkListStateMgr }

procedure TChkListStateMgr.ClickCheckHandler(Sender: TObject);
  {Handles check list box's OnClickCheck event. Sets flag to prevent MouseUp
  event changing state and triggers state change event. Event is passed list
  box's original handler, if any.
    @param Sender [in] List box that triggered event.
  }
begin
  fCheckClicked := True;
  StateChange;
  if Assigned(fOnClickCheckSave) then
    fOnClickCheckSave(fCLB);
end;

constructor TChkListStateMgr.Create(const CLB: TCheckListBox);
  {Class constructor. Sets up object.
    @param CLB [in] Check list box control to be managed.
  }
begin
  Assert(Assigned(CLB), ClassName + '.Create: CLB is nil');
  inherited Create;
  fCLB := CLB;
  fOnClickCheckSave := CLB.OnClickCheck;
  fOnMouseDownSave := CLB.OnMouseDown;
  fOnMouseUpSave := CLB.OnMouseUp;
  CLB.OnClickCheck := ClickCheckHandler;
  CLB.OnMouseDown := MouseDownHandler;
  CLB.OnMouseUp := MouseUpHandler;
end;

destructor TChkListStateMgr.Destroy;
  {Class desctructor. Restores check list box's event handlers.
  }
begin
  fCLB.OnClickCheck := fOnClickCheckSave;
  fCLB.OnMouseDown := fOnMouseDownSave;
  fCLB.OnMouseUp := fOnMouseUpSave;
  inherited;
end;

procedure TChkListStateMgr.MouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  {Handles check list box's OnMouseDown event. Records list box item where mouse
  went down. Event is passed list box's original handler, if any.
    @param Sender [in] List box that triggered event.
  }
begin
  fMouseDownItemIdx := fCLB.ItemAtPos(Point(X, Y), True);
  if Assigned(fOnMouseDownSave) then
    fOnMouseDownSave(Sender, Button, Shift, X, Y);
end;

procedure TChkListStateMgr.MouseUpHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  {Handles check list box's OnMouseUp event. Toggles state of check box if click
  was not on checkbox and mouse is on same list item as mouse down. Event is
  passed list box's original handler, if any.
    @param Sender [in] List box that triggered event.
  }
var
  Idx: Integer; // index of item under mouse
begin
  // we only change state if:
  // 1) check box wasn't clicked: control changes state automatically if so
  // 2) mouse is still over same item as when mouse went down
  if not fCheckClicked then
  begin
    Idx := fCLB.ItemAtPos(Point(X, Y), True);
    if Idx = fMouseDownItemIdx then
      ToggleState(fMouseDownItemIdx)
  end
  else
    fCheckClicked := False;
  fMouseDownItemIdx := -1;
  if Assigned(fOnMouseUpSave) then
    fOnMouseUpSave(Sender, Button, Shift, X, Y);
end;

procedure TChkListStateMgr.StateChange;
  {Triggers OnStateChange event.
  }
begin
  if Assigned(fOnStateChange) then
    fOnStateChange(fCLB);
end;

procedure TChkListStateMgr.ToggleState(const Index: Integer);
  {Toggles state of check box for a list item. Takes account of greyed state
  when AllowGrayed is true. Triggers OnStateChange event.
    @param Index [in] Index of list item for which state should be changed.
  }
var
  State: TCheckBoxState;  // new state of check box
begin
  if Index >= 0 then
  begin
    State := fCLB.State[Index];
    // this code is based on that in CheckLst unit
    case State of
      cbUnchecked:
        if fCLB.AllowGrayed then
          State := cbGrayed
        else
          State := cbChecked;
      cbChecked:
        State := cbUnchecked;
      cbGrayed:
        State := cbChecked;
    end;
    fCLB.State[Index] := State;
    // Notify that state has changed
    StateChange;
  end;
end;

end.

