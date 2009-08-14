{
 * UChkListStateMgr.pas
 *
 * Implements a class that modifies the behaviour of an associated check list
 * box so that clicks anywhere on a list item will toggle the state of the check
 * box.
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
  Classes, CheckLst;


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
    fOnClickSave: TNotifyEvent;       // check list's  orig OnClick handler
    fOnClickCheckSave: TNotifyEvent;  // check list's orig OnClickSave handler
    fOnStateChange: TNotifyEvent;     // ref to OnStateChange event handler
    fCheckClicked: Boolean;           // flag indicating if check box clicked
    procedure ToggleState;
      {Toggles state of check box for selected list item. Takes account of
      greyed state when AllowGrayed is true.
      }
    procedure ClickHandler(Sender: TObject);
      {Handles check list box's OnClick event. Event is passed list box's
      original handler, if any.
        @param Sender [in] List box that triggered event.
      }
    procedure ClickCheckHandler(Sender: TObject);
      {Handles check list box's OnClickCheck event. Event is passed list box's
      original handler, if any.
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
  {Handles check list box's OnClickCheck event. Event is passed list box's
  original handler, if any.
    @param Sender [in] List box that triggered event.
  }
begin
  fCheckClicked := True;
  StateChange;
  if Assigned(fOnClickCheckSave) then
    fOnClickCheckSave(fCLB);
end;

procedure TChkListStateMgr.ClickHandler(Sender: TObject);
  {Handles check list box's OnClick event. Event is passed list box's original
  handler, if any.
    @param Sender [in] List box that triggered event.
  }
begin
  if not fCheckClicked then
    ToggleState
  else
    fCheckClicked := False;
  if Assigned(fOnClickSave) then
    fOnClickSave(Sender);
end;

constructor TChkListStateMgr.Create(const CLB: TCheckListBox);
  {Class constructor. Sets up object.
    @param CLB [in] Check list box control to be managed.
  }
begin
  Assert(Assigned(CLB), ClassName + '.Create: CLB is nil');
  inherited Create;
  fCLB := CLB;
  fOnClickSave := CLB.OnClick;
  fOnClickCheckSave := CLB.OnClickCheck;
  CLB.OnClick := ClickHandler;
  CLB.OnClickCheck := ClickCheckHandler;
end;

destructor TChkListStateMgr.Destroy;
  {Class desctructor. Restores check list box's event handlers.
  }
begin
  fCLB.OnClick := fOnClickSave;
  fCLB.OnClickCheck := fOnClickCheckSave;
  inherited;
end;

procedure TChkListStateMgr.StateChange;
  {Triggers OnStateChange event.
  }
begin
  if Assigned(fOnStateChange) then
    fOnStateChange(fCLB);
end;

procedure TChkListStateMgr.ToggleState;
  {Toggles state of check box for selected list item. Takes account of greyed
  state when AllowGrayed is true.
  }
var
  State: TCheckBoxState;  // new state of check box
  Index: Integer;         // index of item whose check state is being toggled
begin
  Index := fCLB.ItemIndex;
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

