{
 * UMemoCaretPosDisplayMgr.pas
 *
 * Displays the caret position of one or more memo controls in associated label
 * controls. Labels are automatically updated whenever the caret position
 * changes.
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
 * The Original Code is UMemoCaretPosDisplayMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UMemoCaretPosDisplayMgr;


interface


uses
  // Delphi
  Generics.Collections, Classes, Controls, StdCtrls;

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
      // Record of values associated with memo control
      TAssociations = record
        OnKeyUp: TKeyEvent;     // original OnKeyUp event handler
        OnMouseUp: TMouseEvent; // original OnMouseUp event handler
        OnEnter: TNotifyEvent;  // original OnEnter event handler
        DisplayCtrl: TLabel;    // label in which to display caret info
      end;
    var
      // Maps memo to associated display label and saved event handlers
      fMap: TDictionary<TMemo,TAssociations>;
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
    procedure UpdateCaretPos(const SourceCtrl: TMemo);
      {Updates display of a memo control's caret position.
        @param SourceCtrl [in] Memo whose caret position to be displayed.
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
      {Registers a memo control to have caret position displayed in an
      associated label.
        @param SourceCtrl [in] Memo control whose caret position is to be
          displayed.
        @param DisplayCtrl [in] Label used to display caret position.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


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
  end;
  fMap.Free;
  inherited;
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
  Assert(not fMap.ContainsKey(SourceCtrl),
    ClassName + '.Manage: Source memo already managed');
  // save old event handlers
  Associations.OnKeyUp := SourceCtrl.OnKeyUp;
  Associations.OnMouseUp := SourceCtrl.OnMouseUp;
  Associations.OnEnter := SourceCtrl.OnEnter;
  // record display label
  Associations.DisplayCtrl := DisplayCtrl;
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
  SavedOnMouseUp: TMouseEvent;// original event handler
begin
  // call any original event hander
  SavedOnMouseUp := fMap[Sender as TMemo].OnMouseUp;
  if Assigned(SavedOnMouseUp) then
    SavedOnMouseUp(Sender, Button, Shift, X, Y);
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

end.
