{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a hidden window that triggers an event for each message received.
}


unit UMessageWindow;


interface


uses
  // Delphi
  Messages,
  // Project
  UHiddenWindow;


type

  {
  TMessageWindowEvent:
    Prototype of event triggered by TMessageWindow's OnMessage event. Handler
    can choose to process or ignore a message. If message is processed, default
    processing can be inhibited.
      @param Sender [in] Reference to object triggering event.
      @param Msg [in/out] Message to be handled. May be modified in event
        handler.
      @param Handled [in/out] False when passed in. Set to true if message has
        been handled and default processing is to be inhibited.
  }
  TMessageWindowEvent = procedure(Sender: TObject; var Msg: TMessage;
    var Handled: Boolean) of object;

  {
  TMessageWindow:
    Hidden window that triggers an event for each message received. The user can
    handle or ignore the messages. Default processing for handled messages can
    be inhibited.
  }
  TMessageWindow = class(THiddenWindow)
  strict private
    fOnMessage: TMessageWindowEvent;
      {Handler for OnMessage event}
    function DoMessage(var Msg: TMessage): Boolean;
      {Triggers the OnMessage event.
        @param Msg [in/out] Message to be passed to event handler. Unchanged
          here but may be modified by event handler.
        @return True if message handled (any further processing inhibited),
          False otherwise.
      }
  strict protected
    procedure WndProc(var Msg: TMessage); override;
      {Window procedure for hidden window. Triggers OnMessage event. Performs
      default message processing if OnMessage handler does not inhibit it.
        @param Msg [in/out] Message being process. Unchanged here. May be
          modified by OnMessage handler or by default processing.
      }
  public
    property OnMessage: TMessageWindowEvent
      read fOnMessage write fOnMessage;
      {Event triggered for each message received by the window}
  end;

implementation

{ TMessageWindow }

function TMessageWindow.DoMessage(var Msg: TMessage): Boolean;
  {Triggers the OnMessage event.
    @param Msg [in/out] Message to be passed to event handler. Unchanged here
      but may be modified by event handler.
    @return True if message handled (any further processing inhibited), False
      otherwise.
  }
begin
  Result := False;
  if Assigned(fOnMessage) then
    fOnMessage(Self, Msg, Result);
end;

procedure TMessageWindow.WndProc(var Msg: TMessage);
  {Window procedure for hidden window. Triggers OnMessage event. Performs
  default message processing if OnMessage handler does not inhibit it.
    @param Msg [in/out] Message being process. Unchanged here. May be modified
      by OnMessage handler or by default processing.
  }
begin
  if not DoMessage(Msg) then
    inherited WndProc(Msg);
end;

end.

