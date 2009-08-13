{
 * UMessageWindow.pas
 *
 * Implements a hidden window that triggers an event for each message received.
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
 * The Original Code is UMessageWindow.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  private
    fOnMessage: TMessageWindowEvent;
      {Handler for OnMessage event}
    function DoMessage(var Msg: TMessage): Boolean;
      {Triggers the OnMessage event.
        @param Msg [in/out] Message to be passed to event handler. Unchanged
          here but may be modified by event handler.
        @return True if message handled (any further processing inhibited),
          False otherwise.
      }
  protected
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

