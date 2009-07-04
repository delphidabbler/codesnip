{
 * UMultiCastEvents.pas
 *
 * Implements class that maintains, and can trigger, a list of event handlers.
 *
 * v1.0 of 15 Jun 2008  - Original version.
 * v1.1 of 11 Aug 2008  - Added compiler directives to switch off "unsafe"
 *                        compiler warnings.
 * v2.0 of 14 Sep 2008  - Major revision:
 *                        - Added TNotifyEventInfo event type that passes an
 *                          IInterface of an object that provides information
 *                          about an event.
 *                        - Modified TMultiCastEvents to support registration
 *                          and triggering of both TNotifyEventInfo and
 *                          TNotifyEvents.
 *                        - Trigger method modified to take event info object.
 *                        - Events now receive reference to object that owns
 *                          the TMultiCastEvents object instead of the
 *                          TMultiCastEvents object itself.
 *
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
 * The Original Code is UMultiCastEvents.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UMultiCastEvents;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}

interface


uses
  // Delphi
  Classes, Contnrs;


type

  {
  TNotifyEventInfo:
    Type of event that passes an object describing the event to the handler.
      @param Sender [in] Reference to object triggering event.
      @param EvtInfo [in] IInterface of an object that describes the event.
  }
  TNotifyEventInfo = procedure(Sender: TObject; const EvtInfo: IInterface)
    of object;

  {
  TMultiCastEvents:
    Class that maintains, and can trigger, a list of event handlers.
  }
  TMultiCastEvents = class(TObject)
  private
    fOwner: TObject;
      {Object that owns this one and triggers events}
    fHandlers: TObjectList;
      {Stores list of event handlers}
    function IndexOfHandler(const Handler: TMethod): Integer;
      {Finds index of a registered event handler in list of handlers.
        @param Handler [in] Reference to event handler to find.
        @return Index of event handler in list or -1 if handler not found.
      }
  public
    constructor Create(AOwner: TObject = nil);
      {Class constructor. Sets up object.
        @param AOwner [in] Object that owns this object and that triggers
          events. Passed as Sender param to event handlers. May by nil.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure AddHandler(const Handler: TNotifyEvent); overload;
      {Adds a TNotifyEvent event handler to end of event handler list.
        @param Handler [in] Reference to event handler being added.
      }
    procedure AddHandler(const Handler: TNotifyEventInfo); overload;
      {Adds a TNotifyEventInfo event handler to end of event handler list.
        @param Handler [in] Reference to event handler being added.
      }
    procedure RemoveHandler(const Handler: TNotifyEvent); overload;
      {Removes a TNotifyEvent event handler from list. Does nothing if handler
      is not in list.
        @param Reference to event handler to be removed.
      }
    procedure RemoveHandler(const Handler: TNotifyEventInfo); overload;
      {Removes a TNotifyEventInfo event handler from list. Does nothing if
      handler is not in list.
        @param Reference to event handler to be removed.
      }
    procedure TriggerEvents(const EvtInfo: IInterface = nil);
      {Triggers all registered event handlers in order they were added.
        @param EvtInfo [in] Interface to object that provides information about
          the event. May be nil. Passed to event handler if it is of type
          TNotifyEventInfo.
      }
    function Count: Integer;
      {Gets number of registered event handlers.
        @return Number of handlers.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


type

  {
  TEventWrapper:
    Abstract base class for classes that wrap event handler methods in order to
    be able to store them in an object list.
  }
  TEventWrapper = class(TObject)
  private
    fOwner: TObject;
      {Owner of event handler. Used as Sender parameter when calling event
      handler}
    fHandler: TMethod;
      {Reference to event handler}
  public
    constructor Create(const Owner: TObject; const Handler: TMethod);
      {Class constructor. Sets up wrapper object for an event handler method.
        @param Owner [in] Reference to object that is to passed as Sender
          parameter of triggered event.
        @param Handler [in] Reference to event handler to be wrapped.
      }
    procedure Trigger(const EvtInfo: IInterface); virtual; abstract;
      {Triggers event.
        @param EvtInfo [in] Interface to object that provides information about
          the event. May be nil.
      }
    property Handler: TMethod read fHandler;
      {Reference to wrapped event handler}
    property Owner: TObject read fOwner;
      {Owner of event handler. Used as Sender parameter when calling event
      handler}
  end;

  {
  TNotifyEventWrapper:
    Class used to wrap a TNotifyEvent method in order to be able to store it in
    an object list.
  }
  TNotifyEventWrapper = class(TEventWrapper)
  public
    procedure Trigger(const EvtInfo: IInterface); override;
      {Triggers event.
        @param EvtInfo [in] Ignored.
      }
  end;

  TNotifyEventInfoWrapper = class(TEventWrapper)
  public
    procedure Trigger(const EvtInfo: IInterface); override;
      {Triggers event.
        @param EvtInfo [in] Interface to object that provides information about
          the event. May be nil.
      }
  end;

function IsEqualMethod(const M1, M2: TMethod): Boolean;
  {Checks if two methods are equal, i.e. both methods refer to the same method
  in same object.
    @param M1 [in] Reference to first method.
    @parma M2 [in] Reference to second method.
    @return True if methods are equal and False if methods not equal.
  }
begin
  Result := (M1.Code = M2.Code) and (M1.Data = M2.Data);
end;

{ TMultiCastEvents }

procedure TMultiCastEvents.AddHandler(const Handler: TNotifyEvent);
  {Adds a TNotifyEvent event handler to end of event handler list.
    @param Handler [in] Reference to event handler being added.
  }
begin
  if IndexOfHandler(TMethod(Handler)) = -1 then
    fHandlers.Add(TNotifyEventWrapper.Create(fOwner, TMethod(Handler)));
end;

procedure TMultiCastEvents.AddHandler(const Handler: TNotifyEventInfo);
  {Adds a TNotifyEventInfo event handler to end of event handler list.
    @param Handler [in] Reference to event handler being added.
  }
begin
  if IndexOfHandler(TMethod(Handler)) = -1 then
    fHandlers.Add(TNotifyEventInfoWrapper.Create(fOwner, TMethod(Handler)));
end;

function TMultiCastEvents.Count: Integer;
  {Gets number of registered event handlers.
    @return Number of handlers.
  }
begin
  Result := fHandlers.Count;
end;

constructor TMultiCastEvents.Create(AOwner: TObject);
  {Class constructor. Sets up object.
    @param AOwner [in] Object that owns this object and that triggers events.
    Passed as Sender param to event handlers. May by nil.
  }
begin
  inherited Create;
  fOwner := AOwner;
  fHandlers := TObjectList.Create(True);
end;

destructor TMultiCastEvents.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fHandlers);  // frees contained objects
  inherited;
end;

function TMultiCastEvents.IndexOfHandler(const Handler: TMethod): Integer;
  {Finds index of a registered event handler in list of handlers.
    @param Handler [in] Reference to event handler to find.
    @return Index of event handler in list or -1 if handler not found.
  }
var
  Idx: Integer; // loops thru all event handlers in list
begin
  Result := -1;
  for Idx := 0 to Pred(fHandlers.Count) do
  begin
    if IsEqualMethod(
      (fHandlers[Idx] as TEventWrapper).Handler, Handler
    ) then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

procedure TMultiCastEvents.RemoveHandler(const Handler: TNotifyEventInfo);
  {Removes a TNotifyEventInfo event handler from list. Does nothing if handler
  is not in list.
    @param Reference to event handler to be removed.
  }
var
  Idx: Integer; // index of handler in list (-1 if not in list)
begin
  Idx := IndexOfHandler(TMethod(Handler));
  if Idx >= 0 then
    fHandlers.Delete(Idx);  // frees deleted object
end;

procedure TMultiCastEvents.RemoveHandler(const Handler: TNotifyEvent);
  {Removes a TNotifyEvent event handler from list. Does nothing if handler is
  not in list.
    @param Reference to event handler to be removed.
  }
var
  Idx: Integer; // index of handler in list (-1 if not in list)
begin
  Idx := IndexOfHandler(TMethod(Handler));
  if Idx >= 0 then
    fHandlers.Delete(Idx);  // frees deleted object
end;

procedure TMultiCastEvents.TriggerEvents(const EvtInfo: IInterface = nil);
  {Triggers all registered event handlers in order they were added.
    @param EvtInfo [in] Interface to object that provides information about the
      event. May be nil. Passed to event handler if it is of type
      TNotifyEventInfo.
  }
var
  Idx: Integer; // loops thru all event handlers in list
begin
  for Idx := 0 to Pred(fHandlers.Count) do
    (fHandlers[Idx] as TEventWrapper).Trigger(EvtInfo);
end;

{ TEventWrapper }

constructor TEventWrapper.Create(const Owner: TObject; const Handler: TMethod);
  {Class constructor. Sets up wrapper object for an event handler method.
    @param Owner [in] Reference to object that is to passed as Sender parameter
      of triggered event.
    @param Handler [in] Reference to event handler to be wrapped.
  }
begin
  inherited Create;
  fOwner := Owner;
  fHandler := Handler;
end;

{ TNotifyEventWrapper }

procedure TNotifyEventWrapper.Trigger(const EvtInfo: IInterface);
  {Triggers event.
    @param EvtInfo [in] Ignored.
  }
begin
  TNotifyEvent(fHandler)(fOwner);
end;

{ TNotifyEventInfoWrapper }

procedure TNotifyEventInfoWrapper.Trigger(const EvtInfo: IInterface);
  {Triggers event.
    @param EvtInfo [in] Interface to object that provides information about
      the event. May be nil.
  }
begin
  TNotifyEventInfo(fHandler)(fOwner, EvtInfo);
end;

end.

