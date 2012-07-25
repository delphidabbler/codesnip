{
 * UMultiCastEvents.pas
 *
 * Implements class that maintains, and can trigger, a list of event handlers.
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
 * The Original Code is UMultiCastEvents.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UMultiCastEvents;


interface


uses
  // Delphi
  Classes, Generics.Collections;


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
  TEventWrapper:
    Abstract base class for classes that wrap event handler methods in order to
    be able to store them in an object list.
  }
  TEventWrapper = class abstract(TObject)
  strict private
    fOwner: TObject;
      {Owner of event handler. Used as Sender parameter when calling event
      handler}
    fHandler: TMethod;
      {Reference to event handler}
  public
    constructor Create(const Owner: TObject; const Handler: TMethod);
      {Constructor. Sets up wrapper object for an event handler method.
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
  TMultiCastEvents:
    Class that maintains, and can trigger, a list of event handlers.
  }
  TMultiCastEvents = class(TObject)
  strict private
    var
      fOwner: TObject;                        // Object that triggers events
      fHandlers: TObjectList<TEventWrapper>;  // Stores list of event handlers
    function IndexOfHandler(const Handler: TMethod): Integer;
      {Finds index of a registered event handler in list of handlers.
        @param Handler [in] Reference to event handler to find.
        @return Index of event handler in list or -1 if handler not found.
      }
  public
    constructor Create(AOwner: TObject = nil);
      {Constructor. Sets up object.
        @param AOwner [in] Object that owns this object and that triggers
          events. Passed as Sender param to event handlers. May by nil.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
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

  {
  TNotifyEventInfoWrapper:
    Class used to wrap a TNotifyEventInfo method in order to be able to store it
    in an object list.
  }
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
  {Constructor. Sets up object.
    @param AOwner [in] Object that owns this object and that triggers events.
    Passed as Sender param to event handlers. May by nil.
  }
begin
  inherited Create;
  fOwner := AOwner;
  fHandlers := TObjectList<TEventWrapper>.Create(True);
end;

destructor TMultiCastEvents.Destroy;
  {Destructor. Tears down object.
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
    if IsEqualMethod(fHandlers[Idx].Handler, Handler) then
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
  HandlerWrapper: TEventWrapper;  // enumerates event handler wrappers
begin
  for HandlerWrapper in fHandlers do
    HandlerWrapper.Trigger(EvtInfo);
end;

{ TEventWrapper }

constructor TEventWrapper.Create(const Owner: TObject; const Handler: TMethod);
  {Constructor. Sets up wrapper object for an event handler method.
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
  TNotifyEvent(Handler)(Owner);
end;

{ TNotifyEventInfoWrapper }

procedure TNotifyEventInfoWrapper.Trigger(const EvtInfo: IInterface);
  {Triggers event.
    @param EvtInfo [in] Interface to object that provides information about
      the event. May be nil.
  }
begin
  TNotifyEventInfo(Handler)(Owner, EvtInfo);
end;

end.

