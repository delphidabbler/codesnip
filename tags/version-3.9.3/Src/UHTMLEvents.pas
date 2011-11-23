{
 * UHTMLEvents.pas
 *
 * Provides event sinks for the web browser control. Sinks for the browser's
 * document and window objects are implemented.
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
 * The Original Code is UHTMLEvents.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UHTMLEvents;


interface


uses
  // Delphi
  MSHTML, ActiveX,
  // Project
  USimpleDispatch;


type

  {
  THTMLEventInfo:
    Class that provides information about a HTML event.
  }
  THTMLEventInfo = class(TObject)
  strict private
    fCanCancel: Boolean;  // Value of CanCancel property
    fCancelled: Boolean;  // Value of Cancelled property
    fArgs: IHTMLEventObj; // Value of Args property
    fDispatchId: Integer; // Value of DispatchId property
    procedure SetCancelled(const Value: Boolean);
      {Write accessor for Cancelled property. Sets property only if CanCancel
      property is True.
        @param Value [in] New property value.
      }
    function GetName: string;
      {Read accessor for Name property.
        @return Name of event.
      }
  public
    constructor Create(const DispatchID: Integer;
      const Args: IHTMLEventObj; const CanCancel: Boolean);
      {Class constructor. Sets up object.
        @param DispatchId [in] Dispatch ID of the event.
        @param Args [in] Information about the event provided by browser.
        @param CanCancel [in] Flag indicating whether event can be cancelled or
          not.
      }
    property DispatchId: Integer read fDispatchId;
      {Dispatch ID of the event}
    property Name: string read GetName;
      {Name of the event. (Same as Args.type_)}
    property CanCancel: Boolean read fCanCancel;
      {Flag indicating whether event can be cancelled. When true the Cancelled
      property cannot be set}
    property Cancelled: Boolean read fCancelled write SetCancelled
      default False;
      {Flag indicating whether the event should be cancelled. Set to true to
      cancel the event. If CanCancel is false the value of Cancelled cannot be
      changed and is always False}
    property Args: IHTMLEventObj read fArgs;
      {Object exposing information about the event provided by browser object}
  end;

  {
  THTMLEvent:
    Type of the OnEvent event.
      @param Sender [in] Reference to object that triggered the event.
      @param EventInfo [in] Object providing information about the event.
  }
  THTMLEvent = procedure(Sender: TObject; const EventInfo: THTMLEventInfo)
    of object;

  {
  TAbstractHTMLEventSink:
    Abstract base class for non reference counted HTML event sinks. Provides
    core functionality and some helper methods.
  }
  TAbstractHTMLEventSink = class(TSimpleDispatch,
    IUnknown, IDispatch
  )
  strict private
    fSinkIID: TGUID;            // Supported event interface
    fSource: IDispatch;         // Reference to event source object
    fConnectionCookie: Integer; // Identifies connection between source and sink
    fOnEvent: THTMLEvent;       // Reference to any OnEvent handler
  strict protected
    procedure DoInvoke(var InvokeInfo: TInvokeInfo); override;
      {Dispatches events via abstract DispatchEvent method after checking for
      valid context.
        @param InvokeInfo [in/out] Information about method / property
          invocation. Structure is updated with information about invocation.
        @except EBug exception raised if invoked with an property or named
          method.
      }
    procedure DispatchEvent(var InvokeInfo: TInvokeInfo); virtual; abstract;
      {Abstract method called when a event is to be dispatched.
        @param InvokeInfo [in/out] Information about event invocation. Structure
          is to be updated with information about result of invocation.
      }
    procedure GetEventArgParam(var InvokeInfo: TInvokeInfo;
      out EventArgs: IHTMLEventObj);
      {Gets the single IHTMLEventObj parameter of a "standard" HTML event.
      Notifies caller of error if there is more than one parameter or if
      parameter is wrong type.
        @param InvokeInfo [in/out] Information about method invocation.
          Structure is updated with information about any error.
        @param EventArgs [out] Set to object giving information information
          about event arguments or nil if parameter is not valid.
      }
    procedure DispatchStdEvent(var InvokeInfo: TInvokeInfo;
      const CanCancel: Boolean);
      {Triggers given "standard" event, passing information about the event.
      Sets function result if event is a function. Notifies an error if method
      call is not a valid standard event.
        @param InvokeInfo [in/out] Information about event method invocation.
          Structure is updated with information about return value and any error
          condition.
        @param CanCancel [in] Flag indicating whether event can be cancelled.
      }
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      {Checks if the specified interface is supported by this object. If so a
      reference to the interface is passed out. Supported interfaces are
      IUnknown, IDispatch and the interface implemented by the event sink.
        @param IID [in] specifies interface being queried.
        @param Obj [out] Reference to interface implementation or nil if not
          supported.
        @return S_OK if interface supported or E_NOINTERFACE if not supported.
      }
  public
    constructor Create(const SinkIID: TGUID);
      {Class constructor. Sets up the object.
        @param SinkIID [in] Interface to events to be sunk by this object.
      }
    destructor Destroy; override;
      {Class destructor. Disconnects the sink from the event source and tears
      down the object.
      }
    procedure Connect(const Source: IDispatch);
      {Connects the event sink to the object that is the source of the events.
        @param Source [in] Reference to source object.
      }
    procedure Disconnect;
      {Disconnects the event sink from the event source object.
      }
    property OnEvent: THTMLEvent read fOnEvent write fOnEvent;
      {Event triggered when the browser notifies the sink of a "standard" event}
  end;

const
  // Event dispids from HTMLDocumentEvents2 dispinterface
  cDocEventOnHelp               = -2147418102;
  cDocEventOnClick              = -600;
  cDocEventOnDblClick           = -601;
  cDocEventOnKeyDown            = -602;
  cDocEventOnKeyUp              = -604;
  cDocEventOnKeyPress           = -603;
  cDocEventOnMouseDown          = -605;
  cDocEventOnMouseMove          = -606;
  cDocEventOnMouseUp            = -607;
  cDocEventOnMouseOut           = -2147418103;
  cDocEventOnMouseOver          = -2147418104;
  cDocEventOnReadyStateChange   = -609;
  cDocEventOnBeforeUpdate       = -2147418108;
  cDocEventOnAfterUpdate        = -2147418107;
  cDocEventOnRowExit            = -2147418106;
  cDocEventOnRowEnter           = -2147418105;
  cDocEventOnDragStart          = -2147418101;
  cDocEventOnSelectStart        = -2147418100;
  cDocEventOnErrorUpdate        = -2147418099;
  cDocEventOnContextMenu        = 1023;
  cDocEventOnStop               = 1026;
  cDocEventOnRowsDelete         = -2147418080;
  cDocEventOnRowsInserted       = -2147418079;
  cDocEventOnCellChange         = -2147418078;
  cDocEventOnPropertyChange     = -2147418093;
  cDocEventOnDatasetChanged     = -2147418098;
  cDocEventOnDataAvailable      = -2147418097;
  cDocEventOnDatasetComplete    = -2147418096;
  cDocEventOnBeforeEditFocus    = 1027;

type

  {
  THTMLDocEventSink:
    Event sink for HTML document events. Sinks HTMLDocumentEvents2 events.
  }
  THTMLDocEventSink = class(TAbstractHTMLEventSink,
    IUnknown, IDispatch
  )
  strict protected
    procedure DispatchEvent(var InvokeInfo: TInvokeInfo); override;
      {Dispatches HTMLDocumentEvents2 events.
        @param InvokeInfo [in] Structure containing information about the event
          to be dispatched. Fields of this structure are updated to notify
          result of event invocation.
      }
  public
    constructor Create;
      {Class constructor. Sets up object to sink HTMLDocumentEvents2 events.
      }
  end;

const
  // Event dispids from HTMLDocumentEvents2 dispinterface
  cWdwEventOnLoad = 1003;
  cWdwEventOnUnload = 1008;
  cWdwEventOnHelp = -2147418102;
  cWdwEventOnFocus = -2147418111;
  cWdwEventOnBlur = -2147418112;
  cWdwEventOnError = 1002;
  cWdwEventOnResize = 1016;
  cWdwEventOnScroll = 1014;
  cWdwEventOnBeforeUnload = 1017;
  cWdwEventOnBeforePrint = 1024;
  cWdwEventOnAfterPrint = 1025;

type

  {
  THTMLWdwErrorEvent:
    Type of the OnError event.
      @param Sender [in] Reference to object that triggered the event.
      @param Desc [in] Description of error.
      @param URL [in] URL of document triggering error.
      @param Line [in] Line number where error occurred.
  }
  THTMLWdwErrorEvent = procedure(Sender: TObject; const Desc, URL: string;
    const Line: Integer) of object;

  {
  THTMLDocEventSink:
    Event sink for HTML document events. Sinks HTMLDocumentEvents2 events.
  }
  THTMLWdwEventSink = class(TAbstractHTMLEventSink,
    IUnknown, IDispatch
  )
  strict private
    fOnError: THTMLWdwErrorEvent;
      {Reference to any OnError event handler}
  strict protected
    procedure DispatchEvent(var InvokeInfo: TInvokeInfo); override;
      {Dispatches HTMLWindowEvents2 events.
        @param InvokeInfo [in] Structure containing information about event to
          be dispatched. Fields of this structure are updated to notify result
          of event invocation.
      }
  public
    constructor Create;
      {Class constructor. Sets up object to sink HTMLWindowEvents2 events.
      }
    property OnError: THTMLWdwErrorEvent read fOnError write fOnError;
      {Event triggered when browser's onerror event is invoked}
  end;

implementation


uses
  // Delphi
  SysUtils, Variants, Windows, ComObj,
  // Project
  UExceptions;


{ THTMLEventInfo }

constructor THTMLEventInfo.Create(const DispatchID: Integer;
  const Args: IHTMLEventObj; const CanCancel: Boolean);
  {Class constructor. Sets up object.
    @param DispatchId [in] Dispatch ID of the event.
    @param Args [in] Information about the event provided by browser.
    @param CanCancel [in] Flag indicating whether event can be cancelled or not.
  }
begin
  inherited Create;
  // Initialise properties
  fDispatchId := DispatchID;
  fArgs := Args;
  fCanCancel := CanCancel;
  fCancelled := False;
end;

function THTMLEventInfo.GetName: string;
  {Read accessor for Name property.
    @return Name of event.
  }
begin
  Result := fArgs.type_;
end;

procedure THTMLEventInfo.SetCancelled(const Value: Boolean);
  {Write accessor for Cancelled property. Sets property only if CanCancel
  property is True.
    @param Value [in] New property value.
  }
begin
  if CanCancel then
    fCancelled := Value;
end;

{ TAbstractHTMLEventSink }

procedure TAbstractHTMLEventSink.Connect(const Source: IDispatch);
  {Connects the event sink to the object that is the source of the events.
    @param Source [in] Reference to source object.
  }
begin
  Disconnect;
  fSource := Source;
  InterfaceConnect(fSource, fSinkIID, Self, fConnectionCookie);
end;

constructor TAbstractHTMLEventSink.Create(const SinkIID: TGUID);
  {Class constructor. Sets up the object.
    @param SinkIID [in] Interface to events to be sunk by this object.
  }
begin
  inherited Create;
  fSinkIID := SinkIID;
end;

destructor TAbstractHTMLEventSink.Destroy;
  {Class destructor. Disconnects the sink from the event source and tears down
  the object.
  }
begin
  Disconnect;
  inherited;
end;

procedure TAbstractHTMLEventSink.Disconnect;
  {Disconnects the event sink from the event source object.
  }
begin
  if Assigned(fSource) then
  begin
    InterfaceDisconnect(fSource, fSinkIID, fConnectionCookie);
    fSource := nil;
  end;
end;

procedure TAbstractHTMLEventSink.DispatchStdEvent(var InvokeInfo: TInvokeInfo;
  const CanCancel: Boolean);
  {Triggers given "standard" event, passing information about the event. Sets
  function result if event is a function. Notifies an error if method call is
  not a valid standard event.
    @param InvokeInfo [in/out] Information about event method invocation.
      Structure is updated with information about return value and any error
      condition.
    @param CanCancel [in] Flag indicating whether event can be cancelled.
  }
var
  EventArgs: IHTMLEventObj;   // object encapsulating event arguments
  EventInfo: THTMLEventInfo;  // object providing information about event
begin
  // Get event arguments from method's parameter list: stores any error code in
  // InvokeInfo
  GetEventArgParam(InvokeInfo, EventArgs);
  if not Succeeded(InvokeInfo.SCode) then
    Exit;
  // Create object to store info about the event
  EventInfo := THTMLEventInfo.Create(
    InvokeInfo.DispatchID, EventArgs, CanCancel
  );
  try
    // Trigger the event
    if Assigned(fOnEvent) then
      fOnEvent(Self, EventInfo);
    // Store whether event is cancelled as return value only if event can be
    // cancelled
    if EventInfo.CanCancel then
      InvokeInfo.FnResult := not EventInfo.Cancelled
  finally
    FreeAndNil(EventInfo);
  end;
end;

procedure TAbstractHTMLEventSink.DoInvoke(var InvokeInfo: TInvokeInfo);
  {Dispatches events via abstract DispatchEvent method after checking for valid
  context.
    @param InvokeInfo [in/out] Information about method / property invocation.
      Structure is updated with information about invocation.
    @except EBug exception raised if invoked with an property or named method.
  }
begin
  // We only accept method calls, not properties
  if InvokeInfo.Flags and DISPATCH_METHOD = 0 then
    raise EBug.Create(ClassName + ' does not support properties');
  // We don't handle named parameters
  if InvokeInfo.Params.cNamedArgs > 0 then
    raise EBug.Create(ClassName + ' does not support named methods');
  DispatchEvent(InvokeInfo);
end;

procedure TAbstractHTMLEventSink.GetEventArgParam(var InvokeInfo: TInvokeInfo;
  out EventArgs: IHTMLEventObj);
  {Gets the single IHTMLEventObj parameter of a "standard" HTML event. Notifies
  caller of error if there is more than one parameter or if parameter is wrong
  type.
    @param InvokeInfo [in/out] Information about method invocation. Structure is
      updated with information about any error.
    @param EventArgs [out] Set to object giving information information about
      event arguments or nil if parameter is not valid.
  }
begin
  EventArgs := nil;
  if InvokeInfo.Params.cArgs = 1 then
    // Get IHTMLEventObj interface from only param. GetValidParam sets error
    // info in InvokeInfo if param not valid
    GetValidParam(InvokeInfo, 0, varDispatch, IHTMLEventObj, EventArgs)
  else
    // Incorrect number of params: set error code accordingly
    InvokeInfo.SCode := DISP_E_BADPARAMCOUNT;
end;

function TAbstractHTMLEventSink.QueryInterface(const IID: TGUID;
  out Obj): HResult;
  {Checks if the specified interface is supported by this object. If so a
  reference to the interface is passed out. Supported interfaces are IUnknown,
  IDispatch and the interface implemented by the event sink.
    @param IID [in] specifies interface being queried.
    @param Obj [out] Reference to interface implementation or nil if not
      supported.
    @return S_OK if interface supported or E_NOINTERFACE if not supported.
  }
begin
  Result := S_OK;
  if GetInterface(IID, Obj) then
    Exit;
  if IsEqualGUID(fSinkIID, IID) and GetInterface(IDispatch, Obj) then
    Exit;
  Result := E_NOINTERFACE;
  Pointer(Obj) := nil;
end;

{ THTMLDocEventSink }

constructor THTMLDocEventSink.Create;
  {Class constructor. Sets up object to sink HTMLDocumentEvents2 events.
  }
begin
  inherited Create(HTMLDocumentEvents2);
end;

procedure THTMLDocEventSink.DispatchEvent(var InvokeInfo: TInvokeInfo);
  {Dispatches HTMLDocumentEvents2 events.
    @param InvokeInfo [in] Structure containing information about the event to
      be dispatched. Fields of this structure are updated to notify result of
      event invocation.
  }
begin
  inherited;
  // Dispatch events
  case InvokeInfo.DispatchID of
    cDocEventOnHelp:              DispatchStdEvent(InvokeInfo, True);
    cDocEventOnClick:             DispatchStdEvent(InvokeInfo, True);
    cDocEventOnDblClick:          DispatchStdEvent(InvokeInfo, True);
    cDocEventOnKeyDown:           DispatchStdEvent(InvokeInfo, False);
    cDocEventOnKeyUp:             DispatchStdEvent(InvokeInfo, False);
    cDocEventOnKeyPress:          DispatchStdEvent(InvokeInfo, True);
    cDocEventOnMouseDown:         DispatchStdEvent(InvokeInfo, False);
    cDocEventOnMouseMove:         DispatchStdEvent(InvokeInfo, False);
    cDocEventOnMouseUp:           DispatchStdEvent(InvokeInfo, False);
    cDocEventOnMouseOut:          DispatchStdEvent(InvokeInfo, False);
    cDocEventOnMouseOver:         DispatchStdEvent(InvokeInfo, False);
    cDocEventOnReadyStateChange:  DispatchStdEvent(InvokeInfo, False);
    cDocEventOnBeforeUpdate:      DispatchStdEvent(InvokeInfo, True);
    cDocEventOnAfterUpdate:       DispatchStdEvent(InvokeInfo, False);
    cDocEventOnRowExit:           DispatchStdEvent(InvokeInfo, True);
    cDocEventOnRowEnter:          DispatchStdEvent(InvokeInfo, False);
    cDocEventOnDragStart:         DispatchStdEvent(InvokeInfo, True);
    cDocEventOnSelectStart:       DispatchStdEvent(InvokeInfo, True);
    cDocEventOnErrorUpdate:       DispatchStdEvent(InvokeInfo, True);
    cDocEventOnContextMenu:       DispatchStdEvent(InvokeInfo, True);
    cDocEventOnStop:              DispatchStdEvent(InvokeInfo, True);
    cDocEventOnRowsDelete:        DispatchStdEvent(InvokeInfo, False);
    cDocEventOnRowsInserted:      DispatchStdEvent(InvokeInfo, False);
    cDocEventOnCellChange:        DispatchStdEvent(InvokeInfo, False);
    cDocEventOnPropertyChange:    DispatchStdEvent(InvokeInfo, False);
    cDocEventOnDatasetChanged:    DispatchStdEvent(InvokeInfo, False);
    cDocEventOnDataAvailable:     DispatchStdEvent(InvokeInfo, False);
    cDocEventOnDatasetComplete:   DispatchStdEvent(InvokeInfo, False);
    cDocEventOnBeforeEditFocus:   DispatchStdEvent(InvokeInfo, False);
    else
      InvokeInfo.SCode := DISP_E_MEMBERNOTFOUND;
  end;
end;

{ THTMLWdwEventSink }

constructor THTMLWdwEventSink.Create;
  {Class constructor. Sets up object to sink HTMLWindowEvents2 events.
  }
begin
  inherited Create(HTMLWindowEvents2);
end;

procedure THTMLWdwEventSink.DispatchEvent(var InvokeInfo: TInvokeInfo);
  {Dispatches HTMLWindowEvents2 events.
    @param InvokeInfo [in] Structure containing information about event to be
      dispatched. Fields of this structure are updated to notify result of event
      invocation.
  }

  // ---------------------------------------------------------------------------
  procedure DispatchErrorEvent;
    {Dispatches the OnError event.
    }
  var
    Description: WideString;  // description of error
    URL: WideString;          // URL of document that encountered error
    Line: Integer;            // line number of error in document
    V: OleVariant;            // value of each validated parameter
  begin
    // NOTE: parameters are stored in reverse order in InvokeInfo.Params.rgvarg.
    // check required number of params
    if InvokeInfo.Params.cArgs <> 3 then
    begin
      InvokeInfo.SCode := DISP_E_BADPARAMCOUNT;
      Exit;
    end;
    // validate 1st (description) param: must be string
    if not GetValidParam(InvokeInfo, 2, varOleStr, V) then
      Exit;
    Description := V;
    // validate 2nd (URL) param: must be string
    if not GetValidParam(InvokeInfo, 1, varOleStr, V) then
      Exit;
    URL := V;
    // validate 3rd (Line) param: must be integer
    if not GetValidParam(InvokeInfo, 0, varInteger, V) then
      Exit;
    Line := V;
    // Everything's OK: trigger any assigned event
    if Assigned(fOnError) then
      fOnError(Self, Description, URL, Line);
  end;
  // ---------------------------------------------------------------------------

begin
  inherited;
  // Dispatch events
  case InvokeInfo.DispatchID of
    cWdwEventOnLoad:          DispatchStdEvent(InvokeInfo, False);
    cWdwEventOnUnload:        DispatchStdEvent(InvokeInfo, False);
    cWdwEventOnHelp:          DispatchStdEvent(InvokeInfo, True);
    cWdwEventOnFocus:         DispatchStdEvent(InvokeInfo, False);
    cWdwEventOnBlur:          DispatchStdEvent(InvokeInfo, False);
    cWdwEventOnError:         DispatchErrorEvent;
    cWdwEventOnResize:        DispatchStdEvent(InvokeInfo, False);
    cWdwEventOnScroll:        DispatchStdEvent(InvokeInfo, False);
    cWdwEventOnBeforeUnload:  DispatchStdEvent(InvokeInfo, False);
    cWdwEventOnBeforePrint:   DispatchStdEvent(InvokeInfo, False);
    cWdwEventOnAfterPrint:    DispatchStdEvent(InvokeInfo, False);
  else
    InvokeInfo.SCode := DISP_E_MEMBERNOTFOUND;
  end;
end;

end.

