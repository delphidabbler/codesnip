{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides sinks for events triggered by HTML documents loaded in a TWebBrowser
 * control. Sinks for the HTMLDocumentEvents2 and HTMLWindowEvents2
 * dispinterfaces are provided.
}


unit Browser.UHTMLEvents;


interface


uses
  // Delphi
  MSHTML, ActiveX,
  // Project
  USimpleDispatch;


type
  ///  <summary>
  ///  Class that provides information about a HTML event.
  ///  </summary>
  THTMLEventInfo = class(TObject)
  strict private
    var
      ///  <summary>Value of CanCancel property.</summary>
      fCanCancel: Boolean;
      ///  <summary>Value of Cancelled property.</summary>
      fCancelled: Boolean;
      ///  <summary>Value of Args property.</summary>
      fArgs: IHTMLEventObj;
      ///  <summary>Value of EventIntf property.</summary>
      fEventIntf: TGUID;
      ///  <summary>Value of DispatchId property.</summary>
      fDispatchId: Integer;
      ///  <summary>Setter for Cancelled property.</summary>
      ///  <remarks>Sets property only if CanCancel property is True.</remarks>
    procedure SetCancelled(const Value: Boolean);
    ///  <summary>Getter for Name property.</summary>
    function GetName: string;
  public
    ///  <summary>Creates event object instance with specified property values.
    ///  </summary>
    ///  <param name="EventIntf">TGUID [in] GUID of event source object.</param>
    ///  <param name="DispatchId">Integer [in] Dispatch ID of the event.</param>
    ///  <param name="Args">IHTMLEventObj [in] Information about the event
    ///  provided by browser control.</param>
    ///  <param name="CanCancel">Boolean [in] Flag indicating whether or not
    ///  event can be cancelled.</param>
    constructor Create(const EventIntf: TGUID; const DispatchID: Integer;
      const Args: IHTMLEventObj; const CanCancel: Boolean);
    ///  <summary>Checks if the event has a given dispid and belongs to a given
    ///  events interface.</summary>
    ///  <param name="AEventIntf">TGUID [in] GUID of events interface to which
    ///  event must belong.</param>
    ///  <param name="ADispatchID">Integer [in] Dispid of required event.
    ///  </param>
    ///  <returns>Boolean. True if event's Sink and DispatchId properties match
    ///  the parameters, False if not.</returns>
    function IsEvent(const AEventIntf: TGUID; const ADispatchId: Integer):
      Boolean;
    ///  <summary>Checks if the element triggering the event has a specified id.
    ///  </summary>
    ///  <param name="ID">string [in] Required element ID.</param>
    ///  <returns>Boolean. True if triggering element has ID, False otherwise.
    ///  </returns>
    function ElemHasId(const ID: string): Boolean;
    ///  <summary>GUID of event interface to which event belongs.</summary>
    property EventIntf: TGUID read fEventIntf;
    ///  <summary>Dispatch ID of event.</summary>
    property DispatchId: Integer read fDispatchId;
    ///  <summary>Name of event.</summary>
    ///  <remarks>This is the same value as Args.type_.</remarks>
    property Name: string read GetName;
    ///  <summary>Flag indicating whether or not event can be cancelled.
    ///  </summary>
    ///  <remarks>When True the Cancelled property cannot be set.</remarks>
    property CanCancel: Boolean read fCanCancel;
    ///  <summary>Flag indicating whether the event should be cancelled. Set to
    ///  true to cancel the event.</summary>
    ///  <remarks>If CanCancel is False the value of Cancelled cannot be changed
    ///  and is always False.</remarks>
    property Cancelled: Boolean read fCancelled write SetCancelled
      default False;
    ///  <summary>Object exposing information about the event provided by browser
    ///  object.</summary>
    property Args: IHTMLEventObj read fArgs;
  end;

type
  ///  <summary>Type of the OnEvent event of TAbstractHTMLEventSink.</summary>
  ///  <param name="Sender">TObject [in] Reference to object that triggered the
  ///  event.</param>
  ///  <param name="EventInfo">THTMLEventInfo [in] Object providing information
  ///  about the event.</param>
  THTMLEvent = procedure(Sender: TObject; const EventInfo: THTMLEventInfo)
    of object;

type
  ///  <summary>
  ///  Abstract base class for non reference counted HTML event sinks. Provides
  ///  core functionality and some helper methods for handling "standard" HTML
  ///  events.
  ///  </summary>
  ///  <remarks>
  ///  "Standard" HTML events are considered to be events that provide a single
  ///  IHTMLEventObj parameter that provides information about the event. The
  ///  events may or may not be able to be cancelled.
  ///  </remarks>
  TAbstractHTMLEventSink = class abstract(TSimpleDispatch,
    IUnknown, IDispatch
  )
  strict private
    var
      ///  <summary>Reference to event source object.</summary>
      fSource: IDispatch;
      ///  <summary>Identifies connection between source and sink.</summary>
      fConnectionCookie: Integer;
      ///  <summary>Reference to any OnEvent handler.</summary>
      fOnEvent: THTMLEvent;
  strict protected
    ///  <summary>Dispatches events via abstract DispatchEvent method after
    ///  checking for valid context.</summary>
    ///  <param name="InvokeInfo">TInvokeInfo [in/out] Information about event
    ///  invocation. Structure is updated with information following invocation.
    ///  </param>
    ///  <remarks>EBug exception raised if invoked for property or named method.
    ///  </remarks>
    procedure DoInvoke(var InvokeInfo: TInvokeInfo); override;
    ///  <summary>Abstract method called when a event is to be dispatched.
    ///  </summary>
    ///  <param name="InvokeInfo">TInvokeInfo [in/out] Information about event
    ///  invocation. Structure must be updated with information about result of
    ///  invocation.</param>
    ///  <remarks>Descendant classes must override to dispatch the event in a
    ///  meaningful way.</remarks>
    procedure DispatchEvent(var InvokeInfo: TInvokeInfo); virtual; abstract;
    ///  <summary>Gets the single IHTMLEventObj parameter of a "standard" HTML
    ///  event. Notifies caller of error if there is more than one parameter or
    ///  if parameter is wrong type.</summary>
    ///  <param name="InvokeInfo">TInvokeInfo [in/out] Information about method
    ///  invocation. Updated with information about any error.</param>
    ///  <param name="EventArgs">IHTMLEventObj [out] Set to object giving
    ///  information information about event arguments or nil if parameter is
    ///  not valid.</param>
    procedure GetEventArgParam(var InvokeInfo: TInvokeInfo;
      out EventArgs: IHTMLEventObj);
    ///  <summary>Triggers a given "standard" event, passing information about
    ///  the event. Sets function result if event is a function. Notifies an
    ///  error if method call is not a valid standard event.</summary>
    ///  <param name="InvokeInfo">TInvokeInfo [in/out] Information about event
    ///  invocation. Updated with information about return value if event is a
    ///  function or any error condition if not a "standard" HTML event.</param>
    ///  <param name="CanCancel">Boolean [in] Flag indicating whether event can
    ///  be cancelled.</param>
    procedure DispatchStdEvent(var InvokeInfo: TInvokeInfo;
      const CanCancel: Boolean);
    ///  <summary>Checks if the specified interface is supported by this object.
    ///  If so a reference to the interface is passed out. Supported interfaces
    ///  are IUnknown, IDispatch and the interface implemented by the event
    ///  sink.</summary>
    ///  <param name="AIID">TGUID [in] Specifies interface being queried.
    ///  </param>
    ///  <param name="Obj">Untyped [out] Reference to requested interface
    ///  implementation or nil if interface not supported.</param>
    ///  <returns>HResult. S_OK if interface supported or E_NOINTERFACE if not
    ///  supported.</returns>
    ///  <remarks>Re-implementation of method of IUnknown.</remarks>
    function QueryInterface(const AIID: TGUID; out Obj): HResult; stdcall;
  public
    ///  <summary>Returns GUID of supported events object.</summary>
    class function EventIntf: TGUID; virtual; abstract;
    ///  <summary>Disconnects sink from event source and tears down object.
    ///  </summary>
    destructor Destroy; override;
    ///  <summary>Connects event sink to the object that is the source of the
    ///  events.</summary>
    ///  <param name="Source">IDispatch [in] Reference to event source object.
    ///  </param>
    procedure Connect(const Source: IDispatch);
    ///  <summary>Disconnects event sink from event source object.</summary>
    procedure Disconnect;
    ///  <summary>Event triggered when object sinks a "standard" event.
    ///  </summary>
    property OnEvent: THTMLEvent read fOnEvent write fOnEvent;
  end;

type
  ///  <summary>
  ///  Event sink for HTMLDocumentEvents2 events.
  ///  </summary>
  ///  <remarks>
  ///  Events are all "standard" HTML events notified via the inherited OnEvent
  ///  event.
  ///  </remarks>
  THTMLDocumentEvents2Sink = class sealed(TAbstractHTMLEventSink,
    IUnknown, IDispatch
  )
  strict protected
    ///  <summary>Dispatches HTMLDocumentEvents2 events.</summary>
    ///  <param name="InvokeInfo">TInvokeInfo [in/out] Structure containing
    ///  information about the event to be dispatched. Fields of this structure
    ///  are updated to notify result of event invocation.</param>
    procedure DispatchEvent(var InvokeInfo: TInvokeInfo); override;
  public
    const
      // Event dispids from HTMLDocumentEvents2 dispinterface
      DISPID_OnHelp               = -2147418102;
      DISPID_OnClick              = -600;
      DISPID_OnDblClick           = -601;
      DISPID_OnKeyDown            = -602;
      DISPID_OnKeyUp              = -604;
      DISPID_OnKeyPress           = -603;
      DISPID_OnMouseDown          = -605;
      DISPID_OnMouseMove          = -606;
      DISPID_OnMouseUp            = -607;
      DISPID_OnMouseOut           = -2147418103;
      DISPID_OnMouseOver          = -2147418104;
      DISPID_OnReadyStateChange   = -609;
      DISPID_OnBeforeUpdate       = -2147418108;
      DISPID_OnAfterUpdate        = -2147418107;
      DISPID_OnRowExit            = -2147418106;
      DISPID_OnRowEnter           = -2147418105;
      DISPID_OnDragStart          = -2147418101;
      DISPID_OnSelectStart        = -2147418100;
      DISPID_OnErrorUpdate        = -2147418099;
      DISPID_OnContextMenu        = 1023;
      DISPID_OnStop               = 1026;
      DISPID_OnRowsDelete         = -2147418080;
      DISPID_OnRowsInserted       = -2147418079;
      DISPID_OnCellChange         = -2147418078;
      DISPID_OnPropertyChange     = -2147418093;
      DISPID_OnDatasetChanged     = -2147418098;
      DISPID_OnDataAvailable      = -2147418097;
      DISPID_OnDatasetComplete    = -2147418096;
      DISPID_OnBeforeEditFocus    = 1027;
  public
    ///  <summary>Returns GUID of supported HTMLDocumentEvents2 events object.
    ///  </summary>
    class function EventIntf: TGUID; override;
  end;

type
  ///  <summary>Type of the OnError event of THTMLWindowEvents2Sink.</summary>
  ///  <param name="Sender">TObject [in] Reference to object that triggered
  ///  event.</param>
  ///  <param name="Desc">string [in] Description of error.</param>
  ///  <param name="URL">string [in] URL of document triggering error.</param>
  ///  <param name="Line">Integer [in] Line number where error occurred.</param>
  ///  <param name="Handled">Boolean [in/out] Indicates if error has been
  ///  handled. Always passed in as False. Set to True if error handled and
  ///  further processing is to be inhibited.</param>
  ///  <remarks>In the case of script errors, setting Handled to True inhibits
  ///  the browser control's script error dialog box.</remarks>
  THTMLWindowErrorEvent = procedure(Sender: TObject; const Desc, URL: string;
    const Line: Integer; var Handled: Boolean) of object;

type
  ///  <summary>
  ///  Event sink for HTMLWindowEvents2 events.
  ///  </summary>
  ///  <remarks>
  ///  Events are either "standard" HTML events notified via the inherited
  ///  OnEvent event or error events notified by the OnError event.
  ///  </remarks>
  THTMLWindowEvents2Sink = class sealed(TAbstractHTMLEventSink,
    IUnknown, IDispatch
  )
  strict private
    var
      ///  <summary>Reference to any OnError event handler.</summary>
      fOnError: THTMLWindowErrorEvent;
  strict protected
    ///  <summary>Dispatches HTMLWindowEvents2 events.</summary>
    ///  <param name="InvokeInfo">TInvokeInfo [in/out] Structure containing
    ///  information about the event to be dispatched. Fields of this structure
    ///  are updated to notify result of event invocation.</param>
    procedure DispatchEvent(var InvokeInfo: TInvokeInfo); override;
  public
    const
      // Event dispids from HTMLDocumentEvents2 dispinterface
      DISPID_OnLoad         = 1003;
      DISPID_OnUnload       = 1008;
      DISPID_OnHelp         = -2147418102;
      DISPID_OnFocus        = -2147418111;
      DISPID_OnBlur         = -2147418112;
      DISPID_OnError        = 1002;
      DISPID_OnResize       = 1016;
      DISPID_OnScroll       = 1014;
      DISPID_OnBeforeUnload = 1017;
      DISPID_OnBeforePrint  = 1024;
      DISPID_OnAfterPrint   = 1025;
  public
    ///  <summary>Returns GUID of supported HTMLWindowEvents2 events object.
    ///  </summary>
    class function EventIntf: TGUID; override;
    ///  <summary>Event triggered when HTMLWindowEvents2.onerror event is
    ///  invoked.</summary>
    property OnError: THTMLWindowErrorEvent read fOnError write fOnError;
  end;


implementation


uses
  // Delphi
  SysUtils, Variants, Windows, ComObj,
  // Project
  UExceptions, UStrUtils;


{ THTMLEventInfo }

constructor THTMLEventInfo.Create(const EventIntf: TGUID;
  const DispatchID: Integer; const Args: IHTMLEventObj;
  const CanCancel: Boolean);
begin
  inherited Create;
  fEventIntf := EventIntf;
  fDispatchId := DispatchID;
  fArgs := Args;
  fCanCancel := CanCancel;
  fCancelled := False;
end;

function THTMLEventInfo.ElemHasId(const ID: string): Boolean;
begin
  Result := StrSameText(fArgs.srcElement.id, ID);
end;

function THTMLEventInfo.GetName: string;
begin
  Result := fArgs.type_;
end;

function THTMLEventInfo.IsEvent(const AEventIntf: TGUID;
  const ADispatchId: Integer): Boolean;
begin
  Result := IsEqualGUID(AEventIntf, fEventIntf) and (ADispatchId = fDispatchId);
end;

procedure THTMLEventInfo.SetCancelled(const Value: Boolean);
begin
  if CanCancel then
    fCancelled := Value;
end;

{ TAbstractHTMLEventSink }

procedure TAbstractHTMLEventSink.Connect(const Source: IDispatch);
begin
  Disconnect;
  fSource := Source;
  InterfaceConnect(fSource, EventIntf, Self, fConnectionCookie);
end;

destructor TAbstractHTMLEventSink.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TAbstractHTMLEventSink.Disconnect;
begin
  if Assigned(fSource) then
  begin
    InterfaceDisconnect(fSource, EventIntf, fConnectionCookie);
    fSource := nil;
  end;
end;

procedure TAbstractHTMLEventSink.DispatchStdEvent(var InvokeInfo: TInvokeInfo;
  const CanCancel: Boolean);
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
    EventIntf, InvokeInfo.DispatchID, EventArgs, CanCancel
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
    EventInfo.Free;
  end;
end;

procedure TAbstractHTMLEventSink.DoInvoke(var InvokeInfo: TInvokeInfo);
begin
  // We only accept method calls, not properties
  if InvokeInfo.Flags and DISPATCH_METHOD = 0 then
    raise EBug.Create(ClassName + ' does not support properties');
  // We don't handle named parameters
  if InvokeInfo.Params.cNamedArgs > 0 then
    raise EBug.Create(ClassName + ' does not support named parameters');
  DispatchEvent(InvokeInfo);
end;

procedure TAbstractHTMLEventSink.GetEventArgParam(var InvokeInfo: TInvokeInfo;
  out EventArgs: IHTMLEventObj);
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

function TAbstractHTMLEventSink.QueryInterface(const AIID: TGUID;
  out Obj): HResult;
begin
  Result := S_OK;
  if GetInterface(EventIntf, Obj) then
    Exit;
  if IsEqualGUID(EventIntf, AIID) and GetInterface(IDispatch, Obj) then
    Exit;
  Result := E_NOINTERFACE;
  Pointer(Obj) := nil;
end;

{ THTMLDocumentEvents2Sink }

procedure THTMLDocumentEvents2Sink.DispatchEvent(var InvokeInfo: TInvokeInfo);
begin
  inherited;
  // Dispatch events
  case InvokeInfo.DispatchID of
    DISPID_OnHelp:              DispatchStdEvent(InvokeInfo, True);
    DISPID_OnClick:             DispatchStdEvent(InvokeInfo, True);
    DISPID_OnDblClick:          DispatchStdEvent(InvokeInfo, True);
    DISPID_OnKeyDown:           DispatchStdEvent(InvokeInfo, False);
    DISPID_OnKeyUp:             DispatchStdEvent(InvokeInfo, False);
    DISPID_OnKeyPress:          DispatchStdEvent(InvokeInfo, True);
    DISPID_OnMouseDown:         DispatchStdEvent(InvokeInfo, False);
    DISPID_OnMouseMove:         DispatchStdEvent(InvokeInfo, False);
    DISPID_OnMouseUp:           DispatchStdEvent(InvokeInfo, False);
    DISPID_OnMouseOut:          DispatchStdEvent(InvokeInfo, False);
    DISPID_OnMouseOver:         DispatchStdEvent(InvokeInfo, False);
    DISPID_OnReadyStateChange:  DispatchStdEvent(InvokeInfo, False);
    DISPID_OnBeforeUpdate:      DispatchStdEvent(InvokeInfo, True);
    DISPID_OnAfterUpdate:       DispatchStdEvent(InvokeInfo, False);
    DISPID_OnRowExit:           DispatchStdEvent(InvokeInfo, True);
    DISPID_OnRowEnter:          DispatchStdEvent(InvokeInfo, False);
    DISPID_OnDragStart:         DispatchStdEvent(InvokeInfo, True);
    DISPID_OnSelectStart:       DispatchStdEvent(InvokeInfo, True);
    DISPID_OnErrorUpdate:       DispatchStdEvent(InvokeInfo, True);
    DISPID_OnContextMenu:       DispatchStdEvent(InvokeInfo, True);
    DISPID_OnStop:              DispatchStdEvent(InvokeInfo, True);
    DISPID_OnRowsDelete:        DispatchStdEvent(InvokeInfo, False);
    DISPID_OnRowsInserted:      DispatchStdEvent(InvokeInfo, False);
    DISPID_OnCellChange:        DispatchStdEvent(InvokeInfo, False);
    DISPID_OnPropertyChange:    DispatchStdEvent(InvokeInfo, False);
    DISPID_OnDatasetChanged:    DispatchStdEvent(InvokeInfo, False);
    DISPID_OnDataAvailable:     DispatchStdEvent(InvokeInfo, False);
    DISPID_OnDatasetComplete:   DispatchStdEvent(InvokeInfo, False);
    DISPID_OnBeforeEditFocus:   DispatchStdEvent(InvokeInfo, False);
    else
      InvokeInfo.SCode := DISP_E_MEMBERNOTFOUND;
  end;
end;

class function THTMLDocumentEvents2Sink.EventIntf: TGUID;
begin
  Result := HTMLDocumentEvents2;
end;

{ THTMLWindowEvents2Sink }

procedure THTMLWindowEvents2Sink.DispatchEvent(var InvokeInfo: TInvokeInfo);

  // ---------------------------------------------------------------------------
  ///  Dispatches an OnError event.
  procedure DispatchErrorEvent;
  var
    Description: WideString;  // description of error
    URL: WideString;          // URL of document that encountered error
    Line: Integer;            // line number of error in document
    Handled: Boolean;         // flag indicating if error handled
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
    // Everything's OK: trigger any assigned event and return Handled value
    Handled := False;
    if Assigned(fOnError) then
      fOnError(Self, Description, URL, Line, Handled);
    InvokeInfo.FnResult := Handled;
  end;
  // ---------------------------------------------------------------------------

begin
  inherited;
  // Dispatch events
  case InvokeInfo.DispatchID of
    DISPID_OnLoad:          DispatchStdEvent(InvokeInfo, False);
    DISPID_OnUnload:        DispatchStdEvent(InvokeInfo, False);
    DISPID_OnHelp:          DispatchStdEvent(InvokeInfo, True);
    DISPID_OnFocus:         DispatchStdEvent(InvokeInfo, False);
    DISPID_OnBlur:          DispatchStdEvent(InvokeInfo, False);
    DISPID_OnError:         DispatchErrorEvent;
    DISPID_OnResize:        DispatchStdEvent(InvokeInfo, False);
    DISPID_OnScroll:        DispatchStdEvent(InvokeInfo, False);
    DISPID_OnBeforeUnload:  DispatchStdEvent(InvokeInfo, False);
    DISPID_OnBeforePrint:   DispatchStdEvent(InvokeInfo, False);
    DISPID_OnAfterPrint:    DispatchStdEvent(InvokeInfo, False);
  else
    InvokeInfo.SCode := DISP_E_MEMBERNOTFOUND;
  end;
end;

class function THTMLWindowEvents2Sink.EventIntf: TGUID;
begin
  Result := HTMLWindowEvents2;
end;

end.

