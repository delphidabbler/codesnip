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
 * Portions created by the Initial Developer are Copyright (C) 2007-2011 Peter
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
    ///  <param name="DispatchId">Integer [in] Dispatch ID of the event.</param>
    ///  <param name="Args">IHTMLEventObj [in] Information about the event
    ///  provided by browser control.</param>
    ///  <param name="CanCancel">Boolean [in] Flag indicating whether or not
    ///  event can be cancelled.</param>
    constructor Create(const DispatchID: Integer;
      const Args: IHTMLEventObj; const CanCancel: Boolean);
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
  TAbstractHTMLEventSink = class(TSimpleDispatch,
    IUnknown, IDispatch
  )
  strict private
    var
      ///  <summary>Event interface supported by this sink object.</summary>
      fSinkIID: TGUID;
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
    ///  <param name="IID">TGUID [in] Specifies interface being queried.</param>
    ///  <param name="Obj">Untyped [out] Reference to requested interface
    ///  implementation or nil if interface not supported.</param>
    ///  <returns>HResult. S_OK if interface supported or E_NOINTERFACE if not
    ///  supported.</returns>
    ///  <remarks>Re-implementation of method of IUnknown.</remarks>
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    ///  <summary>Creates sink object for a given events interface.</summary>
    ///  <param name="SinkIID">TGUID [in] Interface to events to be sunk by this
    ///  object.</param>
    constructor Create(const SinkIID: TGUID);
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
  ///  <summary>
  ///  Event sink for HTMLDocumentEvents2 events.
  ///  </summary>
  ///  <remarks>
  ///  Events are all "standard" HTML events notified via the inherited OnEvent
  ///  event.
  ///  </remarks>
  THTMLDocEventSink = class(TAbstractHTMLEventSink,
    IUnknown, IDispatch
  )
  strict protected
    ///  <summary>Dispatches HTMLDocumentEvents2 events.</summary>
    ///  <param name="InvokeInfo">TInvokeInfo [in/out] Structure containing
    ///  information about the event to be dispatched. Fields of this structure
    ///  are updated to notify result of event invocation.</param>
    procedure DispatchEvent(var InvokeInfo: TInvokeInfo); override;
  public
    ///  <summary>Creates HTMLDocumentEvents2 event sink object.</summary>
    constructor Create;
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
  ///  <summary>Type of the OnError event of THTMLWdwEventSink.</summary>
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
  THTMLWdwErrorEvent = procedure(Sender: TObject; const Desc, URL: string;
    const Line: Integer; var Handled: Boolean) of object;

type
  ///  <summary>
  ///  Event sink for HTMLWindowEvents2 events.
  ///  </summary>
  ///  <remarks>
  ///  Events are either "standard" HTML events notified via the inherited
  ///  OnEvent event or error events notified by the OnError event.
  ///  </remarks>
  THTMLWdwEventSink = class(TAbstractHTMLEventSink,
    IUnknown, IDispatch
  )
  strict private
    var
      ///  <summary>Reference to any OnError event handler.</summary>
      fOnError: THTMLWdwErrorEvent;
  strict protected
    ///  <summary>Dispatches HTMLWindowEvents2 events.</summary>
    ///  <param name="InvokeInfo">TInvokeInfo [in/out] Structure containing
    ///  information about the event to be dispatched. Fields of this structure
    ///  are updated to notify result of event invocation.</param>
    procedure DispatchEvent(var InvokeInfo: TInvokeInfo); override;
  public
    ///  <summary>Creates HTMLWindowEvents2 event sink object.</summary>
    constructor Create;
    ///  <summary>Event triggered when HTMLWindowEvents2.onerror event is
    ///  invoked.</summary>
    property OnError: THTMLWdwErrorEvent read fOnError write fOnError;
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
begin
  inherited Create;
  fDispatchId := DispatchID;
  fArgs := Args;
  fCanCancel := CanCancel;
  fCancelled := False;
end;

function THTMLEventInfo.GetName: string;
begin
  Result := fArgs.type_;
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
  InterfaceConnect(fSource, fSinkIID, Self, fConnectionCookie);
end;

constructor TAbstractHTMLEventSink.Create(const SinkIID: TGUID);
begin
  inherited Create;
  fSinkIID := SinkIID;
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
    InterfaceDisconnect(fSource, fSinkIID, fConnectionCookie);
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

function TAbstractHTMLEventSink.QueryInterface(const IID: TGUID;
  out Obj): HResult;
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
begin
  inherited Create(HTMLDocumentEvents2);
end;

procedure THTMLDocEventSink.DispatchEvent(var InvokeInfo: TInvokeInfo);
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
begin
  inherited Create(HTMLWindowEvents2);
end;

procedure THTMLWdwEventSink.DispatchEvent(var InvokeInfo: TInvokeInfo);

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

