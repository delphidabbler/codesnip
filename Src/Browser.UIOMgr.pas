{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class that wraps the IE web browser control and provides ability to load and
 * save HTML from files, streams or strings. Also simplifies navigation to
 * documents stored locally or in resources and exposes some HTML events.
}


unit Browser.UIOMgr;


interface


uses
  // Delphi
  SHDocVw, Classes,
  // Project
  Browser.UHTMLEvents;


type
  ///  <summary>Type of event triggered just before browser control navigates to
  ///  a new document.</summary>
  ///  <param name="Sender">TObject [in] Object triggering event.</param>
  ///  <param name="URL">string [in] URL to be navigated to.</param>
  ///  <param name="Cancel">Boolean [in/out] False when called. Set to True to
  ///  prevent the browser navigating to the URL.</param>
  ///  <remarks>Handle this event to intervene in, cancel or get information
  ///  about the navigation.</remarks>
  TWBNavigateEvent = procedure(Sender: TObject; const URL: string;
    var Cancel: Boolean) of object;

type
  ///  <summary>Class that wraps the IE web browser control and provides ability
  ///  to load and save HTML from files, streams or strings. Also simplifies
  ///  navigation to documents stored locally or in resources and exposes some
  ///  HTML events.</summary>
  TWBIOMgr = class(TObject)
  strict private
    ///  <summary>Reference to managed webbrowser.</summary>
    fWB: TWebBrowser;
    ///  <summary>Handler for OnNavigate event.</summary>
    fOnNavigate: TWBNavigateEvent;
    ///  <summary>Handler for OnHTMLEvent event.</summary>
    fOnHTMLEvent: THTMLEvent;
    ///  <summary>Handler for OnHTMLWindowError event.</summary>
    fOnHTMLWindowError: THTMLWindowErrorEvent;
    ///  <summary>Event sink for HTMLDocumentEvents2 events.</summary>
    fDocEvents: THTMLDocumentEvents2Sink;
    ///  <summary>Event sink for HTMLWindowEvents2 events.</summary>
    fWdwEvents: THTMLWindowEvents2Sink;
    ///  <summary>Flag that indicates if a document has been fully loaded into
    ///  the browser control.</summary>
    ///  <remarks>This flag is set false (1) before any documen has been loaded
    ///  and (2) during the process of navigating to a new document. The flag
    ///  is set True only when a document is fully loaded.</remarks>
    fDocLoaded: Boolean;
    ///  <summary>Handles OnEvent events triggered by browser document and
    ///  window event sinks. Triggers OnHTMLEvent and passes parameters to it.
    ///  </summary>
    procedure HTMLEventHandler(Sender: TObject;
      const EventInfo: THTMLEventInfo);
    ///  <summary>Handles OnError events triggered by browser window event sink.
    ///  Triggers OnHTMLWindowError and passes parameters to it.</summary>
    procedure HTMLWindowErrorHandler(Sender: TObject; const Desc, URL: string;
      const Line: Integer; var Handled: Boolean);
    ///  <summary>Waits for a document to complete loading.</summary>
    ///  <remarks>EBug raised if there is no document or it is not a valid HTML
    ///  document.</remarks>
    procedure WaitForDocToLoad;
    ///  <summary>Handles web browser navigation events. Triggers OnNavigate
    ///  event and passes URL and Cancel parameters to it.</summary>
    ///  <remarks>All parameters except URL and Cancel are ignored.</remarks>
    procedure NavigateHandler(Sender: TObject; const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
    ///  <summary>Handles web browser's OnDocumentComplete method to check that
    ///  top level document has loaded.</summary>
    ///  <remarks>Only pDisp parameter is used to check if frame firing event is
    ///  top level.</remarks>
    procedure DocCompleteHandler(Sender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    ///  <summary>Updates the web browser's current document from HTML read from
    ///  given stream.</summary>
    ///  <remarks>EBug raised if updated document is not valid.</remarks>
    procedure InternalLoadDocumentFromStream(const Stream: TStream);
    ///  <summary>Creates an empty document in browser.</summary>
    ///  <remarks>
    ///  <para>This method guarantees that the browser contains a valid document
    ///  object. The browser displays a blank page.</para>
    ///  <para>EBug raised if document is not valid.</para>
    ///  </remarks>
    procedure EmptyDocument;
    ///  <summary>Navigates to a document at a specified URL.</summary>
    ///  <remarks>EBug raised if new document is not valid.</remarks>
    procedure NavigateToURL(const URL: string);
  public
    ///  <summary>Creates object to manage IO for a browser control.</summary>
    ///  <param name="WB">TWebBrowser [in] Managed webbrowser control. Must not
    ///  be nil.</param>
    constructor Create(const WB: TWebBrowser);
    ///  <summary>Tears down object and disconnects event sinks.</summary>
    destructor Destroy; override;
    ///  <summary>Loads HTML contained in given string into browser control.
    ///  </summary>
    ///  <remarks>EBug raised if document is not valid.</remarks>
    procedure LoadFromString(const HTML: string);
    ///  <summary>Loads the document stored as a resource in a module into
    ///  browser control.</summary>
    ///  <param name="Module">HMODULE [in] Handle of module containing resource.
    ///  </param>
    ///  <param name="ResName">PChar [in] Name of the resource.</param>
    ///  <param name="ResType">PChar [in] Type of resource (RT_HTML is assumed
    ///  if ResType is nil).</param>
    ///  <remarks>EBug raised if document is not valid.</remarks>
    procedure NavigateToResource(const Module: HMODULE; const ResName: PChar;
      const ResType: PChar = nil);
    ///  <summary>Checks if a valid HTML document is loaded in the browser
    ///  control.</summary>
    function HTMLDocumentExists: Boolean;
    ///  <summary>Replaces body of an existing HTML document with given HTML.
    ///  </summary>
    ///  <remarks>Given HTML is inserted between existing document's
    ///  &lt;body&gt; tags and must be valid for this purpose.</remarks>
    procedure ReplaceExistingBodyHTML(const HTML: string);
    ///  <summary>Event triggered when browser control is about to navigate to a
    ///  new document.</summary>
    ///  <remarks>Handle this event to intervene in navigation process.
    ///  </remarks>
    property OnNavigate: TWBNavigateEvent
      read fOnNavigate write fOnNavigate;
    ///  <summary>Event triggered when events are detected in browser's window
    ///  and document.</summary>
    property OnHTMLEvent: THTMLEvent
      read fOnHTMLEvent write fOnHTMLEvent;
    ///  <summary>Event triggered when browser window notifies an onerror event.
    ///  </summary>
    property OnHTMLWindowError: THTMLWindowErrorEvent
      read fOnHTMLWindowError write fOnHTMLWindowError;
  end;


implementation


uses
  // Delphi
  SysUtils, ActiveX,
  // Project
  Browser.UControlHelper, UHTMLDOMHelper, UResourceUtils, UUtils;


{ TWBIOMgr }

constructor TWBIOMgr.Create(const WB: TWebBrowser);
begin
  Assert(Assigned(WB), ClassName + '.Create: WB is nil');
  inherited Create;
  fWB := WB;
  fWB.OnBeforeNavigate2 := NavigateHandler;
  fWB.OnDocumentComplete := DocCompleteHandler;
  // Create event sinks and set event handlers
  fDocEvents := THTMLDocumentEvents2Sink.Create;
  fDocEvents.OnEvent := HTMLEventHandler;
  fWdwEvents := THTMLWindowEvents2Sink.Create;
  fWdwEvents.OnEvent := HTMLEventHandler;
  fWdwEvents.OnError := HTMLWindowErrorHandler;
end;

destructor TWBIOMgr.Destroy;
begin
  fWdwEvents.Disconnect;
  FreeAndNil(fWdwEvents);
  fDocEvents.Disconnect;
  FreeAndNil(fDocEvents);
  inherited;
end;

procedure TWBIOMgr.DocCompleteHandler(Sender: TObject; const pDisp: IDispatch;
  var URL: OleVariant);
begin
  // Top level document has finished loading iff pDisp contains reference to
  // browser control's default interface.
  // See http://support.microsoft.com/kb/180366
  if pDisp = (fWB.DefaultInterface as IDispatch) then
    fDocLoaded := True;
end;

procedure TWBIOMgr.EmptyDocument;
begin
  // Load the special blank document
  NavigateToURL('about:blank');
end;

function TWBIOMgr.HTMLDocumentExists: Boolean;
begin
  Result := THTMLDOMHelper.IsValidDocument(fWB.Document);
end;

procedure TWBIOMgr.HTMLEventHandler(Sender: TObject;
  const EventInfo: THTMLEventInfo);
begin
  if Assigned(fOnHTMLEvent) then
    fOnHTMLEvent(Self, EventInfo);
end;

procedure TWBIOMgr.HTMLWindowErrorHandler(Sender: TObject; const Desc,
  URL: string; const Line: Integer; var Handled: Boolean);
begin
  if Assigned(fOnHTMLWindowError) then
    fOnHTMLWindowError(Self, Desc, URL, Line, Handled);
end;

procedure TWBIOMgr.InternalLoadDocumentFromStream(const Stream: TStream);
var
  PersistStreamInit: IPersistStreamInit;  // object used to load stream into doc
  StreamAdapter: IStream;                 // IStream interface to stream
begin
  Assert(Assigned(fWB.Document),
    ClassName + '.InternalLoadDocumentFromStream: No document loaded');
  // Get IPersistStreamInit interface on document object
  if fWB.Document.QueryInterface(
    IPersistStreamInit, PersistStreamInit
  ) = S_OK then
  begin
    // Clear document
    if PersistStreamInit.InitNew = S_OK then
    begin
      // Load data from Stream into WebBrowser
      StreamAdapter := TStreamAdapter.Create(Stream);
      PersistStreamInit.Load(StreamAdapter);
      // Wait for document to finish loading
      WaitForDocToLoad;
    end;
  end;
end;

procedure TWBIOMgr.LoadFromString(const HTML: string);
var
  Stm: TMemoryStream; // stream that received HTML to be loaded

  // ---------------------------------------------------------------------------
  /// Writes bytes from byte array B to Stm
  procedure WriteBytes(const B: TBytes);
  begin
    if Length(B) > 0 then
      Stm.WriteBuffer(Pointer(B)^, Length(B));
  end;
  // ---------------------------------------------------------------------------

begin
  Stm := TMemoryStream.Create;
  try
    // Write HTML in Unicode Little Endian format with BOM
    WriteBytes(TEncoding.Unicode.GetPreamble);
    WriteBytes(TEncoding.Unicode.GetBytes(HTML));
    Stm.Position := 0;
    EmptyDocument;
    InternalLoadDocumentFromStream(Stm);
  finally
    Stm.Free;
  end;
end;

procedure TWBIOMgr.NavigateHandler(Sender: TObject; const pDisp: IDispatch;
  var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
  var Cancel: WordBool);
var
  DoCancel: Boolean;  // re-typing of Cancel parameter
begin
  if Assigned(fOnNavigate) then
  begin
    DoCancel := Cancel;
    fOnNavigate(Sender, URL, DoCancel);
    Cancel := DoCancel;
  end;
  // Flag document as loading only if navigation was not cancelled. fDocLoaded
  // is set back to true in browser control's OnDocumentComplete event handler
  // which does not get fired if navigation was cancelled.
  if not Cancel then
    fDocLoaded := False;
end;

procedure TWBIOMgr.NavigateToResource(const Module: HMODULE; const ResName,
  ResType: PChar);
begin
  NavigateToURL(MakeResourceURL(Module, ResName, ResType));
end;

procedure TWBIOMgr.NavigateToURL(const URL: string);
begin
  // Do the navigation, don't use cache or history and wait for document to load
  fWB.Navigate(
    WideString(URL), navNoHistory or navNoReadFromCache or navNoWriteToCache
  );
  WaitForDocToLoad;
end;

procedure TWBIOMgr.ReplaceExistingBodyHTML(const HTML: string);
begin
  Assert(Self.HTMLDocumentExists,
    ClassName + '.ReplaceExistingBodyHTML: No HTML document exists');
  THTMLDOMHelper.SetInnerHTML(THTMLDOMHelper.GetBodyElem(fWB.Document), HTML);
end;

procedure TWBIOMgr.WaitForDocToLoad;
begin
  // NOTE: do not call this method in a FormCreate event handler since the
  // browser will never reach this state - use a FormShow event handler instead
  // Wait for OnDocumentComplete to fire
  while not fDocLoaded do
    Pause(20);
  // Belt and braces - wait for browser's READYSTATE_COMPLETE state and then
  // check we've actually got an HTML document loaded
  TWBControlHelper.WaitForValidDocToLoad(fWB);                 // can raise EBug
  // connect event sinks to browser document and window
  fDocEvents.Connect(fWB.Document);
  fWdwEvents.Connect(THTMLDOMHelper.ParentWindow(fWB.Document));
end;

end.

