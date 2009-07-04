{
 * UWBIOMgr.pas
 *
 * Class that wraps the IE web browser control and provides ability to load and
 * save HTML from files, streams or strings. Also simplifies navigation to
 * documents stored locally or in resources and exposes some HTML events.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 18 Feb 2005  - Deleted unused TWBIOMgr destructor.
 * v0.3 of 03 Jun 2005  - Added new methods to support loading a document and
 *                        replacing its <body> tag's inner HTML with specified
 *                        code.
 * v1.0 of 25 May 2006  - Improved and corrected comments.
 *                      - Relocated and rationalised $WARN directives.
 *                      - Removed unused unit reference and unused methods.
 * v1.1 of 17 Feb 2007  - Added new OnNavigate event triggered when browser is
 *                        about to navigate to a new document, enabling
 *                        intervention in navigation process.
 *                      - Changed SetBodyHTML method to use THTMLDocHelper
 *                        static class to set the body tag's inner HTML.
 *                      - Changed WaitForDocToLoad to use TWBHelper class to
 *                        perform the wait. Now checks document is valid and
 *                        raises EBug exception if not.
 * v1.2 of 18 Oct 2007  - Added support for HTML events triggered by browser.
 * v1.3 of 25 Jan 2009  - Added new HTMLDocumentExists and
 *                        ReplaceExistingBodyHTML methods.
 *                      - Removed unused ReplaceBodyHTML and HTMLErrorHandler
 *                        methods and OnHTMLError property.
 *                      - Assertions now get class name from ClassName method.
 *                      - Removed protected section, made private section strict
 *                        and made some public methods private.
 *                      - Removed unnecessary $WARN directive.
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
 * The Original Code is UWBIOMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UWBIOMgr;


interface


uses
  // Delphi
  SHDocVw, Classes,
  // Project
  UHTMLEvents;


type

  {
  TWBNavigateEvent:
    Type of event triggered just before browser control navigates to a new
    document. Handle this event to intervene in, cancel or get information about
    the navigation.
      @param Sender [in] Object triggering event.
      @param URL [in] URL to be navigated to.
      @param Cancel [in/out] False when called. Set true to prevent the browser
        navigating to the URL.
  }
  TWBNavigateEvent = procedure(Sender: TObject; const URL: string;
    var Cancel: Boolean) of object;

  {
  TWBIOMgr:
    Class that wraps the IE web browser control and provides ability to load and
    save HTML from files, streams or strings. Also simplifies navigation to
    documents stored locally or in resources and exposes some HTML events.
  }
  TWBIOMgr = class(TObject)
  strict private
    fWB: TWebBrowser;                 // Reference to managed webbrowser
    fOnNavigate: TWBNavigateEvent;    // Handler for OnNavigate event
    fOnHTMLEvent: THTMLEvent;         // Handler for OnHTMLEvent event
    fDocEvents: THTMLDocEventSink;    // Event sink for browser document events
    fWdwEvents: THTMLWdwEventSink;    // Event since for browser window events
    procedure HTMLEventHandler(Sender: TObject;
      const EventInfo: THTMLEventInfo);
      {Handles OnEvent events triggered by browser document and window event
      sinks.
        @param Sender [in] Not used.
        @param EventInfo [in] Object providing information about the event.
      }
    procedure WaitForDocToLoad;
      {Waits for a document to complete loading.
        @except EBug raised if there is no document or it is not a valid HTML
          document.
      }
    procedure NavigateHandler(Sender: TObject; const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
      {Handles web browser navigation events by triggering own OnNavigate event.
        @param Sender [in] Not used.
        @param pDisp [in] Not used.
        @param URL [in/out] URL to access, passed to OnNavigate event handler.
          Left unchanged.
        @param Flags [in/out] Not used.
        @param TargetFrameName [in/out] Not used.
        @param PostData [in/out] Not used.
        @param Headers [in/out] Not used.
        @param Cancel [in] False when passed in. Set to true to cancel browser's
          own navigation or leave false to permit browser to handle navigation.
      }
    procedure InternalLoadDocumentFromStream(const Stream: TStream);
      {Updates the web browser's current document from HTML read from stream.
        @param Stream [in] Stream containing valid HTML source code.
        @except EBug raised if updated document is not valid.
      }
    procedure SetBodyHTML(const HTML: string);
      {Sets inner HTML of browser's current document <body>.
        @param HTML [in] Required inner HTML for <body> tag.
      }
    procedure LoadFromStream(const Stream: TStream);
      {Loads and displays valid HTML document from the current location in a
      stream.
        @param Stream [in] Stream containing the HTML document.
        @except EBug raised if document is not valid.
      }
    procedure EmptyDocument;
      {Creates an empty document. This method guarantees that the browser
      contains a valid document object. The browser displays a blank page.
        @except EBug raised if document is not valid.
      }
    procedure NavigateToURL(const URL: string);
      {Navigates to a document at a specified URL.
        @param URL [in] Full URL of the document.
        @except EBug raised if new document is not valid.
      }
  public
    constructor Create(const WB: TWebBrowser);
      {Class constructor. Sets up object.
        @param WB [in] Managed webbrowser control. Must not be nil.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object and disconnects event sinks.
      }
    procedure LoadFromString(const HTML: string);
      {Loads and displays valid HTML source from a string.
        @param HTML [in] String containing the HTML source.
        @except EBug raised if document is not valid.
      }
    procedure NavigateToResource(const Module: HMODULE; const ResName: PChar;
      const ResType: PChar = nil);
      {Navigates to the document stored as a resource in a module.
        @param Module [in] Handle of module containing resource.
        @param ResName [in] Name of the resource
        @param ResType [in] Type of resource (RT_HTML is assumed if ResType is
          nil).
        @except EBug raised if document is not valid.
      }
    function HTMLDocumentExists: Boolean;
      {Checks if a valid HTML document is loaded in the browser control.
        @return True if valid HTML document is loaded, False if not.
      }
    procedure ReplaceExistingBodyHTML(const HTML: string);
      {Replaces body HTML of an existing HTML document.
        @param HTML [in] New body HTML.
      }
    property OnNavigate: TWBNavigateEvent
      read fOnNavigate write fOnNavigate;
      {Event triggered when browser control is about to navigate to a new
      document. Handle this event to intervene in navigation process}
    property OnHTMLEvent: THTMLEvent
      read fOnHTMLEvent write fOnHTMLEvent;
      {Event triggered in response to browser's "standard" window and document
      event sink events}
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils, ActiveX,
  // Project
  UHTMLDocHelper, UHTMLUtils, UWBHelper;


{ TWBIOMgr }

constructor TWBIOMgr.Create(const WB: TWebBrowser);
  {Class constructor. Sets up object.
    @param WB [in] Managed webbrowser control. Must not be nil.
  }
begin
  Assert(Assigned(WB), ClassName + '.Create: WB is nil');
  inherited Create;
  fWB := WB;
  fWB.OnBeforeNavigate2 := NavigateHandler;
  // Create event sinks and set event handlers
  fDocEvents := THTMLDocEventSink.Create;
  fDocEvents.OnEvent := HTMLEventHandler;
  fWdwEvents := THTMLWdwEventSink.Create;
  fWdwEvents.OnEvent := HTMLEventHandler;
//  fWdwEvents.OnError := HTMLErrorHandler;
end;

destructor TWBIOMgr.Destroy;
  {Class destructor. Tears down object and disconnects event sinks.
  }
begin
  fWdwEvents.Disconnect;
  FreeAndNil(fWdwEvents);
  fDocEvents.Disconnect;
  FreeAndNil(fDocEvents);
  inherited;
end;

procedure TWBIOMgr.EmptyDocument;
  {Creates an empty document. This method guarantees that the browser
  contains a valid document object. The browser displays a blank page.
    @except EBug raised if document is not valid.
  }
begin
  // Load the special blank document
  NavigateToURL('about:blank');                            // ** do not localise
end;

function TWBIOMgr.HTMLDocumentExists: Boolean;
  {Checks if a valid HTML document is loaded in the browser control.
    @return True if valid HTML document is loaded, False if not.
  }
begin
  Result := Assigned(fWB.Document) and
    THTMLDocHelper.IsValidDocument(fWB.Document);
end;

procedure TWBIOMgr.HTMLEventHandler(Sender: TObject;
  const EventInfo: THTMLEventInfo);
  {Handles OnEvent events triggered by browser document and window event sinks.
    @param Sender [in] Not used.
    @param EventInfo [in] Object providing information about the event.
  }
begin
  if Assigned(fOnHTMLEvent) then
    fOnHTMLEvent(Self, EventInfo);
end;

procedure TWBIOMgr.InternalLoadDocumentFromStream(const Stream: TStream);
  {Updates the web browser's current document from HTML read from stream.
    @param Stream [in] Stream containing valid HTML source code.
    @except EBug raised if updated document is not valid.
  }
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
      StreamAdapter:= TStreamAdapter.Create(Stream);
      PersistStreamInit.Load(StreamAdapter);
      // Wait for document to finish loading
      WaitForDocToLoad;
    end;
  end;
end;

procedure TWBIOMgr.LoadFromStream(const Stream: TStream);
  {Loads and displays valid HTML document from the current location in a stream.
    @param Stream [in] Stream containing the HTML document.
    @except EBug raised if document is not valid.
  }
begin
  // Must read into existing document: so load about:blank and ensure loaded
  EmptyDocument;
  // Now do the loading and wait to complete
  InternalLoadDocumentFromStream(Stream);
end;

procedure TWBIOMgr.LoadFromString(const HTML: string);
  {Loads and displays valid HTML source from a string.
    @param HTML [in] String containing the HTML source.
    @except EBug raised if document is not valid.
  }
var
  StringStream: TStringStream;  // stream onto string
begin
  StringStream := TStringStream.Create(HTML);
  try
    LoadFromStream(StringStream);
  finally
    StringStream.Free;
  end;
end;

procedure TWBIOMgr.NavigateHandler(Sender: TObject; const pDisp: IDispatch;
  var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
  var Cancel: WordBool);
  {Handles web browser navigation events by triggering own OnNavigate event.
    @param Sender [in] Not used.
    @param pDisp [in] Not used.
    @param URL [in/out] URL to access, passed to OnNavigate event handler.
      Left unchanged.
    @param Flags [in/out] Not used.
    @param TargetFrameName [in/out] Not used.
    @param PostData [in/out] Not used.
    @param Headers [in/out] Not used.
    @param Cancel [in] False when passed in. Set to true to cancel browser's own
      navigation or leave false to permit browser to handle navigation.
  }
var
  DoCancel: Boolean;  // re-typing of Cancel parameter
begin
  if Assigned(fOnNavigate) then
  begin
    DoCancel := Cancel;
    fOnNavigate(Sender, URL, DoCancel);
    Cancel := DoCancel;
  end;
end;

procedure TWBIOMgr.NavigateToResource(const Module: HMODULE; const ResName,
  ResType: PChar);
  {Navigates to the document stored as a resource in a module.
    @param Module [in] Handle of module containing resource.
    @param ResName [in] Name of the resource
    @param ResType [in] Type of resource (RT_HTML is assumed if ResType is nil).
    @except EBug raised if document is not valid.
  }
begin
  // Create res:// protocol for given module name and navigate to it
  NavigateToURL(MakeResourceURL(Module, ResName, ResType));
end;

procedure TWBIOMgr.NavigateToURL(const URL: string);
  {Navigates to a document at a specified URL.
    @param URL [in] Full URL of the document.
    @except EBug raised if new document is not valid.
  }
var
  Flags: OleVariant;    // flags that determine action
begin
  // ** do not localise string literals in this method
  // Don't record in history
  Flags := navNoHistory;
  if AnsiStartsText('res://', URL) or AnsiStartsText('file://', URL)
    or AnsiStartsText('about:', URL) or AnsiStartsText('javascript:', URL)
    or AnsiStartsText('mailto:', URL) then
    // don't use cache for local files
    Flags := Flags or navNoReadFromCache or navNoWriteToCache;
  // Do the navigation and wait for it to complete loading
  fWB.Navigate(WideString(URL), Flags);
  WaitForDocToLoad;
end;

procedure TWBIOMgr.ReplaceExistingBodyHTML(const HTML: string);
  {Replaces body HTML of an existing HTML document.
    @param HTML [in] New body HTML.
  }
begin
  Assert(Self.HTMLDocumentExists,
    ClassName + '.ReplaceExistingBodyHTML: No HTML document exists');
  SetBodyHTML(HTML);
end;

procedure TWBIOMgr.SetBodyHTML(const HTML: string);
  {Sets inner HTML of browser's current document <body>.
    @param HTML [in] Required inner HTML for <body> tag.
  }
begin
  Assert(THTMLDocHelper.IsValidDocument(fWB.Document),     // ** do not localise
    ClassName + '.SetBodyHTML: Invalid or no document loaded in browser control'
  );
  THTMLDocHelper.SetInnerHTML(THTMLDocHelper.GetBodyElem(fWB.Document), HTML);
end;

procedure TWBIOMgr.WaitForDocToLoad;
  {Waits for a document to complete loading.
    @except EBug raised if there is no document or it is not a valid HTML
      document.
  }
begin
  // NOTE: do not call this method in a FormCreate event handler since the
  // browser will never reach this state - use a FormShow event handler instead
  TWBHelper.WaitForValidDocToLoad(fWB);                        // can raise EBug
  // connect event sinks to browser document and window
  fDocEvents.Connect(fWB.Document);
  fWdwEvents.Connect(THTMLDocHelper.ParentWindow(fWB.Document));
end;

end.

