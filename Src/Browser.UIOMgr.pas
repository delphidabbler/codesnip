{
 * Browser.UIOMgr.pas
 *
 * Class that wraps the IE web browser control and provides ability to load and
 * save HTML from files, streams or strings. Also simplifies navigation to
 * documents stored locally or in resources and exposes some HTML events.
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
 * The Original Code is Browser.UIOMgr.pas, formerly UWBIOMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Browser.UIOMgr;


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
      {Object constructor. Sets up object.
        @param WB [in] Managed webbrowser control. Must not be nil.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object and disconnects event sinks.
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
  SysUtils, ActiveX,
  // Project
  Browser.UControlHelper, UHTMLDocHelper, UResourceUtils;


{ TWBIOMgr }

constructor TWBIOMgr.Create(const WB: TWebBrowser);
  {Object constructor. Sets up object.
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
end;

destructor TWBIOMgr.Destroy;
  {Object destructor. Tears down object and disconnects event sinks.
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
  NavigateToURL('about:blank');
end;

function TWBIOMgr.HTMLDocumentExists: Boolean;
  {Checks if a valid HTML document is loaded in the browser control.
    @return True if valid HTML document is loaded, False if not.
  }
begin
  Result := THTMLDocHelper.IsValidDocument(fWB.Document);
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

procedure TWBIOMgr.LoadFromString(const HTML: string);
  {Loads and displays valid HTML source from a string.
    @param HTML [in] String containing the HTML source.
    @except EBug raised if document is not valid.
  }
var
  Stm: TMemoryStream; // stream that receives HTML to be loaded

  // ---------------------------------------------------------------------------
  // Writes bytes from byte array B to Stm
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
  NavigateToURL(MakeResourceURL(Module, ResName, ResType));
end;

procedure TWBIOMgr.NavigateToURL(const URL: string);
  {Navigates to a document at a specified URL.
    @param URL [in] Full URL of the document.
    @except EBug raised if new document is not valid.
  }
begin
  // Do the navigation, don't use cache or history and wait for document to load
  fWB.Navigate(
    WideString(URL), navNoHistory or navNoReadFromCache or navNoWriteToCache
  );
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
  Assert(THTMLDocHelper.IsValidDocument(fWB.Document),
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
  TWBControlHelper.WaitForValidDocToLoad(fWB);                 // can raise EBug
  // connect event sinks to browser document and window
  fDocEvents.Connect(fWB.Document);
  fWdwEvents.Connect(THTMLDocHelper.ParentWindow(fWB.Document));
end;

end.

