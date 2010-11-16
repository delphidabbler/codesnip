{
 * Web.UXMLRequestor.pas
 *
 * Implements a class that fetches XML documents from the internet via HTTP
 * requests, parses the XML and provides a reference to the DOM.
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
 * The Original Code is Web.UXMLRequestor.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Web.UXMLRequestor;


interface


uses
  // Project
  UExceptions, UXMLDocumentEx, Web.UHTTPEx;


type
  {
  TXMLRequestor:
    Class that fetches XML documents from the internet via HTTP requests, parses
    the XML and provides a reference to the DOM.
  }
  TXMLRequestor = class(TObject)
  strict private
    fHTTP: THTTPEx; // Object used to perform HTTP requests
    function DownloadDocument(const URI: string): string;
      {Downloads XML document from internet.
        @param URI [in] URI of required XML document.
        @return Content of XML document.
        @except EXMLRequestor raised if any web error encountered.
      }
    function ParseXML(const XML: string): IXMLDocumentEx;
      {Parses an XML document and provides reference to DOM.
        @param XML [in] Document's XML as text.
        @return Reference to XML document's DOM.
        @except EXMLRequestor raised if error parsing XML.
      }
  public
    constructor Create;
      {Object constructor. Sets up object.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    function GetDocument(const URI: string): IXMLDocumentEx;
      {Fetches XML document from internet, parses it and provides reference to
      its DOM.
        @param URI [in] URI of required XML document.
        @return Reference to XML document's DOM.
        @except EXMLRequestor raised if error in downloading or parsing XML.
      }
  end;

  {
  EXMLRequestor:
    Class of exceptions raised by TXMLRequestor.
  }
  EXMLRequestor = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils, XMLIntf,
  // Project
  Web.UExceptions;


{ TXMLRequestor }

constructor TXMLRequestor.Create;
  {Object constructor. Sets up object.
  }
begin
  inherited Create;
  fHTTP := THTTPEx.Create;
  fHTTP.MediaType := 'application/xml';
  fHTTP.ContentType := 'application/xml';
end;

destructor TXMLRequestor.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fHTTP.Free;
  inherited;
end;

function TXMLRequestor.DownloadDocument(const URI: string): string;
  {Downloads XML document from internet.
    @param URI [in] URI of required XML document.
    @return Content of XML document.
    @except EXMLRequestor raised if any web error encountered.
  }
begin
  try
    Result := fHTTP.GetText(URI);
  except
    on E: EWebError do // convert any web error to own exception type
      raise EXMLRequestor.Create(E);
  end;
end;

function TXMLRequestor.GetDocument(const URI: string): IXMLDocumentEx;
  {Fetches XML document from internet, parses it and provides reference to its
  DOM.
    @param URI [in] URI of required XML document.
    @return Reference to XML document's DOM.
    @except EXMLRequestor raised if error in downloading or parsing XML.
  }
var
  XML: string;  // content of downloaded XML document
begin
  XML := DownloadDocument(URI);
  Result := ParseXML(XML);
end;

function TXMLRequestor.ParseXML(const XML: string): IXMLDocumentEx;
  {Parses an XML document and provides reference to DOM.
    @param XML [in] Document's XML as text.
    @return Reference to XML document's DOM.
    @except EXMLRequestor raised if error parsing XML.
  }
begin
  try
    Result := TXMLDocumentEx.Create(nil);
    Result.LoadFromXML(XML);
    Result.Active := True;
  except
    on E: EXMLDocError do // convert any XML parsing error to own exception type
      raise EXMLRequestor.Create(E);
  end;
end;

end.
