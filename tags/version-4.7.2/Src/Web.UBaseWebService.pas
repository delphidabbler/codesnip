{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides a base class for all classes that access web services.
}


unit Web.UBaseWebService;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Projects
  UURIParams, Web.UHTTPEx, Web.UInfo;


type

  {
  TWebServiceProgressEvent:
    Type of event handler used to handle TBaseWebService.OnProgress events. Used
    to monitor download progress.
      @param Sender [in] Reference to TBaseWebService object triggering event.
      @param BytesToDate [in] Number of bytes downloaded.
      @param BytesExpected [in] Total number of bytes to be downloaded.
  }
  TWebServiceProgressEvent = procedure(Sender: TObject; const BytesToDate,
    BytesExpected: Int64) of object;

  {
  TBaseWebService:
    Base class for all web service classes. Provides helper methods to ease
    interaction with web services.
  }
  TBaseWebService = class(TObject)
  strict private
    fHTTP: THTTPEx;                         // Class used for HTTP requests
    fScriptURI: string;                     // URI of web service
    fOnProgress: TWebServiceProgressEvent;  // OnProgress event handler
    fWantProgress: Boolean;                 // Value of WantProgress property
    procedure DoPostParams(const Params: TURIParams;
      const PostProc: TProc<TStream>);
      {Helper method that assists in performing POST requests that send encoded
      query strings. Converts query string to raw data, sets required content
      type then hands off actual POST request to a provided anonymous method.
        @param Params [in] Parameters to be POSTed.
        @param PostProc [in] Anonymous method that performs POST request and
          processes response.
      }
    function BuildURI(const ResourcePath: string;
      const Params: TURIParams = nil): string;
      {Builds a URI from web service name and any required parameters.
        @param ResourcePath [in] Path to append to web service URI to specify
          resource in REST APIs.
        @param Params [in] Parameters to append to URI as query string. May be
          empty or nil.
        @return Required URI.
      }
    function GetResponseCharSet: string;
      {Read accessor for ResponseCharSet property.
        @return Character set used by last HTTP response.
      }
  strict protected
    property WantProgress: Boolean read fWantProgress write fWantProgress;
      {Flag that indicates if progress reporting is required. When True the
      OnProgress event is triggered if assigned. The OnProgress event is not
      triggered if this property is False}
    function GetRaw(const ResourcePath: string; const Params: TURIParams = nil):
      TBytes;
      {Performs a GET request on web service with optional query string and
      returns response as raw data.
        @param ResourcePath [in] Path to append to web service URI to specify
          resource in REST APIs.
        @param Params [in] Optional parameters to include in query string.
        @return Response as raw byte array.
      }
    function GetText(const ResourcePath: string;
      const Params: TURIParams = nil): string;
      {Performs a GET request on web service with optional query string and
      returns response as text.
        @param ResourcePath [in] Path to append to web service URI to specify
          resource in REST APIs.
        @param Params [in] Optional parameters to include in query string.
        @return Response as text, decoded according to response header.
      }
    procedure GetStrings(const ResourcePath: string; const Params: TURIParams;
      const Strings: TStrings); overload;
      {Performs a GET request on web service with query string and stores
      response in string list.
        @param ResourcePath [in] Path to append to web service URI to specify
          resource in REST APIs.
        @param Params [in] Parameters to include in query string. May be empty
          or nil.
      }
    procedure GetStrings(const ResourcePath: string; const Strings: TStrings);
      overload;
      {Performs a GET request on web service and stores response in string list.
        @param ResourcePath [in] Path to append to web service URI to specify
          resource in REST APIs.
        @params Strings [in] String list that receives response.
      }
    function PostRaw(const Data: TStream): TBytes; overload;
      {Performs a POST request on web service sending data from a stream and
      returns response as raw data.
        @param Data [in] Data to be posted. Must not be nil.
        @return Response as raw byte array.
      }
    function PostText(const Data: TStream): string; overload;
      {Performs a POST request on web service sending data from a stream and
      returns response as text.
        @param Data [in] Data to be posted. Must not be nil.
        @return Response as text, decoded according to response header.
      }
    function PostText(const Data: TBytes): string; overload;
      {Performs a POST request on web service sending data from a byte array and
      returns response as text.
        @param Data [in] Data to be posted.
        @return Response as text, decoded according to response header.
      }
    function PostRaw(const Params: TURIParams): TBytes; overload;
      {Performs a POST request on web service sending query strings and returns
      response as raw data.
        @param Params [in] Parameters to be posted. Must not be nil.
        @return Response as raw byte array.
      }
    function PostText(const Params: TURIParams): string; overload;
      {Performs a POST request on web service sending query strings and returns
      response as text.
        @param Params [in] Parameters to be posted. Must not be nil.
        @return Response as text, decoded according to response header.
      }
    procedure PostStrings(const Data: TStream; const Strings: TStrings);
      overload;
      {Performs a POST request on web service sending data from a stream and
      stores response in a string list.
        @param Data [in] Data to be posted. Must not be nil.
        @param Strings [in] String list that receives response.
      }
    procedure PostStrings(const Data: TBytes; const Strings: TStrings);
      overload;
      {Performs a POST request on web service sending data from a byte array and
      stores response in a string list.
        @param Data [in] Data to be posted.
        @param Strings [in] String list that receives response.
      }
    procedure PostStrings(const Params: TURIParams; const Strings: TStrings);
      overload;
      {Performs a POST request on web service sending query strings and stores
      response in a string list.
        @param Params [in] Parameters to be posted. Must not be nil.
        @param Strings [in] String list that receives response.
      }
  public
    constructor Create(const WebInfo: TWebServiceInfo);
      {Constructor. Creates and initialises web service object.
        @param WebInfo [in] Information about the web service.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    property OnProgress: TWebServiceProgressEvent
      read fOnProgress write fOnProgress;
      {Event that can be triggered to monitor progress of downloads. Event is
      only triggered when WantProgress property is true}
    property ResponseCharSet: string read GetResponseCharSet;
      {Character set used for last response from web server}
  end;


implementation


uses
  // Project
  UStrUtils;


{ TBaseWebService }

function TBaseWebService.BuildURI(const ResourcePath: string;
  const Params: TURIParams): string;
  {Builds a URI from web service name and any required parameters.
    @param ResourcePath [in] Path to append to web service URI to specify
      resource in REST APIs.
    @param Params [in] Parameters to append to URI as query string. May be empty
      or nil.
    @return Required URI.
  }
begin
  Result := fScriptURI;
  if ResourcePath <> '' then
    Result := Result + '/' + ResourcePath;
  if Assigned(Params) and not Params.IsEmpty then
    Result := Result + '?' + Params.EncodedQueryString;
end;

constructor TBaseWebService.Create(const WebInfo: TWebServiceInfo);
  {Constructor. Creates and initialises web service object.
    @param WebInfo [in] Information about the web service.
  }
begin
  inherited Create;
  // Create and initialise HTTP request object
  fHTTP := THTTPEx.Create(
    procedure (BytesReceived, BytesExpected: Int64)
    begin
      if fWantProgress and Assigned(fOnProgress) then
        fOnProgress(Self, BytesReceived, BytesExpected);
    end
  );
  fHTTP.UserAgent := WebInfo.UserAgent;
  fHTTP.MediaType := WebInfo.MediaType + ', */*';
  // Record script URI for future use
  fScriptURI := WebInfo.ScriptURI;
end;

destructor TBaseWebService.Destroy;
  {Destructor. Tears down object.
  }
begin
  fHTTP.Free;
  inherited;
end;

procedure TBaseWebService.DoPostParams(const Params: TURIParams;
  const PostProc: TProc<TStream>);
  {Helper method that assists in performing POST requests that send encoded
  query strings. Converts query string to raw data, sets required content type
  then hands off actual POST request to a provided anonymous method.
    @param Params [in] Parameters to be POSTed.
    @param PostProc [in] Anonymous method that performs POST request and
      processes response.
  }
var
  Data: TStream;  // contains data sent to web service as part of POST request
begin
  // Create URI encoded parameters and store in stream. Encoded parameters are
  // in ASCII format
  Data := TBytesStream.Create(
    TEncoding.ASCII.GetBytes(Params.EncodedQueryString)
  );
  Data.Position := 0;
  try
    // Perform POST
    fHTTP.ContentType := 'application/x-www-form-urlencoded';
    try
      PostProc(Data);
    finally
      fHTTP.ContentType := '';
    end;
  finally
    Data.Free;
  end;
end;

function TBaseWebService.GetRaw(const ResourcePath: string;
  const Params: TURIParams = nil): TBytes;
  {Performs a GET request on web service with optional query string and returns
  response as raw data.
    @param ResourcePath [in] Path to append to web service URI to specify
      resource in REST APIs.
    @param Params [in] Optional parameters to include in query string.
    @return Response as raw byte array.
  }
begin
  Result := fHTTP.GetRaw(BuildURI(ResourcePath, Params));
end;

function TBaseWebService.GetResponseCharSet: string;
  {Read accessor for ResponseCharSet property.
    @return Character set used by last HTTP response.
  }
begin
  Result := fHTTP.ResponseCharSet;
end;

procedure TBaseWebService.GetStrings(const ResourcePath: string;
  const Strings: TStrings);
  {Performs a GET request on web service and stores response in string list.
    @param ResourcePath [in] Path to append to web service URI to specify
      resource in REST APIs.
    @params Strings [in] String list that receives response.
  }
begin
  GetStrings(ResourcePath, nil, Strings);
end;

procedure TBaseWebService.GetStrings(const ResourcePath: string;
  const Params: TURIParams; const Strings: TStrings);
  {Performs a GET request on web service with query string and stores response
  in string list.
    @param ResourcePath [in] Path to append to web service URI to specify
      resource in REST APIs.
    @param Params [in] Parameters to include in query string. May be empty or
      nil.
  }
begin
  Strings.Text := StrTrim(GetText(ResourcePath, Params));
end;

function TBaseWebService.GetText(const ResourcePath: string;
  const Params: TURIParams = nil): string;
  {Performs a GET request on web service with optional query string and returns
  response as text.
    @param ResourcePath [in] Path to append to web service URI to specify
      resource in REST APIs.
    @param Params [in] Optional parameters to include in query string.
    @return Response as text, decoded according to response header.
  }
begin
  Result := fHTTP.GetText(BuildURI(ResourcePath, Params));
end;

function TBaseWebService.PostRaw(const Params: TURIParams): TBytes;
  {Performs a POST request on web service sending query strings and returns
  response as raw data.
    @param Params [in] Parameters to be posted. Must not be nil.
    @return Response as raw byte array.
  }
var
  RetVal: TBytes; // return value from POST request
begin
  DoPostParams(
    Params,
    procedure(Data: TStream)
    begin
      RetVal := PostRaw(Data)
    end
  );
  Result := RetVal;
end;

function TBaseWebService.PostRaw(const Data: TStream): TBytes;
  {Performs a POST request on web service sending data from a stream and returns
  response as raw data.
    @param Data [in] Data to be posted. Must not be nil.
    @return Response as raw byte array.
  }
var
  RequestData: TBytes;  // request data as byte array
begin
  SetLength(RequestData, Data.Size);
  Data.Position := 0;
  Data.ReadBuffer(Pointer(RequestData)^, Data.Size);
  Result := fHTTP.PostRaw(fScriptURI, RequestData);
end;

procedure TBaseWebService.PostStrings(const Params: TURIParams;
  const Strings: TStrings);
  {Performs a POST request on web service sending query strings and stores
  response in a string list.
    @param Params [in] Parameters to be posted. Must not be nil.
    @param Strings [in] String list that receives response.
  }
begin
  Strings.Text := StrTrim(PostText(Params));
end;

procedure TBaseWebService.PostStrings(const Data: TBytes;
  const Strings: TStrings);
  {Performs a POST request on web service sending data from a byte array and
  stores response in a string list.
    @param Data [in] Data to be posted.
    @param Strings [in] String list that receives response.
  }
begin
  Strings.Text := StrTrim(PostText(Data));
end;

procedure TBaseWebService.PostStrings(const Data: TStream;
  const Strings: TStrings);
  {Performs a POST request on web service sending data from a stream and stores
  response in a string list.
    @param Data [in] Data to be posted. Must not be nil.
    @param Strings [in] String list that receives response.
  }
begin
  Strings.Text := StrTrim(PostText(Data));
end;

function TBaseWebService.PostText(const Params: TURIParams): string;
  {Performs a POST request on web service sending query strings and returns
  response as text.
    @param Params [in] Parameters to be posted. Must not be nil.
    @return Response as text, decoded according to response header.
  }
var
  RetVal: string; // return value from POST request
begin
  DoPostParams(
    Params,
    procedure(Data: TStream)
    begin
      RetVal := PostText(Data);
    end
  );
  Result := RetVal;
end;

function TBaseWebService.PostText(const Data: TBytes): string;
  {Performs a POST request on web service sending data from a byte array and
  returns response as text.
    @param Data [in] Data to be posted.
    @return Response as text, decoded according to response header.
  }
begin
  Result := fHTTP.PostText(fScriptURI, Data);
end;

function TBaseWebService.PostText(const Data: TStream): string;
  {Performs a POST request on web service sending data from a stream and returns
  response as text.
    @param Data [in] Data to be posted. Must not be nil.
    @return Response as text, decoded according to response header.
  }
var
  RequestData: TBytes;  // request data as byte array
begin
  SetLength(RequestData, Data.Size);
  Data.Position := 0;
  Data.ReadBuffer(Pointer(RequestData)^, Data.Size);
  Result := PostText(RequestData);
end;

end.

