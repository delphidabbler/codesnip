{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Class that makes HTTP requests over the internet. Correctly decodes text
 * according to the character set defined in the HTTP response and validates
 * responses that include a checksum. Uses Indy components to make the actual
 * HTTP request.
}


unit Web.UHTTPEx;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Indy
  IdHTTP, IdAntiFreeze, IdException,
  // Project
  UURIParams, Web.UDownloadMonitor;


type

  {
  THTTPDownloadProgressProc:
    Type of closure provided to THTTPEx constructor when download progress
    reports are required.
      @param BytesReceived [in] Number of bytes received to date.
      @param BytesExpected [in] Total number of bytes expected in download.
  }
  THTTPDownloadProgressProc = reference to procedure(BytesReceived,
    BytesExpected: Int64);

  {
  THTTPEx:
    Class that makes HTTP requests over the internet and processes responses
    according to response header information. Text responses are correctly
    decoded according to the response character set and any provided checksums
    are validated.
  }
  THTTPEx = class(TObject)
  strict private
    var
      fHTTP: TIdHTTP;                       // Component used for HTTP requests
      fDownloadMonitor: TDownloadMonitor;   // Monitors download progress
      fProgress: THTTPDownloadProgressProc; // Closure used to report progress
      fMediaType: string;                   // Value of MediaType property
    class var
      fAntiFreeze: TIdAntiFreeze;           // Prevents HTTP requests blocking
    function GetUserAgent: string;
      {Read accessor for UserAgent property.
        @return String describing user agent.
      }
    procedure SetUserAgent(const Value: string);
      {Write accessor for UserAgent property.
        @param Value [in] New property value.
      }
    procedure SetMediaType(const Value: string);
      {Write accessor for MediaType property.
        @param Value [in] New property value.
      }
    function GetContentType: string;
      {Read accessor for ContentType property.
        @return String describing content type.
      }
    procedure SetContentTtype(const Value: string);
      {Write accessor for ContentType property.
        @param Value [in] New property value.
      }
    function GetResponseCharSet: string;
      {Read accessor for ResponseCharSet property.
        @return String containing character set.
      }
  strict protected
    function DoRequestText(const Requestor: TProc<TBytesStream>): string;
      {Performs an HTTP request returning response as text decoded according to
      code page specified in HTTP response.
        @param Requestor [in] Closure that performs a suitable HTTP request.
        @return Response as text.
      }
    function DoRequestRaw(const Requestor: TProc<TBytesStream>): TBytes;
      {Performs an HTTP request returning response as raw data.
        @param Requestor [in] Closure that performs a suitable HTTP request.
        @return Response as raw data.
      }
    procedure DoProgress;
      {Event handler for download monitor's OnProgress event.
      }
    procedure HandleException(const E: EIdException);
      {Handles exceptions raised when performing HTTP requests. Converts some
      Indy exception types to alternative application exception types.
        @param E [in] Exception to be handled.
        @except An exception of either converted or original type is always
          raised.
      }
    procedure ValidateContent(const Content: TStream);
      {Validates HTTP response data against MD5 checksum passed in response's
      Content-MD5 header. Must only be called if Content-MD5 header is present.
        @param Content [in] Stream containing content to be checked.
        @except Exceptions are raised if Content-MD5 header is invalid or if
          content checksum is invalid.
      }
    function ResponseHasChecksum: Boolean;
      {Checks if HTTP response has a checksum included.
        @return True if there is a checsum, False if not.
      }
  public
    class constructor Create;
      {Class constructor. Creates instance of Indy's antifreeze component of
      which there must only be one per application.
      }
    class destructor Destroy;
      {Class destructor. Frees the Indy antifreeze singleton.
      }
    constructor Create(Progress: THTTPDownloadProgressProc); overload;
      {Object constructor. Creates object with progress reporting via provided
      closure.
        @param Progress [in] Closure that handles progress report events.
      }
    constructor Create; overload;
      {Object constructor. Creates object with no progress reporting.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object and frees resources.
      }
    function GetRaw(const URI: string): TBytes;
      {Performs an HTTP GET request returning response as raw data.
        @param URI [in] URI to use for request.
        @return Response as raw data.
      }
    function GetText(const URI: string): string;
      {Performs an HTTP GET request returning response as text decoded according
      to code page specified in HTTP response.
        @param URI [in] URI to use for request.
        @return Response as text.
      }
    function PostRaw(const URI: string; const RequestData: TBytes): TBytes;
      {Performs an HTTP POST request returning response as raw data.
        @param URI [in] URI to use for request.
        @param RequestData [in] Data sent as part of request.
        @return Response as raw data.
      }
    function PostText(const URI: string; const RequestData: TBytes): string;
      {Performs an HTTP POST request returning response as text decoded
      according to code page specified in HTTP response.
        @param URI [in] URI to use for request.
        @param RequestData [in] Data sent as part of request.
        @return Response as text.
      }
    property UserAgent: string read GetUserAgent write SetUserAgent;
      {User agent to be sepcified when making HTTP requests}
    property MediaType: string read fMediaType write SetMediaType;
      {Media type specified when making HTTP requests}
    property ContentType: string read GetContentType write SetContentTtype;
      {Content type specified when making HTTP requests}
    property ResponseCharSet: string read GetResponseCharSet;
      {Character set used for HTTP response}
  end;


implementation


uses
  // Indy
  IdCoderMIME, IdStack,
  // 3rd party
  PJMD5,
  // Project
  UConsts, UStrUtils, Web.UCharEncodings, Web.UExceptions, Web.UInfo;


resourcestring
  // Error messages
  sWebConnectionError = 'There was a problem accessing the internet. Please '
    + 'check your web connection. '
    + 'If you are using a proxy server please check its configuration.'
    + EOL2
    + 'The error reported by Windows was: %0:s.';
  sWebValidationError = 'Validation error: checksum failed. This may have been '
    + 'a transmission error.';
  sWebBase64Error = 'Validation error: checksum not transmitted correctly.';


function Base64Decode(const EncodedText: string): TBytes;
  {Decodes Base64 encoded text into raw data.
    @param EncodedText [in] Base64 encoded text.
    @return Decoded data as byte array.
  }
var
  DecodedStm: TBytesStream; // stream that receives decoded data
  Decoder: TIdDecoderMIME;  // object used to perform decoding
begin
  Decoder := TIdDecoderMIME.Create(nil);
  try
    DecodedStm := TBytesStream.Create;
    try
      Decoder.DecodeBegin(DecodedStm);
      Decoder.Decode(EncodedText);
      Decoder.DecodeEnd;
      Result := DecodedStm.Bytes;
    finally
      DecodedStm.Free;
    end;
  finally
    Decoder.Free;
  end;
end;

{ THTTPEx }

constructor THTTPEx.Create;
  {Constructor. Creates object with no progress reporting.
  }
begin
  // Pass a do-nothing closure to main constructor
  Create(procedure(Received, Expected: Int64) begin end);
end;

constructor THTTPEx.Create(Progress: THTTPDownloadProgressProc);
  {Constructor. Creates object with progress reporting via provided closure.
    @param Progress [in] Closure that handles progress report events.
  }
var
  ProxyInfo: TWebProxyInfo; // details of any proxy server
begin
  inherited Create;
  fHTTP := TIdHTTP.Create(nil);
  fHTTP.HTTPOptions := fHTTP.HTTPOptions - [hoForceEncodeParams];
  fHTTP.Request.AcceptCharSet := TWebCharEncodings.AcceptCharSet;
  fHTTP.Request.AcceptLanguage := 'en-gb, en;q=0.8';
  // Get proxy info
  ProxyInfo := TWebInfo.WebProxyInfo;
  if ProxyInfo.UseProxy then
  begin
    fHTTP.ProxyParams.ProxyServer := ProxyInfo.IPAddress;
    fHTTP.ProxyParams.ProxyPort := ProxyInfo.Port;
    fHTTP.ProxyParams.ProxyUsername := ProxyInfo.UserName;
    fHTTP.ProxyParams.ProxyPassword := ProxyInfo.Password;
  end;
  // Set up download progress monitoring
  fProgress := Progress;
  fDownloadMonitor := TDownloadMonitor.Create(fHTTP, DoProgress);
end;

class constructor THTTPEx.Create;
  {Class constructor. Creates instance of Indy's antifreeze component of which
  there must only be one per application.
  }
begin
  fAntiFreeze := TIdAntiFreeze.Create(nil);
end;

destructor THTTPEx.Destroy;
  {Destructor. Tears down object and frees resources.
  }
begin
  fDownloadMonitor.Free;
  fHTTP.Free;
  inherited;
end;

class destructor THTTPEx.Destroy;
  {Class destructor. Frees the Indy antifreeze singleton.
  }
begin
  fAntiFreeze.Free;
end;

procedure THTTPEx.DoProgress;
  {Event handler for download monitor's OnProgress event.
  }
begin
  // Call user-supplied closure used to do reporting
  fProgress(fDownloadMonitor.BytesReceived, fDownloadMonitor.BytesExpected);
end;

function THTTPEx.DoRequestRaw(const Requestor: TProc<TBytesStream>): TBytes;
  {Performs an HTTP request returning response as raw data.
    @param Requestor [in] Closure that performs a suitable HTTP request.
    @return Response as raw data.
  }
var
  Response: TBytesStream; // receives response from web service as raw bytes
begin
  // Set up request
  Response := TBytesStream.Create;
  try
    // Do request, recording response in byte stream
    try
      Requestor(Response);
    except
      on E: EIdException do
        HandleException(E);
    end;
    Response.Position := 0;
    // Process reponse
    if fHTTP.Response.HasContentLength then
      Response.SetSize(fHTTP.Response.ContentLength);
    if ResponseHasChecksum then
      // Response has MD5 checksum: check content is OK
      ValidateContent(Response);
    Result := Response.Bytes;
  finally
    Response.Free;
  end;
end;

function THTTPEx.DoRequestText(const Requestor: TProc<TBytesStream>): string;
  {Performs an HTTP request returning response as text decoded according to
  code page specified in HTTP response.
    @param Requestor [in] Closure that performs a suitable HTTP request.
    @return Response as text.
  }
var
  Content: TBytes;      // raw data received from web service
  Encoding: TEncoding;  // encoding specified as part of HTTP response
begin
  // Perform request, getting raw content
  Content := DoRequestRaw(Requestor);
  // Get text from raw data, decoded according to HTTP response header
  Encoding := TWebCharEncodings.GetEncoding(GetResponseCharSet);
  try
    Result := Encoding.GetString(Content);
  finally
    TWebCharEncodings.FreeEncoding(Encoding);
  end;
end;

function THTTPEx.GetContentType: string;
  {Read accessor for ContentType property.
    @return String describing content type.
  }
begin
  Result := fHTTP.Request.ContentType;
end;

function THTTPEx.GetRaw(const URI: string): TBytes;
  {Performs an HTTP GET request returning response as raw data.
    @param URI [in] URI to use for request.
    @return Response as raw data.
  }
begin
  Result := DoRequestRaw(
    procedure(ResponseStream: TBytesStream)
    begin
      fHTTP.Get(URI, ResponseStream)
    end
  );
end;

function THTTPEx.GetResponseCharSet: string;
  {Read accessor for ResponseCharSet property.
    @return String containing character set.
  }
begin
  if fHTTP.Response.CharSet <> '' then
    Result := fHTTP.Response.CharSet
  else
    Result := TWebCharEncodings.DefaultCharSet;
end;

function THTTPEx.GetText(const URI: string): string;
  {Performs an HTTP GET request returning response as text decoded according to
  code page specified in HTTP response.
    @param URI [in] URI to use for request.
    @return Response as text.
  }
begin
  Result := DoRequestText(
    procedure(ResponseStream: TBytesStream)
    begin
      fHTTP.Get(URI, ResponseStream)
    end
  )
end;

function THTTPEx.GetUserAgent: string;
  {Read accessor for UserAgent property.
    @return String describing user agent.
  }
begin
  Result := fHTTP.Request.UserAgent;
end;

procedure THTTPEx.HandleException(const E: EIdException);
  {Handles exceptions raised when performing HTTP requests. Converts some Indy
  exception types to alternative application exception types.
    @param E [in] Exception to be handled.
    @except An exception of either converted or original type is always raised.
  }
begin
  if E is EIdHTTPProtocolException then
    raise EHTTPError.Create(E as EIdHTTPProtocolException)
  else if E is EIdSocketError then
    raise EWebConnectionError.CreateFmt(
      sWebConnectionError, [StrTrim(E.Message)]
    )
  else
    raise E;
end;

function THTTPEx.PostRaw(const URI: string; const RequestData: TBytes): TBytes;
  {Performs an HTTP POST request returning response as raw data.
    @param URI [in] URI to use for request.
    @param RequestData [in] Data sent as part of request.
    @return Response as raw data.
  }
var
  RequestStream: TBytesStream;  // stream to contain request data
begin
  RequestStream := TBytesStream.Create(RequestData);
  try
    Result := DoRequestRaw(
      procedure(ResponseStream: TBytesStream)
      begin
        fHTTP.Post(URI, RequestStream, ResponseStream)
      end
    );
  finally
    RequestStream.Free;
  end;
end;

function THTTPEx.PostText(const URI: string; const RequestData: TBytes): string;
  {Performs an HTTP POST request returning response as text decoded according to
  code page specified in HTTP response.
    @param URI [in] URI to use for request.
    @param RequestData [in] Data sent as part of request.
    @return Response as text.
  }
var
  RequestStream: TBytesStream;  // stream to contain request data
begin
  RequestStream := TBytesStream.Create(RequestData);
  try
    Result := DoRequestText(
      procedure(ResponseStream: TBytesStream)
      begin
        fHTTP.Post(URI, RequestStream, ResponseStream)
      end
    );
  finally
    RequestStream.Free;
  end;
end;

function THTTPEx.ResponseHasChecksum: Boolean;
  {Checks if HTTP response has a checksum included.
    @return True if there is a checsum, False if not.
  }
begin
  Result := fHTTP.Response.RawHeaders.IndexOfName('Content-MD5') >= 0;
end;

procedure THTTPEx.SetContentTtype(const Value: string);
  {Write accessor for ContentType property.
    @param Value [in] New property value.
  }
begin
  fHTTP.Request.ContentType := Value;
end;

procedure THTTPEx.SetMediaType(const Value: string);
  {Write accessor for MediaType property.
    @param Value [in] New property value.
  }
begin
  fMediaType := Value;
  fHTTP.Request.Accept := fMediaType + ', */*';
end;

procedure THTTPEx.SetUserAgent(const Value: string);
  {Write accessor for UserAgent property.
    @param Value [in] New property value.
  }
begin
  fHTTP.Request.UserAgent := Value;
end;

procedure THTTPEx.ValidateContent(const Content: TStream);
  {Validates HTTP response data against MD5 checksum passed in response's
  Content-MD5 header. Must only be called if Content-MD5 header is present.
    @param Content [in] Stream containing content to be checked.
    @except Exceptions are raised if Content-MD5 header is invalid or if content
      checksum is invalid.
  }
var
  HeaderMD5Encoded: string;   // encoded md5 from Content-MD5 header
  HeaderMD5: TPJMD5Digest;    // decoded md5 from Content-MD5 header
  ContentMD5: TPJMD5Digest;   // md5 of Content stream
begin
  Assert(ResponseHasChecksum,
    ClassName + '.ValidateContent: No Content-MD5 header present in response');
  // get MD5 from header
  HeaderMD5Encoded := fHTTP.Response.RawHeaders.Values['Content-MD5'];
  try
    HeaderMD5 := Base64Decode(HeaderMD5Encoded);
  except
    raise EWebTransmissionError.Create(sWebBase64Error);
  end;
  // calculate MD5 of received content
  ContentMD5 := TPJMD5.Calculate(Content);
  // check that both MD5s are same and raise exception if not
  if HeaderMD5 <> ContentMD5 then
    raise EWebTransmissionError.Create(sWebValidationError);
end;

end.

