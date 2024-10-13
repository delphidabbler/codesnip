{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Data type that parses and encapsulates a URI.
}

unit CSLE.Utils.URI;

interface

uses

  System.Net.URLClient,
  CSLE.Exceptions;

type
  TURI = record
  strict private
    var
      // Record containing URI information. Must not be accessed if fEmpty is
      // True.
      fURI: System.Net.URLClient.TURI;
      // Flag that indicates whether the URI is empty or not.
      fIsEmpty: Boolean;
    ///  <summary>Attempts to deconstruct a non-empty <c>AURIStr</c> into its
    ///  component parts which are past out in <c>AURI</c>. Returns <c>True</c>
    ///  if the process succeeds or <c>False</c> on error.</summary>
    class function TryDeconstructURI(const AURIStr: string;
      out AURI: System.Net.URLClient.TURI): Boolean; static;
    // Property accessors
    function GetFragment: string;
    function GetHost: string;
    function GetPassword: string;
    function GetPath: string;
    function GetPort: Integer;
    function GetQuery: string;
    function GetScheme: string;
    function GetUsername: string;
    function GetParams: TURIParameters;
  public
    ///  <summary>Create a new URI instance.</summary>
    ///  <param name="AURIStr">The URI as text.</param>
    ///  <param name="APermitEmpty">Controls whether <c>AURIStr</c> may be an
    ///  empty string.</param>
    ///  <exception><c>EURI</c> raised if <c>AURIStr</c> is empty and
    ///  <c>APermitEmpty</c> is <c>False</c> or if a non-empty <c>AURIStr</c> is
    ///  not a valid URI.</exception>
    ///  <remarks>A URI with form <c>file:/path/to/file</c> is not accepted:
    ///  use <c>file:///path/to/file</c> instead.</remarks>
    constructor Create(const AURIStr: string; const APermitEmpty: Boolean);

    ///  <summary>Checks if the URI is empty.</summary>
    function IsEmpty: Boolean;

    ///  <summary>Converts the URI to a string.</summary>
    ///  <remarks>If the URI is empty then an empty string is returned.
    ///  </remarks>
    function ToString: string;

    ///  <summary>Scheme part of the URI.</summary>
    property Scheme: string read GetScheme;

    ///  <summary>Username part of the URI.</summary>
    property Username: string read GetUsername;

    ///  <summary>Password part of the URI.</summary>
    property Password: string read GetPassword;

    ///  <summary>Host part of the URI.</summary>
    property Host: string read GetHost;

    ///  <summary>Port part of the URI.</summary>
    ///  <remarks>If no port is specified in the URI then <c>-1</c> is returned,
    ///  unless the scheme is http or https, when default ports <c>80</c> or
    ///  <c>443</c> respectively are returned.</remarks>
    property Port: Integer read GetPort;

    ///  <summary>Path part of the URI.</summary>
    property Path: string read GetPath;

    ///  <summary>Query part of the URI.</summary>
    property Query: string read GetQuery;

    ///  <summary>Params part of the URI.</summary>
    property Params: TURIParameters read GetParams;

    ///  <summary>Fragment part of the URI.</summary>
    property Fragment: string read GetFragment;

    ///  <summary>Compares two <c>TURI</c> records and returns a 0, -ve or +ve
    ///  value depending on whether the <c>Left</c> is equal to, less than or
    ///  greater than <c>Right</c>, respectively.</summary>
    class function Compare(const Left, Right: TURI): Integer; static;

    ///  <summary>Checks the validity of a given URI. An empty URI is only
    ///  considered to be valid if <c>APermitEmpty</c> is <c>True</c>.</summary>
    class function IsValidURIString(const AURIStr: string;
      const APermitEmpty: Boolean): Boolean; static;

    // Operator overloads
    class operator Equal(const Left, Right: TURI): Boolean;
    class operator NotEqual(const Left, Right: TURI): Boolean;
    class operator Implicit(const AURI: System.Net.URLCLient.TURI): TURI;
  end;

  EURI = class(EExpected);

implementation

uses
  System.SysUtils,
  System.Types;

{ TURI }

class function TURI.Compare(const Left, Right: TURI): Integer;
begin
  // Deal with one or more empty URIs: empty is less than
  if Left.IsEmpty and Right.IsEmpty then
    Exit(EqualsValue);
  if Left.IsEmpty {and not Right.IsEmpty} then
    Exit(LessThanValue);
  if Right.IsEmpty {and not Left.IsEmpty} then
    Exit(GreaterThanValue);

  // If we get here then neither Left nor Right are empty, so we compare URI
  // component parts in order: scheme, username, password, host, port, path,
  // query string & fragment.

  // Schemes are case insensitive per RFC 3986 §3.1
  Result := CompareText(Left.fURI.Scheme, Right.fURI.Scheme, loInvariantLocale);
  if Result <> 0 then
    Exit;
  // User names are case sensitive
  Result := CompareStr(
    Left.fURI.Username, Right.fURI.Username, loInvariantLocale
  );
  if Result <> 0 then
    Exit;
  // Passwords are case sensitive
  Result := CompareStr(
    Left.fURI.Password, Right.fURI.Password, loInvariantLocale
  );
  if Result <> 0 then
    Exit;
  // Host is case insensitive per RFC 3986 §3.2.2
  Result := CompareText(Left.fURI.Host, Right.fURI.Host, loInvariantLocale);
  if Result <> 0 then
    Exit;
  // Compare Port number by substracting right from left
  Result := Left.fURI.Port - Right.fURI.Port;
  if Result <> 0 then
    Exit;
  // Path is case sensitive
  Result := CompareStr(Left.fURI.Path, Right.fURI.Path, loInvariantLocale);
  if Result <> 0 then
    Exit;
  // Query is case sensitive
  Result := CompareStr(Left.fURI.Query, Right.fURI.Query, loInvariantLocale);
  if Result <> 0 then
    Exit;
  // Fragment is case sensitive
  Result := CompareStr(
    Left.fURI.Fragment, Right.fURI.Fragment, loInvariantLocale
  );
end;

constructor TURI.Create(const AURIStr: string; const APermitEmpty: Boolean);
begin
  fIsEmpty := AURIStr.IsEmpty;
  if fIsEmpty and not APermitEmpty then
    raise EURI.Create('Empty URI');
  if not fIsEmpty then
  begin
    if not TryDeconstructURI(AURIStr, fURI) then
      raise EURI.CreateFmt('Invalid URI: %s', [AURIStr]);
  end;
end;

class operator TURI.Equal(const Left, Right: TURI): Boolean;
begin
  Result := Compare(Left, Right) = 0;
end;

function TURI.GetFragment: string;
begin
  if fIsEmpty then
    Result := string.Empty
  else
    Result := fURI.Fragment;
end;

function TURI.GetHost: string;
begin
  if fIsEmpty then
    Result := string.Empty
  else
    Result := fURI.Host;
end;

function TURI.GetParams: TURIParameters;
begin
  if fIsEmpty then
    SetLength(Result, 0)
  else
    Result := fURI.Params;
end;

function TURI.GetPassword: string;
begin
  if fIsEmpty then
    Result := string.Empty
  else
    Result := fURI.Password;
end;

function TURI.GetPath: string;
begin
  if fIsEmpty then
    Result := string.Empty
  else
    Result := fURI.Path;
end;

function TURI.GetPort: Integer;
begin
  if fIsEmpty then
    Result := 0
  else
    Result := fURI.Port;
end;

function TURI.GetQuery: string;
begin
  if fIsEmpty then
    Result := string.Empty
  else
    Result := fURI.Query;
end;

function TURI.GetScheme: string;
begin
  if fIsEmpty then
    Result := string.Empty
  else
    Result := fURI.Scheme;
end;

function TURI.GetUsername: string;
begin
  if fIsEmpty then
    Result := string.Empty
  else
    Result := fURI.Username;
end;

class operator TURI.Implicit(const AURI: System.Net.URLCLient.TURI): TURI;
begin
  Result.fURI := AURI;
  Result.fIsEmpty := False;
end;

function TURI.IsEmpty: Boolean;
begin
  Result := fIsEmpty;
end;

class function TURI.IsValidURIString(const AURIStr: string;
  const APermitEmpty: Boolean): Boolean;
begin
  if AURIStr.IsEmpty then
    Exit(APermitEmpty);
  var Dummy: System.Net.URLClient.TURI;
  Result := TryDeconstructURI(AURIStr, Dummy);
end;

class operator TURI.NotEqual(const Left, Right: TURI): Boolean;
begin
  Result := Compare(Left, Right) <> 0;
end;

function TURI.ToString: string;
begin
  if fIsEmpty then
    Result := string.Empty
  else
    Result := fURI.ToString;
end;

class function TURI.TryDeconstructURI(const AURIStr: string;
  out AURI: System.Net.URLClient.TURI): Boolean;
begin
  Result := True;
  try
    AURI := System.Net.URLClient.TURI.Create(AURIStr);
  except
    on E: ENetURIException do
      Result := False;
  end;
end;

end.
