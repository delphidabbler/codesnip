{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Data types encapsulating information about any testing applied to a snippet.
}

unit CSLE.Snippets.TestInfo;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  CSLE.Exceptions,
  CSLE.Utils.URI;

type

  TTestInfoGeneral = (
    Unknown = 0,
    None = 1,
    Basic = 2,
    Advanced = 3
  );

  TTestInfoAdvanced = (
    UnitTests,
    DemoCode,
    OtherTests
  );

  TTestInfoAdvancedSet = set of TTestInfoAdvanced;

  TSnippetTestInfo = record
  strict private
    var
      fGeneral: TTestInfoGeneral;
      fAdvanced: TTestInfoAdvancedSet;
      fURL: string;

    class function Same(const Left, Right: TSnippetTestInfo): Boolean; static;
  public

    ///  <summary>Creates a new record instance.</summary>
    ///  <param name="AGeneral">[in] Basic test information. If <c>AGeneral</c>
    ///  = <c>TTestInfoGeneral.Advanced</c> then further information may be
    ///  supplied in the following parameters.</param>
    ///  <param name="AAdvanced">[in] Optional additional test information. This
    ///  parameter is a set of zero of more advanced tests that have been
    ///  carried out. Ignored and set to <c>[]</c> if <c>AGeneral</c>
    ///  &lt;&gt; <c>TTestInfoGeneral.Advanced</c>.</param>
    ///  <param name="AURL">[in] Optional URL that leads to source code of any
    ///  advanced tests. Ignored and set to an emptry string if <c>AGeneral</c>
    ///  &lt;&gt; <c>TTestInfoGeneral.Advanced</c> or <c>AAdvanced</c> =
    ///  <c>[]</c>.</param>
    constructor Create(const AGeneral: TTestInfoGeneral;
      const AAdvanced: TTestInfoAdvancedSet = [];
      const AURL: string = string.Empty);

    ///  <summary>Provides general information about testing applied to the
    ///  snippet.</summary>
    property General: TTestInfoGeneral read fGeneral;

    ///  <summary>Provides further information about any advanced testing.
    ///  </summary>
    ///  <remarks>Always returns <c>[]</c> if <c>General</c> &lt;&gt;
    ///  <c>TTestInfoGeneral.Advanced</c>.</remarks>
    property Advanced: TTestInfoAdvancedSet read fAdvanced;

    ///  <summary>A URL that links to source code of any advanced testing. Will
    ///  return the emptry string if there is no URL available.</summary>
    ///  <remarks>Always returns <c>''</c> if <c>General</c> &lt;&gt;
    ///  <c>TTestInfoGeneral.Advanced</c>.</remarks>
    property URL: string read fURL;

    ///  <summary>Checks if the this record's properties have their default
    ///  values.</summary>
    ///  <remarks>The default values are those set when the record is 1st
    ///  initialised.</remarks>
    function IsDefault: Boolean;

    // Default ctor: creates a default test information record.
    class operator Initialize(out Dest: TSnippetTestInfo);

    // Assignment operator
    class operator Assign(var Dest: TSnippetTestInfo;
      const [ref] Src: TSnippetTestInfo);

    // Equality / inequality operators
    class operator Equal(const Left, Right: TSnippetTestInfo): Boolean;
    class operator NotEqual(const Left, Right: TSnippetTestInfo): Boolean;
  end;

  ESnippetTestInfo = class(EExpected);

implementation

uses
  System.StrUtils;

{ TSnippetTestInfo }

class operator TSnippetTestInfo.Assign(var Dest: TSnippetTestInfo;
  const [ref] Src: TSnippetTestInfo);
begin
  Dest.fGeneral := Src.fGeneral;
  Dest.fAdvanced := Src.fAdvanced;
  Dest.fURL := Src.fURL;
end;

constructor TSnippetTestInfo.Create(const AGeneral: TTestInfoGeneral;
  const AAdvanced: TTestInfoAdvancedSet; const AURL: string);
begin
  fGeneral := AGeneral;
  if AGeneral = TTestInfoGeneral.Advanced then
  begin
    fAdvanced := AAdvanced;
    if fAdvanced <> [] then
      fURL := AURL
    else
      fURL := string.Empty;
  end
  else
  begin
    fAdvanced := [];
    fURL := string.Empty;
  end;
  if not TURI.IsValidURIString(fURL, True) then
    raise ESnippetTestInfo.CreateFmt('Invalid URL: %s', [fURL]);
end;

class operator TSnippetTestInfo.Equal(const Left,
  Right: TSnippetTestInfo): Boolean;
begin
  Result := Same(Left, Right);
end;

class operator TSnippetTestInfo.Initialize(out Dest: TSnippetTestInfo);
begin
  Dest.fGeneral := TTestInfoGeneral.Unknown;
  Dest.fAdvanced := [];
  Dest.fURL := string.Empty;
end;

function TSnippetTestInfo.IsDefault: Boolean;
begin
  var T: TSnippetTestInfo;
  Result := Self = T;
end;

class operator TSnippetTestInfo.NotEqual(const Left,
  Right: TSnippetTestInfo): Boolean;
begin
  Result := not Same(Left, Right);
end;

class function TSnippetTestInfo.Same(const Left,
  Right: TSnippetTestInfo): Boolean;
begin
  if Left.General <> Right.General then
    Exit(False);
  // Left.General = Right.General, so check fAdvanced
  if Left.fGeneral <> TTestInfoGeneral.Advanced then
    // We ignore other fields unless advanced testing
    Exit(True);
  if Left.Advanced <> Right.Advanced then
    Exit(False);
  // Left.Advanced = Right.Advanced
  if Left.fAdvanced = [] then
    // We ignore .URL property when .Advanced is []
    Exit(True);
  // Only if Left & Right's .General field is Advanced AND if Left and Right's
  // .Advanced property is not empty set do we compare URLs
  var LeftURI := TURI.Create(Left.fURL, True);
  var RightURI := TURI.Create(Right.fURL, True);
  Result := LeftURI = RightURI;
end;

end.
