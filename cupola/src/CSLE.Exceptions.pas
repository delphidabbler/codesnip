{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Definitions of CodeSnip LE's custom exception classes.

  NOTE:
    EBase is derived from EAssignable from the UExceptions unit in the CodeSnip
    master branch.
}

unit CSLE.Exceptions;

interface

uses
  System.SysUtils;

type
  ///  <summary>Base exception class for all CodeSnip LE's native exceptions.
  ///  </summary>
  ///  <remarks>All descendants inherited the ability to assign one exception to
  ///  another.</remarks>
  EBase = class(Exception)
  public
    ///  <summary>Constructs the exception object that is shallow copy of
    ///  exception <c>E</c>.</summary>
    ///  <remarks>Note that any inner exception of <c>E</c> is not copied.
    ///  </remarks>
    constructor Create(const E: Exception); overload;
    ///  <summary>Sets this exception object's properties to a shallow copy of
    ///  exception <c>E</c>.</summary>
    ///  <remarks>
    ///  <para>Note that any inner exception of <c>E</c> is not copied.</para>
    ///  <para>Descendants should overload if any new properties are added to
    ///  those of the <c>Exception</c> class.</para>
    ///  </remarks>
    procedure Assign(const E: Exception); virtual;
  end;

  ///  <summary>Exceptions that represent expected errors and are handled
  ///  specially by the program.</summary>
  ///  <remarks>This class can either be used as-is or used as base class for
  ///  other expected exception types.</remarks>
  EExpected = class(EBase);

  ///  <summary>Exceptions that are not expected, i.e. may be considered as bugs
  ///  and not handled epxlicitly by the program.</summary>
  ///  <remarks>This class can either be used as-is or used as base class for
  ///  other unexpected exception types.</remarks>
  EUnexpected = class(EBase);

implementation

{ EBase }

procedure EBase.Assign(const E: Exception);
begin
  Self.Message := E.Message;  // only copy Message property
end;

constructor EBase.Create(const E: Exception);
begin
  inherited Create('');
  Assign(E);  // we call assign so that descendants can copy extra properties
end;

end.
