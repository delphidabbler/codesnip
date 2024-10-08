{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Data types that encapsulate the various snippet formats.

  NOTE:
    This unit is based on code taken from the CodeSnip Pavilion branch's
    CS.Database.SnippetKinds and CS.Database.Types units, except that the
    Pavilion code used the names SnippetKind where this code uses SnippetFormat.
    See https://tinyurl.com/22ectu3j and https://tinyurl.com/pju27zby
}

unit CSLE.Snippets.Format;

// TODO: Consider making list class a singleton accessible via class references
{ TODO: Consider renaming as follows:
        TSnippetFormat => TSnippetFormatInfo
        TSnippetFormatList => TSnippetFormatInfos
        ISnippetFormatList => ISnippetFormatInfos}

{$SCOPEDENUMS ON}

interface

uses
  System.Generics.Collections;

type
  ///  <summary>Enumeration of IDs of supported snippet formats.</summary>
  TSnippetFormatID = (
    Freeform = 0,       // free-form code: not in any other supported format
    PascalRoutine = 1,  // Pascal procedure or function in standard format
    PascalConst = 2,    // Pascal constant definition in standard format
    PascalType = 3,     // Pascal type definition in standard format
    PascalUnit = 4,     // Complete Pascal source code unit
    PascalClass = 5     // Delphi class or advanced record with methods
  );

  ///  <summary>Set of supported snippet formats IDs.</summary>
  TSnippetFormatIDs = set of TSnippetFormatID;

  ///  <summary>Encapsulates information about a snippet format.</summary>
  TSnippetFormat = record
  strict private
    var
      ///  <summary>Value of <c>ID</c> property.</summary>
      fID: TSnippetFormatID;
      ///  <summary>Value of <c>DisplayName</c> property.</summary>
      fDisplayName: string;
      ///  <summary>Value of <c>ValidDependIDs</c> property.</summary>
      fValidDependIDs: TSnippetFormatIDs;
  public
    ///  <summary>Initialises record with required property values.</summary>
    constructor Create(AID: TSnippetFormatID; const ADisplayName: string;
      const AValidDependIDs: TSnippetFormatIDs);
    ///  <summary>ID of snippet format.</summary>
    property ID: TSnippetFormatID read fID;
    
    ///  <summary>Display name (description) of snippet format.</summary>
    property DisplayName: string read fDisplayName;
    ///  <summary>Set of IDs of the snippet formats that this snippet format can
    ///  depend upon.</summary>
    property ValidDependIDs: TSnippetFormatIDs read fValidDependIDs;

    // Operators
    class operator Equal(const Left, Right: TSnippetFormat): Boolean;
    class operator NotEqual(const Left, Right: TSnippetFormat): Boolean;
  end;

  ISnippetFormatList = interface(IInterface)
    ['{E4CAEE92-1B1D-46D3-8EC8-127BCFAFE79C}']
    function GetEnumerator: TEnumerator<TSnippetFormat>;
    function GetItem(const FormatID: TSnippetFormatID): TSnippetFormat;
    function GetAllIDs: TSnippetFormatIDs;
    function First: TSnippetFormat; 
    function Last: TSnippetFormat;
    property Items[const FormatID: TSnippetFormatID]: TSnippetFormat
      read GetItem; default;
    property AllIDs: TSnippetFormatIDs read GetAllIDs;
  end;

  ///  <summary>Implements a read only list of all <c>TSnippetFormat</c>
  ///  records.</summary>
  TSnippetFormatList = class(TInterfacedObject, ISnippetFormatList)
  strict private
    type
      ///  <summary>Snippet format list's enumerator.</summary>
      TEnumerator = class(TEnumerator<TSnippetFormat>)
      strict private
        var
          fAtStart: Boolean;
          fCurrent: TSnippetFormatID;
          fMap: TSnippetFormatList;
      strict protected
        function DoGetCurrent: TSnippetFormat; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMap: TSnippetFormatList);
      end;
  strict private
    var
      fMap: array[TSnippetFormatID] of TSnippetFormat;
  public
    constructor Create;
    ///  <summary>Returns the list's enumerator.</summary>
    function GetEnumerator: TEnumerator<TSnippetFormat>;
    ///  <summary>Gets format information about a given snippet format ID.
    ///  </summary>
    function GetItem(const FormatID: TSnippetFormatID): TSnippetFormat;
    ///  <summary>Returns a set of all supported snippet format IDs.</summary>
    function GetAllIDs: TSnippetFormatIDs;
    ///  <summary>Returns the first snippet format in the list.</summary>
    ///  <remarks>Used internally by the list enumerator.</remarks>
    function First: TSnippetFormat;
    ///  <summary>Returns the last snippet format in the list.</summary>
    ///  <remarks>Used internally by the list enumerator.</remarks>
    function Last: TSnippetFormat;
  end;

implementation

{ TSnippetFormat }

constructor TSnippetFormat.Create(AID: TSnippetFormatID;
  const ADisplayName: string; const AValidDependIDs: TSnippetFormatIDs);
begin
  fID := AID;
  fDisplayName := ADisplayName;
  fValidDependIDs := AValidDependIDs;
end;

class operator TSnippetFormat.Equal(const Left, Right: TSnippetFormat): Boolean;
begin
  Result := Left.fID = Right.fID;
end;

class operator TSnippetFormat.NotEqual(const Left, Right: TSnippetFormat):
  Boolean;
begin
  Result := Left.fID <> Right.fID;
end;

{ TSnippetFormatList }

constructor TSnippetFormatList.Create;
resourcestring
  // Snippet Format descriptions
  sFreeForm         = 'Freeform';
  sPascalRoutine    = 'Pascal Routine';
  sPascalConst      = 'Pascal Constant';
  sPascalType       = 'Pascal Simple Type';
  sPascalUnit       = 'Pascal Unit';
  sPascalClass      = 'Pascal Class / Advanced Record';
const
  // Map of snippet Formats onto their descriptions
  Descriptions: array[TSnippetFormatID] of string = (
    sFreeform, sPascalRoutine, sPascalConst, sPascalType, sPascalUnit,
    sPascalClass
  );
  DependIDs: array[TSnippetFormatID] of TSnippetFormatIDs = (

    // TSnippetFormatID.Freeform
    [
      TSnippetFormatID.PascalRoutine, TSnippetFormatID.PascalConst,
      TSnippetFormatID.PascalType, TSnippetFormatID.Freeform
    ],

    // TSnippetFormatID.PascalRoutine
    [
      TSnippetFormatID.PascalRoutine, TSnippetFormatID.PascalConst,
      TSnippetFormatID.PascalType, TSnippetFormatID.PascalClass
    ],

    // TSnippetFormatID.PascalConst
    [TSnippetFormatID.PascalType, TSnippetFormatID.PascalType],

    // TSnippetFormatID.PascalType
    [
      TSnippetFormatID.PascalConst, TSnippetFormatID.PascalType,
      TSnippetFormatID.PascalClass
    ],

    // TSnippetFormatID.PascalUnit
    [],

    // TSnippetFormatID.PascalClass
    [
      TSnippetFormatID.PascalRoutine, TSnippetFormatID.PascalConst,
      TSnippetFormatID.PascalType, TSnippetFormatID.PascalClass
    ]
  );
begin
  inherited Create;
  for var FormatID := Low(TSnippetFormatID) to High(TSnippetFormatID) do
    fMap[FormatID] := TSnippetFormat.Create(
      FormatID, Descriptions[FormatID], DependIDs[FormatID]
    );
end;

function TSnippetFormatList.First: TSnippetFormat;
begin
  Result := fMap[Low(TSnippetFormatID)];
end;

function TSnippetFormatList.GetAllIDs: TSnippetFormatIDs;
begin
  Result := [];
  for var Format in fMap do
    Include(Result, Format.ID);
end;

function TSnippetFormatList.GetEnumerator: TEnumerator<TSnippetFormat>;
begin
  Result := TEnumerator.Create(Self);
end;

function TSnippetFormatList.GetItem(
  const FormatID: TSnippetFormatID): TSnippetFormat;
begin
  Result := fMap[FormatID];
end;

function TSnippetFormatList.Last: TSnippetFormat;
begin
  Result := fMap[High(TSnippetFormatID)];
end;

{ TSnippetFormatList.TEnumerator }

constructor TSnippetFormatList.TEnumerator.Create(
  const AMap: TSnippetFormatList);
begin
  fMap := AMap;
  fAtStart := True;
  fCurrent := AMap.First.ID;
end;

function TSnippetFormatList.TEnumerator.DoGetCurrent: TSnippetFormat;
begin
  Result := fMap.GetItem(fCurrent);
end;

function TSnippetFormatList.TEnumerator.DoMoveNext: Boolean;
begin
  if fCurrent = fMap.Last.ID then
    Exit(False);
  if fAtStart then
  begin
    fCurrent := fMap.First.ID;
    fAtStart := False;
  end
  else
    Inc(fCurrent);
  Result := True;
end;

end.
