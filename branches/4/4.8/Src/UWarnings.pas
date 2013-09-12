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
 * Classes and interfaces that encapsulate Delphi $WARN directives used to
 * switch off unwanted compiler warnings.
}


unit UWarnings;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  IntfCommon, UBaseObjects, USettings;


type
  ///  <summary>Encapsulates information needed to generate a $WARN compiler
  ///  directive to enable or disable a particular Delphi warning type.
  ///  </summary>
  ///  <remarks>$WARN compiler directives apply to Delphi 6 and later.</remarks>
  TWarning = record
  strict private
    var
      ///  <summary>Value of Symbol property.</summary>
      fSymbol: string;
      ///  <summary>Value of MinCompiler property.</summary>
      fMinCompiler: Single;
      ///  <summary>Value of State property.</summary>
      fState: Boolean;
    ///  <summary>Read accessor for MinCompiler property.</summary>
    function GetMinCompiler: Single;
    ///  <summary>Write accessor for MinCompiler property.</summary>
    procedure SetMinCompiler(const Value: Single);
    ///  <summary>Read accessor for Symbol property.</summary>
    function GetSymbol: string;
  public
    const
      ///  <summary>Version number of earliest compiler that supports $WARN
      ///  directive.</summary>
      ///  <remarks>This is Delphi 6.</remarks>
      MinSupportedCompiler = 14.0;
  public
    ///  <summary>Record constructor that supplies values for all properties.
    ///  </summary>
    ///  <param name="ASymbol">string [in] Warning symbol.</param>
    ///  <param name="AMinCompiler">Single [in] Version of earliest compiler
    ///  that supports ASymbol.</param>
    ///  <param name="AState">Boolean [in] Warning state: on or off.</param>
    constructor Create(const ASymbol: string; const AMinCompiler: Single;
      const AState: Boolean);
    ///  <summary>Checks if properties of this warning are valid.</summary>
    function IsValid: Boolean;
    ///  <summary>Symbol used to identify this warning in $WARN compiler
    ///  directives.</summary>
    property Symbol: string read GetSymbol;
    ///  <summary>Version number of earliest compiler that supports this
    ///  warning.</summary>
    property MinCompiler: Single read GetMinCompiler write SetMinCompiler;
    ///  <summary>State of warning switch.</summary>
    property State: Boolean read fState write fState;
  end;

type
  ///  <summary>Interface supported by classes that maintain a list of Delphi
  ///  warnings and generate compiler directives to switch the warnings on or
  ///  off.</summary>
  IWarnings = interface(IInterface)
    ['{EBE8C8BD-535D-4B4B-A6D4-1AFC02E1C5B7}']
    ///  <summary>Adds given warning to list.</summary>
    procedure Add(const AWarning: TWarning);
    ///  <summary>Clears list of warnings.</summary>
    procedure Clear;
    ///  <summary>Returns number of warnings in list.</summary>
    function Count: Integer;
    ///  <summary>Checks whether warnings list is empty.</summary>
    function IsEmpty: Boolean;
    ///  <summary>Checks if a warning with given symbol is present in warnings
    ///  list.</summary>
    function Contains(const ASymbol: string): Boolean;
    ///  <summary>Deletes given warning from list.</summary>
    ///  <remarks>First warning with a matching symbol is deleted, regardless of
    ///  value of other properties.</remarks>
    procedure Delete(const AWarning: TWarning);
    ///  <summary>Generates and returns source code for compiler directives that
    ///  enable or disable warnings in list, taking account of supporting
    ///  compilers.</summary>
    function Render: string;
    ///  <summary>Read accessor for Items[] property.</summary>
    function GetItem(const Idx: Integer): TWarning;
    ///  <summary>Indexed list of warnings.</summary>
    property Items[const Idx: Integer]: TWarning read GetItem; default;
    ///  <summary>Read accessor for Enabled property.</summary>
    function GetEnabled: Boolean;
    ///  <summary>Write accessor for Enabled property.</summary>
    procedure SetEnabled(const Value: Boolean);
    ///  <summary>Indicates whether compiler directives should be emitted for
    ///  listed warnings.</summary>
    property Enabled: Boolean read GetEnabled write SetEnabled;
    ///  <summary>Creates and returns an enumerator for the warnings list.
    ///  </summary>
    ///  <remarks>Caller is responsible for freeing the enumerator.</remarks>
    function GetEnumerator: TEnumerator<TWarning>;
  end;

type
  ///  <summary>Static class that can save and load an IWarnings object's data
  ///  to and from persistent storage.</summary>
  TWarningsPersist = class(TNoConstructObject)
  strict private
    ///  <summary>Constructs the name of a warning value in storage from given
    ///  index number and property name.</summary>
    class function WarningCompoundName(const Idx: Integer; const Prop: string):
      string;
  public
    ///  <summary>Loads data from given persistent storage section into given
    ///  warnings object.</summary>
    class procedure Load(Storage: ISettingsSection; Warnings: IWarnings);
    ///  <summary>Saves data from given warnings object to givenpersistent
    ///  storage section.</summary>
    class procedure Save(Storage: ISettingsSection; Warnings: IWarnings);
  end;

type
  ///  <summary>Class that encapsulates information about Delphi compiler
  ///  warnings and whether code can be generated to supress or enable them.
  ///  </summary>
  TWarnings = class(TInterfacedObject, IWarnings, IAssignable)
  strict private
    var
      ///  <summary>List of warning records.</summary>
      fItems: TList<TWarning>;
      ///  <summary>Value of Enabled property.</summary>
      fEnabled: Boolean;
  public
    ///  <summary>Constructs warnings object.</summary>
    constructor Create;
    ///  <summary>Destroys warnings object.</summary>
    destructor Destroy; override;
    ///  <summary>Creates a TWarnings instance containing default warnings.
    ///  </summary>
    class function Defaults: TWarnings;
    ///  <summary>Adds given warning to list.</summary>
    ///  <remarks>Method of IWarnings.</remarks>
    procedure Add(const AWarning: TWarning);
    ///  <summary>Clears list of warnings.</summary>
    ///  <remarks>Method of IWarnings.</remarks>
    procedure Clear;
    ///  <summary>Returns number of warnings in list.</summary>
    ///  <remarks>Method of IWarnings.</remarks>
    function Count: Integer;
    ///  <summary>Checks whether warnings list is empty.</summary>
    ///  <remarks>Method of IWarnings.</remarks>
    function IsEmpty: Boolean;
    ///  <summary>Checks if a warning with given symbol is present in warnings
    ///  list.</summary>
    ///  <remarks>Method of IWarnings.</remarks>
    function Contains(const ASymbol: string): Boolean;
    ///  <summary>Deletes given warning from list.</summary>
    ///  <remarks>
    ///  <para>First warning with a matching symbol is deleted, regardless of
    ///  value of other properties.</para>
    ///  <para>Method of IWarnings.</para>
    ///  </remarks>
    procedure Delete(const AWarning: TWarning);
    ///  <summary>Generates and returns source code for compiler directives that
    ///  enable or disable warnings in list, taking account of supporting
    ///  compilers.</summary>
    ///  <remarks>Method of IWarnings.</remarks>
    function Render: string;
    ///  <summary>Read accessor for Items[] property.</summary>
    ///  <remarks>Method of IWarnings.</remarks>
    function GetItem(const Idx: Integer): TWarning;
    ///  <summary>Indexed list of warnings.</summary>
    ///  <remarks>Property of IWarnings.</remarks>
    property Items[const Idx: Integer]: TWarning read GetItem; default;
    ///  <summary>Read accessor for Enabled property.</summary>
    ///  <remarks>Method of IWarnings.</remarks>
    function GetEnabled: Boolean;
    ///  <summary>Write accessor for Enabled property.</summary>
    ///  <remarks>Method of IWarnings.</remarks>
    procedure SetEnabled(const Value: Boolean);
    ///  <summary>Indicates whether compiler directives should be emitted for
    ///  listed warnings.</summary>
    ///  <remarks>Property of IWarnings.</remarks>
    property Enabled: Boolean read GetEnabled write SetEnabled;
    ///  <summary>Creates and returns an enumerator for the warnings list.
    ///  </summary>
    ///  <remarks>
    ///  <para>Caller is responsible for freeing the enumerator.</para>
    ///  <para>Method of IWarnings.</para>
    ///  </remarks>
    function GetEnumerator: TEnumerator<TWarning>;
    ///  <summary>Assigns properties of another IWarnings instance to this
    ///  object.</summary>
    ///  <remarks>Method of IAssignable.</remarks>
    procedure Assign(const Src: IInterface);
  end;


implementation


uses
  // Delphi
  SysUtils, Generics.Defaults, Math,
  // Project
  UConsts, UExceptions, UStrUtils;


{ TWarning }

constructor TWarning.Create(const ASymbol: string; const AMinCompiler: Single;
  const AState: Boolean);
begin
  Assert(ASymbol <> '', 'TWarning.Create: ASymbol is empty string');
  fSymbol := ASymbol;
  MinCompiler := AMinCompiler;
  fState := AState;
end;

function TWarning.GetMinCompiler: Single;
begin
  Assert(fMinCompiler >= MinSupportedCompiler,
    'TWarning.GetMinCompiler: fMinCompiler too small');
  Result := fMinCompiler;
end;

function TWarning.GetSymbol: string;
begin
  Assert(fSymbol <> '', 'TWarning.GetSymbol: fSymbol is empty string');
  Result := fSymbol;
end;

function TWarning.IsValid: Boolean;
begin
  Result := (fMinCompiler >= MinSupportedCompiler) and (fSymbol <> '');
end;

procedure TWarning.SetMinCompiler(const Value: Single);
begin
  Assert(Value >= MinSupportedCompiler,
    'TWarning.SetMinCompiler: AValue too small');
  fMinCompiler := Value;
end;

{ TWarnings }

procedure TWarnings.Add(const AWarning: TWarning);
begin
  Assert(AWarning.IsValid, ClassName + '.Add: AWarning not valid');
  if fItems.Contains(AWarning) then
    raise EBug.CreateFmt(
      '%s.Add: AWarning %s already in list', [ClassName, AWarning.Symbol]
    );
  fItems.Add(AWarning);
end;

procedure TWarnings.Assign(const Src: IInterface);
var
  W: TWarning;  // references each in warning in Src.
begin
  Clear;
  for W in (Src as IWarnings) do
    Add(W);
  fEnabled := (Src as IWarnings).Enabled;
end;

procedure TWarnings.Clear;
begin
  fItems.Clear;
end;

function TWarnings.Contains(const ASymbol: string): Boolean;
begin
  Result := fItems.Contains(
    // use fake warning: we only use Symbol property in search
    TWarning.Create(ASymbol, TWarning.MinSupportedCompiler, False)
  );
end;

function TWarnings.Count: Integer;
begin
  Result := fItems.Count;
end;

constructor TWarnings.Create;
begin
  inherited Create;
  // use generic list that sorts on warning's symbol to store warnings
  fItems := TList<TWarning>.Create(
    TDelegatedComparer<TWarning>.Create(
      function(const Left, Right: TWarning): Integer
      begin
        Result := StrCompareText(Left.Symbol, Right.Symbol);
      end
    )
  );
end;

class function TWarnings.Defaults: TWarnings;
begin
  Result := Create;
  Result.Add(TWarning.Create('UNSAFE_TYPE', 15.0, False));
  Result.Add(TWarning.Create('UNSAFE_CAST', 15.0, False));
  Result.Add(TWarning.Create('UNSAFE_CODE', 15.0, False));
  Result.Add(TWarning.Create('SYMBOL_PLATFORM', 14.0, False));
  Result.Add(TWarning.Create('SYMBOL_DEPRECATED', 14.0, False));
  Result.Add(TWarning.Create('SYMBOL_LIBRARY', 14.0, False));
  Result.Add(TWarning.Create('IMPLICIT_STRING_CAST', 20.0, False));
  Result.Add(TWarning.Create('EXPLICIT_STRING_CAST', 20.0, False));
end;

procedure TWarnings.Delete(const AWarning: TWarning);
begin
  fItems.Remove(AWarning);
end;

destructor TWarnings.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TWarnings.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TWarnings.GetEnumerator: TEnumerator<TWarning>;
begin
  Result := fItems.GetEnumerator;
end;

function TWarnings.GetItem(const Idx: Integer): TWarning;
begin
  Result := fItems[Idx];
end;

function TWarnings.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TWarnings.Render: string;
var
  SB: TStringBuilder;           // used to construct source code string
  W: TWarning;                  // each warning in list
  SortedList: TList<TWarning>;  // list of warnings sorted by compiler version
  CurrentVer: Single;           // compiler version currently being processed
  InsideVer: Boolean;           // true if rendering warnings for a compiler ver
const
  // values written to compiler directive, depending on warning state
  StateStrings: array[Boolean] of string = ('OFF', 'ON');
begin
  if not Enabled or IsEmpty then
    Exit('');

  // Create a list of warnings sorted by minimum compiler: we do this so we can
  // group related warnings together in one conditional statement for each
  // minimum compiler version
  SortedList := TList<TWarning>.Create(
    TDelegatedComparer<TWarning>.Create(
      // sort TWarning records by MinCompiler field
      function(const Left, Right: TWarning): Integer
      begin
        Result := Math.CompareValue(Left.MinCompiler, Right.MinCompiler);
      end
    )
  );
  try
    SortedList.AddRange(fItems);
    SortedList.Sort;

    // Generate the source code
    CurrentVer := 0.0;
    InsideVer := False;

    SB := TStringBuilder.Create;
    try
      SB.AppendLine('{$IFNDEF FPC}');
      SB.AppendLine('  {$IFDEF CONDITIONALEXPRESSIONS}');
      for W in SortedList do
      begin
        if not Math.SameValue(W.MinCompiler, CurrentVer) then
        begin
          // required compiler version has changed
          if InsideVer then
          begin
            // we were writing warnings for previous version: close statement
            SB.AppendLine('    {$IFEND}');
            InsideVer := False;
          end;
          // create new condition for new version
          SB.AppendFormat(
            '    {$IF CompilerVersion >= %.2f}' + EOL, [W.MinCompiler]
          );
          InsideVer := True;
          CurrentVer := W.MinCompiler;
        end;
        // write directive to turn warning off
        SB.AppendFormat(
          '      {$WARN %0:s %1:s}' + EOL, [W.Symbol, StateStrings[W.State]]
        );
      end;
      // close off any open conditional statement
      if InsideVer then
        SB.AppendLine('    {$IFEND}');
      // close bounding $IFDEFs
      SB.AppendLine('  {$ENDIF}');
      SB.AppendLine('{$ENDIF}');
      Result := SB.ToString;
    finally
      SB.Free;
    end;
  finally
    SortedList.Free;
  end;
end;

procedure TWarnings.SetEnabled(const Value: Boolean);
begin
  fEnabled := Value;
end;

{ TWarningsPersist }

const
  // Names of values stored in persistent storage
  cWarningsEnabledName = 'EmitWarnDirs';
  cWarningCountName = 'WarningCount';
  cWarningCompoundName = 'Warning%d.%s';
  // Names of properties used in compound value names
  cWarningSymbolProp = 'Symbol';
  cWarningSupportProp = 'MinCompiler';
  cWarningStateProp = 'State';

class procedure TWarningsPersist.Load(Storage: ISettingsSection;
  Warnings: IWarnings);
var
  Idx: Integer;         // loops thru all warnings in storage
  CompilerVer: Double;  // min compiler version for a warning read from storage
  Symbol: string;       // symbol of a warning read from storage
  State: Boolean;       // state of a warning read from storage
begin
  Warnings.Clear;
  Warnings.Enabled := Storage.GetBoolean(cWarningsEnabledName, False);
  for Idx := 0 to Pred(Storage.GetInteger(cWarningCountName, 0)) do
  begin
    Symbol := Storage.GetString(WarningCompoundName(Idx, cWarningSymbolProp));
    if (Symbol = '') or Warnings.Contains(Symbol) then
      Continue;
    CompilerVer := Storage.GetFloat(
      WarningCompoundName(Idx, cWarningSupportProp),
      TWarning.MinSupportedCompiler
    );
    if CompilerVer < TWarning.MinSupportedCompiler then
      CompilerVer := TWarning.MinSupportedCompiler;
    State := Storage.GetBoolean(
      WarningCompoundName(Idx, cWarningStateProp), False
    );
    Warnings.Add(TWarning.Create(Symbol, CompilerVer, State));
  end;
end;

class procedure TWarningsPersist.Save(Storage: ISettingsSection;
  Warnings: IWarnings);
var
  Idx: Integer; // loops through all warnings
begin
  Storage.SetBoolean(cWarningsEnabledName, Warnings.Enabled);
  Storage.SetInteger(cWarningCountName, Warnings.Count);
  for Idx := 0 to Pred(Warnings.Count) do
  begin
    Storage.SetString(
      WarningCompoundName(Idx, cWarningSymbolProp), Warnings[Idx].Symbol
    );
    Storage.SetFloat(
      WarningCompoundName(Idx, cWarningSupportProp), Warnings[Idx].MinCompiler
    );
    Storage.SetBoolean(
      WarningCompoundName(Idx, cWarningStateProp), Warnings[Idx].State
    );
  end;
  Storage.Save;
end;

class function TWarningsPersist.WarningCompoundName(const Idx: Integer;
  const Prop: string): string;
begin
  Result := Format(cWarningCompoundName, [Idx, Prop]);
end;

end.

