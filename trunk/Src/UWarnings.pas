{
 * UWarnings.pas
 *
 * Classes and interfaces that encapsulate Delphi $WARN directives used to
 * switch off unwanted compiler warnings.
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
 * The Original Code is UWarnings.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UWarnings;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  IntfCommon, UBaseObjects, USettings;


type

  {
  TWarning:
    Record that encapsulates information needed to generate a $WARN compiler
    directive.
  }
  TWarning = record
  strict private
    var
      fSymbol: string;        // Value of Symbol property
      fMinCompiler: Single;   // Value of MinCompiler property
      fState: Boolean;        // Value of State property
    function GetMinCompiler: Single;
      {Read accessor for MinCompiler property.
        @return Version number of earliest compiler to support Symbol.
      }
    procedure SetMinCompiler(const Value: Single);
      {Write accessor for MinCompiler property.
        @param Value [in] New compiler version to be assigned to property.
      }
    function GetSymbol: string;
      {Read accessor for Symbol property.
        @return Symbol used in $WARN directive.
      }
  public
    const MinSupportedCompiler = 14.0;
      {Version number of earliest compiler that supports any $WARN directive.
      (Delphi 6)}
  public
    constructor Create(const ASymbol: string; const AMinCompiler: Single;
      const AState: Boolean);
      {Constructor that provides values for both Symbol and MinCompiler
      properties.
        @param ASymbol [in] Warning symbol.
        @param AMinCompiler [in] Earliest compiler that supports ASymbol.
        @param AState [in] Warning state: on or off.
      }
    function IsValid: Boolean;
      {Checks if properties of this warning are valid.
        @return True if warning is valid, False if not.
      }
    property Symbol: string read GetSymbol;
      {Symbol used to specify this warning in $WARN compiler directives}
    property MinCompiler: Single read GetMinCompiler write SetMinCompiler;
      {Version number of earliest compiler that supports this warning}
    ///  <summary>State of warning switch: defaults to False (off).</summary>
    property State: Boolean read fState write fState;
  end;

  {
  IWarnings:
    Interface to class that encapsulates information about warnings and whether
    code can be generated to supress or enable them.
  }
  IWarnings = interface(IInterface)
    ['{EBE8C8BD-535D-4B4B-A6D4-1AFC02E1C5B7}']
    procedure Add(const AWarning: TWarning);
      {Adds a warning to list.
        @param AWarning [in] Warning to be added. Must be unique.
      }
    procedure Clear;
      {Clears list of warnings.
      }
    function Count: Integer;
      {Gets number of warnings in list.
        @return Number of warnings.
      }
    function IsEmpty: Boolean;
      {Checks whether warnings list is empty.
        @return True if warnings list empty, False if not.
      }
    function Contains(const ASymbol: string): Boolean;
      {Checks if a warning with a specified symbol is present in the warnings
      list.
        @param ASymbol [in] Symbol to be checked.
        @return True if warning with symbol is in list, False if not.
      }
    ///  <summary>Deletes given warning from list.</summary>
    ///  <remarks>First warning with a matching symbol is deleted, regardless of
    ///  value of other properties.</remarks>
    procedure Delete(const AWarning: TWarning);
    function Render: string;
      {Generates source code for compiler directives that switch off all
      warnings in list for supporting compilers.
        @return String containing required source code.
      }
    function GetItem(const Idx: Integer): TWarning;
      {Read accessor for Items[] property. Gets warning at specified index in
      warnings list.
        @param Idx [in] Index of required warning.
        @return Warning at specified index.
      }
    property Items[const Idx: Integer]: TWarning read GetItem; default;
      {Array of warnings}
    function GetEnabled: Boolean;
      {Read accessor for Enabled property.
        @return Value of Enabled property.
      }
    procedure SetEnabled(const Value: Boolean);
      {Write accessor for Enabled property. Sets property to specified value.
        @param Value [in] New property value.
      }
    property Enabled: Boolean read GetEnabled write SetEnabled;
      {Property that indicates whether code should be emitted to control listed
      warnings}
    function GetEnumerator: TEnumerator<TWarning>;
      {Creates an enumerator for all the warnings in the warnings list.
        @return Instance of enumerator.
      }
  end;

  {
  TWarningsPersist:
    Static class that can save and load an IWarnings object to and from
    persistent storage.
  }
  TWarningsPersist = class(TNoConstructObject)
  strict private
    class function WarningCompoundName(const Idx: Integer; const Prop: string):
      string;
      {Constructs the name of a warning value in storage. Name comprises an
      index number and a property name.
        @param Idx [in] Index component of name.
        @param Prop [in] Property component of name.
      }
  public
    class procedure Load(Storage: ISettingsSection; Warnings: IWarnings);
      {Loads data from persistent storage into a warnings object.
        @param Storage [in] Reference to storage containing needed data.
        @param Warnings [in] Warnings object to be updated with data from
          storage.
      }
    class procedure Save(Storage: ISettingsSection; Warnings: IWarnings);
      {Saves data from a warnings object to persistent storage.
        @param Storage [in] Reference to storage object to receive data.
        @param Warnings [in] Reference to warnings object to be persisted.
      }
  end;

  {
  TWarnings:
    Class that encapsulates information about warnings and whether code can be
    generated to supress or enable them. Implements IWarnings interface.
  }
  TWarnings = class(TInterfacedObject, IWarnings, IAssignable)
  strict private
    fItems: TList<TWarning>;    // List of warning records
    fEnabled: Boolean;        // Value of Enabled property
  public
    constructor Create;
      {Constructor. Sets up object.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    ///  <summary>Creates a TWarnings instance containing default warnings.
    ///  </summary>
    class function Defaults: TWarnings;
    { IWarnings methods }
    procedure Add(const AWarning: TWarning);
      {Adds a warning to list.
        @param AWarning [in] Warning to be added. Must be unique.
      }
    procedure Clear;
      {Clears list of warnings.
      }
    function Count: Integer;
      {Gets number of warnings in list.
        @return Number of warnings.
      }
    function IsEmpty: Boolean;
      {Checks whether warnings list is empty.
        @return True if warnings list empty, False if not.
      }
    function Contains(const ASymbol: string): Boolean;
      {Checks if a warning with a specified symbol is present in the warnings
      list.
        @param ASymbol [in] Symbol to be checked.
        @return True if warning with symbol is in list, False if not.
      }
    procedure Delete(const AWarning: TWarning);
      {Removes a warning from the list based on its symbol.
        @param AWarning [in] Warning to be removed.
      }
    function Render: string;
      {Generates source code for compiler directives that switch off all
      warnings in list for supporting compilers.
        @return String containing required source code.
      }
    function GetItem(const Idx: Integer): TWarning;
      {Read accessor for Items[] property. Gets warning at specified index in
      warnings list.
        @param Idx [in] Index of required warning.
        @return Warning at specified index.
      }
    property Items[const Idx: Integer]: TWarning read GetItem; default;
      {Array of warnings}
    function GetEnabled: Boolean;
      {Read accessor for Enabled property.
        @return Value of Enabled property.
      }
    procedure SetEnabled(const Value: Boolean);
      {Write accessor for Enabled property. Sets property to specified value.
        @param Value [in] New property value.
      }
    property Enabled: Boolean read GetEnabled write SetEnabled;
      {Property that indicates whether code should be emitted to switch off
      listed warnings}
    function GetEnumerator: TEnumerator<TWarning>;
      {Creates an enumerator for all the warnings in the warnings list.
        @return Instance of enumerator.
      }
    { IAssignable methods }
    procedure Assign(const Src: IInterface);
      {Assigns properties of another IWarnings instance to this object.
        @param Src [in] Reference to object to be assigned. Must support
        IWarnings.
      }
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
  {Constructor that provides values for both Symbol and MinCompiler properties.
    @param ASymbol [in] Warning symbol.
    @param AMinCompiler [in] Earliest compiler that supports ASymbol.
  }
begin
  Assert(ASymbol <> '', 'TWarning.Create: ASymbol is empty string');
  fSymbol := ASymbol;
  MinCompiler := AMinCompiler;
  fState := AState;
end;

function TWarning.GetMinCompiler: Single;
  {Read accessor for MinCompiler property.
    @return Version number of earliest compiler to support Symbol.
  }
begin
  Assert(fMinCompiler >= MinSupportedCompiler,
    'TWarning.GetMinCompiler: fMinCompiler too small');
  Result := fMinCompiler;
end;

function TWarning.GetSymbol: string;
  {Read accessor for Symbol property.
    @return Symbol used in $WARN directive.
  }
begin
  Assert(fSymbol <> '', 'TWarning.GetSymbol: fSymbol is empty string');
  Result := fSymbol;
end;

function TWarning.IsValid: Boolean;
  {Checks if properties of this warning are valid.
    @return True if warning is valid, False if not.
  }
begin
  Result := (fMinCompiler >= MinSupportedCompiler) and (fSymbol <> '');
end;

procedure TWarning.SetMinCompiler(const Value: Single);
  {Write accessor for MinCompiler property.
    @param Value [in] New compiler version to be assigned to property.
  }
begin
  Assert(Value >= MinSupportedCompiler,
    'TWarning.SetMinCompiler: AValue too small');
  fMinCompiler := Value;
end;

{ TWarnings }

procedure TWarnings.Add(const AWarning: TWarning);
  {Adds a warning to list.
    @param AWarning [in] Warning to be added.
    @except EBug raised in warning with same symbol is already in list.
  }
begin
  Assert(AWarning.IsValid, ClassName + '.Add: AWarning not valid');
  if fItems.Contains(AWarning) then
    raise EBug.CreateFmt(
      '%s.Add: AWarning %s already in list', [ClassName, AWarning.Symbol]
    );
  fItems.Add(AWarning);
end;

procedure TWarnings.Assign(const Src: IInterface);
  {Assigns properties of another IWarnings instance to this object.
    @param Src [in] Reference to object to be assigned. Must support IWarnings.
  }
var
  W: TWarning;  // references each in warning in Src.
begin
  Clear;
  for W in (Src as IWarnings) do
    Add(W);
  fEnabled := (Src as IWarnings).Enabled;
end;

procedure TWarnings.Clear;
  {Clears list of warnings.
  }
begin
  fItems.Clear;
end;

function TWarnings.Contains(const ASymbol: string): Boolean;
  {Checks if a warning with a specified symbol is present in the warnings list.
    @param ASymbol [in] Symbol to be checked.
    @return True if warning with symbol is in list, False if not.
  }
begin
  Result := fItems.Contains(
    // use fake warning: we only use Symbol property in search
    TWarning.Create(ASymbol, TWarning.MinSupportedCompiler, False)
  );
end;

function TWarnings.Count: Integer;
  {Gets number of warnings in list.
    @return Number of warnings.
  }
begin
  Result := fItems.Count;
end;

constructor TWarnings.Create;
  {Constructor. Sets up object.
  }
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
  {Removes a warning from the list based on its symbol.
    @param AWarning [in] Warning to be removed.
  }
begin
  fItems.Remove(AWarning);
end;

destructor TWarnings.Destroy;
  {Destructor. Tears down object.
  }
begin
  fItems.Free;
  inherited;
end;

function TWarnings.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TWarnings.GetEnumerator: TEnumerator<TWarning>;
  {Creates an enumerator for all the warnings in the warnings list.
    @return Instance of enumerator.
  }
begin
  Result := fItems.GetEnumerator;
end;

function TWarnings.GetItem(const Idx: Integer): TWarning;
  {Read accessor for Items[] property. Gets warning at specified index in
  warnings list.
    @param Idx [in] Index of required warning.
    @return Warning at specified index.
  }
begin
  Result := fItems[Idx];
end;

function TWarnings.IsEmpty: Boolean;
  {Checks whether warnings list is empty.
    @return True if warnings list empty, False if not.
  }
begin
  Result := Count = 0;
end;

function TWarnings.Render: string;
  {Generates source code for compiler directives that switch off all warnings in
  list for supporting compilers.
    @return String containing required source code.
  }
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
    for W in fItems do
      SortedList.Add(W);
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
  cWarningsEnabledName = 'SwitchOffWarnings'; //! this name is historical
  cWarningCountName = 'WarningCount';
  cWarningCompoundName = 'Warning%d.%s';
  // Names of properties used in compound value names
  cWarningSymbolProp = 'Symbol';
  cWarningSupportProp = 'MinCompiler';
  cWarningStateProp = 'State';

class procedure TWarningsPersist.Load(Storage: ISettingsSection;
  Warnings: IWarnings);
  {Loads data from persistent storage into a warnings object.
    @param Storage [in] Reference to storage containing needed data.
    @param Warnings [in] Warnings object to be updated with data from storage.
  }
var
  Idx: Integer;         // loops thru all warnings in storage
  CompilerVer: Double;  // min compiler version for a warning read from storage
  Symbol: string;       // symbol of a warning read from storage
  State: Boolean;       // state of a warning read from storage
begin
  Warnings.Clear;
  Warnings.Enabled := Boolean(
    StrToIntDef(Storage.ItemValues[cWarningsEnabledName], Ord(False))
  );
  for Idx := 0 to Pred(StrToIntDef(Storage.ItemValues[cWarningCountName], 0)) do
  begin
    Symbol := Storage.ItemValues[WarningCompoundName(Idx, cWarningSymbolProp)];
    if (Symbol = '') or Warnings.Contains(Symbol) then
      Continue;
    CompilerVer := StrToFloatDef(
      Storage.ItemValues[WarningCompoundName(Idx, cWarningSupportProp)],
      TWarning.MinSupportedCompiler
    );
    if CompilerVer < TWarning.MinSupportedCompiler then
      CompilerVer := TWarning.MinSupportedCompiler;
    State := Boolean(
      StrToIntDef(
        Storage.ItemValues[WarningCompoundName(Idx, cWarningStateProp)],
        Ord(False)
      )
    );
    Warnings.Add(TWarning.Create(Symbol, CompilerVer, State));
  end;
end;

class procedure TWarningsPersist.Save(Storage: ISettingsSection;
  Warnings: IWarnings);
  {Saves data from a warnings object to persistent storage.
    @param Storage [in] Reference to storage object to receive data.
    @param Warnings [in] Reference to warnings object to be persisted.
  }
var
  Idx: Integer; // loops through all warnings
begin
  Storage.ItemValues[cWarningsEnabledName] := IntToStr(Ord(Warnings.Enabled));
  Storage.ItemValues[cWarningCountName] := IntToStr(Warnings.Count);
  for Idx := 0 to Pred(Warnings.Count) do
  begin
    Storage.ItemValues[WarningCompoundName(Idx, cWarningSymbolProp)] :=
      Warnings[Idx].Symbol;
    Storage.ItemValues[WarningCompoundName(Idx, cWarningSupportProp)] :=
      Format('%.2f', [Warnings[Idx].MinCompiler]);
    Storage.ItemValues[WarningCompoundName(Idx, cWarningStateProp)] :=
      IntToStr(Ord(Warnings[Idx].State));
  end;
  Storage.Save;
end;

class function TWarningsPersist.WarningCompoundName(const Idx: Integer;
  const Prop: string): string;
  {Constructs the name of a warning value in storage. Name comprises an index
  number and a property name.
    @param Idx [in] Index component of name.
    @param Prop [in] Property component of name.
  }
begin
  Result := Format(cWarningCompoundName, [Idx, Prop]);
end;

end.

