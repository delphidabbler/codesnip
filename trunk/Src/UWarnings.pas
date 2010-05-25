{
 * UWarnings.pas
 *
 * Classes an interfaces that encapsulate Delphi $WARN directives used to switch
 * off unwanted compiler warnings.
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
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
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
  Generics.Collections,
  IntfCommon, UBaseObjects, USettings;

type

  TWarning = record
  strict private
    var fSymbol: string;        // Value of Symbol property
    var fMinCompiler: Single;   // Value of MinCompiler property
    function GetMinCompiler: Single;
    procedure SetMinCompiler(const Value: Single);
    function GetSymbol: string;
  public
    const MinSupportedCompiler = 14.0;  // Delphi 6
    property Symbol: string read GetSymbol;
    property MinCompiler: Single read GetMinCompiler write SetMinCompiler;
    function IsValid: Boolean;
    constructor Create(const ASymbol: string; const AMinCompiler: Single);
      overload;
    constructor Create(const ASymbol: string); overload;
  end;

  IWarningsEnum = interface(IInterface)
    ['{0662984F-8FEC-45FF-9855-9F1E2D6FAC59}']
    function GetCurrent: TWarning;
    property Current: TWarning read GetCurrent;
    function MoveNext: Boolean;
  end;

  IWarnings = interface(IInterface)
    ['{EBE8C8BD-535D-4B4B-A6D4-1AFC02E1C5B7}']
    procedure Clear;
    function Count: Integer;
    function Contains(const ASymbol: string): Boolean;
    procedure Add(const AWarning: TWarning);
    function GetItem(const Idx: Integer): TWarning;
    property Items[const Idx: Integer]: TWarning read GetItem; default;
    function GetSwitchOff: Boolean;
    procedure SetSwitchOff(const Value: Boolean);
    property SwitchOff: Boolean read GetSwitchOff write SetSwitchOff;
    function GetEnumerator: IWarningsEnum;
//    procedure Delete(const AWarning: TWarning); overload;
    procedure Delete(const ASymbol: string);
    function Render: string;
    function IsEmpty: Boolean;
  end;

  TWarningsPersist = class(TNoConstructObject)
  strict private
    class function WarningCompoundName(const Idx: Integer; const Prop: string):
      string;
  public
    class procedure Load(Storage: ISettingsSection; Warnings: IWarnings);
    class procedure Save(Storage: ISettingsSection; Warnings: IWarnings);
  end;

  TWarnings = class(TInterfacedObject, IWarnings, IAssignable)
  strict private
    fWarnings: TList<TWarning>;
    fSwitchOff: Boolean;
    type
      TEnumerator = class(TInterfacedObject, IWarningsEnum)
      strict private
        fEnum: TEnumerator<TWarning>;
      public
        constructor Create(Warnings: TWarnings);
        destructor Destroy; override;
        function GetCurrent: TWarning;
        property Current: TWarning read GetCurrent;
        function MoveNext: Boolean;
      end;
    procedure Delete(const ASymbol: string); overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    function Contains(const ASymbol: string): Boolean;
    procedure Add(const AWarning: TWarning);
    function GetItem(const Idx: Integer): TWarning;
    property Items[const Idx: Integer]: TWarning read GetItem; default;
    function GetSwitchOff: Boolean;
    procedure SetSwitchOff(const Value: Boolean);
    property SwitchOff: Boolean read GetSwitchOff write SetSwitchOff;
    procedure Assign(const Src: IInterface);
    function GetEnumerator: IWarningsEnum;
    procedure Delete(const AWarning: TWarning); overload;
    function Render: string;
    function IsEmpty: Boolean;
//    function Find(const ASymbol: string): TWarning; overload;
//    function Find(const ASymbol: string; out Warning: TWarning): Boolean;
//      overload;
  end;


implementation

uses
  SysUtils, Generics.Defaults, Math,
  UConsts, UExceptions;

{ TWarning }

constructor TWarning.Create(const ASymbol: string; const AMinCompiler: Single);
begin
  Assert(ASymbol <> '', 'TWarning.Create: ASymbol is empty string');
  fSymbol := ASymbol;
  MinCompiler := AMinCompiler;
end;

constructor TWarning.Create(const ASymbol: string);
begin
  Create(ASymbol, MinSupportedCompiler);
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
  if fWarnings.Contains(AWarning) then
    raise EBug.CreateFmt(
      '%s.Add: AWarning %s already in list', [ClassName, AWarning.Symbol]
    );
  fWarnings.Add(AWarning);
end;

procedure TWarnings.Assign(const Src: IInterface);
var
  Idx: Integer; // todo: replace with enumeration
begin
  Clear;
  for Idx := 0 to Pred((Src as IWarnings).Count) do
    Add((Src as IWarnings)[Idx]);
  fSwitchOff := (Src as IWarnings).SwitchOff;
end;

procedure TWarnings.Clear;
begin
  fWarnings.Clear;
end;

function TWarnings.Contains(const ASymbol: string): Boolean;
begin
  Result := fWarnings.Contains(TWarning.Create(ASymbol));
end;

function TWarnings.Count: Integer;
begin
  Result := fWarnings.Count;
end;

constructor TWarnings.Create;
begin
  inherited Create;
  fWarnings := TList<TWarning>.Create(
    TDelegatedComparer<TWarning>.Create(
      function(const Left, Right: TWarning): Integer
      begin
        Result := AnsiCompareText(Left.Symbol, Right.Symbol);
      end
    )
  );
end;

procedure TWarnings.Delete(const AWarning: TWarning);
begin
  fWarnings.Remove(AWarning);
end;

procedure TWarnings.Delete(const ASymbol: string);
begin
  Delete(TWarning.Create(ASymbol));
end;

destructor TWarnings.Destroy;
begin
  fWarnings.Free;
  inherited;
end;

//function TWarnings.Find(const ASymbol: string): TWarning;
//begin
//  if not Find(ASymbol, Result) then
//    raise EBug.CreateFmt('%s.Find: WARNING %s not found', [ClassName, ASymbol]);
//end;
//
//function TWarnings.Find(const ASymbol: string; out Warning: TWarning): Boolean;
//var
//  Idx: Integer;
//begin
//  // we only need ASymbol for comparison. Other fields ignored
//  Idx := fWarnings.IndexOf(TWarning.Create(ASymbol));
//  Result := Idx >= 0;
//  if Result then
//    Warning := fWarnings[Idx];
//end;
//
function TWarnings.GetEnumerator: IWarningsEnum;
begin
  Result := TEnumerator.Create(Self);
end;

function TWarnings.GetItem(const Idx: Integer): TWarning;
begin
  Result := fWarnings[Idx];
end;

function TWarnings.GetSwitchOff: Boolean;
begin
  Result := fSwitchOff;
end;

function TWarnings.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TWarnings.Render: string;
var
  SB: TStringBuilder;
  W: TWarning;
  SortedList: TList<TWarning>;
  CurrentVer: Single;
  InsideVer: Boolean;
begin
  if IsEmpty then
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
    for W in fWarnings do
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
        SB.AppendFormat('      {$WARN %s OFF}' + EOL, [W.Symbol]);
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

procedure TWarnings.SetSwitchOff(const Value: Boolean);
begin
  fSwitchOff := Value;
end;

{ TWarnings.TEnumerator }

constructor TWarnings.TEnumerator.Create(Warnings: TWarnings);
begin
  inherited Create;
  fEnum := Warnings.fWarnings.GetEnumerator;
end;

destructor TWarnings.TEnumerator.Destroy;
begin
  fEnum.Free;
  inherited;
end;

function TWarnings.TEnumerator.GetCurrent: TWarning;
begin
  Result := fEnum.Current;
end;

function TWarnings.TEnumerator.MoveNext: Boolean;
begin
  Result := fEnum.MoveNext;
end;

{ TWarningsPersist }

const
  cInhibitedName = 'SwitchOffWarnings';
  cWarningCountName = 'WarningCount';
  cWarningCompoundName = 'Warning%d.%s';
  cWarningSymbolProp = 'Symbol';
  cWarningSupportProp = 'MinCompiler';

class procedure TWarningsPersist.Load(Storage: ISettingsSection;
  Warnings: IWarnings);
var
  Idx: Integer;
  CompilerVer: Double;
  Symbol: string;
begin
  Warnings.Clear;
  Warnings.SwitchOff := Boolean(
    StrToIntDef(Storage.ItemValues[cInhibitedName], Ord(False))
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
    Warnings.Add(TWarning.Create(Symbol, CompilerVer));
  end;
end;

class procedure TWarningsPersist.Save(Storage: ISettingsSection;
  Warnings: IWarnings);
var
  Idx: Integer;
begin
  Storage.ItemValues[cInhibitedName] := IntToStr(Ord(Warnings.SwitchOff));
  Storage.ItemValues[cWarningCountName] := IntToStr(Warnings.Count);
  for Idx := 0 to Pred(Warnings.Count) do
  begin
    Storage.ItemValues[WarningCompoundName(Idx, cWarningSymbolProp)] :=
      Warnings[Idx].Symbol;
    Storage.ItemValues[WarningCompoundName(Idx, cWarningSupportProp)] :=
      Format('%.2f', [Warnings[Idx].MinCompiler]);
  end;
  Storage.Save;
end;

class function TWarningsPersist.WarningCompoundName(const Idx: Integer;
  const Prop: string): string;
begin
  Result := Format(cWarningCompoundName, [Idx, Prop]);
end;

end.
