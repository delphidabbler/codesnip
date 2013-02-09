{
  Delphi DUnit Test Case for the DB.USnippetKindGroup Unit
  --------------------------------------------------------

  $Rev$
  $Date$
}

unit TestDBUSnippetKindGroup;

interface

uses
  TestFramework, DB.UBaseGroup, DB.UConsts, DB.USnippetKindGroup;

type

  // Test methods for record TDBSnippetKindGroupKey
  TestTDBSnippetKindGroupKey = class(TTestCase)
  published
    procedure TestCreate;
    procedure TestEqualityOperators;
    procedure TestImplicitTSnippetKindCast;
  end;

  // Test methods for class TDBSnippetKindGroup
  TestTDBSnippetKindGroup = class(TTestCase)
  strict private
    function CreateObject(const Kind: TDBSnippetKind): TDBSnippetKindGroup;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
  end;

implementation

uses
  UTestHelpers;

{ TestTDBSnippetKindGroupKey }

procedure TestTDBSnippetKindGroupKey.TestCreate;
var
  K: TDBSnippetKindGroupKey;
begin
  K := TDBSnippetKindGroupKey.Create(skRoutine);
  Check(K.Kind = skRoutine);
  K := TDBSnippetKindGroupKey.Create(skConstant);
  Check(K.Kind = skConstant);
  K := TDBSnippetKindGroupKey.Create(skTypeDef);
  Check(K.Kind = skTypeDef);
  K := TDBSnippetKindGroupKey.Create(skFreeform);
  Check(K.Kind = skFreeform);
end;

procedure TestTDBSnippetKindGroupKey.TestEqualityOperators;
var
  K0, K2, K2a, K3: TDBSnippetKindGroupKey;
begin
  K0 := TDBSnippetKindGroupKey.Create(skRoutine);
  K2 := TDBSnippetKindGroupKey.Create(skTypeDef);
  K2a := TDBSnippetKindGroupKey.Create(skTypeDef);
  K3 := TDBSnippetKindGroupKey.Create(skFreeform);

  CheckFalse(K0 = K3);
  CheckTrue(K0 <> K3);

  CheckFalse(K3 = K2);
  CheckTrue(K3 <> K2);

  CheckTrue(K2 = K2a);
  CheckFalse(K2 <> K2a);
end;

procedure TestTDBSnippetKindGroupKey.TestImplicitTSnippetKindCast;
var
  Key: TDBSnippetKindGroupKey;
  Kind: TDBSnippetKind;
begin
  Key := TDBSnippetKindGroupKey.Create(skConstant);
  Kind := Key;
  Check(Kind = skConstant);
end;

{ TestTDBSnippetKindGroup }

function TestTDBSnippetKindGroup.CreateObject(const Kind: TDBSnippetKind):
  TDBSnippetKindGroup;
begin
  Result := TDBSnippetKindGroup.Create(Kind);
  Result.FreeController := TAlwaysFreeController.Create;
end;

procedure TestTDBSnippetKindGroup.SetUp;
begin
end;

procedure TestTDBSnippetKindGroup.TearDown;
begin
end;

procedure TestTDBSnippetKindGroup.TestCreate;
var
  O0, O1, O2, O3: TDBSnippetKindGroup;
resourcestring
  sRoutine = 'Routine';
  sConstant = 'Constant';
  sType = 'Type Definition';
  sFreeform = 'Freeform';
begin
  O0 := nil;  O1 := nil;  O2 := nil;  O3 := nil;
  try
    O0 := CreateObject(skRoutine);
    O1 := CreateObject(skConstant);
    O2 := CreateObject(skTypeDef);
    O3 := CreateObject(skFreeform);

    Check(O0.Key.Kind = skRoutine, 'Expected O0.Key.Kind = skRoutine');
    Check(O1.Key.Kind = skConstant, 'Expected O1.Key.Kind = skConstant');
    Check(O2.Key.Kind = skTypeDef, 'Expected O2.Key.Kind = skType');
    Check(O3.Key.Kind = skFreeform, 'Expected O3.Key.Kind = skFreeform');

    Check(O0.DisplayName = sRoutine, 'Expected O0.DisplayName = ' + sRoutine);
    Check(O1.DisplayName = sConstant, 'Expected O0.DisplayName = ' + sConstant);
    Check(O2.DisplayName = sType, 'Expected O0.DisplayName = ' + sType);
    Check(O3.DisplayName = sFreeform, 'Expected O0.DisplayName = ' + sFreeform);
  finally
    O0.Free;  O1.Free;  O2.Free;  O3.Free;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTDBSnippetKindGroupKey.Suite);
  RegisterTest(TestTDBSnippetKindGroup.Suite);
end.

