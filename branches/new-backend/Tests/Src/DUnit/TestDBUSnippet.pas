{
  Delphi DUnit Test Cases for the DB.USnippet Unit
  ------------------------------------------------

  $Rev$
  $Date$
}

unit TestDBUSnippet;

interface

uses
  TestFramework, DB.UDataItem, DB.UConsts, DB.USnippet, UBaseObjects;

type

  // Test methods for record TDBSnippetKey
  // must be run before TestTDBSnippet
  TestTDBSnippetKey = class(TTestCase)
  published
    procedure TestCreate;
    procedure TestEqualityOperators;
    procedure TestCompareTo;
  end;

  // Test methods for record TDBSnippetData
  // must be run before TestTDBSnippet
  TestTDBSnippetData = class(TTestCase)
  published
    procedure TestClone;
  end;

  // Test methods for class TDBSnippet
  TestTDBSnippet = class(TTestCase)
  strict private
    function CreateObject(const Data: TDBSnippetData): TDBSnippet;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
  end;

implementation

uses
  SysUtils, UTestHelpers;

{ TestTDBSnippetKey }

procedure TestTDBSnippetKey.TestCompareTo;
var
  K1, K2, K3, K4: TDBSnippetKey;
begin
  K1 := TDBSnippetKey.Create('AAAA', True);
  K2 := TDBSnippetKey.Create('BBBB', True);
  K3 := TDBSnippetKey.Create('BBBB', False);
  K4 := TDBSnippetKey.Create('BBBB', False);

  Check(K1.CompareTo(K2) < 0);
  Check(K2.CompareTo(K1) > 0);
  Check(K1.CompareTo(K3) < 0);
  Check(K3.CompareTo(K1) > 0);
  Check(K2.CompareTo(K3) > 0);
  Check(K3.CompareTo(K2) < 0);
  Check(K3.CompareTo(K4) = 0);
  Check(K4.CompareTo(K3) = 0);
end;

procedure TestTDBSnippetKey.TestCreate;
var
  K1, K2: TDBSnippetKey;
begin
  K1 := TDBSnippetKey.Create('MyName', False);
  K2 := TDBSnippetKey.Create('MyOtherName', True);
  Check(K1.Name = 'MyName',
    Format('Expected K1.Name=''MyName'', got ''%s''', [K1.Name]));
  Check(K1.UserDefined = False,
    Format('Expected K1.UserDefined=False, got %s',
      [BoolToStr(K1.UserDefined)]));
  Check(K2.Name = 'MyOtherName',
    Format('Expected K2.Name=''MyOtherName'', got ''%s''', [K2.Name]));
  Check(K2.UserDefined = True,
    Format('Expected K2.UserDefined=True, got %s',
      [BoolToStr(K2.UserDefined)]));
end;

procedure TestTDBSnippetKey.TestEqualityOperators;
var
  K1, K2, K3, K4, K5: TDBSnippetKey;
begin
  K1 := TDBSnippetKey.Create('AAAA', True);
  K2 := TDBSnippetKey.Create('BBBB', True);
  K3 := TDBSnippetKey.Create('BBBB', False);
  K4 := TDBSnippetKey.Create('BBBB', False);
  K5 := TDBSnippetKey.Create('CCCC', True);

  CheckFalse(K1 = K2);
  CheckFalse(K1 > K2);
  CheckFalse(K1 >= K2);
  CheckTrue(K1 < K2);
  CheckTrue(K1 <= K2);
  CheckTrue(K1 <> K2);

  CheckFalse(K5 = K4);
  CheckTrue(K5 > K4);
  CheckTrue(K5 >= K4);
  CheckFalse(K5 < K4);
  CheckFalse(K5 <= K4);
  CheckTrue(K5 <> K4);

  CheckTrue(K3 = K4);
  CheckFalse(K3 > K4);
  CheckTrue(K3 >= K4);
  CheckFalse(K3 < K4);
  CheckTrue(K3 <= K4);
  CheckFalse(K3 <> K4);

  CheckFalse(K2 = K3);
  CheckTrue(K2 > K3);
  CheckTrue(K2 >= K3);
  CheckFalse(K2 < K3);
  CheckFalse(K2 <= K3);
  CheckTrue(K2 <> K3);
end;

{ TestTDBSnippet }

function TestTDBSnippet.CreateObject(const Data: TDBSnippetData): TDBSnippet;
begin
  Result := TDBSnippet.Create(Data);
  Result.FreeController := TAlwaysFreeController.Create;
end;

procedure TestTDBSnippet.SetUp;
begin
end;

procedure TestTDBSnippet.TearDown;
begin
end;

procedure TestTDBSnippet.TestCreate;
var
  Data: TDBSnippetData;
  Obj: TDBSnippet;
begin
  Data.Name := 'MyName';
  Data.UserDefined := True;
  Data.Description := 'My Description';
  Data.Kind := skConstant;
  // ...

  Obj := CreateObject(Data);
  try
    Check(Obj.Key.Name = 'MyName');
    Check(Obj.Key.UserDefined = True);
    Check(Obj.Name = 'MyName');
    Check(Obj.UserDefined = True);
    Check(Obj.Description = 'My Description');
    Check(Obj.Kind = skConstant);
    // ...
  finally
    Obj.Free;
  end;

end;

{ TestTDBSnippetData }

procedure TestTDBSnippetData.TestClone;
var
  A, B: TDBSnippetData;
begin
  A.Name := 'AName';
  A.UserDefined := True;
  A.Description := 'A Description';
  A.Kind := skTypeDef;
  // ...

  B.Name := 'AnotherName';
  B.UserDefined := False;
  B.Description := 'Another Description';
  B.Kind := skConstant;
  // ...

  Check(B.Name = 'AnotherName');
  Check(B.UserDefined = False);
  Check(B.Description = 'Another Description');
  Check(B.Kind = skConstant);
  // ...

  B := A.Clone;

  Check(B.Name = 'AName');
  Check(B.UserDefined = True);
  Check(B.Description = 'A Description');
  Check(B.Kind = skTypeDef);
  // ...
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTDBSnippetKey.Suite);
  RegisterTest(TestTDBSnippetData.Suite);
  RegisterTest(TestTDBSnippet.Suite);
end.

