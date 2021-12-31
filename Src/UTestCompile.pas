{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class that performs a test compilation of a snippet using all supported and
 * installed versions of Delphi and returns details of success or failure.
}


unit UTestCompile;


interface


uses
  // Delphi
  Classes,
  // Project
  Compilers.UGlobals, DB.USnippet, UBaseObjects;


type

  {
  TTestCompile:
    Class that performs a test compilation of a snippet using all supported and
    installed compilers and returns details of success or failure.
  }
  TTestCompile = class(TNoPublicConstructObject)
  strict private
    fSnippet: TSnippet;
      {The snippet we are to compile}
    fCompilers: ICompilers;
      {Object used to perform compilation}
    procedure GenerateSourceFile(out FileName: string);
      {Generates a source file for snippet under test.
        @param FileName [out] Name of the generated file.
      }
    class function CompileSourceFile(const SrcFile: string;
      const Compiler: ICompiler): TCompileResult;
      {Compiles a source file using a specified compiler.
        @param SrcFile [in] Fully specified name of the source file.
        @param Compiler [in] Compiler to be used.
        @return Result of the compilation.
      }
    function DoCompile: TCompileResults;
      {Compiles snippet under test with all installed and supported compiler
      versions.
        @return Compilation results for each supported compiler (crQuery is
          returned for each supported compiler that is not installed).
      }
  strict protected
    constructor InternalCreate(const ACompilers: ICompilers;
      const ASnippet: TSnippet);
      {Class constructor. Sets up object that can test compile a snippet.
        @param ACompilers [in] Compilers object used to perform compilation.
        @param ASnippet [in] Snippet to be test compiled.
      }
  public
    class function Compile(const ACompilers: ICompilers;
      const ASnippet: TSnippet): TCompileResults;
      {Compiles a specified snippet with all installed and supported compilers.
        @param ACompilers [in] Compilers object used to perform compilation.
        @param ASnippet [in] Snippet to be compiled.
        @return Compilation results for each supported compiler (crQuery is
          returned for each supported compiler that is not installed).
      }
  end;

  {
  TTestCompileThread:
    Thread that performs a test compilation of a snippet.
  }
  TTestCompileThread = class(TThread)
  strict private
    var fCompilers: ICompilers; // Compilers used for test compilation
    var fSnippet: TSnippet;     // Snippet to be compiled
  strict protected
    procedure Execute; override;
      {Performs test compilation in a thread.
      }
  public
    constructor Create(ACompilers: ICompilers; ASnippet: TSnippet);
      {Object constructor. Sets up suspended thread.
        @param ACompilers [in] Compilers to be used for test compilation.
        @param ASnippet [in] Snippet to be compiled.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining},
  // Project
  UTestUnit;


{ TTestCompile }

class function TTestCompile.Compile(const ACompilers: ICompilers;
  const ASnippet: TSnippet): TCompileResults;
  {Compiles a specified snippet with all installed and supported compilers.
    @param ACompilers [in] Compilers object used to perform compilation.
    @param ASnippet [in] Snippet to be compiled.
    @return Compilation results for each supported compiler (crQuery is returned
      for each supported compiler that is not installed).
  }
begin
  with InternalCreate(ACompilers, ASnippet) do
    try
      Result := DoCompile;
    finally
      Free;
    end;
end;

class function TTestCompile.CompileSourceFile(const SrcFile: string;
  const Compiler: ICompiler): TCompileResult;
  {Compiles a source file using a specified compiler.
    @param SrcFile [in] Fully specified name of the source file.
    @param Compiler [in] Compiler to be used.
    @return Result of the compilation.
  }
var
  Dir: string;  // Directory containing source file and used for output
begin
  Dir := ExtractFileDir(SrcFile);
  Result := Compiler.Compile(Dir, ExtractFileName(SrcFile));
  Compiler.DeleteObjFiles(Dir, ExtractFileName(SrcFile));
end;

function TTestCompile.DoCompile: TCompileResults;
  {Compiles snippet under test with all installed and supported compiler
  versions.
    @return Compilation results for each supported compiler (crQuery is returned
      for each supported compiler that is not installed).
  }
var
  Compiler: ICompiler;  // references each supported compiler
  SrcFileName: string;  // name of test unit source file
begin
  GenerateSourceFile(SrcFileName);
  try
    for Compiler in fCompilers do
      if Compiler.IsAvailable then
        Result[Compiler.GetID] := CompileSourceFile(SrcFileName, Compiler)
      else
        Result[Compiler.GetID] := crQuery;
  finally
    SysUtils.DeleteFile(SrcFileName);
  end;
end;

procedure TTestCompile.GenerateSourceFile(out FileName: string);
  {Generates a source file for snippet under test.
    @param FileName [out] Name of the generated file.
  }
begin
  with TTestUnit.Create(fSnippet) do
    try
      SaveUnit(FileName);
    finally
      Free;
    end;
end;

constructor TTestCompile.InternalCreate(const ACompilers: ICompilers;
  const ASnippet: TSnippet);
  {Class constructor. Sets up object that can test compile a snippet.
    @param ACompilers [in] Compilers object used to perform compilation.
    @param ASnippet [in] Snippet to be test compiled.
  }
begin
  Assert(Assigned(ASnippet), ClassName + '.InternalCreate: ASnippet is nil');
  Assert(Assigned(ACompilers),
    ClassName + '.InternalCreate: ACompilers is nil');
  inherited InternalCreate;
  fSnippet := ASnippet;
  fCompilers := ACompilers;
end;

{ TTestCompileThread }

constructor TTestCompileThread.Create(ACompilers: ICompilers;
  ASnippet: TSnippet);
  {Object constructor. Sets up suspended thread.
    @param ACompilers [in] Compilers to be used for test compilation.
    @param ASnippet [in] Snippet to be compiled.
  }
begin
  inherited Create(True);
  fCompilers := ACompilers;
  fSnippet := ASnippet;
end;

procedure TTestCompileThread.Execute;
  {Performs test compilation in a thread.
  }
begin
  TTestCompile.Compile(fCompilers, fSnippet);
end;

end.

