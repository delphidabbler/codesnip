{
 * UTestCompile.pas
 *
 * Class that performs a test compilation of a routine using all supported and
 * installed versions of Delphi and returns details of success or failure.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 18 Feb 2005  - Deleted unused TTestCompile destructor.
 * v0.3 of 23 Feb 2005  - Added static, overloaded, Compile method to
 *                        TTestCompile that compiles a specified routine without
 *                        user having to instantiate the class.
 * v0.4 of 05 Apr 2005  - Changed to use TCompilerID to list supported
 *                        compilers.
 *                      - Changed to use revised ICompiler based compiler object
 *                        to perform actual compilation.
 *                      - Changed to call ICompiler object to tidy binary files
 *                        rather than deleting in this object: this allows for
 *                        different file type across compilers.
 * v0.5 of 20 Apr 2005  - Changed to use renamed USourceGen unit and associated
 *                        renamed class and method.
 *                      - Changed to use renamed IntfCompilers unit.
 * v0.6 of 08 Jan 2006  - Altered some method signatures to remove unused
 *                        parameters.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Removed unused unit references.
 * v1.1 of 04 Dec 2006  - Changed to use TTestUnit to generate test unit and
 *                        removed code from this class that formerly did this.
 * v1.2 of 01 Mar 2007  - Added new TTestCompileThread class.
 * v1.3 of 24 Aug 2008  - Added Windows unit to enable inlining in Delphi 2006.
 * v1.4 of 04 Oct 2008  - Made constructor protected and removed Compile
 *                        instance method, leaving only class Compile method.
 *                        static. Modified other methods accordingly.
 *                      - Made private and protected sections strict.
 *                      - Moved TTestCompile class to implementation.
 *                      - Now use ClassName method in all assert statements.
 * v1.5 of 16 Dec 2008  - Iteration of Compilers changed to use for..in loop.
 * v1.6 of 06 Jan 2009  - Changed to use a specified compilers object to perform
 *                        test compilation instead of global Compilers object.
 * v1.7 of 11 Jan 2009  - Moved TTestCompile to interface.
 *                      - TTestCompileThread removed as no longer required.
 *
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
 * The Original Code is UTestCompile.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UTestCompile;


interface


uses
  // Project
  IntfCompilers, UBaseObjects, USnippets;


type

  {
  TTestCompile:
    Class that performs a test compilation of a routine using all supported and
    installed compilers and returns details of success or failure.
  }
  TTestCompile = class(TNoPublicConstructObject)
  strict private
    fRoutine: TRoutine;
      {The routine we are to compile}
    fCompilers: ICompilers;
      {Object used to perform compilation}
    procedure GenerateSourceFile(out FileName: string);
      {Generates a source file for routine under test.
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
      {Compiles routine under test with all installed and supported compiler
      versions.
        @return Compilation results for each supported compiler (crQuery is
          returned for each supported compiler that is not installed).
      }
  strict protected
    constructor InternalCreate(const ACompilers: ICompilers;
      const ARoutine: TRoutine);
      {Class constructor. Sets up object that can test compile a routine.
        @param ACompilers [in] Compilers object used to perform compilation.
        @param ARoutine [in] Routine to be test compiled.
      }
  public
    class function Compile(const ACompilers: ICompilers;
      const ARoutine: TRoutine): TCompileResults;
      {Compiles a specified routine with all installed and supported compilers.
        @param ACompilers [in] Compilers object used to perform compilation.
        @param ARoutine [in] Routine to be compiled.
        @return Compilation results for each supported compiler (crQuery is
          returned for each supported compiler that is not installed).
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
  const ARoutine: TRoutine): TCompileResults;
  {Compiles a specified routine with all installed and supported compilers.
    @param ACompilers [in] Compilers object used to perform compilation.
    @param ARoutine [in] Routine to be compiled.
    @return Compilation results for each supported compiler (crQuery is returned
      for each supported compiler that is not installed).
  }
begin
  with InternalCreate(ACompilers, ARoutine) do
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
  {Compiles routine under test with all installed and supported compiler
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
  {Generates a source file for routine under test.
    @param FileName [out] Name of the generated file.
  }
begin
  with TTestUnit.Create(fRoutine) do
    try
      SaveUnit(FileName);
    finally
      Free;
    end;
end;

constructor TTestCompile.InternalCreate(const ACompilers: ICompilers;
  const ARoutine: TRoutine);
  {Class constructor. Sets up object that can test compile a routine.
    @param ACompilers [in] Compilers object used to perform compilation.
    @param ARoutine [in] Routine to be test compiled.
  }
begin
  Assert(Assigned(ARoutine),                               // ** do not localise
    ClassName + '.InternalCreate: ARoutine is nil');
  Assert(Assigned(ACompilers),                             // ** do not localise
    ClassName + '.InternalCreate: ACompilers is nil');
  inherited InternalCreate;
  fRoutine := ARoutine;
  fCompilers := ACompilers;
end;

end.

