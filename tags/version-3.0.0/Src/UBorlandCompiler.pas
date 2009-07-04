{ ##
  @FILE                     UBorlandCompiler.pas
  @COMMENTS                 Abstract base class for classes that control and
                            provide information about Borland compilers.
                            Provides common functionality.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None.
  @HISTORY(
    @REVISION(
      @VERSION              0.1
      @DATE                 08/01/2006
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              0.2
      @DATE                 09/01/2006
      @COMMENTS             Added new CreateCopy constructor.
    )
    @REVISION(
      @VERSION              1.0
      @DATE                 24/05/2006
      @COMMENTS             + Improved and corrected comments.
                            + Relocated and rationalised $WARN directives.
                            + Removed warnings about un-fixed interfaces - now
                              fixed.
                            + Moved unit reference from interface to
                              implementation.
    )
  )
}


{
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
 * The Original Code is UBorlandCompiler.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit UBorlandCompiler;


interface


uses
  // Project
  IntfCompilers, UCompilerBase;


type

  {
  TBorlandCompiler:
    Abstract base class for classes that control and provide information about
    Borland compilers. Provides common functionality.
  }
  TBorlandCompiler = class(TCompilerBase)
  private
    fId: TCompilerID;
      {Identifies compiler}
  protected
    function InstallationRegKey: string; virtual; abstract;
      {Returns name of registry key where records compiler's installation path
      is recorded.
        @return Name of key.
      }
    { ICompilerAutoDetect }
    function DetectExeFile: Boolean;
      {Detects and records path to command line compiler if present.
        @return True if compiler path found, false otherwise.
      }
    { ICompiler }
    function GetDefaultSwitches: string; override;
      {Returns default command line switches for compiler.
        @return Switches separated by commas.
      }
    procedure DeleteObjFiles(const Path, Project: string); override;
      {Deletes binary intermdiates files created during a compilation. Does
      nothing if there has been no compilation.
        @param Path Path where project file is found.
        @param Project Name of project (source file)
      }
    function GetID: TCompilerID; override;
      {Provides the unique id of the compiler.
        @return Compiler id.
      }
  public
    constructor Create(const Id: TCompilerID);
      {Class constructor. Creates object for a Borland compiler.
        @param Id Compiler id.
      }
    constructor CreateCopy(const Obj: TBorlandCompiler);
      {Copy constructor. Creates a new object that is copy of another object.
        @param Obj Compiler object to copy.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Registry, Windows,
  // Project
  UUtils;


constructor TBorlandCompiler.Create(const Id: TCompilerID);
  {Class constructor. Creates object for a Borland compiler.
    @param Id Compiler id.
  }
begin
  inherited Create;
  fId := Id;
end;

constructor TBorlandCompiler.CreateCopy(const Obj: TBorlandCompiler);
  {Copy constructor. Creates a new object that is copy of another object.
    @param Obj Compiler object to copy.
  }
begin
  inherited CreateCopy(Obj);
  fId := Obj.GetID;
end;

procedure TBorlandCompiler.DeleteObjFiles(const Path, Project: string);
  {Deletes binary intermdiates files created during a compilation. Does
  nothing if there has been no compilation.
    @param Path Path where project file is found.
    @param Project Name of project (source file)
  }
begin
  SysUtils.DeleteFile(DirToPath(Path) + ChangeFileExt(Project, '.dcu'));
end;

function TBorlandCompiler.DetectExeFile: Boolean;
  {Detects and records path to command line compiler if present.
    @return True if compiler path found, false otherwise.
  }
var
  Reg: TRegistry;     // registry object
  InstDir: string;    // installation root directory
begin
  Result := False;
  // Open registry
  Reg := TRegistry.Create;
  try
    // Open key where required Delphi version stores root directory
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(InstallationRegKey) then
    begin
      // Get compiler file spec. First read Delphi's root path from registry,
      // and check for errors Reg.ReadString may raise exception. We also check
      // for no value from registry
      InstDir := Reg.ReadString('RootDir');                // ** do not localise
      Result := (InstDir <> '');
    end;
    if Result then
      SetExecFile(DirToPath(InstDir) + 'Bin\DCC32.exe');   // ** do not localise
  finally
    // Close registry
    Reg.Free;
  end;
end;

function TBorlandCompiler.GetDefaultSwitches: string;
  {Returns default command line switches for Borland compilers.
    @return Switches separated by commas.
  }
begin
  Result := '-Q,'     // Quiet mode: suppress some output
         +  '-W+,'    // Warnings ON
         +  '-$B-,'   // Full boolean Evaluation OFF
         +  '-$T-,'   // Typed @ operator OFF
         +  '-$H+,'   // Use long strings by default ON
         +  '-$I+,'   // I/O checking ON
         +  '-$V+,'   // Strict var-strings ON
         +  '-$J-,'   // Writeable structured consts OFF
         +  '-$X+,'   // Extended syntax ON
         +  '-$M-,'   // Runtime type info OFF
         +  '-$O+,'   // Optimization ON
         +  '-$Z1,'   // Minimum size of enum types = 1
         +  '-$P+';   // Open string params ON
end;

function TBorlandCompiler.GetID: TCompilerID;
  {Provides the unique id of the compiler.
    @return Compiler id.
  }
begin
  Result := fId;
end;

end.

