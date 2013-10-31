{
 * Compilers.UBorland.pas
 *
 * Abstract base class for classes that control and provide information about
 * Borland compilers.
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
 * The Original Code is Compilers.UBorland.pas, formerly UBorlandCompiler.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Compilers.UBorland;


interface


uses
  // Delphi
  Windows,
  // Project
  Compilers.UCompilerBase, Compilers.UGlobals;


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
    function InstallPathFromReg(const RootKey: HKEY): string;
      {Gets compiler install root path from given registry root key, if present.
        @param RootKey [in] Given registry root key.
        @return Required root path or '' if not compiler not installed.
      }
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
  SysUtils, Registry,
  // Project
  USystemInfo;


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
  SysUtils.DeleteFile(
    IncludeTrailingPathDelimiter(Path) + ChangeFileExt(Project, '.dcu')
  );
end;

function TBorlandCompiler.DetectExeFile: Boolean;
  {Detects and records path to command line compiler if present.
    @return True if compiler path found, false otherwise.
  }
var
  InstDir: string;    // installation root directory
begin
  // try HKLM
  InstDir := InstallPathFromReg(HKEY_LOCAL_MACHINE);
  if InstDir = '' then
    // in case install was for user only, try HKCU
    InstDir := InstallPathFromReg(HKEY_CURRENT_USER);
  if InstDir = '' then
    Exit(False);
  SetExecFile(IncludeTrailingPathDelimiter(InstDir) + 'Bin\DCC32.exe');
  Result := True;
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

function TBorlandCompiler.InstallPathFromReg(const RootKey: HKEY): string;
  {Gets compiler install root path from given registry root key, if present.
    @param RootKey [in] Given registry root key.
    @return Required root path or '' if not compiler not installed.
  }
var
  Reg: TRegistry; // registry accessor
begin
  if TOSInfo.CheckReportedOS(TOSInfo.WinXP) then
    Reg := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY)
  else
    // KEY_WOW64_64KEY is not supported on Windows 2000
    Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := RootKey;
    if not Reg.OpenKey(InstallationRegKey, False) then
      Exit('');
    Result := Reg.ReadString('RootDir');
  finally
    Reg.Free;
  end;
end;

end.

