{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Abstract base class for classes that control and provide information about
 * Borland compilers.
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
  strict private
    var
      fId: TCompilerID;
        {Identifies compiler}
    function InstallPathFromReg(const RootKey: HKEY): string;
      {Gets compiler install root path from given registry root key, if present.
        @param RootKey [in] Given registry root key.
        @return Required root path or '' if not compiler not installed.
      }
  strict protected
    function SearchDirParams: string; override;
      {One of more parameters that define any search directories to be passed
      to compiler on command line.
        @return Required space separated parameter(s).
      }
    function InstallationRegKey: string; virtual; abstract;
      {Returns name of registry key where records compiler's installation path
      is recorded.
        @return Name of key.
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
  end;


implementation


uses
  // Delphi
  SysUtils, Registry,
  // Project
  UIStringList, UStrUtils, USystemInfo;


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
  if TOSInfo.IsReallyWindowsXPOrGreater then
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

function TBorlandCompiler.SearchDirParams: string;
  {One of more parameters that define any search directories to be passed to
  compiler on command line.
    @return Required space separated parameter(s).
  }
var
  Dirs: IStringList;  // list of search directory strings
begin
  if GetSearchDirs.IsEmpty then
    Exit('');
  Dirs := TIStringList.Create(GetSearchDirs.ToStrings);
  Result := StrQuoteSpaced('-U' + Dirs.GetText(';', False))
    + ' ' + StrQuoteSpaced('-I' + Dirs.GetText(';', False))
    + ' ' + StrQuoteSpaced('-O' + Dirs.GetText(';', False))
    + ' ' + StrQuoteSpaced('-R' + Dirs.GetText(';', False));
end;

end.

