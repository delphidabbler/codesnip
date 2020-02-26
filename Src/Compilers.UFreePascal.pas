{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements class that wraps the Free Pascal compiler. Controls compilation,
 * processes compiler output and provides information about the compiler.
}


unit Compilers.UFreePascal;


interface


uses
  // Project
  Compilers.UCompilerBase, Compilers.UGlobals, IntfCommon;


type

  {
  TFreePascalCompiler:
    Class that wraps the Free Pascal compiler. Controls compilation, processes
    compiler output and provides information about the compiler.
  }
  TFreePascalCompiler = class(TCompilerBase,
    IClonable,      // can clone this object
    ICompiler       // this is a compiler
  )
  strict protected
    function SearchDirParams: string; override;
      {One of more parameters that define any search directories to be passed
      to compiler on command line.
        @return Required space separated parameter(s).
      }
  public
    { IClonable }
    function Clone: IInterface;
      {Creates a new instance of the object that is an extact copy and returns
      it.
        @return Cloned object.
      }
    { ICompiler method overrides }
    function GetName: string; override;
      {Provides the human readable name of the compiler.
        @return Name of the compiler.
      }
    function GetID: TCompilerID; override;
      {Provides the unique id of the compiler.
        @return Compiler id.
      }
    function GetIDString: string; override;
      {Provides a non-localisable string that identifies the compiler.
        @return Compiler id string.
      }
    function GetDefaultSwitches: string; override;
      {Returns default command line switches for compiler.
        @return Switches separated by commas.
      }
    procedure DeleteObjFiles(const Path, Project: string); override;
      {Deletes binary intermdiates files created during a compilation. Does
      nothing if there has been no compilation.
        @param Path [in] Path where project file is found.
        @param Project [in] Name of project (source file)
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining},
  // Project
  UIStringList, UStrUtils;


{ TFreePascalCompiler }

function TFreePascalCompiler.Clone: IInterface;
  {Creates a new instance of the object that is an extact copy and returns it.
    @return Cloned object.
  }
begin
  Result := TFreePascalCompiler.CreateCopy(Self);
end;

procedure TFreePascalCompiler.DeleteObjFiles(const Path, Project: string);
  {Deletes binary intermdiates files created during a compilation. Does
  nothing if there has been no compilation.
    @param Path [in] Path where project file is found.
    @param Project [in] Name of project (source file)
  }
begin
  SysUtils.DeleteFile(
    IncludeTrailingPathDelimiter(Path) + ChangeFileExt(Project, '.o')
  );
  SysUtils.DeleteFile(
    IncludeTrailingPathDelimiter(Path) + ChangeFileExt(Project, '.ppu')
  );
end;

function TFreePascalCompiler.GetDefaultSwitches: string;
  {Returns default command line switches for compiler.
    @return Switches separated by commas.
  }
begin
  Result := '-vew,'       // emit error and warning messages
         + '-Mdelphi,'    // compile in Delphi mode
         + '-Sghia,'      // label/goto command,
                          // ANSI strings,
                          // C++ style inline
                          // Compile in Assert statements
         + '-Twin32,'     // Windows 32 target
         + '-Ci';         // I/O checking
end;

function TFreePascalCompiler.GetID: TCompilerID;
  {Provides the unique id of the compiler.
    @return Compiler id.
  }
begin
  Result := ciFPC;
end;

function TFreePascalCompiler.GetIDString: string;
  {Provides a non-localisable string that identifies the compiler.
    @return Compiler id string.
  }
begin
  Result := 'FPC';
end;

function TFreePascalCompiler.GetName: string;
  {Provides the human readable name of the compiler.
    @return Name of the compiler.
  }
resourcestring
  sFreePascalName = 'Free Pascal';    // name of compiler
begin
  Result := sFreePascalName;
end;

function TFreePascalCompiler.SearchDirParams: string;
  {One of more parameters that define any search directories to be passed
  to compiler on command line.
    @return Required space separated parameter(s).
  }
var
  Dirs: IStringList;  // list of search directory names
  DirName: string;    // each search directory name
begin
  if GetSearchDirs.IsEmpty then
    Exit('');
  Dirs := TIStringList.Create;
  for DirName in GetSearchDirs do
  begin
    Dirs.Add(StrQuoteSpaced('-Fu' + DirName));
    Dirs.Add(StrQuoteSpaced('-Fi' + DirName));
    Dirs.Add(StrQuoteSpaced('-Fl' + DirName));
    Dirs.Add(StrQuoteSpaced('-Fo' + DirName));
  end;
  Result := Dirs.GetText(' ', False);
end;

end.

