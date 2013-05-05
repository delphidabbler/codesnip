{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Class that controls and provides information about the Delphi v2-7 compilers.
}


unit Compilers.UDelphi;


interface


uses
  // Project
  Compilers.UBorland, Compilers.UGlobals, IntfCommon;


type

  {
  TDelphiCompiler:
    Class that controls and provides information about the Delphi v2-7
    compilers.
  }
  TDelphiCompiler = class(TBorlandCompiler,
    IClonable,            // can clone this object
    ICompiler,            // this is a compiler
    ICompilerAutoDetect   // can auto detect compiler exec file path
  )
  strict private
    function CompilerIDToVerNum: Integer;
      {Converts the compiler ID that defines the compiler to the Delphi version
      number.
        @return Delphi version number.
      }
  strict protected
    function InstallationRegKey: string; override;
      {Returns name of registry key where compiler's installation path is
      recorded.
        @return Name of key.
      }
  public
    constructor Create(const Ver: TCompilerID);
      {Class constructor. Creates object for a classic Delphi version.
        @param Ver [in] Version of Delphi.
      }
    { IClonable }
    function Clone: IInterface;
      {Create a new instance of the object that is an extact copy and return it.
        @return Cloned object.
      }
    { ICompiler method overrides }
    function GetName: string; override;
      {Provides the human readable name of the compiler.
        @return Name of the compiler.
      }
    function GetIDString: string; override;
      {Provides a non-localisable string that identifies the compiler.
        @return Compiler id string.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions;


{ TDelphiCompiler }

function TDelphiCompiler.Clone: IInterface;
  {Create a new instance of the object that is an extact copy and return it.
    @return Cloned object.
  }
begin
  Result := TDelphiCompiler.CreateCopy(Self);
end;

function TDelphiCompiler.CompilerIDToVerNum: Integer;
  {Converts the compiler ID that defines the compiler to the Delphi version
  number.
    @return Delphi version number.
  }
begin
  // The following code assumes ciD2 to ciD7 are contiguous
  Result := 2 + Ord(GetID) - Ord(ciD2)
end;

constructor TDelphiCompiler.Create(const Ver: TCompilerID);
  {Class constructor Creates object for a classic Delphi version.
    @param Ver [in] Version of Delphi.
  }
begin
  Assert(Ver in cClassicDelphiCompilers, ClassName + '.Create: Invalid Ver');
  inherited Create(Ver);
end;

function TDelphiCompiler.GetIDString: string;
  {Provides a non-localisable string that identifies the compiler.
    @return Compiler id string.
  }
begin
  Result := Format('D%d', [CompilerIDToVerNum]);
end;

function TDelphiCompiler.GetName: string;
  {Provides the human readable name of the compiler.
    @return Name of the compiler.
  }
resourcestring
  // Template for name of compiler
  sDelphiName = 'Delphi %d';  // template for name of compiler
begin
  Result := Format(sDelphiName, [CompilerIDToVerNum]);
end;

function TDelphiCompiler.InstallationRegKey: string;
  {Returns name of registry key where compiler's installation path is recorded.
    @return Name of key.
  }
begin
  Result := Format('\SOFTWARE\Borland\Delphi\%d.0', [CompilerIDToVerNum]);
end;

end.

