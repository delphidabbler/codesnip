{
 * UBDSCompiler.pas
 *
 * Class that controls and provides information about Borland Development System
 * Win32 compilers.
 *
 * v0.1 of 08 Jan 2006  - Original version.
 * v0.2 of 09 Jan 2006  - Changed Clone method to call new CreateCopy
 *                        constructor rather than doing copy itself.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Removed unused unit reference.
 * v1.1 of 08 May 2007  - Added support for Delphi 2007 to TBDSCompiler.
 * v1.2 of 11 Aug 2008  - Changed to use single resource name "BDS" for all
 *                        compiler glyphs.
 * v1.3 of 11 Oct 2008  - Added support for Delphi 2009 to TBDSCompiler.
 *                      - Made protected and private section of class strict.
 *                      - Assert and EBug messages now use ClassName to get name
 *                        of class.
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
 * The Original Code is UBDSCompiler.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UBDSCompiler;


interface


uses
  // Project
  IntfCommon, IntfCompilers, UBorlandCompiler;


type

  {
  TBDSCompiler:
    Class that controls and provides information about Borland Development
    System Delphi Win32 compilers.
  }
  TBDSCompiler = class(TBorlandCompiler,
    IClonable,            // can clone this object
    ICompiler,            // this is a compiler
    ICompilerAutoDetect   // can auto detect compiler exec file path
  )
  strict private
    function ProductVersion: Integer;
      {Delphi version number.
        @return Required major version number.
      }
  strict protected
    function GlyphResourceName: string; override;
      {Name of any resource containing a "glyph" bitmap for a compiler.
        @return Resource name or '' if the compiler has no glyph.
      }
    function InstallationRegKey: string; override;
      {Returns name of registry key where records compiler's installation path
      is recorded.
        @return Name of key.
      }
  protected
    { IClonable }
    function Clone: IInterface;
      {Create a new instance of the object that is an extact copy and return it.
        @return New object's IInterface interface.
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
  public
    constructor Create(const Id: TCompilerID);
      {Class constructor: creates object for a BDS compiler.
        @param Id [in] Identifies compiler version.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions;


{ TBDSCompiler }

function TBDSCompiler.Clone: IInterface;
  {Create a new instance of the object that is an extact copy and return it.
    @return New object's IInterface interface.
  }
begin
  Result := TBDSCompiler.CreateCopy(Self);
end;

constructor TBDSCompiler.Create(const Id: TCompilerID);
  {Class constructor: creates object for a BDS compiler.
    @param Id [in] Identifies compiler version.
  }
begin
  Assert(Id in [ciD2005w32, ciD2006w32, ciD2007, ciD2009w32],
    ClassName + '.Create: Invalid Id');                    // ** do not localise
  inherited Create(Id);
end;

function TBDSCompiler.GetIDString: string;
  {Provides a non-localisable string that identifies the compiler.
    @return Compiler id string.
  }
begin
  // ** do not localise string literals in this method
  Result := Format('D%d', [ProductVersion]);
  if GetID in [ciD2005w32, ciD2006w32, ciD2009w32] then
    Result := Result + 'w32';
end;

function TBDSCompiler.GetName: string;
  {Provides the human readable name of the compiler.
    @return Name of the compiler.
  }
resourcestring
  sCompilerName = 'Delphi %d';  // template for name of compiler
begin
  Result := Format(sCompilerName, [ProductVersion])
end;

function TBDSCompiler.GlyphResourceName: string;
  {Name of any resource containing a "glyph" bitmap for a compiler.
    @return Resource name or '' if the compiler has no glyph.
  }
begin
  Result := 'BDS';                                         // ** do not localise
end;

function TBDSCompiler.InstallationRegKey: string;
  {Returns name of registry key where records compiler's installation path
  is recorded.
    @return Name of key.
  }
begin
  // ** do not localise any literal strings in this method
  case GetID of
    ciD2005w32: Result := '\SOFTWARE\Borland\BDS\3.0';
    ciD2006w32: Result := '\SOFTWARE\Borland\BDS\4.0';
    ciD2007   : Result := '\SOFTWARE\Borland\BDS\5.0';
    ciD2009w32: Result := '\SOFTWARE\CodeGear\BDS\6.0';
    else
      raise EBug.Create(
        ClassName + '.InstallationRegKey: Invalid ID'
      );
  end;
end;

function TBDSCompiler.ProductVersion: Integer;
  {Delphi version number.
    @return Required major version number.
  }
begin
  case GetID of
    ciD2005w32: Result := 2005;
    ciD2006w32: Result := 2006;
    ciD2007:    Result := 2007;
    ciD2009w32: Result := 2009;
    else
      raise EBug.Create(
        ClassName + '.ProductVersion: Invalid ID'          // ** do not localise
      );
  end;
end;

end.

