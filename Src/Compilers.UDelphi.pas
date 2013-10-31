{
 * Compilers.UDelphi.pas
 *
 * Class that controls and provides information about the Delphi v2-7 compilers.
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
 * The Original Code is Compilers.UDelphi.pas, formerly UDelphiCompiler.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  private
    function CompilerIDToVerNum: Integer;
      {Converts the compiler ID that defines the compiler to the Delphi version
      number.
        @return Delphi version number.
      }
  protected
    function GlyphResourceName: string; override;
      {Name of any resource containing a "glyph" bitmap for a compiler.
        @return Resource name or '' if the compiler has no glyph.
      }
  protected
    function InstallationRegKey: string; override;
      {Returns name of registry key where records compiler's installation path
      is recorded.
        @return Name of key.
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
  public
    constructor Create(const Ver: TCompilerID);
      {Class constructor. Creates object for a classic Delphi version.
        @param Ver [in] Version of Delphi.
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

function TDelphiCompiler.GlyphResourceName: string;
  {Name of any resource containing a "glyph" bitmap for a compiler.
    @return Resource name or '' if the compiler has no glyph.
  }
begin
  case GetID of
    ciD2, ciD3: Result := 'DELPHI2AND3';
    ciD4: Result := 'DELPHI4';
    ciD5: Result := 'DELPHI5';
    ciD6: Result := 'DELPHI6';
    ciD7: Result := 'DELPHI7';
    else raise EBug.Create(ClassName + '.GlyphResourceName: Invalid ID');
  end;
end;

function TDelphiCompiler.InstallationRegKey: string;
  {Returns name of registry key where records compiler's installation path
  is recorded.
    @return Name of key.
  }
begin
  Result := Format('\SOFTWARE\Borland\Delphi\%d.0', [CompilerIDToVerNum]);
end;

end.

