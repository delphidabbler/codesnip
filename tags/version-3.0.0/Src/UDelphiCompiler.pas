{
 * UDelphiCompiler.pas
 *
 * Class that controls and provides information about the Delphi v2-7 compilers.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 18 Feb 2005  - Fixed bug in ExecAndWaitRedirect to make return
 *                        expected MaxInt rather than -MaxInt when requested
 *                        program can't be started.
 *                      - Fixed bug where singletion Compilers object was not
 *                        being freed when application closes.
 * v0.3 of 18 Feb 2005  - Deleted unused units from uses clauses.
 * v0.4 of 22 Feb 2005  - Localised compiler error & warning string literals.
 * v0.5 of 24 Feb 2005  - Added Names[] property to TDelphiCompilers that
 *                        returns of a compiler now names removed from
 *                        UCompilerTypes.
 * v0.6 of 05 Mar 2005  - Rewritten to provide only the TDelphiCompiler object
 *                        that now descends from TBaseCompiler and implements
 *                        ICompiler.
 *                      - TDelphiCompilers deleted (now replaced by TCompilers
 *                        in UBaseCompiler).
 *                      - Moved helper routines to UCompilerUtils.
 *                      - Deleted redundant code
 * v0.7 of 20 Apr 2005  - Changed to use renamed IntfCompilers unit.
 * v0.8 of 08 Jan 2006  - Major revision:
 *                        - Pushed up a lot of functionality common with
 *                          TFreePascalCompiler to TCompilerBase.
 *                        - Pushed up other functionality common with new
 *                          TBDSCompiler to new TBorlandCompiler.
 *                        - Added support for ICompilerAutoDetect interface.
 *                        - Made changes to reflect new place in compiler class
 *                          heirachy.
 *                        - Added support for user-defined and default command
 *                          line switches.
 * v0.9 of 09 Jan 2006  - Changed Clone method to call new CreateCopy
 *                        constructor rather than doing copy itself.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                        Removed unused unit references.
 * v1.1 of 11 Aug 2008  - Changed to use renamed glyph resource for Delphi 2 and
 *                        Delphi 3 compilers.
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
 * The Original Code is UDelphiCompiler.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit UDelphiCompiler;


interface


uses
  // Project
  IntfCommon, IntfCompilers, UBorlandCompiler;


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
      {Class constructor. Creates object for a Delphi version.
        @param Ver [in] Version of Delphi.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


resourcestring
  // Template for name of compiler
  sDelphiName = 'Delphi %d';


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
  {Class constructor Creates object for a Delphi version.
    @param Ver [in] Version of Delphi.
  }
begin
  Assert(Ver in [ciD2..ciD7], 'TDelphiCompiler.Create: Invalid Ver');
  inherited Create(Ver);
end;

function TDelphiCompiler.GetIDString: string;
  {Provides a non-localisable string that identifies the compiler.
    @return Compiler id string.
  }
begin
  Result := Format('D%d', [CompilerIDToVerNum]);    // ** do not localise
end;

function TDelphiCompiler.GetName: string;
  {Provides the human readable name of the compiler.
    @return Name of the compiler.
  }
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
  end;
end;

function TDelphiCompiler.InstallationRegKey: string;
  {Returns name of registry key where records compiler's installation path
  is recorded.
    @return Name of key.
  }
begin
  Result := Format(                      // ** do not localise this registry key
    '\SOFTWARE\Borland\Delphi\%d.0', [CompilerIDToVerNum]
  );
end;

end.

