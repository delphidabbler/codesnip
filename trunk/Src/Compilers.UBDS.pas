{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Class that controls and provides information about Borland CodeGear and
 * Embarcadero "BDS" Win32 compilers.
}


unit Compilers.UBDS;


interface


uses
  // Project
  Compilers.UBorland, Compilers.UGlobals, IntfCommon;


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
    var
      ///  <summary>Space separated list of RTL namespaces to be passed to
      ///  compiler.</summary>
      fRTLNamespaces: string;
    function ProductVersion: Integer;
      {Delphi version number.
        @return Required major version number.
      }
  strict protected
    function InstallationRegKey: string; override;
      {Returns name of registry key where records compiler's installation path
      is recorded.
        @return Name of key.
      }
    ///  <summary>Returns any namespace parameter to be passed to compiler on
    ///  command line.</summary>
    function NamespaceParam: string; override;
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
    ///  <summary>Checks if compiler has RTL unit names that are prefixed by
    ///  its namespace.</summary>
    function RequiresRTLNamespaces: Boolean; override;
    ///  <summary>Returns a space separated list of compiler's default RTL unit
    ///  namespaces.</summary>
    function GetDefaultRTLNamespaces: string; override;
    ///  <summary>Returns a space separated list of user-defined RTL unit
    ///  namespaces.</summary>
    function GetRTLNamespaces: string; override;
    ///  <summary>Sets user defined RTL unit namespaces.</summary>
    ///  <remarks>Namespaces is expected to be a space separated list of valid
    ///  Pascal identfiers.</remarks>
    procedure SetRTLNamespaces(const Namespaces: string); override;
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
  UExceptions, UIStringList;


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
  Assert(Id in cBDSCompilers, ClassName + '.Create: Invalid Id');
  inherited Create(Id);
  fRTLNamespaces := GetDefaultRTLNamespaces;
end;

function TBDSCompiler.GetDefaultRTLNamespaces: string;
begin
  if not RequiresRTLNamespaces then
    Exit('');
  Result := 'System Vcl winapi Vcl.Imaging';
end;

function TBDSCompiler.GetIDString: string;
  {Provides a non-localisable string that identifies the compiler.
    @return Compiler id string.
  }
begin
  case GetID of
    ciD2005w32, ciD2006w32, ciD2009w32:
      Result := Format('D%dw32', [ProductVersion]);
    ciD2007, ciD2010:
      Result := Format('D%d', [ProductVersion]);
    ciDXE:
      Result := 'DXE';
    ciDXE2:
      Result := 'DXE2';
    ciDXE3:
      Result := 'DXE3';
    else raise EBug.Create(ClassName + '.GetIDString: Invalid ID');
  end;
end;

function TBDSCompiler.GetName: string;
  {Provides the human readable name of the compiler.
    @return Name of the compiler.
  }
resourcestring
  sCompilerName = 'Delphi %d';  // template for name of compiler
  sDelphiXE = 'Delphi XE';      // name of Delphi XE compiler
  sDelphiXE2 = 'Delphi XE2';    // name of Delphi XE2 compiler
  sDelphiXE3 = 'Delphi XE3';    // name of Delphi XE3 compiler
begin
  case GetID of
    ciDXE:
      Result := sDelphiXE;
    ciDXE2:
      Result := sDelphiXE2;
    ciDXE3:
      Result := sDelphiXE3;
    else
      Result := Format(sCompilerName, [ProductVersion]);
  end;
end;

function TBDSCompiler.GetRTLNamespaces: string;
begin
  if not RequiresRTLNamespaces then
    Exit('');
  Result := fRTLNamespaces;
end;

function TBDSCompiler.InstallationRegKey: string;
  {Returns name of registry key where records compiler's installation path
  is recorded.
    @return Name of key.
  }
begin
  case GetID of
    ciD2005w32: Result := '\SOFTWARE\Borland\BDS\3.0';
    ciD2006w32: Result := '\SOFTWARE\Borland\BDS\4.0';
    ciD2007   : Result := '\SOFTWARE\Borland\BDS\5.0';
    ciD2009w32: Result := '\SOFTWARE\CodeGear\BDS\6.0';
    ciD2010   : Result := '\SOFTWARE\CodeGear\BDS\7.0';
    ciDXE     : Result := '\Software\Embarcadero\BDS\8.0';
    ciDXE2    : Result := '\Software\Embarcadero\BDS\9.0';
    ciDXE3    : Result := '\Software\Embarcadero\BDS\10.0';
    else raise EBug.Create(ClassName + '.InstallationRegKey: Invalid ID');
  end;
end;

function TBDSCompiler.NamespaceParam: string;
var
  Namespaces: IStringList;
begin
  if not RequiresRTLNamespaces then
    Exit('');
  Namespaces := TIStringList.Create(fRTLNamespaces, ' ', False, True);
  if Namespaces.Count = 0 then
    Exit('');
  Result := '-NS' + Namespaces.GetText(';', False);
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
    ciD2010:    Result := 2010;
    ciDXE:      Result := 2011;
    ciDXE2:     Result := 2012;
    ciDXE3:     Result := 2013;
    else raise EBug.Create(ClassName + '.ProductVersion: Invalid ID');
  end;
end;

function TBDSCompiler.RequiresRTLNamespaces: Boolean;
begin
  Result := not (
    GetID in [ciD2005w32, ciD2006w32, ciD2007, ciD2009w32, ciD2010, ciDXE]
  );
end;

procedure TBDSCompiler.SetRTLNamespaces(const Namespaces: string);
begin
  if not RequiresRTLNamespaces then
    Exit;
  fRTLNamespaces := Namespaces;
end;

end.
