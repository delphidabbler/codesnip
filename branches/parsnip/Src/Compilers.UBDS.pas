{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2015, Peter Johnson (www.delphidabbler.com).
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
  ///  <summary>Class that controls and provides information about BDS based
  ///  Delphi compilers.</summary>
  TBDSCompiler = class(TBorlandCompiler,
    IClonable, ICompiler, ICompilerAutoDetect
  )
  strict private
    var
      ///  <summary>Space separated list of RTL namespaces to be passed to
      ///  compiler.</summary>
      fRTLNamespaces: string;
    ///  <summary>Returns major version number of compiler.</summary>
    function ProductVersion: Integer;
  strict protected
    ///  <summary>Returns name of registry key where compiler's installation
    ///  path is recorded.</summary>
    function InstallationRegKey: string; override;
    ///  <summary>Returns any namespace parameter to be passed to compiler on
    ///  command line.</summary>
    function NamespaceParam: string; override;

  public
    ///  <summary>Constructs new compiler instance for given compiler ID.
    ///  </summary>
    constructor Create(const Id: TCompilerID);

    ///  <summary>Returns reference to a new instance of this object that is an
    ///  exact copy of this instance.</summary>
    ///  <remarks>Method of IClonable.</remarks>
    function Clone: IInterface;

    ///  <summary>Returns the human readable name of the compiler.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function GetName: string; override;

    ///  <summary>Returns the compiler's unique ID.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function GetIDString: string; override;

    ///  <summary>Checks if the compiler has RTL unit names that are prefixed by
    ///  its namespace.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function RequiresRTLNamespaces: Boolean; override;

    ///  <summary>Returns a space separated list of the compiler's default RTL
    ///  unit namespaces.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function GetDefaultRTLNamespaces: string; override;

    ///  <summary>Returns a space separated list of user-defined RTL unit
    ///  namespaces to be searched by the compiler.</summary>
    ///  <remarks>Method of ICompiler.</remarks>
    function GetRTLNamespaces: string; override;

    ///  <summary>Records a list of user defined RTL unit namespaces to be
    ///  searched by the compiler.</summary>
    ///  <remarks>
    ///  <para>Namespaces is expected to be a space separated list of valid
    ///  Pascal identfiers.</para>
    ///  <para>Method of ICompiler.</para>
    ///  </remarks>
    procedure SetRTLNamespaces(const Namespaces: string); override;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions, UIStringList;


{ TBDSCompiler }

function TBDSCompiler.Clone: IInterface;
begin
  Result := TBDSCompiler.CreateCopy(Self);
end;

constructor TBDSCompiler.Create(const Id: TCompilerID);
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
begin
  case GetID of
    ciD2005, ciD2006, ciD2007, ciD2009, ciD2010:
      Result := Format('D%d', [ProductVersion]);
    ciDXE:
      Result := 'DXE';
    ciDXE2:
      Result := 'DXE2';
    ciDXE3:
      Result := 'DXE3';
    ciDXE4:
      Result := 'DXE4';
    ciDXE5:
      Result := 'DXE5';
    ciDXE6:
      Result := 'DXE6';
    ciDXE7:
      Result := 'DXE7';
    ciDXE8:
      Result := 'DXE8';
    ciD10S:
      Result := 'D10S';
    else raise EBug.Create(ClassName + '.GetIDString: Invalid ID');
  end;
end;

function TBDSCompiler.GetName: string;
resourcestring
  sCompilerName = 'Delphi %d';      // template for Delpi 2005-2010 names
  sDelphiXE = 'Delphi XE';          // name of Delphi XE compiler
  sDelphiXE2 = 'Delphi XE2';        // name of Delphi XE2 compiler
  sDelphiXE3 = 'Delphi XE3';        // name of Delphi XE3 compiler
  sDelphiXE4 = 'Delphi XE4';        // name of Delphi XE4 compiler
  sDelphiXE5 = 'Delphi XE5';        // name of Delphi XE5 compiler
  sDelphiXE6 = 'Delphi XE6';        // name of Delphi XE6 compiler
  sDelphiXE7 = 'Delphi XE7';        // name of Delphi XE7 compiler
  sDelphiXE8 = 'Delphi XE8';        // name of Delphi XE8 compiler
  sDelphi10S = 'Delphi 10 Seattle'; // name of Delphi 10 Seattle compiler
begin
  case GetID of
    ciDXE:
      Result := sDelphiXE;
    ciDXE2:
      Result := sDelphiXE2;
    ciDXE3:
      Result := sDelphiXE3;
    ciDXE4:
      Result := sDelphiXE4;
    ciDXE5:
      Result := sDelphiXE5;
    ciDXE6:
      Result := sDelphiXE6;
    ciDXE7:
      Result := sDelphiXE7;
    ciDXE8:
      Result := sDelphiXE8;
    ciD10S:
      Result := sDelphi10S;
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
begin
  case GetID of
    ciD2005: Result := '\SOFTWARE\Borland\BDS\3.0';
    ciD2006: Result := '\SOFTWARE\Borland\BDS\4.0';
    ciD2007: Result := '\SOFTWARE\Borland\BDS\5.0';
    ciD2009: Result := '\SOFTWARE\CodeGear\BDS\6.0';
    ciD2010: Result := '\SOFTWARE\CodeGear\BDS\7.0';
    ciDXE  : Result := '\Software\Embarcadero\BDS\8.0';
    ciDXE2 : Result := '\Software\Embarcadero\BDS\9.0';
    ciDXE3 : Result := '\Software\Embarcadero\BDS\10.0';
    ciDXE4 : Result := '\Software\Embarcadero\BDS\11.0';
    ciDXE5 : Result := '\Software\Embarcadero\BDS\12.0';
    ciDXE6 : Result := '\Software\Embarcadero\BDS\14.0';
    ciDXE7 : Result := '\Software\Embarcadero\BDS\15.0';
    ciDXE8 : Result := '\Software\Embarcadero\BDS\16.0';
    ciD10S : Result := '\Software\Embarcadero\BDS\17.0';
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
  if Namespaces.IsEmpty then
    Exit('');
  Result := '-NS' + Namespaces.GetText(';', False);
end;

function TBDSCompiler.ProductVersion: Integer;
begin
  case GetID of
    ciD2005: Result := 2005;
    ciD2006: Result := 2006;
    ciD2007: Result := 2007;
    ciD2009: Result := 2009;
    ciD2010: Result := 2010;
    else     Result := 0;      // not used for Delphi XE and later
  end;
end;

function TBDSCompiler.RequiresRTLNamespaces: Boolean;
begin
  Result := not (GetID in [ciD2005, ciD2006, ciD2007, ciD2009, ciD2010, ciDXE]);
end;

procedure TBDSCompiler.SetRTLNamespaces(const Namespaces: string);
begin
  if not RequiresRTLNamespaces then
    Exit;
  fRTLNamespaces := Namespaces;
end;

end.

