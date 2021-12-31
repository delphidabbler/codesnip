{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
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
  Result := 'System Vcl winapi Vcl.Imaging System.Win';
end;

function TBDSCompiler.GetIDString: string;
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
    ciD101B:
      Result := 'D101B';
    ciD102T:
      Result := 'D102T';
    ciD103R:
      Result := 'D103R';
    ciD104S:
      Result := 'D104S';
    ciD11A:
      Result := 'D11A';
    else
      raise EBug.Create(ClassName + '.GetIDString: Invalid ID');
  end;
end;

function TBDSCompiler.GetName: string;
resourcestring
  // template for name of compiler with simple integer version number
  sCompilerName = 'Delphi %d';
  // names of compilers not suitable for use with template
  sDelphiXE = 'Delphi XE';
  sDelphiXE2 = 'Delphi XE2';
  sDelphiXE3 = 'Delphi XE3';
  sDelphiXE4 = 'Delphi XE4';
  sDelphiXE5 = 'Delphi XE5';
  sDelphiXE6 = 'Delphi XE6';
  sDelphiXE7 = 'Delphi XE7';
  sDelphiXE8 = 'Delphi XE8';
  sDelphi10S = 'Delphi 10 Seattle';
  sDelphi101B = 'Delphi 10.1 Berlin';
  sDelphi102T = 'Delphi 10.2 Tokyo';
  sDelphi103R = 'Delphi 10.3 Rio';
  sDelphi104S = 'Delphi 10.4 Sydney';
  sDelphi11A =  'Delphi 11 Alexandria';
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
    ciD101B:
      Result := sDelphi101B;
    ciD102T:
      Result := sDelphi102T;
    ciD103R:
      Result := sDelphi103R;
    ciD104S:
      Result := sDelphi104S;
    ciD11A:
      Result := sDelphi11A;
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
    ciD2005w32: Result := '\SOFTWARE\Borland\BDS\3.0';
    ciD2006w32: Result := '\SOFTWARE\Borland\BDS\4.0';
    ciD2007   : Result := '\SOFTWARE\Borland\BDS\5.0';
    ciD2009w32: Result := '\SOFTWARE\CodeGear\BDS\6.0';
    ciD2010   : Result := '\SOFTWARE\CodeGear\BDS\7.0';
    ciDXE     : Result := '\Software\Embarcadero\BDS\8.0';
    ciDXE2    : Result := '\Software\Embarcadero\BDS\9.0';
    ciDXE3    : Result := '\Software\Embarcadero\BDS\10.0';
    ciDXE4    : Result := '\Software\Embarcadero\BDS\11.0';
    ciDXE5    : Result := '\Software\Embarcadero\BDS\12.0';
    ciDXE6    : Result := '\Software\Embarcadero\BDS\14.0';
    ciDXE7    : Result := '\Software\Embarcadero\BDS\15.0';
    ciDXE8    : Result := '\Software\Embarcadero\BDS\16.0';
    ciD10S    : Result := '\Software\Embarcadero\BDS\17.0';
    ciD101B   : Result := '\Software\Embarcadero\BDS\18.0';
    ciD102T   : Result := '\Software\Embarcadero\BDS\19.0';
    ciD103R   : Result := '\Software\Embarcadero\BDS\20.0';
    ciD104S   : Result := '\Software\Embarcadero\BDS\21.0';
    ciD11A    : Result := '\Software\Embarcadero\BDS\22.0';
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
begin
  case GetID of
    ciD2005w32: Result := 2005;
    ciD2006w32: Result := 2006;
    ciD2007:    Result := 2007;
    ciD2009w32: Result := 2009;
    ciD2010:    Result := 2010;
    else        Result := 0;      // not used for Delphi XE and later
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

