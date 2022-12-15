{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that can detect and register Delphi compilers
 * present on the user's system that are not yet registered with CodeSnip.
}


unit Compilers.UAutoDetect;

interface

uses
  Compilers.UGlobals,
  Compilers.UCompilers,
  UBaseObjects;

type
  TCompilerAutoDetect = class(TNoConstructObject)
  public
    type
      TCallback = reference to procedure (Compiler: ICompiler);
  strict private
    class procedure DoCallback(const Callback: TCallback;
      Compiler: ICompiler);
  public
    class procedure RegisterCompilers(Compilers: ICompilers;
      const Callback: TCallback = nil); overload;
    class procedure RegisterSpecificCompilers(AllCompilers: ICompilers;
      const RegList: TCompilerList; const Callback: TCallback = nil);
    class procedure ListRegisterableCompilers(Compilers: ICompilers;
      const Registerable: TCompilerList);
  end;

implementation

uses
  SysUtils;

{ TCompilerAutoDetect }

class procedure TCompilerAutoDetect.DoCallback(
  const Callback: TCallback; Compiler: ICompiler);
begin
  if Assigned(Callback) then
    Callback(Compiler);
end;

class procedure TCompilerAutoDetect.ListRegisterableCompilers(
  Compilers: ICompilers; const Registerable: TCompilerList);
var
  Compiler: ICompiler;
begin
  Registerable.Clear;
  for Compiler in Compilers do
  begin
    if not Supports(Compiler, ICompilerAutoDetect) then
      Continue; // compiler can't be auto-detected/registered
    if not (Compiler as ICompilerAutoDetect).IsInstalled then
      Continue; // compiler is not installed on the user's system
    if Compiler.IsAvailable then
      Continue; // compiler installed & already registered for use by CodeSnip
    if not (Compiler as ICompilerAutoDetect).GetCanAutoInstall then
      Continue; // user has excluded this compiler from being auto-registered
    // We get here then we have an installed, un-registered, auto-detectable
    // compiler with permission to register
    Registerable.Add(Compiler);
  end;
end;

class procedure TCompilerAutoDetect.RegisterSpecificCompilers(
  AllCompilers: ICompilers; const RegList: TCompilerList;
  const Callback: TCallback);
var
  Compiler: ICompiler;
begin
  for Compiler in AllCompilers do
  begin
    if RegList.IndexOf(Compiler) >= 0 then
    begin
      Assert(Supports(Compiler, ICompilerAutoDetect), ClassName +
        '.RegisterCompilers: Compiler does not support ICompilerAutoDetect');
      if (Compiler as ICompilerAutoDetect).DetectExeFile then
        DoCallback(Callback, Compiler);
    end;
  end;
end;

class procedure TCompilerAutoDetect.RegisterCompilers(Compilers: ICompilers;
  const Callback: TCallback);
var
  Registerable: TCompilerList;
  Compiler: ICompiler;
begin
  Registerable := TCompilerList.Create;
  try
    ListRegisterableCompilers(Compilers, Registerable);
    for Compiler in Registerable do
    begin
      Assert(Supports(Compiler, ICompilerAutoDetect), ClassName +
        '.RegisterCompilers: Compiler does not support ICompilerAutoDetect');
      if (Compiler as ICompilerAutoDetect).DetectExeFile then
        DoCallback(Callback, Compiler);
    end;
  finally
    Registerable.Free;
  end;
end;

end.

