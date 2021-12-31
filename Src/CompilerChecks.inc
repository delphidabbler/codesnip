{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Include file that checks details of compiler to determine whether or not
 * CodeSnip can be compiled.
 *
 * CodeSnip can be complied only if the following conditions are met:
 *
 *   o Compiler is Delphi XE or later.
 *   o Target platform is Windows 32 bit.
 *   o Program is compiled as a native application, not .NET.
 *   o Program is not being compiled as a console application.
}

// Check for supported compiler
{$UNDEF SupportedCompiler}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 22.0} // Delphi XE and later
    {$DEFINE SupportedCompiler}
  {$IFEND}
{$ENDIF}
{$IFNDEF SupportedCompiler}
  {$MESSAGE FATAL 'CodeSnip requires Delphi XE or later to compile'}
{$ENDIF}

// Check for Windows 32 bit target platform
{$IFNDEF WIN32}
  {$MESSAGE FATAL 'CodeSnip must be built as a 32 bit Windows application'}
{$ENDIF}

// Check for .NET
{$IFDEF CLR}
  {$MESSAGE FATAL 'CodeSnip cannot be compiled as a .NET application'}
{$ENDIF}

// Check not being compiled as a console application
{$IFDEF CONSOLE}
  {$MESSAGE FATAL 'CodeSnip cannot be compiled as a console application'}
{$ENDIF}

