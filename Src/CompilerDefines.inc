{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Include file that defines symbols used for conditional compilation and checks
 * defined symbols to determine if CodeSnip can be compiled.
}

{
  Explanation of symbols:

  CanCompile
    Defined if compiler can be used to compile CodeSnip. Compilation halts with
    a fatal error if this symbol is not defined. This is a custom symbol.
    
  WIN32
    Defined only if target platform is 32 bit Windows. This is a predefined
    symbol.
  
  CONSOLE
    Defined if program is being compiled as a console application. This is a
    predefined symbol.
 }

{$UNDEF CanCompile}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 21.0} // Delphi 2010 and later
    {$DEFINE CanCompile}
  {$IFEND}
{$ENDIF}

{$IFNDEF CanCompile}
  {$MESSAGE FATAL 'CodeSnip requires Delphi 2010 or later to compile'}
{$ENDIF}

{$IFNDEF WIN32}
  {$MESSAGE FATAL 'CodeSnip must be built as a 32 bit Windows application'}
{$ENDIF}

{$IFDEF CONSOLE}
  {$MESSAGE FATAL 'CodeSnip cannot be compiled as a console application'}
{$ENDIF}
