{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Include file that defines various symbols used for conditional compilation
 * depending on which compiler is being used.
}

{
  Supported symbols are:

  CanCompile
    Defined if compiler can be used to compile CodeSnip. Compilation halts with
    a fatal error if this symbol is not defined.
    
  *** No other symbols are supported in this version of the file ***
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
