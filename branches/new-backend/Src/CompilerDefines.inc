{
 * CompilerDefines.inc
 *
 * Include file that defines various symbols used for conditional compilation.
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
 * The Original Code is CompilerDefines.inc
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}

{
  Supported symbols are:

  CanCompile
    Defined if compiler can be used to compile CodeSnip.
 }


{$UNDEF CanCompile}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 21.0} // Delphi 2010 and later
    {$DEFINE CanCompile}        // Minimum compiler is Delphi 2010.
  {$IFEND}
{$ENDIF}

{$IFNDEF CanCompile}
  {$MESSAGE FATAL 'CodeSnip requires Delphi 2010 or later to compile'}
{$ENDIF}

// Unlikely this test will fail since required compilers support: just here for
// completeness and ultimate safety!
{$IFNDEF UNICODE}
 {$MESSAGE FATAL 'CodeSnip requires Unicode support'}
{$ENDIF}
