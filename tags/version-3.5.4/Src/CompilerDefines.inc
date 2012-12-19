{
 * CompilerDefines.inc
 *
 * Include file that defines various symbols used for conditional compilation
 * depending on which compiler is being used.
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

  SupportsMainFormOnTaskBar
    Defined for compilers that have the TApplication.MainFormOnTaskBar property.
    When not defined the program needs to provide the correct Vista
    compatibility.

  SupportsUseLatestCommonDialogs
    Defined for compilers that support the UseLatestCommonDialogs global
    variable that is used to enable or disable Vista style common dialogs.

  AltBugFixNeeded
    Defined for compilers that exhibit the Alt Key Bug and need the program to
    provide a fix. (For details of the Alt Key Bug see Embarcadero Quality
    Central bug report #37403.)

  ThreadResumeDeprecated
    Defined for compilers where TThread.Resume is a deprecated method.
 }


{$UNDEF CanCompile}

{$UNDEF SupportsMainFormOnTaskBar}
{$DEFINE AltBugFixNeeded}
{$UNDEF ThreadResumeDeprecated}
{$UNDEF SupportsUseLatestCommonDialogs}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 18.0} // Delphi 2006 and later
    {$DEFINE CanCompile}
  {$IFEND}
  {$IF CompilerVersion >= 18.5} // Delphi 2007 and later
    {$DEFINE SupportsMainFormOnTaskBar}
    {$DEFINE SupportsUseLatestCommonDialogs}
    {$UNDEF AltBugFixNeeded}
  {$IFEND}
  {$IF CompilerVersion >= 21.0} // Delphi 2010 and later
    {$DEFINE ThreadResumeDeprecated}
  {$IFEND}
{$ENDIF}

{$IFNDEF CanCompile}
  {$MESSAGE FATAL 'CodeSnip requires Delphi 2006 or later to compile'}
{$ENDIF}