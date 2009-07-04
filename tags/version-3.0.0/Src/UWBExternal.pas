{
 * UWBExternal.pas
 *
 * COM object that extends the "external" object accessible from JavaScript in a
 * browser control and notifies application of events triggered via JavaScript
 * calls to the external object's methods.
 *
 * v0.1 of 22 Feb 2005  - Original version. Replaced former UExtInfo and
 *                        UExtCompCheck units that provided different external
 *                        object extensions for different browser controls.
 * v0.2 of 25 Apr 2005  - Implemented new HelpTopic and ConfigCompilers methods
 *                        of IWBExternal by calling methods of same names in
 *                        notifier object.
 *                      - Deleted implementation QuickStartHelp method that was
 *                        removed from IWBExternal.
 *                      - Added new InstalledCompilers method of IWBExternal
 *                        that returns number of available compilers to
 *                        JavaScript.
 * v0.3 of 03 Jun 2005  - Deleted InstalledCompilers method since no longer
 *                        required.
 * v1.0 of 05 Jun 2006  - Improved and corrected comments.
 *                      - Relocated and rationalised $WARN directives.
 *                      - Removed unused unit reference.
 *                      - Changed to get path to executable file from TAppInfo
 *                        static class rather than direct.
 *                      - Changed to use renamed IntfExternalObj unit (was
 *                        previously CodeSnip_TLB).
 * v1.1 of 23 Nov 2006  - Added ToggleTestUnit method and implemented by calling
 *                        equivalent method in notifier object.
 *                      - Changed to support IWBExternal2 instead of
 *                        IWBExternal.
 * v1.2 of 04 Nov 2007  - Changed to implement IWBExternal3 instead of
 *                        IWBExternal2 and removed HelpTopic method as
 *                        consequence.
 * v1.3 of 14 Sep 2008  - Changed to implement IWBExternal4 instead of
 *                        IWBExternal3:
 *                        - Changed signature of DisplayRoutine and modified the
 *                          implementation to call the equivalent modified
 *                          notifier method.
 *                        - Added new EditRoutine method that calls method of
 *                          same name in Notifier.
 * v1.4 of 17 Jun 2009  - Changed to implement IWBExternal5 instead of
 *                        IWBExternal4:
 *                        - ShowTestUnit method replaces ToggleTestUnit.
 *                        - Donate method added.
 *                        - Renamed DisplayRoutine method as DisplaySnippet,
 *                          CompileRoutine as CompileSnippet, EditRoutine as
 *                          EditSnippet.
 *                      - Made private section strict.
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
 * The Original Code is UWBExternal.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UWBExternal;


interface


uses
  // Delphi
  ComObj, ActiveX,
  // Auto-generated project unit
  IntfExternalObj, // derived indirectly from ExternalObj.idl
  // Project
  IntfNotifier;


type

  {
  TWBExternal:
    COM object that extends the "external" object accessible from JavaScript in
    a web browser and notifies application of events triggered via JavaScript
    calls to the external object's methods. Uses notification object to pass
    events to application.
  }
  TWBExternal = class(TAutoIntfObject,
    IWBExternal5,           // browser external object's methods
    ISetNotifier            // sets object used to notify app of events
    )
  strict private
    fNotifier: INotifier; // Notifies application of events triggered by user
  protected // do not make strict
    { IWBExternal5: defined in type library }
    procedure UpdateDbase; safecall;
      {Updates database from internet via notifier.
      }
    procedure DisplaySnippet(const SnippetName: WideString;
      UserDefined: WordBool); safecall;
      {Displays the named snippet via notifier.
        @param SnippetName [in] Name of snippet to display.
        @param UserDefined [in] Whether snippet is user defined.
      }
    procedure CompileSnippet; safecall;
      {Compiles the current snippet via notifier.
      }
    procedure ViewCompilerLog(Ver: SYSINT); safecall;
      {Displays a compiler log via notifier.
        @param Ver [in] Version of Delphi for which we need to display log. Ver
          is the ordinal value of the required compiler version enumerated type.
      }
    procedure ShowHint(const Hint: WideString); safecall;
      {Displays the given hint via notifier.
        @param Hint [in] Hint to be displayed.
      }
    procedure ConfigCompilers; safecall;
      {Displays configure compilers dialog box via notifier.
      }
    procedure ShowTestUnit; safecall;
      {Display the test unit for current snippet.
      }
    procedure EditSnippet(const SnippetName: WideString); safecall;
      {Edits the named snippet.
        @param SnippetName [in] Name of snippet to be edited. Must be a user
          defined snippet.
      }
    procedure Donate; safecall;
      {Displays the Donate dialog box via notifier.
      }
    { ISetNotifier }
    procedure SetNotifier(const Notifier: INotifier);
      {Sets the object's notifier object that is called in response to user
      input.
        @param Notifier [in] Notifier object.
      }
  public
    constructor Create;
      {Class constructor. Sets up object using embedded type library.
      }
  end;


implementation


uses
  // Project
  UAppInfo, UUtils;


{ TWBExternal }

procedure TWBExternal.CompileSnippet;
  {Compiles the current snippet via notifier.
  }
begin
  if Assigned(fNotifier) then
    fNotifier.CompileRoutine;
end;

procedure TWBExternal.ConfigCompilers;
  {Displays configure compilers dialog box via notifier.
  }
begin
  if Assigned(fNotifier) then
    fNotifier.ConfigCompilers;
end;

constructor TWBExternal.Create;
  {Class constructor. Sets up object using embedded type library.
  }
var
  TypeLib: ITypeLib;    // type library
  ExeName: WideString;  // name of this executable file
begin
  // Get type library info from exe file
  ExeName := DirToPath(TAppInfo.AppExeDir) + TAppInfo.AppExeFile;
  OleCheck(LoadTypeLib(PWideChar(ExeName), TypeLib));
  // Create the object using type library
  inherited Create(TypeLib, IWBExternal5);
end;

procedure TWBExternal.DisplaySnippet(const SnippetName: WideString;
  UserDefined: WordBool);
  {Displays the named snippet via notifier.
    @param SnippetName [in] Name of snippet to display.
    @param UserDefined [in] Whether snippet is user defined.
  }
begin
  if Assigned(fNotifier) then
    fNotifier.DisplayRoutine(SnippetName, UserDefined);
end;

procedure TWBExternal.Donate;
  {Displays the Donate dialog box via notifier.
  }
begin
  if Assigned(fNotifier) then
    fNotifier.Donate;
end;

procedure TWBExternal.EditSnippet(const SnippetName: WideString);
  {Edits the named snippet.
    @param SnippetName [in] Name of snippet to be edited. Must be a user defined
      snippet.
  }
begin
  if Assigned(fNotifier) then
    fNotifier.EditRoutine(SnippetName);
end;

procedure TWBExternal.SetNotifier(const Notifier: INotifier);
  {Sets the object's notifier object that is called in response to user input.
    @param Notifier [in] Notifier object.
  }
begin
  fNotifier := Notifier;
end;

procedure TWBExternal.ShowHint(const Hint: WideString);
  {Displays the given hint via notifier.
    @param Hint [in] Hint to be displayed.
  }
begin
  if Assigned(fNotifier) then
    fNotifier.ShowHint(Hint);
end;

procedure TWBExternal.ShowTestUnit;
  {Display the test unit for current snippet.
  }
begin
  if Assigned(fNotifier) then
    fNotifier.ShowTestUnit;
end;

procedure TWBExternal.UpdateDbase;
  {Updates database from internet via notifier.
  }
begin
  if Assigned(fNotifier) then
    fNotifier.UpdateDbase;
end;

procedure TWBExternal.ViewCompilerLog(Ver: SYSINT);
  {Displays a compiler log via notifier.
    @param Ver [in] Version of Delphi for which we need to display log. Ver is
      the ordinal value of the required compiler version enumerated type.
  }
begin
  if Assigned(fNotifier) then
    fNotifier.ViewCompilerLog(Ver);
end;

end.

