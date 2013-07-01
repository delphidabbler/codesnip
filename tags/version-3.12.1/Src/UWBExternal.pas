{
 * UWBExternal.pas
 *
 * COM object that extends the "external" object accessible from JavaScript in a
 * browser control and notifies application of events triggered via JavaScript
 * calls to the external object's methods.
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
 * The Original Code is UWBExternal.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
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
    IWBExternal6, // browser external object's methods
    ISetNotifier  // sets object used to notify app of events
    )
  strict private
    fNotifier: INotifier; // Notifies application of events triggered by user
    procedure HandleException;
      {Gets application to handle current exception.
      }
  protected // do not make strict
    { IWBExternal5: defined in type library }
    procedure UpdateDbase; safecall;
      {Updates database from internet.
      }
    procedure DisplaySnippet(const SnippetName: WideString;
      UserDefined: WordBool); safecall;
      {Displays a named snippet.
        @param SnippetName [in] Name of snippet to display.
        @param UserDefined [in] Whether snippet is user defined.
      }
    procedure CompileSnippet; safecall;
      {Compiles the current snippet via notifier.
      }
    procedure ViewCompilerLog(Ver: SYSINT); safecall;
      {Displays a compiler log.
        @param Ver [in] Version of Delphi for which we need to display log. Ver
          is the ordinal value of the required compiler version enumerated type.
      }
    procedure ShowHint(const Hint: WideString); safecall;
      {Displays a hint.
        @param Hint [in] Hint to be displayed.
      }
    procedure ConfigCompilers; safecall;
      {Displays the Configure Compilers dialog box.
      }
    procedure ShowTestUnit; safecall;
      {Displays test unit for current snippet.
      }
    procedure EditSnippet(const SnippetName: WideString); safecall;
      {Edits a named snippet.
        @param SnippetName [in] Name of snippet to be edited. Must be a user
          defined snippet.
      }
    procedure Donate; safecall;
      {Displays the Donate dialog box.
      }
    procedure DisplayCategory(const CatID: WideString); safecall;
      {Displays a category.
        @param CatID [in] ID of category to display.
      }
    { ISetNotifier }
    procedure SetNotifier(const Notifier: INotifier);
      {Records the notifier object that is called in response to user input.
        @param Notifier [in] Notifier object.
      }
  public
    constructor Create;
      {Class constructor. Sets up object using embedded type library.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Forms,
  // Project
  UAppInfo;


{ TWBExternal }

procedure TWBExternal.CompileSnippet;
  {Compiles the current snippet.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.CompileRoutine;
  except
    HandleException;
  end;
end;

procedure TWBExternal.ConfigCompilers;
  {Displays the Configure Compilers dialog box.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.ConfigCompilers;
  except
    HandleException;
  end;
end;

constructor TWBExternal.Create;
  {Class constructor. Sets up object using embedded type library.
  }
var
  TypeLib: ITypeLib;    // type library
  ExeName: WideString;  // name of this executable file
begin
  // Get type library info from exe file
  ExeName := TAppInfo.AppExeFilePath;
  OleCheck(LoadTypeLib(PWideChar(ExeName), TypeLib));
  // Create the object using type library
  inherited Create(TypeLib, IWBExternal6);
end;

procedure TWBExternal.DisplayCategory(const CatID: WideString);
  {Displays a category.
    @param CatID [in] ID of category to display.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.DisplayCategory(CatID);
  except
    HandleException;
  end;
end;

procedure TWBExternal.DisplaySnippet(const SnippetName: WideString;
  UserDefined: WordBool);
  {Displays a named snippet.
    @param SnippetName [in] Name of snippet to display.
    @param UserDefined [in] Whether snippet is user defined.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.DisplayRoutine(SnippetName, UserDefined);
  except
    HandleException;
  end;
end;

procedure TWBExternal.Donate;
  {Displays the Donate dialog box.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.Donate;
  except
    HandleException;
  end;
end;

procedure TWBExternal.EditSnippet(const SnippetName: WideString);
  {Edits a named snippet.
    @param SnippetName [in] Name of snippet to be edited. Must be a user defined
      snippet.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.EditRoutine(SnippetName);
  except
    HandleException;
  end;
end;

procedure TWBExternal.HandleException;
  {Gets application to handle current exception.
  }
begin
  Application.HandleException(ExceptObject);
end;

procedure TWBExternal.SetNotifier(const Notifier: INotifier);
  {Records the notifier object that is called in response to user input.
    @param Notifier [in] Notifier object.
  }
begin
  fNotifier := Notifier;
end;

procedure TWBExternal.ShowHint(const Hint: WideString);
  {Displays a hint.
    @param Hint [in] Hint to be displayed.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.ShowHint(Hint);
  except
    HandleException;
  end;
end;

procedure TWBExternal.ShowTestUnit;
  {Displays test unit for current snippet.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.ShowTestUnit;
  except
    HandleException;
  end;
end;

procedure TWBExternal.UpdateDbase;
  {Updates database from internet.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.UpdateDbase;
  except
    HandleException;
  end;
end;

procedure TWBExternal.ViewCompilerLog(Ver: SYSINT);
  {Displays a compiler log.
    @param Ver [in] Version of Delphi for which we need to display log. Ver is
      the ordinal value of the required compiler version enumerated type.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.ViewCompilerLog(Ver);
  except
    HandleException;
  end;
end;

end.

