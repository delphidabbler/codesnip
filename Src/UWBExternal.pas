{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * COM object that extends the "external" object accessible from JavaScript in a
 * browser control and notifies application of events triggered via JavaScript
 * calls to the external object's methods.
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
    IWBExternal9, // browser external object's methods
    ISetNotifier  // sets object used to notify app of events
    )
  strict private
    fNotifier: INotifier; // Notifies application of events triggered by user
    procedure HandleException;
      {Gets application to handle current exception.
      }
  protected // do not make strict
    { IWBExternal8: defined in type library }
    procedure UpdateDbase; safecall;
      {Updates database from internet.
      }
    procedure DisplaySnippet(const SnippetName: WideString;
      UserDefined: WordBool; NewTab: WordBool); safecall;
      {Displays a named snippet.
        @param SnippetName [in] Name of snippet to display.
        @param UserDefined [in] Whether snippet is user defined.
        @param NewTab [in] Whether to display in new tab in detail pane.
      }
    procedure ShowHint(const Hint: WideString); safecall;
      {Displays a hint.
        @param Hint [in] Hint to be displayed.
      }
    procedure ConfigCompilers; safecall;
      {Displays the Configure Compilers dialog box.
      }
    procedure EditSnippet(const SnippetName: WideString); safecall;
      {Edits a named snippet.
        @param SnippetName [in] Name of snippet to be edited. Must be a user
          defined snippet.
      }
    procedure Donate; safecall;
      {Displays the Donate dialog box.
      }
    procedure DisplayCategory(const CatID: WideString; NewTab: WordBool);
      safecall;
      {Displays a category.
        @param CatID [in] ID of category to display.
        @param NewTab [in] Whether to display in new tab in detail pane.
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
  Forms,
  // Project
  UAppInfo;


{ TWBExternal }

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
  inherited Create(TypeLib, IWBExternal9);
end;

procedure TWBExternal.DisplayCategory(const CatID: WideString;
  NewTab: WordBool);
  {Displays a category.
    @param CatID [in] ID of category to display.
    @param NewTab [in] Whether to display in new tab in detail pane.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.DisplayCategory(CatID, NewTab);
  except
    HandleException;
  end;
end;

procedure TWBExternal.DisplaySnippet(const SnippetName: WideString;
  UserDefined: WordBool; NewTab: WordBool);
  {Displays a named snippet.
    @param SnippetName [in] Name of snippet to display.
    @param UserDefined [in] Whether snippet is user defined.
    @param NewTab [in] Whether to display in new tab in detail pane.
  }
begin
  try
    if Assigned(fNotifier) then
      fNotifier.DisplaySnippet(SnippetName, UserDefined, NewTab);
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
      fNotifier.EditSnippet(SnippetName);
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

end.

