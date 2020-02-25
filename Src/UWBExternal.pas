{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a COM object that extends a browser control's "external" object
 * enabling application code to be called from JavaScript.
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
  ///  <summary>COM object that implements the methods of the IWBExternal12
  ///  interface that extend the browser control's 'external' object.</summary>
  ///  <remarks>
  ///  <para>This class enables application code to be called from JavaScript
  ///  running in the browser control.</para>
  ///  <para>The methods a declared in the type library that is defined in
  ///  External.idl.</para>
  ///  </remarks>
  TWBExternal = class(TAutoIntfObject, IWBExternal12, ISetNotifier)
  strict private
    var
      ///  <summary>Object used to call application code in response to
      ///  JavaScript calls.</summary>
      fNotifier: INotifier;

    ///  <summary>Handles exceptions raised by getting the application object to
    ///  handle.</summary>
    ///  <remarks>Exceptions must be handle by application objects to avoid them
    ///  causing browser error messages.</remarks>
    procedure HandleException;

  public

    ///  <summary>Constructs a new object instance and connects it to the type
    ///  library.</summary>
    constructor Create;

    ///  <summary>Updates database from internet.</summary>
    ///  <remarks>Method of IWBExternal12.</remarks>
    procedure UpdateDbase; safecall;

    ///  <summary>Displays a named snippet.</summary>
    ///  <param name="SnippetName">WideString [in] Name of snippet to be
    ///  displayed.</param>
    ///  <param name="UserDefined">WordBool [in] Whether the snippet is user
    ///  defined.</param>
    ///  <param name="NewTab">WordBool [in] Whether to display snippet in a new
    ///  tab.</param>
    ///  <remarks>Method of IWBExternal12.</remarks>
    procedure DisplaySnippet(const SnippetName: WideString;
      UserDefined: WordBool; NewTab: WordBool); safecall;

    ///  <summary>Displays the Configure Compilers dialogue box.</summary>
    ///  <remarks>Method of IWBExternal12.</remarks>
    procedure ConfigCompilers; safecall;

    ///  <summary>Edits a named snippet.</summary>
    ///  <param name="SnippetName">WideString [in] Name of snippet to be edited.
    ///  </param>
    ///  <remarks>
    ///  <para>The named snippet must be user defined.</para>
    ///  <para>Method of IWBExternal12.</para>
    ///  </remarks>
    procedure EditSnippet(const SnippetName: WideString); safecall;

    ///  <summary>Displays the Donate dialogue box.</summary>
    ///  <remarks>Method of IWBExternal12.</remarks>
    procedure Donate; safecall;

    ///  <summary>Displays a named category.</summary>
    ///  <param name="CatID">WideString [in] ID of category to be displayed.
    ///  </param>
    ///  <param name="NewTab">WordBool [in] Whether to display category in a new
    ///  tab.</param>
    ///  <remarks>Method of IWBExternal12.</remarks>
    procedure DisplayCategory(const CatID: WideString; NewTab: WordBool);
      safecall;

    ///  <summary>Opens Snippet Editor ready to create a new snippet.</summary>
    ///  <remarks>Method of IWBExternal12.</remarks>
    procedure NewSnippet; safecall;

    ///  <summary>Shows latest news items from CodeSnip news feed.</summary>
    ///  <remarks>Method of IWBExternal12.</remarks>
    procedure ShowNews; safecall;

    ///  <summary>Checks for program updates.</summary>
    ///  <remarks>Method of IWBExternal12.</remarks>
    procedure CheckForUpdates; safecall;

    ///  <summary>Displays the program's About Box.</summary>
    ///  <remarks>Method of IWBExternal12.</remarks>
    procedure ShowAboutBox; safecall;

    ///  <summary>Displays specified page of the Preferences dialogue.</summary>
    ///  <param name="ClsName">WideString [in] Class name of the frame that
    ///  implements the required preferences page.</param>
    ///  <remarks>Method of IWBExternal12.</remarks>
    procedure ShowPrefsPage(const ClsName: WideString); safecall;

    ///  <summary>Records the notifier object that is used to call application
    ///  code in response to JavaScript calls running in browser documents.
    ///  </summary>
    ///  <param name="Notifier">INotifier [in] The notifier object.</param>
    ///  <remarks>Method of ISetNotifier.</remarks>
    procedure SetNotifier(const Notifier: INotifier);
  end;


implementation


uses
  // Delphi
  Forms,
  // Project
  UAppInfo;


{ TWBExternal }

procedure TWBExternal.CheckForUpdates;
begin
  try
    if Assigned(fNotifier) then
      fNotifier.CheckForUpdates;
  except
    HandleException;
  end;
end;

procedure TWBExternal.ConfigCompilers;
begin
  try
    if Assigned(fNotifier) then
      fNotifier.ConfigCompilers;
  except
    HandleException;
  end;
end;

constructor TWBExternal.Create;
var
  TypeLib: ITypeLib;    // type library
  ExeName: WideString;  // name of this executable file
begin
  // Get type library info from exe file
  ExeName := TAppInfo.AppExeFilePath;
  OleCheck(LoadTypeLib(PWideChar(ExeName), TypeLib));
  // Create the object using type library
  inherited Create(TypeLib, IWBExternal12);
end;

procedure TWBExternal.DisplayCategory(const CatID: WideString;
  NewTab: WordBool);
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
begin
  try
    if Assigned(fNotifier) then
      fNotifier.DisplaySnippet(SnippetName, UserDefined, NewTab);
  except
    HandleException;
  end;
end;

procedure TWBExternal.Donate;
begin
  try
    if Assigned(fNotifier) then
      fNotifier.Donate;
  except
    HandleException;
  end;
end;

procedure TWBExternal.EditSnippet(const SnippetName: WideString);
begin
  try
    if Assigned(fNotifier) then
      fNotifier.EditSnippet(SnippetName);
  except
    HandleException;
  end;
end;

procedure TWBExternal.HandleException;
begin
  Application.HandleException(ExceptObject);
end;

procedure TWBExternal.NewSnippet;
begin
  try
    if Assigned(fNotifier) then
      fNotifier.NewSnippet;
  except
    HandleException;
  end;
end;

procedure TWBExternal.SetNotifier(const Notifier: INotifier);
begin
  fNotifier := Notifier;
end;

procedure TWBExternal.ShowAboutBox;
begin
  try
    if Assigned(fNotifier) then
      fNotifier.ShowAboutBox;
  except
    HandleException;
  end;
end;

procedure TWBExternal.ShowNews;
begin
  try
    if Assigned(fNotifier) then
      fNotifier.ShowNews;
  except
    HandleException;
  end;
end;

procedure TWBExternal.ShowPrefsPage(const ClsName: WideString); safecall;
begin
  try
    if Assigned(fNotifier) then
      fNotifier.ShowPrefsPage(ClsName);
  except
    HandleException;
  end;
end;

procedure TWBExternal.UpdateDbase;
begin
  try
    if Assigned(fNotifier) then
      fNotifier.UpdateDbase;
  except
    HandleException;
  end;
end;

end.

