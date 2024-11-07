{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
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
  ///  <summary>COM object that implements the methods of the IWBExternal15
  ///  interface that extend the browser control's 'external' object.</summary>
  ///  <remarks>
  ///  <para>This class enables application code to be called from JavaScript
  ///  running in the browser control.</para>
  ///  <para>The methods a declared in the type library that is defined in
  ///  External.idl.</para>
  ///  </remarks>
  TWBExternal = class(TAutoIntfObject, IWBExternal15, ISetNotifier)
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
    ///  <remarks>Method of IWBExternal15.</remarks>
    procedure UpdateDbase; safecall;

    ///  <summary>Display snippet identified by key and collection ID.</summary>
    ///  <param name="Key">WideString [in] Snippet's key.</param>
    ///  <param name="CollectionIDAsHex">WideString [in] Hex representation of
    ///  snippet's collection ID.</param>
    ///  <param name="NewTab">WordBool [in] Whether to display snippet in a new
    ///  tab.</param>
    ///  <remarks>Method of IWBExternal15.</remarks>
    procedure DisplaySnippet(const Key: WideString;
      const CollectionIDAsHex: WideString; NewTab: WordBool); safecall;

    ///  <summary>Displays the Configure Compilers dialogue box.</summary>
    ///  <remarks>Method of IWBExternal15.</remarks>
    procedure ConfigCompilers; safecall;

    ///  <summary>Edits the snippet identified by its key.</summary>
    ///  <param name="Key">WideString [in] Key of snippet to edit.</param>
    ///  <remarks>
    ///  <para>The snippet must be user defined.</para>
    ///  <para>Method of IWBExternal15.</para>
    ///  </remarks>
    procedure EditSnippet(const Key: WideString); safecall;

    ///  <summary>Displays a named category.</summary>
    ///  <param name="CatID">WideString [in] ID of category to be displayed.
    ///  </param>
    ///  <param name="NewTab">WordBool [in] Whether to display category in a new
    ///  tab.</param>
    ///  <remarks>Method of IWBExternal15.</remarks>
    procedure DisplayCategory(const CatID: WideString; NewTab: WordBool);
      safecall;

    ///  <summary>Opens Snippet Editor ready to create a new snippet.</summary>
    ///  <remarks>Method of IWBExternal15.</remarks>
    procedure NewSnippet; safecall;

    ///  <summary>Shows latest news items from CodeSnip news feed.</summary>
    ///  <remarks>Method of IWBExternal15.</remarks>
    procedure ShowNews; safecall;

    ///  <summary>Displays the program's About Box.</summary>
    ///  <remarks>Method of IWBExternal15.</remarks>
    procedure ShowAboutBox; safecall;

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
  DB.UCollections,
  UAppInfo;


{ TWBExternal }

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
  inherited Create(TypeLib, IWBExternal15);
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

procedure TWBExternal.DisplaySnippet(const Key, CollectionIDAsHex: WideString;
  NewTab: WordBool);
begin
  try
    if Assigned(fNotifier) then
      fNotifier.DisplaySnippet(
        Key, TCollectionID.CreateFromHexString(CollectionIDAsHex), NewTab
      );
  except
    HandleException;
  end;
end;

procedure TWBExternal.EditSnippet(const Key: WideString);
  {TODO -cVault: change to take a collection ID as hex string as 2nd param &
          lift restriction on having to be user defined.}
begin
  try
    if Assigned(fNotifier) then
      fNotifier.EditSnippet(Key);
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

