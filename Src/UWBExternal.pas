{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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
  ///  <summary>COM object that implements the methods of the IWBExternal14
  ///  interface that extend the browser control's 'external' object.</summary>
  ///  <remarks>
  ///  <para>This class enables application code to be called from JavaScript
  ///  running in the browser control.</para>
  ///  <para>The methods a declared in the type library that is defined in
  ///  External.idl.</para>
  ///  </remarks>
  TWBExternal = class(TAutoIntfObject, IWBExternal14, ISetNotifier)
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

    ///  <summary>Displays the given snippet.</summary>
    ///  <param name="SnippetID">WideString [in] ID of snippet to be displayed.
    ///  </param>
    ///  <param name="NewTab">WordBool [in] Whether to display snippet in a new
    ///  tab.</param>
    ///  <remarks>Method of IWBExternal14.</remarks>
    procedure DisplaySnippet(const SnippetID: WideString; NewTab: WordBool);
      safecall;

    ///  <summary>Displays the Configure Compilers dialogue box.</summary>
    ///  <remarks>Method of IWBExternal14.</remarks>
    procedure ConfigCompilers; safecall;

    ///  <summary>Opens Snippet Editor ready to create a new snippet.</summary>
    ///  <remarks>Method of IWBExternal14.</remarks>
    procedure NewSnippet; safecall;

    ///  <summary>Displays the program's About Box.</summary>
    ///  <remarks>Method of IWBExternal14.</remarks>
    procedure ShowAboutBox; safecall;

    ///  <summary>Displays the given tag.</summary>
    ///  <param name="Tag">WideString [in] Tag to be displayed.</param>
    ///  <param name="NewTab">WordBool [in] Whether to display tag in a new tab.
    ///  </param>
    ///  <remarks>Method of IWBExternal14.</remarks>
    procedure DisplayTag(const Tag: WideString; NewTab: WordBool); safecall;

    ///  <summary>Removes a tag from a snippet's tag list.<summary>
    ///  <param name="SnippetID">WideString [in] ID of snippet.</param>
    ///  <param name="Tag">WideString [in] Tag to be removed.</param>
    ///  <remarks>Method of IWBExternal14.</remarks>
    procedure RemoveTag(const SnippetID: WideString; const Tag: WideString);
      safecall;

    ///  <summary>Displays the source code language with the given ID.</summary>
    ///  <param name="LangID">WideSting [in] ID of language to be displayed.
    ///  </param>
    ///  <param name="NewTab">WordBool [in] Whether to display language in a new
    ///  tab.</param>
    ///  <remarks>Method of IWBExternal14.</remarks>
    procedure DisplayLanguage(const LangID: WideString; NewTab: WordBool);
      safecall;

    ///  <summary>Sets the the Starred property of a snippet.</summary>
    ///  <param name="SnippetID">WideString [in] ID of snippet.</param>
    ///  <param name="State">WordBool [in] New star state: True if snippet is to
    ///  be starred, False if not.</param>
    procedure ChangeSnippetStar(const SnippetID: WideString;
      State: WordBool); safecall;

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
  CS.SourceCode.Languages,
  CS.Database.Types,
  UAppInfo;


{ TWBExternal }

procedure TWBExternal.ChangeSnippetStar(const SnippetID: WideString;
  State: WordBool);
begin
  try
    if Assigned(fNotifier) then
      fNotifier.ChangeSnippetStar(TSnippetID.Create(SnippetID), State);
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
  inherited Create(TypeLib, IWBExternal14);
end;

procedure TWBExternal.DisplayLanguage(const LangID: WideString;
  NewTab: WordBool);
begin
  try
    if Assigned(fNotifier) then
      fNotifier.DisplayLanguage(TSourceCodeLanguageID.Create(LangID), NewTab);
  except
    HandleException;
  end;
end;

procedure TWBExternal.DisplaySnippet(const SnippetID: WideString;
  NewTab: WordBool);
begin
  try
    if Assigned(fNotifier) then
      fNotifier.DisplaySnippet(TSnippetID.Create(SnippetID), NewTab);
  except
    HandleException;
  end;
end;

procedure TWBExternal.DisplayTag(const Tag: WideString; NewTab: WordBool);
begin
  try
    if Assigned(fNotifier) then
      fNotifier.DisplayTag(TTag.Create(Tag), NewTab);
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

procedure TWBExternal.RemoveTag(const SnippetID, Tag: WideString);
begin
  try
    if Assigned(fNotifier) then
      fNotifier.RemoveTag(TSnippetID.Create(SnippetID), TTag.Create(Tag));
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

end.

