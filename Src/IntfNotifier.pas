{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines interfaces to set up and use the notifier object that triggers
 * actions in response to user initiated events in the GUI.
}


unit IntfNotifier;


interface


uses
  // Delphi
  Classes, ActiveX, Windows,
  // Project
  DB.UCollections,
  UView;


type
  ///  <summary>Interface that defines methods that trigger actions in response
  ///  to user intitiated events.</summary>
  INotifier = interface(IInterface)
    ['{13962DE4-784A-4B70-9D3F-FD434FAE4F4F}']

    ///  <summary>Requests a database update.</summary>
    procedure UpdateDbase;

//    ///  <summary>Displays a snippet.</summary>
//    ///  <param name="SnippetName">WideString [in] Name of required snippet.
//    ///  </param>
//    ///  <param name="UserDefined">WordBool [in] Indicates whether snippet is
//    ///  user defined.</param>
//    ///  <param name="NewTab">WordBool [in] Whether to display snippet in a new
//    ///  detail pane tab.</param>
//    procedure DisplaySnippet(const SnippetName: WideString;
//      UserDefined: WordBool; NewTab: WordBool); overload;

    ///  <summary>Displays a snippet.</summary>
    ///  <param name="SnippetName">WideString [in] Name of required snippet.
    ///  </param>
    ///  <param name="ACollectionID">TCollectionID [in] ID of the snippet's
    ///  collection.</param>
    ///  <param name="NewTab">WordBool [in] Whether to display snippet in a new
    ///  detail pane tab.</param>
    procedure DisplaySnippet(const SnippetName: WideString;
      ACollectionID: TCollectionID; NewTab: WordBool);

    ///  <summary>Displays a category.</summary>
    ///  <param name="CatId">WideString [in] ID of required category.</param>
    ///  <param name="NewTab">WordBool [in] Whether to display category in a new
    ///  detail pane tab.</param>
    procedure DisplayCategory(const CatID: WideString; NewTab: WordBool);

    ///  <summary>Displays Configure Compilers dialogue box.</summary>
    procedure ConfigCompilers;

    ///  <summary>Displays a view item.</summary>
    ///  <param name="View">IView [in] Required view item.</param>
    ///  <param name="NewTab">Boolean [in] Whether to display view item in a new
    ///  detail pane tab.</param>
    procedure ShowViewItem(View: IView; const NewTab: Boolean);

    ///  <summary>Changes display style of overview pane.</summary>
    ///  <param name="Style">Integer [in] Required display style.</param>
    ///  <remarks>Style is index of an overview pane tab.</remarks>
    procedure ChangeOverviewStyle(const Style: Integer);

    ///  <summary>Changes displayed pane in detail display area.</summary>
    ///  <param name="Pane">Integer [in] Index of required pane.</param>
    procedure ChangeDetailPane(const Pane: Integer);

    ///  <summary>Edits a snippet in Snippets Editor.</summary>
    ///  <param name="SnippetName">WideString [in] Name of snippet.</param>
    ///  <remarks>Snippet must be user defined.</remarks>
    procedure EditSnippet(const SnippetName: WideString);

    ///  <summary>Opens Snippets Editor ready to create a new snippet.</summary>
    procedure NewSnippet;

    ///  <summary>Displays news items from the CodeSnip news feed.</summary>
    procedure ShowNews;

    ///  <summary>Displays the program's About Box.</summary>
    procedure ShowAboutBox;

  end;

type
  ///  <summary>Interface that defines methods for associating action objects
  ///  with the methods of INotifier.</summary>
  ///  <remarks>Any object that implements INotifier must also implement this
  ///  interface.</remarks>
  ISetActions = interface(IInterface)
    ['{A4B7AFE2-EE6C-4D39-BEA6-B52CC8AAC1DE}']

    ///  <summary>Sets action used to request a database update.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    procedure SetUpdateDbaseAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display a snippet.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    procedure SetDisplaySnippetAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display Configure Compilers dialogue
    ///  box.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    procedure SetConfigCompilersAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display a view item.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    procedure SetShowViewItemAction(const Action: TBasicAction);

    ///  <summary>Sets actions used to change display style of overview pane.
    ///  </summary>
    ///  <param name="Actions">array of TBasicAction [in] Array of required
    ///  actions.</param>
    ///  <remarks>Actions array must have one action for each supported display
    ///  style.</remarks>
    procedure SetOverviewStyleChangeActions(
      const Actions: array of TBasicAction);

    ///  <summary>Sets action used to change displayed pane in detail display
    ///  area.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    procedure SetDetailPaneChangeAction(const Action: TBasicAction);

    ///  <summary>Sets action used to edit a snippet in Snippets Editor.
    ///  </summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    procedure SetEditSnippetAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display a category.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    procedure SetDisplayCategoryAction(const Action: TBasicAction);

    ///  <summary>Sets action used to open snippets editor to create a new
    ///  snippet.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    procedure SetNewSnippetAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display news items from the CodeSnip news
    ///  feed.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    procedure SetNewsAction(const Action: TBasicAction);

    ///  <summary>Sets action used to display the program's About Box.</summary>
    ///  <param name="Action">TBasicAction [in] Required action.</param>
    procedure SetAboutBoxAction(const Action: TBasicAction);

  end;

type
  ///  <summary>Interface that provides a method used to assign a notifier
  ///  object to an object that needs to notify the application of events.
  ///  </summary>
  ///  <remarks>The interface should be supported by all UI objects other than
  ///  menus and buttons that initiate events. Such objects must call relevant
  ///  notifier methods to trigger the events.</remarks>
  ISetNotifier = interface(IInterface)
    ['{83283DBB-A8E3-42CA-9840-B7E2AC4BC79A}']
    ///  <summary>Sets the implementing object's notifier object.</summary>
    ///  <param name="Notifier">INotifier [in] Required notifier object.</param>
    procedure SetNotifier(const Notifier: INotifier);
  end;


implementation

end.

