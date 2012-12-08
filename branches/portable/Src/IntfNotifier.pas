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
 * Defines interfaces to set up and use the notifier object that triggers
 * actions in response to user initiated events in the GUI.
}


unit IntfNotifier;


interface


uses
  // Delphi
  Classes, ActiveX, Windows,
  // Project
  UView;


type

  {
  INotifier:
    Interface that defines methods that trigger actions in response to user
    intitiated events.
  }
  INotifier = interface(IInterface)
    ['{13962DE4-784A-4B70-9D3F-FD434FAE4F4F}']
    procedure UpdateDbase;
      {Updates database.
      }
    procedure DisplaySnippet(const SnippetName: WideString;
      UserDefined: WordBool; NewTab: WordBool);
      {Displays a named snippey.
        @param SnippetName [in] Name of snippet to display.
        @param UserDefined [in] Whether snippet is user defined.
        @param NewTab [in] Whether to display in new tab in detail pane.
      }
    procedure DisplayCategory(const CatID: WideString; NewTab: WordBool);
      {Displays an identified category.
        @param CatID [in] Id of category to display.
        @param NewTab [in] Whether to display in new tab in detail pane.
      }
    procedure ConfigCompilers;
      {Displays configure compilers dialog box.
      }
    procedure ShowViewItem(View: IView; const NewTab: Boolean);
      {Displays a view item.
        @param ViewItem [in] View item to display.
        @param NewTab [in] Flag indicates whether view is to be displayed in
          new tab.
      }
    procedure ChangeOverviewStyle(const Style: Integer);
      {Changes display style of overview pane.
        @param Style [in] Required display style.
      }
    procedure ChangeDetailPane(const Pane: Integer);
      {Changes displayed pane in detail display area.
        @param Pane [in] Required new pane.
      }
    procedure EditSnippet(const SnippetName: WideString);
      {Edits a snippet.
        @param SnippetName [in] Name of snippet. Must be user defined.
      }
    procedure Donate;
      {Displays donate dialog box.
      }
    procedure NewSnippet;
      {Opens Snippets Editor ready to create a new snippet.
      }
    procedure ShowNews;
      {Shows news items from CodeSnip news feed.
      }
    procedure CheckForUpdates;
      {Checks for program updates.
      }
    procedure ShowAboutBox;
      {Displays the program's About box.
      }
  end;

  {
  ISetActions:
    Interface used to associated action objects with the various methods of
    INotifier. Any object that implements INotifies must also implement this
    interface.
  }
  ISetActions = interface(IInterface)
    ['{A4B7AFE2-EE6C-4D39-BEA6-B52CC8AAC1DE}']
    procedure SetUpdateDbaseAction(const Action: TBasicAction);
      {Sets action triggered when user requests database update.
        @param Action [in] Required action.
      }
    procedure SetDisplaySnippetAction(const Action: TBasicAction);
      {Sets action triggered when a named snippet is requested to be displayed.
        @param Action [in] Required action.
      }
    procedure SetConfigCompilersAction(const Action: TBasicAction);
      {Sets action triggered when user requests that configure compilers dialog
      box is to be displayed.
        @param Action [in] Required action.
      }
    procedure SetShowViewItemAction(const Action: TBasicAction);
      {Sets action triggered when user requests a view item is displayed.
        @param Action [in] Required action.
      }
    procedure SetOverviewStyleChangeActions(
      const Actions: array of TBasicAction);
      {Sets actions that are triggered when different overview display styles
      are requested.
        @param Actions [in] Dynamic array of required actions: one per display
          style.
      }
    procedure SetDetailPaneChangeAction(const Action: TBasicAction);
      {Sets action that us triggered when different detail panes are required
      to be shown.
        @param Action [in] Required action.
      }
    procedure SetEditSnippetAction(const Action: TBasicAction);
      {Sets action triggered when user requests a user defined snippet is to be
      edited.
        @param Action [in] Required action.
      }
    procedure SetDonateAction(const Action: TBasicAction);
      {Sets action triggered when user requests that the donate dialog box is
      displayed.
        @param Action [in] Required action.
      }
    procedure SetDisplayCategoryAction(const Action: TBasicAction);
      {Sets actions triggered when a category is requested to be displayed.
        @param Action [in] Required action.
      }
    procedure SetNewSnippetAction(const Action: TBasicAction);
      {Sets action triggered when user requests that the Snippets Editor is
      opened ready to create a new snippet.
      }
    procedure SetNewsAction(const Action: TBasicAction);
      {Sets action triggered when user requests that news items from CodeSnip
      news feed are displayed.
      }
    procedure SetCheckForUpdatesAction(const Action: TBasicAction);
      {Sets action triggered when user requests a check for program updates.
      }
    procedure SetAboutBoxAction(const Action: TBasicAction);
      {Sets action triggered when user requests that the About box is displayed.
      }
  end;

  {
  ISetNotifier:
    Interface used to assign a notifier object to an object that needs to notify
    the application of events. The interface must be supported by all UI objects
    other than menus and buttons that initiate events. Such objects must call
    relevant notifier methods to trigger the events.
  }
  ISetNotifier = interface(IInterface)
    ['{83283DBB-A8E3-42CA-9840-B7E2AC4BC79A}']
    procedure SetNotifier(const Notifier: INotifier);
      {Sets the object's notifier to be called in response to user input.
        @param Notifier [in] Required notifier object.
      }
  end;


implementation

end.

