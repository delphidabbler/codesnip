{
 * IntfNotifier.pas
 *
 * Defines interfaces to set up and use the notifier object that triggers
 * actions in response to user initiated events in the GUI.
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
 * The Original Code is IntfNotifier.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
    procedure DisplayRoutine(const RoutineName: WideString;
      UserDefined: WordBool);
      {Displays a named routine.
        @param RoutineName [in] Name of routine to display.
        @param UserDefined [in] Whether routine is user defined.
      }
    procedure DisplayCategory(const CatID: WideString);
      {Displays an identified category.
        @param CatID [in] Id of category to display.
      }
    procedure CompileRoutine;
      {Compiles the current routine.
      }
    procedure ViewCompilerLog(Ver: SYSINT);
      {Displays a compiler log.
        @param Ver [in] Version of Delphi for which we need to display log. Ver
          is the ordinal value of the required compiler version enumerated type.
      }
    procedure ShowHint(const Hint: WideString);
      {Displays a hint.
        @param Hint [in] Hint to be displayed.
      }
    procedure ConfigCompilers;
      {Displays configure compilers dialog box.
      }
    procedure ShowViewItem(const ViewItem: TViewItem);
      {Displays a view item.
        @param ViewItem [in] View item to display.
      }
    procedure ChangeOverviewStyle(const Style: Integer);
      {Changes display style of overview pane.
        @param Style [in] Required display style.
      }
    procedure ChangeDetailPane(const Pane: Integer);
      {Changes displayed pane in detail display area.
        @param Pane [in] Required new pane.
      }
    procedure ShowTestUnit;
      {Displays test unit.
      }
    procedure EditRoutine(const RoutineName: WideString);
      {Edits a routine.
        @param RoutineName [in] Name of routine. Must be user defined.
      }
    procedure Donate;
      {Displays donate dialog box.
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
    procedure SetDisplayRoutineAction(const Action: TBasicAction);
      {Sets action triggered when a named routine is requested to be displayed.
        @param Action [in] Required action.
      }
    procedure SetCompileRoutineAction(const Action: TBasicAction);
      {Sets action triggered when user wants to test-compile the current
      routine.
        @param Action [in] Required action.
      }
    procedure SetViewCompilerLogAction(const Action: TBasicAction);
      {Sets action triggered when user wants to view a compiler log.
        @param Action [in] Required action.
      }
    procedure SetShowHintAction(const Action: TBasicAction);
      {Sets action triggered when user moves mouse over hot links etc. that
      cause hints to be displayed.
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
    procedure SetDetailPaneChangeActions(const Actions: array of TBasicAction);
      {Sets actions that are triggered when different detail panes are required
      to be shown.
        @param Actions [in] Dynamic array of required actions: one per detail
          display tab.
      }
    procedure SetShowTestUnitAction(const Action: TBasicAction);
      {Sets action triggered where displays a test unit.
        @param Action [in] Required action.
      }
    procedure SetEditRoutineAction(const Action: TBasicAction);
      {Sets action triggered when user requests a user defined routine is to be
      edited.
        @param Action [in] Required action.
      }
    procedure SetDonateAction(const Action: TBasicAction);
      {Sets action triggered when user requests that the donate dialog box is
      displays.
        @param Action [in] Required action.
      }
    procedure SetDisplayCategoryAction(const Action: TBasicAction);
      {Sets actions triggered when a category is requested to be displayed.
        @param Action [in] Required action.
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

