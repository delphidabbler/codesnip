{
 * URoutineAction.pas
 *
 * Custom action used to request display of a routine by name. Stores name of
 * required routine and its source database in properties.
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
 * The Original Code is URoutineAction.pas
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


unit URoutineAction;


interface


uses
  // Delphi
  Classes,
  // Project
  IntfNotifier;


type

  {
  TRoutineAction:
    Custom action used to request display of a named routine. Stores name of
    required routine, and whether it is user defined, in properties.
  }
  TRoutineAction = class(TBasicAction, ISetNotifier)
  strict private
    fRoutineName: string;   // Value of RoutineName property
    fUserDefined: Boolean;  // Value of UserDefined property
    fNotifier: INotifier;   // Reference to notifier object
  public
    function Execute: Boolean; override;
      {Executes action by displaying a snippet in the main display via the
      Notifier object. Any OnExcute event handler is ignored.
        @return False to indicate OnExecute event handler not called.
      }
    procedure SetNotifier(const Notifier: INotifier);
      {Stores a reference to the notifier object.
        @param Notifier [in] Required notifier object.
      }
    property RoutineName: string read fRoutineName write fRoutineName;
      {Name of routine to be displayed}
    property UserDefined: Boolean read fUserDefined write fUserDefined;
      {Flag indicating whether routine is user defined, i.e. it comes from user
      database}
  end;


implementation


uses
  // Project
  USnippets, UView;


{ TRoutineAction }

function TRoutineAction.Execute: Boolean;
  {Executes action by displaying a snippet in the main display via the Notifier
  object. Any OnExcute event handler is ignored.
    @return False to indicate OnExecute event handler not called.
  }
var
  Snippet: TRoutine;    // snippet to be displayed
  ViewItem: TViewItem;  // view item for snippet
begin
  Assert(Assigned(fNotifier), ClassName + '.Execute: Notifier not set');
  Assert(RoutineName <> '', ClassName + '.Execute: RoutineName not provided');
  Snippet := Snippets.Routines.Find(RoutineName, UserDefined);
  Assert(Assigned(Snippet), ClassName + '.Execute: RoutineName not valid');
  // Create a view item for category and get notifier to display it
  ViewItem := TViewItem.Create(Snippet);
  try
    fNotifier.ShowViewItem(ViewItem);
  finally
    ViewItem.Free;
  end;
  Result := False;
end;

procedure TRoutineAction.SetNotifier(const Notifier: INotifier);
  {Stores a reference to the notifier object.
    @param Notifier [in] Required notifier object.
  }
begin
  fNotifier := Notifier;
end;

end.

