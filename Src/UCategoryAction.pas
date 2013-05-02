{
 * UCategoryAction.pas
 *
 * Custom action used to display a category by ID. Stores id of required
 * category in properties. Displays category by creating a view and using
 * notifier to display it.
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
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCategoryAction;


interface


uses
  // Delphi
  Classes,
  // Project
  IntfNotifier;


type

  {
  TCategoryAction:
    Custom action used to request display of a category. Stores ID of required
    category in properties.
  }
  TCategoryAction = class(TBasicAction, ISetNotifier)
  strict private
    fCatID: string;       // Value of CatID property
    fNotifier: INotifier; // Value of Notifier property.
  public
    function Execute: Boolean; override;
      {Executes action by displaying a category in the main display via the
      Notifier object. Any OnExcute event handler is ignored.
        @return False to indicate OnExecute event handler not called.
      }
    procedure SetNotifier(const Notifier: INotifier);
      {Stores a reference to the notifier object.
        @param Notifier [in] Required notifier object.
      }
    property CatID: string read fCatID write fCatID;
      {ID of category to be displayed}
  end;


implementation


uses
  // Project
  USnippets, UView;


{ TCategoryAction }

function TCategoryAction.Execute: Boolean;
  {Executes action by displaying a category in the main display via the Notifier
  object. Any OnExcute event handler is ignored.
    @return False to indicate OnExecute event handler not called.
  }
var
  Cat: TCategory;       // category to be displayed
  ViewItem: TViewItem;  // view item for category
begin
  Assert(Assigned(fNotifier), ClassName + '.Execute: Notifier not set');
  Assert(CatID <> '', ClassName + '.Execute: CatID not provided');
  Cat := Snippets.Categories.Find(CatID);
  Assert(Assigned(Cat), ClassName + '.Execute: CatID not valid');
  // Create a view item for category and get notifier to display it
  ViewItem := TViewItem.Create(Cat);
  try
    fNotifier.ShowViewItem(ViewItem);
  finally
    ViewItem.Free;
  end;
  Result := False;
end;

procedure TCategoryAction.SetNotifier(const Notifier: INotifier);
  {Stores a reference to the notifier object.
    @param Notifier [in] Required notifier object.
  }
begin
  fNotifier := Notifier;
end;

end.
