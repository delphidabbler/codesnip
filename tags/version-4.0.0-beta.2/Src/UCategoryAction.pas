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
 * Portions created by the Initial Developer are Copyright (C) 2009-2012 Peter
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
  ///  <summary>
  ///  Custom action used to request display of a category.
  ///  </summary>
  TCategoryAction = class(TBasicAction, ISetNotifier)
  strict private
    var
      ///  <summary>Value of CatID property.</summary>
      fCatID: string;
      ///  <summary>Value of NewTab property.</summary>
      fNewTab: Boolean;
      ///  <summary>Reference to Notifier object.</summary>
      fNotifier: INotifier;
  public
    ///  <summary>Performs the action by displaying a category in the main
    ///  display.</summary>
    ///  <remarks>
    ///  <para>Notifier object is used to cause the category to be displayed.
    ///  </para>
    ///  <para>Any OnExecute event handler is ignored.</para>
    ///  </remarks>
    ///  <returns>Boolean. False to indicate OnExecute event handler not called.
    ///  </returns>
    function Execute: Boolean; override;
    ///  <summary>Stores reference to given notifier object.</summary>
    ///  <remarks>Implements ISetNotifier.SetNotifier</remarks>
    procedure SetNotifier(const Notifier: INotifier);
    ///  <summary>ID of category to be displayed.</summary>
    property CatID: string read fCatID write fCatID;
    ///  <summary>Flag indicating if category is to be displayed in new detail
    ///  pane tab.</summary>
    property NewTab: Boolean read fNewTab write fNewTab;
  end;


implementation


uses
  // Project
  DB.UCategory, DB.UMain, UView;


{ TCategoryAction }

function TCategoryAction.Execute: Boolean;
var
  Cat: TCategory;   // category to be displayed
begin
  Assert(Assigned(fNotifier), ClassName + '.Execute: Notifier not set');
  Assert(CatID <> '', ClassName + '.Execute: CatID not provided');
  Cat := Database.Categories.Find(CatID);
  Assert(Assigned(Cat), ClassName + '.Execute: CatID not valid');
  // Create a view item for category and get notifier to display it
  fNotifier.ShowViewItem(TViewFactory.CreateCategoryView(Cat), NewTab);
  Result := False;
end;

procedure TCategoryAction.SetNotifier(const Notifier: INotifier);
begin
  fNotifier := Notifier;
end;

end.

