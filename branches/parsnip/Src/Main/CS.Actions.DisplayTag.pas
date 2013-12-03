{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a custom action that requests display of a tag.
}


unit CS.Actions.DisplayTag;


interface


uses
  // Delphi
  Classes,
  // Project
  CS.Database.Types,
  IntfNotifier;


type
  ///  <summary>Custom action that requests display of a tag.</summary>
  TDisplayTagAction = class(TBasicAction, ISetNotifier)
  strict private
    var
      ///  <summary>Value of Tag property.</summary>
      fTag: TTag;
      ///  <summary>Value of NewTab property.</summary>
      fNewTab: Boolean;
      ///  <summary>Reference to Notifier object.</summary>
      fNotifier: INotifier;
  public
    ///  <summary>Performs the action by displaying tag in the main display.
    ///  </summary>
    ///  <remarks>
    ///  <para>Notifier object is used to cause the tag to be displayed.
    ///  </para>
    ///  <para>Any OnExecute event handler is ignored.</para>
    ///  </remarks>
    ///  <returns>Boolean. False to indicate OnExecute event handler not called.
    ///  </returns>
    function Execute: Boolean; override;
    ///  <summary>Stores reference to given notifier object.</summary>
    ///  <remarks>Implements ISetNotifier.SetNotifier</remarks>
    procedure SetNotifier(const Notifier: INotifier);
    ///  <summary>Tag to be displayed.</summary>
    property Tag: TTag read fTag write fTag;
    ///  <summary>Flag indicating if tag is to be displayed in a new detail pane
    ///  tab.</summary>
    property NewTab: Boolean read fNewTab write fNewTab;
  end;


implementation


uses
  // Project
  UView;


{ TDisplayTagAction }

function TDisplayTagAction.Execute: Boolean;
begin
  Assert(Assigned(fNotifier), ClassName + '.Execute: Notifier not set');
  // Create a view item for tag and get notifier to display it
  fNotifier.ShowViewItem(TViewFactory.CreateTagView(Tag), NewTab);
  Result := False;
end;

procedure TDisplayTagAction.SetNotifier(const Notifier: INotifier);
begin
  fNotifier := Notifier;
end;

end.
