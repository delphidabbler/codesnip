{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a custom action used to request change of tab in detail pane.
}


unit UDetailTabAction;


interface


uses
  // Delphi
  Classes;


type
  ///  <summary>Custom action used to request change of tab in detail pane.
  ///  </summary>
  TDetailTabAction = class(TBasicAction)
  strict private
    var
      ///  <summary>Value of TabIndex property.</summary>
      fTabIndex: Integer;
  public
    ///  <summary>Index of detail pane tab to be displayed when the action is
    ///  executed.</summary>
    property TabIndex: Integer read fTabIndex write fTabIndex;
  end;


implementation

end.
