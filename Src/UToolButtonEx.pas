{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements class that extends TToolButton to permit new tool buttons and
 * separators to be added dynamically to a toolbar.
}


unit UToolButtonEx;


interface


uses
  // Delphi
  ActnList, Classes, ComCtrls;


type
  {
  TToolButtonEx:
    Class that extends TToolButton to permit new tool buttons and separators to
    be added dynamically to a toolbar.
  }
  TToolButtonEx = class(TToolButton)
  public
    class function NewHostedButton(const ToolBar: TToolBar;
      const Action: TCustomAction): TToolButtonEx;
      {Creates a new tool button and adds to a toolbar.
        @param ToolBar [in] Toolbar to which tool button is to be added.
        @param Action [in] Action to be associated with tool button.
        @return New tool button instance.
      }
    class function NewHostedSeparator(const ToolBar: TToolBar): TToolButtonEx;
      {Creates a new tool button separator and adds to a toolbar.
        @param ToolBar [in] Toolbar to which tool button is to be added.
        @return New tool button separator instance.
      }
  end;


implementation


{ TToolButtonEx }

class function TToolButtonEx.NewHostedButton(const ToolBar: TToolBar;
  const Action: TCustomAction): TToolButtonEx;
  {Creates a new tool button and adds to a toolbar.
    @param ToolBar [in] Toolbar to which tool button is to be added.
    @param Action [in] Action to be associated with tool button.
    @return New tool button instance.
  }
begin
  Result := Create(ToolBar);
  Result.Action := Action;
  Result.SetToolBar(ToolBar);
end;

class function TToolButtonEx.NewHostedSeparator(
  const ToolBar: TToolBar): TToolButtonEx;
  {Creates a new tool button separator and adds to a toolbar.
    @param ToolBar [in] Toolbar to which tool button is to be added.
    @return New tool button separator instance.
  }
const
  cSeparatorWidth = 8;  // width of a separator in pixels
begin
  Result := Create(ToolBar);
  Result.Style := tbsSeparator;
  Result.Width := cSeparatorWidth;
  Result.SetToolBar(ToolBar);
end;

end.

