{
 * UToolButtonEx.pas
 *
 * Implements class that extends TToolButton to permit new tool buttons and
 * separators to be added dynamically to a toolbar.
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
 * The Original Code is UToolButtonEx.pas
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

