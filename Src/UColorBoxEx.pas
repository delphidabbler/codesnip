{
 * UColorBoxEx.pas
 *
 * Implements a subclass of TColorBox that modifies the colour dialog box
 * displayed when "Custom Colour" is chosen from the drop down list. This
 * subclass works around a Delphi / Vista display bug. Also the colour dialog
 * is displayed expanded with a custom title.
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
 * The Original Code is UColorBoxEx.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UColorBoxEx;

{$WARN UNSAFE_CAST OFF}

interface


uses
  // Delphi
  Dialogs, Classes, ExtCtrls;


type

  {
  TColorBoxEx:
    Subclass of TColorBox that modifies the colour dialog box displayed when
    "Custom Colour" is chosen from the drop down list. This subclass works
    around a Delphi / Vista display bug. Also the colour dialog is displayed
    expanded with a custom title.
  }
  TColorBoxEx = class(TColorBox)
  private
    fColorDialog: TColorDialog;
      {Value of ColorDialog property}
    procedure SetColorDialog(const Value: TColorDialog);
      {Write accessor for ColorDialog property. If new value is assigned then
      cbCustomColor is included in Style property.
        @param Value [in] Reference to colour dialog box or nil.
      }
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
      {Responds to notifications that components are being created or destroyed.
      Sets ColorDialog property to nil if assigned dialog box component is
      freed.
        @param AComponent [in] Reference to component being created or
          destroyed.
        @param Operation [in] Whether component being created or destroyed.
      }
    function PickCustomColor: Boolean; override;
      {Called when user selects the custom colour item. Overridden to use a
      custom colour dialog box and to create dialog with owner of TColorBoxEx.
        @return True if user OKs colour dialog box, False if user cancels.
      }
  published
    property ColorDialog: TColorDialog
      read fColorDialog write SetColorDialog;
      {Reference to dialog box to be displayed when custom colour is selected in
      dialog box. Provided to enable an alternative dialog box to be used}
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics;


{ TColorBoxEx }

procedure TColorBoxEx.Notification(AComponent: TComponent;
  Operation: TOperation);
  {Responds to notifications that components are being created or destroyed.
  Sets ColorDialog property to nil if assigned dialog box component is freed.
    @param AComponent [in] Reference to component being created or destroyed.
    @param Operation [in] Whether component being created or destroyed.
  }
begin
  inherited;
  if (AComponent = fColorDialog) and (Operation = opRemove) then
    fColorDialog := nil;
end;

function TColorBoxEx.PickCustomColor: Boolean;
  {Called when user selects the custom colour item. Overridden to use a custom
  colour dialog box and to create dialog with owner of TColorBoxEx.
    @return True if user OKs colour dialog box, False if user cancels.
  }
var
  CustomColor: TColor;  // current custom colour from colour combo box
begin
  // Changes from Delphi's TColorBox are that we use color referenced by
  // ColorDialog property if set instead of built in dialog box.
  // If ColorDialog property is not set we use default processing and internal
  // dialog box.
  if Assigned(fColorDialog) then
  begin
    CustomColor := ColorToRGB(TColor(Self.Items.Objects[0]));
    fColorDialog.Color := CustomColor;
    Result := fColorDialog.Execute;
    if Result then
    begin
      Self.Items.Objects[0] := TObject(fColorDialog.Color);
      Self.Invalidate;
    end;
  end
  else
    Result := inherited PickCustomColor;
end;

procedure TColorBoxEx.SetColorDialog(const Value: TColorDialog);
  {Write accessor for ColorDialog property. If new value is assigned then
  cbCustomColor is included in Style property.
    @param Value [in] Reference to colour dialog box or nil.
  }
begin
  fColorDialog := Value;
  if Assigned(fColorDialog) then
    Self.Style := Self.Style + [cbCustomColor];
end;

end.

