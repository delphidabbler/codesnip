{
 * UColorBoxEx.pas
 *
 * Implements a subclass of TColorBox that enables an alternative colour dialog
 * box to be displayed when "Custom Colour" is chosen from the combo box's drop
 * down list.
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
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UColorBoxEx;


interface


uses
  // Delphi
  Dialogs, Classes, ExtCtrls;


type

  {
  TColorBoxEx:
    Subclass of TColorBox that enables an alternative colour dialog box to be
    displayed when "Custom Colour" is chosen from the combo box's drop down
    list.
  }
  TColorBoxEx = class(TColorBox)
  strict private
    var fColorDialog: TColorDialog; // Value of ColorDialog property
    procedure SetColorDialog(const Value: TColorDialog);
      {Write accessor for ColorDialog property. If a non-nil new value is assigned
      then cbCustomColor is included in the Style property.
        @param Value [in] Reference to a colour dialog box or nil.
      }
  strict protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
      {Responds to notifications that components are being created or destroyed.
      Sets ColorDialog property to nil if any assigned dialog box component is
      being destroyed.
        @param AComponent [in] Reference to component being created or
          destroyed.
        @param Operation [in] Indicates whether component is being created or
          destroyed.
      }
    function PickCustomColor: Boolean; override;
      {Called when user selects the custom colour item. Overridden to get the
      custom colour from the colour dialog referenced the the ColorDialog
      property if set. If ColorDialog is not set then TColorBox's default dialog
      is used.
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
  Sets ColorDialog property to nil if any assigned dialog box component is being
  destroyed.
    @param AComponent [in] Reference to component being created or destroyed.
    @param Operation [in] Indicates whether component is being created or
      destroyed.
  }
begin
  inherited;
  if (AComponent = fColorDialog) and (Operation = opRemove) then
    fColorDialog := nil;
end;

function TColorBoxEx.PickCustomColor: Boolean;
  {Called when user selects the custom colour item. Overridden to get the custom
  colour from the colour dialog referenced the the ColorDialog property if set.
  If ColorDialog is not set then TColorBox's default dialog is used.
    @return True if user OKs colour dialog box, False if user cancels.
  }
var
  CustomColor: TColor;  // current custom colour from colour combo box
begin
  if Assigned(fColorDialog) then
  begin
    // custom colour stored as first element of combo's Items.Objects[] property
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
  {Write accessor for ColorDialog property. If a non-nil new value is assigned
  then cbCustomColor is included in the Style property.
    @param Value [in] Reference to a colour dialog box or nil.
  }
begin
  fColorDialog := Value;
  if Assigned(fColorDialog) then
    Self.Style := Self.Style + [cbCustomColor];
end;

end.

