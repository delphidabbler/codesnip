{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a subclass of TColorBox that enables an alternative colour dialog
 * box to be displayed when "Custom Colour" is chosen from the combo box's drop
 * down list.
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
  Graphics;


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

