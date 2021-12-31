{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame containing a rich edit control that is placed in a "show
 * case", i.e. behind a transparent control that prevents the RTF control and
 * its text from being selected.
}


unit FrRTFShowCase;


interface


uses
  // Delphi
  Forms, Classes, Controls, StdCtrls, ComCtrls,
  // Project
  UShowCaseCtrl;


type

  {
  TRTFShowCaseFrame:
    Frame containing a rich edit control that is placed in a "show case", i.e.
    behind a transparent control that prevents the RTF control and its text from
    being selected.
  }
  TRTFShowCaseFrame = class(TFrame)
    reView: TRichEdit;
  strict private
    fShowCase: TShowCaseCtrl;
      {Transparent "show case" control placed in front of rich edit control}
    function GetRichEdit: TRichEdit;
      {Read accessor for RichEdit property.
        @return Reference to rich edit control.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Creates "show case" control and places in front of
      rich edit.
        @param AOwner [in] Unused. Passed to inherited constructor.
      }
    property RichEdit: TRichEdit read GetRichEdit;
      {Reference to contained rich edit control. Users should use this property
      instead of access rich edit control directly}
  end;


implementation


uses
  // Delphi
  Windows;


{$R *.dfm}

{ TRTFShowCaseFrame }

constructor TRTFShowCaseFrame.Create(AOwner: TComponent);
  {Class constructor. Creates "show case" control and places in front of rich
  edit.
    @param AOwner [in] Unused. Passed to inherited constructor.
  }
begin
  inherited;
  fShowCase := TShowCaseCtrl.Create(Self);
  fShowCase.Parent := Self;
  fShowCase.Align := alClient;
  fShowCase.BringToFront;
end;

function TRTFShowCaseFrame.GetRichEdit: TRichEdit;
  {Read accessor for RichEdit property.
    @return Reference to rich edit control.
  }
begin
  Result := reView;
end;

end.
