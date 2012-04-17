{
 * FrRTFShowCase.pas
 *
 * Implements a frame containing a rich edit control that is placed in a "show
 * case", i.e. behind a transparent control that prevents the RTF control and
 * its text from being selected.
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
 * The Original Code is FrRTFShowCase.pas
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
  private
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
