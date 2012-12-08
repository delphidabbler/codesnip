{
 * FrHTMLTpltDlg.pas
 *
 * Frame containing a web browser control that displays HTML content generated
 * from a template that also takes on the appearance of a dialog box.
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
 * The Original Code is FrHTMLTpltDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrHTMLTpltDlg;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  FrHTMLDlg;


type

  {
  THTMLTpltDlgFrame:
    Frame containing a web browser control that displays content generated from
    a HTML template that also takes on the appearance of a dialog box.
  }
  THTMLTpltDlgFrame = class(THTMLDlgFrame)
  public
    procedure Initialise(const ResName: string; const Values: TStrings);
      {Initialises display by loading HTML template and replacing placeholders
      with given values.
        @param ResName Name of RT_HTML resource containing HTML template.
        @param Values List of strings in form Name=Value where Name is
          placeholder name and Value is the value to be given to placeholder.
      }
  end;


implementation


uses
  // Project
  UHTMLTemplate;


{$R *.dfm}


{ THTMLTpltDlgFrame }

procedure THTMLTpltDlgFrame.Initialise(const ResName: string;
  const Values: TStrings);
  {Initialises display by loading HTML template and replacing placeholders
  with given values.
    @param ResName Name of RT_HTML resource containing HTML template.
    @param Values List of strings in form Name=Value where Name is placeholder
      name and Value is the value to be given to placeholder.
  }
var
  HTMLTplt: THTMLTemplate;  // object used to create HTML from template
  Idx: Integer;             // loops thru all placeholder values
begin
  // Creates HTML template object
  HTMLTplt := THTMLTemplate.Create(HInstance, ResName);
  try
    // Replace all placeholders with values
    for Idx := 0 to Pred(Values.Count) do
      HTMLTplt.ResolvePlaceholderHTML(
        Values.Names[Idx], Values.ValueFromIndex[Idx]
      );
    // Load the completed HTML into web browser
    WBController.IOMgr.LoadFromString(HTMLTplt.HTML);
  finally
    HTMLTplt.Free;
  end;
end;

end.

