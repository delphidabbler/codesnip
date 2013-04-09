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
 * Custom action used to request display of a page of Preferences dialogue box
 * specified by the name of the frame class that implements the page.
}


unit UShowPrefsPageAction;


interface


uses
  // Delphi
  Classes;


type
  ///  <summary>Custom action used to request that a page of the Preferences
  ///  dialogue box is displayed.</summary>
  ///  <remarks>The page is specified by the class name of the frame that
  ///  implements it.</remarks>
  TShowPrefsPageAction = class(TBasicAction)
  strict private
    var
      ///  <summary>Value of FrameClassName property.</summary>
      fFrameClassName: string;
  public
    ///  <summary>Name of frame class that implements the required Preferences
    ///  dialogue box page.</summary>
    property FrameClassName: string read fFrameClassName write fFrameClassName;
  end;


implementation

end.
