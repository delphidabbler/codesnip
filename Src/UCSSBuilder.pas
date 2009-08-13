{
 * UCSSBuilder.pas
 *
 * Classes that help create and manage cascading style sheet code.
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
 * The Original Code is UCSSBuilder.pas
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


unit UCSSBuilder;


interface


uses
  // Delphi
  Classes,
  // Project
  UIStringList;


type

  {
  TCSSSelector:
    Class that encapsulates a CSS selector and outputs its CSS code.
  }
  TCSSSelector = class(TObject)
  strict private
    fProperties: IStringList;
      {Value of Properties property}
    fSelector: string;
      {Name of selector}
  public
    constructor Create(const Selector: string);
      {Class constructor. Creates a CSS selector object with given name.
        @param Selector [in] Name of selector.
      }
    function AsString: string;
      {Generates the CSS code representing the selector.
        @return Required CSS code.
      }
    function IsEmpty: Boolean;
      {Checks whether the selector is empty, i.e. contains no code.
        @return True if selector is empty and false if not.
      }
    procedure AddProperty(const CSSProp: string);
      {Adds a new CSS property to the selector.
        @param CSSProp [in] CSS property to be added.
      }
    property Selector: string read fSelector;
      {Name of selector}
  end;

  {
  TCSSBuilder:
    Class that creates a CSS style sheet and outputs its CSS code.
  }
  TCSSBuilder = class(TObject)
  strict private
    fSelectors: TStringList;
      {Stores map of selector names to selector objects}
    function GetSelector(const Selector: string): TCSSSelector;
      {Read access method for Selectors property. Returns selector object with
      given name.
        @param Selector [in] Name of required selector.
        @return Selector object with given name or nil if not found.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function AddSelector(const Selector: string): TCSSSelector;
      {Adds a new empty selector with given name to style sheet.
        @param Selector [in] Name of new selector.
        @return New empty selector object.
      }
    procedure Clear;
      {Clears all selectors from style sheet and frees selector objects.
      }
    function AsString: string;
      {Generates CSS code representing the style sheet.
        @return Required CSS code.
      }
    property Selectors[const Selector: string]: TCSSSelector
      read GetSelector;
      {Array of CSS selectors in style sheet, indexed by selector name}
  end;


implementation


uses
  // Project
  UConsts;


{ TCSSSelector }

procedure TCSSSelector.AddProperty(const CSSProp: string);
  {Adds a new CSS property to the selector.
    @param CSSProp [in] CSS property to be added.
  }
begin
  fProperties.Add(CSSProp);
end;

function TCSSSelector.AsString: string;
  {Generates the CSS code representing the selector.
    @return Required CSS code.
  }
var
  Lines: IStringList;   // lines of CSS
begin
  Lines := TIStringList.Create;
  // Compose CSS selector statement
  Lines.Add(Selector + ' {');
  if fProperties.Count > 0 then
    Lines.Add(fProperties);
  Lines.Add('}');
  // Write CSS in string
  Result := Lines.GetText(EOL, False) + EOL;
end;

constructor TCSSSelector.Create(const Selector: string);
  {Class constructor. Creates a CSS selector object with given name.
    @param Selector [in] Name of selector.
  }
begin
  Assert(Selector <> '',                                   // ** do not localise
    ClassName + '.Create: selector is empty string');
  inherited Create;
  fSelector := Selector;
  fProperties := TIStringList.Create;
end;

function TCSSSelector.IsEmpty: Boolean;
  {Checks whether the selector is empty, i.e. contains no code.
    @return True if selector is empty and false if not.
  }
begin
  // Selector is empty if background colour not set, there is no font,
  // margin is not defined and there is no extra CSS
  Result := fProperties.Count = 0;
end;

{ TCSSBuilder }

function TCSSBuilder.AddSelector(const Selector: string): TCSSSelector;
  {Adds a new empty selector with given name to style sheet.
    @param Selector Name of new selector.
    @return New empty selector object.
  }
begin
  Result := TCSSSelector.Create(Selector);
  fSelectors.AddObject(Selector, Result);
end;

function TCSSBuilder.AsString: string;
  {Generates CSS code representing the style sheet.
    @return Required CSS code.
  }
var
  Idx: Integer;       // loops thru all selectors in style sheet
  Sel: TCSSSelector;  // reference to a selector
begin
  Result := '';
  for Idx := 0 to Pred(fSelectors.Count) do
  begin
    // Add selector to code if selector not empty
    Sel := fSelectors.Objects[Idx] as TCSSSelector;
    if not Sel.IsEmpty then
      Result := Result + Sel.AsString;
  end;
end;

procedure TCSSBuilder.Clear;
  {Clears all selectors from style sheet and frees selector objects.
  }
var
  Idx: Integer; // loops thru selectors in style sheet
begin
  for Idx := Pred(fSelectors.Count) downto 0 do
    fSelectors.Objects[Idx].Free;
  fSelectors.Clear;
end;

constructor TCSSBuilder.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  fSelectors := TStringList.Create;
end;

destructor TCSSBuilder.Destroy;
  {Class destructor. Tears down object.
  }
begin
  Clear;              // frees selector objects in fSelectors
  fSelectors.Free;
  inherited;
end;

function TCSSBuilder.GetSelector(const Selector: string): TCSSSelector;
  {Read access method for Selectors property. Returns selector object with given
  name.
    @param Selector Name of required selector.
    @return Selector object with given name or nil if not found.
  }
var
  Idx: Integer; // index of selector in list
begin
  Idx := fSelectors.IndexOf(Selector);
  if Idx >= 0 then
    Result := fSelectors.Objects[Idx] as TCSSSelector
  else
    Result := nil;
end;

end.

