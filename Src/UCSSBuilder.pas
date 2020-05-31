{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Classes that help create and manage cascading style sheet code.
}


unit UCSSBuilder;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UIStringList;


type

  {
  TCSSSelector:
    Class that encapsulates a CSS selector and outputs its CSS code.
  }
  TCSSSelector = class(TObject)
  strict private
    var
      fProperties: IStringList; // List of properties for this selector
      fSelector: string;        // Name of selector
  public
    constructor Create(const Selector: string);
      {Constructor. Creates a CSS selector object with given name.
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
    type
      // Class that maps CSS selector names to selector objects
      TCSSSelectorMap = TObjectDictionary<string,TCSSSelector>;
    var
      fSelectors: TCSSSelectorMap;  // Maps selector names to selector objects
    function GetSelector(const Selector: string): TCSSSelector;
      {Read access method for Selectors property. Returns selector object with
      given name.
        @param Selector [in] Name of required selector.
        @return Selector object with given name or nil if not found.
      }
  public
    constructor Create;
      {Constructor. Sets up object.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
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
  UComparers, UConsts;


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
  {Constructor. Creates a CSS selector object with given name.
    @param Selector [in] Name of selector.
  }
begin
  Assert(Selector <> '', ClassName + '.Create: selector is empty string');
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
    @param Selector [in] Name of new selector.
    @return New empty selector object.
  }
begin
  Result := TCSSSelector.Create(Selector);
  fSelectors.Add(Selector, Result);
end;

function TCSSBuilder.AsString: string;
  {Generates CSS code representing the style sheet.
    @return Required CSS code.
  }
var
  Selector: TCSSSelector; // reference to each selector in map
begin
  Result := '';
  for Selector in fSelectors.Values do
    if not Selector.IsEmpty then
      Result := Result + Selector.AsString;
end;

procedure TCSSBuilder.Clear;
  {Clears all selectors from style sheet and frees selector objects.
  }
begin
  fSelectors.Clear; // frees selector objects in .Values[]
end;

constructor TCSSBuilder.Create;
  {Constructor. Sets up object.
  }
begin
  inherited;
  // fSelectors treats selector names are not case sensitive
  // fSelectors owns value objects and frees them when they are removed from map
  fSelectors := TCSSSelectorMap.Create(
    [doOwnsValues], TTextEqualityComparer.Create
  );
end;

destructor TCSSBuilder.Destroy;
  {Destructor. Tears down object.
  }
begin
  fSelectors.Free;    // frees selector objects in fSelectors.Values[]
  inherited;
end;

function TCSSBuilder.GetSelector(const Selector: string): TCSSSelector;
  {Read access method for Selectors property. Returns selector object with given
  name.
    @param Selector [in] Name of required selector.
    @return Selector object with given name or nil if not found.
  }
begin
  if fSelectors.ContainsKey(Selector) then
    Result := fSelectors[Selector]
  else
    Result := nil;
end;

end.

