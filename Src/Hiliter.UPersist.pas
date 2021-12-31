{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that can persist syntax highlighter attributes
 * to/from a given storage.
}


unit Hiliter.UPersist;


interface


uses
  // Project
  Hiliter.UGlobals, UBaseObjects, USettings;


type
  ///  <summary>Static class that can save and load syntax highlighter
  ///  attributes to and from persistent storage.</summary>
  THiliterPersist = class(TNoConstructObject)
  strict private
    ///  <summary>Saves a syntax highlighter's attributes to a section of
    ///  persistent storage.</summary>
    ///  <param name="Storage">ISettingsSection [in] Storage section in which
    ///  highlighter attributes are to be saved.</param>
    ///  <param name="Hiliter">IHiliteAttrs [in] Highlighter attributes to be
    ///  saved.</param>
    ///  <param name="Prefix">string [in] Text to be prefixed to each value
    ///  name in storage section. May be empty string if no prefix required.
    ///  </param>
    class procedure InternalSave(Storage: ISettingsSection;
      Hiliter: IHiliteAttrs; const Prefix: string);

    ///  <summary>Loads a syntax highlighter's attributes from a section of
    ///  persistent storage.</summary>
    ///  <param name="Storage">ISettingsSection [in] Storage section from which
    ///  highlighter attributes are to be loaded.</param>
    ///  <param name="Hiliter">IHiliteAttrs [in] Highlighter attributes object
    ///  to be updated with loaded attributes.</param>
    ///  <param name="Prefix">string [in] Text to be prefixed to each value
    ///  name in storage section. May be empty string if no prefix required.
    ///  </param>
    class procedure InternalLoad(Storage: ISettingsSection;
      Hiliter: IHiliteAttrs; const Prefix: string);

  public
    ///  <summary>Saves the given un-named syntax highlighter's attributes to
    ///  the given persistent storage section.</summary>
    ///  <remarks>Any earlier attributes written using this method are
    ///  overwritten.</remarks>
    class procedure Save(const Storage: ISettingsSection;
      const Hiliter: IHiliteAttrs);

    ///  <summary>Loads the given un-named highlighter attributes from the given
    ///  storage section.</summary>
    class procedure Load(const Storage: ISettingsSection;
      const Hiliter: IHiliteAttrs);

    ///  <summary>Saves all the given named highlighter attributes to the given
    ///  persistent storage section.</summary>
    ///  <remarks>Value names for each highlighter's attributes are given a
    ///  unique prefix in the storage section.</remarks>
    class procedure SaveNamed(Storage: ISettingsSection;
      NamedHiliters: INamedHiliteAttrs);

    ///  <summary>Load all named highlighter attributes from the given
    ///  persistent storage section into the given named highighter attributes
    ///  list.</summary>
    ///  <remarks>NamedHiliters is cleared before the persistent attributes are
    ///  loaded.</remarks>
    class procedure LoadNamed(Storage: ISettingsSection;
      NamedHiliters: INamedHiliteAttrs);
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  Hiliter.UAttrs, UIStringList;


const
  // Storage names
  cFontNameName = 'FontName';
  cFontSizeName = 'FontSize';
  cElemColorName = 'Color';
  cElemStyleName = 'Style';
  cElemCompoundName = 'Elem%d.%s';
  cNamedHiliterCountName = 'NamedHiliterCount';
  cNamedHilterName = 'HilterName%d';
  cNamedHiliterPrefix = 'NamedHiliter%d.';


///  <summary>Returns a value name for the PropName property of source code
///  element with id ElemId.</summary>
function ElemValueName(const ElemId: THiliteElement;
  const PropName: string): string;
begin
  Result := Format(cElemCompoundName, [Ord(ElemId), PropName]);
end;

///  <summary>Returns a bit flag that can uniquely represent the given font
///  style in a bitmask.</summary>
function FontStyleToBitFlag(const FontStyle: TFontStyle): Cardinal;
begin
  Result := 1 shl Ord(FontStyle);
end;

///  <summary>Returns a bit mask representing all the given font styles.
///  </summary>
function FontStylesToBitmask(const FontStyles: TFontStyles): Cardinal;
var
  FS: TFontStyle; // iterates thru all font styles
begin
  Result := 0;
  for FS := Low(TFontStyle) to High(TFontStyle) do
    if FS in FontStyles then
      Result := Result or FontStyleToBitFlag(FS);
end;

///  <summary>Returns the set of font styles represent by the given bitmask.
///  </summary>
///  <remarks>The bit mask should be in the same format as originally generated
///  by FontStylesToBitmask.</remarks>
function BitmaskToFontStyles(const Bitmask: Cardinal): TFontStyles;

  // Tests if a the given bit in the given bit mask is set.</summary>
  function IsBitSet(const Bit, Mask: Cardinal): Boolean;
  begin
    Result := Bit and Bitmask = Bit;
  end;

var
  FS: TFontStyle; // iterates thru all font styles
begin
  Result := [];
  for FS := Low(TFontStyle) to High(TFontStyle) do
    if IsBitSet(FontStyleToBitFlag(FS), Bitmask) then
      Include(Result, FS);
end;

{ THiliterPersist }

class procedure THiliterPersist.InternalLoad(Storage: ISettingsSection;
  Hiliter: IHiliteAttrs; const Prefix: string);
var
  ElemId: THiliteElement; // loops thru all hiliter elements
  Elem: IHiliteElemAttrs; // reference to a hiliter element
  DefAttrs: IHiliteAttrs; // default attributes to use if no setting available
begin
  // Create default attributes object
  DefAttrs := THiliteAttrsFactory.CreateDefaultAttrs;
  // Read font info main highlight output section
  Hiliter.FontSize := Storage.GetInteger(
    Prefix + cFontSizeName, DefAttrs.FontSize
  );
  Hiliter.FontName := Storage.GetString(
    Prefix + cFontNameName, DefAttrs.FontName
  );
  // Read each highlighter element from its own subsection
  for ElemId := Low(THiliteElement) to High(THiliteElement) do
  begin
    Elem := Hiliter.Elements[ElemId];
    Elem.ForeColor := TColor(
      Storage.GetInteger(
        Prefix + ElemValueName(ElemId, cElemColorName),
        DefAttrs.Elements[ElemId].ForeColor
      )
    );
    Elem.FontStyle := BitmaskToFontStyles(
      Storage.GetInteger(
        Prefix + ElemValueName(ElemId, cElemStyleName),
        FontStylesToBitmask(DefAttrs.Elements[ElemId].FontStyle)
      )
    );
  end;
end;

class procedure THiliterPersist.InternalSave(Storage: ISettingsSection;
  Hiliter: IHiliteAttrs; const Prefix: string);
var
  ElemId: THiliteElement; // loops thru all hiliter elements
  Elem: IHiliteElemAttrs; // reference to a hiliter element
begin
  // Store font info
  Storage.SetInteger(Prefix + cFontSizeName, Hiliter.FontSize);
  Storage.SetString(Prefix + cFontNameName, Hiliter.FontName);
  // Store each highlighter element
  for ElemId := Low(THiliteElement) to High(THiliteElement) do
  begin
    Elem := Hiliter.Elements[ElemId];
    Storage.SetInteger(
      Prefix + ElemValueName(ElemId, cElemColorName),
      Integer(Elem.ForeColor)
    );
    Storage.SetInteger(
      Prefix + ElemValueName(ElemId, cElemStyleName),
      FontStylesToBitmask(Elem.FontStyle)
    );
  end;
  // Save the storage
  Storage.Save;
end;

class procedure THiliterPersist.Load(const Storage: ISettingsSection;
  const Hiliter: IHiliteAttrs);
begin
  InternalLoad(Storage, Hiliter, '');
end;

class procedure THiliterPersist.LoadNamed(Storage: ISettingsSection;
  NamedHiliters: INamedHiliteAttrs);
var
  Hiliter: IHiliteAttrs;
  Names: IStringList;
  Idx: Integer;
begin
  NamedHiliters.Clear;
  Names := Storage.GetStrings(cNamedHiliterCountName, cNamedHilterName);
  for Idx := 0 to Pred(Names.Count) do
  begin
    Hiliter := THiliteAttrsFactory.CreateNulAttrs;
    InternalLoad(
      Storage,
      Hiliter,
      Format(cNamedHiliterPrefix, [Idx])
    );
    NamedHiliters[Names[Idx]] := Hiliter;
  end;
end;

class procedure THiliterPersist.Save(const Storage: ISettingsSection;
  const Hiliter: IHiliteAttrs);
begin
  InternalSave(Storage, Hiliter, '');
end;

class procedure THiliterPersist.SaveNamed(Storage: ISettingsSection;
  NamedHiliters: INamedHiliteAttrs);
var
  Names: IStringList;
  Idx: Integer;
begin
  Names := TIStringList.Create(NamedHiliters.Names);
  Storage.SetStrings(cNamedHiliterCountName, cNamedHilterName, Names);
  for Idx := 0 to Pred(Names.Count) do
    InternalSave(
      Storage,
      NamedHiliters[Names[Idx]],
      Format(cNamedHiliterPrefix, [Idx])
    );
end;

end.

