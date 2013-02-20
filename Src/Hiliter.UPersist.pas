{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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

  {
  THiliterPersist:
    Static class that can save and load syntax highlighter attributes to and
    from persistent storage.
  }
  THiliterPersist = class(TNoConstructObject)
  strict private
    class procedure InternalSave(Storage: ISettingsSection;
      Hiliter: IHiliteAttrs; const Prefix: string);
    class procedure InternalLoad(Storage: ISettingsSection;
      Hiliter: IHiliteAttrs; const Prefix: string);
  public
    class procedure Save(const Storage: ISettingsSection;
      const Hiliter: IHiliteAttrs);
      {Saves a syntax highlighter's attributes to persistent storage.
        @param Storage [in] Storage in which to save highligher information.
        @param Hiliter [in] Highlighter attributes to save.
      }
    class procedure Load(const Storage: ISettingsSection;
      const Hiliter: IHiliteAttrs);
      {Loads a syntax highlighter's attributes from persistent storage.
        @param Storage [in] Storage containing highligher information.
        @param Hiliter [in] Highlighter attributes to load.
      }
    class procedure SaveNamed(Storage: ISettingsSection;
      NamedHiliters: INamedHiliteAttrs);
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


function ElemValueName(const ElemId: THiliteElement;
  const PropName: string): string;
  {Builds a value name for the property of a particular element.
    @param ElemId [in] Id of element for which we want value name.
    @param PropName [in] Name of property within element.
    @return Required value name.
  }
begin
  Result := Format(cElemCompoundName, [Ord(ElemId), PropName]);
end;

function FontStyleToBitFlag(const FontStyle: TFontStyle): Cardinal;
  {Converts a font style ordinal into a bit flag that can uniquely represent
  the font style in a bitmask.
    @param FontStyle [in] Font style to convert.
    @return Equivalent bitflag.
  }
begin
  Result := 1 shl Ord(FontStyle);
end;

function FontStylesToBitmask(const FontStyles: TFontStyles): Cardinal;
  {Converts a set of font styles into an equivalent 32 bit bitmask.
    @param FontStyles [in] Set of font styles to be converted.
    @return Equivalent bitmask.
  }
var
  FS: TFontStyle; // iterates thru all font styles
begin
  Result := 0;
  for FS := Low(TFontStyle) to High(TFontStyle) do
    if FS in FontStyles then
      Result := Result or FontStyleToBitFlag(FS);
end;

function BitmaskToFontStyles(const Bitmask: Cardinal): TFontStyles;
  {Converts a bitmask representation of a set of font styles back into the
  equivalent set.
    @param Bitmask [in] Bitmask to be converted.
    @return Equivalent set of font styles.
  }
  // ---------------------------------------------------------------------------
  function IsBitSet(const Bit, Mask: Cardinal): Boolean;
    {Tests if a bit in a bit mask is set.
      @param Bit [in] Bit to set.
      @param Mask [in] Bitmask to be tested.
      @return True if bit in mask.
    }
  begin
    Result := Bit and Bitmask = Bit;
  end;
  // ---------------------------------------------------------------------------
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
  Hiliter.FontSize := StrToIntDef(
    Storage.ItemValues[Prefix + cFontSizeName], DefAttrs.FontSize
  );
  Hiliter.FontName := Storage.ItemValues[Prefix + cFontNameName];
  if Hiliter.FontName = '' then
    Hiliter.FontName := DefAttrs.FontName;
  // Read each highlighter element from its own subsection
  for ElemId := Low(THiliteElement) to High(THiliteElement) do
  begin
    Elem := Hiliter.Elements[ElemId];
    Elem.ForeColor := TColor(
      StrToIntDef(
        Storage.ItemValues[Prefix + ElemValueName(ElemId, cElemColorName)],
        Integer(DefAttrs.Elements[ElemId].ForeColor)
      )
    );
    Elem.FontStyle := BitmaskToFontStyles(
      StrToIntDef(
        Storage.ItemValues[Prefix + ElemValueName(ElemId, cElemStyleName)],
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
  Storage.ItemValues[Prefix + cFontSizeName] := IntToStr(Hiliter.FontSize);
  Storage.ItemValues[Prefix + cFontNameName] := Hiliter.FontName;
  // Store each highlighter element
  for ElemId := Low(THiliteElement) to High(THiliteElement) do
  begin
    Elem := Hiliter.Elements[ElemId];
    Storage.ItemValues[Prefix + ElemValueName(ElemId, cElemColorName)] :=
      IntToStr(Integer(Elem.ForeColor));
    Storage.ItemValues[Prefix + ElemValueName(ElemId, cElemStyleName)] :=
      IntToStr(FontStylesToBitmask(Elem.FontStyle));
  end;
  // Save the storage
  Storage.Save;
end;

class procedure THiliterPersist.Load(const Storage: ISettingsSection;
  const Hiliter: IHiliteAttrs);
  {Loads a syntax highlighter's attributes from persistent storage.
    @param Storage [in] Storage containing highligher information.
    @param Hiliter [in] Highlighter attributes to load.
  }
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
  {Saves a syntax highlighter's attributes to persistent storage.
    @param Storage [in] Storage in which to save highligher information.
    @param Hiliter [in] Highlighter attributes to save.
  }
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

