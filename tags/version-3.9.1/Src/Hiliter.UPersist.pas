{
 * Hiliter.UPersist.pas
 *
 * Implements a static class that can persist syntax highlighter attributes
 * to/from a given storage.
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
 * The Original Code is Hiliter.UPersist.pas, formerly UHiliterPersist.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  Hiliter.UAttrs;


const
  // Storage names
  cFontNameName = 'FontName';
  cFontSizeName = 'FontSize';
  cElemColorName = 'Color';
  cElemStyleName = 'Style';
  cElemCompoundName = 'Elem%d.%s';


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

class procedure THiliterPersist.Load(const Storage: ISettingsSection;
  const Hiliter: IHiliteAttrs);
  {Loads a syntax highlighter's attributes from persistent storage.
    @param Storage [in] Storage containing highligher information.
    @param Hiliter [in] Highlighter attributes to load.
  }
var
  ElemId: THiliteElement; // loops thru all hiliter elements
  Elem: IHiliteElemAttrs; // reference to a hiliter element
  DefAttrs: IHiliteAttrs; // default attributes to use if no setting available
begin
  // Create default attributes object
  DefAttrs := THiliteAttrsFactory.CreateDefaultAttrs;
  // Read font info main highlight output section
  Hiliter.FontSize := StrToIntDef(
    Storage.ItemValues[cFontSizeName], DefAttrs.FontSize
  );
  Hiliter.FontName := Storage.ItemValues[cFontNameName];
  if Hiliter.FontName = '' then
    Hiliter.FontName := DefAttrs.FontName;
  // Read each highlighter element from its own subsection
  for ElemId := Low(THiliteElement) to High(THiliteElement) do
  begin
    Elem := Hiliter.Elements[ElemId];
    Elem.ForeColor := TColor(
      StrToIntDef(
        Storage.ItemValues[ElemValueName(ElemId, cElemColorName)],
        Integer(DefAttrs.Elements[ElemId].ForeColor)
      )
    );
    Elem.FontStyle := BitmaskToFontStyles(
      StrToIntDef(
        Storage.ItemValues[ElemValueName(ElemId, cElemStyleName)],
        FontStylesToBitmask(DefAttrs.Elements[ElemId].FontStyle)
      )
    );
  end;
end;

class procedure THiliterPersist.Save(const Storage: ISettingsSection;
  const Hiliter: IHiliteAttrs);
  {Saves a syntax highlighter's attributes to persistent storage.
    @param Storage [in] Storage in which to save highligher information.
    @param Hiliter [in] Highlighter attributes to save.
  }
var
  ElemId: THiliteElement; // loops thru all hiliter elements
  Elem: IHiliteElemAttrs; // reference to a hiliter element
begin
  // Store font info
  Storage.ItemValues[cFontSizeName] := IntToStr(Hiliter.FontSize);
  Storage.ItemValues[cFontNameName] := Hiliter.FontName;
  // Store each highlighter element
  for ElemId := Low(THiliteElement) to High(THiliteElement) do
  begin
    Elem := Hiliter.Elements[ElemId];
    Storage.ItemValues[ElemValueName(ElemId, cElemColorName)] :=
      IntToStr(Integer(Elem.ForeColor));
    Storage.ItemValues[ElemValueName(ElemId, cElemStyleName)] :=
      IntToStr(FontStylesToBitmask(Elem.FontStyle));
  end;
  // Save the storage
  Storage.Save;
end;

end.

