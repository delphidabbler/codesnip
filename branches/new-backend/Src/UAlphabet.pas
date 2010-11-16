{
 * UAlphabet.pas
 *
 * Defines a class that encapsulates the English upper case alphabet plus the
 * underscore character. The class can only be instantiated as a singleton and
 * can't be freed until the unit is finalised. The class maintains a list of
 * objects representing each letter / underscore. Similarly, these object cannot
 * be freed until the alphabet object is itself destroyed.
 *
 * The only reason to go through these convolutions was to retrofit the
 * alphabet view item to TViewItem. TViewItem expects view items that maintain
 * some value or description (in this case a letter of the alphabet) to be
 * represented as an object to which TViewItem maintains a reference. This means
 * that the lifetime of the object must be greater than any TViewItem. The best
 * way to do this seemed to maintain a list of objects, one per letter in the
 * alphabet + the underscore, that can't be destroyed and have a lifetime the
 * same as the program. Once TViewItem can be re-written, there will be no need
 * for these classes.
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
 * The Original Code is UAlphabet.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UAlphabet;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UBaseObjects;


type

  {
  TLetter:
    Abstract class that encapsulates a letter of the alphabet or underscore.
    Descendant must make concrete.
  }
  TLetter = class abstract(TNoPublicConstructObject)
  strict protected
    function GetLetter: Char; virtual; abstract;
      {Read accessor for Letter property. To be defined by descendant classes.
        @return Required letter (A..Z or underscore).
      }
  public
    property Letter: Char read GetLetter;
      {Encapsulated letter (A..Z or underscore}
  end;

  {
  TAlphabet:
    Class that encapsulates the upper case alphabet. Each letter is stored as a
    separate object, accessed by the Letters[] default array property.
  }
  TAlphabet = class(TNoPublicConstructObject)
  strict private
    type
      // Type of dictionary used to map letter characters to letter objects
      TAlphaDictionary = TDictionary<Char,TLetter>;
      // Type of enumerator for TAlphabet
      TEnumerator = TAlphaDictionary.TValueEnumerator;
    var
      fLetters: TAlphaDictionary;   // Maps letters to associated letter objects
      fEnum: TEnumerator;           // Enumerates all letter objects
    class var fInstance: TAlphabet; // Stores singleton instance
    class function GetInstance: TAlphabet; static;
      {Gets an instance of the object. When first first called the object is
      created. Subsequent calls return the same instance.
        @return Singletom object instance.
      }
    function GetLetter(Ch: Char): TLetter;
      {Read accessor for Letters[] property.
        @param Ch [in] Specifies required Letter object.
        @return Required letter object.
      }
  strict protected
    constructor InternalCreate;
      {Constructor called by GetInstance. Sets up object and creates all
      required TLetter objects.
      }
  public
    destructor Destroy; override;
      {Destructor. Tears down object. Frees all letter instances by setting
      their CanDestroy flag before freeing.
      }
    procedure InitEnum;
      {Initialises enumeration of all contained letters.
      }
    function NextLetter(out Letter: TLetter): Boolean;
      {Gets next letter in enumeration.
        @param Letter [out] Required letter. Undefined at end of enumeration.
        @return True if enumeration contains more letter, false if not.
      }
    class property Instance: TAlphabet read GetInstance;
      {Singleton instance of this object}
    property Letters[Ch: Char]: TLetter read GetLetter; default;
      {List of TLetter objects for whole upper case alphabet}
  end;


implementation


uses
  // Delphi
  SysUtils, Character;


type

  {
  TLetterImpl:
    Concrete implementation of TLetter that can't be freed except by owning
    object.
  }
  TLetterImpl = class(TLetter)
  strict private
    var fLetter: Char;  // Value of Letter property
  strict protected
    function GetLetter: Char; override;
      {Read accessor for Letter property.
        @return Required letter.
      }
  public
    var CanDestroy: Boolean;  // Flag indicating if object can be freed
    constructor Create(const Ch: Char);
      {Constructor. Creates a non-destructable object.
        @param Ch [in] Letter to be encapsulated. Must be A..Z or underscore.
      }
    procedure FreeInstance; override;
      {Override of method that releases object's memory on destruction that
      does not release memory unless CanDestroy is true. This effectively means
      the instance cannot be freed until CanDestroy is set true.
      }
  end;

  {
  TAlphabetImpl:
    Extension of TAlphabet that can't be destroyed except in finalization
    section of this unit.
  }
  TAlphabetImpl = class(TAlphabet)
  public
    class var CanDestroy: Boolean;  // flag the permits object to be freed
    destructor Destroy; override;
      {Destructor. Only calls inherited destructor to destroy Letters object if
      CanDestroy is True.
      }
    procedure FreeInstance; override;
      {Override of method that releases object's memory on destruction that
      does not release memory unless CanDestroy is true. This effectively means
      the instance cannot be freed until CanDestroy is set true.
      }
  end;

{ TLetterImpl }

constructor TLetterImpl.Create(const Ch: Char);
  {Constructor. Creates a non-destructable object.
    @param Ch [in] Letter to be encapsulated. Must be A..Z or underscore.
  }
begin
  Assert(CharInSet(Ch, ['A'..'Z', '_']),  // 'A'..'Z' (not any letter) or '_'
    ClassName + '.Create: Ch must be A..Z or underscore.');
  inherited InternalCreate;
  fLetter := Ch;
  CanDestroy := False;
end;

procedure TLetterImpl.FreeInstance;
  {Override of method that releases object's memory on destruction that
  does not release memory unless CanDestroy is true. This effectively means
  the instance cannot be freed until CanDestroy is set true.
  }
begin
  if CanDestroy then
    inherited;
end;

function TLetterImpl.GetLetter: Char;
  {Read accessor for Letter property.
    @return Required letter.
  }
begin
  Result := fLetter;
end;

{ TAlphabet }

destructor TAlphabet.Destroy;
  {Destructor. Tears down object. Frees all letter instances by setting their
  CanDestroy flag before freeing.
  }
var
  Letter: TLetter;  // each letter object in map
begin
  for Letter in fLetters.Values do
  begin
    (Letter as TLetterImpl).CanDestroy := True;
    Letter.Free;
  end;
  fEnum.Free;
  fLetters.Free;
  fInstance := nil;
  inherited;
end;

class function TAlphabet.GetInstance: TAlphabet;
  {Gets an instance of the object. When first first called the object is
  created. Subsequent calls return the same instance.
    @return Singletom object instance.
  }
begin
  if not Assigned(fInstance) then
    // Object created as TAlphabetImpl to get the hidden "no-free" functionality
    // of that class without exposing CanDestroy flag to wider world
    fInstance := TAlphabetImpl.InternalCreate;
  Result := fInstance;
end;

function TAlphabet.GetLetter(Ch: Char): TLetter;
  {Read accessor for Letters[] property.
    @param Ch [in] Specifies required Letter object.
    @return Required letter object.
  }
begin
  Assert(CharInSet(TCharacter.ToUpper(Ch), ['_', 'A'..'Z']),
    ClassName + '.GetLetter: Ch must be A..Z or underscore');
  Result := fLetters[Ch];
end;

procedure TAlphabet.InitEnum;
  {Initialises enumeration of all contained letters.
  }
begin
  fEnum.Free;
  fEnum := fLetters.Values.GetEnumerator;
end;

constructor TAlphabet.InternalCreate;
  {Constructor called by GetInstance. Sets up object and creates all required
  TLetter objects.
  }
var
  Ch: Char; // letters in alphabet
begin
  inherited InternalCreate;
  // use native default ordering for characters
  fLetters := TAlphaDictionary.Create;
  fLetters.Add('_', TLetterImpl.Create('_'));         // add underscore
  for Ch := 'A' to 'Z' do                             // add A..Z
    fLetters.Add(Ch, TLetterImpl.Create(Ch));
end;

function TAlphabet.NextLetter(out Letter: TLetter): Boolean;
  {Gets next letter in enumeration.
    @param Letter [out] Required letter. Undefined at end of enumeration.
    @return True if enumeration contains more letter, false if not.
  }
begin
  Result := fEnum.MoveNext;
  if Result then
    Letter := fEnum.Current;
end;

{ TAlphabetImpl }

destructor TAlphabetImpl.Destroy;
  {Destructor. Only calls inherited destructor to destroy Letters object if
  CanDestroy is True.
  }
begin
  if CanDestroy then
    inherited;
end;

procedure TAlphabetImpl.FreeInstance;
  {Override of method that releases object's memory on destruction that does not
  release memory unless CanDestroy is true. This effectively means the instance
  cannot be freed until CanDestroy is set true.
  }
begin
  if CanDestroy then
    inherited;
end;


initialization


finalization

// Destroy the TAlphabet singleton
TAlphabetImpl.CanDestroy := True;
TAlphabetImpl.Instance.Free;

end.

