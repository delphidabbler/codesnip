{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines interface to an object that stores and manipulates a list of strings,
 * along with a class that implements the interface.
}


unit UIStringList;


interface


uses
  // Delphi
  System.Classes,
  // Project
  IntfCommon;


type

  {
  IStringList:
    Interface to an object that stores and manipulates a list of strings.
  }
  IStringList = interface(IInterface)
    ['{D8A8E21F-FFE9-486D-8DA3-37CD08922023}']
    function Add(const Str: string): Integer; overload;
      {Adds a string to end of list.
        @param Str [in] String to be added to list.
        @return Index of new string in list.
      }
    procedure Add(const Strs: TStrings); overload;
      {Adds all items from a string list to end of list.
        @param Strs [in] String list to be added.
      }
    procedure Add(const Strs: IStringList); overload;
      {Adds all items from another IStringList instance to end of list.
        @param Strs [in] String list to be added.
      }
    procedure Add(const Strs: array of string); overload;
      {Adds all strings from an array to end of list.
        @param Strs [in] Dynamic array of strings to be added.
      }
    procedure Add(const Str: string; const Delim: string;
      const AllowEmpty: Boolean; const Trim: Boolean = False); overload;
      {Splits a string at delimiter and adds component parts of string to end of
      list.
        @param Str [in] String to be split.
        @param Delim [in] String that delimits components of string.
        @param AllowEmpty [in] Determines whether empty components are added to
          list (True) or ignored (False).
        @param Trim [in] Determines whether strings are trimmed of trailing and
          leading spaces before adding to list.
      }
    procedure SetText(const Text: string; const Delim: string;
      const AllowEmpty: Boolean; const Trim: Boolean = False);
      {Sets list to component parts of a string, based on a delimiter.
        @param Str [in] String to be split.
        @param Delim [in] String that delimits components of string.
        @param AllowEmpty [in] Determines whether empty components are stored in
          list (True) or ignored (False).
        @param Trim [in] Determines whether strings are trimmed of trailing and
          leading spaces before adding to list.
      }
    function GetText(const Glue: string; const AllowEmpty: Boolean): string;
      {Gets contents of list as single string with items joined by a string.
        @param Glue [in] String used to glue list items together.
        @param AllowEmpty [in] Determines whether empty items are to be included
          in rendered text.
        @return Required text string.
      }
    procedure Clear;
      {Clears the list.
      }
    function IndexOf(const Str: string): Integer;
      {Gets index of a string in list.
        @param Str [in] String to be found.
        @return Index of string in list or -1 if string not found.
      }
    function Contains(const Str: string): Boolean;
      {Checks whether list contains a string.
        @param Str [in] String to be found.
        @return True if Str in list, False otherwise.
      }
    function Count: Integer;
      {Gets number of items in list.
        @return Required number of items.
      }
    procedure Delete(const Idx: Integer);
      {Deletes a string from list.
        @param Index of string to be deleted.
      }
    function GetItem(const Idx: Integer): string;
      {Gets string at a given position in list.
        @param Idx [in] Index of required string. Must be in range.
        @return Required string.
      }
    procedure SetItem(const Idx: Integer; const Str: string);
      {Sets string at a given position in list.
        @param Idx [in] Index in list to receive strings. Must be in range.
        @param Str [in] String to be stored in list.
      }
    property Items[const Idx: Integer]: string
      read GetItem write SetItem; default;
      {Indexed access to strings in list}
    function GetCaseSensitive: Boolean;
      {Checks whether searching in string list is to be case sensitive.
        @return True if searching is case sensitive, False if not case
          sensitive.
      }
    procedure SetCaseSensitive(const Flag: Boolean);
      {Sets search case sensitivity.
        @param Flag [in] True if case sensitive searching is required, False if
          case insensitive searching is required.
      }
    property CaseSensitive: Boolean
      read GetCaseSensitive write SetCaseSensitive; // default False;
      {Determines whether searching is case sensitive or case insensitive}
    function GetEnumerator: TStringsEnumerator;
      {Creates an enumerator for the string list.
        @return Enumerator instance.
      }
    procedure CopyTo(const SL: TStrings; const Overwrite: Boolean = False);
      {Copies this object's string to a string list.
        @param SL [in] String list to receive copy of this object's strings.
        @param Overwrite [in] Flag indicating whether to clear SL before copying
          (True) or to append strings to SL (False).
      }
    procedure CopyFrom(const SL: TStrings; const Overwrite: Boolean = False);
      {Copies a string list's strings to this object.
        @param SL [in] String list containing strings to be copies.
        @param Overwrite [in] Flag indicating whether to clear this object
          before copying (True) or to append the strings to this object (False).
      }
    function IsValidIndex(const Idx: Integer): Boolean;
      {Checks if an index into the string list is valid, i.e. it is in range and
      can be used as an index into the Items[] property without error.
        @param Idx [in] Index to check.
        @return True if index is valid and False if invalid.
      }
    function ToArray: TArray<string>;
      {Copies strings from string list into an array of strings.
        @return Array of strings.
      }
    procedure Sort;
      {Sorts the string list alphabetically.
      }
    ///  <summary>Return a copy of this string list</summary>
    function Clone: IStringList;
    ///  <summary>Replace this string list with a copy of string list <c>Src</c>
    ///  </summary>
    procedure Assign(Src: IStringList);
  end;

  {
  TIStringList:
    Class that implements IStringList and provides various overloaded
    constructors.
  }
  TIStringList = class(TInterfacedObject,
    IStringList,
    { TODO -cRefactor: Remove IAssignable & IClonable interfaces & methods
                        - replacements added to IStringList }
    IAssignable, IClonable
  )
  strict private
    var
      fStrings: TStringList;
        {Stores string list}
  public
    constructor Create; overload;
      {Class constructor. Creates new empty list.
      }
    constructor Create(const Strs: TStrings); overload;
      {Class constructor. Creates new list containing specified strings.
        @param Strs [in] List of strings to be stored in list.
      }
    constructor Create(const Strs: IStringList); overload;
      {Class constructor. Creates new list containing strings from another
      IStringList instance.
        @param Strs [in] List of strings to be stored in list.
      }
    constructor Create(const Str: string); overload;
      {Class constructor. Creates new list containing a single string.
        @param Str [in] String to be included in list.
      }
    constructor Create(const Str, Delim: string; const AllowEmpty: Boolean;
      const Trim: Boolean = False); overload;
      {Class constructor. Creates new list containing strings split at a
      delimiter.
        @param Str [in] String to be split.
        @param Delim [in] String that delimits components of string.
        @param AllowEmpty [in] Determines whether empty components are stored in
          list (True) or ignored (False).
        @param Trim [in] Determines whether strings are trimmed of trailing and
          leading spaces before adding to list.
      }
    constructor Create(const Strs: array of string); overload;
      {Class constructor. Creates new list containing strings from array.
        @param Strs [in] Array of strings to be included in list.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    { IStringList methods }
    function Add(const Str: string): Integer; overload;
      {Adds a string to end of list.
        @param Str [in] String to be added to list.
        @return Index of new string in list.
      }
    procedure Add(const Strs: TStrings); overload;
      {Adds all items from a string list to end of list.
        @param Strs [in] String list to be added.
      }
    procedure Add(const Strs: IStringList); overload;
      {Adds all items from another IStringList instance to end of list.
        @param Strs [in] String list to be added.
      }
    procedure Add(const Strs: array of string); overload;
      {Adds all strings from an array to end of list.
        @param Strs [in] Dynamic array of strings to be added.
      }
    procedure Add(const Str: string; const Delim: string;
      const AllowEmpty: Boolean; const Trim: Boolean = False); overload;
      {Splits a string at delimiter and adds component parts of string to end of
      list.
        @param Str [in] String to be split.
        @param Delim [in] String that delimits components of string.
        @param AllowEmpty [in] Determines whether empty components are added to
          list (True) or ignored (False).
        @param Trim [in] Determines whether strings are trimmed of trailing and
          leading spaces before adding to list.
      }
    procedure SetText(const Text: string; const Delim: string;
      const AllowEmpty: Boolean; const Trim: Boolean = False);
      {Sets list to component parts of a string, based on a delimiter.
        @param Str [in] String to be split.
        @param Delim [in] String that delimits components of string.
        @param AllowEmpty [in] Determines whether empty components are stored in
          list (True) or ignored (False).
        @param Trim [in] Determines whether strings are trimmed of trailing and
          leading spaces before adding to list.
      }
    function GetText(const Glue: string; const AllowEmpty: Boolean): string;
      {Gets contents of list as single string with items joined by a string.
        @param Glue [in] String used to glue list items together.
        @param AllowEmpty [in] Determines whether empty items are to be included
          in rendered text.
        @return Required text string.
      }
    procedure Clear;
      {Clears the list.
      }
    function IndexOf(const Str: string): Integer;
      {Gets index of a string in list.
        @param Str [in] String to be found.
        @return Index of string in list or -1 if string not found.
      }
    function Contains(const Str: string): Boolean;
      {Checks whether list contains a string.
        @param Str [in] String to be found.
        @return True if Str in list, False otherwise.
      }
    function Count: Integer;
      {Gets number of items in list.
        @return Required number of items.
      }
    procedure Delete(const Idx: Integer);
      {Deletes a string from list.
        @param Index of string to be deleted.
      }
    function GetItem(const Idx: Integer): string;
      {Gets string at a given position in list.
        @param Idx [in] Index of required string. Must be in range.
        @return Required string.
      }
    procedure SetItem(const Idx: Integer; const Str: string);
      {Sets string at a given position in list.
        @param Idx [in] Index in list to receive strings. Must be in range.
        @param Str [in] String to be stored in list.
      }
    function GetCaseSensitive: Boolean;
      {Checks whether searching in string list is to be case sensitive.
        @return True if searching is case sensitive, False if not case
          sensitive.
      }
    procedure SetCaseSensitive(const Flag: Boolean);
      {Sets search case sensitivity.
        @param Flag [in] True if case sensitive searching is required, False if
          case insensitive searching is required.
      }
    function GetEnumerator: TStringsEnumerator;
      {Creates an enumerator for the string list.
        @return Enumerator instance.
      }
    procedure CopyTo(const SL: TStrings; const Overwrite: Boolean = False);
      {Copies this object's string to a string list.
        @param SL [in] String list to receive copy of this object's strings.
        @param Overwrite [in] Flag indicating whether to clear SL before copying
          (True) or to append strings to SL (False).
      }
    procedure CopyFrom(const SL: TStrings; const Overwrite: Boolean = False);
      {Copies a string list's strings to this object.
        @param SL [in] String list containing strings to be copies.
        @param Overwrite [in] Flag indicating whether to clear this object
          before copying (True) or to append the strings to this object (False).
      }
    function IsValidIndex(const Idx: Integer): Boolean;
      {Checks if an index into the string list is valid, i.e. it is in range and
      can be used as an index into the Items[] property without error.
        @param Idx [in] Index to check.
        @return True if index is valid and False if invalid.
      }
    function ToArray: TArray<string>;
      {Copies strings from string list into an array of strings.
        @return Array of strings.
      }
    procedure Sort;
      {Sorts the string list alphabetically.
      }
    ///  <summary>Return a copy of this string list</summary>
    function Clone: IStringList;
    ///  <summary>Replace this string list with a copy of string list <c>Src</c>
    ///  </summary>
    procedure Assign(Src: IStringList); overload;
    { IAssignable methods }
    procedure Assign(const Src: IInterface); overload;
      {Sets list to a copy of another IStringList instance.
        @param Src [in] String list to be copied. Must support IStringList.
        @except EBug raised if Src does not support IStringList.
      }
    { IClonable methods }
    function IClonable.Clone = IClonableClone;
    function IClonableClone: IInterface;
      {Creates a new instance of the object that is an extact copy of this
      instance.
        @return New object's IInterface interface.
      }
  end;


implementation


uses
  // Delphi
  System.SysUtils,
  // Project
  UExceptions, UStrUtils;


{ TIStringList }

procedure TIStringList.Add(const Strs: TStrings);
  {Adds all items from a string list to end of list.
    @param Strs [in] String list to be added.
  }
begin
  fStrings.AddStrings(Strs);
end;

function TIStringList.Add(const Str: string): Integer;
  {Adds a string to end of list.
    @param Str [in] String to be added to list.
    @return Index of new string in list.
  }
begin
  Result := fStrings.Add(Str);
end;

procedure TIStringList.Add(const Strs: IStringList);
  {Adds all items from another IStringList instance to end of list.
    @param Strs [in] String list to be added.
  }
begin
  for var S in Strs do
    Add(S);
end;

procedure TIStringList.Add(const Str: string; const Delim: string;
  const AllowEmpty: Boolean; const Trim: Boolean);
  {Splits a string at delimiter and adds component parts of string to end of
  list.
    @param Str [in] String to be split.
    @param Delim [in] String that delimits components of string.
    @param AllowEmpty [in] Determines whether empty components are added to list
      (True) or ignored (False).
    @param Trim [in] Determines whether strings are trimmed of trailing and
      leading spaces before adding to list.
  }
var
  SL: TStringList;  // string list to receive exploded string
begin
  // Explode string an store in string list
  SL := TStringList.Create;
  try
    StrExplode(Str, Delim, SL, AllowEmpty, Trim);
    // Add strings to this list
    Add(SL);
  finally
    SL.Free;
  end;
end;

procedure TIStringList.Add(const Strs: array of string);
  {Adds all strings from an array to end of list.
    @param Strs [in] Dynamic array of strings to be added.
  }
begin
  for var S in Strs do
    Add(S);
end;

procedure TIStringList.Assign(const Src: IInterface);
  {Sets list to a copy of another IStringList instance.
    @param Src [in] String list to be copied. Must support IStringList.
    @except EBug raised if Src does not support IStringList.
  }
begin
  if not Supports(Src, IStringList) then
    raise EBug.Create(ClassName + '.Assign: Src must support IStringList');
  Clear;
  Add(Src as IStringList);
end;

procedure TIStringList.Assign(Src: IStringList);
begin
  Clear;
  Add(Src);
end;

procedure TIStringList.Clear;
  {Clears the list.
  }
begin
  fStrings.Clear;
end;

function TIStringList.Clone: IStringList;
  {Creates a new instance of the object that is an extact copy of this instance.
    @return New object's IInterface interface.
  }
begin
  Result := TIStringList.Create(Self);
end;

function TIStringList.Contains(const Str: string): Boolean;
  {Checks whether list contains a string.
    @param Str [in] String to be found.
    @return True if Str in list, False otherwise.
  }
begin
  Result := IndexOf(Str) >= 0;
end;

procedure TIStringList.CopyFrom(const SL: TStrings; const Overwrite: Boolean);
  {Copies a string list's strings to this object.
    @param SL [in] String list containing strings to be copies.
    @param Overwrite [in] Flag indicating whether to clear this object before
      copying (True) or to append the strings to this object (False).
  }
begin
  if OverWrite then
    fStrings.Assign(SL)
  else
    Self.Add(SL);
end;

procedure TIStringList.CopyTo(const SL: TStrings; const Overwrite: Boolean);
  {Copies this object's string to a string list.
    @param SL [in] String list to receive copy of this object's strings.
    @param Overwrite [in] Flag indicating whether to clear SL before copying
      (True) or to append strings to SL (False).
  }
begin
  if Overwrite then
    SL.Assign(fStrings)
  else
    SL.AddStrings(fStrings);
end;

function TIStringList.Count: Integer;
  {Gets number of items in list.
    @return Required number of items.
  }
begin
  Result := fStrings.Count;
end;

constructor TIStringList.Create;
  {Class constructor. Creates new empty list.
  }
begin
  inherited Create;
  fStrings := TStringList.Create;
end;

constructor TIStringList.Create(const Strs: TStrings);
  {Class constructor. Creates new list containing specified strings.
    @param Strs [in] List of strings to be stored in list.
  }
begin
  Create;
  Add(Strs);
end;

constructor TIStringList.Create(const Str: string);
  {Class constructor. Creates new list containing a single string.
    @param Str [in] String to be included in list.
  }
begin
  Create;
  Add(Str);
end;

constructor TIStringList.Create(const Strs: IStringList);
  {Class constructor. Creates new list containing strings from another
  IStringList instance.
    @param Strs [in] List of strings to be stored in list.
  }
begin
  Create;
  Add(Strs);
end;

constructor TIStringList.Create(const Str, Delim: string;
  const AllowEmpty, Trim: Boolean);
  {Class constructor. Creates new list containing strings split at a delimiter.
    @param Str [in] String to be split.
    @param Delim [in] String that delimits components of string.
    @param AllowEmpty [in] Determines whether empty components are stored in
      list (True) or ignored (False).
    @param Trim [in] Determines whether strings are trimmed of trailing and
      leading spaces before adding to list.
  }
begin
  Create;
  Add(Str, Delim, AllowEmpty, Trim);
end;

constructor TIStringList.Create(const Strs: array of string);
  {Class constructor. Creates new list containing strings from array.
    @param Strs [in] Array of strings to be included in list.
  }
begin
  Create;
  Add(Strs);
end;

procedure TIStringList.Delete(const Idx: Integer);
  {Deletes a string from list.
    @param Index of string to be deleted.
  }
begin
  fStrings.Delete(Idx);
end;

destructor TIStringList.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fStrings.Free;
  inherited;
end;

function TIStringList.GetCaseSensitive: Boolean;
  {Checks whether searching in string list is to be case sensitive.
    @return True if searching is case sensitive, False if not case sensitive.
  }
begin
  Result := fStrings.CaseSensitive;
end;

function TIStringList.GetEnumerator: TStringsEnumerator;
  {Creates an enumerator for the string list.
    @return Enumerator instance.
  }
begin
  Result := fStrings.GetEnumerator;
end;

function TIStringList.GetItem(const Idx: Integer): string;
  {Gets string at a given position in list.
    @param Idx [in] Index of required string. Must be in range.
    @return Required string.
  }
begin
  Result := fStrings[Idx];
end;

function TIStringList.GetText(const Glue: string;
  const AllowEmpty: Boolean): string;
  {Gets contents of list as single string with items joined by a string.
    @param Glue [in] String used to glue list items together.
    @param AllowEmpty [in] Determines whether empty items are to be included in
      rendered text.
    @return Required text string.
  }
begin
  Result := StrJoin(fStrings, Glue, AllowEmpty);
end;

function TIStringList.IClonableClone: IInterface;
begin
  Result := Clone;
end;

function TIStringList.IndexOf(const Str: string): Integer;
  {Gets index of a string in list.
    @param Str [in] String to be found.
    @return Index of string in list or -1 if string not found.
  }
begin
  Result := fStrings.IndexOf(Str);
end;

function TIStringList.IsValidIndex(const Idx: Integer): Boolean;
  {Checks if an index into the string list is valid, i.e. it is in range and can
  be used as an index into the Items[] property without error.
    @param Idx [in] Index to check.
    @return True if index is valid and False if invalid.
  }
begin
  Result := (Idx >= 0) and (Idx <= Pred(Count));
end;

procedure TIStringList.SetCaseSensitive(const Flag: Boolean);
  {Sets search case sensitivity.
    @param Flag [in] True if case sensitive searching is required, False if case
      insensitive searching is required.
  }
begin
  fStrings.CaseSensitive := Flag;
end;

procedure TIStringList.SetItem(const Idx: Integer; const Str: string);
  {Sets string at a given position in list.
    @param Idx [in] Index in list to receive strings. Must be in range.
    @param Str [in] String to be stored in list.
  }
begin
  fStrings[Idx] := Str;
end;

procedure TIStringList.SetText(const Text: string; const Delim: string;
  const AllowEmpty: Boolean; const Trim: Boolean);
  {Sets list to component parts of a string, based on a delimiter.
    @param Str [in] String to be split.
    @param Delim [in] String that delimits components of string.
    @param AllowEmpty [in] Determines whether empty components are stored in
      list (True) or ignored (False).
    @param Trim [in] Determines whether strings are trimmed of trailing and
      leading spaces before adding to list.
  }
begin
  Clear;
  Add(Text, Delim, AllowEmpty, Trim);
end;

procedure TIStringList.Sort;
  {Sorts the string list alphabetically.
  }
begin
  fStrings.Sort;
end;

function TIStringList.ToArray: TArray<string>;
  {Copies strings from string list into an array of strings.
    @return Array of strings.
  }
begin
  Result := fStrings.ToStringArray;
end;

end.

