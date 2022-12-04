{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides various class helpers for VCL classes.
}


unit UClassHelpers;

{ TODO: Separate different helpers into their own units, within a ClassHelpers
        scope. E.g. ClassHelpers.Controls, ClassHelper.Graphics }

interface


uses
  // Delphi
  Controls, Menus, ImgList, Graphics, ActnList, GIFImg;


type
  ///  <summary>Class helper that adds functionality to TControl.</summary>
  TControlHelper = class helper for TControl
  public
    ///  <summary>Gets reference to pop-up menu assigned to protected PopupMenu
    ///  property.</summary>
    function GetPopupMenu: TPopupMenu;
    ///  <summary>Checks if protected PopupMenu property is assigned.</summary>
    function HasPopupMenu: Boolean;
    ///  <summary>Refreshes control's action. Any changes in action that affect
    ///  state of control are reflected in control.</summary>
    procedure RefreshAction;
    ///  <summary>Refreshes all owned controls to reflect any changes in their
    ///  associated actions.</summary>
    procedure RefreshActions;
  end;

type
  ///  <summary>Class helper that adds a method to TCustomImageList that can
  ///  load images from resources of any type into the image list.</summary>
  ///  <remarks>By default TCustomImageList can only load images from Bitmap,
  ///  Cursor or Icon resources. Because of limitations of the resource compiler
  ///  used to build CodeSnip, 32 bit bitmaps can't be added to BITMAP resources
  ///  and so are stored in a format not supported by TCustomImageList.
  ///  </remarks>
  TImageListHelper = class helper for TCustomImageList
  public
    ///  <summary>Loads images from given resource.</summary>
    ///  <param name="ResType">PChar [in] Type of resource.</param>
    ///  <param name="ResName">string [in] Name of resource.</param>
    ///  <param name="Size">Integer [in] Size of individual images in resource
    ///  (must be square).</param>
    ///  <param name="MaskColour">TColor [in] Colour used as transparency mask.
    ///  </param>
    ///  <remarks>Resource is loaded from the current program instance.
    ///  </remarks>
    procedure LoadFromResource(ResType: PChar; const ResName: string;
      Size: Integer; MaskColour: TColor);
  end;

type
  ///  <summary>Class helper that adds a method to TCustomActionList that can
  ///  update all the actions in the list.</summary>
  TActionListHelper = class helper for TCustomActionList
  public
    ///  <summary>Updates all actions in the action list by calling their Update
    ///  methods.</summary>
    procedure Update;
  end;

type
  ///  <summary>Class helper that adds a method to TGIFImage that adds a similar
  ///  method to one present in 3rd party TGIFImage to load an image from
  ///   resources.</summary>
  TGIFImageHelper = class helper for TGIFImage
  public
    ///  <summary>Load a GIF image from given resource.</summary>
    ///  <param name="Module">HINSTANCE [in] Module containing resource.</param>
    ///  <param name="ResName">string [in] Name of resource to be loaded.
    ///  </param>
    ///  <param name="ResType">PChar [in] Type of resource to be loaded.</param>
    procedure LoadFromResource(const Module: HMODULE; const ResName: string;
      const ResType: PChar);
  end;


implementation


uses
  // Delphi
  Actions, Classes;


{ TControlHelper }

function TControlHelper.GetPopupMenu: TPopupMenu;
begin
  Result := PopupMenu;
end;

function TControlHelper.HasPopupMenu: Boolean;
begin
  Result := Assigned(PopupMenu);
end;

procedure TControlHelper.RefreshAction;
begin
  if Assigned(Action) then
    ActionChange(Action, False);
end;

procedure TControlHelper.RefreshActions;
var
  Idx: Integer; // loops through all controls
begin
  for Idx := 0 to Pred(ComponentCount) do
    if Components[Idx] is TControl then
      (Components[Idx] as TControl).RefreshAction;
end;

{ TImageListHelper }

procedure TImageListHelper.LoadFromResource(ResType: PChar;
  const ResName: string; Size: Integer; MaskColour: TColor);
var
  MasterBmp: TBitmap;   // bitmap containing all images
  Bmp: TBitmap;         // a bitmap of a single image
  RS: TResourceStream;  // stream used to resources
  LeftOffset: Integer;  // left offset of an image in master
  TopOffset: Integer;   // top offset of an image in master

begin
  // Set required size of images in image list
  Width := Size;
  Height := Size;
  // Load master bitmap containing all images from resources.
  MasterBmp := TBitmap.Create;
  try
    RS := TResourceStream.Create(HInstance, ResName, ResType);
    try
      MasterBmp.LoadFromStream(RS);
    finally
      RS.Free;
    end;
    // Split master bitmap into individual bitmaps and load them into image list
    // create bitmap of correct size and bit depth: we re-use it for each bitmap
    Bmp := TBitmap.Create;
    try
      Bmp.Width := Width;
      Bmp.Height := Height;
      Bmp.PixelFormat := MasterBmp.PixelFormat;
      // scan across then down bitmaps: works for strips or rectangular master
      // bitmaps
      TopOffset := 0;
      while TopOffset < MasterBmp.Height do
      begin
        LeftOffset := 0;
        while LeftOffset < MasterBmp.Width do
        begin
          // copy the bitmap from the master into the image list
          Bmp.Canvas.CopyRect(
            Rect(0, 0, Width, Height),
            MasterBmp.Canvas,
            Bounds(LeftOffset, TopOffset, Width, Height)
          );
          Self.AddMasked(Bmp, clFuchsia);
          Inc(LeftOffset, Width);
        end;
        Inc(TopOffset, Height);
      end;
    finally
      Bmp.Free;
    end;
  finally
    MasterBmp.Free;
  end;
end;

{ TActionListHelper }

procedure TActionListHelper.Update;
var
  Action: TContainedAction; // each action in list
begin
  for Action in Self do
    Action.Update;
end;

{ TGIFImageHelper }

procedure TGIFImageHelper.LoadFromResource(const Module: HMODULE;
  const ResName: string; const ResType: PChar);
var
  Stm: TResourceStream;
begin
  Stm := TResourceStream.Create(Module, ResName, ResType);
  try
    LoadFromStream(Stm);
  finally
    Stm.Free;
  end;
end;

end.

