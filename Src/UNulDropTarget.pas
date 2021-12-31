{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Contains class that implements the IDropTarget interface that is designed to
 * inhibit all drag and drop operations.
}


unit UNulDropTarget;


interface


uses
  // Delphi
  ActiveX, Windows;


type

  {
  TNulDropTarget:
    Implementation of IDropTarget designed to inhibit all drag and drop
    operations.
  }
  TNulDropTarget = class(TInterfacedObject,
    IDropTarget
  )
  public
    { IDropTarget methods }
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
      {Determines whether a drop can be accepted and its effect if it is
      accepted. We indicate drops not accepted and do nothing else.
        @param dataObj Data object being transferred in the drag-and-drop
          operation.
        @param grfKeyState Current state of keyboard modifier keys.
        @param pt Cursor co-ordinates in drop target window.
        @param dwEffect Specifies current effect flag per DROPEFFECT_* flags.
        @return S_OK if method completes successfully or error values otherwise.
      }
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
      {Provides target feedback to the user as mouse moves over drop window. We
      indicate that drop is not accepted.
        @param grfKeyState Current state of keyboard modifier keys.
        @param pt Cursor co-ordinates in drop target window.
        @param dwEffect Specifies current effect flag per DROPEFFECT_* flags.
        @return S_OK if method completes successfully or error values otherwise.
      }
    function DragLeave: HResult; stdcall;
      {Removes target feedback and releases the data object. We do nothing.
        @return S_OK if method completes successfully or error values otherwise.
      }
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
      {Called when user drops data object on drop window. Incorporates source
      data into target window, removes target feedback and releases data object.
      In this case we no nothing.
        @param dataObj Data object being transferred in the drag-and-drop
          operation.
        @param grfKeyState Current state of keyboard modifier keys.
        @param pt Cursor co-ordinates in drop target window.
        @param dwEffect Specifies current effect flag per DROPEFFECT_* flags.
        @return S_OK if method completes successfully or error values otherwise.
      }
  end;


implementation


{ TNulDropTarget }

function TNulDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
  {Determines whether a drop can be accepted and its effect if it is accepted.
  We indicate drops not accepted and do nothing else.
    @param dataObj Data object being transferred in the drag-and-drop operation.
    @param grfKeyState Current state of keyboard modifier keys.
    @param pt Cursor co-ordinates in drop target window.
    @param dwEffect Specifies current effect flag per DROPEFFECT_* flags.
    @return S_OK if method completes successfully or error values otherwise.
  }
begin
  // Indicate we don't accept drag-drop
  dwEffect := DROPEFFECT_NONE;  // "no entry" drop cursor
  Result := S_OK;
end;

function TNulDropTarget.DragLeave: HResult;
  {Removes target feedback and releases the data object. We do nothing.
    @return S_OK if method completes successfully or error values otherwise.
  }
begin
  Result := S_OK;
end;

function TNulDropTarget.DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
  {Provides target feedback to the user as mouse moves over drop window. We
  indicate that drop is not accepted.
    @param grfKeyState Current state of keyboard modifier keys.
    @param pt Cursor co-oridinates in drop target window.
    @param dwEffect Specifies current effect flag per DROPEFFECT_* flags.
    @return S_OK if method completes successfully or error values otherwise.
  }
begin
  // Indicate we don't accept drag-drop
  dwEffect := DROPEFFECT_NONE;  // "no entry" drop cursor
  Result := S_OK;
end;

function TNulDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: Integer): HResult;
  {Called when user drops data object on drop window. Incorporates source data
  into target window, removes target feedback and releases data object. In this
  case we no nothing.
    @param dataObj Data object being transferred in the drag-and-drop operation.
    @param grfKeyState Current state of keyboard modifier keys.
    @param pt Cursor co-oridinates in drop target window.
    @param dwEffect Specifies current effect flag per DROPEFFECT_* flags.
    @return S_OK if method completes successfully or error values otherwise.
  }
begin
  // Indicate we don't accept drag-drop
  dwEffect := DROPEFFECT_NONE;  // "no entry" drop cursor
  Result := S_OK;
end;

end.

