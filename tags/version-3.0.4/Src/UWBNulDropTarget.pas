{ ##
  @FILE                     UWBNulDropTarget.pas
  @COMMENTS                 Contains class that implements IDropTarget interface
                            that is designed to be associated with a web browser
                            control to inhibit drag and drop operations.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None
  @HISTORY(
    @REVISION(
      @VERSION              0.1
      @DATE                 16/04/2006
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.0
      @DATE                 24/05/2006
      @COMMENTS             Improved and corrected comments.
    )
  )
}


{
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
 * The Original Code is UWBNulDropTarget.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit UWBNulDropTarget;


interface


uses
  // Delphi
  ActiveX, Windows;


type

  {
  TWBDropTarget:
    Implementation of IDropTarget designed to be associated with a web browser
    control to inhibit drag and drop operations.
  }
  TWBNulDropTarget = class(TInterfacedObject,
    IDropTarget
  )
  protected
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


{ TWBNulDropTarget }

function TWBNulDropTarget.DragEnter(const dataObj: IDataObject;
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

function TWBNulDropTarget.DragLeave: HResult;
  {Removes target feedback and releases the data object. We do nothing.
    @return S_OK if method completes successfully or error values otherwise.
  }
begin
  Result := S_OK;
end;

function TWBNulDropTarget.DragOver(grfKeyState: Integer; pt: TPoint;
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

function TWBNulDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Integer;
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
