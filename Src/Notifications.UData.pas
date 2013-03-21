{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a record that stores information to be displayed in a notification
 * window along with related actions that can be performed from the window.
}


unit Notifications.UData;


interface


uses
  SysUtils, Classes;


type
  ///  <summary>Stores information to be displayed in a notification window
  ///  along with related actions that can be performed from the window.
  ///  </summary>
  ///  <remarks>
  ///  <para>A notification always has a title and at least one paragraph of
  ///  content text.</para>
  ///  <para> An associated action, help keyword and the facility to inhibit
  ///  similar notifications in future are all optional.</para>
  ///  </remarks>
  TNotificationData = record
  strict private
    var
      ///  <summary>Value of Title property.</summary>
      fTitle: string;
      ///  <summary>Value of Content property.</summary>
      fContent: TArray<string>;
      ///  <summary>Value of HelpKeyword property.</summary>
      fHelpKeyword: string;
      ///  <summary>Value of Action property.</summary>
      fAction: TBasicAction;
      ///  <summary>Value of InhibitCallback property.</summary>
      fInhibitCallback: TProc;

  public
    ///  <summary>Title of notification.</summary>
    property Title: string read fTitle;

    ///  <summary>Notification content.</summary>
    ///  <remarks>Each array element is a paragraph of text.</remarks>
    property Content: TArray<string> read fContent;

    ///  <summary>Keyword of any related help topic.</summary>
    ///  <remarks>Set to the empty string to inhibit display of help.</remarks>
    property HelpKeyword: string read fHelpKeyword;

    ///  <summary>Action triggered by user from notification window.</summary>
    ///  <remarks>Set to nil if no acction is associated with notification.
    ///  </remarks>
    property Action: TBasicAction read fAction;

    ///  <summary>Callback procedure called to inhibit future notifications of
    ///  this kind.</summary>
    ///  <remarks>Set to nil if notification can't be inhibited.</remarks>
    property InhibitCallback: TProc read fInhibitCallback;

    ///  <summary>Creates an initialised notification data record.</summary>
    ///  <param name="ATitle">string [in] Notification title. Must be empty
    ///  string or contain only white space.</param>
    ///  <param name="AContent">array of string [in] Paragraphs of text forming
    ///  content of notification. Must contain at least one element that
    ///  contains non-whitespace text. Empty elements or elements containing
    ///  only white space are ignored.</param>
    ///  <param name="AHelpKeyword">string [in] A-link keyword of associated
    ///  help topic or empty string if there is no such help topic.</param>
    ///  <param name="AAction">TBasicAction [in] Action that can be triggered
    ///  from notification window or nil if there is no such action.</param>
    ///  <param name="AInhibitCallback">TProc [in] Procedure to be called from
    ///  notification window if user chooses to inhibit similar messages in
    ///  future, or nil if message can't be inhibited.</param>
    constructor Create(const ATitle: string; const AContent: array of string;
      const AHelpKeyord: string; const AAction: TBasicAction;
      const AInhibitCallback: TProc);
  end;

implementation

{ TNotificationData }

constructor TNotificationData.Create(const ATitle: string;
  const AContent: array of string; const AHelpKeyord: string;
  const AAction: TBasicAction; const AInhibitCallback: TProc);
var
  I: Integer;
begin
  Assert(ATitle <> '', 'TNotificationData.Create: ATitle = empty string');
  Assert((Length(AContent) > 0),
    'TNotificationData.Create: AContent has no elements');
  fTitle := ATitle;
  SetLength(fContent, Length(AContent));
  for I := 0 to Pred(Length(AContent)) do
    fContent[I] := AContent[I];
  fHelpKeyword := AHelpKeyord;
  fAction := AAction;
  fInhibitCallback := AInhibitCallback;
end;

end.
