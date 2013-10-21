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
 * window along with a related task that can be performed from the window.
}


unit Notifications.UData;


interface


uses
  // Delphi
  SysUtils;


type
  ///  <summary>Stores information to be displayed in a notification window
  ///  along with a related task that can be performed from the window.
  ///  </summary>
  ///  <remarks>
  ///  <para>A notification always has a title and at least one paragraph of
  ///  content text.</para>
  ///  <para>An associated task and help keyword are optional.</para>
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
      ///  <summary>Valure of TaskCallback property.</summary>
      fTaskCallback: TProc;
      ///  <summary>Value of TaskPrompt property.</summary>
      fTaskPrompt: string;
  public
    ///  <summary>Title of notification.</summary>
    property Title: string read fTitle;

    ///  <summary>Notification content.</summary>
    ///  <remarks>Each array element is a paragraph of text.</remarks>
    property Content: TArray<string> read fContent;

    ///  <summary>Keyword of any related help topic.</summary>
    ///  <remarks>Set to the empty string to inhibit display of help.</remarks>
    property HelpKeyword: string read fHelpKeyword;

    ///  <summary>Callback procedure called from notification window when user
    ///  clicks the task button.</summary>
    ///  <remarks>Note that no task button will appear if this property is nil.
    ///  </remarks>
    property TaskCallback: TProc read fTaskCallback;

    ///  <summary>Prompt text that appears in notification window's task button.
    ///  </summary>
    ///  <remarks>This property is ignored if TaskCallback is nil.</remarks>
    property TaskPrompt: string read fTaskPrompt;

    ///  <summary>Creates an initialised notification data record.</summary>
    ///  <param name="ATitle">string [in] Notification title. Must be empty
    ///  string or contain only white space.</param>
    ///  <param name="AContent">array of string [in] Paragraphs of text forming
    ///  content of notification. Must contain at least one element that
    ///  contains non-whitespace text. Empty elements or elements containing
    ///  only white space are ignored.</param>
    ///  <param name="AHelpKeyword">string [in] A-link keyword of associated
    ///  help topic or empty string if there is no such help topic.</param>
    ///  <param name="ATaskCallback">TProc [in] Procedure to be called from
    ///  notification window if user clicks the task button, or nil if no
    ///  such button is required.</param>
    ///  <param name="ATaskPrompt">string [in] Prompt or caption to appear on
    ///  any task button. May be empty string if ATaskCallback is nil.</param>
    ///  <param name="AInhibitCallback">TProc [in] Procedure to be called from
    ///  notification window if user chooses to inhibit similar messages in
    ///  future, or nil if message can't be inhibited.</param>
    constructor Create(const ATitle: string; const AContent: array of string;
      const AHelpKeyord: string; const ATaskCallback: TProc;
      const ATaskPrompt: string);
  end;

implementation

uses
  // Project
  UContainers;

{ TNotificationData }

constructor TNotificationData.Create(const ATitle: string;
  const AContent: array of string; const AHelpKeyord: string;
  const ATaskCallback: TProc; const ATaskPrompt: string);
begin
  Assert(ATitle <> '', 'TNotificationData.Create: ATitle is empty string');
  Assert((Length(AContent) > 0),
    'TNotificationData.Create: AContent has no elements');
  fTitle := ATitle;
  fContent := TArrayHelper.Copy<string>(AContent);
  fHelpKeyword := AHelpKeyord;
  fTaskCallback := ATaskCallback;
  fTaskPrompt := ATaskPrompt;
end;

end.

