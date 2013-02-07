{
 * PJShellFolders.pas
 *
 * Run time unit that defines shell folders components, classes and routines.
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
 * The Original Code is PJShellFolders.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   Philippe Lucarz
 *   Ryan Fischbach
 *
 * ***** END LICENSE BLOCK *****
}


unit PJShellFolders;


interface


// Determine compiler
{$UNDEF DELPHI6ANDUP}
{$UNDEF DELPHI7ANDUP}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$DEFINE DELPHI6ANDUP}
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    {$DEFINE DELPHI7ANDUP}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$IFEND}
{$ENDIF}


uses
  // Delphi
  SysUtils, Windows, Classes, Controls, Messages, ShlObj
  {$IFDEF DELPHI6ANDUP}
  // include this unit for extra shell folder identifiers
  , SHFolder
  {$ENDIF}
  ;


{$IFNDEF DELPHI6ANDUP}
const
  //
  // CSIDL_ constants provided in SHFolder unit in Delphi 6 and later
  //
  // Folder ids
  CSIDL_LOCAL_APPDATA             = $001C;
  {$EXTERNALSYM CSIDL_LOCAL_APPDATA}
  CSIDL_COMMON_APPDATA            = $0023;
  {$EXTERNALSYM CSIDL_COMMON_APPDATA}
  CSIDL_WINDOWS                   = $0024;
  {$EXTERNALSYM CSIDL_WINDOWS}
  CSIDL_SYSTEM                    = $0025;
  {$EXTERNALSYM CSIDL_SYSTEM}
  CSIDL_PROGRAM_FILES             = $0026;
  {$EXTERNALSYM CSIDL_PROGRAM_FILES}
  CSIDL_MYPICTURES                = $0027;
  {$EXTERNALSYM CSIDL_MYPICTURES}
  CSIDL_PROGRAM_FILES_COMMON      = $002B;
  {$EXTERNALSYM CSIDL_PROGRAM_FILES_COMMON}
  CSIDL_COMMON_DOCUMENTS          = $002E;
  {$EXTERNALSYM CSIDL_COMMON_DOCUMENTS}
  CSIDL_COMMON_ADMINTOOLS         = $002F;
  {$EXTERNALSYM CSIDL_COMMON_ADMINTOOLS}
  CSIDL_ADMINTOOLS                = $0030;
  {$EXTERNALSYM CSIDL_ADMINTOOLS}
  // Flag
  CSIDL_FLAG_CREATE               = $8000;
  {$EXTERNALSYM CSIDL_FLAG_CREATE}
{$ENDIF}

const
  //
  // Further CSIDL constants from MSDN not defined in all Delphis
  //
  // Folder ids
  CSIDL_CDBURN_AREA               = $003B;
  {$EXTERNALSYM CSIDL_CDBURN_AREA}
  CSIDL_COMMON_MUSIC              = $0035;
  {$EXTERNALSYM CSIDL_COMMON_MUSIC}
  CSIDL_COMMON_PICTURES           = $0036;
  {$EXTERNALSYM CSIDL_COMMON_PICTURES}
  CSIDL_COMMON_TEMPLATES          = $002D;
  {$EXTERNALSYM CSIDL_COMMON_TEMPLATES}
  CSIDL_COMMON_VIDEO              = $0037;
  {$EXTERNALSYM CSIDL_COMMON_VIDEO}
  CSIDL_COMPUTERSNEARME           = $003D;
  {$EXTERNALSYM CSIDL_COMPUTERSNEARME}
  CSIDL_CONNECTIONS               = $0031;
  {$EXTERNALSYM CSIDL_CONNECTIONS}
  CSIDL_MYDOCUMENTS               = $000C;
  {$EXTERNALSYM CSIDL_MYDOCUMENTS}
  CSIDL_MYMUSIC                   = $000D;
  {$EXTERNALSYM CSIDL_MYMUSIC}
  CSIDL_MYVIDEO                   = $000E;
  {$EXTERNALSYM CSIDL_MYVIDEO}
  CSIDL_PROFILE                   = $0028;
  {$EXTERNALSYM CSIDL_PROFILE}
  CSIDL_PROFILES                  = $003E;
  {$EXTERNALSYM CSIDL_PROFILES}
  CSIDL_SYSTEMX86                 = $0029;
  {$EXTERNALSYM CSIDL_SYSTEMX86}
  CSIDL_PROGRAM_FILESX86          = $002A;
  {$EXTERNALSYM CSIDL_PROGRAM_FILESX86}
  CSIDL_PROGRAM_FILES_COMMONX86   = $002C;
  {$EXTERNALSYM CSIDL_PROGRAM_FILES_COMMONX86}
  CSIDL_RESOURCES                 = $0038;
  {$EXTERNALSYM CSIDL_RESOURCES}
  CSIDL_RESOURCES_LOCALIZED       = $0039;
  {$EXTERNALSYM CSIDL_RESOURCES_LOCALIZED}
  CSIDL_COMMON_OEM_LINKS          = $003A;
  {$EXTERNALSYM CSIDL_COMMON_OEM_LINKS}
  // Flags
  CSIDL_FOLDER_MASK               = $00FF;
  {$EXTERNALSYM CSIDL_FOLDER_MASK}
  CSIDL_FLAG_PER_USER_INIT        = $0080;
  {$EXTERNALSYM CSIDL_FLAG_PER_USER_INIT}
  CSIDL_FLAG_NO_ALIAS             = $1000;
  {$EXTERNALSYM CSIDL_FLAG_NO_ALIAS}
  CSIDL_FLAG_DONT_VERIFY          = $4000;
  {$EXTERNALSYM CSIDL_FLAG_DONT_VERIFY}
  CSIDL_FLAG_DONT_UNEXPAND        = $2000;
  {$EXTERNALSYM CSIDL_FLAG_DONT_UNEXPAND}
  CSIDL_FLAG_MASK                 = $FF00;
  {$EXTERNALSYM CSIDL_FLAG_MASK}

  //
  // Browse dialog customisation flags
  //
  BIF_NEWDIALOGSTYLE = $0040;
  {$EXTERNALSYM BIF_NEWDIALOGSTYLE}
  BIF_UAHINT         = $0100;
  {$EXTERNALSYM BIF_UAHINT}


type
  {
  IPJSpecialFolderEnum:
    Interface to enumerator of the identifiers of the Shell's special folders.
  }
  IPJSpecialFolderEnum = interface
    ['{0958B8A0-1D56-11D5-852A-EE0AA7BFE914}']
    procedure Init;
      {Intialises enumeration.
      }
    function Next: Integer;
      {Gets next special folder identifier in enumeration, or -1 if at end
      of enumeration.
        @return Folder identifier.
      }
    function AtEnd: Boolean;
      {Checks if at end of enumeration.
        @return True if at end of enumeration and false otherwise.
      }
    function Count: Integer;
      {Gets number of folder ids in enumeration.
        @return Number of folders ids.
      }
  end;

  {
  TPJSpecialFolderEnum
    Class that enumerates the indentifiers for the Shell's special folders.
  }
  TPJSpecialFolderEnum = class(TInterfacedObject,
    IPJSpecialFolderEnum
  )
  private
    fIndex: Integer;  // Index of current folder in folder lookup table
  public
    constructor Create;
      {Class constructor. Sets up object and intialises enumeration.
      }
    procedure Init;
      {Intialises enumeration.
      }
    function Next: Integer;
      {Gets next special folder identifier in enumeration, or -1 if at end of
      enumeration.
        @return Folder identifier.
      }
    function AtEnd: Boolean;
      {Checks if at end of enumeration.
        @return True if at end of enumeration and false otherwise.
      }
    function Count: Integer;
      {Gets number of folder ids in enumeration.
        @return Number of folders ids.
      }
  end;

  {
  TPJSpecialFolderInfo:
    Component that provides information about the Shell's special folders.
  }
  TPJSpecialFolderInfo = class(TComponent)
  private
    fFolderID: Integer;     // Value of FolderID property
    fPath: string;          // Value of Path property
    fDisplayName: string;   // Value of DisplayName property
    fIsVirtual: Boolean;    // Value of IsVirtual property
    fIsSupported: Boolean;  // Value of IsSupported property
    procedure SetFolderID(const Value: Integer);
      {Write accessor method for FolderID property. Reads information about
      specified folder.
        @param Value [in] New property value.
        @except EPJShellFolders raised if Value is not a valid special folder
          id.
      }
  protected
    procedure GetCurrentFolderInfo; virtual;
      {Retrieves information about special folder specified by FolderID property
      and updates read only properties accordingly.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object and reads information about default
      special folder.
        @param AOwner [in] Component's owner.
      }
    property Path: string read fPath;
      {The path to the special folder. This is '' if folder is virtual or not
      supported}
    property DisplayName: string read fDisplayName;
      {Display name of special folder. This is '' if folder is not supported}
    property IsVirtual: Boolean read fIsVirtual;
      {True if the special folder is virtual - i.e. not part of the physical
      file system. False if folder is part of virtual file system or is not
      supported on this system}
    property IsSupported: Boolean read fIsSupported;
      {True if the current folder ID is supported on the underlying OS, false
      otherwise}
  published
    property FolderID: Integer
      read fFolderID write SetFolderID default CSIDL_DESKTOP;
      {ID of the the current folder. Setting this property causes other, read
      only properties to be updated to provide information about the folder}
  end;

  {
  TPJBrowseSelChangeEvent:
    Type of event triggered by TPJBrowseDialog when selected folder changes.
      @param Sender [in] Reference to component triggering event.
      @param FolderName [in] Name of selected folder or '' if virtual folder.
      @param DisplayName [in] Display name of selected folder.
      @param StatusText [in/out] Set to '' when called. May be updated to
        provide status text to be displayed in old style browse dialog. Ignored
        by new style dialog box.
      @param OKEnabled [in/out] Set to state of browse dialog's OK button when
        called. May be set true to enable OK button or false to disable it.
  }
  TPJBrowseSelChangeEvent = procedure(Sender: TObject;
    FolderName, DisplayName: string; var StatusText: string;
    var OKEnabled: Boolean) of object;

  {
  TPJBrowseSelChangeEventEx:
    Type of event triggered by TPJBrowseDialog when selected folder changes:
    gives access to folder's PIDL.
      @param Sender [in] Reference to component triggering event.
      @param PIDL [in] PIDL representing selected folder.
      @param StatusText [in/out] Set to '' when called. May be updated to
        provide status text to be displayed in old style browse dialog. Ignored
        by new style dialog box.
      @param OKEnabled [in/out] Set to state of browse dialog's OK button when
        called. May be set true to enable OK button or false to disable it.
  }
  TPJBrowseSelChangeEventEx = procedure(Sender: TObject;
    PIDL: PItemIDList; var StatusText: string;
    var OKEnabled: Boolean) of object;

  {
  TPJBrowseValidationFailedEvent:
    Type of event triggered by TPJBrowseDialog when an invalid folder path is
    entered in the dialog's edit control and the dialog is closed.
      @param Sender [in] Reference to component triggering event.
      @param EditText [in] Text entered in browse dialog's edit control.
      @param CanClose [in/out] True when handler called: permits dialog box to
        close. Change to False to prevent dialog from closing.
  }
  TPJBrowseValidationFailedEvent = procedure(Sender: TObject;
    const EditText: string; var CanClose: Boolean) of object;

  {
  TPJBrowseHelpEvent:
    Type of event triggered by TPJBrowserDialg when help is requested.
      @param Sender [in] Reference to component triggering event.
      @param Cancel [in/out] False when handler called: permits help request to
        be passed to help system via Application object. Change to True to
        prevent Application object handling help request: request must be
        handled in event handler.
  }
  TPJBrowseHelpEvent = procedure(Sender: TObject; var Cancel: Boolean)
    of object;

  {
  TPJBrowseDlgOption:
    Enumeration of options available to Options property of TPJBrowseDlg.
  }
  TPJBrowseDlgOption = (
    boShowHelp,           // show help button
                          // (old style dialog only)
    boContextHelp,        // show context help icon in title
                          // (not available on Vista & later)
    boStatusText,         // show status text in dlg box
                          // (old style dialog only)
    boDirsOnly,           // only allow selection of items in file system
    boNewDlgStyle,        // use new dialog style
                          // (requires shlobj.dll v5 or later)
    boHideMakeFolderBtn,   // hide Make New Folder button
                          // (new style dialog only)
    boEditBox,            // display folder edit box
    boHint                // display usage hint
                          // (new style dialog only, no boEditBox)
                          // (requires v6 of shlobj.dll)
  );

  {
  TPJBrowseDlgOptions:
    Set of options available to Options property of TPJBrowseDlg.
  }
  TPJBrowseDlgOptions = set of TPJBrowseDlgOption;

  {
  TPJBrowseDialog:
    Displays browse dialog box.
  }
  TPJBrowseDialog = class(TComponent)
  private
    fFolderName: string;            // Value of FolderName property
    fHeadline: string;              // Value of Headline property
    fDisplayName: string;           // Value of DisplayName property
    fRootFolderID: Integer;         // Value of RootFolderID property
    fTitle: TCaption;               // Value of Title property
    fOptions: TPJBrowseDlgOptions;  // Value of Options property
    fHelpContext: THelpContext;     // Value of HelpContext property
    {$IFDEF DELPHI6ANDUP}
    fHelpType: THelpType;           // Value of HelpType property
    fHelpKeyword: string;           // Value of HelpKeyword property
    {$ENDIF}
    fOnInitialise: TNotifyEvent;    // References OnInitialise event handler
    fOnSelChange:                   // References OnSelChange event handler
      TPJBrowseSelChangeEvent;
    fOnSelChangeEx:                 // References OnSelChangeEx event handler
      TPJBrowseSelChangeEventEx;
    fOnClose: TNotifyEvent;         // References OnClose event handler
    fOnValidationFailed:            // Ref to OnValidationFailed event handler
      TPJBrowseValidationFailedEvent;
    fOnHelp: TPJBrowseHelpEvent;    // Reference to OnHelp event handler
    fData:                          // Info passed to and from callback proc
      array[1..SizeOf(THandle) + SizeOf(Pointer)] of Byte;
    fOldBrowseWndProc: Pointer;     // Address of dialog's original winproc
    fNewBrowseWndProc: Pointer;     // Address of dialog's new window procedure
    function GetHandle: THandle;
      {Read accessor for Handle property.
        @return Handle to browse dialog box while Execute method is running or 0
          otherwise.
      }
    procedure SetRootFolderID(const Value: Integer);
      {Write accessor for RootFolderID property.
        @param Value [in] New property value.
        @except EPJShellFolders raised if Value is not a valid special folder
          id.
      }
  protected
    function IsNewStyle: Boolean;
      {Checks if a new style dialog box being displayed.
        @return True if new style dialog or False if not.
      }
    function IsDlgBtnEnabled(const BtnID: Integer): Boolean;
      {Checks if a browse dialog button is enabled.
        @return True if button enabled, False if disabled.
      }
    function IsHelpAvailable: Boolean;
      {Checks if any help is available per component's properties.
        @return True if help is available.
      }
    procedure DoHelp;
      {Triggers a help request if help is available. OnHelp event handler is
      called if assigned. Application help is called if request not cancelled in
      any OnHelp event handler.
      }
    function GetHWND: THandle;
      {Gets window handle of any TWinControl that owns this component.
        @return Owner handle or 0 if owner is nil or not a TWinControl.
      }
    procedure BrowseWndProc(var Msg: TMessage); virtual;
      {Window procedure used to subclass browse dlg box.
        @param Msg [in] Message to be handled by window procedure.
      }
    procedure InitBrowseWindow;
      {Initialises the browse dialog box. This method is called from the
      dialog's callback function.
      }
    procedure SelectionChanged(PIDL: PItemIDList);
      {Triggers OnSelChange and OnSelChangeEx events for currently selected
      folder. Updates status text and OK button according to values returned
      from the event handler. This method is called from the dialog's callback
      function.
        @param PIDL [in] PIDL representing currently selected folder.
      }
    function ValidationFailed(const EditText: string): Boolean;
      {Triggers browse dialog's OnValidationFailed event, if assigned and
      returns value indicating if dialog can close.
        @param EditText [in] Text from browser's edit control that references an
          invalid path.
        @return True if dialog box can close, False if not.
      }
    procedure IncludeHelpButton;
      {Creates a help button and displays it in the browse dlg box, providing
      that an old style dialog box is to be displayed. Other buttons are
      rearranged.
      }
    procedure HideMakeNewFolderButton;
      {Hides the dialog's Make New Folder button, providing that a new style
      dialog box is to be displayed.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up component and creates window procedure used to
      subclass then browse dialog box.
        @param AOwner [in] Dialog box's owner.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object and frees browse dialog's window
      procedure.
      }
    function Execute: Boolean;
      {Initialises and displays browse dialog box. Updates properties to record
      user's entry if dialog box is OKd. Some of the dialog's initialisation is
      performed in the dialog box's BrowseCallbackProc callback function.
        @return True if user OKs, False if dialog is cancelled.
      }
    property DisplayName: string read fDisplayName;
      {The display name of the selected folder}
    property Handle: THandle read GetHandle;
      {The window handle of the browse dlg box: this returns 0 if the dlg box is
      not currently displayed}
  published
    property Title: TCaption
      read fTitle write fTitle;
      {The dialog box's window title. If this property is not set (i.e. is the
      empty string) the dialog box displays 'Browse for Folder')}
    property FolderName: string
      read fFolderName write fFolderName;
      {The name of the folder chosen in the dialog box. Setting this property
      before executing the dialog box causes any valid folder to be highlighted
      in the dialog box}
    property RootFolderID: Integer
      read fRootFolderID write SetRootFolderID default CSIDL_DESKTOP;
      {ID of the folder to be displayed as the root in the dialog box. This can
      be any of the special shell folders. If a particular folder is not
      supported on the system then an exception is raised}
    property Headline: string
      read fHeadline write fHeadline;
      {"Headline" that appears in the body of the dialog box above the tree
      view}
    property Options: TPJBrowseDlgOptions
      read fOptions write fOptions default [boContextHelp, boDirsOnly];
      {Set of options that customise the appearance of the dialog box}
    property HelpContext: THelpContext
      read fHelpContext write fHelpContext default 0;
      {Numeric ID for components's context-sensitive help topic. Used when
      Options does not contain boContextHelp and F1 is pressed or when a help
      button is displayed and pressed. On supporting compilers HelpType must
      also be set to htContext}
    {$IFDEF DELPHI6ANDUP}
    property HelpKeyword: string
      read fHelpKeyword write fHelpKeyword;
      {Keyword for component's context-sensitive help topic. Used when HelpType
      is htKeyword and Options does not contain boContextHelp and F1 is pressed
      or when a help button is displayed and pressed}
    property HelpType: THelpType
      read fHelpType write fHelpType default htContext;
      {Indicates whether the component's context sensitive help topic is
      identified by context ID or by keyword}
    {$ENDIF}
    property OnInitialise: TNotifyEvent
      read fOnInitialise write fOnInitialise;
      {Event triggered when browse dlg box is initialised. This occurs after the
      window title is set and the initial selection is made. The dialog's window
      can be accessed via the Handle property}
    property OnSelChange: TPJBrowseSelChangeEvent
      read fOnSelChange write fOnSelChange;
      {Event triggered when the selection changes in the dialog box. The
      dialog's window can be accessed via the Handle property}
    property OnSelChangeEx: TPJBrowseSelChangeEventEx
      read fOnSelChangeEx write fOnSelChangeEx;
      {Event triggered when the selection changes in the dialog box, after
      OnSelChange. The dialog's window can be accessed via the Handle property}
    property OnClose: TNotifyEvent
      read fOnClose write fOnClose;
      {Event triggered when the browse dialog closes. The dialog's window can
      be accessed via the Handle property}
    property OnValidationFailed: TPJBrowseValidationFailedEvent
      read fOnValidationFailed write fOnValidationFailed;
      {Event triggered to check if browse dialog can close after user enters an
      invalid path in its edit control. Only triggered when boEditBox is
      included in Options}
    property OnHelp: TPJBrowseHelpEvent
      read fOnHelp write fOnHelp;
      {Event triggered when help is requested and is available. User can handle
      help request in this handler and either prevent or permit request to be
      passed to Application object}
  end;

  {
  EPJShellFolders:
    Class used for exceptions raised within this unit.
  }
  EPJShellFolders = class(Exception);

{ Special folder routines }

function SpecialFolderIdToStr(ID: Integer): string;
  {Gets name of constant representing a special folder.
    @param ID [in] Special folder id.
    @return Name of special folder id.
    @except EPJShellFolders raised if ID is not valid.
  }

function StrToSpecialFolderID(const IDStr: string): Integer;
  {Gets special folder id associated with a constant.
    @param IDStr [in] Name of special folder constant.
    @return Associated folder id.
    @except EPJShellFolders raised if IDStr is unknown.
  }

function IsValidSpecialFolderId(ID: Integer): Boolean;
  {Checks if a value is a valid special folder identifier.
    @param Id [in] Folder id to check.
    @return True if folder ID is valid, False if not.
  }

function NumSpecialFolderIds: Integer;
  {Gets number of supported special folder identifiers.
    @return Number of special folders.
  }

{ PIDL information routines }

function PIDLToFolderPath(PIDL: PItemIDList): string;
  {Gets the path of a folder from a PIDL.
    @param PIDL [in] PIDL containing path.
    @return Required folder path.
  }

function PIDLToFolderDisplayName(PIDL: PItemIDList): string;
  {Gets a display name of a folder from a PIDL.
    @param PIDL [in] PIDL containing display name.
    @return Required display name.
  }


implementation


uses
  // Delphi
  ActiveX, Forms, ShellAPI;


{ Error handling }

resourcestring
  {Error messages}
  sBadSpecialFolderID     = 'Invalid special folder ID';
  sBadSpecialFolderIDStr  = '"%s" is not a valid special folder ID constant';
  sNoRootFolder           = 'Root folder not supported on this system';

procedure Error(const Msg: string);
  {Raises exception.
    @param Msg [in] Exception message.
    @except EPJShellFolders exception.
  }
begin
  raise EPJShellFolders.Create(Msg);
end;

procedure ErrorFmt(const Msg: string; Args: array of const);
  {Raises exception.
    @param FmtStr [in] Message format string.
    @param Args [in] Array of arguments to format string.
    @except EPJShellFolder exception
  }
begin
  Error(Format(Msg, Args));
end;

{ PIDL information routines }

function PIDLToFolderPath(PIDL: PItemIDList): string;
  {Gets the path of a folder from a PIDL.
    @param PIDL [in] PIDL containing path.
    @return Required folder path.
  }
var
  Path: PChar;  // buffer to hold folder's path
begin
  Path := StrAlloc(MAX_PATH);
  try
    SHGetPathFromIDList(PIDL, Path);
    Result := Path;
  finally
    StrDispose(Path);
  end;
end;

function PIDLToFolderDisplayName(PIDL: PItemIDList): string;
  {Gets a display name of a folder from a PIDL.
    @param PIDL [in] PIDL containing display name.
    @return Required display name.
  }
var
  FileInfo: TSHFileInfo;  // file info passed back from SHGetFileInfo
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  SHGetFileInfo(
    PChar(PIDL),
    0,
    FileInfo,
    SizeOf(FileInfo),
    SHGFI_PIDL or SHGFI_DISPLAYNAME
  );
  Result := FileInfo.szDisplayName;
end;

{ Special folder identifier constants and routines }

const
  // Table mapping all special folder identifiers defined by Windows to string
  // representations of the constants
  cFolders: array[1..58] of record    // table of special folder IDs
    ID:   Integer;    // special folder identifier value
    Name: string;     // constant used to represent special folder
  end =
  (
    (ID: CSIDL_ADMINTOOLS;              Name: 'CSIDL_ADMINTOOLS';),
    (ID: CSIDL_ALTSTARTUP;              Name: 'CSIDL_ALTSTARTUP';),
    (ID: CSIDL_APPDATA;                 Name: 'CSIDL_APPDATA';),
    (ID: CSIDL_BITBUCKET;               Name: 'CSIDL_BITBUCKET';),
    (ID: CSIDL_CDBURN_AREA;             Name: 'CSIDL_CDBURN_AREA';),
    (ID: CSIDL_COMMON_ADMINTOOLS;       Name: 'CSIDL_COMMON_ADMINTOOLS';),
    (ID: CSIDL_COMMON_ALTSTARTUP;       Name: 'CSIDL_COMMON_ALTSTARTUP';),
    (ID: CSIDL_COMMON_APPDATA;          Name: 'CSIDL_COMMON_APPDATA';),
    (ID: CSIDL_COMMON_DESKTOPDIRECTORY; Name: 'CSIDL_COMMON_DESKTOPDIRECTORY';),
    (ID: CSIDL_COMMON_DOCUMENTS;        Name: 'CSIDL_COMMON_DOCUMENTS';),
    (ID: CSIDL_COMMON_FAVORITES;        Name: 'CSIDL_COMMON_FAVORITES';),
    (ID: CSIDL_COMMON_MUSIC;            Name: 'CSIDL_COMMON_MUSIC';),
    (ID: CSIDL_COMMON_OEM_LINKS;        Name: 'CSIDL_COMMON_OEM_LINKS';),
    (ID: CSIDL_COMMON_PICTURES;         Name: 'CSIDL_COMMON_PICTURES';),
    (ID: CSIDL_COMMON_PROGRAMS;         Name: 'CSIDL_COMMON_PROGRAMS';),
    (ID: CSIDL_COMMON_STARTMENU;        Name: 'CSIDL_COMMON_STARTMENU';),
    (ID: CSIDL_COMMON_STARTUP;          Name: 'CSIDL_COMMON_STARTUP';),
    (ID: CSIDL_COMMON_TEMPLATES;        Name: 'CSIDL_COMMON_TEMPLATES';),
    (ID: CSIDL_COMMON_VIDEO;            Name: 'CSIDL_COMMON_VIDEO';),
    (ID: CSIDL_COMPUTERSNEARME;         Name: 'CSIDL_COMPUTERSNEARME';),
    (ID: CSIDL_CONNECTIONS;             Name: 'CSIDL_CONNECTIONS';),
    (ID: CSIDL_CONTROLS;                Name: 'CSIDL_CONTROLS';),
    (ID: CSIDL_COOKIES;                 Name: 'CSIDL_COOKIES';),
    (ID: CSIDL_DESKTOP;                 Name: 'CSIDL_DESKTOP';),
    (ID: CSIDL_DESKTOPDIRECTORY;        Name: 'CSIDL_DESKTOPDIRECTORY';),
    (ID: CSIDL_DRIVES;                  Name: 'CSIDL_DRIVES';),
    (ID: CSIDL_FAVORITES;               Name: 'CSIDL_FAVORITES';),
    (ID: CSIDL_FONTS;                   Name: 'CSIDL_FONTS';),
    (ID: CSIDL_HISTORY;                 Name: 'CSIDL_HISTORY';),
    (ID: CSIDL_INTERNET;                Name: 'CSIDL_INTERNET';),
    (ID: CSIDL_INTERNET_CACHE;          Name: 'CSIDL_INTERNET_CACHE';),
    (ID: CSIDL_LOCAL_APPDATA;           Name: 'CSIDL_LOCAL_APPDATA';),
    (ID: CSIDL_MYDOCUMENTS;             Name: 'CSIDL_MYDOCUMENTS';),
    (ID: CSIDL_MYMUSIC;                 Name: 'CSIDL_MYMUSIC';),
    (ID: CSIDL_MYPICTURES;              Name: 'CSIDL_MYPICTURES';),
    (ID: CSIDL_MYVIDEO;                 Name: 'CSIDL_MYVIDEO';),
    (ID: CSIDL_NETHOOD;                 Name: 'CSIDL_NETHOOD';),
    (ID: CSIDL_NETWORK;                 Name: 'CSIDL_NETWORK';),
    (ID: CSIDL_PERSONAL;                Name: 'CSIDL_PERSONAL';),
    (ID: CSIDL_PRINTERS;                Name: 'CSIDL_PRINTERS';),
    (ID: CSIDL_PRINTHOOD;               Name: 'CSIDL_PRINTHOOD';),
    (ID: CSIDL_PROFILE;                 Name: 'CSIDL_PROFILE';),
    (ID: CSIDL_PROFILES;                Name: 'CSIDL_PROFILES';),
    (ID: CSIDL_PROGRAM_FILES;           Name: 'CSIDL_PROGRAM_FILES';),
    (ID: CSIDL_PROGRAM_FILESX86;        Name: 'CSIDL_PROGRAM_FILESX86';),
    (ID: CSIDL_PROGRAM_FILES_COMMON;    Name: 'CSIDL_PROGRAM_FILES_COMMON';),
    (ID: CSIDL_PROGRAM_FILES_COMMONX86; Name: 'CSIDL_PROGRAM_FILES_COMMONX86';),
    (ID: CSIDL_PROGRAMS;                Name: 'CSIDL_PROGRAMS';),
    (ID: CSIDL_RECENT;                  Name: 'CSIDL_RECENT';),
    (ID: CSIDL_RESOURCES;               Name: 'CSIDL_RESOURCES';),
    (ID: CSIDL_RESOURCES_LOCALIZED;     Name: 'CSIDL_RESOURCES_LOCALIZED'),
    (ID: CSIDL_SENDTO;                  Name: 'CSIDL_SENDTO';),
    (ID: CSIDL_STARTMENU;               Name: 'CSIDL_STARTMENU';),
    (ID: CSIDL_STARTUP;                 Name: 'CSIDL_STARTUP';),
    (ID: CSIDL_SYSTEM;                  Name: 'CSIDL_SYSTEM';),
    (ID: CSIDL_SYSTEMX86;               Name: 'CSIDL_SYSTEMX86';),
    (ID: CSIDL_TEMPLATES;               Name: 'CSIDL_TEMPLATES';),
    (ID: CSIDL_WINDOWS;                 Name: 'CSIDL_WINDOWS';)
  );

function NumSpecialFolderIds: Integer;
  {Gets number of supported special folder identifiers.
    @return Number of special folders.
  }
begin
  Result := High(cFolders) - Low(cFolders) + 1;
end;

function IsValidSpecialFolderId(ID: Integer): Boolean;
  {Checks if a value is a valid special folder identifier.
    @param Id [in] Folder id to check.
    @return True if folder ID is valid, False if not.
  }
var
  Idx: Integer; // loops through ID table
begin
  Result := False;
  for Idx := Low(cFolders) to High(cFolders) do
    if (cFolders[Idx].ID and not CSIDL_FLAG_CREATE) = ID then
    begin
      Result := True;
      Break;
    end;
end;

function SpecialFolderIdToStr(ID: Integer): string;
  {Gets name of constant representing a special folder.
    @param ID [in] Special folder id.
    @return Name of special folder id.
    @except EPJShellFolders raised if ID is not valid.
  }
var
  Idx: Integer; // loops thru ID table
begin
  // Assume no match
  Result := '';
  // Search for match to ID and get it's constant name
  for Idx := Low(cFolders) to High(cFolders) do
    if cFolders[Idx].ID and CSIDL_FOLDER_MASK = ID then
    begin
      Result := cFolders[Idx].Name;
      Break;
    end;
  // Raise exception if we didn't find a match
  if Result = '' then
    Error(sBadSpecialFolderID);
end;

function StrToSpecialFolderID(const IDStr: string): Integer;
  {Gets special folder id associated with a constant.
    @param IDStr [in] Name of special folder constant.
    @return Associated folder id.
    @except EPJShellFolders raised if IDStr is unknown.
  }
var
  Idx: Integer; // loops thru ID table
begin
  // Assume we don't find constant
  Result := -1;
  // Scan lookup table looking for constant
  for Idx := Low(cFolders) to High(cFolders) do
    if CompareText(cFolders[Idx].Name, IDStr) = 0 then
    begin
      Result := cFolders[Idx].ID;
      Break;
    end;
  // Raise exception if constant never found
  if Result = -1 then
    ErrorFmt(sBadSpecialFolderIDStr, [IDStr]);
end;

{ TPJSpecialFolderEnum }

function TPJSpecialFolderEnum.AtEnd: Boolean;
  {Checks if at end of enumeration.
    @return True if at end of enumeration and false otherwise.
  }
begin
  Result := fIndex > High(cFolders);
end;

function TPJSpecialFolderEnum.Count: Integer;
  {Gets number of folder ids in enumeration.
    @return Number of folders ids.
  }
begin
  Result := NumSpecialFolderIds;
end;

constructor TPJSpecialFolderEnum.Create;
  {Class constructor. Sets up object and intialises enumeration.
  }
begin
  inherited Create;
  Init;
end;

procedure TPJSpecialFolderEnum.Init;
  {Intialises enumeration.
  }
begin
  fIndex := Low(cFolders);
end;

function TPJSpecialFolderEnum.Next: Integer;
  {Gets next special folder identifier in enumeration, or -1 if at end of
  enumeration.
    @return Folder identifier.
  }
begin
  if not AtEnd then
  begin
    Result := cFolders[fIndex].ID;
    Inc(fIndex);
  end
  else
    Result := -1;
end;

{ TPJSpecialFolderInfo }

constructor TPJSpecialFolderInfo.Create(AOwner: TComponent);
  {Class constructor. Sets up object and reads information about default special
  folder.
    @param AOwner [in] Component's owner.
  }
begin
  inherited Create(AOwner);
  // Set default property values
  fFolderID := CSIDL_DESKTOP;
  // Now get folder info accordingly
  GetCurrentFolderInfo;
end;

procedure TPJSpecialFolderInfo.GetCurrentFolderInfo;
  {Retrieves information about special folder specified by FolderID property and
  updates read only properties accordingly.
  }
var
  PIDL: PItemIDList;  // PIDL to special folder
begin
  // Get special folder's PIDL
  fIsSupported := Succeeded(SHGetSpecialFolderLocation(0, fFolderID, PIDL));
  if fIsSupported then
  begin
    try
      // Special folder is supported on this system: set required properties
      fPath := PIDLToFolderPath(PIDL);
      fDisplayName := PIDLToFolderDisplayName(PIDL);
      fIsVirtual := (fPath = '');
    finally
      CoTaskMemFree(PIDL);
    end;
  end
  else
  begin
    // Special folder not supported on this system: set property values
    fPath := '';
    fDisplayName := '';
    fIsVirtual := False;
  end;
end;

procedure TPJSpecialFolderInfo.SetFolderID(const Value: Integer);
  {Write accessor method for FolderID property. Reads information about
  specified folder.
    @param Value [in] New property value.
    @except EPJShellFolders raised if Value is not a valid special folder id.
  }
begin
  if fFolderID <> Value then
  begin
    if not IsValidSpecialFolderId(Value) then
      Error(sBadSpecialFolderID);
    fFolderID := Value;
    GetCurrentFolderInfo;
  end;
end;

{ TPJBrowseDialog }

type
  {
  TCBData:
    Record that contains information passed betwenen TPJBrowseDialog component
    and the Browse dlg box's callback method. The dialog box's fData field is
    cast to this type when used. This was done to avoid declaring this type with
    public scope.
  }
  TCBData = packed record
    Handle: THandle;        // window handle of dlg box (0 if not active)
    Obj: TPJBrowseDialog;   // reference to component instance
  end;

  {
  PCBData:
    Pointer to TCBData.
  }
  PCBData = ^TCBData;


const
  // Control identifiers for Browse dialog boc
  // Not all these values are user in this class: left here for future reference

  // Identifiers common to old and new style dialog box
  cOKBtnID = $0001;
  cCancelBtnID = $0002;
  cHeadlineTextID = $3742;

  // Identifiers valid only in old style dialog box
  cStatusTextID = $3743;    // not present in new style dialog box
  cOldTreeViewID = $3741;   // id different in old and new style dialog boxes

  // Identifiers valid only in new style dialog box
  cNewFolderBtnId = $3746;  // not present in old style dialog box
  cNewTreeViewID = $0064;   // id different in old and new style dialog boxes

  // Identifier of custom help button that may be added to Browse dlg box
  // note that this button is not present in the dialog box natively - it is
  // added by this component
  cHelpBtnID = $1000;

function BrowseCallbackProc(HWnd: THandle; Msg: LongWord;
  LParam, Data: LongInt): Integer; stdcall;
  {Callback function called by browse dialog box. This function has two
  purposes: (1) to initialise the dialog box - properties not definable using
  BrowseInfo structure are set up here (2) special processing performed when
  selection changes - including triggering events.
    @param HWnd [in] Handle to browser dialog box.
    @param Msg [in] Identifies event that caused the callback to be called.
    @param LParam [in] Message specific value - depends on Msg.
    @param Data [in] Application-defined value specified in the lParam member of
      the TBrowseInfo structure in SHBrowseForFolder call.
    @return Always zero.
  }
var
  Obj: TPJBrowseDialog;   // reference to component
  PDataRec: PCBData;      // pointer to data record passed in Data param
begin
  // This function must always return zero
  Result := 0;
  // Get reference to data structure (which is component's fData field)
  PDataRec := PCBData(Data);
  // Store dialog box's window handle in data structure
  PDataRec^.Handle := HWnd;
  // Record reference to TPJBrowseDlg component stored in Data param
  Obj := TPJBrowseDialog(PDataRec^.Obj);
  // Process event notifications
  case Msg of
    BFFM_INITIALIZED:     // Perform initialisation
      // hand off to component
      Obj.InitBrowseWindow;
    BFFM_SELCHANGED:      // Perform any selection change processing
      // hand off to component: LParam contains PIDL of selected folder
      Obj.SelectionChanged(PItemIDList(LParam));
    BFFM_VALIDATEFAILED:  // Entry in edit box is invalid path
      // hand off to component: LParam contains text box entry. Return from Obj
      // is true if dialog can close, false if not. Return from this callback
      // must be non-zero to keep dialog open, 0 to close.
      Result := Ord(not Obj.ValidationFailed(PChar(LParam)));
  end;
end;

procedure TPJBrowseDialog.BrowseWndProc(var Msg: TMessage);
  {Window procedure used to subclass browse dlg box.
    @param Msg [in] Message to be handled by window procedure.
  }
var
  HelpBtnHWnd: THandle; // window handle of help button
  Handled: Boolean;     // whether message was handled
begin
  // Assume we don't handle message
  Handled := False;
  // Handle supported messages
  case Msg.Msg of
    WM_COMMAND:
      // Only handle commands from Help button when it is shown
      if (boShowHelp in fOptions) and not IsNewStyle then
      begin
        // Find help button's window
        HelpBtnHWnd := GetDlgItem(Handle, cHelpBtnID);
        // Check if message came from help button (sender's hwnd in LParam)
        if THandle(Msg.LParam) = HelpBtnHWnd then
        begin
          // Check for button clicked notification and display help topic if so
          if Msg.WParamHi = BN_CLICKED then
          begin
            DoHelp;
            Msg.Result := 0;
            Handled := True;
          end;
        end;
      end;
    WM_HELP:
      // Only handle this if we're not responding to context help: F1 pressed
      if not (boContextHelp in fOptions) then
      begin
        DoHelp;
        Msg.Result := 1;
        Handled := True;
      end;
    WM_DESTROY:
      // Trigger OnClose event and allow message to pass to orig window proc
      if Assigned(fOnClose) then
        fOnClose(Self);
  end;
  // Pass any unhandled message to original window proc
  if not Handled then
    Msg.Result := CallWindowProc(
      fOldBrowseWndProc, Handle, Msg.Msg, Msg.WParam, Msg.LParam
    );
end;

constructor TPJBrowseDialog.Create(AOwner: TComponent);
  {Class constructor. Sets up component and creates window procedure used to
  subclass then browse dialog box.
    @param AOwner [in] Dialog box's owner.
  }
begin
  inherited Create(AOwner);
  // Set default values
  fRootFolderID := CSIDL_DESKTOP;
  fOptions := [boContextHelp, boDirsOnly];
  {$IFDEF DELPHI6ANDUP}
  fHelpType := htContext;
  {$ENDIF}
  with TCBData(fData) do
  begin
    Handle := 0;
    Obj := Self;
  end;
  // Create window procedure to be used for sub classing browse dlg box
  if not (csDesigning in ComponentState) then
    // call MakeObjectInstance from appropriate unit for compiler
    {$IFDEF DELPHI6ANDUP}
    fNewBrowseWndProc := Classes.MakeObjectInstance(BrowseWndProc);
    {$ELSE}
    fNewBrowseWndProc := Forms.MakeObjectInstance(BrowseWndProc);
    {$ENDIF}
end;

destructor TPJBrowseDialog.Destroy;
  {Class destructor. Tears down object and frees browse dialog's window
  procedure.
  }
begin
  if Assigned(fNewBrowseWndProc) then
    // call FreeObjectInstance from appropriate unit for compiler
    {$IFDEF DELPHI6ANDUP}
    Classes.FreeObjectInstance(fNewBrowseWndProc);
    {$ELSE}
    Forms.FreeObjectInstance(fNewBrowseWndProc);
    {$ENDIF}
  inherited Destroy;
end;

procedure TPJBrowseDialog.DoHelp;
  {Triggers a help request if help is available. OnHelp event handler is called
  if assigned. Application help is called if request not cancelled in any OnHelp
  event handler.
  }
var
  Cancelled: Boolean;  // flag telling whether to cancel calling help system
begin
  if IsHelpAvailable then
  begin
    try
      Cancelled := False;
      if Assigned(fOnHelp) then
        fOnHelp(Self, Cancelled);
      if not Cancelled then
      begin
        {$IFDEF DELPHI6ANDUP}
        case fHelpType of
          htKeyword: Application.HelpKeyword(fHelpKeyword);
          htContext: Application.HelpContext(fHelpContext);
        end;
        {$ELSE}
        Application.HelpContext(fHelpContext);
        {$ENDIF}
      end;
    except
      Application.HandleException(ExceptObject);
    end;
  end;
end;

function TPJBrowseDialog.Execute: Boolean;
  {Initialises and displays browse dialog box. Updates properties to record
  user's entry if dialog box is OKd. Some of the dialog's initialisation is
  performed in the dialog box's BrowseCallbackProc callback function.
    @return True if user OKs, False if dialog is cancelled.
  }
var
  BI: TBrowseInfo;  // structure that controls appearance of browse dlg box
  pDisplayName: array [0..MAX_PATH] of Char;  // used to return display name
  pidlRootFolder: PItemIDList;  // PIDL of root folder to be displayed
  pidlFolder: PItemIDList;      // PIDL of selected folder
begin
  // Get PIDL for required root folder (if desktop use nil)
  if fRootFolderID = CSIDL_DESKTOP then
    pidlRootFolder := nil
  else
    if not Succeeded(
      SHGetSpecialFolderLocation(0, fRootFolderID, pidlRootFolder)
    ) then
      Error(sNoRootFolder);
  try
    // Set up structure that defines properties of browse dlg box
    with BI do
    begin
      hwndOwner := GetHWND;         // window that owns dlg is component's owner
      pidlRoot := pidlRootFolder;                  // the root folder in dlg box
      pszDisplayName := pDisplayName;  // stores display name of selected folder
      lpszTitle := PChar(fHeadline);                // any body text for dlg box
      ulFlags := 0;                                          // initialise flags
      if boDirsOnly in fOptions then
        ulFlags := ulFlags or BIF_RETURNONLYFSDIRS;
      if boNewDlgStyle in fOptions then
        ulFlags := ulFlags or BIF_NEWDIALOGSTYLE;
      if boEditBox in fOptions then
        ulFlags := ulFlags or BIF_EDITBOX or BIF_VALIDATE;
      if IsNewStyle then
      begin
        if (boHint in fOptions) and not (boEditBox in fOptions) then
          ulFlags := ulFlags or BIF_UAHINT;
      end
      else
      begin
        if boStatusText in fOptions then
          ulFlags := ulFlags or BIF_STATUSTEXT;
      end;
      lpfn := @BrowseCallbackProc;         // callback function to handle events
      lParam := Integer(@fData);         // reference to this component instance
      iImage := 0;                                                     // unused
    end;
    // Display the dlg box: returns non-nil PIDL if user OKs
    pidlFolder := SHBrowseForFolder(BI);
    TCBData(fData).Handle := 0;
    if pidlFolder <> nil then
    begin
      // User OK'd
      Result := True;
      // Store folder and display names in properties
      try
        fFolderName := PIDLToFolderPath(pidlFolder);
        fDisplayName := BI.pszDisplayName;
      finally
        // Release the selected folder PIDL's memory
        CoTaskMemFree(pidlFolder);
      end;
    end
    else
      // User cancelled
      Result := False;
  finally
    // Release any root folder PIDL memory
    if pidlRootFolder <> nil then
      CoTaskMemFree(pidlRootFolder);
  end;
end;

function TPJBrowseDialog.GetHandle: THandle;
  {Read accessor for Handle property.
    @return Handle to browse dialog box while Execute method is running or 0
      otherwise.
  }
begin
  Result := TCBData(fData).Handle;
end;

function TPJBrowseDialog.GetHWND: THandle;
  {Gets window handle of any TWinControl that owns this component.
    @return Owner handle or 0 if owner is nil or not a TWinControl.
  }
begin
  if (Owner <> nil) and (Owner is TWinControl) then
    Result := (Owner as TWinControl).Handle
  else
    Result := 0;
end;

procedure TPJBrowseDialog.HideMakeNewFolderButton;
  {Hides the dialog's Make New Folder button, providing that a new style dialog
  box is to be displayed.
  }
begin
  ShowWindow(GetDlgItem(Self.Handle, cNewFolderBtnId), SW_HIDE);
end;

procedure TPJBrowseDialog.IncludeHelpButton;
  {Creates a help button and displays it in the browse dlg box, providing
  that an old style dialog box is to be displayed. Other buttons are
  rearranged.
  }

  // ---------------------------------------------------------------------------
  procedure GetBounds(HWnd, HParentWnd: THandle;
    PLeft, PTop, PWidth, PHeight: PInteger);
    {Gets bounds of a window in terms of parent control's coordinates.
      @param HWnd [in] Handle of window for which we want bounds.
      @param HParentWnd [in] Handler of window's parent control.
      @param PLeft [in] Pointer to variable that receives left coordinate of
        window. Ignored if PLeft is nil.
      @param PTop [in] Pointer to variable that receives top coordinate of
        window. Ignored if PTop is nil.
      @param PWidth [in] Pointer to variable that receives width of window.
        Ignored if PWidth is nil.
      @param PHeight [in] Pointer to variable that receives height of window.
        Ignored if PHeight is nil.
    }
  var
    ScreenRect: TRect;  // bounding rectangle of window in screen coordinates
    TopLeft: TPoint;    // coordinates of top left corner of widnow
  begin
    // Get bounding rectangle of window (in screen coords)
    GetWindowRect(Hwnd, ScreenRect);
    // Get top left corner of window in (parent's coordinates)
    TopLeft := ScreenRect.TopLeft;
    Windows.ScreenToClient(HParentWnd, TopLeft);
    // Calculate the required value for Left, Top, Width and Height
    if Assigned(PTop) then PTop^ := TopLeft.Y;
    if Assigned(PLeft) then PLeft^ := TopLeft.X;
    if Assigned(PWidth) then PWidth^ := ScreenRect.Right - ScreenRect.Left;
    if Assigned(PHeight) then PHeight^ := ScreenRect.Bottom - ScreenRect.Top;
  end;
  // ---------------------------------------------------------------------------

resourcestring
  sHelpBtnCaption = '&Help'; // caption of help button
const
  cButtonClass = 'Button';   // button window class name
var
  HOKBtn, HCancelBtn, HHelpBtn: THandle;  // handles to buttons in dlg
  FontH: HFONT;                           // handle to font used on buttons
  BtnWidth, BtnHeight, BtnTop: Integer;   // width, height and top of dlg btns
  HelpLeft, OKLeft, CancelLeft: Integer;  // left of each of dlg's buttons
  StyleFlags: DWORD;                      // window style flags
begin
  // Get handles to OK and cancel buttons
  HOKBtn := GetDlgItem(Handle, cOKBtnID);
  HCancelBtn := GetDlgItem(Handle, cCancelBtnID);
  // Get button size and positions
  GetBounds(HOKBtn, Handle, @OKLeft, @BtnTop, @BtnWidth, @BtnHeight);
  GetBounds(HCancelBtn, Handle, @CancelLeft, nil, nil, nil);
  // Move OK & Cancel buttons left to leave room for help button on right
  HelpLeft := CancelLeft;
  CancelLeft := OKLeft;
  OKLeft := CancelLeft - (HelpLeft - CancelLeft);
  FontH := SendMessage(HOKBtn, WM_GETFONT, 0, 0);
  SetWindowPos(HOKBtn, 0, OKLeft, BtnTop, 0, 0,
    SWP_NOCOPYBITS or SWP_NOOWNERZORDER or SWP_NOSIZE	or SWP_NOZORDER);
  SetWindowPos(HCancelBtn, 0, CancelLeft, BtnTop, 0, 0,
    SWP_NOCOPYBITS or SWP_NOOWNERZORDER or SWP_NOSIZE	or SWP_NOZORDER);
  // Create the help button in the required place
  // set style flags - help button disabled if help not available
  StyleFlags := WS_VISIBLE or WS_CHILD or BS_PUSHBUTTON or WS_TABSTOP;
  if not IsHelpAvailable then
    StyleFlags := StyleFlags or WS_DISABLED;
  // create the window
  HHelpBtn := CreateWindowEx(
    0,                      // no extended style
    cButtonClass,           // use pre-defined button class
    PChar(sHelpBtnCaption), // button's text
    StyleFlags,             // window style
    HelpLeft,               // left of button in parent's client coords
    BtnTop,                 // top of button in parent's client coords
    BtnWidth,               // width of button
    BtnHeight,              // height of button
    Handle,                 // handle of parent window
    cHelpBtnID,             // id of button
    GetWindowLong(Handle, GWL_HINSTANCE), // instance of owning app
    nil                     // no CREATESTRUCT data
  );
  // Set the font of the help button to he same as other buttons
  SendMessage(HHelpBtn, WM_SETFONT, FontH, MakeLParam(1, 0));
end;

procedure TPJBrowseDialog.InitBrowseWindow;
  {Initialises the browse dialog box. This method is called from the dialog's
  callback function.
  }
var
  pidlRootFolder: PItemIDList;  // PIDL of root folder
begin
  // Sub class the browse window using new wnd proc
  fOldBrowseWndProc := Pointer(
    SetWindowLong(Handle, GWL_WNDPROC, LongInt(fNewBrowseWndProc))
  );
  // Select any folder in browse dlg box as specified by user
  if FolderName <> '' then
    SendMessage(Handle, BFFM_SETSELECTION, 1, LongInt(PChar(fFolderName)));
  // If user specified title, display it in window caption
  if Title <> '' then
    SetWindowText(Handle, PChar(Title));
  // Install help button if required (must have old style dlg)
  if (boShowHelp in fOptions) and not IsNewStyle then
    IncludeHelpButton;
  // Hide Make New Folder button if required (must have new style dlg)
  if (boHideMakeFolderBtn in fOptions) and IsNewStyle then
    // we handle hiding of Make New Folder button directly rather than using
    // BIF_NONEWFOLDERBUTTON flag since the flag required shlobj.dll v6 and we
    // want this feature on earlier versions
    HideMakeNewFolderButton;
  // Hide context help window caption icon if required
  if not (boContextHelp in fOptions) then
    SetWindowLong(Handle, GWL_EXSTYLE,
      GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_CONTEXTHELP);
  // Trigger OnInitialise event
  if Assigned(fOnInitialise) then
    fOnInitialise(Self);
  // Make sure selection change event is triggered for root folder. We need
  // this to disable new style dlg OK button if error triggered when displayed.
  if fRootFolderID <> CSIDL_DESKTOP then
  begin
    if Succeeded(
      SHGetSpecialFolderLocation(0, fRootFolderID, pidlRootFolder)
    ) then
      SelectionChanged(pidlRootFolder);
  end;
end;

function TPJBrowseDialog.IsDlgBtnEnabled(const BtnID: Integer): Boolean;
  {Checks if a browse dialog button is enabled.
    @return True if button enabled, False if disabled.
  }
begin
  Result := IsWindowEnabled(GetDlgItem(Handle, BtnID));
end;

function TPJBrowseDialog.IsHelpAvailable: Boolean;
  {Checks if any help is available per component's properties.
    @return True if help is available.
  }
begin
  // Help is available if relevant properties have non-nul values
  {$IFDEF DELPHI6ANDUP}
  case HelpType of
    htContext: Result := HelpContext <> 0;
    htKeyword: Result := HelpKeyword <> '';
    else Result := False; // keep compiler happy!
  end;
  {$ELSE}
  Result := HelpContext <> 0;
  {$ENDIF}
end;

function TPJBrowseDialog.IsNewStyle: Boolean;
  {Checks if a new style dialog box being displayed.
    @return True if new style dialog or False if not.
  }
begin
  Result := boNewDlgStyle in fOptions;
end;

procedure TPJBrowseDialog.SelectionChanged(PIDL: PItemIDList);
  {Triggers OnSelChange and OnSelChangeEx events for currently selected folder.
  Updates status text and OK button according to values returned from the event
  handler. This method is called from the dialog's callback function.
    @param PIDL [in] PIDL representing currently selected folder.
  }
var
  SelFolder: string;    // path of the folder now selected
  SelDispName: string;  // display name of the folder now selected
  StatusText: string;   // any status text to be displayed in dlg
  OKEnabled: Boolean;   // whether OK button is to be enabled or disabled
begin
  // Get folder and display name of selected item
  SelFolder := PIDLToFolderPath(PIDL);
  SelDispName := PIDLToFolderDisplayName(PIDL);
  if SelDispName = '' then
    SelDispName := SelFolder;              // no display name -> use folder name
  if SelDispName = SelFolder then
    SelDispName := ExtractFileName(SelFolder);             // don't use all path
  // Trigger and act on OnSelChange and OnSelChangeEx events if assigned
  // find if OK button is enabled or disabled
  OKEnabled := IsDlgBtnEnabled(cOKBtnID) and
    (not (boDirsOnly in fOptions) or (SelFolder <> ''));
  // set default status text value
  StatusText := '';
  // now trigger events, which may change StatusText and OKEnabled
  if Assigned(fOnSelChange) then
    fOnSelChange(Self, SelFolder, SelDispName, StatusText, OKEnabled);
  if Assigned(fOnSelChangeEx) then
    fOnSelChangeEx(Self, PIDL, StatusText, OKEnabled);
  // enable / disable OK button if user changed status of button
  if OKEnabled <> IsDlgBtnEnabled(cOKBtnID) then
    // NOTE: format of BFFM_ENABLEOK differs from API docs - button state passed
    // in LParam, not WParam
    SendMessage(Handle, BFFM_ENABLEOK, 0, Ord(OKEnabled));
  // Display any status text in dlg if user has specified this option
  if (boStatusText in fOptions) and not IsNewStyle then
    SendMessage(Handle, BFFM_SETSTATUSTEXT, 0, LongInt(PChar(StatusText)));
end;

procedure TPJBrowseDialog.SetRootFolderID(const Value: Integer);
  {Write accessor for RootFolderID property.
    @param Value [in] New property value.
    @except EPJShellFolders raised if Value is not a valid special folder id.
  }
begin
  if fRootFolderID <> Value then
  begin
    if not IsValidSpecialFolderId(Value) then
      Error(sBadSpecialFolderID);
    fRootFolderID := Value;
  end;
end;

function TPJBrowseDialog.ValidationFailed(const EditText: string): Boolean;
  {Triggers browse dialog's OnValidationFailed event, if assigned and returns
  value indicating if dialog can close.
    @param EditText [in] Text from browser's edit control that references an
      invalid path.
    @return True if dialog box can close, False if not.
  }
begin
  Result := True;
  if Assigned(fOnValidationFailed) then
    fOnValidationFailed(Self, EditText, Result);
end;

initialization

// Initialize COM: required per MS documentation
OleInitialize(nil);

finalization

// Uninitialize OLE
OleUninitialize;

end.

