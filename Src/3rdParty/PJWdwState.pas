{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1999-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev: 1966 $
 * $Date: 2014-10-28 01:20:04 +0000 (Tue, 28 Oct 2014) $
 *
 * DelphiDabbler Window state components.
}


unit PJWdwState;

// Conditional defines
// Note: There is no version checking for Delphi 1 and 2 not since this unit
// will not compile with those compilers.
{$DEFINE WarnDirs}          // $WARN compiler directives available
{$DEFINE RegAccessFlags}    // TRegistry access flags available
{$DEFINE RequiresFileCtrl}  // FileCtrl unit is required for ForceDirectories
{$UNDEF RTLNameSpaces}      // Don't qualify RTL units names with namespaces
{$UNDEF TScrollStyleMoved}  // TScrollStyle hasn't moved to System.UITypes units
{$UNDEF SupportsPathDelim}  // PathDelim and related routine not defined
{$IFDEF VER100} // Delphi 3
  {$UNDEF WarnDirs}
  {$UNDEF RegAccessFlags}
{$ENDIF}
{$IFDEF VER120} // Delphi 4
  {$UNDEF WarnDirs}
  {$UNDEF RegAccessFlags}
{$ENDIF}
{$IFDEF VER130} // Delphi 5
  {$UNDEF WarnDirs}
  {$UNDEF RegAccessFlags}
{$ENDIF}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
    {$DEFINE TScrollStyleMoved}
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE RTLNameSpaces}
  {$IFEND}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$DEFINE SupportsPathDelim}
    {$UNDEF WarnDirs}
    {$UNDEF RequiresFileCtrl}
  {$IFEND}
{$ENDIF}


interface


uses
  // Delphi
  {$IFDEF RTLNameSpaces}
  System.Classes, Vcl.Controls, Winapi.Messages, Winapi.Windows, Vcl.Forms,
  System.SysUtils, System.Win.Registry;
  {$ELSE}
  Classes, Controls, Messages, Windows, Forms, SysUtils, Registry
  {$IFDEF RequiresFileCtrl}
  , FileCtrl  // needed for ForceDirectories since it's not in SysUtils yet.
  {$ENDIF}
  ;
  {$ENDIF}


const
  // Custom messages used internally
  // instructs component to set window state (normal, minimized or maximized)
  PJM_SETWDWSTATE = WM_USER + 0;
  // instructs MDI child components they can restore their windows
  PJM_RESTOREMDICHILD = WM_USER + 1;


type

  TPJCustomWdwState = class;

  {
  TPJWdwStateHook:
    Class that hooks into window of owning form and passes on WMCreate and
    WMDestroy messages to TPJCustomWdwState component. Instances of this class
    should not be created by user code - this control is designed for private
    use by TPJCustomWdwState.
  }
  TPJWdwStateHook = class(TWinControl)
  private
    fWdwState: TPJCustomWdwState;
      {Reference to owning window state component}
    procedure SendMsgToOwner(var Msg: TMessage);
      {Dispatches given message to component's owner component.
        @param Msg [in/out] The message. May be changed by message handler.
      }
    procedure WMDestroy(var Msg: TMessage); message WM_DESTROY;
      {Handles WM_DESTROY message and dispatches it to owning window state
      component.
        @param Msg [in/out] The message. May be modified by message handler.
      }
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHOWINGCHANGED;
      {Handles CM_SHOWINGCHANGED message and dispatches it to owning window
      state component.
        @param Msg [in/out] The message. May be modified by message handler.
      }
    procedure PJMSetWindowState(var Msg: TMessage); message PJM_SETWDWSTATE;
      {Handles PJM_SETWDWSTATE message and dispatches it to owning window state
      component.
        @param Msg [in/out] The message. May be modified by message handler.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Records reference to owner.
        @param AOwner [in] Owning component. Must be a TPJCustomWdwState.
      }
  end;

  {
  EPJCustomWdwState:
    Type of exception raised by TPJCustomWdwState.
  }
  EPJCustomWdwState = class(Exception);

  {
  TPJWdwStateReadEvent:
    Type of event triggered after the window's stored placement information is
    read from storage. Event handlers can adjust the values before they are used
    to restore the window.
      @param Sender [in] Reference to component triggering the event.
      @param Left [in/out] Left edge of window. In: value read from storage.
        Out: value modified in event handler. Leave unchanged or set to MaxInt
        to use default value.
      @param Top [in/out] Top edge of window. In: value read from storage. Out:
        value modified in event handler. Leave unchanged or set to MaxInt to use
        default value.
      @param Width [in/out] Width of window. In: value read from storage. Out:
        value modified in event handler. Leave unchanged or set to MaxInt to use
        default value.
      @param Height [in/out] Height of window. In: value read from storage. Out:
        value modified in event handler. Leave unchanged or set to MaxInt to use
        default value.
      @param State [in/out] State of window. In: value read from storage. Out:
        value modified in event handler. This value is the ordinal value of a
        TWindowState value. Leave unchanged or set to MaxInt to use default
        value.
  }
  TPJWdwStateReadEvent = procedure(Sender: TObject; var Left, Top, Width,
    Height, State: Integer) of object;

  {
  TPJWdwStateOptions
    Set of values that are stored in the component's Options property.
  }
  TPJWdwStateOptions = set of (
    woIgnoreState,    // stored wdw state is ignored and wdw is display normal
    woIgnoreSize,     // stored wdw size is ignored and wdw's defaults are used
    woFitWorkArea     // restored wdw appears wholy within work area
                      // (wdw may be resized to fit if woIgnoreSize not set)
                      // (work area is desktop or MDI client area for MDI child
                      // windows)
  );

  {
  TPJCustomWdwState:
    Abstract base class for components that record window size, position and
    state between program executions.
  }
  TPJCustomWdwState = class(TComponent)
  private
    fAutoSaveRestore: Boolean;
      {Value of AutoSaveRestore property}
    fMinimizeDelay: Integer;
      {Value of MinimizeDelay property}
    fOnReadWdwState: TPJWdwStateReadEvent;
      {Event handler for OnReadWdwState event}
    fOnAfterWindowSized: TNotifyEvent;
      {Event handler for OnAfterWindowSized event}
    fOnAfterWindowRestored: TNotifyEvent;
      {Event handler for OnAfterWindowRestored event}
    fOptions: TPJWdwStateOptions;
      {Value of Options property}
    fHook: TPJWdwStateHook;
      {Instance of a privately owned windowed control that is used to intercept
      relevant messages in owning form and custom messages posted by this
      component and to notify this component of the messages}
    fWindow: TForm;
      {Instance of form on which to operate}
    fFormShown: Boolean;
      {Flag false until form has been shown and true afterwards. Any call to
      Restore method while this flag is false is recorded as pending and called
      again once form has been shown}
    fFormRestored: Boolean;
      {Flag false until component has fully restored its window}
    fRestorePending: Boolean;
      {Flag true if Restore method has been called while fFormShown flag false:
      such calls set this flag which in turn causes Restore method to be called
      again once the form has been shown}
    function GetIgnoreState: Boolean;
      {Read accessor for IgnoreState property. Checks for presence of
      woIgnoreState in Options property.
        @return True if Options contain woIgnoreState, False otherwise.
      }
    procedure SetIgnoreState(const Value: Boolean);
      {Write accessor for IgnoreState property. Includes or excludes
      woIgnoreState property in Options according to Value.
        @param Value [in] New property value.
      }
  protected
    function GetMDIParentForm: TForm;
      {Finds an MDI child form's parent form.
        @return Reference to parent form. Nil if parent not found or if
          component's form is not an MDI child form.
      }
    function GetWdwStateCmp(const Form: TForm): TPJCustomWdwState;
      {Finds any TPJCustomWdwState component placed on a form.
        @param Form [in] Form to be searched.
        @return Reference to window state component or nil if no such component
          on form.
      }
    function CanRestoreMDIChild: Boolean;
      {Checks if an MDI child form can be restored.
        @return True if form can be restored, False otherwise.
      }
    procedure WMDestroy(var Msg: TMessage); message WM_DESTROY;
      {Message handler for owning form's WM_DESTROY message. The message is sent
      to this component by the hook window component. Save the form's state if
      the AutoSaveRestore property is true.
        @param Msg [in/out] Not used.
      }
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHOWINGCHANGED;
      {Message handler for owning form's CM_SHOWINGCHANGED message. The message
      ismsent to this component by the hook window component to indicate that
      the form's showing state has changed. This causes any pending window
      restoration to be executed.
        @param Msg [in/out] Not used.
      }
    procedure PJMSetWindowState(var Msg: TMessage); message PJM_SETWDWSTATE;
      {Message handler for owning form's custom PJM_SETWDWSTATE message. The
      message is sent to this component by the hook window component. We update
      the form's window state. A message is used for this purpose since the
      Restore method needs to ensure the form has been shown before we execute
      this code. Posting this message enables this to happen.
        @param Msg [in/out] Message structure containing the required window
          state in its LParam field. Message is not modified.
      }
    procedure PJMRestoreMDIChild(var Msg: TMessage);
      message PJM_RESTOREMDICHILD;
      {Message handled for custom PJM_RESTOREMDICHILD message. This message is
      dispatched by a MDI parent form to all MDI child forms when the parent
      form has restored. In response to the message we restore the form if the
      AutoSaveRestore property is true and form has not been restored.
        @param Msg [in/out] Not used.
      }
    procedure DispatchMDIChildMessages;
      {Dispatches PJM_RESTOREMDICHILD messages to window state components on MDI
      child forms.
      }
    procedure ReadWdwState(var Left, Top, Width, Height, State: Integer);
      virtual; abstract;
      {Read state of window from storage. Caller passes default values and
      method returns new values from storage, or defaults if no stored values
      found. Implementation depends on method of storage used by derived
      classes.
        @param Left [in/out] Left side of window. Default passed in. Value read
          from storage passed out, or default if no value read.
        @param Top [in/out] Top size of window. Default passed in. Value read
          from storage passed out, or default if no value read.
        @param Width [in/out] Width of window. Default width passed in. Value
          read from storage passed out, or default if no value read.
        @param Height [in/out] Height of window. Default height passed in. Value
          read from storage passed out, or default if no value read.
        @param State [in/out] Code describing state of window. Default state
          passed in. Value read from storage passed out, or default if no value
          read. This value is the ordinal value of a TWindowState value.
      }
    procedure DoReadWdwState(var Left, Top, Width, Height, State: Integer);
      virtual;
      {Fetches the state of the window from storage via the abstract
      ReadWdwState method then triggers the OnReadWdwState event which permits
      any of the values read to be modified before passing them back to the
      caller. The default window values per the window's form properties are
      passed into the method.
        @param Left [in/out] Left side of window. Default passed in. Value read
          from storage passed out, or default if no value read.
        @param Top [in/out] Top size of window. Default passed in. Value read
          from storage passed out, or default if no value read.
        @param Width [in/out] Width of window. Default width passed in. Value
          read from storage passed out, or default if no value read.
        @param Height [in/out] Height of window. Default height passed in. Value
          read from storage passed out, or default if no value read.
        @param State [in/out] Code describing state of window. Default state
          passed in. Value read from storage passed out, or default if no value
          read. This value is the ordinal value of a TWindowState value.
      }
    procedure SaveWdwState(const Left, Top, Width, Height, State: Integer);
      virtual; abstract;
      {Save state of window to storage. Implementation depends on method of
      storage used by derived classes.
        @param Left [in] Left side of window.
        @param Top [in] Top side of window.
        @param Width [in] Width of window.
        @param Height [in] Height of window.
        @param State [in] Code representing state of window. This is the ordinal
          value of a TWindowState value.
      }
    procedure SetParentComponent(Value: TComponent); override;
      {Override of SetParentComponent method. Sets the parent of the hook
      window. This enables the hook window to receive messages from the parent
      window. The method normally called by the streaming system when component
      is loaded on a form. We have made method public so that it can be called
      explicitly when component is dynamically created.
        @param Value [in] Reference to parent component.
      }
    property OnReadWdwState: TPJWdwStateReadEvent
      read fOnReadWdwState write fOnReadWdwState;
      {Event triggered just after the window's state is read from storage. Any
      of the values read can be altered before the component sets the window's
      properties. Setting any of the values to MaxInt causes the form's default
      value to be used in place of the stored value}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Records reference to any owning form, sets default
      property values and creates a hook window to trap messages from owning
      form. Permits only one TPJCustomWdwState derived component to be placed on
      the form.
      NOTE: This constructor is only suitable for components present at design
      time. When constructing components dynamically use the CreateStandAlone
      constructor instead.
        @param AOwner [in] Owning component. Must be a TForm.
        @except EPJCustomWdwState raised if Owner is not a TForm.
        @except EPJCustomWdwState raised if there is already a TPJCustomWdwState
          component on the form.
      }
    constructor CreateStandAlone(AOwner: TForm); virtual;
      {Class constructor. Creates instance of component dynamically, ensuring
      all required housekeeping is performed. Use when constructing a component
      that is not present at design time. AOwner must be a TForm.
        @param AOwner [in] Owning component. Must be a TForm.
        @except EPJCustomWdwState raised if Owner is not a TForm.
        @except EPJCustomWdwState raised if there is already a TPJCustomWdwState
          component on the form.
      }
    procedure Restore;
      {Reads window placement and state from storage and set up the window's
      size, position and state as required.
      }
    procedure Save;
      {Save window placement, size and state to storage.
      }
  published
    property AutoSaveRestore: Boolean
      read fAutoSaveRestore write fAutoSaveRestore default False;
      {When true component automatically restores window state on form creation
      and saves it on form destruction}
    property IgnoreState: Boolean
      read GetIgnoreState write SetIgnoreState default False;
      {When true Restore method ignores the saved state of the window and leaves
      current state unchanged while still setting size and position. When false
      Restore also sets the window state according to the saved state. Changing
      this property updates the Options property: including and excluding
      woIgnoreState in the set as necessary.
      NOTE: Use of IgnoreState is now deprecated and Options should be used
      instead}
    property MinimizeDelay: Integer
      read fMinimizeDelay write fMinimizeDelay default 100;
      {When a form is to be started minimized this property determines the delay
      (in ms) between displaying the normalised form on screen and minimising
      it}
    property Options: TPJWdwStateOptions
      read fOptions write fOptions default [];
      {Provides a set of display options that affect how the window is displayed
      or if certain stored values are ignored. See the TPJWdwStateOptions type
      definition for details. Including/excluding the woIgnoreState value is the
      same as setting IgnoreState to true or false respectively}
    property OnAfterWindowSized: TNotifyEvent
      read fOnAfterWindowSized write fOnAfterWindowSized;
      {Event triggered immediately after window has been sized, but before it
      has been restored}
    property OnAfterWindowRestored: TNotifyEvent
      read fOnAfterWindowRestored write fOnAfterWindowRestored;
      {Event triggered immediately after window has been restored to required
      state}
  end;

  {
  TPJWdwStateData:
    Record used to store window state information.
  }
  TPJWdwStateData = record
    Left: Integer;    // position of left side of window
    Top: Integer;     // position of top of window
    Width: Integer;   // width of window
    Height: Integer;  // height of window
    State: Integer;   // state of window (ordinal value of TWindowState value)
  end;

  {
  TPJWdwStateReadData:
    Type of event triggered by TPJUserWdwState when window state data is
    to be read from persistent storage.
      @param Sender [in] Reference to component triggering this event.
      @param Data [in/out] Window state informatiom. Set to default values when
        called. Handler should set Data fields to values it reads from
        persistent storage.
  }
  TPJWdwStateReadData = procedure(Sender: TObject; var Data: TPJWdwStateData)
    of object;

  {
  TPJWdwStateSaveData:
    Type of event triggered by TPJUserWdwState when window state data is
    to be written to persistent storage.
      @param Sender [in] Reference to component triggering this event.
      @param Data [in] Window state data to be written to persistent storage.
  }
  TPJWdwStateSaveData = procedure(Sender: TObject; const Data: TPJWdwStateData)
    of object;

  {
  TPJUserWdwState:
    Implements a component that records a window's size, position and state
    between program executions in persistent storage. The user must provide the
    mechanism for storing and saving by handling the OnReadData and OnSaveData
    events.
  }
  TPJUserWdwState = class(TPJCustomWdwState)
  private
    fOnReadData: TPJWdwStateReadData;
      {Event handler for OnReadData event}
    fOnSaveData: TPJWdwStateSaveData;
      {Event handler for OnSaveData event}
  protected
    procedure ReadWdwState(var Left, Top, Width, Height, State: Integer);
      override;
      {Gets window state information from OnReadData event. If no event handler
      is assigned default window state is used.
        @param Left [in/out] Left side of window. Default passed in. Value read
          from storage passed out, or default if no value read.
        @param Top [in/out] Top size of window. Default passed in. Value read
          from storage passed out, or default if no value read.
        @param Width [in/out] Width of window. Default width passed in. Value
          read from storage passed out, or default if no value read.
        @param Height [in/out] Height of window. Default height passed in. Value
          read from storage passed out, or default if no value read.
        @param State [in/out] Code describing state of window. Default state
          passed in. Value read from storage passed out, or default if no value
          read. This value is the ordinal value of a TWindowState value.
      }
    procedure SaveWdwState(const Left, Top, Width, Height, State: Integer);
      override;
      {Triggers OnSaveData event to request handler to store window state. If no
      event handler is assigned window state is not recorded.
        @param Left [in] Left side of window.
        @param Top [in] Top side of window.
        @param Width [in] Width of window.
        @param Height [in] Height of window.
        @param State [in] Code representing state of window. This is the ordinal
          value of a TWindowState value.
      }
  published
    property OnReadData: TPJWdwStateReadData
      read fOnReadData write fOnReadData;
      {Event triggered when window state data is to be read from persistent
      storage. User must read required data in response to this event. If the
      event is not handled default window state is used}
    property OnSaveData: TPJWdwStateSaveData
      read fOnSaveData write fOnSaveData;
      {Event triggered when window state is to be written to persistent storage.
      User must write provided data in response to this event. If the event is
      not handled window state is not saved}
  end;

  {
  TPJWdwStateIniRootDir:
    Identifiers of the directories supported in the TPJWdwState.IniRootDir
    property.
  }
  TPJWdwStateIniRootDir = (
    rdWindowsDir,       // Windows system directory: not recommended
    rdExeDir,           // Program directory: use for portable programs only
    rdAppDataDir,       // Per-user application data directory
    rdProgramDataDir    // Common application data directory
  );

  {
  TPJWdwStateGetIniData:
    Type of event that is triggered just before ini file is accessed. It allows
    the handler to change the ini file name and section to be used.
      @param AIniFileName [in/out] Value of IniFileName property passed in.
        Handler can change this value. If the value passed out is a relative
        path the file will be relative to the folder specified by the IniRootDir
        property.
      @param ASection [in/out] Default ini section name passed in. Handler can
        change this value.
  }
  TPJWdwStateGetIniData = procedure(var AIniFilename, ASection: string)
    of object;

  {
  TPJWdwStateGetIniDataEx:
    Type of event that is triggered just before ini file is accessed. It allows
    handler to change the ini root folder, file name and section to be used.
      @param AIniRootDir [in/out] Value of IniRootDir property passed in.
        Hander can change this value. If the value passed out in AIniFileName is
        a relative path then AIniRootDir will be used to determine the folder
        used to store the file.
      @param AIniFileName [in/out] Value of IniFileName property passed in.
        Handler can change this value. If the value passed out is a relative
        path the file will be relative to the folder specified by the
        AIniRootDir parameter.
      @param ASection [in/out] Default ini section name passed in. Handler can
        change this value.
  }
  TPJWdwStateGetIniDataEx = procedure(var AIniRootDir: TPJWdwStateIniRootDir;
    var AIniFilename, ASection: string) of object;

  {
  TPJWdwState:
    Implements a component that records a window's size, position and state
    between program executions. An ini file is used to store the information.
  }
  TPJWdwState = class(TPJCustomWdwState)
  private
    fSection: string;
      {Value of Section property}
    fIniFileName: string;
      {Value in IniFileName property}
    fIniRootDir: TPJWdwStateIniRootDir;
      {Value of IniRootDir property}
    fOnGetIniData: TPJWdwStateGetIniData;
      {Event handler for OnGetIniData event}
    fOnGetIniDataEx: TPJWdwStateGetIniDataEx;
      {Event handler for OnGetIniDataEx event}
    function BuildIniFileName(AIniRootDir: TPJWdwStateIniRootDir;
      AIniFileName: string): string;
      {Constructs the ini file name to be used.
        @param AIniRootDir [in] ID of ini file root directory use for relative
          ini file names.
        @param AIniFileName [in] Name of ini file. If this is a relative path
          it will have a directory specified by AIniRootDir prepended.
        @return Required file name. This will always be a rooted file spec.
      }
    function IniRootPath(const AIniRootDir: TPJWdwStateIniRootDir): string;
      {Returns the root path specified by the given root directory ID. This root
      directory is used for any ini file names that are relative paths.
        @param AIniRootDir [in] ID of require root directory.
        @return Required path. This is always a rooted path.
      }
  protected
    procedure GetIniInfo(var AIniFileName, ASection: string);
      {Triggers OnGetIniData event to get ini file and section names to be used
      when restoring / saving window state.
        @param AIniFileName [in/out] Required ini file name. Set to value of
          IniFileName property when called. Can be changed by event handler.
        @param ASection [in/out] Required section name. Set to value of Section
          property when called. Can be changed by event handler.
      }
    procedure ReadWdwState(var Left, Top, Width, Height, State: Integer);
      override;
      {Reads window state from ini file.
        @param Left [in/out] Left side of window. Default passed in. Value read
          from ini file passed out, or default if no value read.
        @param Top [in/out] Top size of window. Default passed in. Value read
          from ini file passed out, or default if no value read.
        @param Width [in/out] Width of window. Default width passed in. Value
          read from ini file passed out, or default if no value read.
        @param Height [in/out] Height of window. Default height passed in. Value
          read from ini file passed out, or default if no value read.
        @param State [in/out] Code describing state of window. Default state
          passed in. Value read from ini file passed out, or default if no value
          read. This value is the ordinal value of a TWindowState value.
      }
    procedure SaveWdwState(const Left, Top, Width, Height, State: Integer);
      override;
      {Writes window state to ini file.
        @param Left [in] Left side of window.
        @param Top [in] Top side of window.
        @param Width [in] Width of window.
        @param Height [in] Height of window.
        @param State [in] Code representing state of window. This is the ordinal
          value of a TWindowState value.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets default property values.
        @param AOwner [in] Owning component. Must be a TForm.
        @except EPJCustomWdwState raised if Owner is not a TForm.
        @except EPJCustomWdwState raised if there is already a TPJCustomWdwState
          component on the form.
      }
    function IniFilePath: string;
      {Returns the fully specified file to the ini file used to store window
      state information.
      NOTE: This method will trigger the OnGetIniData and OnGetIniDataEx events.
      }
  published
    // Published inherited property
    property OnReadWdwState;
    // New properties
    property IniRootDir: TPJWdwStateIniRootDir
      read fIniRootDir write fIniRootDir default rdAppDataDir;
      {An identifier that specifies the root directory to be used for any
      relative ini file name specified in the IniFileName property. If
      IniFileName contains no path information, and IniRootDir is either
      rdAppData or rdProgramData then the "DelphiDabbler\WindowStateStore\"
      sub-directory of rdAppData or rdProgramData is used. The actual value used
      to generate the file name can be changed in the OnGetIniDataEx event
      handler.}
    property IniFileName: string read fIniFileName write fIniFileName;
      {The name of the ini file in which to save window information. If this
      file name is a fully specified file path it is used as-is. If the file
      name is relative it is stored in the root directory specified by the
      IniRootDir parameter. If IniFileName is the empty string then the ini file
      is has the same name as the program file, with the extension changed to
      .ini. The actual value used to generate the file name can be changed in
      the OnGetIniData or OnGetIniDataEx event handlers.}
    property Section: string read fSection write fSection;
      {The name of the section in ini file in which to save window information.
      Uses "Window_<Form Name>" (eg 'Window_Form1') if set to empty string
      (default). The actual section name used can be changed in the OnGetIniData
      or OnGetIniDataEx event handlers.}
    property OnGetIniData: TPJWdwStateGetIniData
      read fOnGetIniData write fOnGetIniData;
      {Event triggered just before ini file is read when restoring and saving
      window state. By handling this event you can change the ini file name and
      section from those specified in the IniFileName and Section properties.
      NOTE 1: If a relative path is specified for the file name it will be
      appended to the sub-folder specified by the IniRootDir property.
      NOTE 2: The IniFileName and Section properties are not modified.
      NOTE 3: This event is not triggered if OnGetIniDataEx is handled.}
    property OnGetIniDataEx: TPJWdwStateGetIniDataEx
      read fOnGetIniDataEx write fOnGetIniDataEx;
      {Event triggered just before ini file is read when restoring and saving
      window state. By handling this event you can change the default ini root
      directory file name and section from those specified in the IniRootDir,
      IniFileName and Section properties.
      NOTE 1: If a relative path is specified for the file name it will be
      appended to the sub-folder specified by the value returned in the event
      handler's AIniRootDir parameter.
      NOTE 2: The IniRootDir, IniFileName and Section properties are not
      modified.
      NOTE 3: If this event is handled then the OnGetIniData event is not
      triggered.}
  end;

  {
  TPJWdwStateGetRegData:
    Type of event that is triggered just before registry is accessed. It allows
    handler to change the registry root key and sub key to be used.
      @param RootKey [in/out] Registry root key. Default HKEY value passed in.
        May be changed in event handler.
      @param SubKey [in/out] Registry sub key. Default value passed in. May be
        changed in event handler.
  }
  TPJWdwStateGetRegData = procedure(var RootKey: HKEY;
    var SubKey: string) of object;

  {TPJRegRootKey:
    Enumeration of values that represent the registry root keys supported by
    TPJRegWdwState. Each value represents and maps to the similarly named
    HKEY_* constant, as shown in the comments.
  }
  TPJRegRootKey = (
    hkClassesRoot,      // HKEY_CLASSES_ROOT
    hkCurrentUser,      // HKEY_CURRENT_USER
    hkLocalMachine,     // HKEY_LOCAL_MACHINE
    hkUsers,            // HKEY_USERS
    hkPerformanceData,  // HKEY_PERFORMANCE_DATA
    hkCurrentConfig,    // HKEY_CURRENT_CONFIG
    hkDynData           // HKEY_DYN_DATA
  );

  {
  TPJWdwStateGetRegDataEx:
    Type of event that is triggered just before registry is accessed. It allows
    handler to change the registry root key and sub key to be used.
      @param RootKeyEx [in/out] Registry root key. Default TPJRegRootKey value
        passed in. May be changed in event handler.
      @param SubKey [in/out] Registry sub key. Default value passed in. May be
        changed in event handler.
  }
  TPJWdwStateGetRegDataEx = procedure(var RootKeyEx: TPJRegRootKey;
    var SubKey: string) of object;

  {
  TPJWdwStateRegAccessEvent:
    Type of event that is triggered after registry is opened, ready for access.
    Permits handler to read / write additional data to sub key.
    Added by BJM.
      @param Reg [in] Reference to registry object that allows registry to be
        read. Reg is set to the registry sub key where window state data is
        stored and can be used to read / write additional data.
  }
  TPJWdwStateRegAccessEvent = procedure(const Reg: TRegistry) of object;

  {
  TPJRegWdwState:
    Implements a component that records a window's size, position and state
    between program executions. The registry is used to store the information.
    NOTE: Do not use TPJRegWdwState in programs compiled with Delphi 5 and
    earlier if the program is to run on 64 bit Windows: The version of TRegistry
    used by these early Delphis does not fully support access to the 64 bit
    registry view.
  }
  TPJRegWdwState = class(TPJCustomWdwState)
  private // properties
    fRootKeyEx: TPJRegRootKey;
      {Value of RootKeyEx property}
    fSubKey: string;
      {Value of SubKey property}
    fOnGetRegData: TPJWdwStateGetRegData;
      {Event handler for OnGetRegData event}
    fOnGetRegDataEx: TPJWdwStateGetRegDataEx;
      {Event handler for OnGetRegDataEx event}
    fOnGettingRegData: TPJWdwStateRegAccessEvent; // Added by BJM
      {Event handler for OnGettingRegData event}
    fOnPuttingRegData: TPJWdwStateRegAccessEvent; // Added by BJM
      {Event handler for OnPuttingRegData event}
    function GetRootKey: HKEY;
      {Read accessor for RootKey property.
        @return Required property value.
      }
    procedure SetRootKey(const Value: HKEY);
      {Write accessor for RootKey property.
        @param Value [in] New property value.
        @exception ERangeError raised if value is not a recognised HKEY_* value.
      }
    procedure SetSubKey(const Value: string);
      {Write accessor method for SubKey property.
        @param Value [in] New property value. If Value='' then the property is
          set to \Software\<App File Name>\Window\<Form Name>.
      }
  protected
    procedure GetRegInfo(var ARootKey: TPJRegRootKey; var ASubKey: string);
      {Triggers OnGetRegData event to get registry root key and sub key to be
      used when restoring / saving window state.
        @param ARootKey [in/out] Required root key value. Set to value of
          RootKey property by default. May be changed in event handler.
        @param ASubKey [in/ou] Required sub key. Set to value of SubKey property
          when called. May be changed in event handler.
      }
    procedure ReadWdwState(var Left, Top, Width, Height, State: Integer);
      override;
      {Reads window state from registry.
        @param Left [in/out] Left side of window. Default passed in. Value read
          from registry passed out, or default if no value read.
        @param Top [in/out] Top size of window. Default passed in. Value read
          from registry passed out, or default if no value read.
        @param Width [in/out] Width of window. Default width passed in. Value
          read from registry passed out, or default if no value read.
        @param Height [in/out] Height of window. Default height passed in. Value
          read from registry passed out, or default if no value read.
        @param State [in/out] Code describing state of window. Default state
          passed in. Value read from registry passed out, or default if no value
          read. This value is the ordinal value of a TWindowState value.
      }
    procedure SaveWdwState(const Left, Top, Width, Height, State: Integer);
      override;
      {Writes window state to registry.
        @param Left [in] Left side of window.
        @param Top [in] Top side of window.
        @param Width [in] Width of window.
        @param Height [in] Height of window.
        @param State [in] Code representing state of window. This is the ordinal
          value of a TWindowState value.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets default property values.
        @param AOwner [in] Owning component. Must be a TForm.
        @except EPJCustomWdwState raised if Owner is not a TForm.
        @except EPJCustomWdwState raised if there is already a TPJCustomWdwState
          component on the form.
      }
  published
    // Published inherited property
    property OnReadWdwState;
    // New properties
    property RootKey: HKEY read GetRootKey write SetRootKey
      default HKEY_CURRENT_USER;
      {Registry root key to use. Must be set to a valid HKEY value. Setting this
      property also sets RootKeyEx to a corresponding value}
    property RootKeyEx: TPJRegRootKey read fRootKeyEx write fRootKeyEx
      stored False default hkCurrentUser;
      {Registry root key to use as specified by a value from the TPJRegRootKey
      enumeration. Setting this property also sets RootKey to a corresponding
      value.
      NOTE: This property is provided to make it easier to set root keys at
      design time to avoid remembering the root key value as an integer}
    property SubKey: string read fSubKey write SetSubKey;
      {The sub-key below root key where window state is to be stored. If set to
      empty string the value of '/Software/<Program Name>/Window/<Form Name>'
      is used}
    property OnGetRegData: TPJWdwStateGetRegData
      read fOnGetRegData write fOnGetRegData;
      {Event triggered just before registry is read when restoring and saving
      window state. Allows handler to change root key and subkey to be used to
      store window state. Root key is specified via its HKEY value. If this
      event is handled then RootKey, RootKeyEx and SubKey properties are all
      ignored}
    property OnGetRegDataEx: TPJWdwStateGetRegDataEx
      read fOnGetRegDataEx write fOnGetRegDataEx;
      {Event triggered just before registry is read when restoring and saving
      window state. Allows handler to change root key and subkey to be used to
      store window state. Root key is specified via its TPJRegRootKey value. If
      this event is handled then RootKey, RootKeyEx and SubKey properties are
      all ignored}
    property OnGettingRegData: TPJWdwStateRegAccessEvent  // Added by BJM
      read fOnGettingRegData write fOnGettingRegData;
      {Event triggered when component is reading window state data from
      registry. Handle this event to read any additional data from registry}
    property OnPuttingRegData: TPJWdwStateRegAccessEvent  // Added by BJM
      read fOnPuttingRegData write fOnPuttingRegData;
      {Event triggered when component is writing window state data to registry.
      Handle this event to write any additional data to registry}
  end;


procedure Register;
  {Registers the components.
  }


implementation


uses
  // Delphi
  {$IFDEF RTLNameSpaces}
  System.IniFiles, Winapi.MultiMon, Vcl.StdCtrls, Winapi.ActiveX, Winapi.ShlObj
  {$IFDEF TScrollStyleMoved}
  , System.UITypes
  {$ENDIF}
  ;
  {$ELSE}
  IniFiles, MultiMon, StdCtrls, ActiveX, ShlObj;
  {$ENDIF}


{ Component registration routine }

procedure Register;
  {Registers the components.
  }
begin
  RegisterComponents(
    'DelphiDabbler',
    [TPJWdwState, TPJRegWdwState, TPJUserWdwState]
  );
end;

{$IFNDEF SupportsPathDelim}
// Definitions used for versions of Delphi that don't implement the following
// constant and function in SysUtils.

const
  // File path delimiter
  PathDelim = '/';

// Ensures that given directory or path ends with exactly one path delimiter.
function IncludeTrailingPathDelimiter(const PathOrDir: string): string;
begin
  Result := PathOrDir;
  // remove all trailing path delimiters if any, to get rid of any duplicates
  while (Result <> '') and (Result[Length(Result)] = PathDelim) do
    Result := Copy(Result, 1, Length(Result) - 1);
  // add a single trailing delimiter
  Result := Result + PathDelim;
end;
{$ENDIF}

{ TPJWdwStateHook }

procedure TPJWdwStateHook.CMShowingChanged(var Msg: TMessage);
  {Handles CM_SHOWINGCHANGED message and dispatches it to owning window state
  component.
    @param Msg [in/out] The message. May be modified by message handler.
  }
begin
  inherited;
  SendMsgToOwner(Msg);
end;

constructor TPJWdwStateHook.Create(AOwner: TComponent);
  {Class constructor. Records reference to owner.
    @param AOwner [in] Owning component. Must be a TPJCustomWdwState.
  }
begin
  Assert(Assigned(AOwner));
  Assert(AOwner is TPJCustomWdwState);
  inherited;
  fWdwState := AOwner as TPJCustomWdwState;
end;

procedure TPJWdwStateHook.PJMSetWindowState(var Msg: TMessage);
  {Handles PJM_SETWDWSTATE message and dispatches it to owning window state
  component.
    @param Msg [in/out] The message. May be modified by message handler.
  }
begin
  inherited;
  SendMsgToOwner(Msg);
end;

procedure TPJWdwStateHook.SendMsgToOwner(var Msg: TMessage);
  {Dispatches given message to component's owner component.
    @param Msg [in/out] The message. May be changed by message handler.
  }
begin
  fWdwState.Dispatch(Msg);
end;

procedure TPJWdwStateHook.WMDestroy(var Msg: TMessage);
  {Handles WM_DESTROY message and dispatches it to owning window state
  component.
    @param Msg [in/out] The message. May be modified by message handler.
  }
begin
  SendMsgToOwner(Msg);
  inherited;
end;

{ TPJCustomWdwState }

resourcestring
  // Error messages
  sErrFormRequired = 'TPJCustomWdwState.Create():'#13#10
    + 'Window state components must be placed on a form. '
    + 'To create a component dynamically use the CreateStandAlone constructor.';
  sErrDynamic = 'TPJCustomWdwState.CreateStandAlone():'#13#10
    + 'A non-nil parent form is required.';
  sErrSingleInstance = 'TPJCustomWdwState.Create():'#13#10
    + 'Only one window state component is permitted on a  form: %s is already '
    + 'present on %s.';

function TPJCustomWdwState.CanRestoreMDIChild: Boolean;
  {Checks if an MDI child form can be restored.
    @return True if form can be restored, False otherwise.
  }
var
  ParentForm: TForm;              // MDI child's parent form
  ParentCmp: TPJCustomWdwState;   // parent form's window state control, if any
begin
  ParentForm := GetMDIParentForm;
  ParentCmp := GetWdwStateCmp(ParentForm);
  if woFitWorkArea in fOptions then
  begin
    // Fitting to work area => we need information about parent form's client
    // area.
    if Assigned(ParentCmp) then
      // Parent has a window state control that may alter client area. Therefore
      // we can only restore this child form once parent has restored itself.
      Result := ParentCmp.fFormRestored
    else
      // Parent has no window state control to alter client area. Assuming (as
      // we do) that client area is known before this method is called we can
      // go ahead and restore
      Result := True;
  end
  else
    // Not fitting to work area. Always OK to restore since we don't need any
    // info from parent form before restoring
    Result := True;
end;

procedure TPJCustomWdwState.CMShowingChanged(var Msg: TMessage);
  {Message handler for owning form's CM_SHOWINGCHANGED message. The message is
  sent to this component by the hook window component to indicate that the
  form's showing state has changed. This causes any pending window restoration
  to be executed.
    @param Msg [in/out] Not used.
  }
begin
  inherited;
  // We only act on this method the first time it's called: fFormShown indicates
  // if we've been here before
  if not fFormShown then
  begin
    // This code executed first time called only
    fFormShown := True;
    if not (csDesigning in ComponentState)
      and (AutoSaveRestore or fRestorePending) then
      // We call restore method if a call to the method is pending or we're
      // auto-saving
      Restore;
  end;
end;

constructor TPJCustomWdwState.Create(AOwner: TComponent);
  {Class constructor. Records reference to any owning form, sets default
  property values and creates a hook window to trap messages from owning form.
  Permits only one TPJCustomWdwState derived component to be placed on the form.
  NOTE: This constructor is only suitable for components present at design time.
  When constructing components dynamically use the CreateStandAlone constructor
  instead.
    @param AOwner [in] Owning component. Must be a TForm.
    @except EPJCustomWdwState raised if Owner is not a TForm.
    @except EPJCustomWdwState raised if there is already a TPJCustomWdwState
      component on the form.
  }
var
  Idx: Integer; // loops thru form's components
begin
  // Record reference to owning form (raise execption if no owning form)
  if not Assigned(AOwner) or not (AOwner is TForm) then
    raise EPJCustomWdwState.Create(sErrFormRequired);
  fWindow := AOwner as TForm;
  // Ensure there is only one TPJCustomWdwState component on a form
  for Idx := 0 to Pred(AOwner.ComponentCount) do
    if AOwner.Components[Idx] is TPJCustomWdwState then
      raise EPJCustomWdwState.CreateFmt(sErrSingleInstance,
        [AOwner.Components[Idx].Name, AOwner.Name]);
  // All OK: go ahead and create component
  inherited Create(AOwner);
  // Set default property values
  fAutoSaveRestore := False;
  fOptions := [];
  fMinimizeDelay := 100;
  // Hook into owning form (run time only)
  // the hook control is auto-destroyed when this component is destroyed
  if not (csDesigning in ComponentState) then
    fHook := TPJWdwStateHook.Create(Self);
  // Set default flags
  fFormShown := False;
  fRestorePending := False;
end;

constructor TPJCustomWdwState.CreateStandAlone(AOwner: TForm);
  {Class constructor. Creates instance of component dynamically, ensuring all
  required housekeeping is performed. Use when constructing a component that is
  not present at design time. AOwner must be a TForm.
    @param AOwner [in] Owning component. Must be a TForm.
    @except EPJCustomWdwState raised if Owner is not a TForm.
    @except EPJCustomWdwState raised if there is already a TPJCustomWdwState
      component on the form.
  }
begin
  if not Assigned(AOwner) then
    raise EPJCustomWdwState.Create(sErrDynamic);
  // Do default construction
  Create(AOwner);
  // Make given owner the component's parent
  SetParentComponent(AOwner);
end;

procedure TPJCustomWdwState.DispatchMDIChildMessages;
  {Dispatches PJM_RESTOREMDICHILD messages to window state components on MDI
  child forms.
  }
var
  ChildIdx: Integer;            // loops thru all owner form's MDI child forms
  WdwState: TPJCustomWdwState;  // ref to MDI child form's window state comp
  Msg: TMessage;                // message sent to each MDI child form
begin
  inherited;
  if not Assigned(fWindow) or (fWindow.FormStyle <> fsMDIForm) then
    Exit;
  // Set up message
  Msg.Msg := PJM_RESTOREMDICHILD;
  Msg.LParam := 0;
  Msg.WParam := 0;
  Msg.Result := 0;
  // Send message to each MDI child
  for ChildIdx := 0 to Pred(fWindow.MDIChildCount) do
  begin
    WdwState := GetWdwStateCmp(fWindow.MDIChildren[ChildIdx]);
    if Assigned(WdwState) then
      WdwState.Dispatch(Msg);
  end;
end;

procedure TPJCustomWdwState.DoReadWdwState(var Left, Top, Width, Height,
  State: Integer);
  {Fetches the state of the window from storage via the abstract ReadWdwState
  method then triggers the OnReadWdwState event which permits any of the values
  read to be modified before passing them back to the caller. The default window
  values per the window's form properties are passed into the method
    @param Left [in/out] Left side of window. Default passed in. Value read
      from storage passed out, or default if no value read.
    @param Top [in/out] Top size of window. Default passed in. Value read
      from storage passed out, or default if no value read.
    @param Width [in/out] Width of window. Default width passed in. Value
      read from storage passed out, or default if no value read.
    @param Height [in/out] Height of window. Default height passed in. Value
      read from storage passed out, or default if no value read.
    @param State [in/out] Code describing state of window. Default state
      passed in. Value read from storage passed out, or default if no value
      read. This value is the ordinal value of a TWindowState value.
  }
var
  DefLeft, DefTop: Integer;     // default window position
  DefWidth, DefHeight: Integer; // default window size
  DefState: Integer;            // default window state
begin
  // Record default placement: this is form's window default window placement
  DefLeft := Left;
  DefTop := Top;
  DefWidth := Width;
  DefHeight := Height;
  DefState := State;
  // Read details from storage (storage method defined by sub-classes)
  ReadWdwState(Left, Top, Width, Height, State);
  if Assigned(fOnReadWdwState) then
  begin
    // Trigger OnReadWdwState event: permits read values to be overridden
    fOnReadWdwState(Self, Left, Top, Width, Height, State);
    // If any of values set to MaxInt in event handler, restore default value
    if Left = MaxInt then Left := DefLeft;
    if Top = MaxInt then Top := DefTop;
    if Width = MaxInt then Width := DefWidth;
    if Height = MaxInt then Height := DefHeight;
    if State = MaxInt then State := DefState;
  end;
end;

function TPJCustomWdwState.GetIgnoreState: Boolean;
  {Read accessor for IgnoreState property. Checks for presence of woIgnoreState
  in Options property.
    @return True if Options contain woIgnoreState, False otherwise.
  }
begin
  Result := woIgnoreState in fOptions;
end;

function TPJCustomWdwState.GetMDIParentForm: TForm;
  {Finds an MDI child form's parent form.
    @return Reference to parent form. Nil if parent not found or if component's
      form is not an MDI child form.
  }
var
  FormIdx: Integer;   // loops through all forms in project
  Form: TForm;        // reference to each form in project
  ChildIdx: Integer;  // loops thru MDI form's MDI child forms.
begin
  Result := nil;
  if fWindow.FormStyle <> fsMDIChild then
    Exit;
  for FormIdx := 0 to Pred(Screen.FormCount) do
  begin
    Form := Screen.Forms[FormIdx];
    if Form.FormStyle = fsMDIForm then
    begin
      for ChildIdx := 0 to Pred(Form.MDIChildCount) do
      begin
        if Form.MDIChildren[ChildIdx] = fWindow then
        begin
          Result := Form;
          Exit;
        end;
      end;
    end;
  end;
end;

function TPJCustomWdwState.GetWdwStateCmp(
  const Form: TForm): TPJCustomWdwState;
  {Finds any TPJCustomWdwState component placed on a form.
    @param Form [in] Form to be searched.
    @return Reference to window state component or nil if no such component on
      form.
  }
var
  CmpIdx: Integer;  // loops thru all components on form
begin
  Result := nil;
  if not Assigned(Form) then
    Exit;
  for CmpIdx := 0 to Pred(Form.ComponentCount) do
  begin
    if Form.Components[CmpIdx] is TPJCustomWdwState then
    begin
      Result := Form.Components[CmpIdx] as TPJCustomWdwState;
      Break;
    end;
  end;
end;

procedure TPJCustomWdwState.PJMRestoreMDIChild(var Msg: TMessage);
  {Message handled for custom PJM_RESTOREMDICHILD message. This message is
  dispatched by a MDI parent form to all MDI child forms when the parent form
  has restored. In response to the message we restore the form if the
  AutoSaveRestore property is true and form has not been restored.
    @param Msg [in/out] Not used.
  }
begin
  inherited;
  if not Assigned(fWindow) then
    Exit;
  if (fWindow.FormStyle = fsMDIChild) and not fFormRestored then
  begin
    if not (csDesigning in ComponentState)
      and (AutoSaveRestore or fRestorePending) then
      Restore;
  end;
end;

procedure TPJCustomWdwState.PJMSetWindowState(var Msg: TMessage);
  {Message handler for owning form's custom PJM_SETWDWSTATE message. The message
  is sent to this component by the hook window component. We update the form's
  window state. A message is used for this purpose since the Restore method
  needs to ensure the form has been shown before we execute this code. Posting
  this message enables this to happen.
    @param Msg [in/out] Message structure containing the required window state
      in its LParam field. Message is not modified.
  }

  procedure BusyWait(const Interval: Cardinal);
    {Performs a busy wait for a specified time.
      @param Interval [in] Time to wait in milliseconds.
    }
  var
    Start: Cardinal;    // Number of ticks at start of loop
    TicksNow: Cardinal; // Number of ticks at current time
  begin
    Start := GetTickCount;
    repeat
      Application.ProcessMessages;
      TicksNow := GetTickCount;
    until (Int64(TicksNow) - Int64(Start) >= Interval)  // time elapsed
      or (TicksNow < Start);  // ticks have wrapped round (v unlikely!)
  end;

begin
  inherited;
  // Check that a form window is available
  if Assigned(fWindow) then
  begin
    // Change the state of the window as required
    case TWindowState(Msg.LParam) of
      wsMaximized:
        // maximise the window
        fWindow.WindowState := wsMaximized;
      wsMinimized:
      begin
        // minimize the window:
        // pause for required interval before doing minimization
        if fMinimizeDelay > 0 then
          BusyWait(fMinimizeDelay);
        // if window is main form, minimize the whole app else minimize window
        if Application.MainForm = fWindow then
          Application.Minimize
        else
          fWindow.WindowState := wsMinimized;
      end;
      wsNormal:
        // normal window state
        fWindow.WindowState := wsNormal;
    end;
    // Note that the form is now restored
    fFormRestored := True;
    // Notify any MDI child forms that this form has been restored
    DispatchMDIChildMessages;
  end;
  // Trigger event to inform that window has been restored
  if Assigned(fOnAfterWindowRestored) then
    fOnAfterWindowRestored(Self);
end;

procedure TPJCustomWdwState.Restore;
  {Reads window placement and state from storage and set up the window's size,
  position and state as required.
  }

  //----------------------------------------------------------------------------
  function WindowScrollbars(const Wnd: HWND): TScrollStyle;
    {Finds which, if any scrollbars are displayed by a window.
      @param Wnd [in] Handle of window we are checking.
      @return Identifies scroll bars displayed if any.
    }
  var
    StyleFlags: DWORD;  // window style flags providing scrollbar info
  begin
    // Get style flags (allow WS_VSCROLL and WS_HSCROLL only)
    StyleFlags:= GetWindowLong(Wnd, GWL_STYLE) and (WS_VSCROLL or WS_HSCROLL);
    // Convert style flags to TScrollStyle
    case StyleFlags of
      0: Result := ssNone;
      WS_VSCROLL: Result := ssVertical;
      WS_HSCROLL: Result := ssHorizontal;
      else Result := ssBoth;
    end;
  end;
  //----------------------------------------------------------------------------

var
  Left, Top: Integer;         // position of window
  Width, Height: integer;     // dimensions of window
  State: Integer;             // state of window as integer
  Pl: TWindowPlacement;       // info structure for placement of window
  WorkArea: TRect;            // work area in which form to be aligned
  AMonitor: HMonitor;         // handle to a monitor
  MonitorInfo: TMonitorInfo;  // receives info about a monitor
  MDIParent: TForm;           // reference to parent form of MDI child
  Scrollbars: TScrollStyle;   // scrollbars displayed by MDI child
begin
  Assert(not (csDesigning in ComponentState));

  // Check if there is an owning window and get out if not
  if fWindow = nil then
    Exit;
  // If not ready to act on restore command set pending flag for later handling
  if not fFormShown
    or ((fWindow.FormStyle = fsMDIChild) and not CanRestoreMDIChild) then
  begin
    // note that a restore is pending
    fRestorePending := True;
    Exit;
  end;

  // We are actually doing the restoration: note that not pending any more
  fRestorePending := False;

  // Read info from storage

  // Set default values in case any of items are not recorded in storage
  Left := fWindow.Left;
  Top := fWindow.Top;
  Width := fWindow.Width;
  Height := fWindow.Height;
  State := Ord(fWindow.WindowState);

  // Read state and size of window from storage - using defaults set above. The
  // values could be altered by OnReadWdwState event handler that is triggered
  // inside DoReadWdwState
  DoReadWdwState(Left, Top, Width, Height, State);

  // Adjust read values

  // Restore default window size if we're ignoring recorded window size
  if woIgnoreSize in fOptions then
  begin
    Width := fWindow.Width;
    Height := fWindow.Height;
  end;

  // Set state to normal if we're ignoring recorded window state
  if woIgnoreState in fOptions then
    State := Ord(wsNormal);

  // Ensure window fits in desktop workarea if woFitWorkArea specified
  // We proceed differently depending on whether form is MDI child or not
  if (woFitWorkArea in Options) then
  begin
    // Get size of desktop workarea (excludes taskbar and any other toolbars)
    // Work area is different for MDI and SDI forms
    if (fWindow.FormStyle = fsMDIChild) then  // Added by BJM
    begin
      // We have MDI form. Work area in this case is client area of parent form
      // that MDI child windows.
      // We should not be able to get here until we have received unlock message
      // from parent window.
      // NOTE: this requires that parent form also has TPJCustomWdwState
      // component.
      MDIParent := GetMDIParentForm;
      if Assigned(MDIParent) then
      begin
        // Important: use Windows.GetClientRect here since
        // fMDIParentForm.ClientWidth and fMDIParentForm.ClientHeight don't have
        // correct values and they don't allow for status bars, toolbars etc.
        // If form has scrollbars client width is underestimated so we adjust
        // for that.
        Scrollbars := WindowScrollbars(MDIParent.ClientHandle);
        GetClientRect(MDIParent.ClientHandle, WorkArea);
        if Scrollbars in [ssHorizontal, ssBoth] then
          Inc(WorkArea.Bottom, GetSystemMetrics(SM_CXHSCROLL));
        if Scrollbars in [ssVertical, ssBoth] then
          Inc(WorkArea.Right, GetSystemMetrics(SM_CYHSCROLL));
      end
      else
        // Can't read parent form (possibly because it has no TPJCustomWdwState
        // component) => we can't get client rectangle. We make work area empty.
        WorkArea := Rect(0, 0, 0, 0);
    end
    else
    begin
      // Not MDI child form. Work area is that of desktop in current monitor.
      // This code provided by CS
      // First get bounds rectangle of restored window
      SetRect(WorkArea, Left, Top, Left + Width, Top + Height);
      {$IFDEF WarnDirs}{$WARN UNSAFE_CODE OFF}{$ENDIF}
      // Next find out which monitor window is on
      AMonitor := MonitorFromRect(@WorkArea, MONITOR_DEFAULTTONEAREST);
      // Finally, get work area of relevant monitor
      MonitorInfo.cbSize := SizeOf(MonitorInfo);
      GetMonitorInfo(AMonitor, @MonitorInfo);
      {$IFDEF WarnDirs}{$WARN UNSAFE_CODE ON}{$ENDIF}
      WorkArea:= MonitorInfo.rcWork;
      // offset work area so it has top left of (0,0): fix re issue#26
      OffsetRect(WorkArea, -WorkArea.Left, -WorkArea.Top);
    end;

    // Adjust window if we have got a work area
    if not IsRectEmpty(WorkArea) then
    begin

      // Resize window if too wide or high if resizing permitted
      if Width > WorkArea.Right - WorkArea.Left then
        Width := WorkArea.Right - WorkArea.Left;
      if Height > WorkArea.Bottom - WorkArea.Top then
        Height := WorkArea.Bottom - WorkArea.Top;
      // Adjust left of window if off left or right of work area
      if Left + Width > WorkArea.Right then
        Left := WorkArea.Right - Width;
      if Left < WorkArea.Left then
        Left := WorkArea.Left;
      // Adjust height of window if off top or bottom of work area
      if Top + Height > WorkArea.Bottom then
        Top := WorkArea.Bottom - Height;
      if Top < WorkArea.Top then
        Top := WorkArea.Top;
    end;
  end;

  // Set window to required size

  // Set up the window placement structure
  FillChar(Pl, SizeOf(Pl), #0);
  Pl.Length := SizeOf(TWindowPlacement);
  Pl.rcNormalPosition.Left := Left;
  Pl.rcNormalPosition.Top := Top;
  Pl.rcNormalPosition.Right := Left + Width;
  Pl.rcNormalPosition.Bottom := Top + Height;
  Pl.showCmd := SW_SHOW;      // needed when restore called late in start-up
  // Finally, set the actual size. This call allows for task bar etc.
  {$IFDEF WarnDirs}{$WARN UNSAFE_CODE OFF}{$ENDIF}
  SetWindowPlacement(fWindow.Handle, @Pl);
  {$IFDEF WarnDirs}{$WARN UNSAFE_CODE ON}{$ENDIF}
  // Trigger event to inform that window has been sized
  if Assigned(fOnAfterWindowSized) then
    fOnAfterWindowSized(Self);

  // Set window state

  // Window state is set by posting message to ensure it is done after form
  // shown
  PostMessage(fHook.Handle, PJM_SETWDWSTATE, 0, State);
end;

procedure TPJCustomWdwState.Save;
  {Save window placement, size and state to storage.
  }
var
  Pl: TWindowPlacement; // info structure for placement of window
  R: TRect;             // rectangle to hold normal position & size details
  State: Integer;       // state of window
begin
  Assert(not (csDesigning in ComponentState));

  // Check if there is an owning window - get out if not
  if fWindow = nil then
    Exit;

  // Calculate window's normal size and position using Windows API call - the
  // form's Width, Height, Top and Left properties will give actual window size
  // if form is maximised, which is not what we want here
  Pl.Length := SizeOf(TWindowPlacement);
  {$IFDEF WarnDirs}{$WARN UNSAFE_CODE OFF}{$ENDIF}
  GetWindowPlacement(fWindow.Handle, @Pl);
  {$IFDEF WarnDirs}{$WARN UNSAFE_CODE ON}{$ENDIF}
  R := Pl.rcNormalPosition;

  // Record window state (maximised, minimised or normal)
  // we have a special case when form is app's main form: here if minimised it's
  // the application window that's actually minimised not the form
  if (Application.MainForm = fWindow) and IsIconic(Application.Handle) then
    // minimized main form
    State := Ord(wsMinimized)
  else
    // not mimimized main form - we can rely on window state of form
    State := Ord(fWindow.WindowState);

  // Save window info
  SaveWdwState(R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top, State);
end;

procedure TPJCustomWdwState.SetIgnoreState(const Value: Boolean);
  {Write accessor for IgnoreState property. Includes or excludes woIgnoreState
  property in Options according to Value.
    @param Value [in] New property value.
  }
begin
  if Value <> GetIgnoreState then
  begin
    if Value then
      Include(fOptions, woIgnoreState)
    else
      Exclude(fOptions, woIgnoreState);
  end;
end;

procedure TPJCustomWdwState.SetParentComponent(Value: TComponent);
  {Override of SetParentComponent method. Sets the parent of the hook
  window. This enables the hook window to receive messages from the parent
  window. The method normally called by the streaming system when component is
  loaded on a form. We have made method public so that it can be called
  explicitly when component is dynamically created.
    @param Value [in] Reference to parent component.
  }
begin
  inherited;
  if not (csDesigning in ComponentState) and (Value is TWinControl) then
    fHook.Parent := Value as TWinControl;
end;

procedure TPJCustomWdwState.WMDestroy(var Msg: TMessage);
  {Message handler for owning form's WM_DESTROY message. The message is sent to
  this component by the hook window component. Save the form's state if the
  AutoSaveRestore property is true.
    @param Msg [in/out] Not used.
  }
begin
  if not (csDesigning in ComponentState) and AutoSaveRestore then
    Save;
  inherited;
end;

{ TPJWdwState }

function TPJWdwState.BuildIniFileName(AIniRootDir: TPJWdwStateIniRootDir;
  AIniFileName: string): string;
  {Constructs the ini file name to be used.
    @param AIniRootDir [in] ID of ini file root directory use for relative ini
      file names.
    @param AIniFileName [in] Name of ini file. If this is a relative path it
      will have a directory specified by AIniRootDir prepended.
    @return Required file name. This will always be a rooted file spec.
  }
var
  SubDir: string; // any sub directory to be inserted in relative paths
begin
  if AIniFileName = '' then
    AIniFileName := ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
  if ExtractFileDrive(AIniFileName) = '' then
  begin
    // relative file path
    if (AnsiPos(PathDelim, AIniFileName) = 0)
      and (AIniRootDir in [rdAppDataDir, rdProgramDataDir]) then
      // fIniFileName is a simple file name with no path. Since it's not good
      // practise to write a file in the root of %AppData% or %ProgramData% we
      // interpose a suitable subdirectory for the ini file
      SubDir := 'DelphiDabbler\WindowStateStore\'
    else
      SubDir := '';
    Result := IniRootPath(AIniRootDir) + SubDir + AIniFileName;
  end
  else
    // fully specified file name
    Result := AIniFileName;
end;

constructor TPJWdwState.Create(AOwner: TComponent);
  {Class constructor. Sets default property values.
    @param AOwner [in] Owning component. Must be a TForm.
    @except EPJCustomWdwState raised if Owner is not a TForm.
    @except EPJCustomWdwState raised if there is already a TPJCustomWdwState
      component on the form.
  }
begin
  inherited Create(AOwner);
  fIniFileName := '';
  fSection := '';
  fIniRootDir := rdAppDataDir;
end;

procedure TPJWdwState.GetIniInfo(var AIniFileName, ASection: string);
  {Triggers OnGetIniData event to get ini file and section names to be used when
  restoring / saving window state.
    @param AIniFileName [in/out] Required ini file name. Set to value of
      IniFileName property when called. Can be changed by event handler.
    @param ASection [in/out] Required section name. Set to value of Section
      property when called. Can be changed by event handler.
  }
var
  RootDir: TPJWdwStateIniRootDir;
  FileName: string;
begin
  // Use IniFileName and Section properties as default values
  RootDir := fIniRootDir;
  FileName := fIniFileName;
  ASection := fSection;
  // Allow user to change these by handling OnGetIniData event
  if Assigned(fOnGetIniDataEx) then
    fOnGetIniDataEx(RootDir, FileName, ASection)
  else if Assigned(fOnGetIniData) then
    fOnGetIniData(FileName, ASection);
  AIniFileName := BuildIniFileName(RootDir, FileName);
  if (ASection = '') then
    ASection := 'Window_' + fWindow.Name;
end;

function TPJWdwState.IniFilePath: string;
  {Returns the fully specified file to the ini file used to store window state
  information.
  NOTE: This method will trigger the OnGetIniData and OnGetIniDataEx events.
  }
var
  DummySection: string; // section name returned from GetIniInfo (ignored)
begin
  GetIniInfo(Result, DummySection);
end;

function TPJWdwState.IniRootPath(const AIniRootDir: TPJWdwStateIniRootDir):
  string;
  {Returns the root path specified by the given root directory ID. This root
  directory is used for any ini file names that are relative paths.
    @param AIniRootDir [in] ID of require root directory.
    @return Required path. This is always a rooted path.
  }

  function WindowsFolder: string;
    {Gets the Windows installation directory.
      @return Required directory.
    }
  begin
    SetLength(Result, MAX_PATH);
    SetLength(
      Result, GetWindowsDirectory(PChar(Result), MAX_PATH)
    );
  end;

  function SpecialFolderPath(CSIDL: Integer): string;
    {Gets a specified special folder path.
      @param CSIDL [in] CSDIL_* identifier of required special folder.
      @return Required path.
    }
  var
    PIDL: PItemIDList; // PIDL of the special folder
  begin
    Result := '';
    if Succeeded(SHGetSpecialFolderLocation(0, CSIDL, PIDL)) then
    begin
      try
        SetLength(Result, MAX_PATH);
        if SHGetPathFromIDList(PIDL, PChar(Result)) then
          Result := PChar(Result)
        else
          Result := '';
      finally
        CoTaskMemFree(PIDL);
      end;
    end
  end;

const
  CSIDL_APPDATA = $001a;
  CSIDL_COMMON_APPDATA = $0023;
begin
  case AIniRootDir of
    rdWindowsDir:
      Result := WindowsFolder;
    rdExeDir:
      Result := ExtractFileDir(ParamStr(0));
    rdAppDataDir:
      Result := SpecialFolderPath(CSIDL_APPDATA);
    rdProgramDataDir:
      Result := SpecialFolderPath(CSIDL_COMMON_APPDATA);
  end;
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TPJWdwState.ReadWdwState(var Left, Top, Width, Height,
  State: Integer);
  {Reads window state from ini file.
    @param Left [in/out] Left side of window. Default passed in. Value read from
      ini file passed out, or default if no value read.
    @param Top [in/out] Top size of window. Default passed in. Value read from
      ini file passed out, or default if no value read.
    @param Width [in/out] Width of window. Default width passed in. Value read
      from ini file passed out, or default if no value read.
    @param Height [in/out] Height of window. Default height passed in. Value
      read from ini file passed out, or default if no value read.
    @param State [in/out] Code describing state of window. Default state passed
      in. Value read from ini file passed out, or default if no value read. This
      value is the ordinal value of a TWindowState value.
  }
var
  Ini: TIniFile;        // instance of ini file class used to read info
  AIniFileName: string; // name of ini file from which to read window state
  ASection: string;     // section of ini file from which to read window state
begin
  // Get name of ini file name and section to read window state from
  GetIniInfo(AIniFileName, ASection);
  // Open ini file and read window info from it
  Ini := TIniFile.Create(AIniFileName);
  try
    Left   := Ini.ReadInteger(ASection, 'Left', Left);
    Top    := Ini.ReadInteger(ASection, 'Top', Top);
    Width  := Ini.ReadInteger(ASection, 'Width', Width);
    Height := Ini.ReadInteger(ASection, 'Height', Height);
    State  := Ini.ReadInteger(ASection, 'State', State);
  finally
    Ini.Free;
  end;
end;

procedure TPJWdwState.SaveWdwState(const Left, Top, Width, Height,
  State: Integer);
  {Writes window state to ini file.
    @param Left [in] Left side of window.
    @param Top [in] Top side of window.
    @param Width [in] Width of window.
    @param Height [in] Height of window.
    @param State [in] Code representing state of window. This is the ordinal
      value of a TWindowState value.
  }
var
  Ini: TIniFile;        // instance of ini file class used to write info
  AIniFileName: string; // name of ini file in which to save window state
  ASection: string;     // section of ini file in which to save window state
  ADir: string;         // directory containing ini file
begin
  // Get name of ini file name and section to save window state to
  GetIniInfo(AIniFileName, ASection);
  // Ensure path to ini file exists
  ADir := ExtractFileDir(AIniFileName);
  if ADir <> '' then
    ForceDirectories(ADir);
  // Open ini file and write window info to it
  Ini := TIniFile.Create(AIniFileName);
  try
    Ini.WriteInteger(ASection, 'Left',   Left);
    Ini.WriteInteger(ASection, 'Top',    Top);
    Ini.WriteInteger(ASection, 'Width',  Width);
    Ini.WriteInteger(ASection, 'Height', Height);
    Ini.WriteInteger(ASection, 'State',  State);
  finally
    Ini.Free;
  end;
end;

{ TPJRegWdwState }

resourcestring
  // Error messages
  sErrBadHKEY = '%d is not a valid HKEY value.';

const
  // Map of supported HKEY_ constants onto corresponding TPJRegRootKey values.
  RegRootKeyMap: array[TPJRegRootKey] of HKEY = (
    HKEY_CLASSES_ROOT,          // hkClassesRoot
    HKEY_CURRENT_USER,          // hkCurrentUser
    HKEY_LOCAL_MACHINE,         // hkLocalMachine
    HKEY_USERS,                 // hkUsers
    HKEY_PERFORMANCE_DATA,      // hkPerformanceData
    HKEY_CURRENT_CONFIG,        // hkCurrentConfig
    HKEY_DYN_DATA               // hkDynData
  );

function TryHKEYToCode(const RootKey: HKEY; var Value: TPJRegRootKey): Boolean;
  {Attempts to convert a HKEY value into the corresponding TPJRegRootKey value.
    @param RootKey [in] HKEY value to convert.
    @param Value [in/out] Set to TPJRegRootKey value corresponding to RootKey.
      Value is undefined if RootKey has no corresponding TPJRegRootKey value.
    @return True if RootKey is valid and has corresponding TPJRegRootKey value
      or False of not.
  }
var
  Code: TPJRegRootKey;
begin
  Result := True;
  for Code := Low(TPJRegRootKey) to High(TPJRegRootKey) do
    if RegRootKeyMap[Code] = RootKey then
    begin
      Value := Code;
      Exit;
    end;
  Result := False;
end;

function ReadRegInt(const Reg: TRegistry; const AName: string;
  const ADefault: Integer): Integer;
  {Reads integer value from current sub key in registry, using a default value
  if value doesn't exist in sub key.
    @param Reg [in] Object used to read registry.
    @param AName [in] Name of registry value to read.
    @param ADefault [in] Value to use if AName does not exist.
    @return Value read from registry or default if value doesn't exist.
  }
begin
  if Reg.ValueExists(AName) then
    Result := Reg.ReadInteger(AName)
  else
    Result := ADefault;
end;

procedure WriteRegInt(const Reg: TRegistry; const AName: string;
  const AnInt: Integer);
  {Writes an integer value to current registry key.
    @param Reg [in] Object used to write registry.
    @param AName [in] Name of registry value.
    @param Value [in] Value to be written.
  }
begin
  Reg.WriteInteger(AName, AnInt);
end;

function SafeCreateReg: TRegistry;
  {Safely open registry for read/write, using 64 bit registry view on 64 bits if
  possible.
  NOTE: Versions of Delphi where TRegistry does not permit access flags to be
  changed cannot support using the 64 bit reqistry view.
    @return New TRegistry instance. The user is responsible for freeing this
      object.
  }
{$IFDEF REGACCESSFLAGS}
const
  KEY_WOW64_64KEY = $0100;  // registry access flag not defined in all Delphis
{$ENDIF}
begin
  Result := TRegistry.Create;
  {$IFDEF RegAccessFlags}
  // We use the KEY_WOW64_64KEY access flag to force 32 bit applications running
  // on 64 bit Windows to ue the 64 bit registry view. This flag is ignored by
  // when running on 32 bit Windows from Windows XP (v5.1), but is not supported
  // by Windows 2000 and earlier.
  if (Win32MajorVersion > 5) or
    ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1)) then
    // XP or later
    Result.Access := Result.Access or KEY_WOW64_64KEY;
  {$ENDIF}
end;

constructor TPJRegWdwState.Create(AOwner: TComponent);
  {Class constructor. Sets default property values.
    @param AOwner [in] Owning component. Must be a TForm.
    @except EPJCustomWdwState raised if Owner is not a TForm.
    @except EPJCustomWdwState raised if there is already a TPJCustomWdwState
      component on the form.
  }
begin
  inherited Create(AOwner);
  fRootKeyEx := hkCurrentUser;
  SetSubKey('');
end;

procedure TPJRegWdwState.GetRegInfo(var ARootKey: TPJRegRootKey;
  var ASubKey: string);
  {Triggers the OnGetRegDateEx event or, if that is not assigned, the
  OnGetRegData event, to get registry root key and sub key to be used when
  restoring / saving window state.
    @param ARootKey [in/out] Required root key value. Set to value of RootKey
      property by default. May be changed in event handler.
    @param ASubKey [in/ou] Required sub key. Set to value of SubKey property
      when called. May be changed in event handler.
  }
var
  RootHKey: HKEY; // used to get root key via its HKEY value
begin
  // Use RootKeyEx and SubKey property values by default
  ARootKey := RootKeyEx;
  ASubKey := SubKey;
  // Allow user to change these by handling either OnGetRegDataEx or
  // OnGetRegData event
  if Assigned(fOnGetRegDataEx) then
    fOnGetRegDataEx(ARootKey, ASubKey)
  else if Assigned(fOnGetRegData) then
  begin
    RootHKey := RegRootKeyMap[ARootKey];
    fOnGetRegData(RootHKey, ASubKey);
    if not TryHKEYToCode(RootHKey, ARootKey) then
      raise ERangeError.CreateFmt(sErrBadHKEY, [RootHKey]);
  end;
end;

function TPJRegWdwState.GetRootKey: HKEY;
  {Read accessor for RootKey property.
    @return Required property value.
  }
begin
  Result := RegRootKeyMap[fRootKeyEx];
end;

procedure TPJRegWdwState.ReadWdwState(var Left, Top, Width, Height,
  State: Integer);
  {Reads window state from registry.
    @param Left [in/out] Left side of window. Default passed in. Value read from
      registry passed out, or default if no value read.
    @param Top [in/out] Top size of window. Default passed in. Value read from
      registry passed out, or default if no value read.
    @param Width [in/out] Width of window. Default width passed in. Value read
      from registry passed out, or default if no value read.
    @param Height [in/out] Height of window. Default height passed in. Value
      read from registry passed out, or default if no value read.
    @param State [in/out] Code describing state of window. Default state passed
      in. Value read from registry passed out, or default if no value read. This
      value is the ordinal value of a TWindowState value.
  }
var
  Reg: TRegistry;           // instance of registry object used to read info
  ARootKey: TPJRegRootKey;  // registry root key where window state is stored
  ASubKey: string;          // registry sub key from which to read window state
begin
  // Get registry keys from which to read window state
  GetRegInfo(ARootKey, ASubKey);
  // Open registry at required key
  Reg := SafeCreateReg;
  try
    Reg.RootKey := RegRootKeyMap[ARootKey];
    if Reg.OpenKey(ASubKey, False) then
    begin
      // Read position, size and state of window
      Left   := ReadRegInt(Reg, 'Left',   Left);
      Top    := ReadRegInt(Reg, 'Top',    Top);
      Width  := ReadRegInt(Reg, 'Width',  Width);
      Height := ReadRegInt(Reg, 'Height', Height);
      State  := ReadRegInt(Reg, 'State',  State);
      // Trigger event to enable user to read further data if required
      if Assigned(fOnGettingRegData) then // Added by BJM
        fOnGettingRegData(Reg);
    end;
  finally
    Reg.Free;
  end;
end;

procedure TPJRegWdwState.SaveWdwState(const Left, Top, Width, Height,
  State: Integer);
  {Writes window state to registry.
    @param Left [in] Left side of window.
    @param Top [in] Top side of window.
    @param Width [in] Width of window.
    @param Height [in] Height of window.
    @param State [in] Code representing state of window. This is the ordinal
      value of a TWindowState value.
  }
var
  Reg: TRegistry;           // instance of registry object used to write info
  ARootKey: TPJRegRootKey;  // registry root key where window state is stored
  ASubKey: string;          // sub key of registry in which to save window state
begin
  // Get registry keys in which to save window state
  GetRegInfo(ARootKey, ASubKey);
  // Open registry at required key
  Reg := SafeCreateReg;
  try
    Reg.RootKey := RegRootKeyMap[ARootKey];
    if Reg.OpenKey(ASubKey, True) then
    begin
      // Write window size, position and state from registry
      WriteRegInt(Reg, 'Left',   Left);
      WriteRegInt(Reg, 'Top',    Top);
      WriteRegInt(Reg, 'Width',  Width);
      WriteRegInt(Reg, 'Height', Height);
      WriteRegInt(Reg, 'State',  State);
      // Trigger event to enable user to write further data if required
      if Assigned(fOnPuttingRegData) then // Added by BJM
        fOnPuttingRegData(Reg);
    end;
  finally
    Reg.Free;
  end;
end;

procedure TPJRegWdwState.SetRootKey(const Value: HKEY);
  {Write accessor for RootKey property.
    @param Value [in] New property value.
    @exception ERangeError raised if value is not a recognised HKEY_* value.
  }
begin
  if not TryHKEYToCode(Value, fRootKeyEx) then
  begin
    fRootKeyEx := hkCurrentUser;
    raise ERangeError.CreateFmt(sErrBadHKEY, [Value]);
  end;
end;

procedure TPJRegWdwState.SetSubKey(const Value: string);
  {Write accessor method for SubKey property.
    @param Value [in] New property value. If Value='' then the property is set
      to \Software\<App File Name>\Window\<Form Name>.
  }
begin
  if (Value = '') and not (csDesigning in ComponentState) then
    fSubKey := Format(
      '\Software\%s\Window\%s',
      [ExtractFileName(ParamStr(0)), fWindow.Name]
    )
  else
    fSubKey := Value;
end;

{ TPJUserWdwState }

procedure TPJUserWdwState.ReadWdwState(var Left, Top, Width, Height,
  State: Integer);
  {Gets window state information from OnReadData event. If no event handler is
  assigned default window state is used.
    @param Left [in/out] Left side of window. Default passed in. Value read from
      storage passed out, or default if no value read.
    @param Top [in/out] Top size of window. Default passed in. Value read from
      storage passed out, or default if no value read.
    @param Width [in/out] Width of window. Default width passed in. Value read
      from storage passed out, or default if no value read.
    @param Height [in/out] Height of window. Default height passed in. Value
      read from storage passed out, or default if no value read.
    @param State [in/out] Code describing state of window. Default state passed
      in. Value read from storage passed out, or default if no value read. This
      value is the ordinal value of a TWindowState value.
  }
var
  Data: TPJWdwStateData;  // record containing window state data
begin
  if Assigned(fOnReadData) then
  begin
    // Record default window data
    Data.Left := Left;
    Data.Top := Top;
    Data.Width := Width;
    Data.Height := Height;
    Data.State := State;
    // Trigger event handler
    fOnReadData(Self, Data);
    // Record window data set in event handler
    Left := Data.Left;
    Top := Data.Top;
    Height := Data.Height;
    Width := Data.Width;
    State := Data.State;
  end;
end;

procedure TPJUserWdwState.SaveWdwState(const Left, Top, Width,
  Height, State: Integer);
  {Triggers OnSaveData event to request handler to store window state. If no
  event handler is assigned window state is not recorded.
    @param Left [in] Left side of window.
    @param Top [in] Top side of window.
    @param Width [in] Width of window.
    @param Height [in] Height of window.
    @param State [in] Code representing state of window. This is the ordinal
      value of a TWindowState value.
  }
var
  Data: TPJWdwStateData;  // record containing window state data
begin
  if Assigned(fOnSaveData) then
  begin
    // Record window state information in record
    Data.Left := Left;
    Data.Top := Top;
    Data.Width := Width;
    Data.Height := Height;
    Data.State := State;
    // Trigger event
    fOnSaveData(Self, Data);
  end;
end;

end.

