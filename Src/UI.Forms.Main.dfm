inherited MainForm: TMainForm
  ClientHeight = 855
  ClientWidth = 1373
  ExplicitLeft = 4
  ExplicitTop = 4
  ExplicitWidth = 1397
  ExplicitHeight = 919
  PixelsPerInch = 168
  TextHeight = 30
  object MainMenuBar: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 1373
    Height = 46
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    UseSystemFont = False
    ActionManager = ActionManager
    Caption = 'MainMenuBar'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 10461087
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    Spacing = 0
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 818
    Width = 1373
    Height = 37
    Panels = <
      item
        Width = 260
      end
      item
        Width = 160
      end
      item
        Width = 50
      end>
  end
  object tbarMain: TToolBar
    Left = 0
    Top = 46
    Width = 1373
    Height = 47
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 39
    ButtonWidth = 40
    Caption = 'tbarMain'
    EdgeBorders = [ebTop, ebBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    object tbSaveSnippet: TToolButton
      Left = 0
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSaveSnippet
    end
    object tbSaveUnit: TToolButton
      Left = 40
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSaveUnit
    end
    object tbSpacer7: TToolButton
      Left = 80
      Top = 0
      Width = 14
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'tbSpacer7'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbPrint: TToolButton
      Left = 94
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actPrint
    end
    object tbSpacer1: TToolButton
      Left = 134
      Top = 0
      Width = 14
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'tbSpacer1'
      ImageIndex = 7
      Style = tbsSeparator
    end
    object tbFindText: TToolButton
      Left = 148
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actFindText
    end
    object tbFindCompiler: TToolButton
      Left = 188
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actFindCompiler
    end
    object tbSelectSnippets: TToolButton
      Left = 228
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSelectSnippets
    end
    object tbFindClear: TToolButton
      Left = 268
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actFindClear
    end
    object tbSpacer3: TToolButton
      Left = 308
      Top = 0
      Width = 14
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'tbSpacer3'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object tbGoBack: TToolButton
      Left = 322
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actGoBack
      Style = tbsDropDown
    end
    object tbGoForward: TToolButton
      Left = 391
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actGoForward
      Style = tbsDropDown
    end
    object tbSpacer8: TToolButton
      Left = 460
      Top = 0
      Width = 14
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'tbSpacer8'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbAddSnippet: TToolButton
      Left = 474
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actAddSnippet
    end
    object tbEditSnippet: TToolButton
      Left = 514
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actEditSnippet
    end
    object tbDeleteSnippet: TToolButton
      Left = 554
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actDeleteSnippet
    end
    object tbSaveDatabase: TToolButton
      Left = 594
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actSaveDatabase
    end
    object tbFavourites: TToolButton
      Left = 634
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actAddFavourite
    end
    object tbSpacer5: TToolButton
      Left = 674
      Top = 0
      Width = 14
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'tbSpacer5'
      ImageIndex = 13
      Style = tbsSeparator
    end
    object tbTestCompile: TToolButton
      Left = 688
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actTestCompile
    end
    object tbSpacer6: TToolButton
      Left = 728
      Top = 0
      Width = 14
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'tbSpacer6'
      ImageIndex = 12
      Style = tbsSeparator
    end
    object tbHelpContents: TToolButton
      Left = 742
      Top = 0
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actHelpContents
    end
  end
  object pnlBody: TPanel
    Left = 0
    Top = 93
    Width = 1373
    Height = 725
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 3
    ExplicitWidth = 1385
    ExplicitHeight = 727
    object splitVert: TSplitter
      Left = 325
      Top = 2
      Width = 11
      Height = 721
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Beveled = True
      ExplicitLeft = 327
      ExplicitTop = 4
      ExplicitHeight = 682
    end
    object pnlLeft: TPanel
      Left = 2
      Top = 2
      Width = 323
      Height = 721
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      BevelOuter = bvLowered
      TabOrder = 0
      ExplicitHeight = 723
      inline frmOverview: TOverviewFrame
        Left = 1
        Top = 1
        Width = 321
        Height = 719
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitLeft = -234
        inherited pnlTitle: TPanel
          Width = 321
          inherited bvlTop: TBevel
            Width = 321
          end
          inherited tbarOverview: TToolBar
            Left = 227
          end
        end
        inherited tcDisplayStyle: TTabControl
          Width = 321
          Height = 672
          inherited tvSnippets: TTreeView
            Top = 76
            Width = 313
            Height = 592
          end
        end
      end
    end
    object pnlRight: TPanel
      Left = 336
      Top = 2
      Width = 1035
      Height = 721
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BevelOuter = bvLowered
      TabOrder = 1
      ExplicitWidth = 1047
      ExplicitHeight = 723
      inline frmDetail: TDetailFrame
        Left = 1
        Top = 1
        Width = 1033
        Height = 719
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 14
        ExplicitTop = 14
        inherited pnlTitle: TPanel
          Width = 1033
          inherited bvlTop: TBevel
            Width = 1033
          end
        end
        inherited frmDetailView: TDetailViewFrame
          Width = 1033
          Height = 649
          ExplicitTop = 70
          ExplicitWidth = 557
          ExplicitHeight = 347
          inherited pnlBrowser: TPanel
            Width = 1033
            Height = 649
            ExplicitWidth = 557
            ExplicitHeight = 347
            inherited wbBrowser: TWebBrowser
              Width = 1033
              Height = 649
              ExplicitWidth = 557
              ExplicitHeight = 347
              ControlData = {
                4C000000DE2000006E1300000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
        inherited tcViews: TTabControl
          Width = 1033
        end
      end
    end
  end
  object ActionList: TActionList
    Left = 414
    Top = 530
    object actSaveSnippet: TAction
      Category = 'File'
      Caption = 'Save Annotated Source...'
      Hint = 
        'Save annotated source code|Save the annotated source code of the' +
        ' selected routine or category to a file'
      ImageIndex = 18
      ShortCut = 24654
    end
    object actAddSnippet: TAction
      Category = 'Snippets'
      Caption = 'New Snippet...'
      Hint = 
        'Create new snippet|Create a new snippet and add it to the user d' +
        'atabase'
      ImageIndex = 27
      ShortCut = 16429
    end
    object actDeleteSnippet: TAction
      Category = 'Snippets'
      Caption = 'Delete Snippet'
      Hint = 
        'Delete snippet|Delete the selected snippet from the user databas' +
        'e'
      ImageIndex = 26
      ShortCut = 16430
    end
    object actSaveUnit: TAction
      Category = 'File'
      Caption = 'Save Unit...'
      Hint = 
        'Save unit|Generate and save a Pascal unit containing valid snipp' +
        'ets in the current selection'
      ImageIndex = 14
      ShortCut = 16469
    end
    object actWelcome: TAction
      Category = 'View'
      Caption = 'Welcome Page'
      Hint = 'Welcome page|Display the welcome page'
      ImageIndex = 5
      ShortCut = 32804
    end
    object actEditSnippet: TAction
      Category = 'Snippets'
      Caption = 'Edit Snippet...'
      Hint = 'Edit snippet|Edit the selected user-defined snippet'
      ImageIndex = 28
      ShortCut = 16497
    end
    object actExit: TFileExit
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit|Close the application'
      ImageIndex = 0
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      Hint = 'About box|Display the program'#39's about box'
      ImageIndex = 2
    end
    object actCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      Hint = 'Copy|Copy the selected text to the clipboard'
      ImageIndex = 1
      ImageName = 'outline_print_white'
      ShortCut = 16451
    end
    object actBugReport: TAction
      Category = 'Tools'
      Caption = 'Report Bug Online...'
      Hint = 'Report a bug|Report a bug using the online bug tracker'
      ImageIndex = 17
    end
    object actFindCompiler: TAction
      Category = 'Search'
      Caption = 'Find Compiler(s)...'
      Hint = 
        'Find compiler(s)|Search for all snippets with a given compiler c' +
        'ompatibility'
      ImageIndex = 4
      ShortCut = 24646
    end
    object actFindText: TAction
      Category = 'Search'
      Caption = 'Find Text...'
      Hint = 'Find text|Search for all snippets containing specified text'
      ImageIndex = 3
      ShortCut = 16454
    end
    object actGoBack: TAction
      Category = 'View'
      Caption = 'Previous'
      Hint = 'Previous item|Display the previous item in the history list'
      ImageIndex = 10
      ShortCut = 32805
    end
    object actGoForward: TAction
      Category = 'View'
      Caption = 'Next'
      Hint = 'Next item|Display the next item in the history list'
      ImageIndex = 11
      ShortCut = 32807
    end
    object actCopySnippet: TAction
      Category = 'Edit'
      Caption = 'Copy Annotated Source'
      Hint = 
        'Copy annotated source code|Copy the annotated source code of the' +
        ' selected routine or category to the clipboard'
      ImageIndex = 1
      ImageName = 'outline_print_white'
      ShortCut = 16462
    end
    object actSelectAll: TAction
      Category = 'Edit'
      Caption = 'Select All'
      Hint = 'Select all text|Select all text in the Details pane'
      ShortCut = 16449
    end
    object actViewCategorised: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Categorised Snippets'
      Checked = True
      GroupIndex = 100
      Hint = 'Categorised snippets|View snippets grouped by category'
    end
    object actViewAlphabetical: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Alphabetical Snippets'
      GroupIndex = 100
      Hint = 
        'Alphabetical snippets|View snippets in alphabetical order groupe' +
        'd by initial letter'
    end
    object actViewSnippetKinds: TAction
      Category = 'View'
      Caption = 'Snippets By Kind'
      GroupIndex = 100
      Hint = 'Snippets by kind|View snippets grouped by kind'
    end
    object actHelpQuickStart: TAction
      Category = 'Help'
      Caption = 'QuickStart Guide'
      Hint = 'QuickStart guide|Display the QuickStart help topic'
    end
    object actCompilers: TAction
      Category = 'Tools'
      Caption = 'Configure Compilers...'
      Hint = 
        'Configure compilers|Configure the program to use installed Delph' +
        'i or Free Pascal compilers'
      ImageIndex = 15
    end
    object actHelpCompChecks: TAction
      Category = 'Help'
      Caption = 'About Compiler Checks'
      Hint = 'About compiler checks|Display help about test-compiling snippets'
    end
    object actHelpContents: TAction
      Category = 'Help'
      Caption = 'Contents'
      Hint = 'Help contents|Display the help contents page'
      ImageIndex = 1
      ShortCut = 112
    end
    object actPreferences: TAction
      Category = 'Tools'
      Caption = 'Preferences...'
      Hint = 'Preferences|Customise the program'
      ImageIndex = 16
    end
    object actLicense: TAction
      Category = 'Help'
      Caption = 'License'
      Hint = 'View license|View a summary of the end user license agreement'
      ImageIndex = 35
    end
    object actGitHubHome: TBrowseURL
      Category = 'Help'
      Caption = 'CodeSnip On GitHub'
      Hint = 
        'Codesnip GitHub project|Display CodeSnip'#39's GitHub web page in th' +
        'e default web browser'
      ImageIndex = 6
    end
    object actFindXRefs: TAction
      Category = 'Search'
      Caption = 'Find Cross Refs...'
      Hint = 
        'Find X-refs|Search for cross references to and/or from the selec' +
        'ted snippet'
      ImageIndex = 20
      ShortCut = 16466
    end
    object actSelectSnippets: TAction
      Category = 'Search'
      Caption = 'Select Snippets...'
      Hint = 'Select snippets|Manually select the snippets to be displayed'
      ImageIndex = 19
      ShortCut = 24659
    end
    object actFindClear: TAction
      Category = 'Search'
      Caption = 'Show All'
      Hint = 'Show all snippets|Clear the last search and display all snippets'
      ImageIndex = 9
      ShortCut = 24641
    end
    object actTestCompile: TAction
      Category = 'Compile'
      Caption = 'Test Compile Snippet...'
      Hint = 
        'Test compile|Test compile the currently selected snippet using a' +
        'll configured compilers'
      ImageIndex = 12
      ShortCut = 120
    end
    object actViewCompErrs: TAction
      Category = 'Compile'
      Caption = 'View Compile Errors...'
      Hint = 
        'View compile errors|Display any errors and warnings resulting fr' +
        'om the last test compilation'
      ImageIndex = 34
    end
    object actViewTestUnit: TAction
      Category = 'Compile'
      Caption = 'View Test Unit...'
      Hint = 
        'View test unit|Display the unit used to test compile the selecte' +
        'd snippet'
      ImageIndex = 22
    end
    object actTestBug: TAction
      Caption = 'actTestBug'
      ShortCut = 57410
    end
    object actNextTab: TAction
      Caption = 'actNextTab'
      ShortCut = 16393
    end
    object actPreviousTab: TAction
      Caption = 'actPreviousTab'
      ShortCut = 24585
    end
    object actPrint: TAction
      Category = 'File'
      Caption = 'Print...'
      Hint = 'Print|Print the selected snippet or category'
      ImageIndex = 24
      ShortCut = 16464
    end
    object actBackupDatabase: TAction
      Category = 'Database'
      Caption = 'Backup User Database...'
      Hint = 'Backup user database|Backup the user-defined snippet database'
      ImageIndex = 33
    end
    object actRestoreDatabase: TAction
      Category = 'Database'
      Caption = 'Restore User Database...'
      Hint = 
        'Restore user database|Restore the user-defined snippet database ' +
        'from a backup'
      ImageIndex = 32
    end
    object actSaveDatabase: TAction
      Category = 'Database'
      Caption = 'Save User Database'
      Hint = 
        'Save user database|Save all changes to the user-defined snippet ' +
        'database'
      ImageIndex = 25
      ShortCut = 16467
    end
    object actUpdateDbase: TAction
      Category = 'Database'
      Caption = 'Install or Update DelphiDabbler Snippets Database...'
      Hint = 
        'Install or update DelphiDabbler Code Snippets database|Install o' +
        'r update the main DelphiDabbler Code Snippets database'
    end
    object actExportCode: TAction
      Category = 'Snippets'
      Caption = 'Export Snippets...'
      Hint = 'Export snippets|Export one or more snippets to a file'
    end
    object actImportCode: TAction
      Category = 'Snippets'
      Caption = 'Import Snippets...'
      Hint = 'Import snippets|Import one or more snippets from a file'
    end
    object actCopyInfo: TAction
      Category = 'Edit'
      Caption = 'Copy Information'
      Hint = 
        'Copy information|Copy information about the selected snippet to ' +
        'the clipboard'
      ShortCut = 16457
    end
    object actViewDependencies: TAction
      Category = 'View'
      Caption = 'Dependencies...'
      Hint = 
        'View dependencies|Display the names of snippets that depend on, ' +
        'or are required by, the selected snippet'
      ImageIndex = 31
      ShortCut = 16452
    end
    object actCollapseTree: TAction
      Category = 'View'
      Caption = 'Collapse All Sections'
      Hint = 'Collapse all sections|Collapse all sections in the Overview pane'
      ImageIndex = 47
    end
    object actExpandTree: TAction
      Category = 'View'
      Caption = 'Expand All Sections'
      Hint = 'Expand all sections|Expand all sections in the Overview pane'
      ImageIndex = 46
    end
    object actExpandNode: TAction
      Category = 'View'
      Caption = 'Expand Section'
      Hint = 'Expand section|Expand the current section of the Overview pane'
      ImageIndex = 30
    end
    object actCollapseNode: TAction
      Category = 'View'
      Caption = 'Collapse Section'
      Hint = 
        'Collapse section|Collapse the current section of the Overview pa' +
        'ne'
      ImageIndex = 29
    end
    object actCopySource: TAction
      Category = 'Edit'
      Caption = 'Copy Source Code'
      Hint = 
        'Copy source code|Copy the source code of the selected snippet to' +
        ' the clipboard'
      ShortCut = 24643
    end
    object actAddCategory: TAction
      Category = 'Categories'
      Caption = 'New Category...'
      Hint = 
        'New category|Create a new category and add it to the user databa' +
        'se'
      ImageIndex = 8
    end
    object actRenameCategory: TAction
      Category = 'Categories'
      Caption = 'Rename Category...'
      Hint = 'Rename category|Rename a user-defined category'
      ImageIndex = 21
    end
    object actDeleteCategory: TAction
      Category = 'Categories'
      Caption = 'Delete Category...'
      Hint = 'Delete category|Delete an empty category from the user database'
      ImageIndex = 13
    end
    object actNewDetailsTab: TAction
      Category = 'View'
      Caption = 'New Tab'
      Hint = 'New tab|Create a new, empty, tab the Details pane'
      ImageIndex = 39
      ShortCut = 16468
    end
    object actCloseDetailsTab: TAction
      Category = 'View'
      Caption = 'Close Tab'
      Hint = 'Close tab|Close the currently selected tab in the Details pane'
      ImageIndex = 38
      SecondaryShortCuts.Strings = (
        'Ctrl+W')
      ShortCut = 16499
    end
    object actFAQs: TBrowseURL
      Category = 'Help'
      Caption = 'FAQs'
      Hint = 
        'FAQs|Display CodeSnip'#39's online Frequently Asked Questions in the' +
        ' default web browser'
      ImageIndex = 6
    end
    object actDuplicateSnippet: TAction
      Category = 'Snippets'
      Caption = 'Duplicate Snippet...'
      Hint = 
        'Duplicate snippet|Duplicate the selected snippet and add it to t' +
        'he user-defined database'
      ImageIndex = 37
      ShortCut = 24644
    end
    object actSaveSelection: TAction
      Category = 'File'
      Caption = 'Save Selection...'
      Hint = 
        'Save current selection|Save information about the currently sele' +
        'cted snippets to a file'
      ShortCut = 41043
    end
    object actLoadSelection: TAction
      Category = 'File'
      Caption = 'Load Selection...'
      Hint = 
        'Load saved selection|Restore the snippet selection recorded in a' +
        ' previously saved file'
      ShortCut = 41036
    end
    object actCloseUnselectedDetailsTabs: TAction
      Category = 'View'
      Caption = 'Close All Other Tabs'
      Hint = 
        'Close all other tabs|Close all tabs in the Details pane except f' +
        'or the current one'
      ShortCut = 24691
    end
    object actCloseAllDetailsTabs: TAction
      Category = 'View'
      Caption = 'Close All Tabs'
      Hint = 'Close all tabs|Close all tabs in the Details pane'
    end
    object actFavourites: TAction
      Category = 'Snippets'
      Caption = 'Show Favourites...'
      Hint = 'Show favourites|Display the Favourites dialogue box'
      ShortCut = 115
    end
    object actAddFavourite: TAction
      Category = 'Snippets'
      Caption = 'Add To Favourites'
      Hint = 'Add to favourites|Make the selected snippet a Favourite'
      ImageIndex = 48
      ShortCut = 41030
    end
    object actMoveUserDatabase: TAction
      Category = 'Database'
      Caption = 'Move User Database...'
      Hint = 
        'Move user database|Move the user-defined snippet database to a n' +
        'ew directory'
    end
    object actSWAGImport: TAction
      Category = 'Snippets'
      Caption = 'Import Snippets From SWAG...'
      Hint = 
        'Import snippets from SWAG|Import one or more snippets into the u' +
        'ser database from the SWAG database'
    end
    object actBlog: TBrowseURL
      Category = 'Help'
      Caption = 'CodeSnip News Blog'
      Hint = 
        'Display CodeSnip news blog|Display the CodeSnip News Blog in the' +
        ' default web browser'
      ImageIndex = 6
    end
    object actDeleteUserDatabase: TAction
      Category = 'Database'
      Caption = 'Delete User Database...'
      Hint = 
        'Delete User Database|Deletes the user'#39's snippets database - USE ' +
        'WITH CAUTION'
    end
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = actSaveSnippet
                Caption = '&Save Annotated Source...'
                ImageIndex = 18
                ShortCut = 24654
              end
              item
                Action = actSaveUnit
                Caption = 'S&ave Unit...'
                ImageIndex = 14
                ShortCut = 16469
              end
              item
                Caption = '-'
              end
              item
                Action = actSaveSelection
                Caption = 'Sa&ve Selection...'
                ShortCut = 41043
              end
              item
                Action = actLoadSelection
                Caption = '&Load Selection...'
                ShortCut = 41036
              end
              item
                Caption = '-'
              end
              item
                Action = actPrint
                Caption = '&Print...'
                ImageIndex = 24
                ShortCut = 16464
              end
              item
                Caption = '-'
              end
              item
                Action = actExit
                Caption = '&Exit'
                ImageIndex = 0
                ImageName = 'outline_cancel_white'
              end>
            Caption = 'F&ile'
          end
          item
            Items = <
              item
                Action = actCopy
                Caption = '&Copy'
                ImageIndex = 1
                ImageName = 'outline_print_white'
                ShortCut = 16451
              end
              item
                Caption = '-'
              end
              item
                Action = actCopySource
                Caption = 'C&opy Source Code'
                ShortCut = 24643
              end
              item
                Action = actCopySnippet
                Caption = 'Co&py Annotated Source'
                ImageIndex = 1
                ImageName = 'outline_print_white'
                ShortCut = 16462
              end
              item
                Action = actCopyInfo
                Caption = 'Cop&y Information'
                ShortCut = 16457
              end
              item
                Caption = '-'
              end
              item
                Action = actSelectAll
                Caption = '&Select All'
                ShortCut = 16449
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Action = actGoBack
                Caption = '&Previous'
                ImageIndex = 10
                ShortCut = 32805
              end
              item
                Action = actGoForward
                Caption = '&Next'
                ImageIndex = 11
                ShortCut = 32807
              end
              item
                Action = actWelcome
                Caption = '&Welcome Page'
                ImageIndex = 5
                ShortCut = 32804
              end
              item
                Caption = '-'
              end
              item
                Action = actViewCategorised
                Caption = 'C&ategorised Snippets'
              end
              item
                Action = actViewAlphabetical
                Caption = 'A&lphabetical Snippets'
              end
              item
                Action = actViewSnippetKinds
                Caption = '&Snippets By Kind'
              end
              item
                Caption = '-'
              end
              item
                Action = actNewDetailsTab
                Caption = 'New &Tab'
                ImageIndex = 39
                ShortCut = 16468
              end
              item
                Action = actCloseDetailsTab
                Caption = 'Cl&ose Tab'
                ImageIndex = 38
                ShortCut = 16499
              end
              item
                Action = actCloseAllDetailsTabs
                Caption = 'Close All Ta&bs'
              end
              item
                Action = actCloseUnselectedDetailsTabs
                Caption = 'Close All Ot&her Tabs'
                ShortCut = 24691
              end
              item
                Caption = '-'
              end
              item
                Action = actViewDependencies
                Caption = '&Dependencies...'
                ImageIndex = 31
                ShortCut = 16452
              end
              item
                Caption = '-'
              end
              item
                Action = actCollapseNode
                Caption = 'Collapse Sect&ion'
                ImageIndex = 29
              end
              item
                Action = actExpandNode
                Caption = 'E&xpand Section'
                ImageIndex = 30
              end
              item
                Action = actCollapseTree
                Caption = '&Collapse All Sections'
                ImageIndex = 47
              end
              item
                Action = actExpandTree
                Caption = '&Expand All Sections'
                ImageIndex = 46
              end>
            Caption = '&View'
          end
          item
            Items = <
              item
                Action = actFindText
                Caption = '&Find Text...'
                ImageIndex = 3
                ShortCut = 16454
              end
              item
                Action = actFindCompiler
                Caption = 'F&ind Compiler(s)...'
                ImageIndex = 4
                ShortCut = 24646
              end
              item
                Action = actFindXRefs
                Caption = 'Fi&nd Cross Refs...'
                ImageIndex = 20
                ShortCut = 16466
              end
              item
                Action = actSelectSnippets
                Caption = '&Select Snippets...'
                ImageIndex = 19
                ShortCut = 24659
              end
              item
                Caption = '-'
              end
              item
                Action = actFindClear
                Caption = 'S&how All'
                ImageIndex = 9
                ShortCut = 24641
              end>
            Caption = '&Search'
          end
          item
            Items = <
              item
                Action = actAddSnippet
                Caption = '&New Snippet...'
                ImageIndex = 27
                ShortCut = 16429
              end
              item
                Action = actEditSnippet
                Caption = '&Edit Snippet...'
                ImageIndex = 28
                ShortCut = 16497
              end
              item
                Action = actDuplicateSnippet
                Caption = '&Duplicate Snippet...'
                ImageIndex = 37
                ShortCut = 24644
              end
              item
                Action = actDeleteSnippet
                Caption = 'De&lete Snippet'
                ImageIndex = 26
                ShortCut = 16430
              end
              item
                Caption = '-'
              end
              item
                Action = actExportCode
                Caption = 'E&xport Snippets...'
              end
              item
                Action = actImportCode
                Caption = '&Import Snippets...'
              end
              item
                Action = actSWAGImport
                Caption = 'I&mport Snippets From SWAG...'
              end
              item
                Caption = '-'
              end
              item
                Action = actAddFavourite
                Caption = '&Add To Favourites'
                ImageIndex = 48
                ShortCut = 41030
              end
              item
                Action = actFavourites
                Caption = '&Show Favourites...'
                ShortCut = 115
              end>
            Caption = 'S&nippets'
          end
          item
            Items = <
              item
                Action = actAddCategory
                Caption = '&New Category...'
                ImageIndex = 8
              end
              item
                Action = actRenameCategory
                Caption = '&Rename Category...'
                ImageIndex = 21
              end
              item
                Action = actDeleteCategory
                Caption = '&Delete Category...'
                ImageIndex = 13
              end>
            Caption = '&Categories'
          end
          item
            Items = <
              item
                Action = actSaveDatabase
                Caption = '&Save User Database'
                ImageIndex = 25
                ShortCut = 16467
              end
              item
                Caption = '-'
              end
              item
                Action = actBackupDatabase
                Caption = '&Backup User Database...'
                ImageIndex = 33
              end
              item
                Action = actRestoreDatabase
                Caption = '&Restore User Database...'
                ImageIndex = 32
              end
              item
                Caption = '-'
              end
              item
                Action = actUpdateDbase
                Caption = '&Install or Update DelphiDabbler Snippets Database...'
              end
              item
                Caption = '-'
              end
              item
                Action = actMoveUserDatabase
                Caption = '&Move User Database...'
              end
              item
                Caption = '-'
              end
              item
                Action = actDeleteUserDatabase
                Caption = '&Delete User Database...'
              end>
            Caption = '&Database'
          end
          item
            Items = <
              item
                Action = actTestCompile
                Caption = '&Test Compile Snippet...'
                ImageIndex = 12
                ShortCut = 120
              end
              item
                Action = actViewCompErrs
                Caption = '&View Compile Errors...'
                ImageIndex = 34
              end
              item
                Action = actViewTestUnit
                Caption = 'V&iew Test Unit...'
                ImageIndex = 22
              end>
            Caption = 'C&ompile'
          end
          item
            Items = <
              item
                Action = actPreferences
                Caption = '&Preferences...'
                ImageIndex = 16
              end
              item
                Action = actCompilers
                Caption = '&Configure Compilers...'
                ImageIndex = 15
              end
              item
                Caption = '-'
              end
              item
                Action = actBugReport
                Caption = '&Report Bug Online...'
                ImageIndex = 17
              end>
            Caption = '&Tools'
          end
          item
            Items = <
              item
                Action = actHelpContents
                Caption = '&Contents'
                ImageIndex = 1
                ImageName = 'outline_print_white'
                ShortCut = 112
              end
              item
                Action = actHelpQuickStart
                Caption = '&QuickStart Guide'
              end
              item
                Action = actHelpCompChecks
                Caption = '&About Compiler Checks'
              end
              item
                Action = actLicense
                Caption = '&License'
                ImageIndex = 35
              end
              item
                Caption = '-'
              end
              item
                Action = actBlog
                Caption = 'C&odeSnip News Blog'
                ImageIndex = 6
              end
              item
                Action = actGitHubHome
                Caption = 'Co&deSnip On GitHub'
                ImageIndex = 6
              end
              item
                Action = actFAQs
                Caption = '&FAQs'
                ImageIndex = 6
              end
              item
                Caption = '-'
              end
              item
                Action = actAbout
                Caption = 'A&bout...'
                ImageIndex = 2
              end>
            Caption = '&Help'
          end>
        ActionBar = MainMenuBar
      end
      item
      end>
    LinkedActionLists = <
      item
        ActionList = ActionList
        Caption = 'ActionList'
      end>
    Left = 408
    Top = 650
    StyleName = 'Platform Default'
  end
end
