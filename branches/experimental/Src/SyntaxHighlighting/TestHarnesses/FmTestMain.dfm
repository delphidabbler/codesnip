object MainTestForm: TMainTestForm
  Left = 0
  Top = 0
  Caption = 'CodeSnip Experiments'
  ClientHeight = 603
  ClientWidth = 906
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 906
    Height = 603
    ActivePage = tsThemesLoader
    Align = alClient
    TabOrder = 0
    object tsThemesLoader: TTabSheet
      Caption = 'Themes Loader'
      DesignSize = (
        898
        575)
      object btnLoadUserThemes: TButton
        Left = 3
        Top = 3
        Width = 118
        Height = 25
        Caption = 'Load User Themes'
        TabOrder = 0
        OnClick = btnLoadUserThemesClick
      end
      object edLoadedThemes: TMemo
        Left = 0
        Top = 40
        Width = 898
        Height = 535
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 5
        WantReturns = False
        WordWrap = False
      end
      object btnLoadDefaultThemes: TButton
        Left = 127
        Top = 3
        Width = 114
        Height = 25
        Caption = 'Load Default Themes'
        TabOrder = 1
        OnClick = btnLoadDefaultThemesClick
      end
      object btnDisplayThemes: TButton
        Left = 374
        Top = 3
        Width = 106
        Height = 25
        Caption = 'Display Themes'
        TabOrder = 3
        OnClick = btnDisplayThemesClick
      end
      object btnSaveAllThemes: TButton
        Left = 486
        Top = 3
        Width = 122
        Height = 25
        Caption = 'Save All Themes'
        TabOrder = 4
        OnClick = btnSaveAllThemesClick
      end
      object btnClearThemes: TButton
        Left = 247
        Top = 3
        Width = 122
        Height = 25
        Caption = 'Clear Themes'
        TabOrder = 2
        OnClick = btnClearThemesClick
      end
    end
    object tsHighlighting: TTabSheet
      Caption = 'Highlighting'
      ImageIndex = 1
      DesignSize = (
        898
        575)
      object lblChooseTheme: TLabel
        Left = 3
        Top = 5
        Width = 117
        Height = 13
        Caption = 'Choose a loaded theme:'
        FocusControl = cbChooseTheme
      end
      object lblChooseBrush: TLabel
        Left = 219
        Top = 5
        Width = 70
        Height = 13
        Caption = 'Choose brush:'
        FocusControl = cbChooseBrush
      end
      object cbChooseTheme: TComboBox
        Left = 3
        Top = 24
        Width = 190
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
      object cbChooseBrush: TComboBox
        Left = 243
        Top = 24
        Width = 190
        Height = 21
        Style = csDropDownList
        TabOrder = 1
      end
      object pcHighlighting: TPageControl
        Left = 3
        Top = 64
        Width = 894
        Height = 508
        ActivePage = tsBrushAttrs
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 2
        object tsBrushAttrs: TTabSheet
          Caption = 'Brush Attributes'
          DesignSize = (
            886
            480)
          object btnDisplayBrushAttrs: TButton
            Left = 3
            Top = 3
            Width = 230
            Height = 25
            Caption = 'Display Resolved Brush Attributes'
            TabOrder = 0
            OnClick = btnDisplayBrushAttrsClick
          end
          object edBrushAttrs: TMemo
            Left = 3
            Top = 35
            Width = 880
            Height = 444
            Anchors = [akLeft, akTop, akRight, akBottom]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
          end
        end
        object tsCodeEditor: TTabSheet
          Caption = 'Code Editor'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          DesignSize = (
            886
            480)
          object btnDisplaySource: TButton
            Left = 3
            Top = 3
            Width = 231
            Height = 25
            Caption = 'Display Default Source Code For Brush'
            TabOrder = 0
            OnClick = btnDisplaySourceClick
          end
          object btnChangeTheme: TButton
            Left = 240
            Top = 3
            Width = 90
            Height = 25
            Caption = 'Change Theme'
            TabOrder = 1
            OnClick = btnChangeThemeClick
          end
          inline frmCodeEditor: TTCodeEditorFrame
            Left = 3
            Top = 35
            Width = 880
            Height = 444
            Anchors = [akLeft, akTop, akRight, akBottom]
            TabOrder = 2
            ExplicitLeft = 3
            ExplicitTop = 35
            ExplicitWidth = 880
            ExplicitHeight = 444
          end
        end
        object tsRendering: TTabSheet
          Caption = 'Rendering'
          ImageIndex = 2
          DesignSize = (
            886
            480)
          object pcRendering: TPageControl
            Left = 3
            Top = 40
            Width = 880
            Height = 437
            ActivePage = tsMockRendering
            Anchors = [akLeft, akTop, akRight, akBottom]
            TabOrder = 0
            object tsMockRendering: TTabSheet
              Caption = 'Mock Rendering'
              object edMockRender: TMemo
                Left = 0
                Top = 0
                Width = 872
                Height = 409
                Align = alClient
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Courier New'
                Font.Style = []
                ParentFont = False
                ScrollBars = ssBoth
                TabOrder = 0
                WordWrap = False
              end
            end
            object tsXHTMLFragSource: TTabSheet
              Caption = 'XHTML Fragment Source'
              ImageIndex = 1
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object edXTHMLFragSource: TMemo
                Left = 0
                Top = 0
                Width = 872
                Height = 409
                Align = alClient
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Courier New'
                Font.Style = []
                ParentFont = False
                ScrollBars = ssBoth
                TabOrder = 0
                WordWrap = False
              end
            end
            object tsXHTMLDocSource: TTabSheet
              Caption = 'XHTML Document Source'
              ImageIndex = 2
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object edXHTMLDocSource: TMemo
                Left = 0
                Top = 0
                Width = 872
                Height = 409
                Align = alClient
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Courier New'
                Font.Style = []
                ParentFont = False
                ScrollBars = ssBoth
                TabOrder = 0
                WordWrap = False
              end
            end
            object tsRTFDocSource: TTabSheet
              Caption = 'RTF Document Source'
              ImageIndex = 3
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object edRTFDocSource: TMemo
                Left = 0
                Top = 0
                Width = 872
                Height = 409
                Align = alClient
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Courier New'
                Font.Style = []
                ParentFont = False
                ScrollBars = ssBoth
                TabOrder = 0
                WordWrap = False
              end
            end
          end
          object btnRenderSyntaxHilite: TButton
            Left = 3
            Top = 9
            Width = 194
            Height = 25
            Caption = 'Render With Brush'
            TabOrder = 1
            OnClick = btnRenderSyntaxHiliteClick
          end
          object btnRenderNull: TButton
            Left = 232
            Top = 9
            Width = 194
            Height = 25
            Caption = 'Render With Null Highlighter'
            TabOrder = 2
            OnClick = btnRenderNullClick
          end
        end
      end
    end
  end
end
