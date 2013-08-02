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
          ExplicitWidth = 583
          ExplicitHeight = 352
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
            ExplicitWidth = 577
            ExplicitHeight = 316
          end
        end
        object tsCodeEditor: TTabSheet
          Caption = 'Code Editor'
          ImageIndex = 1
          ExplicitWidth = 583
          ExplicitHeight = 352
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
            ExplicitWidth = 577
            ExplicitHeight = 316
          end
        end
      end
    end
  end
end
