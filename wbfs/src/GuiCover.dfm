object CoverForm: TCoverForm
  Left = 0
  Top = 0
  Caption = 'CoverForm'
  ClientHeight = 500
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXButton1: TSpTBXButton
    Left = 56
    Top = 8
    Width = 129
    Height = 25
    Action = DownloadCovers
    TabOrder = 0
  end
  object CoverPanel: TSpTBXDockablePanel
    Left = 56
    Top = 39
    Width = 297
    Height = 409
    Caption = 'Cover'
    TabOrder = 1
    object Viewer: TGLSceneViewer
      Left = 0
      Top = 19
      Width = 297
      Height = 369
      Camera = Camera
      Buffer.AntiAliasing = aa4xHQ
      FieldOfView = 117.563194274902300000
      Align = alClient
      OnMouseDown = ViewerMouseDown
      OnMouseMove = ViewerMouseMove
      OnMouseUp = ViewerMouseUp
    end
    object CoverLangChooser: TSpTBXComboBox
      Left = 0
      Top = 388
      Width = 297
      Height = 21
      Align = alBottom
      ItemHeight = 13
      TabOrder = 2
      OnChange = CoverLangChooserChange
    end
  end
  object ActionList: TActionList
    Left = 8
    Top = 16
    object DownloadInfos: TAction
      Caption = 'DownloadInfos'
      OnExecute = DownloadInfosExecute
    end
    object DownloadCovers: TAction
      Caption = 'DownloadCovers'
      OnExecute = DownloadCoversExecute
    end
  end
  object XMLDoc: TXMLDocument
    Left = 8
    Top = 48
    DOMVendorDesc = 'MSXML'
  end
  object CoverThread: TJvThread
    Exclusive = False
    MaxCount = 0
    RunOnCreate = True
    FreeOnTerminate = True
    OnBegin = CoverThreadBegin
    OnExecute = CoverThreadExecute
    OnFinish = CoverThreadFinish
    Left = 8
    Top = 80
  end
  object Scene: TGLScene
    Left = 8
    Top = 112
    object Lights: TGLDummyCube
      CubeSize = 1.000000000000000000
      object Light: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {6666263F6666263F6666263F0000803F}
        Position.Coordinates = {000000000000A040000020410000803F}
        SpotCutOff = 180.000000000000000000
      end
      object LightLeft: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {3333B33E3333B33E3333B33E0000803F}
        Position.Coordinates = {000020C10000A041000020410000803F}
        SpotCutOff = 180.000000000000000000
      end
      object LightRight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {0000003F0000003F0000003F0000803F}
        Position.Coordinates = {000040410000A041000020C10000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
    object GridAxes: TGLDummyCube
      ShowAxes = True
      Visible = False
      CubeSize = 1.000000000000000000
    end
    object Origin3dsmax: TGLDummyCube
      Direction.Coordinates = {000000000000803FF6443FB300000000}
      PitchAngle = 90.000000000000000000
      Scale.Coordinates = {CDCCCC3CCDCCCC3CCDCCCC3C00000000}
      Up.Coordinates = {00000000F6443FB3000080BF00000000}
      CubeSize = 1.000000000000000000
      object Wiibox: TGLDummyCube
        CubeSize = 1.000000000000000000
        object Box: TGLFreeForm
          Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        end
        object Cover: TGLFreeForm
          Material.Texture.MinFilter = miNearestMipmapNearest
        end
        object FakeCenter: TGLDummyCube
          Position.Coordinates = {0000000000000000000096420000803F}
          CubeSize = 1.000000000000000000
        end
      end
    end
    object Mirror: TGLMirror
      Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F9A99193F}
      Material.BlendingMode = bmTransparency
      Direction.Coordinates = {49AB9FB905F67F3F92F28E3C00000000}
      PitchAngle = 90.000000000000000000
      TurnAngle = -90.000000000000000000
      Up.Coordinates = {05F67F3FE3E737B370F88E3C00000000}
      MirrorObject = Origin3dsmax
      Height = 1.000000000000000000
      Width = 1.000000000000000000
      Radius = 35.000000000000000000
      Slices = 48
      Shape = msDisk
    end
    object Camera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 90.000000000000000000
      TargetObject = FakeCenter
      Position.Coordinates = {000040C000000040000000410000803F}
      object CameraLight: TGLLightSource
        ConstAttenuation = 1.000000000000000000
        Diffuse.Color = {9A99993E9A99993E9A99993E0000803F}
        SpotCutOff = 180.000000000000000000
      end
    end
  end
  object Cadencer: TGLCadencer
    Scene = Scene
    SleepLength = 20
    OnProgress = CadencerProgress
    Left = 8
    Top = 144
  end
  object FormStorage: TJvFormStorage
    AppStorage = MainForm.Storage
    AppStoragePath = '%FORM_NAME%\'
    Options = []
    StoredProps.Strings = (
      'CoverLangChooser.ItemIndex')
    StoredValues = <>
    Left = 8
    Top = 176
  end
end
