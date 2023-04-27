unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCPanel, BCSVGButton, BCSVGViewer, BCListBox, FileUtil;

type

  { TpnlDirectory }

  TpnlDirectory = class(TBCPanel)
  private
    FDirectory: String;
    FIcon: String;
    function GetDirectory: String;
    function GetIcon: String;
    procedure SetDirectory(AValue: String);
    procedure SetIcon(AValue: String);
  public
    constructor Create(TheOwner: TComponent); override;
    property Directory: String read GetDirectory write SetDirectory;
    property Icon: String read GetIcon write SetIcon;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    imgHome: TBCSVGButton;
    imgFavorites: TBCSVGButton;
    imgMenu: TBCSVGButton;
    lblTitleHome: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    pnlHome: TPanel;
    pnlClient: TScrollBox;
    pnlHiddens: TFlowPanel;
    pnlLeft: TPanel;
    pnlTop: TPanel;
    pnlUsers: TFlowPanel;
    procedure FormCreate(Sender: TObject);
    procedure ChangeView(tab: Integer);
    procedure ChangeMode(dark: Boolean);
    procedure pnlEnter(Sender: TObject);
    procedure pnlExit(Sender: TObject);
  private
    FUser: String;
    FTab : Integer;
    FDark : Boolean;
  public

  end;

var
  frmMain: TfrmMain;


const
  FTabDashboard = 1;

  clDarkFont = TColor($cccccc);
  clDarkToolbar = TColor($383838);
  clDarkWorkspace = TColor($303030);

  clLightFont = TColor($222222);
  clLightToolbar = TColor($eeeeee);
  clLightWorkspace = TColor($dddddd);

implementation

{$R *.lfm}

{ TpnlDirectory }

function TpnlDirectory.GetDirectory: String;
begin
  result := FDirectory;
end;

function TpnlDirectory.GetIcon: String;
begin
  result := FIcon;
end;

procedure TpnlDirectory.SetDirectory(AValue: String);
begin
  FDirectory := AValue;
end;

procedure TpnlDirectory.SetIcon(AValue: String);
begin
  FIcon := AValue;
end;

constructor TpnlDirectory.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  BorderSpacing.Right := 8;
  BorderSpacing.Bottom := 8;

end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FUser := GetEnvironmentVariable('USER');

  ChangeView(FTabDashboard);
  ChangeMode(true);


end;

procedure TfrmMain.ChangeView(tab: Integer);
var
  i    : Integer;
  pnl  : TPanel;
  img  : TBCSVGViewer;
  splt : TStringArray;
  last : String;
  dirs : TStringList;
begin

  FTab := tab;

  case FTab of
    FTabDashboard:
    begin
      dirs := TStringList.Create;
      dirs := FileUtil.FindAllDirectories('/home/marion', false);
      dirs.Sort;
      for i := 0 to dirs.Count - 1 do
      begin
        splt := dirs[i].Split(PathDelim, TStringSplitOptions.ExcludeEmpty);
        last := splt[Length(splt) - 1];
        if (last.StartsWith('.')) then
        begin
          pnl := TPanel.Create(pnlHiddens);
          pnl.Parent := pnlHiddens;
        end
        else
        begin
          pnl := TPanel.Create(pnlUsers);
          pnl.Parent := pnlUsers;
        end;
        img := TBCSVGViewer.Create(pnl);
        img.Parent := pnl;
        img.Align := alLeft;
        img.Width := 50;
        img.ColorOpacity := 0;
        case last of
          'Games'    : img.SVG.LoadFromFile('game.svg');
          'Dropbox'  : img.SVG.LoadFromFile('dropbox.svg');
          'Desktop'  : img.SVG.LoadFromFile('desktop.svg');
          'Documents': img.SVG.LoadFromFile('document.svg');
          'Downloads': img.SVG.LoadFromFile('download.svg');
          'Pictures' : img.SVG.LoadFromFile('picture.svg');
          'Videos'   : img.SVG.LoadFromFile('video.svg');
          'Music'    : img.SVG.LoadFromFile('music.svg');
          else
            img.SVG.LoadFromFile('folder.svg');
        end;
        pnl.BevelOuter := bvNone;
        pnl.Caption := last;
        pnl.Width := 250;
        pnl.Height := 50;
        pnl.BorderSpacing.Right := 8;
        pnl.BorderSpacing.Bottom := 8;
        pnl.Color := RGBToColor(60, 60, 60);
        pnl.OnMouseEnter := @pnlEnter;
        pnl.OnMouseLeave  := @pnlExit;
      end;
    end;
  end;
end;

procedure TfrmMain.ChangeMode(dark: Boolean);
begin
  FDark := dark;

  if (FDark) then
  begin
    self.Color      := clDarkWorkspace;
    self.Font.Color := clDarkFont;
    pnlTop.Color    := clDarkToolbar;
    pnlLeft.Color   := clDarkToolbar;
    pnlClient.Color := clDarkWorkspace;
    pnlTop.Font.Color    := clDarkFont;
    pnlLeft.Font.Color   := clDarkFont;
    pnlClient.Font.Color := clDarkFont;
    lblTitleHome.Font.Color := clDarkFont;

    imgMenu.SVGNormalXML.Text := imgMenu.SVGNormalXML.Text.Replace('rgb(0,0,0)', ColorToString(clDarkFont).Substring(3).PadLeft(7, '#'));
    imgHome.SVGNormalXML.Text := imgHome.SVGNormalXML.Text.Replace('rgb(0,0,0)', ColorToString(clDarkFont).Substring(3).PadLeft(7, '#'));
    imgFavorites.SVGNormalXML.Text := imgFavorites.SVGNormalXML.Text.Replace('rgb(0,0,0)', ColorToString(clDarkFont).Substring(3).PadLeft(7, '#'));
  end
  else
  begin
    self.Color      := clLightWorkspace;
    self.Font.Color := clLightFont;
    pnlTop.Color    := clLightToolbar;
    pnlLeft.Color   := clLightToolbar;
    pnlClient.Color := clLightWorkspace;
    pnlTop.Font.Color    := clLightFont;
    pnlLeft.Font.Color   := clLightFont;
    pnlClient.Font.Color := clLightFont;
    lblTitleHome.Font.Color := clLightFont;

    imgMenu.SVGNormalXML.Text := imgMenu.SVGNormalXML.Text.Replace('rgb(0,0,0)', ColorToString(clLightFont).Substring(3).PadLeft(7, '#'));
    imgHome.SVGNormalXML.Text := imgHome.SVGNormalXML.Text.Replace('rgb(0,0,0)', ColorToString(clLightFont).Substring(3).PadLeft(7, '#'));
    imgFavorites.SVGNormalXML.Text := imgFavorites.SVGNormalXML.Text.Replace('rgb(0,0,0)', ColorToString(clLightFont).Substring(3).PadLeft(7, '#'));

  end;

end;

procedure TfrmMain.pnlEnter(Sender: TObject);
begin
  TPanel(Sender).Color:= RGBToColor(50, 50, 50);
end;

procedure TfrmMain.pnlExit(Sender: TObject);
begin
  TPanel(Sender).Color:= RGBToColor(60, 60, 60);
end;

initialization


end.

