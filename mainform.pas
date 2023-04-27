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
    imgFavorites: TBCSVGButton;
    imgHome: TBCSVGButton;
    imgMenu: TBCSVGButton;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    pnlHome: TPanel;
    pnlClient: TScrollBox;
    pnlHiddens: TFlowPanel;
    pnlTop: TBCPanel;
    pnlLeft: TBCPanel;
    pnlUsers: TFlowPanel;
    procedure FormCreate(Sender: TObject);
    procedure ChangeView(tab: Integer);
    procedure pnlEnter(Sender: TObject);
    procedure pnlExit(Sender: TObject);
  private
    FUser: String;
    FTab : Integer;
  public

  end;

var
  frmMain: TfrmMain;

const
  FTabDashboard = 1;

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

  imgFavorites.SVGNormalXML.Text := imgFavorites.SVGNormalXML.Text.Replace('rgb(0,0,0)', 'rgb(255,255,255)');

end;

procedure TfrmMain.ChangeView(tab: Integer);
var
  i, j : Integer;
  pnl : TPanel;
  img : TBCSVGViewer;
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

