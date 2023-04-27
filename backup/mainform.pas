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

  TFavorite: String;

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

TFavorite = '<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"><svg version="1.2" width="254mm" height="254mm" viewBox="0 0 25400 25400" preserveAspectRatio="xMidYMid" fill-rule="evenodd" stroke-width="28.222" stroke-linejoin="round" xmlns="http://www.w3.org/2000/svg" xmlns:ooo="http://xml.openoffice.org/svg/export" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:presentation="http://sun.com/xmlns/staroffice/presentation" xmlns:smil="http://www.w3.org/2001/SMIL20/" xmlns:anim="urn:oasis:names:tc:opendocument:xmlns:animation:1.0" xml:space="preserve"><defs class="ClipPathGroup"><clipPath id="presentation_clip_path" clipPathUnits="userSpaceOnUse"><rect x="0" y="0" width="25400" height="25400"/></clipPath><clipPath id="presentation_clip_path_shrink" clipPathUnits="userSpaceOnUse"><rect x="25" y="25" width="25350" height="25350"/></clipPath></defs><defs class="TextShapeIndex"><g ooo:slide="id1" ooo:id-list="id3"/></defs><defs class="EmbeddedBulletChars"><g id="bullet-char-template-57356" transform="scale(0.00048828125,-0.00048828125)"><path d="M 580,1141 L 1163,571 580,0 -4,571 580,1141 Z"/></g><g id="bullet-char-template-57354" transform="scale(0.00048828125,-0.00048828125)"><path d="M 8,1128 L 1137,1128 1137,0 8,0 8,1128 Z"/></g><g id="bullet-char-template-10146" transform="scale(0.00048828125,-0.00048828125)"><path d="M 174,0 L 602,739 174,1481 1456,739 174,0 Z M 1358,739 L 309,1346 659,739 1358,739 Z"/></g><g id="bullet-char-template-10132" transform="scale(0.00048828125,-0.00048828125)"><path d="M 2015,739 L 1276,0 717,0 1260,543 174,543 174,936 1260,936 717,1481 1274,1481 2015,739 Z"/></g><g id="bullet-char-template-10007" transform="scale(0.00048828125,-0.00048828125)"><path d="M 0,-2 C -7,14 -16,27 -25,37 L 356,567 C 262,823 215,952 215,954 215,979 228,992 255,992 264,992 276,990 289,987 310,991 331,999 354,1012 L 381,999 492,748 772,1049 836,1024 860,1049 C 881,1039 901,1025 922,1006 886,937 835,863 770,784 769,783 710,716 594,584 L 774,223 C 774,196 753,168 711,139 L 727,119 C 717,90 699,76 672,76 641,76 570,178 457,381 L 164,-76 C 142,-110 111,-127 72,-127 30,-127 9,-110 8,-76 1,-67 -2,-52 -2,-32 -2,-23 -1,-13 0,-2 Z"/></g><g id="bullet-char-template-10004" transform="scale(0.00048828125,-0.00048828125)"><path d="M 285,-33 C 182,-33 111,30 74,156 52,228 41,333 41,471 41,549 55,616 82,672 116,743 169,778 240,778 293,778 328,747 346,684 L 369,508 C 377,444 397,411 428,410 L 1163,1116 C 1174,1127 1196,1133 1229,1133 1271,1133 1292,1118 1292,1087 L 1292,965 C 1292,929 1282,901 1262,881 L 442,47 C 390,-6 338,-33 285,-33 Z"/></g><g id="bullet-char-template-9679" transform="scale(0.00048828125,-0.00048828125)"><path d="M 813,0 C 632,0 489,54 383,161 276,268 223,411 223,592 223,773 276,916 383,1023 489,1130 632,1184 813,1184 992,1184 1136,1130 1245,1023 1353,916 1407,772 1407,592 1407,412 1353,268 1245,161 1136,54 992,0 813,0 Z"/></g><g id="bullet-char-template-8226" transform="scale(0.00048828125,-0.00048828125)"><path d="M 346,457 C 273,457 209,483 155,535 101,586 74,649 74,723 74,796 101,859 155,911 209,963 273,989 346,989 419,989 480,963 531,910 582,859 608,796 608,723 608,648 583,586 532,535 482,483 420,457 346,457 Z"/></g><g id="bullet-char-template-8211" transform="scale(0.00048828125,-0.00048828125)"><path d="M -4,459 L 1135,459 1135,606 -4,606 -4,459 Z"/></g><g id="bullet-char-template-61548" transform="scale(0.00048828125,-0.00048828125)"><path d="M 173,740 C 173,903 231,1043 346,1159 462,1274 601,1332 765,1332 928,1332 1067,1274 1183,1159 1299,1043 1357,903 1357,740 1357,577 1299,437 1183,322 1067,206 928,148 765,148 601,148 462,206 346,322 231,437 173,577 173,740 Z"/></g></defs><g><g id="id2" class="Master_Slide"><g id="bg-id2" class="Background"/><g id="bo-id2" class="BackgroundObjects"/></g></g><g class="SlideGroup"><g><g id="container-id1"><g id="id1" class="Slide" clip-path="url(#presentation_clip_path)"><g class="Page"><g class="com.sun.star.drawing.PolyPolygonShape"><g id="id3"><rect class="BoundingBox" stroke="none" fill="none" x="5080" y="5080" width="15241" height="15241"/><path fill="none" stroke="rgb(0,0,0)" stroke-width="1270" stroke-linejoin="round" stroke-linecap="round" d="M 12697,5715 L 10414,9906 5715,11054 8890,14478 8429,19685 12700,17145 16969,19685 16256,14605 19685,11054 14986,10033 12697,5715 Z"/></g></g></g></g></g></g></g></svg>';


end.

