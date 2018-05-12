(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.controls.banner.txt> }
unit Codebot.Controls.Banner;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls;

type
  TAnchorStash = record
    Anchors: TAnchors;
    Control: TControl;
  end;

  TAnchorStashes = TArrayList<TAnchorStash>;

{ TBannerText is used to render titles and descriptions inside a banner
  See also
  <link Overview.Codebot.Banner.TBannerText, TBannerText members> }

type
  TBannerText = class(TChangeNotifier)
  private
    FParentFont: Boolean;
    FFont: TFont;
    FText: string;
    FX: Integer;
    FY: Integer;
    procedure SetParentFont(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetText(Value: string);
    procedure SetFX(Value: Integer);
    procedure SetFY(Value: Integer);
    procedure FontChange(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ParentFont: Boolean read FParentFont write SetParentFont;
    property Font: TFont read FFont write SetFont;
    property Text: string read FText write SetText;
    property X: Integer read FX write SetFX;
    property Y: Integer read FY write SetFY;
  end;

{ TBannerBackground is used to render the background of a banner
  See also
  <link Overview.Codebot.Controls.Banner.TBannerBackground, TBannerBackground members> }

  TBannerBackground = class(TChangeNotifier)
  private
    FColor: TColor;
    FHeight: Integer;
    FImage: TSurfaceBitmap;
    FMix: TSurfaceBitmap;
    FColorBalance: Float;
    FImageBalance: Float;
    procedure ImageChange(Sender: TObject);
    procedure SetColor(Value: TColor);
    procedure SetColorBalance(Value: Float);
    procedure SetHeight(Value: Integer);
    procedure SetImage(Value: TSurfaceBitmap);
    procedure SetImageBalance(Value: Float);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw(Surface: ISurface);
  published
    property Color: TColor read FColor write SetColor;
    property ColorBalance: Float read FColorBalance write SetColorBalance;
    property Height: Integer read FHeight write SetHeight;
    property Image: TSurfaceBitmap read FImage write SetImage;
    property ImageBalance: Float read FImageBalance write SetImageBalance;
  end;

{ TBanner }

  TBanner = class(TRenderGraphicControl)
  private
    FLogo: TSurfaceBitmap;
    FBackground: TBannerBackground;
    FTitle: TBannerText;
    FTitleSub: TBannerText;
    FShadow: Boolean;
    procedure SetLogo(Value: TSurfaceBitmap);
    procedure SetBackground(Value: TBannerBackground);
    procedure SetTitle(Value: TBannerText);
    procedure SetTitleSub(Value: TBannerText);
    procedure SetShadow(Value: Boolean);
    procedure PartChange(Sender: TObject);
  protected
    procedure Render; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Logo: TSurfaceBitmap read FLogo write SetLogo;
    property Background: TBannerBackground read FBackground write SetBackground;
    property Title: TBannerText read FTitle write SetTitle;
    property TitleSub: TBannerText read FTitleSub write SetTitleSub;
    property Shadow: Boolean read FShadow write SetShadow default True;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property ShowHint;
    property ParentBidiMode;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRender;
    property OnResize;
    property OnChangeBounds;
  end;

{ TBannerFormOptions displays a banner across the top along with images
  See also
  <link Overview.Codebot.Controls.Banner.TBannerFormOptions, TBannerFormOptions members>
  <link Overview.Codebot.Controls.Banner.TBannerForm, TBannerForm class> }

  TBannerFormOptions = set of (
    { Reanchor all controls after reszing to boundary }
    boReanchor,
    { Render a shadow underneath the banner }
    boBannerShadow,
    { Render a shadow underneath at the top of the footer }
    boFooterShadow,
    { Render a size grip in the bottom right corner }
    boFooterGrip
  );

{ TBannerForm displays a banner across the top along with a logo and text
  See also
  <link Overview.Codebot.Controls.Banner.TBannerForm, TBannerForm members> }

  TBannerForm = class(TRenderForm)
  private
    FLogo: TSurfaceBitmap;
    FBanner: TBannerBackground;
    FTitle: TBannerText;
    FBoundary: TControl;
    FTitleSub: TBannerText;
    FOptions: TBannerFormOptions;
    FStash: TAnchorStashes;
    procedure PartChange(Sender: TObject);
    procedure SetLogo(Value: TSurfaceBitmap);
    procedure SetBanner(Value: TBannerBackground);
    procedure SetTitle(Value: TBannerText);
    procedure SetTitleSub(Value: TBannerText);
    procedure SetBoundary(Value: TControl);
    procedure SetOptions(Value: TBannerFormOptions);
  protected
    procedure DoShow; override;
    procedure Loaded; override;
    procedure Render; override;
    function ThemeAware: Boolean; override;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
    property ClientHandle;
    property DockManager;
  published
    property Options: TBannerFormOptions read FOptions write SetOptions;
    property Logo: TSurfaceBitmap read FLogo write SetLogo;
    property Banner: TBannerBackground read FBanner write SetBanner;
    property Title: TBannerText read FTitle write SetTitle;
    property TitleSub: TBannerText read FTitleSub write SetTitleSub;
    property Boundary: TControl read FBoundary write SetBoundary;
    property Action;
    property ActiveControl;
    property Align;
    property AllowDropFiles;
    property AlphaBlend default False;
    property AlphaBlendValue default 255;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property BorderIcons;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DefaultMonitor;
    property DockSite;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FormStyle;
    property HelpFile;
    property Icon;
    property KeyPreview;
    property Menu;
    property OnActivate;
    property OnChangeBounds;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDeactivate;
    property OnDestroy;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDropFiles;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnHelp;
    property OnHide;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnRender;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnShowHint;
    property OnStartDock;
    property OnUnDock;
    property OnUTF8KeyPress;
    property OnWindowStateChange;
    property ParentBiDiMode;
    property ParentFont;
    property PixelsPerInch;
    property PopupMenu;
    property PopupMode;
    property PopupParent;
    property Position;
    property SessionProperties;
    property ShowHint;
    property ShowInTaskBar;
    property ThemeName;
    property UseDockManager;
    property Visible;
    property WindowState;
  end;

implementation

{ TBannerText }

constructor TBannerText.Create;
begin
  inherited Create;
  ParentFont := True;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
end;

destructor TBannerText.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TBannerText.FontChange(Sender: TObject);
begin
  Change;
end;

procedure TBannerText.SetFont(Value: TFont);
begin
  Font.Assign(Value);
  Change;
end;

procedure TBannerText.SetFX(Value: Integer);
begin
  if FX = Value then Exit;
  FX := Value;
  Change;
end;

procedure TBannerText.SetFY(Value: Integer);
begin
  if FY = Value then Exit;
  FY := Value;
  Change;
end;

procedure TBannerText.SetParentFont(Value: Boolean);
begin
  if FParentFont = Value then Exit;
  FParentFont := Value;
  Change;
end;

procedure TBannerText.SetText(Value: string);
begin
  if FText = Value then Exit;
  FText := Value;
  Change;
end;

{ TBannerBackground }

constructor TBannerBackground.Create;
begin
  inherited Create;
  FColor := clNone;
  FColorBalance := 0.5;
  FHeight := 100;
  FImage := TSurfaceBitmap.Create;
  FImage.OnChange := ImageChange;
  FImageBalance := 0.5;
end;

destructor TBannerBackground.Destroy;
begin
  FImage.Free;
  FMix.Free;
  inherited Destroy;
end;

procedure TBannerBackground.Draw(Surface: ISurface);
var
  C: TColorB;
  R: TRectI;
begin
  if FImage.Empty then
  begin
    if FHeight < 1 then
      Exit;
    if FColor <> clNone then
    begin
      C := FColor;
      if FColorBalance > 0.5 then
        C := C.Lighten((FColorBalance - 0.5) * 2)
      else if FColorBalance < 0.5 then
        C := C.Darken((0.5 - FColorBalance) * 2);
      R := TRectI.Create(5000, FHeight);
      FillRectColor(Surface, R, C);
    end;
    if Theme.Selected then
      Theme.DrawHeader(FHeight);
  end
  else
  begin
    if (FColor <> clNone) and (FMix = nil) then
    begin
      FMix := TSurfaceBitmap.Create;
      FMix.Assign(FImage);
      if FImageBalance > 0.5 then
        FMix.Lighten((FImageBalance - 0.5) * 2)
      else if FImageBalance < 0.5 then
        FMix.Darken((FImageBalance - 0.5) * -2);
      C := FColor;
      if FColorBalance > 0.5 then
        C := C.Lighten((FColorBalance - 0.5) * 2)
      else if FColorBalance < 0.5 then
        C := C.Darken((0.5 - FColorBalance) * 2);
      FMix.Colorize(C);
    end;
    if FMix <> nil then
      FMix.Draw(Surface, 0, 0)
    else
      FImage.Draw(Surface, 0, 0);
  end;
end;

procedure TBannerBackground.ImageChange(Sender: TObject);
begin
  FMix.Free;
  FMix := nil;
  Change;
end;

procedure TBannerBackground.SetHeight(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value > 1000 then
    Value := 1000;
  if FHeight = Value then Exit;
  FHeight := Value;
  Change;
end;

procedure TBannerBackground.SetImage(Value: TSurfaceBitmap);
begin
  if FImage = Value then Exit;
  FImage.Assign(Value);
end;

procedure TBannerBackground.SetColor(Value: TColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
  FMix.Free;
  FMix := nil;
  Change;
end;

procedure TBannerBackground.SetColorBalance(Value: Float);
begin
  Value := Clamp(Value);
  if FColorBalance = Value then Exit;
  FColorBalance := Value;
  FMix.Free;
  FMix := nil;
  Change;
end;

procedure TBannerBackground.SetImageBalance(Value: Float);
begin
  Value := Clamp(Value);
  if FImageBalance = Value then Exit;
  FImageBalance := Value;
  FMix.Free;
  FMix := nil;
  Change;
end;

{$R banner_blank.res}

{ TBanner }

constructor TBanner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 256;
  Height := 160;
  FLogo := TSurfaceBitmap.Create;
  FLogo.SetSize(1, 1);
  FLogo.OnChange := PartChange;
  FBackground := TBannerBackground.Create;
  FBackground.OnChange.Add(PartChange);
  FTitle := TBannerText.Create;
  FTitle.Text := 'Your title here';
  FTitle.ParentFont := False;
  FTitle.Font.Size := 20;
  FTitle.Font.Style := [fsBold];
  FTitle.OnChange.Add(PartChange);
  FTitleSub := TBannerText.Create;
  FTitleSub.Text := 'Your description here';
  FTitleSub.ParentFont := True;
  FTitleSub.OnChange.Add(PartChange);
  FShadow := True;
end;

destructor TBanner.Destroy;
begin
  FLogo.Free;
  inherited Destroy;
end;

procedure TBanner.Loaded;
begin
  inherited Loaded;
  if (FLogo.Width = 1) and (FLogo.Height = 1) then
    FLogo.Clear;
end;

procedure TBanner.PartChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TBanner.Render;
const
  Margin = 8;
var
  Pen: IPen;
  F: IFont;
  R: TRectI;
  S: string;
  I: Integer;
begin
  if (FLogo.Width = 1) and (FLogo.Height = 1) then
    FLogo.LoadFromResourceName(HINSTANCE, 'banner_blank');
  FBackground.Draw(Surface);
  if not FLogo.Empty then
    FLogo.Draw(Surface, 8, 8);
  if FShadow then
  begin
    I := FBackground.Height;
    if not FBackground.Image.Empty then
      I := FBackground.Image.Height;
    Theme.DrawHeaderShadow(I);
  end;
  R := ClientRect;
  R.Inflate(-Margin, -Margin);
  R.Bottom := FBackground.Height;
  R.Left := R.Left + FLogo.Width + Margin;
  S := Title.Text.Trim;
  if S <> '' then
  begin
    if Title.ParentFont then
      F := NewFont(Font)
    else
      F := NewFont(Title.Font);
    F.Color := Title.Font.Color;
    R.Offset(Title.X, Title.Y);
    Surface.TextOut(F, S, R, drWrap);
    R.Top := R.Top + Round(Surface.TextHeight(F, S, R.Width));
  end;
  S := TitleSub.Text.Trim;
  if S <> '' then
  begin
    if TitleSub.ParentFont then
      F := NewFont(Font)
    else
      F := NewFont(TitleSub.Font);
    F.Color := TitleSub.Font.Color;
    R.Offset(TitleSub.X, TitleSub.Y);
    Surface.TextOut(F, S, R, drWrap);
  end;
  if csDesigning in ComponentState then
  begin
    Pen := NewPen(clBlack);
    Pen.LinePattern := pnDash;
    Surface.StrokeRect(Pen, ClientRect);
  end;
  inherited Render;
end;

procedure TBanner.SetBackground(Value: TBannerBackground);
begin
  if FBackground = Value then Exit;
  FBackground := Value;
end;

procedure TBanner.SetLogo(Value: TSurfaceBitmap);
begin
  if FLogo = Value then Exit;
  FLogo.Assign(Value);
end;

procedure TBanner.SetTitle(Value: TBannerText);
begin
  if FTitle = Value then Exit;
  FTitle.Assign(Value);
end;

procedure TBanner.SetTitleSub(Value: TBannerText);
begin
  if FTitleSub = Value then Exit;
  FTitleSub.Assign(Value);
end;

procedure TBanner.SetShadow(Value: Boolean);
begin
  if FShadow = Value then Exit;
  FShadow := Value;
  Invalidate;
end;

{ TBannerForm }

constructor TBannerForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);
  Position := poDesktopCenter;
  FLogo := TSurfaceBitmap.Create;
  FLogo.SetSize(1, 1);
  FLogo.OnChange := PartChange;
  FBanner := TBannerBackground.Create;
  FBanner.OnChange.Add(PartChange);
  FTitle := TBannerText.Create;
  FTitle.Text := 'Your title here';
  FTitle.ParentFont := False;
  FTitle.Font.Size := 20;
  FTitle.Font.Style := [fsBold];
  FTitle.OnChange.Add(PartChange);
  FTitleSub := TBannerText.Create;
  FTitleSub.Text := 'Your description here';
  FTitleSub.ParentFont := True;
  FTitleSub.OnChange.Add(PartChange);
  FOptions := [boReanchor, boBannerShadow, boFooterShadow, boFooterGrip];
end;

destructor TBannerForm.Destroy;
begin
  Boundary := nil;
  FLogo.Free;
  FBanner.Free;
  FTitle.Free;
  FTitleSub.Free;
  inherited Destroy;
end;

procedure TBannerForm.DoShow;
var
  Stash: TAnchorStash;
begin
  inherited DoShow;
  if csDesigning in ComponentState then
    Exit;
  if FStash.Length > 0 then
  begin
    for Stash in FStash do
      Stash.Control.Anchors := Stash.Anchors;
    FStash.Clear;
    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;
  end;
end;

procedure TBannerForm.Loaded;
const
  Margin = 8;
var
  Boundary: TPointI;
  Control: TControl;
  Stash: TAnchorStash;
  I: Integer;
begin
  inherited Loaded;
  if (FLogo.Width = 1) and (FLogo.Height = 1) then
    FLogo.Clear;
  if csDesigning in ComponentState then
    Exit;
  if (ControlCount < 1) or ([boReanchor] * Options = []) then
    Exit;
  Boundary := TPointI.Create(0, 0);
  FStash.Length := ControlCount;
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    Stash.Control := Control;
    Stash.Anchors := Control.Anchors;
    FStash[I] := Stash;
    Control.Anchors := [akLeft, akTop];
    if Control.Left + Control.Width > Boundary.X then
      Boundary.X := Control.Left + Control.Width;
    if Control.Top + Control.Height > Boundary.Y then
      Boundary.Y := Control.Top + Control.Height;
  end;
  ClientWidth := Boundary.X + Margin;
  ClientHeight := Boundary.Y + Margin;
end;

procedure TBannerForm.Render;
const
  Margin = 8;
var
  F: IFont;
  R: TRectI;
  S: string;
  I: Integer;
begin
  if (FLogo.Width = 1) and (FLogo.Height = 1) then
    FLogo.LoadFromResourceName(HINSTANCE, 'banner_blank');
  FBanner.Draw(Surface);
  if not FLogo.Empty then
    FLogo.Draw(Surface, 8, 8);
  if boBannerShadow in Options then
  begin
    I := FBanner.Height;
    if not FBanner.Image.Empty then
      I := FBanner.Image.Height;
    Theme.DrawHeaderShadow(I);
  end;
  R := ClientRect;
  R.Inflate(-Margin, -Margin);
  R.Bottom := FBanner.Height;
  R.Left := R.Left + FLogo.Width + Margin;
  S := Title.Text.Trim;
  if S <> '' then
  begin
    if Title.ParentFont then
      F := NewFont(Font)
    else
      F := NewFont(Title.Font);
    F.Color := Title.Font.Color;
    R.Left := R.Left + Title.X;
    R.Top := R.Top + Title.Y;
    Surface.TextOut(F, S, R, drWrap);
    R.Top := R.Top + Round(Surface.TextHeight(F, S, R.Width));
  end;
  S := TitleSub.Text.Trim;
  if S <> '' then
  begin
    if TitleSub.ParentFont then
      F := NewFont(Font)
    else
      F := NewFont(TitleSub.Font);
    F.Color := TitleSub.Font.Color;
    R.Left := R.Left + TitleSub.X;
    R.Top := R.Top + TitleSub.Y;
    Surface.TextOut(F, S, R, drWrap);
  end;
  if boFooterShadow in Options then
    Theme.DrawFooter;
  if boFooterGrip in Options then
    Theme.DrawFooterGrip;
  inherited Render;
end;

function TBannerForm.ThemeAware: Boolean;
begin
  Result := True;
end;

procedure TBannerForm.PartChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TBannerForm.SetOptions(Value: TBannerFormOptions);
begin
  if FOptions = Value then Exit;
  FOptions := Value;
  Invalidate;
end;

procedure TBannerForm.SetBoundary(Value: TControl);
begin
  if FBoundary = Value then Exit;
  FBoundary := Value;
end;

procedure TBannerForm.SetBanner(Value: TBannerBackground);
begin
  if FBanner = Value then Exit;
  FBanner.Assign(Value);
end;

procedure TBannerForm.SetLogo(Value: TSurfaceBitmap);
begin
  if FLogo = Value then Exit;
  FLogo.Assign(Value);
end;

procedure TBannerForm.SetTitle(Value: TBannerText);
begin
  if FTitle = Value then Exit;
  FTitle.Assign(Value);
end;

procedure TBannerForm.SetTitleSub(Value: TBannerText);
begin
  if FTitleSub = Value then Exit;
  FTitleSub.Assign(Value);
end;

end.

