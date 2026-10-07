(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified October 2026                               *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.webkit.controls.extras.txt> }
unit Codebot.WebKit.Controls.Extras;

{$i webkit.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, Buttons, Menus, LCLType,
  Codebot.Controls.Containers,
  Codebot.WebKit.Controls;

{ TWebAddressBar is a bar for navigating a browser.

  The bar has an edit showing the location of the browser. Pressing enter in
  the edit loads what was typed, and pressing escape restores the location.

  Buttons selects the buttons shown on the bar. The navigation buttons are
  shown to the left of the edit and the tool buttons to the right of it.
  OnButtonClick is invoked when any button is clicked, before the bar acts on
  the click. }

type
  TWebAddressButton = (
    { Navigate back one step in history }
    abBack,
    { Navigate forward one step in history }
    abForward,
    { Load the home page }
    abHome,
    { Reload the page, or stop the load while a page is loading }
    abRefresh,
    { Show a menu of the pages back and forward in history }
    abHistory,
    { Show or hide the inspector }
    abInspect,
    { Show the print dialog }
    abPrint,
    { Capture an image of the page to the clipboard }
    abCapture,
    { Show a menu of the files downloaded by the browser }
    abDownloads,
    { Settings does nothing by itself and is meant to be handled in OnButtonClick }
    abSettings);
  TWebAddressButtons = set of TWebAddressButton;

const
  DefaultAddressButtons = [abBack, abForward, abRefresh];

{ TWebAddressButtonEvent is invoked when a button on an address bar is clicked.
  Set Handled to True to prevent the default action of the button. }

type
  TWebAddressButtonEvent = procedure(Sender: TObject; Button: TWebAddressButton;
    var Handled: Boolean) of object;

  TWebAddressBar = class(TCustomControl)
  private
    FWebBrowser: TCustomWebBrowser;
    FButtons: TWebAddressButtons;
    FItems: array[TWebAddressButton] of TSpeedButton;
    FEdit: TEdit;
    FMenu: TPopupMenu;
    FArranging: Boolean;
    FHomePage: string;
    FOnButtonClick: TWebAddressButtonEvent;
    procedure Arrange;
    procedure BrowserNotify(Sender: TObject; Notify: TWebNotify);
    procedure ItemClick(Sender: TObject);
    procedure ToggleInspector;
    function AddMenuItem(const Caption: string; Tag: Integer; OnClick: TNotifyEvent): TMenuItem;
    procedure ShowMenu(Button: TWebAddressButton);
    procedure ShowHistory;
    procedure ShowDownloads;
    procedure HistoryClick(Sender: TObject);
    procedure DownloadClick(Sender: TObject);
    procedure DownloadFolderClick(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpdateState;
    procedure SetButtons(Value: TWebAddressButtons);
    procedure SetWebBrowser(Value: TCustomWebBrowser);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Click a button as if the user had clicked it }
    procedure ButtonClick(Button: TWebAddressButton);
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    { The buttons shown on the bar }
    property Buttons: TWebAddressButtons read FButtons write SetButtons default DefaultAddressButtons;
    property Constraints;
    property Enabled;
    property Font;
    { The location loaded by the home button }
    property HomePage: string read FHomePage write FHomePage;
    property ParentFont;
    property Visible;
    { The browser to navigate }
    property WebBrowser: TCustomWebBrowser read FWebBrowser write SetWebBrowser;
    { OnButtonClick is invoked when any button is clicked }
    property OnButtonClick: TWebAddressButtonEvent read FOnButtonClick write FOnButtonClick;
  end;

{ TWebStatusIndicator shows the status of a browser.

  The indicator shows the uri of the link under the mouse, or the location
  being loaded when there is no link, along with the progress of the load. }

  TWebStatusIndicator = class(TGraphicControl)
  private
    FWebBrowser: TCustomWebBrowser;
    procedure BrowserNotify(Sender: TObject; Notify: TWebNotify);
    function GetLink: string;
    function GetProgress: Integer;
    procedure SetWebBrowser(Value: TCustomWebBrowser);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { The uri of the link under the mouse }
    property Link: string read GetLink;
    { The progress of the current load ranging from 0 to 100 }
    property Progress: Integer read GetProgress;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Font;
    property ParentColor;
    property ParentFont;
    property Visible;
    { The browser to show the status of }
    property WebBrowser: TCustomWebBrowser read FWebBrowser write SetWebBrowser;
  end;

implementation

uses
  LCLIntf;

{ The button captions are icons from the Material Design Icons font, which
  is in the fonts folder of the library and needs to be installed. The icons
  are encoded as utf8 and are in order arrow-left $F004D, arrow-right $F0054,
  home $F02DC, refresh $F0450, history $F02DA, code-tags $F0174,
  printer $F042A, camera $F0100, download $F01DA, and cog $F0493. The stop
  icon is close $F0156. }

const
  GlyphFont = 'Material Design Icons';
  Glyphs: array[TWebAddressButton] of string = (
    #$F3#$B0#$81#$8D, #$F3#$B0#$81#$94, #$F3#$B0#$8B#$9C, #$F3#$B0#$91#$90,
    #$F3#$B0#$8B#$9A, #$F3#$B0#$85#$B4, #$F3#$B0#$90#$AA, #$F3#$B0#$84#$80,
    #$F3#$B0#$87#$9A, #$F3#$B0#$92#$93);
  GlyphStop = #$F3#$B0#$85#$96;
  Hints: array[TWebAddressButton] of string = (
    'Go back', 'Go forward', 'Home', 'Reload', 'History', 'Inspect', 'Print',
    'Copy an image of the page', 'Downloads', 'Settings');
  HintStop = 'Stop';
  { The buttons before FirstToolButton are shown to the left of the edit }
  FirstToolButton = abInspect;
  EditMargin = 3;
  { The most pages shown in each direction in the history menu }
  MenuLimit = 15;
  { The most completed downloads shown in the downloads menu }
  RecentLimit = 10;

{ TWebAddressBar }

constructor TWebAddressBar.Create(AOwner: TComponent);
var
  B: TWebAddressButton;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  Width := 400;
  Height := 32;
  FButtons := DefaultAddressButtons;
  FHomePage := 'about:blank';
  for B := Low(FItems) to High(FItems) do
  begin
    FItems[B] := TSpeedButton.Create(Self);
    { A control which is not visible is still shown at design time unless it
      has the csNoDesignVisible style }
    FItems[B].ControlStyle := FItems[B].ControlStyle + [csNoDesignSelectable,
      csNoDesignVisible];
    FItems[B].Flat := True;
    FItems[B].Font.Name := GlyphFont;
    FItems[B].Caption := Glyphs[B];
    FItems[B].Hint := Hints[B];
    FItems[B].ShowHint := True;
    FItems[B].Tag := Ord(B);
    FItems[B].Visible := B in FButtons;
    FItems[B].OnClick := ItemClick;
    FItems[B].Parent := Self;
  end;
  FEdit := TEdit.Create(Self);
  FEdit.ControlStyle := FEdit.ControlStyle + [csNoDesignSelectable];
  FEdit.Parent := Self;
  FEdit.OnKeyDown := EditKeyDown;
  FMenu := TPopupMenu.Create(Self);
  Arrange;
  UpdateState;
end;

destructor TWebAddressBar.Destroy;
begin
  WebBrowser := nil;
  inherited Destroy;
end;

procedure TWebAddressBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FWebBrowser) then
  begin
    FWebBrowser := nil;
    if not (csDestroying in ComponentState) then
      UpdateState;
  end;
end;

procedure TWebAddressBar.Resize;
begin
  inherited Resize;
  if FEdit <> nil then
    Arrange;
end;

{ The buttons are square with icons sized to fit them. The navigation buttons
  are placed from the left, the tool buttons are placed up to the right, and
  the edit fills the space between them.

  An edit sizes its own height to fit its font, so only its width is set and
  it is centered vertically. Setting its height would be undone by the edit,
  causing the bar to arrange again without end. }

procedure TWebAddressBar.Arrange;
var
  B: TWebAddressButton;
  Size, L, R, X: Integer;
begin
  if FArranging then
    Exit;
  FArranging := True;
  try
    Size := ClientHeight;
    for B := Low(FItems) to High(FItems) do
      FItems[B].Font.Height := -(Size * 5 div 8);
    L := 0;
    for B := Low(FItems) to Pred(FirstToolButton) do
      if B in FButtons then
      begin
        FItems[B].SetBounds(L, 0, Size, Size);
        Inc(L, Size);
      end;
    R := ClientWidth;
    for B := FirstToolButton to High(FItems) do
      if B in FButtons then
        Dec(R, Size);
    X := R;
    for B := FirstToolButton to High(FItems) do
      if B in FButtons then
      begin
        FItems[B].SetBounds(X, 0, Size, Size);
        Inc(X, Size);
      end;
    FEdit.SetBounds(L + EditMargin, (Size - FEdit.Height) div 2,
      R - L - EditMargin * 2, FEdit.Height);
  finally
    FArranging := False;
  end;
end;

procedure TWebAddressBar.BrowserNotify(Sender: TObject; Notify: TWebNotify);
begin
  if Notify in [wnReady, wnLocation, wnLoadStatus] then
  begin
    { The location is not replaced while the user is typing }
    if not FEdit.Focused then
      FEdit.Text := FWebBrowser.Location;
    UpdateState;
  end;
end;

procedure TWebAddressBar.ItemClick(Sender: TObject);
begin
  ButtonClick(TWebAddressButton(TSpeedButton(Sender).Tag));
end;

{ OnButtonClick is invoked for every button, including those with no default
  action such as settings }

procedure TWebAddressBar.ButtonClick(Button: TWebAddressButton);
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, Button, Handled);
  if Handled or (FWebBrowser = nil) then
    Exit;
  case Button of
    abBack: FWebBrowser.GoBack;
    abForward: FWebBrowser.GoForward;
    abHome: FWebBrowser.Load(FHomePage);
    abRefresh:
      if FWebBrowser.Loading then
        FWebBrowser.Stop
      else
        FWebBrowser.Reload;
    abHistory: ShowHistory;
    abInspect: ToggleInspector;
    abPrint: FWebBrowser.Print;
    abCapture: FWebBrowser.CaptureToClipboard;
    abDownloads: ShowDownloads;
    abSettings: ;
  end;
end;

{ When the browser has an inspector control it is switched on and shown, or
  switched off and hidden. An inspector control placed in a caption box is
  shown or hidden along with its caption box. Without an inspector control
  the inspector is shown in a window of its own. }

procedure TWebAddressBar.ToggleInspector;
var
  Inspector: TWebInspector;
  Box: TControl;
begin
  Inspector := FWebBrowser.Inspector;
  if Inspector = nil then
  begin
    FWebBrowser.ShowInspector;
    Exit;
  end;
  if Inspector.Parent is TCaptionBox then
    Box := Inspector.Parent
  else
    Box := Inspector;
  if Inspector.Active and Inspector.Visible and Box.Visible then
  begin
    Inspector.Active := False;
    Box.Visible := False;
  end
  else
  begin
    Box.Visible := True;
    Inspector.Visible := True;
    Inspector.Active := True;
  end;
end;

{ An ampersand in a menu caption marks a shortcut, so it is doubled to show
  it as written }

function TWebAddressBar.AddMenuItem(const Caption: string; Tag: Integer;
  OnClick: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(FMenu);
  if Caption = '-' then
    Result.Caption := Caption
  else
    Result.Caption := StringReplace(Caption, '&', '&&', [rfReplaceAll]);
  Result.Tag := Tag;
  Result.OnClick := OnClick;
  Result.Enabled := Assigned(OnClick);
  FMenu.Items.Add(Result);
end;

{ The menu is shown below its button }

procedure TWebAddressBar.ShowMenu(Button: TWebAddressButton);
var
  P: TPoint;
begin
  P := FItems[Button].ClientToScreen(Point(0, FItems[Button].Height));
  FMenu.PopUp(P.X, P.Y);
end;

{ The history menu lists the pages forward in history, the current page, and
  then the pages back in history. The tag of each item is the number of steps
  to the page. }

procedure TWebAddressBar.ShowHistory;
var
  Title, Uri: string;
  Item: TMenuItem;
  I: Integer;
begin
  FMenu.Items.Clear;
  for I := MenuLimit downto -MenuLimit do
  begin
    if not FWebBrowser.HistoryItem(I, Title, Uri) then
      Continue;
    if Title = '' then
      Title := Uri;
    if I = 0 then
    begin
      Item := AddMenuItem(Title, I, nil);
      Item.Checked := True;
    end
    else
      AddMenuItem(Title, I, HistoryClick);
  end;
  if FMenu.Items.Count = 0 then
    AddMenuItem('No history', 0, nil);
  ShowMenu(abHistory);
end;

procedure TWebAddressBar.HistoryClick(Sender: TObject);
begin
  if FWebBrowser <> nil then
    FWebBrowser.BackOrForward(TMenuItem(Sender).Tag);
end;

{ The downloads menu lists every download in progress with its progress,
  followed by the downloads completed most recently, with the newest first in
  both. The tag of each item is the index of the download. Clicking a
  completed download opens the file. The item at the bottom opens the download
  folder to show all downloads. }

procedure TWebAddressBar.ShowDownloads;
var
  Download: TWebDownload;
  S: string;
  I, Count: Integer;
begin
  FMenu.Items.Clear;
  for I := FWebBrowser.DownloadCount - 1 downto 0 do
  begin
    Download := FWebBrowser.Downloads[I];
    if Download.Status = dsActive then
      AddMenuItem(ExtractFileName(Download.FileName) + ' - ' +
        IntToStr(Download.Progress) + '%', I, nil);
  end;
  if FMenu.Items.Count > 0 then
    AddMenuItem('-', 0, nil);
  Count := 0;
  for I := FWebBrowser.DownloadCount - 1 downto 0 do
  begin
    if Count = RecentLimit then
      Break;
    Download := FWebBrowser.Downloads[I];
    S := ExtractFileName(Download.FileName);
    case Download.Status of
      dsFinished: AddMenuItem(S, I, DownloadClick);
      dsFailed: AddMenuItem(S + ' - failed', I, nil);
    else
      Continue;
    end;
    Inc(Count);
  end;
  if FWebBrowser.DownloadCount = 0 then
    AddMenuItem('No downloads', 0, nil);
  AddMenuItem('-', 0, nil);
  AddMenuItem('Show all downloads', 0, DownloadFolderClick);
  ShowMenu(abDownloads);
end;

procedure TWebAddressBar.DownloadClick(Sender: TObject);
var
  I: Integer;
begin
  if FWebBrowser = nil then
    Exit;
  I := TMenuItem(Sender).Tag;
  if (I > -1) and (I < FWebBrowser.DownloadCount) then
    OpenDocument(FWebBrowser.Downloads[I].FileName);
end;

procedure TWebAddressBar.DownloadFolderClick(Sender: TObject);
begin
  if FWebBrowser <> nil then
    OpenDocument(FWebBrowser.ActiveDownloadFolder);
end;

procedure TWebAddressBar.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FWebBrowser = nil then
    Exit;
  case Key of
    VK_RETURN:
      begin
        Key := 0;
        FWebBrowser.Load(FEdit.Text);
        FEdit.Text := FWebBrowser.Location;
        if FWebBrowser.CanFocus then
          FWebBrowser.SetFocus;
      end;
    VK_ESCAPE:
      begin
        Key := 0;
        FEdit.Text := FWebBrowser.Location;
        FEdit.SelectAll;
      end;
  end;
end;

{ The settings button is always enabled as it does not act on the browser }

procedure TWebAddressBar.UpdateState;
var
  B: TWebAddressButton;
begin
  for B := Low(FItems) to High(FItems) do
    FItems[B].Enabled := FWebBrowser <> nil;
  FItems[abSettings].Enabled := True;
  if FWebBrowser = nil then
    Exit;
  FItems[abBack].Enabled := FWebBrowser.CanGoBack;
  FItems[abForward].Enabled := FWebBrowser.CanGoForward;
  if FWebBrowser.Loading then
  begin
    FItems[abRefresh].Caption := GlyphStop;
    FItems[abRefresh].Hint := HintStop;
  end
  else
  begin
    FItems[abRefresh].Caption := Glyphs[abRefresh];
    FItems[abRefresh].Hint := Hints[abRefresh];
  end;
end;

procedure TWebAddressBar.SetButtons(Value: TWebAddressButtons);
var
  B: TWebAddressButton;
begin
  if Value = FButtons then
    Exit;
  FButtons := Value;
  for B := Low(FItems) to High(FItems) do
    FItems[B].Visible := B in FButtons;
  Arrange;
end;

procedure TWebAddressBar.SetWebBrowser(Value: TCustomWebBrowser);
begin
  if Value = FWebBrowser then
    Exit;
  if FWebBrowser <> nil then
  begin
    FWebBrowser.RemoveListener(BrowserNotify);
    FWebBrowser.RemoveFreeNotification(Self);
  end;
  FWebBrowser := Value;
  if FWebBrowser <> nil then
  begin
    FWebBrowser.FreeNotification(Self);
    FWebBrowser.AddListener(BrowserNotify);
    FEdit.Text := FWebBrowser.Location;
  end
  else if not (csDestroying in ComponentState) then
    FEdit.Text := '';
  if not (csDestroying in ComponentState) then
    UpdateState;
end;

{ TWebStatusIndicator }

constructor TWebStatusIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  Width := 400;
  Height := 22;
end;

destructor TWebStatusIndicator.Destroy;
begin
  WebBrowser := nil;
  inherited Destroy;
end;

procedure TWebStatusIndicator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FWebBrowser) then
  begin
    FWebBrowser := nil;
    if not (csDestroying in ComponentState) then
      Invalidate;
  end;
end;

procedure TWebStatusIndicator.BrowserNotify(Sender: TObject; Notify: TWebNotify);
begin
  if Notify <> wnTitle then
    Invalidate;
end;

procedure TWebStatusIndicator.Paint;
const
  Margin = 4;
  BarHeight = 2;
var
  Style: TTextStyle;
  Loading: Boolean;
  R: TRect;
  S: string;
begin
  R := ClientRect;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);
  S := '';
  Loading := False;
  if FWebBrowser <> nil then
  begin
    Loading := FWebBrowser.Loading;
    S := FWebBrowser.HoverLink;
    if (S = '') and Loading then
      S := 'Loading ' + FWebBrowser.Location;
  end
  else if csDesigning in ComponentState then
    S := Name;
  if Loading then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.FillRect(Rect(R.Left, R.Bottom - BarHeight,
      R.Left + (R.Right - R.Left) * FWebBrowser.Progress div 100, R.Bottom));
  end;
  if S = '' then
    Exit;
  Canvas.Font := Font;
  Canvas.Brush.Style := bsClear;
  Style := Canvas.TextStyle;
  Style.SingleLine := True;
  Style.EndEllipsis := True;
  Style.Layout := tlCenter;
  Style.Clipping := True;
  Inc(R.Left, Margin);
  Dec(R.Right, Margin);
  Canvas.TextRect(R, R.Left, R.Top, S, Style);
end;

function TWebStatusIndicator.GetLink: string;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.HoverLink
  else
    Result := '';
end;

function TWebStatusIndicator.GetProgress: Integer;
begin
  if FWebBrowser <> nil then
    Result := FWebBrowser.Progress
  else
    Result := 0;
end;

procedure TWebStatusIndicator.SetWebBrowser(Value: TCustomWebBrowser);
begin
  if Value = FWebBrowser then
    Exit;
  if FWebBrowser <> nil then
  begin
    FWebBrowser.RemoveListener(BrowserNotify);
    FWebBrowser.RemoveFreeNotification(Self);
  end;
  FWebBrowser := Value;
  if FWebBrowser <> nil then
  begin
    FWebBrowser.FreeNotification(Self);
    FWebBrowser.AddListener(BrowserNotify);
  end;
  if not (csDestroying in ComponentState) then
    Invalidate;
end;

end.
