unit Codebot.Render.Scenes.Widgets;

{$i render.inc}

interface

uses
  Codebot.System,
  Codebot.Graphics.Types,
  Codebot.Render.Graphics,
  Codebot.Render.Scenes,
  Codebot.Render.Widgets,
  Codebot.Render.Widgets.Themes,
  Codebot.Render.Widgets.Dialogs;

{ TWidgetScene provides UI widgets through the Widget property. Input reaches
  the widgets first and is passed on to the scene when no widget handles it.
  Call WidgetsRender from Render to draw the widgets. In most cases it makes
  sense to have only one widget scene in your application.

  The main widget of the first widget scene becomes WidgetDialogHost, where
  the file and picture dialogs made from widgets are shown. }

type
  TWidgetScene = class(TScene)
  private
    FWidget: TMainWidget;
    FDefaultTheme: TTheme;
    FWidgetMatrix: IMatrix;
    procedure MouseMatrix(var Args: TSceneMouseArgs);
    procedure SetWidgetMatrix(Value: IMatrix);
    function GetWidget: TMainWidget;
  protected
    { Render all widgets using the current widget transform matrix. The
      widgets are drawn in their own canvas frame, so do not call this
      between your own canvas frame calls. }
    procedure WidgetsRender;
    { Override this method to set your preferred default theme }
    function DefaultTheme: TTheme; virtual;
  public
    procedure Initialize; override;
    procedure Finalize; override;
    procedure DoKeyDown(var Args: TSceneKeyArgs); override;
    procedure DoKeyUp(var Args: TSceneKeyArgs); override;
    procedure DoTextInput(var Args: TSceneTextArgs); override;
    procedure DoMouseDown(var Args: TSceneMouseArgs); override;
    procedure DoMouseMove(var Args: TSceneMouseArgs); override;
    procedure DoMouseUp(var Args: TSceneMouseArgs); override;
    procedure DoMouseWheel(var Args: TSceneWheelArgs); override;
    { The main widget for the scene }
    property Widget: TMainWidget read GetWidget;
    { Widget matrix can be used to transform all widgets }
    property WidgetMatrix: IMatrix read FWidgetMatrix write SetWidgetMatrix;
  end;

{ TSimpleWidgetScene is a widget scene which needs no Render method. It
  clears the scene and draws the widgets each frame, and the first time it
  renders it places the widgets added to Widget in the middle of the scene.
  They are placed once, so a window can still be dragged from there. Widgets
  with a Sector are left where their sector puts them.

  CloseEvent can be given to the OnClick of a button or the OnClose of a
  window to end the program. }

  TSimpleWidgetScene = class(TWidgetScene)
  private
    FCentered: Boolean;
  protected
    { Place the widgets added to Widget in the middle of the scene }
    procedure CenterWidgets;
  public
    procedure Render; override;
    { An event handler which closes the window of the host }
    procedure CloseHandler(Sender: TObject);
  end;

implementation

{ TWidgetScene }

procedure TWidgetScene.Initialize;
begin
  inherited Initialize;
  FWidgetMatrix := NewMatrix;
end;

procedure TWidgetScene.Finalize;
begin
  if (FWidget <> nil) and (WidgetDialogHost = FWidget) then
    WidgetDialogHost := nil;
  FWidget.Free;
  FWidget := nil;
  FDefaultTheme.Free;
  FDefaultTheme := nil;
  FWidgetMatrix := nil;
  inherited Finalize;
end;

procedure TWidgetScene.MouseMatrix(var Args: TSceneMouseArgs);
var
  P: TPointF;
begin
  P.X := Args.X;
  P.Y := Args.Y;
  P := FWidgetMatrix.Inverse.Multiply(P);
  Args.X := P.X;
  Args.Y := P.Y;
end;

procedure TWidgetScene.SetWidgetMatrix(Value: IMatrix);
begin
  FWidgetMatrix.Copy(Value);
end;

procedure TWidgetScene.WidgetsRender;
var
  Buffer: IBackBuffer;
begin
  if (Canvas = nil) or (FWidget = nil) then
    Exit;
  Buffer := Canvas as IBackBuffer;
  Buffer.Flip(Width, Height);
  try
    Canvas.Matrix := FWidgetMatrix;
    FWidget.Render(Width, Height, Time);
  finally
    Buffer.Flip(Width, Height);
  end;
end;

procedure TWidgetScene.DoKeyDown(var Args: TSceneKeyArgs);
begin
  if FWidget <> nil then
    FWidget.DispatchKeyDown(Args);
  if not Args.Handled then
    inherited DoKeyDown(Args);
end;

procedure TWidgetScene.DoKeyUp(var Args: TSceneKeyArgs);
begin
  if FWidget <> nil then
    FWidget.DispatchKeyUp(Args);
  if not Args.Handled then
    inherited DoKeyUp(Args);
end;

procedure TWidgetScene.DoTextInput(var Args: TSceneTextArgs);
begin
  if FWidget <> nil then
    FWidget.DispatchTextInput(Args);
  if not Args.Handled then
    inherited DoTextInput(Args);
end;

procedure TWidgetScene.DoMouseDown(var Args: TSceneMouseArgs);
var
  A: TSceneMouseArgs;
begin
  A := Args;
  MouseMatrix(A);
  if FWidget <> nil then
    FWidget.DispatchMouseDown(A);
  Args.Handled := A.Handled;
  if not Args.Handled then
    inherited DoMouseDown(Args);
end;

procedure TWidgetScene.DoMouseMove(var Args: TSceneMouseArgs);
var
  A: TSceneMouseArgs;
begin
  A := Args;
  MouseMatrix(A);
  if FWidget <> nil then
    FWidget.DispatchMouseMove(A);
  Args.Handled := A.Handled;
  if not Args.Handled then
    inherited DoMouseMove(Args);
end;

procedure TWidgetScene.DoMouseUp(var Args: TSceneMouseArgs);
var
  A: TSceneMouseArgs;
begin
  A := Args;
  MouseMatrix(A);
  if FWidget <> nil then
    FWidget.DispatchMouseUp(A);
  Args.Handled := A.Handled;
  if not Args.Handled then
    inherited DoMouseUp(Args);
end;

procedure TWidgetScene.DoMouseWheel(var Args: TSceneWheelArgs);
var
  A: TSceneWheelArgs;
  P: TPointF;
begin
  A := Args;
  P.X := A.X;
  P.Y := A.Y;
  P := FWidgetMatrix.Inverse.Multiply(P);
  A.X := P.X;
  A.Y := P.Y;
  if FWidget <> nil then
    FWidget.DispatchMouseWheel(A);
  Args.Handled := A.Handled;
  if not Args.Handled then
    inherited DoMouseWheel(Args);
end;

function TWidgetScene.DefaultTheme: TTheme;
begin
  if FDefaultTheme = nil then
    FDefaultTheme := NewTheme(Canvas, TArcDarkTheme);
  Result := FDefaultTheme;
end;

function TWidgetScene.GetWidget: TMainWidget;
begin
  if FWidget = nil then
  begin
    FWidget := TMainWidget.Create(DefaultTheme);
    if WidgetDialogHost = nil then
      WidgetDialogHost := FWidget;
  end;
  Result := FWidget;
end;

{ TSimpleWidgetScene }

procedure TSimpleWidgetScene.CenterWidgets;
var
  W: TWidget;
  I: Integer;
begin
  for I := 0 to Widget.ChildCount - 1 do
  begin
    W := Widget.Child[I];
    if W.Sector <> 0 then
      Continue;
    { Pack sizes the widget to fit what it holds }
    W.Pack;
    W.X := Round((Width - W.Width) / 2);
    W.Y := Round((Height - W.Height) / 2);
  end;
end;

procedure TSimpleWidgetScene.Render;
begin
  inherited Render;
  if not FCentered then
  begin
    FCentered := True;
    CenterWidgets;
  end;
  WidgetsRender;
end;

procedure TSimpleWidgetScene.CloseHandler(Sender: TObject);
begin
  if Host.Window <> nil then
    Host.Window.Close;
end;

end.
