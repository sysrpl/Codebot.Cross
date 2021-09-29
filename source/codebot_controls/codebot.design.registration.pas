(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.design.registration.txt> }
unit Codebot.Design.Registration;

{$i codebot.inc}

interface

uses
  Classes, PropEdits, ComponentEditors,
  Codebot.Design.Editors,
  Codebot.Design.Forms,
  Codebot.Graphics,
  Codebot.Animation,
  Codebot.Controls,
  Codebot.Controls.Extras,
  Codebot.Controls.Grids,
  Codebot.Controls.Banner,
  Codebot.Controls.Buttons,
  Codebot.Controls.Containers,
  Codebot.Controls.Colors,
  Codebot.Controls.Scrolling,
  Codebot.Controls.Sliders,
  Codebot.Text.Store;

procedure Register;

implementation

{$R palette_icons.res}

procedure Register;
begin
  { Components }
  // TRenderImage, TRenderBox,
  RegisterComponents('Codebot', [TImageStrip, TSlideBar, TThinButton,
    TRenderImage, TRenderBox,
    TIndeterminateProgress, TStepBubbles,
    THuePicker, TSaturationPicker, TBanner, TContentGrid,
    TSizingPanel, THeaderBar, TDrawList, TDrawTextList, TDetailsList, TAnimationTimer,
    TTextStorage]);
  { Property editors }
  {$ifndef lclgtk2}
  RegisterPropertyEditor(TypeInfo(Integer), TThinButton, 'ImageIndex',
    TImageStripIndexPropertyEditor);
  {$endif}
  RegisterPropertyEditor(TypeInfo(string), nil, 'ThemeName',
    TThemeNamePropertyEditor);
  RegisterPropertyEditor(TSurfaceBitmap.ClassInfo, nil, '',
    TSurfaceBitmapPropertyEditor);
  { Component editors }
  RegisterComponentEditor(TSizingPanel, TSizingPanelEditor);
  RegisterComponentEditor(TRenderImage, TRenderImageEditor);
  RegisterComponentEditor(TImageStrip, TImageStripEditor);
  { Custom forms }
  RegisterForm(TRenderForm, 'Render Form', 'A form with surface and theme support',
    'Codebot.Controls');
  RegisterForm(TBannerForm, 'Banner Form', 'A form a customizable header and footer',
    'Codebot.Controls.Banner');
end;

end.
