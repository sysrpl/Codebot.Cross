(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified September 2013                             *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.graphics.design.registration.txt> }
unit Codebot.Design.Registration;

{$i ../codebot/codebot.inc}

interface

uses
  Classes, PropEdits, ComponentEditors, MenuIntf,
  Codebot.Design.Editors,
  Codebot.Design.Forms,
  Codebot.Design.AppExplorer,
  Codebot.Graphics,
  Codebot.Animation,
  Codebot.Controls,
  Codebot.Controls.Edits,
  Codebot.Controls.Extras,
  Codebot.Controls.Grids,
  Codebot.Controls.Banner,
  Codebot.Controls.Buttons,
  Codebot.Controls.Containers,
  Codebot.Controls.Colors,
  Codebot.Controls.Scrolling,
  Codebot.Controls.Sliders,
  Codebot.Forms.ColorDialog,
  Codebot.Text.Store,
  Codebot.Process;

procedure Register;

implementation

{$R palette_icons.res}

procedure AppExplorerItemClick(Sender: TObject);
begin
  ShowAppExplorer;
end;

procedure Register;
begin
  { Components }
  // TDrawImage, TDrawBox,
  RegisterComponents('Codebot Controls', [TImageStrip, TSlideBar, TThinButton,
    TDrawImage, TDrawBox, TDrawPanel,
    TIndeterminateProgress, TStepBubbles,
    THuePicker, TSaturationPicker, TAlphaPicker, TAnglePicker, TBanner, TContentGrid,
    TSizingPanel, TCaptionBox, THeaderBar, TDrawList, TDrawTextList, TDetailsList, TAnimationTimer,
    TTextStorage, TSlideEdit, TColorSlideEdit, TAdvancedColorDialog, TExternalCommand]);
  { Property editors }
  {$ifndef lclgtk2}
  RegisterPropertyEditor(TypeInfo(Integer), TThinButton, 'ImageIndex',
    TImageStripIndexPropertyEditor);
  {$endif}
  RegisterPropertyEditor(TypeInfo(string), nil, 'ThemeName',
    TThemeNamePropertyEditor);
  RegisterPropertyEditor(TSurfaceBitmap.ClassInfo, nil, '',
    TSurfaceBitmapPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTextItems), TTextStorage, 'Items',
    TTextItemsPropertyEditor);
  { Component editors }
  RegisterComponentEditor(TSizingPanel, TSizingPanelEditor);
  RegisterComponentEditor(TDrawImage, TRenderImageEditor);
  RegisterComponentEditor(TImageStrip, TImageStripEditor);
  RegisterComponentEditor(TTextStorage, TTextStorageComponentEditor);
  { Custom forms }
  RegisterForm(TSurfaceForm, 'Render Form', 'A form with surface and theme support',
    'Codebot.Controls');
  RegisterForm(TBannerForm, 'Banner Form', 'A form a customizable header and footer',
    'Codebot.Controls.Banner');
  { The application explorer is added to the information items of the IDE menu }
  RegisterIDEMenuCommand(itmInfoHelps, 'AppExplorerItem', 'Application Explorer',
    nil, AppExplorerItemClick, nil, 'menu_information');
end;

end.
