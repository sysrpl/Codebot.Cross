{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit codebot_render;

{$warn 5023 off : no warning about unused units}
interface

uses
  Codebot.OpenGL, Codebot.Render.Buffers, Codebot.Render.Contexts, 
  Codebot.Render.Fonts, Codebot.Render.Scenes, Codebot.Render.Shaders, 
  Codebot.Render.Textures, Codebot.Render.World, Codebot.Render.TrueType, 
  Codebot.Render.FontStash, Codebot.Render.NanoVG, Codebot.Render.Graphics, 
  Codebot.Interop.Assimp, Codebot.Render.SVG, Codebot.Render.Widgets, 
  Codebot.Render.Widgets.Themes, Codebot.Render.Widgets.Custom, 
  Codebot.Render.Scenes.Widgets, Codebot.Interop.Chipmunk2D, Codebot.Physics, 
  Codebot.Render.Scenes.Physics, Codebot.Interop.SDL2, 
  Codebot.Render.Widgets.Dialogs, Codebot.Interop.MPV, 
  Codebot.Render.Widgets.Video, Codebot.Interop.MiniMp3, 
  Codebot.Interop.Vorbis, Codebot.Interop.Xmp, Codebot.Hardware, 
  Codebot.OpenGL.SDL, Codebot.Interop.MinGW, Codebot.Render.Assets, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('codebot_render', @Register);
end.
