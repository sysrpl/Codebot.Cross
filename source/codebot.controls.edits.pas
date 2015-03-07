(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://www.codebot.org                              *)
(*  Modified February 2015                              *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.edits.txt> }
unit Codebot.Controls.Edits;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Graphics,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls;

type

  { TCustomEdit }

  TCustomEdit = class(TRenderCustomControl)
  protected
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TCustomEdit }

constructor TCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  Width := 100;
  Height := TextHeight + 8;
end;

procedure TCustomEdit.Render;
begin
end;

end.

