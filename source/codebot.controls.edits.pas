(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified March 2015                                 *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.edits.txt> }
unit Codebot.Controls.Edits;

{$i codebot.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls,
  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,
  Codebot.Controls;

{ TCustomRenderEdit }

type
  TCustomRenderEdit = class(TRenderCustomControl)
  protected
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ THotShiftState }

  {THotkeyName = type string;
  THotkeyValue = type Word;

  THotkeyModifiers = set of (ssShift, ssAlt, ssCtrl, ssSuper);

  THotkey = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Apply;
    procedure Cancel;
    property Valid: Boolean read GetValid;
    property Editing: Boolean read GetEditing;
    property KeyValue: THotkeyValue read GetValue write SetValue;
  published
    property AssociateEdit:
    property Active: Boolean read FActive write SetActive;
    property Editing: Boolean read FEditing write SetEditing;
    property KeyName: THotkeyKey string read GetKeyName write SetKeyName;
    property Modifiers: THotkeyModifiers read GetModifiers write SetModifiers;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;}

implementation

{ TCustomRenderEdit }

constructor TCustomRenderEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  Width := 100;
  Height := TextHeight + 8;
end;

procedure TCustomRenderEdit.Render;
begin
end;

end.

