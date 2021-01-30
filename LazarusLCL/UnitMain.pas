unit UnitMain;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, Calc;

type

  { TSpeedButton }

  TSpeedButton = class(Buttons.TSpeedButton)
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure PaintBackground(var PaintRect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TFormMain }

  TFormMain = class(TForm)
    EditDisp: TEdit;
    PanelDisp: TPanel;
    PanelButtons: TPanel;
    ButtonAllClear: TSpeedButton;
    Button5: TSpeedButton;
    Button6: TSpeedButton;
    ButtonSub: TSpeedButton;
    Button1: TSpeedButton;
    Button2: TSpeedButton;
    Button3: TSpeedButton;
    ButtonAdd: TSpeedButton;
    ButtonPoint: TSpeedButton;
    Button0: TSpeedButton;
    ButtonToggleSign: TSpeedButton;
    ButtonBackspace: TSpeedButton;
    ButtonExe: TSpeedButton;
    ButtonSqrt: TSpeedButton;
    ButtonDiv: TSpeedButton;
    Button7: TSpeedButton;
    Button8: TSpeedButton;
    Button9: TSpeedButton;
    ButtonMul: TSpeedButton;
    Button4: TSpeedButton;
    TimerBusy: TTimer;
    procedure ButtonBackspaceClick(Sender: TObject);
    procedure ButtonDivClick(Sender: TObject);
    procedure ButtonPointClick(Sender: TObject);
    procedure ButtonExeClick(Sender: TObject);
    procedure ButtonSqrtClick(Sender: TObject);
    procedure ButtonSubClick(Sender: TObject);
    procedure ButtonMulClick(Sender: TObject);
    procedure ButtonNumberClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonToggleSignClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonAllClearClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerBusyTimer(Sender: TObject);
  protected
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer; AKeepBase: Boolean); override;
  private
    Calc: TCalc;
    procedure DispChange(Sender: TObject; IsBusy: Boolean);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TSpeedButton }

procedure TSpeedButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := 1;
  PreferredHeight := 1;
end;

procedure TSpeedButton.PaintBackground(var PaintRect: TRect);
begin
  if Self.FState = bsDown then
    Canvas.Brush.Color := RGBToColor(
      Red(Self.Color) * 2 div 3,
      Green(Self.Color) * 2 div 3,
      Blue(Self.Color) * 2 div 3
    )
  else
    Canvas.Brush.Color := Self.Color;
  Canvas.FillRect(PaintRect);
  //inherited PaintBackground(PaintRect);
end;

constructor TSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Font.Color := clWhite;
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Self.Calc := TCalc.Create(8);
  Self.Calc.OnChange := DispChange;

  Self.EditDisp.Text := Self.Calc.Text;
end;

procedure TFormMain.ButtonNumberClick(Sender: TObject);
begin
  Self.Calc.PressDigit((Sender as TControl).Tag);
end;

procedure TFormMain.ButtonAddClick(Sender: TObject);
begin
  Self.Calc.PressAdd;
end;

procedure TFormMain.ButtonPointClick(Sender: TObject);
begin
  Self.Calc.PressPoint;
end;

procedure TFormMain.ButtonExeClick(Sender: TObject);
begin
  Self.Calc.PressExe;
end;

procedure TFormMain.ButtonSubClick(Sender: TObject);
begin
  Self.Calc.PressSub;
end;

procedure TFormMain.ButtonMulClick(Sender: TObject);
begin
  Self.Calc.PressMul;
end;

procedure TFormMain.ButtonBackspaceClick(Sender: TObject);
begin
  Self.Calc.PressDelete;
end;

procedure TFormMain.ButtonDivClick(Sender: TObject);
begin
  Self.Calc.PressDiv;
end;

procedure TFormMain.ButtonToggleSignClick(Sender: TObject);
begin
  Self.Calc.PressToggleSign;
end;

procedure TFormMain.ButtonSqrtClick(Sender: TObject);
begin
  Self.Calc.PressSqrt;
end;

procedure TFormMain.ButtonAllClearClick(Sender: TObject);
begin
  Self.Calc.PressAllClear;
end;

procedure TFormMain.FormResize(Sender: TObject);
var
  Size, I: Integer;
begin
  Size := 70;
  Self.Canvas.Font.Size := Size;
  while Self.Canvas.TextWidth('-.000000000000') > EditDisp.ClientWidth do
  begin
    Dec(Size);
    Self.Canvas.Font.Size := Size;
  end;

  EditDisp.Font.Size := Size;

  for I := 0 to PanelButtons.ControlCount - 1 do
  begin
    PanelButtons.Controls[I].Font.Size := Trunc(Size * 0.8);
  end;
end;

procedure TFormMain.TimerBusyTimer(Sender: TObject);
begin
  Self.EditDisp.Text := Self.Calc.Text;
  TimerBusy.Enabled := False;
end;

procedure TFormMain.ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer;
  AKeepBase: Boolean);
begin
  AHeight := Trunc(AWidth * 1.25);

  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, AKeepBase);
end;

procedure TFormMain.DispChange(Sender: TObject; IsBusy: Boolean);
begin
  if IsBusy then
  begin
    Self.EditDisp.Text := '';
    TimerBusy.Enabled := True;
  end else
  begin
    Self.EditDisp.Text := Self.Calc.Text;
  end;

  // hack for hide caret
  Self.EditDisp.Enabled := False;
  Self.EditDisp.Enabled := True;
end;

end.

