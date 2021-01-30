unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, Calc;

type
  TFormMain = class(TForm)
    LabelDisp: TLabel;
    GridPanelLayout1: TGridPanelLayout;
    ButtonAC: TSpeedButton;
    ButtonDel: TSpeedButton;
    ButtonSqrt: TSpeedButton;
    ButtonDiv: TSpeedButton;
    ButtonExe: TSpeedButton;
    ButtonMul: TSpeedButton;
    ButtonSub: TSpeedButton;
    ButtonAdd: TSpeedButton;
    ButtonNegative: TSpeedButton;
    ButtonDot: TSpeedButton;
    Button0: TSpeedButton;
    Button1: TSpeedButton;
    Button2: TSpeedButton;
    Button3: TSpeedButton;
    Button4: TSpeedButton;
    Button5: TSpeedButton;
    Button6: TSpeedButton;
    Button7: TSpeedButton;
    Button8: TSpeedButton;
    Button9: TSpeedButton;
    StyleBook1: TStyleBook;
    procedure ButtonACClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonDelClick(Sender: TObject);
    procedure ButtonSqrtClick(Sender: TObject);
    procedure ButtonDivClick(Sender: TObject);
    procedure ButtonMulClick(Sender: TObject);
    procedure ButtonSubClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonExeClick(Sender: TObject);
    procedure ButtonNegativeClick(Sender: TObject);
    procedure ButtonDotClick(Sender: TObject);
    procedure ButtonDigitClick(Sender: TObject);
  private
    { Private declarations }
    Calc: TCalc;

    procedure CalcChange(Sender: TObject; IsBusy: Boolean);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.ButtonDigitClick(Sender: TObject);
begin
  Calc.PressDigit(TControl(Sender).Tag);
end;

procedure TFormMain.ButtonACClick(Sender: TObject);
begin
  Calc.PressAllClear;
end;

procedure TFormMain.ButtonAddClick(Sender: TObject);
begin
  Calc.PressAdd;
end;

procedure TFormMain.ButtonDelClick(Sender: TObject);
begin
  Calc.PressDelete;
end;

procedure TFormMain.ButtonDivClick(Sender: TObject);
begin
  Calc.PressDiv;
end;

procedure TFormMain.ButtonDotClick(Sender: TObject);
begin
  Calc.PressPoint;
end;

procedure TFormMain.ButtonExeClick(Sender: TObject);
begin
  Calc.PressExe;
end;

procedure TFormMain.ButtonMulClick(Sender: TObject);
begin
  Calc.PressMul;
end;

procedure TFormMain.ButtonNegativeClick(Sender: TObject);
begin
  Calc.PressToggleSign;
end;

procedure TFormMain.ButtonSqrtClick(Sender: TObject);
begin
  Calc.PressSqrt;
end;

procedure TFormMain.ButtonSubClick(Sender: TObject);
begin
  Calc.PressSub;
end;

procedure TFormMain.CalcChange(Sender: TObject; IsBusy: Boolean);
begin
  LabelDisp.Text := Calc.Text;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Calc := TCalc.Create(8);
  Calc.OnChange := CalcChange;
  LabelDisp.Text := Calc.Text;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Calc.Free;
end;

end.
