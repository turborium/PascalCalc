program CalcFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMain in 'UnitMain.pas' {FormMain},
  Calc in 'Calc.pas',
  CalcDisplay in 'CalcDisplay.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
