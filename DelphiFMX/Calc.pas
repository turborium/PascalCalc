unit Calc;

{$ifdef fpc}{$mode delphi}{$endif}

interface

uses
  Classes, SysUtils, CalcDisplay;

type
  // Cобытие изменения Calc
  TCalcChangeEvent = procedure(Sender: TObject; IsBusy: Boolean) of object;

  // Возможные операции
  TOperation = (opNone, opAdd, opSub, opMul, opDiv, opNegative, opSqrt);

  // Возможные состояния
  TCalcState = (csHold, csInput, csResult, csError);

  { TCalc }

  TCalc = class
  private
    // регистр экрана
    RegisterX: TCalcDisplay;
    // регистр операнда
    RegisterY: Double;
    // регистр константы
    RegisterK: Double;

    // состояние
    State: TCalcState;

    // текущая операция
    CurrentOperation: TOperation;

    FOnChange: TCalcChangeEvent;
    procedure Change(IsBusy: Boolean);
    procedure ClearAll;
    procedure PerformOperation(Operation: TOperation);
    procedure DoOperation(Operation: TOperation);
    procedure DoFunction(Operation: TOperation);
  public
    constructor Create(const MaxCount: Integer);
    destructor Destroy; override;
    // Действия кнопок
    procedure PressAllClear;
    procedure PressDigit(N: Integer);
    procedure PressPoint;
    procedure PressToggleSign;
    procedure PressDelete;
    procedure PressAdd;
    procedure PressSub;
    procedure PressMul;
    procedure PressDiv;
    procedure PressSqrt;
    procedure PressExe;
    // Text возвращает текст на "экране"
    function Text: string;
    // Событие изменения состояния калькулятора
    property OnChange: TCalcChangeEvent read FOnChange write FOnChange;
  end;

implementation

uses
  Math;

const
  sCalcError = 'Error';

{ TCalc }

constructor TCalc.Create(const MaxCount: Integer);
begin
  inherited Create;

  RegisterX := TCalcDisplay.Create(MaxCount);
end;

destructor TCalc.Destroy;
begin
  RegisterX.Free;

  inherited Destroy;
end;

procedure TCalc.Change(IsBusy: Boolean);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, IsBusy);
end;

procedure TCalc.PressAllClear;
begin
  ClearAll;
  Change(True);
end;

procedure TCalc.PressDigit(N: Integer);
begin
  // При наличии ошибки игнорируем ввод
  if State = csError then
    exit;
  // Если калькулято не в состоянии ввода, то начинаем новый ввод
  if State <> csInput then
  begin
    RegisterX.Clear;
    State := csInput;
  end;
  // Пробуем добавить цифру
  RegisterX.TryAddDigit(N);
  // Уведомляем об обновлении
  Change(False);
end;

procedure TCalc.PressPoint;
begin
  // При наличии ошибки игнорируем ввод
  if State = csError then
    exit;
  if State <> csInput then
  begin
    RegisterX.Clear;
    State := csInput;
  end;
  RegisterX.TryAddPoint;
  Change(False);
end;

procedure TCalc.PressDelete;
begin
  if State <> csInput then
    Exit;
  RegisterX.TryDeleteSymbol;
  Change(False);
end;

procedure TCalc.PressToggleSign;
begin
  // При наличии ошибки игнорируем ввод
  if State = csError then
    exit;

  if State <> csInput then
  begin
    DoFunction(opNegative);
    Exit;
  end;

  RegisterX.TryToggleSign;
  Change(False);
end;

procedure TCalc.PressAdd;
begin
  DoOperation(opAdd);
end;

procedure TCalc.PressSub;
begin
  DoOperation(opSub);
end;

procedure TCalc.PressMul;
begin
  DoOperation(opMul);
end;

procedure TCalc.PressDiv;
begin
  DoOperation(opDiv);
end;

procedure TCalc.PressSqrt;
begin
  DoFunction(opSqrt);
end;

procedure TCalc.ClearAll;
begin
  // очищаем регистры x, y
  RegisterX.Clear;
  RegisterY := 0;

  // состояние ввода
  State := csInput;

  // устанавливаем пустую операцию
  CurrentOperation := opNone;
end;

procedure TCalc.PerformOperation(Operation: TOperation);
var
  X, Y: Double;
begin
  X := RegisterX.Value;
  Y := RegisterY;

  case Operation of

    opAdd:
      X := Y + X;

    opSub:
      X := Y - X;

    opMul:
      X := Y * X;

    opDiv:
      if not IsZero(X) then
        X := Y / X
      else
      begin
        State := csError;
        Exit;
      end;

    opNegative:
      X := -X;

    opSqrt:
      if X >= 0 then
        X := Sqrt(X)
      else
      begin
        State := csError;
        Exit;
      end;

    opNone:;// nope

    else
      Exception.Create('BAD OPERATION!');
  end;

  // Проверка на переполнение
  if Abs(X) >= Power(10, RegisterX.MaxCount) - 0.5 then
  begin
    State := csError;
    Exit;
  end;

  RegisterX.Value := X;
end;

procedure TCalc.DoFunction(Operation: TOperation);
begin
  // При наличии ошибки игнорируем
  if State = csError then
    exit;

  PerformOperation(Operation);

  if (State = csInput) and (State <> csError) then
    State := csHold;

  Change(True);
end;

procedure TCalc.DoOperation(Operation: TOperation);
begin
  // При наличии ошибки игнорируем
  if State = csError then
    exit;

  if State = csInput then
    PerformOperation(CurrentOperation);

  if State <> csError then
  begin
    RegisterY := RegisterX.Value;
    CurrentOperation := Operation;
    State := csHold;
  end;

  Change(True);
end;

procedure TCalc.PressExe;
begin
  // При наличии ошибки игнорируем ввод
  if State = csError then
    exit;

  if State <> csResult then
  begin
    RegisterK := RegisterX.Value;
    State := csResult;
  end else
  begin
    RegisterY := RegisterX.Value;
    RegisterX.Value := RegisterK;
  end;

  PerformOperation(CurrentOperation);

  Change(True);
end;

function TCalc.Text: string;
begin
  if State <> csError then
    Result := RegisterX.ToString
  else
    Result := sCalcError;
end;

end.

