unit fpexprpars_addon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpexprpars;

Procedure ExprDegToRad(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprRadToDeg(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);

Procedure ExprTan(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprCot(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);

Procedure ExprArcsin(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprArccos(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprArccot(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);

Procedure ExprCosh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprCoth(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprSinh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprTanh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);

Procedure ExprArcosh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprArsinh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprArtanh(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprArcoth(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);

Procedure ExprSinc(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);

Procedure ExprPower(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprHypot(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);

Procedure ExprLog10(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprLog2(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);

Procedure ExprErf(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprErfc(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);

Procedure ExprGammaP(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprGammaQ(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprBetaI(var Result: TFPExpressionResult; Const Args: TExprParameterArray);

Procedure ExprChi2Dist(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprtDist(var Result: TFPExpressionResult; const Args: TExprParameterArray);
Procedure ExprFDist(var Result: TFPExpressionResult; const Args: TExprParameterArray);

Procedure ExprI0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprI1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprJ0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprJ1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprK0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprK1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprY0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
Procedure ExprY1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);

function FixDecSep(const AExpression: String): String;


implementation

uses
  Math,
  typ, spe;  // numlib

{ Additional functions for the parser }

procedure ExprDegToRad(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := degtorad(x);
end;

procedure ExprRadToDeg(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := radtodeg(x);
end;

procedure ExprTan(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := tan(x);
end;

procedure ExprCot(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := cot(x);
end;

procedure ExprArcsin(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := arcsin(x);
end;

procedure ExprArccos(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := arccos(x);
end;

procedure ExprArccot(Var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := pi/2 - arctan(x);
end;

procedure ExprCosh(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := cosh(x);
end;

{ Hyperbolic cotangent coth(x); x <> 0 }
procedure ExprCoth(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := 1/tanh(x);
end;

procedure ExprSinh(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := sinh(x);
end;

procedure ExprTanh(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := tanh(x);
end;

procedure ExprArcosh(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := arcosh(x);
end;

procedure ExprArsinh(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgtoFloat(Args[0]);
  Result.resFloat := arsinh(x);
end;

procedure ExprArtanh(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := artanh(x);
end;

procedure ExprArcoth(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := artanh(1.0/x);
end;

procedure ExprSinc(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  if x = 0 then
    Result.ResFloat := 1.0
  else
    Result.resFloat := sin(x)/x;
end;

procedure ExprPower(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x,y: Double;
begin
  x := ArgToFloat(Args[0]);
  y := ArgToFloat(Args[1]);
  Result.resFloat := Power(x, y);
end;

procedure ExprLg(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := log10(x);
end;

procedure ExprLog10(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := log10(x);
end;

procedure ExprLog2(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := log2(x);
end;

procedure ExprMax(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x1, x2: Double;
begin
  x1 := ArgToFloat(Args[0]);
  x2 := ArgToFloat(Args[1]);
  Result.resFloat := Max(x1, x2);
end;

procedure ExprMin(var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x1, x2: Double;
begin
  x1 := ArgToFloat(Args[0]);
  x2 := ArgToFloat(Args[1]);
  Result.resFloat := Min(x1, x2);
end;

Procedure ExprHypot(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x,y: Double;
begin
  x := ArgToFloat(Args[0]);
  y := ArgToFloat(Args[1]);
  Result.resFloat := Hypot(x,y);
end;

Procedure ExprErf(Var Result: TFPExpressionResult; const Args: TExprParameterArray);
// Error function
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := speerf(x);
end;

Procedure ExprErfc(Var Result: TFPExpressionResult; const Args: TExprParameterArray);
// Error function complement
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := speefc(x);
end;

// Incomplete gamma function P
Procedure ExprGammaP(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x, s: Double;
begin
  s := ArgToFloat(Args[0]);
  x := ArgToFloat(Args[1]);
  Result.resFloat := gammap(s, x);
end;

// Incomplete gamma function Q
Procedure ExprGammaQ(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  x, s: Double;
begin
  s := ArgToFloat(Args[0]);
  x := ArgToFloat(Args[1]);
  Result.resFloat := gammaq(s, x);
end;

// Incomplete beta function
Procedure ExprBetaI(var Result: TFPExpressionResult; Const Args: TExprParameterArray);
var
  a, b, x: Double;
begin
  a := ArgToFloat(Args[0]);
  b := ArgToFloat(Args[1]);
  x := ArgToFloat(Args[2]);
  Result.resFloat := betai(a, b, x);
end;

Procedure ExprChi2Dist(var  Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
  n: Double;
begin
  x := ArgToFloat(Args[0]);
  n := ArgToFloat(Args[1]);
  Result.resFloat := chi2dist(x, round(n));
end;

Procedure ExprtDist(var  Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
  n: Double;
begin
  x := ArgToFloat(Args[0]);
  n := ArgToFloat(Args[1]);
  Result.resFloat := tdist(x, round(n), 2);
end;

Procedure ExprFDist(var  Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  x: Double;
  n1, n2: Double;
begin
  x := ArgToFloat(Args[0]);
  n1 := ArgToFloat(Args[1]);
  n2 := ArgToFloat(Args[2]);
  Result.resFloat := Fdist(x, round(n1), round(n2));
end;

Procedure ExprI0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the first kind I0(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := spebi0(x);
end;

Procedure ExprI1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the first kind I1(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := spebi1(x);
end;

Procedure ExprJ0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the first kind J0(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := spebj0(x);
end;

Procedure ExprJ1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the first kind J1(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := spebj1(x);
end;

Procedure ExprK0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the second kind K0(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := spebk0(x);
end;

Procedure ExprK1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the second kind K1(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := spebk1(x);
end;

Procedure ExprY0(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the second kind Y0(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := speby0(x);
end;

Procedure ExprY1(Var Result: TFPExpressionResult; Const Args: TExprParameterArray);
// Bessel function of the second kind Y1(x)
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := speby1(x);
end;

function FixDecSep(const AExpression: String): String;
var
  i: Integer;
begin
  Result := AExpression;
  for i:=1 to Length(Result) do begin
    if Result[i] = ',' then Result[i] := '.';
  end;
end;

//---------------------

procedure RegisterExprParserAddons;
begin
  with BuiltinIdentifiers do begin
    AddFunction(bcMath, 'degtorad', 'F', 'F', @ExprDegtorad);
    AddFunction(bcMath, 'radtodeg', 'F', 'F', @ExprRadtodeg);

    AddFunction(bcMath, 'tan', 'F', 'F', @ExprTan);
    AddFunction(bcMath, 'cot', 'F', 'F', @ExprCot);
    AddFunction(bcMath, 'arcsin', 'F', 'F', @ExprArcSin);
    AddFunction(bcMath, 'arccos', 'F', 'F', @ExprArcCos);
    AddFunction(bcMath, 'arccot', 'F', 'F', @ExprArcCot);
    AddFunction(bcMath, 'cosh', 'F', 'F', @ExprCosh);
    AddFunction(bcMath, 'coth', 'F', 'F', @ExprCoth);
    AddFunction(bcMath, 'sinh', 'F', 'F', @ExprSinh);
    AddFunction(bcMath, 'tanh', 'F', 'F', @ExprTanh);
    AddFunction(bcMath, 'arcosh', 'F', 'F', @ExprArcosh);
    AddFunction(bcMath, 'arsinh', 'F', 'F', @ExprArsinh);
    AddFunction(bcMath, 'artanh', 'F', 'F', @ExprArtanh);
    AddFunction(bcMath, 'arcoth', 'F', 'F', @ExprArcoth);
    AddFunction(bcMath, 'sinc', 'F', 'F', @ExprSinc);

    AddFunction(bcMath, 'power', 'F', 'FF', @ExprPower);
    AddFunction(bcMath, 'hypot', 'F', 'FF', @ExprHypot);

    AddFunction(bcMath, 'lg', 'F', 'F', @ExprLog10);
    AddFunction(bcMath, 'log10', 'F', 'F', @ExprLog10);
    AddFunction(bcMath, 'log2', 'F', 'F', @ExprLog2);

    // Error function
    AddFunction(bcMath, 'erf', 'F', 'F', @ExprErf);
    AddFunction(bcMath, 'erfc', 'F', 'F', @ExprErfc);

    // Incomplete gamma and beta functions
    AddFunction(bcMath, 'gammap', 'F', 'FF', @ExprGammaP);
    AddFunction(bcMath, 'gammaq', 'F', 'FF', @ExprGammaQ);
    AddFunction(bcMath, 'betai', 'F', 'FFF', @ExprBetaI);

    // Probability distributions
    AddFunction(bcMath, 'chi2dist', 'F', 'FI', @ExprChi2Dist);
    AddFunction(bcMath, 'tdist', 'F', 'FI', @Exprtdist);
    AddFunction(bcMath, 'Fdist', 'F', 'FII', @ExprFDist);

    // Bessel functions of the first kind
    AddFunction(bcMath, 'I0', 'F', 'F', @ExprI0);
    AddFunction(bcMath, 'I1', 'F', 'F', @ExprI1);
    AddFunction(bcMath, 'J0', 'F', 'F', @ExprJ0);
    AddFunction(bcMath, 'J1', 'F', 'F', @ExprJ1);

    // Bessel functions of the second kind
    AddFunction(bcMath, 'K0', 'F', 'F', @ExprK0);
    AddFunction(bcMath, 'K1', 'F', 'F', @ExprK1);
    AddFunction(bcMath, 'Y0', 'F', 'F', @ExprY0);
    AddFunction(bcMath, 'Y1', 'F', 'F', @ExprY1);
  end;
end;

initialization
  RegisterExprParserAddons;

end.

