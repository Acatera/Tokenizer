program Drafts;

{$APPTYPE CONSOLE}

uses
  Windows,
  Registry,
  SysUtils,
  Classes,
  Math,
  Evaluator in 'Evaluator.pas',
  Tokenizer in 'Tokenizer.pas',
  SimpleStack in 'SimpleStack.pas';

procedure WriteTokens(Tks: TDynStrArray);
var
  i: Integer;
  List: TStringList;
begin
  List := TStringList.Create;
  for i := 0 to Length(Tks) - 1 do begin
    List.Add(Tks[i]);
    WriteLn(Tks[i] + ',');
  end;
  List.SaveToFile('test.txt');
  List.Free;
end;

var
  Tokenizer: TTokenizer;
  code: string;
begin
  
  try
    if (false) then begin
      TestEvaluator;
    end else begin
      Tokenizer := TTokenizer.Create;
      code :=
        'function MyFunction(Value: integer);'#13#10 +
        'var'#13#10 +
        '  i: integer;'#13#10 +
        'begin'#13#10 +
        '  i := 10;'#13#10 +
        '  if (''alex'' = #32) then'#13#10 +
        '    WriteLn(Value);'#13#10 +
        'end;';
      code := 'sql.GetField(''cc'').From(''plc0315'').Where(''cont=5121'').Limit(''1'');';
      Writeln(Code);
      Writeln('---------------------------------');
      WriteTokens(Tokenizer.Tokenize(code));
//      WriteTokens(Tokenizer.TokenizeFile('Tokenizer.txt'));
      Tokenizer.Free;
    end;
    Readln(code);
  except
    on E: Exception do
      Writeln(E.Message);
  end;
end.
