unit FDMigrator.Runner;

interface

uses
  System.StrUtils,
  System.SysUtils,
  System.Generics.Collections,
  FDMigrator.Engine;

type
  TCommandOptions = TDictionary<string, string>;

  TDbMigratorRunner = class
  public const
    Version = '0.9.0';
    Product = 'fdmigrator';

  public type
    TCommand = (unknown, help, steps, status, defs, up, down, init, new);
  public const
    CommandNames: array [TCommand] of string = ('help', 'steps', 'status', 'defs', 'up', 'down', 'init', 'new', '');
  private
    FEngine: TDbMigratorEngine;
    FCommand: TCommand;
    FWorkDir: TFileName;
    FCommandOpts: TCommandOptions;
    FDryRun: Boolean;
    procedure DoHelpCmd;
    procedure DoStepsCmd;
    procedure DoStatusCmd;
    procedure DoDefsCmd;
    procedure DoUpCmd;
    procedure DoDownCmd;
    function ProcessCommandLine: Boolean;
    procedure ProcessOption(var ParamIdx: Integer);
    function IsOption(const ParamIdx: Integer): Boolean;
    procedure DoNewCmd;
    procedure DoInitCmd;
  protected
    function GetEngine: TDbMigratorEngine;
    procedure SetWorkDir(const Value: TFileName);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property WorkDir: TFileName read FWorkDir write SetWorkDir;
    property DryRun: Boolean read FDryRun write FDryRun;
    property Engine: TDbMigratorEngine read GetEngine;
    class procedure Run;
  end;

implementation

uses
  System.IOUtils;

const
{$I 'help-content.txt'}

{ TDbMigratorRunner }

class procedure TDbMigratorRunner.Run;
begin
  var
  Runner := TDbMigratorRunner.Create;
  try
    try
      Runner.Execute;
    except
      on E: Exception do
      begin
        WriteLn;
        Writeln('ERROR: ' + E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Runner.Free;
  end;
end;

procedure TDbMigratorRunner.SetWorkDir(const Value: TFileName);
begin
  if TPath.IsPathRooted(Value) then
    FWorkDir := Value
  else
    FWorkDir := TPath.GetFullPath(Value);
end;

constructor TDbMigratorRunner.Create;
begin
  inherited;
  FCommandOpts := TCommandOptions.Create;
end;

destructor TDbMigratorRunner.Destroy;
begin
  FCommandOpts.Free;
  FEngine.Free;
  inherited;
end;

function TDbMigratorRunner.GetEngine: TDbMigratorEngine;
begin
  if FEngine = nil then
  begin
    if string(FWorkDir).IsEmpty then
      WorkDir := 'migrations';
    FEngine := TDbMigratorEngine.Create(WorkDir, DryRun);
  end;
  Result := FEngine;
end;

procedure TDbMigratorRunner.ProcessOption(var ParamIdx: Integer);
begin
  var
  Option := UpperCase(Copy(ParamStr(ParamIdx), 2));
  if Option.StartsWith('-') then
    Delete(Option,1, 1);

  if FCommand <> unknown then
  begin
    Inc(ParamIdx);
    var
    Value := ParamStr(ParamIdx);
    FCommandOpts.Add(Option, Value);
    Inc(ParamIdx);
  end
  // Global opts
  else if (Option = 'W') then
  begin
    Inc(ParamIdx);
    WorkDir := ParamStr(ParamIdx);
    Inc(ParamIdx);
  end
  else if (Option = 'D') then
  begin
    DryRun := True;
    Inc(ParamIdx);
  end;
end;

function TDbMigratorRunner.IsOption(const ParamIdx: Integer): Boolean;
begin
  Result := (ParamIdx <= ParamCount) and CharInSet(ParamStr(ParamIdx)[1],
    ['-', '/']);
end;

function TDbMigratorRunner.ProcessCommandLine: Boolean;
begin
  Result := True;
  var
  ParamIdx := 1;
  while ParamIdx <= ParamCount do
  begin
    while IsOption(ParamIdx) do
      ProcessOption(ParamIdx);
    FCommand := TCommand((1 + IndexText(ParamStr(ParamIdx), CommandNames))
      mod SizeOf(CommandNames));
    if FCommand = unknown then
      Exit(False);
    Inc(ParamIdx);
    if ParamIdx > ParamCount then
      Break;
    while IsOption(ParamIdx) do
      ProcessOption(ParamIdx);
  end;
end;

procedure TDbMigratorRunner.Execute;
begin
  if ProcessCommandLine then
    GetEngine;
  case FCommand of
    unknown:
      DoHelpCmd;
    help:
      DoHelpCmd;
    steps:
      DoStepsCmd;
    status:
      DoStatusCmd;
    defs:
      DoDefsCmd;
    up:
      DoUpCmd;
    down:
      DoDownCmd;
    init:
      DoInitCmd;
    new:
      DoNewCmd;
  end;
end;

procedure TDbMigratorRunner.DoHelpCmd;
begin
  var
  help := Help_en_GB.Replace('{product}', Product, [rfReplaceAll])
    .Replace('{version}', Version);
  Writeln(help);
end;

procedure TDbMigratorRunner.DoStatusCmd;
begin
  var StatusText := 'Last Step: ' + Engine.Config.Status;
  if Engine.Config.Status.IsEmpty then
    StatusText := 'No steps applied';

  Writeln(StatusText);
end;

procedure TDbMigratorRunner.DoDefsCmd;
begin
  for var def in Engine.defs do
  begin
    Writeln(def);
  end;
end;

procedure TDbMigratorRunner.DoStepsCmd;
begin
  var
  Criteria: TStepsCriteria := [Pending];
  if FCommandOpts.ContainsKey('A') then
    Criteria := [];
  for var step in Engine.GetSteps(Criteria) do
  begin
    Writeln(Format('id: %s | name: %s', [step.ID, step.Name]));
  end;
end;

procedure TDbMigratorRunner.DoUpCmd;
begin
  var
  n := -1; // all steps by default;
  if FCommandOpts.ContainsKey('N') then
    n := StrToIntDef(FCommandOpts['N'], 1);
  Engine.DbUp(n);
end;

procedure TDbMigratorRunner.DoDownCmd;
begin
  var
  n := 1; // one steps by default;
  if FCommandOpts.ContainsKey('N') then
  begin
    if (FCommandOpts['N'] = '*') then
      n := -1
    else
      n := StrToIntDef(FCommandOpts['N'], 1);
  end;
  Engine.DbDown(n);
end;

procedure TDbMigratorRunner.DoInitCmd;
begin
  var Kind: TStepIDKind;
  var Fmt: string;

  if FCommandOpts.ContainsKey('SEQ') then
  begin
    kind := TStepIDKind.Sequence;
    if (FCommandOpts['SEQ'] = '') then
      Fmt := '00000'
    else
      Fmt := FCommandOpts['SEQ']
  end
  else if FCommandOpts.ContainsKey('TS') or FCommandOpts.IsEmpty then
  begin
    kind := TStepIDKind.TimeStamp;
    if FCommandOpts.IsEmpty or (FCommandOpts['TS'] = '')  then
      Fmt := 'yyyymmddhhnnsszzz'
    else
      Fmt := FCommandOpts['TS']
  end
  else
    raise Exception.Create( 'Inavlid option ' + FCommandOpts.Keys.ToArray[0]);

  Engine.Init(Kind, Fmt);
end;

procedure TDbMigratorRunner.DoNewCmd;
begin
  if not FCommandOpts.ContainsKey('D') or  FCommandOpts['D'].IsEmpty then
    raise Exception.Create( 'missing -d arg');

  var Name := FCommandOpts['D'];
  if not FCommandOpts.ContainsKey('T') then
    Engine.NewStep(Name)
  else
    Engine.NewStep(Name, FCommandOpts['T']);
end;

end.
