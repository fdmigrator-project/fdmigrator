program fdmigrator;
{$APPTYPE CONSOLE}

uses
  FDMigrator.Runner in 'FDMigrator.Runner.pas',
  FDMigrator.Engine in 'FDMigrator.Engine.pas',
  FDMigrator.Engine.Templates in 'FDMigrator.Engine.Templates.pas';

{$I .\FireDAC.inc}
{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  TDbMigratorRunner.Run;
end.
