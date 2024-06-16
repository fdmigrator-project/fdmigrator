unit FDMigrator.Engine.Templates;

interface

const
  DefaultConfigTT = '''
{
  "status": ""
}
''';

  DefaultTT = '''
@__prelude__.

-- ! if migrate = up

-- ! end migrate

-- ! if migrate = down

-- ! end migrate
''';

  TableTT = '''
@__prelude__.

-- ! if migrate = up
CREATE TABLE {id tablename} (
	{id id}	!IDENTITY,
	PRIMARY KEY({id id})
);
-- ! end migrate

-- ! if migrate = down
DROP TABLE {id tablename};
-- ! end migrate
''';

  FDConnectionDefsTT = '''
[FDConnectionDefs.ini]
Encoding=UTF8

[MyDatabase]
User_Name=root
Server=
Password=
Database=
DriverID=MySQL

''';

  PreludeTT = '''
@__!DriverID.prelude
''';
  MySQlPreludeTT = '''
DEF IDENTITY="BIGINT AUTO_INCREMENT"
''';

  MSSQlPreludeTT = '''
DEF IDENTITY="INT IDENTITY NOT NULL"
''';

implementation

end.
