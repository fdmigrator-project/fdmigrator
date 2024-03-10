# fdmigrator 
<img src="fdmigrator.jpeg" width="100">

**Database Migration Tool** based on the powers of FireDac! 

**Overview:**
fdmigrator is a versatile and user-friendly command-line tool designed to simplify the database migration process across various environments. Engineered with flexibility and simplicity at its core, fdmigrator facilitates the efficient management and execution of database migrations, ensuring seamless transitions between different database systems or versions.

**Key Features:**
- **Cross-Database Support:** Works effortlessly across multiple database systems, including MySQL, PostgreSQL, SQLite, and more, leveraging FireDAC's powerful database connectivity.
- **Automated Migration Handling:** Automatically applies, reverts, and manages migration steps, providing a clear path from development through to production.
- **Directives for Conditional SQL:** Utilizes FireDAC's escape sequences to write adaptable SQL scripts, making your migrations smart and flexible.

I will transform coffe into code :), so please:
 
<a href="https://www.buymeacoffee.com/jcangas" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png" alt="Buy Me A Coffee" style="height: 60px !important;width: 217px !important;" ></a>

**Getting Started:**
To get started with fdmigrator, install the  [latest release](https://github.com/jcangas/fdmigrator/releases/latest). Detailed setup instructions and examples can be found in the [Documentation](#documentation).

**Contribute:**
fdmigrator thrives on community contributions. Whether it's adding new features, improving documentation, or reporting bugs, your input is welcome. Check out our contributing guidelines to get involved.

**License:**
fdmigrator is released under the MIT License. See the LICENSE file for more details.

**Support:**
For support, feature requests, or bug reporting, please open an issue on our GitHub repository.

Enjoy a streamlined migration experience with fdmigrator, your go-to tool for hassle-free database migrations.

## Documentation

Use the integrated help in the tool:

```powershell
c:\>fdmigrator help
fdmigrator v0.8.0 | (c)2024, j.cangas@pm.me

Usage: fdmigrator <global options> <command> <arguments>

Global Options:
  -w <WorkDir>    Sets the working directory. Default is "./migrations".
  -d (dry-run).   Executes the process without applying changes.

Commands and Arguments:
  init            Creates the working structure in <WorkDir>

  steps           Lists the steps pending application.
    -a            Lists all steps

  status          Displays the last applied step.

  new             Creates a new migration step.
    -d            Description. Required. Used to generate the step file name
    -t            Template to use ("table" or "default")

  defs            Displays the connection names in use. Names starting with "!" are ignored.

  up              Applies all pending steps.
    -n <n>        Applies only the next <n> steps.

  down            Reverts the last applied step.
    -n <n>        Reverts the next <n> steps. Pass * for all.

  help            Displays this help.

Usage Examples:

fdmigrator -w ./db steps list
Uses "./db" as the working directory and lists the pending migration steps.

fdmigrator -w ./db up -n 3
Uses "./db" as the working directory and executes the next 3 pending migration steps.

fdmigrator down
Reverts the last applied migration step.

fdmigrator down -n 2
Reverts the last 2 applied migration steps.

fdmigrator down -n *
Reverts all previously applied migration steps.

fdmigrator status
Displays the last applied migration step.
```
