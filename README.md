## rebar3_project_utils
A utility plugin that
- wraps `{rebar3_lint, 0.1.11}` plugin
- integrates with `cuttlefish`
- provides the following commands
  - `check-deps` - check all `rebar.config` dependecies are included in the application's runtime dependencies i.e. included in `.app.src` file.
  - `debian` - package release into a `.deb`
  - `bump` - manage version similar to ruby's bump gem.
- `debian-control-files` template to generate debian package control files

## Usage

Add the plugin to your rebar config:

    {project_plugins, [
        {rebar3_project_utils, {git, "https://host/user/rebar3_project_utils.git", {tag, "<release tag>"}}}
    ]}.

### check-deps
```bash
$ rebar3 check-deps
```
### lint
```bash
$ rebar3 lint
```
If `elvis.config` is present in the project, this command applies the rules from that files. Otherwise a default set of rules (embedded in `rebar3_lint` plugin) is applied.

### debian-control-files
```bash
$ rebar3 new debian-control-files
```
This command generates the control files in `package/debian` directory.

### bump
```bash
$ rebar3 bump
```
To display the app's version. Use `rebar3 help bump` for more options.

### Cuttlefish
The plugin integrates with `cuttlefish`. Some of the features in other cuttlefish integration plugins might be missing. This plugin does not support `file_name` option in cuttlefish configuration.

The plugin provides a pre-start hook script that creates `generated.sys.config` and `generated.vm.args` at runtime. The applications' `sys.config` and `vm.args` files can then include the generated files.

To disable schema discovery
  ```erlang
  {cuttlefish,
  [{schema_discovery, false}]}
  ```


To enable schema discovery and specify schema load order
```
cuttlefish,
 [{schema_discovery, true},
  {schema_order, [ %% list the schema from dependencies in load order
    cuttle,
    disk,
    erlang_vm
  ]}
]}.
```

To disable cuttelfish
```erlang
{cuttlefish,
 [{enable, false}]} %% default is true
```

**NOTE:** The plugin does not process the schema file embedded in cuttlefish project.
