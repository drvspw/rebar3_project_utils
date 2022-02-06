rebar3_project_utils
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_project_utils, {git, "https://host/user/rebar3_project_utils.git", {tag, "<release tag>"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_project_utils
    ===> Fetching rebar3_project_utils
    ===> Compiling rebar3_project_utils
    <Plugin Output>
