### sample_service
An OTP release

### Usage
Add `sample_service` as a dependency in `rebar.config`
```erlang
{deps, [
	{sample_service, "<latest release>"}
	]}.
```

If not using `hex.pm`
```erlang
{deps, [
	{sample_service, {git, "https://github.com/drvspw/sample_service.git", {tag, "<latest release>"}}}
	]}.
```

### Contributing