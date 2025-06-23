# Hot Code Reloading

Short description of the workflow for hot code reloading. The used OTP
application can be downloaded from this
[repo](https://github.com/schnef/helloworld).

Tested with:
 - Erlang 27.3.4.1
 - Rebar3 3.25.0
 
## Version 0.1.0

Fetch version 0.1.0:

	~/helloworld$ git checkout 0.1.0 

The file `rebar.conf` contains:

	{erl_opts, [debug_info]}.

	{deps, []}.

	{shell,
	 [{apps, [helloworld]}]
	}.

	{relx,
	 [{release, {helloworld, "0.1.0"},
	   [helloworld, sasl]},

	  {dev_mode, true},
	  {include_erts, false},

	  {extended_start_script, true}]
	}.

	{profiles,
	 [{prod,
	   [{relx,
		 [{dev_mode, false},
		  {include_erts, true}]
		}]
	  }]
	}.
 
Create a release and start it:

	~/helloworld$ rebar3 release

When you create a regular release, the
`_build/default/rel/helloworld/lib/` directory uses symlinks to the
libraries used, including `kernel`, `sasl`, etc. This leads to
problems when performing a hot code update because you (thankfully)
cannot update these libraries. The release must be built including a
copy of the libraries.

Create a release including the libraries according to the `prod`
profile (`{dev_mode, false}, {include_erts, true}`):

	~/helloworld$ rebar3 as prod release

Start the release:

	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld daemon
	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld versions
	Installed versions:
	* 0.1.0	permanent
	
	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld attach
	(helloworld@tp)2> helloworld:helloworld().
	"Helo, world!"

Clearly a typo!

## Version 0.2.0

Changes:
 1. In `rebar.conf`:
    - Add release 0.2.0
    - Add the `appup` plugin including `provider_hooks`. Do not add the `{pre, [{tar, {appup, tar}}]}` `provider_hook` as indicated in the original README, as it causes issues with `rebar3 tar`.
 2. Update the version number in `helloworld.app.src`.
 3. Correct the typo in `helloworld.erl`.
 4. Add `helloworld.appup.src` appup configuration:

		{"0.2.0",
		[{"0.1.0", [{load_module, helloworld}]}],
		[{"0.1.0", [{load_module, helloworld}]}]
		}.
	`rebar3 appup generate` crashes, so we need to specify our own appup configuration.
	
Fetch version 0.2.0:

	~/helloworld$ git checkout 0.2.0 

Create a new (as prod) release:

	~/helloworld$ rebar3 as prod release
	...
	===> Compiling helloworld.appup.src to /home/erlang/helloworld/_build/prod/lib/helloworld/ebin/helloworld.appup
	...

Note the line `Compiling helloworld.appup.src`, which indicates that
the appup configuration file has been included (thanks to the appup
plugin?).

Create a relup and a tar file to distribute the release:

	~/helloworld$ rebar3 as prod relup -n helloworld -v "0.2.0" -u "0.1.0"
	~/helloworld$ rebar3 as prod tar -v "0.2.0" -u "0.1.0"

The old release is still in production:

	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld versions
	Installed versions:
	* 0.1.0	permanent

Prepare the tar file so it can be picked up during the upgrade and
perform the upgrade:

	~/helloworld$ mv _build/prod/rel/helloworld/helloworld-0.2.0.tar.gz _build/prod/rel/helloworld/releases/0.2.0/

	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld upgrade 0.2.0
	Release 0.2.0 not found, attempting to unpack releases/0.2.0/helloworld-0.2.0.tar.gz
	Unpacked successfully: "0.2.0"
	Installed Release: 0.2.0
	Made release permanent: "0.2.0"

	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld versions
	Installed versions:
	* 0.2.0	permanent
	* 0.1.0	old

	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld attach
	(helloworld@tp)1> helloworld:helloworld().
	"Hello, world!"
	(helloworld@tp)2> 

Typo fixed and done!

## Encountered Issues
 1. It was initially unclear to me that, to get this working, the
    options `{dev_mode, false}` and `{include_erts, true}` for relx
    were essential. During the update, you receive an error message
    about an unsafe path.
 2. The command `rebar3 appup generate` crashes. Is this significant,
    or do you always end up writing your own appup configuration in
    practice?
 3. If the pre `provider_hook` for tar is specified, then `rebar3 tar
    ...` also crashes. Does the appup tar provider_hook add anything?
 
