# Hot code reloading

Korte beschrijving van de workflow bij hot code reloading. Gebruikte
OTP applicatie kan worden gedownload van deze
[repo](https://github.com/schnef/helloworld)

Getest met:
 - erlang 27.3.4.1
 - rebar3 3.25.0
 
## Versie 0.1.0

Haal versie 0.1.0 op:

	~/helloworld$ git checkout 0.1.0 

Bestand rebar.conf bevat:

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
 
Maak een release en start deze:

	~/helloworld$ rebar3 release

Als je nu een gewone release maakt worden
`_build/default/rel/helloworld/lib/` links gebruikt naar de gebruikte
libraries waaronder `kernel`, `sasl` etc. Dit leidt tot problemen
wanneer je een hot code update uitvoert omdat je deze libraries
(goddank) niet mag updaten. De release moet worden gebouwd inclusief
een kopie van de libraries.

Maak een release inclusief de libararies volgens het profile `prod`
(`{dev_mode, false}, {include_erts, true}`):

	~/helloworld$ rebar3 as prod release

Start release:

	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld daemon
	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld versions
	Installed versions:
	* 0.1.0	permanent
	
	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld attach
	(helloworld@tp)2> helloworld:helloworld().
	"Helo, world!"

Overduidelijk een typo!

## Versie 0.2.0

Aanpassingen:
 1. In `rebar.conf`:
    - release 0.2.0 toevoegen
    - Voeg plugin `appup` toe inclusief `provider_hooks` Voeg niet
      zoals aangegeven in de originele README de `{pre, [{tar, {appup,
      tar}}]}` `provider_hook` toe omdat deze problemen geeft bij
      `rebar3 tar`
    - Versienummer in `helloworld.app.src` aanpassen
    - Typo corrigeren in `hellowworld.erl`
    - Voeg `helloworld.appup.src` appup configuratie toe:

		{"0.2.0",
		[{"0.1.0", [{load_module, helloworld}]}],
		[{"0.1.0", [{load_module, helloworld}]}]
		}.
	`rebar3 appup generate` crasht, dus we moeten zelf een appup configuratie opgeven.
	
Haal versie 0.2.0 op:

	~/helloworld$ git checkout 0.2.0 

Maak een nieuwe (as prod) release aan:

	~/helloworld$ rebar3 as prod release
	...
	===> Compiling helloworld.appup.src to /home/erlang/helloworld/_build/prod/lib/helloworld/ebin/helloworld.appup
	...

Let op de regel `Compiling helloworld.appup.src` die aangeeft dat het
appup configuratie bestand is meegenomen (dankzij de plugin appup?).

Maak een relup aan en een tar file om de release te distribueren:

	~/helloworld$ rebar3 as prod relup -n helloworld -v "0.2.0" -u "0.1.0"
	~/helloworld$ rebar3 as prod tar -v "0.2.0" -u "0.1.0"

De oude release is nog steeds in productie:

	~/helloworld$ ./_build/prod/rel/helloworld/bin/helloworld versions
	Installed versions:
	* 0.1.0	permanent

Zet de tar file klaar zodat deze kan worden opgepikt tijdens de
upgrade en voer de upgrade uit:

	~/helloworld$ mv _build/prod//rel/helloworld/helloworld-0.2.0.tar.gz _build/prod/rel/helloworld/releases/0.2.0/

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

Typo fixed en klaar!

## Ondervonden problemen
 1. Het was mij eerst niet duidelijk dat, om dit werkend te krijgen,
    voor relx de opties `{dev_mode, false}` en `{include_erts, true}`
    essentieel waren. Bij de update krijg je een foutmelding over een
    unsafe path
 2. De opdracht `rebar3 appup generate` crasht. Is dat van belang of
    schrijf je in de praktijk toch steeds je eigen appup configuratie?
 3. Als de pre `provider_hook` voor tar is gespecificeerd, dan crasht
    `rebar3 tar …` ook. Voegt de appup tar provider_hook iets toe?
