# player-indiecito

Aclaración de @riq

> Los .sid de sidtracker64 usan shadow variables, asi que que se pueden
> implementar vumeters y demás sin necesidad de modificar manualmente el sid.
>
> Las variables se encuentran haciendo esto:
>
> - ir a la posición 0x1005... ahí hay un jsr a la rutina que copia las shadows
>   al sid:
>   - en el ejemplito salta a $1f78
>
> - todas las shadow estan juntas, salvo los gates.
>   - en el ejemplito las shadows van de $1870 a $1870 + 21  (la info de los
>     filtros puede que este también pero no estoy 100% seguro).
>   - y las gates estan en $189f, $189f + 7, $189f + 14... la gate se ve porque
>     luego del "lda" hace el "and" con la dirección de la gate.
>
> Ejemplito de como encontrarlos:
> https://asciinema.org/a/7cibrvfpn20wjhi7ak7ag64rk
