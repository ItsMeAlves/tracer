-module(tracer).
-compile(export_all).

Port = 33434.
Timeout = 2000.
MaxHops = 30.
IPPROTO_IP = 0.
IP_TTL = 2.

{ok, SSND} = gen_udp:open(Port, [binary, {active, false}]).
{ok, SRCV} = gen_icmp:open().

run(ttl) ->
    inet:setopts(SSND, [{raw, IPPROTO_IP, IP_TTL, <<ttl:8/native>>}]),
    gen_udp:send(SSND, {64,233,190,105}, Port, " "),
    gen_icmp:recv(SRCV, 0).


