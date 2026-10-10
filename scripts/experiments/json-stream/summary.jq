def median:
  sort | length as $n |
  if $n % 2 == 1 then .[($n / 2 | floor)]
  else (.[($n / 2)-1] + .[$n / 2]) / 2 end;
group_by([.kind, .fixture, .chunk]) |
map(.[0] as $row |
  {kind:$row.kind, fixture:$row.fixture, chunk:$row.chunk,
   variants:(group_by(.variant) | map({key:.[0].variant, value:{
     samples:length, ns_median:(map(.ns_per_pass)|median),
     ns_min:(map(.ns_per_pass)|min), ns_max:(map(.ns_per_pass)|max),
     allocated_median:(map(.allocated_per_pass)|median),
     max_live_bytes:(map(.max_live_bytes)|max),
     successes:(map(.good)|add), passes:(map(.count)|add)
   }}) | from_entries)})
