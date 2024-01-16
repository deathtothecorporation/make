/-  spider, v=vapor, scanner
/+  *strandio, e=ethio, eth=ethereum, *naive, *vapor
=,  strand=strand:spider
|_  url=@t
++  since-block
  |=  block=@udblocknumber
  =/  m  (strand ,vase)  ^-  form:m
  %-  pure:m
  !>  ^-  updates:make:v
  block+`block
++  block-number
  |=  which=?(%latest %specific)
  =/  m  (strand ,vase)  ^-  form:m
  ;<  bloq=block:jael  bind:m  (get-latest-block:e url)
  %-  pure:m
  !>  ^-  updates:make:v
  block+`number.id.bloq
++  contract-name
  |=  =oath:v
  =/  m  (strand ,vase)  ^-  form:m
  =/  input  (name:form:ubi oath)
  ~&  >>  input
  ;<  n=*  bind:m  (read-contract:e input)
  %-  pure:m  !>(n)
  ::
++  pad-64  |=(a=addy:v ((x-co:co 64) a))
++  data-to-hex
  |=  data=@t
  ?~  data  *@ux
  ?:  =(data '0x')  *@ux
  (hex-to-num:eth data)
++  fake-log
  =,  dejs:format
  |=  log=json
  =;    [data=@t topics=(lest @ux)]
    [*@ux data topics]
  =-  ((ot -) log)
  :~
      [%data so]
      =-  [%topics (cu - (ar so))]
      |=  r=(list @t)
      ^-  (lest @ux)
      ?>  ?=([@t *] r)
      [(hex-to-num:eth i.r) (turn t.r hex-to-num:eth)]
  ==
++  scry
  |%  
    ++  block
      |=  =path
      .^(@udblocknumber %gx [%block path])
  --
++  bonds
  |%
    ++  registry  0x67d1.2c4d.b022.c543.cb7a.678f.882e.dc93.5b89.8940
    ++  milady    0x5af0.d982.7e0c.53e4.799b.b226.655a.1de1.52a4.25a5
    ++  onboarding  0x1271.f55f.03ee.1110.4e52.f19a.7bc8.eb18.4b79.fd3b
  --
++  ubi
  |%
  ++  topi
    |=  =tape
    ^-  [@ @]
    =/  label
      =+  (cook |=(a=^tape (crip (cass a))) (star alf))
      (more next -)
    =-  [- -:(rash (crip tape) label)]
    %-  hex-to-num:eth
    %-  crip
    %+  weld  "0x"
    %+  render-hex-bytes:eth  32
    %-  keccak-256:keccak:crypto
    %-  as-octs:mimes:html
    (crip tape)
  ++  form
    |%
    ++  name
      |=  =oath:v
      ^-  [@t proto-read-request:rpc:eth]
      =-  [url -]
      =-  [~ oath -]
      ['name()' ~]
    ++  mint  *@
    ++  pill-sold
      %-  hex-to-num:eth
      '0x8521335d13b4288ab290d8fcca260b5f42ea5adc0533b02a6b03ff12566eec34'
    --
  --
++  hunt
  |%
  +$  state
    $:  %0
      rpc=node:v
      boq=@udblocknumber
      =oath:v
      oaths=(set oath:v)
      to=(unit @ux)
      =bowl:gall
      watcher=?(%scanner %eth-watcher)
      id=(unit @ux)
      pot=(list @ux)
    ==
  +$  event  [log=event-log boq=@ud]
  +$  event-log
    $:  address=@ux
        data=@t
        topics=(lest @ux)
    ==
  +$  effects    (list diff)
  ++  diff
    $%  [%transfer from=address to=address]
        $:  %pill  id=@ux
            $%  [%owner =address]
                [%url url=@t]
        ==  ==
        [%pack data=vase path=(unit path)]
    ==
  ++  pokes
    |_  [state pat=path]
    ++  abed
      |=  [=_rpc =_boq =_oath =_bowl =_watcher]
      ^-  (list card)
      %_  loan
          pot
        `(list @ux)`[-:bulk:coin -:solo:coin -:solo:toke ~]:log-names
        rpc  rpc
        boq  boq
        oath  oath
        oaths  (~(put in oaths) oath)
        bowl  bowl
        watcher  watcher
      ==
    ++  base  
      ^-  card
      =/  funs=(list @ux)  ~[-:sale:toga -:novo:toga]:log-names
      =+  deeds=`(list @ux)`~[registry onboarding]:bonds 
      =/  =config:scanner
        [rpc %& ~s30 ~m15 boq ~ deeds `5 [funs 0x0 0x0 0x0 ~]]
      =/  =wire  (lane %hund `[-:deeds ~])
      =/  =path
        (welp wire /(scot %ud boq))
      =-  [%pass path %agent [our.bowl watcher] %poke -]
      scanner-poke+!>(`poke:scanner`watch+[base+wire config])
  :: 
    ++  loan
        ^-  (list card)
        :-  base
        =-  (turn ~(tap in oaths) -)
        |=  =oath:v
        =/  top=topics:scanner  [pot 0x0 0x0 0x0 ~] 
        =/  =config:scanner
          [rpc %& ~s30 ~m15 boq ~ [oath]~ `5 top]
        =/  =wire  (lane %hund `[oath ~])
        =/  =path
          (welp wire /(scot %ud boq))
        ~&  path+path
        ~&  wire+wire
        =-  [%pass path %agent [our.bowl watcher] %poke -]
        scanner-poke+!>(`poke:scanner`watch+[trans+wire config])
    --
  ++  deco
    |%
    ++  diam
      |=  data=@ux
      ^-  (list [id=@ux amt=@ux])
      =/  len  (div (met 3 data) 2)
      =/  ids  (cut 3 [0 len] data)
      =/  amts  (cut 3 [len len] data)
      =/  id-size  32
      =/  amt-size  32
      %+  turn  (gulf 0 (dec (div len id-size)))
      |=  i=@ud
      =/  id-offset  (mul i id-size)
      =/  amt-offset  (mul i amt-size)
      =/  id  (cut 3 [id-offset id-size] ids)
      =/  amt  (cut 3 [amt-offset amt-size] amts)
      [id amt]
    ++  diti
      |=  data=@ux
      ^-  [id=@ux amt=@ux]
      =/  id  (cut 3 [0 32] data)
      =/  amt  (cut 3 [32 32] data) 
      [id amt]
    --
  ++  log-names
    |%
    ++   match  |*(* =(+<- +<+<))
    ++   dead 
      |=  @ux
      ^-  (unit cord)
      ?:  (match +< novo:toga)
        `(tail novo:toga)
      ?:  (match +< sale:toga)
        `(tail sale:toga)
      ?:  (match +< solo:toke)
        `(tail solo:toke)
      ?:  (match +< solo:coin)
        `(tail solo:coin)
      ?:  (match +< bulk:coin)
        `(tail bulk:coin)
      ~
    ::  token gated
    ++  toga
      |%
      ++  novo
        %-  topi:form:ubi
        "AccountCreated(address,address,uint256,address,uint256,uint256)"
      ++  sale
        %-  topi:form:ubi
        "PackageBought(address,uint256,uint32,uint256)"
      --
    ::  e721
    ++  toke
      |%
      ++  solo
        %-  topi:form:ubi
        "Transfer(address,address,uint256)"
      -- 
    ::  e1155
    ++  coin
      |%
      ++  solo
        %-  topi:form:ubi
        "TransferSingle(address,address,address,uint256,uint256)"
      ::
      ++  bulk
        %-  topi:form:ubi
        "TransferBatch(address,address,address,uint256[],uint256[])"
      --
    --
  ++  process
    =,  log-names
    |=  [=state log=event-log]
    ^-  [effects ^state]
    =*  log-name  i.topics.log
    =/  full=(pole @ux)  t.topics.log
    =/  dota
      (crip ((x-co:co 128) data.log))
    ~&  >>>  function+(dead:log-names log-name)
    ?:  (match log-name solo:toke)
      ?+    full  ~&("cant parse transfer" `state)
          [from=@ to=@ ~]
        `state(to `to.full)
          [from=@ to=@ id=@ ~]
        :_  state(to `to.full, id `id.full)
        [%pack !>(to.full) `/owner]~
      ==
    ?:  (match log-name solo:coin)
      ?+    full  ~&("cant parse ERC-1155 transfer" `state)
          [@ from=@ to=@ ~]
        =+  (decode-results:abi:eth data.log ~[%uint %uint])
        :_  state(to `to.full, id ``@ux`(head -))
        [%pack !>(-) `/(scot %tas (tail solo:coin))]~
      ==
    ?:  (match log-name bulk:coin)
      ?+    full  ~&("cant parse ERC-1155 batch transfer" `state)
          [@ from=@ to=@ ~]
        =+  ids=(diam:deco `@ux`data.log)
        ~&  >>  "ids: {<ids>}"
        :_  state(to `to.full, id `id:(head ids))
        [%pack !>(ids) `/(scot %tas (tail bulk:coin))]~
      ==
    ?:  (match log-name novo:toga)
      ?+    full  ~&("cant parse account created" `state)
          [@ tc=@ id=@ ~]
        =/  desu  |=(@ (decode-results:abi:eth +< ~[%address]))
        =/  nova=@ux
         =+   res=(mule |.((desu data.log)))
         ?-  -.res
           %&  p.res
           %|  (desu dota)
         ==
        =/  id=@ux  id.full
        :_  state(id `id, oath registry:bonds)
        [%pack !>(nova) `/(scot %tas (tail novo:toga))/aliases/(scot %ux id)]~
      ==
    ?:  (match log-name sale:toga)
      ?+    full  ~&("cant parse package bought: {<full>}" `state)
          [eoa=@ id=@ ship=@ ~]
        =/  desu  |=(@ (decode-results:abi:eth +< ~[%uint]))
        =/  pill=@ud
         =+   res=(mule |.((desu data.log)))
         ?-  -.res
           %&  p.res
           %|  (desu dota)
         ==
        =,  full
        ~&  >>  "eoa: {<`@ux`eoa>}"
        ~&  >>  "id: {<id>}"
        ~&  >>  "ship: {<`@p`ship>}"
        :_  state(id `id, to `eoa, oath onboarding:bonds)
        [%pack !>([eoa ship id]) `/(scot %tas (tail sale:toga))/aliases/(scot %ux eoa)]~
      ==
    :_  state
    [%pack !>(data.log) ~]~
  --
--
