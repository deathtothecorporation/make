/-  v=vapor
/+  w=work, default-agent, dbug, pp=pprint,
    auto=language-server-complete, shoe, sole
::
|%
+$  card  $+(shoe-card card:shoe)
::
+$  versioned-state
  $%(state-0)
::
+$  state-0
  $:  %0
      width=@ud                                     ::  display width
      eny=@uvJ                                      ::  entropy
      arena=?(%make %ware)                          ::  module arena
  ==
::
+$  command
  $%
      [%help ~]                                     ::  print usage info
      [%mode ~]                                     ::  print mode info
      [%settings ~]                                 ::  print settings
      [%toggle ~]                                   ::  switch arenas
      [%ls ~]                                       ::  ls your namespace
      ::  make
      [%wrap =oath:v =deed:v]                       ::  attach to a slip
      :: ware
      [%show =oath:v =deed:v]                       ::  show a slip
  ==
::
--
%+  verb:w  |
%-  agent:dbug
=|  [versioned-state log=[verb=_& state=_| type=state-0]]
=*  state  -.-
=*  log  log.-
::    +main
::
::  main shoe core
^-  agent:gall
%-  (agent:shoe command)
^-  (shoe:shoe command)
=<
  |_  =bowl:gall
  +*  this       .
      make-core  +>
      cc         ~(. make-core(eny eny.bowl) bowl)
      def        ~(. (default-agent this %|) bowl)
      des        ~(. (default:shoe this command) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    =^  cards  state  (prep:cc ~)
    [cards this]
  ::
  ++  on-save  !>(state)
  ::
  ++  on-load
    |=  old-state=vase
    ^-  (quip card _this)
    =/  maybe-old  (mule |.(!<(versioned-state old-state)))
    =/  old=versioned-state
      ?:  ?=(%| -.maybe-old)  *state-0  p.maybe-old
    =^  cards  state
    ?-  -.old
      %0  (prep:cc ~)
    ==
    [cards this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark        (on-poke:def mark vase)
        %noun         (poke-noun:cc !<(* vase))
      ==
    [cards this]
  ::
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      ?-    -.sign
        %poke-ack   [- state]:(on-agent:def wire sign)
        %watch-ack  [- state]:(on-agent:def wire sign)
      ::
          %kick
        :_  state
        ?+  wire  ~
          [%make ~]  ~[connect:cc]
        ==
      ::
          %fact
        ?+  p.cage.sign  ~|([%make-cli-bad-sub-mark wire p.cage.sign] !!)
            %make-delta
          ~|([%make-cli-bad-sub-mark wire p.cage.sign] !!)
        ==
      ==
    [cards this]
  ::
  ++  on-arvo  on-arvo:def
  ::
  ++  on-fail   on-fail:def
  ++  command-parser
    |=  =sole-id:sole
    parser:sh:cc
  ::
  ++  tab-list
    |=  =sole-id:sole
    =/  tuck
      |=  [term=cord detail=tank]
      [(cat 3 ';' term) detail]
    ;:  weld
      ~[['--common--' leaf+"common commands"]]
      %+  turn  comm:tab-list:sh:cc  tuck
      ?:  ?=(%make arena)
      ::  ~[['--make--' leaf+"%make commands"]]
        %+  turn  make:tab-list:sh:cc  tuck
      ::  ~[['--ware--' leaf+"%ware commands"]]
      %+  turn  ware:tab-list:sh:cc  tuck
    ==
  ::
  ++  on-command
    |=  [=sole-id:sole =command]
    =^  cards  state
      (work:sh:cc command)
    [cards this]
  ::
  ++  on-connect
    |=  =sole-id:sole
    ^-  (quip card _this)
    [[prompt:sh-out:cc ~] this]
  ::
  ++  can-connect     can-connect:des
  ++  on-disconnect   on-disconnect:des
  --
::
=|  cards=(list card)
|_  =bowl:gall
::
++  this  .
::
++  prep
  |=  old=(unit versioned-state)
  ^-  (quip card _state)
  ?~  old
    =^  cards  state
      :-  ~[connect]
      %_  state
        width     80
      ==
    [cards state]
  [~ state(width 80)]
::
++  emit
  |=  car=card
  this(cards [car cards])
::
++  emil
  |=  rac=(list card)
  |-  ^+  this
  ?~  rac
    this
  =.  cards  [i.rac cards]
  $(rac t.rac)
::
++  abet
  ^-  (quip card _state)
  [(flop cards) state]
::
++  connect
  ^-  card
  [%pass /make %agent [our-self %make] %watch /make-primary]
::
++  our-self  our.bowl
::
++  poke-noun
  |=  a=*
  ^-  (quip card _state)
  ?:  ?=(%connect a)
    abet:(emit connect)
  [~ state]
::    +sh: parsing and input
::
::  command format is defined here as
::  the command prefixed by a mic (;).
::
::  %help: ;help
::  %toggle: ;toggle
::  %mode: ;mode
::  %ls: ;ls
::  %settings: ;settings
::  %show: ;show oath deed
::
::  oath: '0x0ffdasfa1' or 0x0ffda.sfa1
::  deed: '123453' or 123.453
++  sh
  |%
  ::
  ::    +parser: command parser
  ::
  ::  parses the command line buffer.
  ::  produces commands which can be executed by +work.
  ::
  ++  parser
    |^
      %+  stag  |
      =-  ;~(pfix mic -)
      ;~  pose
        ;~(plug (tag %help) (easy ~))
        ;~(plug (tag %toggle) (easy ~))
        ;~(plug (tag %mode) (easy ~))
        ;~(plug (tag %ls) (easy ~))
        ?.  =(%make arena)  ware
        make
      ==
    ::
    ++  ware
      ;~((glue ace) (tag %show) bo it)
    ::
    ++  make
      ;~  pose
        ;~((glue ace) (tag %wrap) bo it)
        ;~(plug (tag %settings) (easy ~))
      ==
    ::
    ++  bo
      %+  cook  oath:v
      ;~(pose bos ;~(pfix (jest '0x') hex:ag))
    ::
    ++  bos
      %+  ifix  [soq soq]
      ;~(pfix (jest '0x') hex)
    ++  it  ;~(pose (cold ~ sig) ;~(pose its (cook |*(@ `deed:v`[~ +<]) dem)))
    ++  its  (cook |*(@ `deed:v`[~ +<]) (ifix [soq soq] dem))
    ++  none  ;~(pfix ace sig)
    ++  spade  ;~(pfix ace sym)
    ++  proj  ;~(pfix ace ;~(pfix cen sym))
    ++  spads
      %+  cook  ~(gas in *(set desk))
      (most ;~(plug com (star ace)) sym)
    ++  tag   |*(a=@tas (cold a (jest a)))
    ++  bool
      ;~  pose
        (cold %| (jest '%.y'))
        (cold %& (jest '%.n'))
      ==
    --
  ::  +tab-list: static list of autocomplete entries
  ::
  ++  tab-list
    |%
    ++  comm
      ^-  (list [@tas tank])
      :~
        [%help leaf+";help"]
        [%settings leaf+";settings"]
        [%toggle leaf+";toggle"]
        [%mode leaf+";mode"]
        [%ls leaf+";ls"]
      ==
    ++  ware
      ^-  (list [@tas tank])
      :~
        [%show leaf+";show oath deed"]
      ==
    ++  make
      ^-  (list [@tas tank])
      :~
        [%wrap leaf+";wrap oath deed"]
      ==
    --
  ::  +work: run user command
  ::
  ++  work
    |=  job=command
    ^-  (quip card _state)
    =/  cmd  (print:sh-out "args: {<+.job>}")
    |^  ?-  -.job
          %help      abet:help
          %toggle    abet:toggle
          %settings  abet:settings
          %mode  abet:help
          %ls    abet:ls
          :: ware
            %show
          =<  abet
          ~(show ware +.job)
          :: make
            %wrap
          =<  abet
          ~(wrap make +.job)
        ==
    ::
    ++  settings
      ^+  this
      %-  emit
      =-  (print-more:sh-out -)
      :~
        ">> {<arena>} mode ({<`@tas`(end [3 1] arena)>})"
        ">> width: {(scow %ud width)}"
        ">> log: {<log>}"
      ==
    ::
    ++  help
      ^+  this
      %-  emit
      =-  (print-more:sh-out -)
      :~
        ">> {<arena>} mode ({<`@tas`(end [3 1] arena)>})"
      ==
    ::
    ++  toggle
      =.  arena  ?:(?=(%make arena) %ware %make)
      %-  emit
      prompt:sh-out
    ::
    ++  ls
      %-  emit
      =-  (print:sh-out -)
      <(seek (list deed:v) /)>
    ::
    ++  ware
      |_  [=oath:v =deed:v]
      ++  show
        %-  emil
        :~  (print:sh-out "show: {<oath>} {<deed>}")
          %^  act  %show  %make
          :-  %vapor-make-action
          !>  ^-  actions:make:v
          [%show ~]
        ==
      --
    ++  make
      |_  [=oath:v =deed:v]
      ++  wrap
        %-  emil
        :~  (print:sh-out "wrapping: {<oath>} {<deed>}")
          %^  act  %wrap  %make
          :-  %vapor-make-action
          !>  ^-  actions:make:v
          [%wrap oath deed]
        ==
      --
    ::
    ++  act
      |=  [what=term app=term =cage]
      ^-  card
      :*  %pass
          /cli/make/[what]
          %agent
          [our-self app]
          %poke
          cage
      ==
    --
  --
::
::    +sh-out: output
::
::  cards for producing cli output
::
++  sh-out
  |%
  ::  +effect: console effect card
  ::
  ++  effect
    |=  effect=sole-effect:sole
    ^-  card
    [%shoe ~ %sole effect]
  ::  +prant: prints a list of tanks
  ::
  ++  prant
    |=  tal=(list tank)
    ^-  card
    (effect %tan tal)
  ::  +print: puts some text into the cli as-is
  ::
  ++  print
    |=  txt=tape
    ^-  card
    (effect %txt ">> {txt}")
  ::  +print-more: puts lines of text into the cli
  ::
  ++  print-more
    |=  txs=(list tape)
    ^-  card
    %+  effect  %mor
    (turn txs |=(t=tape [%txt t]))
  ::  +note: prints left-padded ---| txt
  ::
  ++  note
    |=  txt=tape
    ^-  card
    =+  lis=(simple-wrap txt (sub width 16))
    %-  print-more
    =+  ?:((gth (lent lis) 0) (snag 0 lis) "")
    :-  (runt [14 '-'] '|' ' ' -)
    %+  turn  (slag 1 lis)
    |=(a=tape (runt [14 ' '] '|' ' ' a))
  ::  +prompt: update prompt to display current audience
  ::
  ++  prompt
    ^-  card
    %+  effect  %pro
    :+  &  %make-line
    ^-  tape
    " ({<arena>})> "
  ::
  --
::
++  simple-wrap
  |=  [txt=tape wid=@ud]
  ^-  (list tape)
  ?~  txt  ~
  =/  [end=@ud nex=?]
    ?:  (lte (lent txt) wid)  [(lent txt) &]
    =+  ace=(find " " (flop (scag +(wid) `tape`txt)))
    ?~  ace  [wid |]
    [(sub wid u.ace) &]
  :-  (tufa (scag end `(list @)`txt))
  $(txt (slag ?:(nex +(end) end) `tape`txt))
::
++  seek
  |*  [=mold =path]
  .^  mold
    %gy
    (scot %p our.bowl)
    %make
    (scot %da now.bowl)
  ==
::
++  look
  |*  [=mold =path]
  .^  mold
    %gx
    (scot %p our.bowl)
    %make
    (scot %da now.bowl)
    (snoc path %noun)
  ==
--
