::  Tell app to print what it's doing
::
::  For apps that use lib/work, :app +work toggles verbosity.
::
:-  %say
|=  [* arg=?(~ [%state ~]) ~]
[%work ?~(arg %loud %state)]
