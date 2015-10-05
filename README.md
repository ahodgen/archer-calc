Archer-Calc
====

Archer-Calc is an implementation of calculated fields in RSA Archer.

One can build and test programs, and compile them into a format
suitable for Archer.

Quick example of the REPL:

```
     _             _                ____      _
    / \   _ __ ___| |__   ___ _ __ / ___|__ _| | ___
   / _ \ | '__/ __| '_ \ / _ \ '__| |   / _` | |/ __| Version 0.0.0.0-Devel
  / ___ \| | | (__| | | |  __/ |  | |__| (_| | | (__  Type :help for help
 /_/   \_\_|  \___|_| |_|\___|_|   \____\__,_|_|\___|

ArcherCalc is provided with ABSOLUTELY NO WARRANTY.
ArcherCalc> let days_ago x = datedif x now Day;
ArcherCalc> let date_color x = if ((days_ago x) < 30) then "Green" else "Red";
ArcherCalc> field incdate = "Incident Date" : Date as ISODate "2015-04-06";
ArcherCalc> date_color incdate;
"Red" : Text
ArcherCalc> :emit date_color incdate;
IF(DATEDIF([Incident Date],NOW(),DAY)<30,"Green","Red")
ArcherCalc>
```
