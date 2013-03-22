# Lock1

## Sleep 2000 Work 2000

(muty@localhost)7> muty:stop().
John: 51 locks taken, 1968.1960784313726 ms (avg) for taking, 0 withdrawals
Paul: 46 locks taken, 2053.6739130434785 ms (avg) for taking, 0 withdrawals
stop
Ringo: lock released
Ringo: 48 locks taken, 1956.6458333333333 ms (avg) for taking, 0 withdrawals
George: lock taken in 3954 ms
George: lock released
George: 50 locks taken, 1999.16 ms (avg) for taking, 0 withdrawals
(muty@localhost)8>


## Sleep 20 Work 16000
(muty@localhost)3> muty:stop().
stop
Paul: lock released
Paul: 12 locks taken, 6528.666666666667 ms (avg) for taking, 25 withdrawals
John: lock taken in 5156 ms
John: lock released
John: 18 locks taken, 6120.111111111111 ms (avg) for taking, 22 withdrawals
George: lock taken in 6222 ms
Ringo: giving up
Ringo: 12 locks taken, 5258.25 ms (avg) for taking, 31 withdrawals
George: lock released
George: 15 locks taken, 5781.2 ms (avg) for taking, 24 withdrawals
(muty@localhost)4>


## Sleep 20 Work 20
(muty@localhost)5> muty:stop().
stop
George: giving up
John: giving up
George: 355 locks taken, 291.58028169014085 ms (avg) for taking, 21 withdrawals
John: 356 locks taken, 514.943820224719 ms (avg) for taking, 11 withdrawals
Ringo: lock taken in 7979 ms
Ringo: lock released
Paul: lock taken in 7989 ms
Ringo: 345 locks taken, 300.28985507246375 ms (avg) for taking, 21 withdrawals
Paul: lock released
Paul: 350 locks taken, 409.92857142857144 ms (avg) for taking, 16 withdrawals
(muty@localhost)6>

# Lock2

## Sleep 2000 Work 2000
(muty@localhost)10> muty:stop().
stop
John: lock released
John: 46 locks taken, 836.3478260869565 ms (avg) for taking, 0 withdrawals
George: lock taken in 1463 ms
Paul: lock taken in 660 ms
Ringo: lock taken in 1106 ms
Ringo: lock released
Ringo: 36 locks taken, 1441.6944444444443 ms (avg) for taking, 0 withdrawals
Paul: lock released
Paul: 29 locks taken, 1976.8620689655172 ms (avg) for taking, 1 withdrawals
George: lock released
George: 20 locks taken, 3570.1 ms (avg) for taking, 2 withdrawals
(muty@localhost)11>

## Sleep 20 Work 16000
(muty@localhost)3> muty:stop().
stop
George: giving up
George: 0 locks taken, 0 ms (avg) for taking, 55 withdrawals
Paul: giving up
Paul: 0 locks taken, 0 ms (avg) for taking, 55 withdrawals
Ringo: lock released
Ringo: 25 locks taken, 4675.56 ms (avg) for taking, 10 withdrawals
John: lock taken in 6329 ms
John: lock released
John: 26 locks taken, 3872.5 ms (avg) for taking, 18 withdrawals
(muty@localhost)4>

## Sleep 20 Work 20
(muty@localhost)5> muty:stop().
stop
John: 2863 locks taken, 8.308767027593433 ms (avg) for taking, 0 withdrawals
Paul: lock released
Paul: 1675 locks taken, 30.918805970149254 ms (avg) for taking, 0 withdrawals
George: lock released
George: 1253 locks taken, 48.65203511572226 ms (avg) for taking, 0 withdrawals
Ringo: giving up
Ringo: 2300 locks taken, 15.862173913043478 ms (avg) for taking, 1 withdrawals
(muty@localhost)6>

# Lock3

## Sleep 2000 Work 2000

## Sleep 20 Work 16000

## Sleep 20 Work 20
