IOHKTT="`stack path --dist-dir`/build/iohktt/iohktt"

$IOHKTT slave localhost 8081 &
$IOHKTT slave localhost 8082 &
$IOHKTT slave localhost 8083 &
$IOHKTT slave localhost 8084 &
$IOHKTT slave localhost 8085 &
sleep 1
$IOHKTT --send-for 10 --wait-for 4 --with-seed 0 master localhost 8080 &
