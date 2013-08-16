set +H

java -server -Xmx6G -Dfile.encoding=utf-8 -jar /work1/allan/jdalton/factorie-kbbridge-plugin/target/factorie-kbbridge-1.0-SNAPSHOT-jar-with-dependencies.jar $1 $2 $3
