PRIORITY=$1
DEPTH=$2

mongo alife --quiet --eval "db.references.find({\"citingFilled\":false,\"depth\":{\$gt:$DEPTH},\"priority\":{\$lt:$PRIORITY}}).count()"

