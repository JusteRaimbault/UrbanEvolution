
* keyword search "artificial intelligence urban", 200 ref, depth 2
* keyword search ""artificial life" urban", 50 ref, depth 0
* set priorities (BUG: corrected)
* collect x2 (no conso, max depth >0) -> db.references.count() = 6263; links = 6518
* ! bug in ip logging?
* db.references.find({"horizontalDepth.artificial_life":{$gt:-1}}).count() = 50 => refs to be updated for depth
db.references.updateMany({"horizontalDepth.artificial_life":{$gt:-1}},{$set:{depth:2}})
{ "acknowledged" : true, "matchedCount" : 47, "modifiedCount" : 47 } (3 with update before: NO 47 only?)
! set integer db.references.updateMany({"horizontalDepth.artificial_life":{$gt:-1}},{$set:{depth:NumberInt(2)}}) (otherwise bug with priorities)
db.references.updateMany({depth:2},{$set:{depth:NumberInt(2)}}) => 3 missing without horizontalDepth (why?)
* java -jar bibliodata.jar --database --priority alife 2 (update priorities) : Priorities : updating 5855 references
* rerun collection: java -jar bibliodata.jar --citation --mongo alife 5000 500 true false
* rq: still relevant for alife at 200 -> collect missing 150 (surely less with citations already collected and art. intelligence); java -jar bibliodata.jar --keywords --mongo data/alife_request.csv alife 200 2 false urban; re-set priorities ; pb with depth: update depth to 2 
db.references.find({"horizontalDepth.artificial_life":{$gt:-1}}).count()
200
db.references.find({$and:[{"horizontalDepth.artificial_life":{$gt:-1}},{priority:{$gt:-1}},{depth:2}]}).count()
53 => should be 50 ~ ok. -> can update all alife, deeper levels not collected yet.
db.references.updateMany({"horizontalDepth.artificial_life":{$gt:-1}},{$set:{depth:NumberInt(2)}})
{ "acknowledged" : true, "matchedCount" : 200, "modifiedCount" : 147 }
some priorities at 1000, some not set: ??? + does not correspond to hdepth for some with only alife
* run // collection: ./parrun.sh "java -jar bibliodata.jar --citation --mongo alife 1000 250 true" 10


