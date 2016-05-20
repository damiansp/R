d.mon start=x0
d.rast snow
v.digit -n map=vsnow bgcmd="d.rast map=snow"
d.erase
d.vect vsnow
v.digit map=vsnow bgcmd="d.rast map=snow ; d.vect vsnow"
d.erase
d.vect vsnow
v.digit map=vsnow bgcmd="d.rast map=snow ; d.vect vsnow"
d.erase
 ; d.vect vsno ; d.vect vsno ; d.vect vsno ; d.vect vsno ; d.vect vsno ; d.vect vsnowwwwww
d.vect vsnow
v.digit map=vsnow bgcmd="d.rast map=snow ; d.vect vsnow"
d.erase
d.vect vsnow
v.digit map=vsnow bgcmd="d.rast map=snow ; d.vect vsnow"
d.erase
d.vect vsnow
v.digit map=vsnow bgcmd="d.rast map=snow ; d.vect vsnow"
d.erase
d.vect vsnow
v.ogr.out input=vsnow dsn="." olayer="vsnow"
v.out.ogr input=vsnow dsn="." olayer="vsnow"
v.out.ogr input=vsnow type=area dsn="." olayer="vsnow"
R
v.digit map=vsnow bgcmd="d.rast map=snow ; d.vect vsnow"
v.out.ogr input=vsnow type=area dsn="." olayer="vsnow"
v.digit -n map=pumps2 bgcmd="d.rast map=snow ; d.vect vsnow"
d.erase
d.vect vsnow
d.vect -o pump2
d.vect pump2
d.vect pumps2
g.region
d.vect vsnow
d.vect pumps2
g.region
d.vect vsnow
d.vect pumps2
g.region
g.region -p 
d.vect vsnow
d.zoom
d.zoom
d.zoom
g.region -p
g.region
g.region -p
v.to.rast input=vsnow output=rsnow use=val value=1
d.erase
d.rast rsnow
v.info vsnow
v.clean input=vsnow output=vsnow2 type=centroid,area tool=bpol
v.to.rast input=vsnow2 output=rsnow use=val value=1
v.to.rast input=vsnow2 output=rsnow2 use=val value=1
d.erase
d.rast rsnow2
v.out.ogr input=vsnow type=area dsn="." olayer="vsnow"
v.out.ogr input=pumps2 type=point dsn="." olayer="pumps2"
v.in.ogr -o dsn="." output=vsnow3 layer="vsnow" 
d.erase
d.vect vsnow3
v.to.rast input=vsnow3 output=rsnow3 use=val value=1
d.erase
d.rast rsnow3
d.vect pumps2
r.stats rsnow3
r.reclass rsnow3 rsnow3a
r.reclass input=rsnow3 output=rsnow3a
r.reclass
r.mapcalc
r.stats rsnow3b
r.report rsnow3b
r.report rsnow3b units=c
d.erase
d.rast rsnow3b
r.cost
g.list rast
r.cost -v input=rsnow3 output=snowcost start_points=pumps2
r.info snowcost
r.mapcalc
r.cost -v input=rsnow3b output=snowcost start_points=pumps2
g.remove snowcost
r.cost -v input=rsnow3b output=snowcost start_points=pumps2
d.erase
d.rast snowcost
d.vect pumps2
v.to.rast input=pumps2 output=rpumps2
v.to.rast input=pumps2 output=rpumps2 use=cat
r.report rpumps2 units=c
r.watershed elevation=snowcost depression=rpupms2 basin=whichpump
r.watershed elevation=snowcost depression=rpupms2 threshold=100 basin=whichpump
r.watershed elevation=snowcost depression=rpumps2 threshold=100 basin=whichpump
r.report whichpump units=c
d.rast whichpump
d.rast snowcost
d.vect pumps2
d.zoom
v.in.ogr -o dsn="." layer="deaths3" output=deaths3
d.vect map=death3 icon=basic/circle
d.vect map=deaths3 icon=basic/circle
r.flow elevin=snowcost flout=vflow dsout=rdsnow
d.vect vflow
r.flow elevin=snowcost skip=25 flout=vflow1 dsout=rdsnow1
d.rast snowcost
d.vect vflow2
d.vect vflow1
r.flow elevin=snowcost skip=10 flout=vflow2 dsout=rdsnow2
d.rast rdsnow2
d.rast snowcost
d.vect vflow2
d.rast snowcost
d.vect vflow
v.digit map=vflow bgcmd="d.rast map=snowcost ; d.vect vflow"
v.clean input=vflow output=vflow_c type=line tool=snap,rmdandle,rmdup thresh=2.5,2.5,2.5
v.clean input=vflow output=vflow_c type=line tool=snap,rmdangle,rmdup thresh=2.5,2.5,2.5
v.clean input=vflow output=vflow_c type=line tool=snap,rmdangle,rmdupl thresh=2.5,2.5,2.5
d.rast snowcost
d.vect vflow_c
v.digit map=vpump bgcmd="d.rast map=snowcost ; d.vect vflow_c"
v.info vflow_c
d.vect vflow_c
v.digit map=vpump bgcmd="d.rast map=snowcost ; d.vect vflow_c"
v.digit -n map=vpump bgcmd="d.rast map=snowcost ; d.vect vflow_c"
d.erase
d.vect vpump
d.vect pumps2
d.vect map=deaths3 icon=basic/circle
db.tables pumps2
db.tables database=pumps2
db.connect driver=dbf database='$GISDBASE/$LOCATION_NAME/$MAPSET/dbf/'
db.tables -p
db.columns table=deaths3
echo "select Num_Cases from deaths3" | db.select
d.vect pumps2 color=red
d.vect vsnow color=yellow
d.vect vpump color=green
exit
exit
PS1="GRASS 6.1.cvs > " ; export PS1
d.mon start=x0
v.digit map=vsnow bgcmd="d.rast map=snow ; d.vect vsnow"
g.region
g.region -p
v.digit map=vsnow bgcmd="d.rast map=snow ; d.vect vsnow"
d.erase
d.rast snow
d.vect vsnow
v.digit map=vsnow bgcmd="d.rast map=snow ; d.vect vsnow"
d.rast snow
d.vect vsnow
v.digit map=vsnow bgcmd="d.rast map=snow ; d.vect vsnow"
d.rast snow
d.vect vsnow
d.what.rast
v.clean input=vsnow output=vsnow2 type=centroid,area tool=bpol
v.clean input=vsnow output=vsnow2a type=centroid,area tool=bpol
v.out.ogr input=vsnow type=area dsn="." olayer="vsnow"
v.in.ogr -o dsn="." output=vsnow4 layer="vsnow" 
d.rast snow
d.vect vsnow4
g.region
g.region -p
d.rast snow
d.vect vsnow4
d.erase
d.vect vsnow4
v.to.rast input=vsnow4 output=rfsnow use=val value=1
d.rast rfsnow
r.mapcalc
r.mapcalc
r.report rfsnow,rfsnow1 units=c
r.report rfsnow1 units=c
d.erase
d.rast rfsnow1
g.region
d.erase
d.rast rfsnow1
g.region save=rsnow
r.buffer input=rfsnow1 output=buff1 distances=2.5
d.rast buff1
r.report buff1 units=c
r.mapcalc
r.report buff2 units=c
d.rast buff2
r.cost -v input=buff2 output=snowcost_b start_points=pumps2
d.erase
d.rast snowcost_b
d.vect pumps2
r.flow elevin=snowcosti_b skip=10 flout=vflow _b dsout=rdsnow_b
r.flow elevin=snowcost_b skip=10 flout=vflow_b dsout=rdsnow_b
d.vect vflow_b
d.rast rdsnow_b
r.flow elevin=snowcost_b skip=5 flout=vflow_b1 dsout=rdsnow_b1
d.rast snowcost_b
d.vect vflow_b1
d.erase
d.rast snowcost_b
d.vect pumps2
d.vect deaths3 col=blue
d.erase
d.rast snowcost_b
db.columns table=deaths3
d.vect.thematic deaths3 col=Num_Cases type=point themetype="graduated points" icon=basic/circle
d.vect.thematic deaths3 column=Num_Cases type=point themetype="graduated points" icon=basic/circle
d.erase
v.digit -n map=vpump_broad bgcmd="d.rast map=snow"
g.region
v.digit -n map=vpump_broad bgcmd="d.rast map=snow"
g.region -p
d.erase
d.rast snow
v.digit -n map=vpump_broad bgcmd="d.rast map=snow"
v.digit -n map=vpump_not_broad bgcmd="d.rast map=snow"
g.region
d.erase
d.rast snow
d.vect vpump_not_broad col=red
d.vect vpump_broad col=green
g.region
d.erase
d.rast snow
d.vect vpump_broad col=green
d.vect vpump_not_broad col=red
v.digit -n map=vpump_not_broad bgcmd="d.rast map=snow"
v.
v.digit map=vpump_not_broad bgcmd="d.rast map=snow; d.vect vpump_not_broad col=red"
d.erase
d.rast snow
d.vect vpump_broad col=green
d.vect vpump_not_broad col=red
g.region
d.erase
d.rast snow
d.vect vpump_not_broad col=red
d.vect vpump_broad col=green
g.region
d.erase
r.cost -v input=buff2 output=snowcost_broad start_points=vpump_broad
d.rast snowcost_broad
r.cost -v input=buff2 output=snowcost_not_broad start_points=vpump_not_broad
d.rast snowcost_not_broad
r.mapcalc
d.rast wpump
d.vect.thematic deaths3 column=Num_Cases type=point themetype="graduated points" icon=basic/circle
v.out.ascii deaths3
v.out.ascii deaths3 | r.what input=snowcost_not_broad,snowcost_broad
v.out.ascii deaths3 | awk -F"|" "{print $1, $2}" | r.what input=snowcost_not_broad,snowcost_broad
v.out.ascii deaths3 | awk -F"|" "{print($1, $2)}" | r.what input=snowcost_not_broad,snowcost_broad
v.out.ascii deaths3 | awk -F"|" '{print($1, $2)}' | r.what input=snowcost_not_broad,snowcost_broad
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=snowcost_not_broad,snowcost_broad,wpump
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=buff2
d.erase
d.rast buff2
d.vect deaths3
r.buffer input=rfsnow1 output=buff1a distances=3
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=buff1a
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=buff1a | grep "\*$"
r.buffer input=rfsnow1 output=buff1a distances=3.5
r.buffer input=rfsnow1 output=buff1b distances=3.5
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=buff1b | grep "\*$"
r.buffer input=rfsnow1 output=buff1c distances=4.5
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=buff1c | grep "\*$"
r.buffer input=rfsnow1 output=buff1c distances=4
r.buffer input=rfsnow1 output=buff1d distances=4
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=buff1d | grep "\*$"
r.report buff1d
g.remove buff2
r.mapcalc
r.report buff2 units=c
d.erase
d.rast buff2
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=buff2 | grep "\*$"
d.vect.thematic deaths3 column=Num_Cases type=point themetype="graduated points" icon=basic/circle
r.cost -v input=buff2 output=snowcost_not_broad start_points=vpump_not_broad
g.remove snowcost_not_broad
r.cost -v input=buff2 output=snowcost_not_broad start_points=vpump_not_broad
g.remove snowcost_broad
r.cost -v input=buff2 output=snowcost_broad start_points=vpump_broad
d.rast snowcost_broad
g.remove wpump
r.mapcalc
r.mapcalc
d.rast wpump
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=snowcost_not_broad,snowcost_broad,wpump
d.rast snowcost_broad
d.rast snowcost_not_broad
g.remove wpump
r.mapcalc
d.rast wpump
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=snowcost_not_broad,snowcost_broad,wpump
v.out.ascii deaths3 | awk -F"|" '{print($1, $2, $3)}' | r.what input=snowcost_not_broad,snowcost_broad,wpump > dist.txt
v.out.ogr input=vsnow type=area dsn="." olayer="vsnow"


v.out.ogr input=vpumps_not_broad type=point dsn="." olayer="pumps_nb"
v.out.ogr input=vpump_not_broad type=point dsn="." olayer="pumps_nb"
v.out.ogr input=vpump_broad type=point dsn="." olayer="pumps_b"
d.vect.thematic deaths3 column=Num_Cases type=point themetype="graduated points" icon=basic/circle
d.vect vpump_broad col=green
d.vect vpump_not_broad col=blue
d.rast snowcost_broad
d.vect.thematic deaths3 column=Num_Cases type=point themetype="graduated points" icon=basic/circle
d.vect vpump_not_broad col=blue
d.vect vpump_broad col=green
d.erase
d.vect vsnow
d.erase
g.list vect
d.vect vsnow4
d.rast snowcost_broad
d.erase
d.vect vsnow4
d.rast -o snowcost_broad
ls -l
r.out.gdal input=snowcost_broad type=Float32 output=cost_b
r.out.gdal input=snowcost_broad format=AAIGrid output=cost_b
r.out.gdal input=snowcost_broad format=AAIGrid type=Float32 output=cost_b
r.out.arc input=snowcost_broad output=cost_b
more cost_b
r.out.arc input=snowcost_not_broad output=cost_nb
ls -l cost*
R
R
ls ~/topics/grassdata/snow2
ls ~/topics/grassdata/snow2/rsb
ls -a ~/topics/grassdata/snow2/rsb
more ~/topics/grassdata/snow2/rsb/.bash_history
cp ~/topics/grassdata/snow2/rsb/.bash_history grasshist1
more ~/topics/grassdata/snow2/rsb/.bash_history
cp ~/topics/grassdata/snow2/rsb/.bash_history grasshist2
PS1="GRASS 6.1.cvs > " ; export PS1
v.in.ogr dsn="." output=HUCs layer=HUCs type=boundary location=HUCS
g.list rast
d.mon start=x0
d.rast snowcost_broad
g.list vect
d.vect vsnow3
d.vect vsnow4
d.vect vpump_broad
d.rast snowcost_broad
d.vect vpump_broad
d.vect vpump_not_broad
g.list rast
R
R
echo $GISRC
g.gisenv
echo $LD_LIBRARY_PATH
R
echo $PATH
R
system("g.list rast")
g.list rast
sh
