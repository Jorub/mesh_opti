#!/bin/bash

function run_drutes {

 # rm -rf $1

  #mkdir $1

  #cp -a drutemp/* $1

  cd $1
  
  soil=$2
  bound=$3
  lowsoil=$4
  helper=$5
  anom=$6
  tdr=$7
  top=$8
  
#   substitution of parameters into input files for drutes 
  sed -e 's/!upsoil/'$soil'/g' -e 's/!bndsoil/'$bound'/g' -e 's/!lowsoil/'$lowsoil'/g' -e 's/!help/'$helper'/g' -e 's/!anom/'$anom'/g' -e 's/!tdr/'$tdr'/g' -e 's/!top/'$top'/g' drutes.conf/mesh/meshconf.geo > drutes.conf/mesh/mesh.geo
  
  /usr/local/src/gmsh-2.15.0-Linux/bin/gmsh drutes.conf/mesh/mesh.geo -2 -o drutes.conf/mesh/mesh.msh
  sed -n '13p' < drutes.conf/mesh/mesh.msh > nodes.out
# /proc/time only runs on linux 
  #read start tmp < /proc/uptime
  #execute drutes, send the output into "black hole"
  bin/drutes --tmax 5 min > /dev/null
  #read end tmp < /proc/uptime
  
  #time=`echo "$end-$start" | bc` 
  #echo $time > time.out
  echo 'process': $1 'done' > objfnc.val
  cd ..
    
    
}


rm -f drutes.vals

#count the number of processes       
let nproc=0
while read l a b c d e f ; do
    if [[  $l == "p"  ]]; then
      let nproc=nproc+1
    fi
  done < pars.in
  
#execute drutes function in parallel
let z=0
while read l a b c d e f
  do
    if [[  $l == "p"  ]]; then
      let z=z+1
      if [[ $z -lt $nproc ]] ; then
        run_drutes $z $a $b $c $d $e $f &
      else
        run_drutes $z $a $b $c $d $e $f
      fi
    fi
  done < pars.in
 

#at the end of drutes function file obj.val is created, if all files for each process exist then we have finished 
let z=0    
while [[ $z -lt $nproc ]]
  do
  let z=0
    for i in `seq 1 $nproc`
      do
        FILE="$i/objfnc.val"
        if [ -f $FILE ];
          then
             let z=z+1
        fi
    done    
done

for i in `seq 1 $nproc` ; do
  val=`cat $i/objfnc.val`
  echo $i $val >> drutes.vals
done
