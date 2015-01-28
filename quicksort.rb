#!/usr/bin/ruby

#quicksort with linear, in-place partitioning
def quicksort!(ary, start=0, stop=ary.length-1)
  #base case of array with size 0 or 1
  if (stop-start<=0) then return ary end
  
  #randomly select pivot location
  loc = rand(stop-start+1)+start
  #swap pivot into to start of our subrange
  ary[start], ary[loc] = ary[loc], ary[start]
  #get pivot value
  pivot = ary[start]

  #i is boundry between < pivot and > pivot
  i = start+1

  for j in start+1..stop
    if ary[j] < pivot
      #swap with pivot's end and increment i
      ary[i], ary[j] = ary[j], ary[i]
      i=i+1
    end #else ary[j] > pivot, just j++ and we're good
  end

  #move pivot to correct location now that we're through our subrange
  ary[start], ary[i-1] = ary[i-1], ary[start]

  #divide and conqueror!
  quicksort!(ary, start,i-2) #pivot is now in correct location, so we skip it
  quicksort!(ary, i,stop)

  ary
end

#tests
ARRAY_SIZE = 30
MAX_VALUE = 100
for i in 0...1000
  ary = Array(0...ARRAY_SIZE).map! { rand(MAX_VALUE)}.shuffle
  pre_ary = Array.new(ary)
  quicksort! ary

  if (pre_ary.sort != ary)
    raise "Test failed with array #{pre_ary}!\n Ours:   #{ary} \n Theirs: #{pre_ary.sort}"
  end
end
