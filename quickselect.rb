#!/usr/bin/ruby
# -*- coding: utf-8 -*-

#Selects Nth smallest array element (starting at 1)
# in average O(n) time.
def select_nth(ary, index, start=0, stop=ary.length-1)
  if (ary.empty?) then raise "Empty array!" end 

  #base case of array with size 1
  if (stop==start) then return ary[start] end
  
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

  #we now know the item at i-1 is in its proper place in the sorted array
  #so we set statistic to be how many items it is from the array's start
  statistic = i-1-start+1

  if (statistic == index) #this is the element we wanted
    pivot
  elsif (statistic < index)
    #check right half, subtracting current statistic from index
    select_nth(ary, index-statistic, i, stop)
  else #elsif (statistic > index)
    #check left half, no change to index
    select_nth(ary, index, start, i-2)
  end
end



#tests
ARRAY_SIZE = 30
MAX_VALUE = 100
for i in 0...1000
  ary = Array.new(ARRAY_SIZE) { |i| rand(MAX_VALUE)}
  index = rand(ARRAY_SIZE)+1

  itm = select_nth(ary, index)

  if (itm != ary.sort.fetch(index-1))
    raise "Test failed with array #{ary.sort} and item #{index}!\n Ours:   #{itm} \n Theirs: #{ary.sort.fetch(index-1)}"
  end
end
