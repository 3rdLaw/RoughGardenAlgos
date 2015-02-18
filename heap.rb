#!/usr/bin/env ruby

#Simple min-heap priority-queue implementation
class Heap
  @ary

  def initialize()
    @ary = Array.new()
  end

  def insert(itm, value)
    index = @ary.size() #location index for new item
    node = Pair.new(itm, value) #make new item
    @ary.push(node) #push appends to the end
    if index==0 then return end #if array was empty, no need to call bubble up
    bubble_up(index, node) #ensure heap invariant is retained
  end
  
  #move element up until heap invariant is restored
  def bubble_up(index, node)
    while (index!=0 && @ary[(index-1)/2].key > node.key) #while parent is bigger,
      @ary[index], @ary[(index-1)/2] = @ary[(index-1)/2], @ary[index] #swap
      index = (index-1)/2
    end
  end

  def extract_min()
    if @ary.empty? then raise "Extract min called on empty heap!" end

    minimum = @ary[0]
    last = @ary.pop #pop removes & returns last element

    if @ary.length!=0
      @ary[0] = last #place last at top
      bubble_down(0)
    end

    minimum.obj #return min
  end

  #move element down until heap invariant is restored
  def bubble_down(index)
    left  = index*2+1 #left child
    right = index*2+2 #right child

    smaller_child_index = pick_smaller(left, right)

    while (smaller_child_index!=nil && @ary[smaller_child_index].key < @ary[index].key)

      @ary[index], @ary[smaller_child_index] = @ary[smaller_child_index], @ary[index] #swap

      index = smaller_child_index
      left  = index*2+1 #left child
      right = index*2+2 #right child
      smaller_child_index = pick_smaller(left, right)

    end
  end

  def pick_smaller(left, right)
    #pick the smaller of the two while filtering out any our-of-bounds indices
    [left,right].select {|c| c<@ary.length}.min_by {|c| @ary[c].key}
  end

  def size()
    @ary.length
  end

  def to_s()
    @ary.to_s()
  end

  #check that the heap ordering invariant is maintained
  def check_invariants()
    index = 0
    left = index*2+1
    right = index*2+2

    while (left < @ary.size)
      if @ary[left].key < @ary[index].key
        raise "Smaller left child with parent index=#{index} and value=#{@ary[index].key} and left child #{@ary[left].key}"
      end

      if (right < @ary.size) && @ary[right].key < @ary[index].key
        raise "Smaller right child with parent index=#{index} and value=#{@ary[index].key} and right child #{@ary[right].key}"
      end

      index = index + 1
      left = index*2+1
      right = index*2+2

    end
  end
end

class Pair
  @obj
  @key

  def initialize(obj, key)
    @obj = obj
    @key = key
  end

  attr_accessor :obj , :key
end

#Tests
TRIALS = 150
LENGTH = 80
MAX_ELEMENT = 500
TRIALS.times do
  heap = Heap.new()
  
  LENGTH.times do 
    num = rand(MAX_ELEMENT)
    heap.insert(num,num+1)
  end
  
  heap.check_invariants()

  output = Array.new()
  LENGTH.times do 
    output.push(heap.extract_min())
  end
  
  if output != output.sort
    raise "Not-in-order extraction! #{output}"
  end
end
