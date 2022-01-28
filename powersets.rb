# http://nabetani.sakura.ne.jp/yokohamarb/103mask/

=begin
# https://dixq.net/forum/viewtopic.php?t=14893

struct { struct bintree *node; int state; } stack[1000];
int sp = 0;
#define PUSH() (stack[sp].node = node, stack[sp++].state = state)
#define POP() (node = stack[--sp].node, state = stack[sp].state)

int sum(struct bintree *node)
{
    int state = 0, s = 0;
    PUSH(), state = 1;
    while (state)
        if (state == 1) {
            //s += node->val;  // preorder
            state++;
            if (node->left) PUSH(), state = 1, node = node->left;
        }
        else if (state == 2) {
            s += node->val;  // in-order
            state++;
            if (node->right) PUSH(), state = 1, node = node->right;
        }
        else {
            //s += node->val;  // postorder
            POP();
        }
    return s;
}

=end

# For debugging and my preference.
# A Symbol object whose equality is defined only by the Object identity.
# So the users of the class must refer instances of this class by the initially
# assigned constant. While the default Symbol object is always refered by its
# literal.
class UniqueSymbol
  def initialize sym
    @sym = sym
  end

  def inspect
    "#{self.class.name}.new(#{@sym.inspect})"
  end
end

module PowerSetGen
  class Cursor < Struct.new(:node, :direction)
    def dup
      Cursor.new(self.node, self.direction)
    end
  end

  class Node < Struct.new(:index, :parent_elements, :should_add)
    ROOT = self.new(-1, [], nil)

    def has_left? bit_numbers
      self.index < bit_numbers.length
    end

    alias has_right? has_left?

    def make_left bit_numbers
      make_deeper bit_numbers, true
    end

    def make_right bit_numbers
      make_deeper bit_numbers, false
    end

    def all_elements bit_numbers
      if self.should_add
        self.parent_elements + [bit_numbers[self.index]]
      else
        self.parent_elements
      end
    end

    private
    def make_deeper bit_numbers, next_should_add
      new_parents = self.all_elements(bit_numbers)
      self.class.new(self.index + 1, new_parents, next_should_add)
    end
  end

  INIT  = UniqueSymbol.new :INIT
  LEFT  = UniqueSymbol.new :LEFT
  RIGHT = UniqueSymbol.new :RIGHT
  UP    = UniqueSymbol.new :UP

  # bit_numbers: 入力した整数における何ビット目が立っているかを表す配列
  #              べき集合を計算することで、回答となる数が
  #             「何ビット目を立てるか」を表す配列の配列を返す
  def self.enumerator bit_numbers
    Enumerator.new do |y|
      stack = []

      current = Cursor.new(Node::ROOT, INIT)
      stack.push(current.dup)

      current.direction = LEFT
      until current.direction == INIT
        case current.direction
        when LEFT
          # 現在のノードが次に行く場所はRIGHT、と記録した上で、
          current.direction = RIGHT
          if current.node.has_left?(bit_numbers)
            # まだ下のノードがいれば、現在のノードを保存して続けて左側を探索する
            stack.push(current.dup)
            current = Cursor.new(current.node.make_left(bit_numbers), LEFT)
          else
            y.yield current.node.parent_elements if current.node.should_add
          end
        when RIGHT
          current.direction = UP
          if current.node.has_right?(bit_numbers)
            stack.push(current.dup)
            current = Cursor.new(current.node.make_right(bit_numbers), LEFT)
          end
        else
          current = stack.pop
        end
      end
    end
  end

end

# https://stackoverflow.com/questions/32575630/powerset-of-a-set-with-list-comprehension-in-haskell
=begin
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = let ps = powerset xs in [x:p | p <- ps] ++ ps
=end
def powerset_rec bns
  return [[]] if bns.empty?
  x = bns.shift
  ps = powerset_rec(bns)
  ps.map {|p| [x] + p } + ps
end

def powerset_bintree bns
  PowerSetGen.enumerator(bns).to_a
end

=begin
1
3
4
[[]]
[[4], []]
[[3, 4], [3], [4], []]
# https://xoinu.hatenablog.com/entry/2015/07/25/112906
=end
def powerset_reduce xs
  xs.reduce([[]]) do |last_layer, x|
    last_layer.flat_map {|l| [l, l + [x]] }
  end
end

if __FILE__ == $PROGRAM_NAME
  $stdin.each_line do |line|
    cmd_and_args = line.split
    cmd = cmd_and_args.shift
    if cmd.nil?
      warn "No method name given"
      next
    end
    result =
      __send__("powerset_#{cmd}", cmd_and_args.map {|arg| Integer(arg, 10)})
    result.sort!
    puts result.map {|r| r.join ',' }.join(' ')
  end
end
