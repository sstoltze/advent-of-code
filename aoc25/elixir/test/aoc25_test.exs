defmodule Aoc25Test do
  use ExUnit.Case

  test "day 1" do
    input = ~S"
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
"

    assert 3 == Aoc25.Day1.part_1(input)

    assert 6 == Aoc25.Day1.part_2(input)
  end

  test "day 2" do
    input = ~S(
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
    )
    assert 1_227_775_554 == Aoc25.Day2.part_1(input)
    assert 4_174_379_265 == Aoc25.Day2.part_2(input)
  end

  test "number invalidity" do
    assert Aoc25.Day2.really_invalid?(999)
    assert Aoc25.Day2.really_invalid?(1010)
    assert Aoc25.Day2.really_invalid?(2_121_212_121)
    refute Aoc25.Day2.really_invalid?(21_212_121_212)
    refute Aoc25.Day2.really_invalid?(1_010_101)
  end

  test "day 3" do
    input = ~S<
987654321111111
811111111111119
234234234234278
818181911112111
>
    assert 357 == Aoc25.Day3.part_1(input)
    assert 3_121_910_778_619 = Aoc25.Day3.part_2(input)
  end
end
