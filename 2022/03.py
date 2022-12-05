
def priority(item):
  if item.islower():
    return ord(item) - ord('a') + 1
  else:
    return ord(item) - ord('A') + 27

def part1(data):
  lines = data.splitlines()
  priority_sum = 0
  for l in lines:
    compartment1 = l[:len(l) // 2]
    compartment2 = l[len(l) // 2:]
    common = set(compartment1).intersection(compartment2)
    if len(common) != 1:
      raise ValueError("Common: %s" % common)
    priority_sum += priority(next(iter(common)))
  return priority_sum

def part2(data):
  lines = data.splitlines()
  lines_by_3 = [lines[i:i + 3] for i in range(0, len(lines), 3)]
  priority_sum = 0
  for group in lines_by_3:
    s1, s2, s3 = map(set, group)
    common_item = s1.intersection(s2, s3)
    priority_sum += priority(next(iter(common_item)))
  return priority_sum


def main():
  data = open("03.input").read()
  print(part1(data))
  print(part2(data))

if __name__ == '__main__':
  main()
