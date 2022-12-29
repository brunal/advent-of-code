import re

*grid, _, path = open('22.input')
pos, dir = grid[0].index('.') * 1j, 1j
grid = {(x+y*1j): c for x,l in enumerate(grid)
                    for y,c in enumerate(l) if c in '.#'}

def wrap(pos,dir):
    x, y = pos.real, pos.imag
    match dir, x//50, y//50:
        case  1j, 0, _: return complex(149-x, 99), -1j
        case  1j, 1, _: return complex( 49,x+ 50), -1
        case  1j, 2, _: return complex(149-x,149), -1j
        case  1j, 3, _: return complex(149,x-100), -1
        case -1j, 0, _: return complex(149-x,  0),  1j
        case -1j, 1, _: return complex(100,x- 50),  1
        case -1j, 2, _: return complex(149-x, 50),  1j
        case -1j, 3, _: return complex(  0,x-100),  1
        case  1 , _, 0: return complex(  0,y+100),  1
        case  1 , _, 1: return complex(100+y, 49), -1j
        case  1 , _, 2: return complex(-50+y, 99), -1j
        case -1 , _, 0: return complex( 50+y, 50),  1j
        case -1 , _, 1: return complex(100+y,  0),  1j
        case -1 , _, 2: return complex(199,y-100), -1

for move in re.findall(r'\d+|[RL]', path):
    match move:
        case 'L': dir *= +1j
        case 'R': dir *= -1j
        case _:
            for _ in range(int(move)):
                p, d = pos + dir, dir
                if p not in grid: p, d = wrap(p, d)
                if grid[p] == '.': pos, dir = p, d
                print('Pos = (%s %s)' % (int(pos.real), int(pos.imag)))

print(1000 * (pos.real+1) + 4 * (pos.imag+1) + [1j,1,-1j,-1].index(dir))
