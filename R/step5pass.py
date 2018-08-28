from numba import jit
"""
naive implementation of steps 1-5

i'm anticipating even with jit poor
relative performance in comparison to
an algorithm designed for large scale iteration
but this can serve as clear, readable psuedo code
as well as identify any issues with a linear approach

ben escobar, csafe-isu 2k24

src:
    IEEE TRANSACTIONS ON SYSTEMS, MAN, 
    AND CYBERNETICS, VOL. SMC - 13, 
    NO. 1, JANUARY/FEBRUARY 1983 

!!! recall all programatic dimensions will be Row x Col, not X x Y
"""
# constants
BLACK = 0
WHITE = 1
DNC = 3  # "do not care"
"""
# globals
g_matrix = []
fill = []
clean = []
"""
# probably should be a regex tostring ?? idk
I1MASK = [
    [DNC, DNC, BLACK, BLACK, BLACK, DNC, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [BLACK, BLACK, BLACK, WHITE, BLACK, BLACK, BLACK],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, DNC, BLACK, BLACK, BLACK, DNC, DNC]
]
I2MASK = [
    [DNC, DNC, BLACK, BLACK, BLACK, DNC, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [BLACK, BLACK, BLACK, WHITE, BLACK, BLACK, BLACK],
    [BLACK, BLACK, BLACK, WHITE, BLACK, BLACK, BLACK],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, DNC, BLACK, BLACK, BLACK, DNC, DNC]
]
I3MASK = [
    [DNC, DNC, BLACK, BLACK, BLACK, DNC, DNC, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [BLACK, BLACK, BLACK, WHITE, BLACK, BLACK, BLACK],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, DNC, BLACK, BLACK, BLACK, DNC, DNC]
]

# iterates through consant masks defined above
# compares current


@jit
def compareMask(mask, sr, sc, matrix):
    print('comparing masks')
    sc_copy = sc
    for i in range(len(mask)):
        for j in range(len(mask[0])):
            if(sr > len(matrix) or sc > len(matrix[0]) or sr < 0 or sc < 0):
                return False
            value = matrix[sr][sc]
            if(not value == mask[i][j]):
                return False
            sc = sc + 1
        sr = sr + 1
        sc = sc_copy
    return True


@jit
def process(sr, sc, process_type, fill, clean, matrix):
    if(process_type == "3x3"):
        print('processing 3x3"')
        p3x3(sr, sc, fill, clean, matrix)
    elif(process_type == "4x3"):
        print('processing 4x3"')
        p4x3(sr, sc, fill, clean, matrix)
    elif(process_type == "3x4"):
        print('processing 4x3"')
        p3x4(sr, sc, fill, clean, matrix)
    else:
        print("error in process_type")


"""
kinda gross and manual
all pXxX execute as follows:
1) ensure H1,H2,H3 is fit
2) check corners
3) if corner is white mark neighbords for deletion
4) if neither corner is white check I1,I2,I3
H1 -> I1, H2 -> I3, H3 -> I2
"""
#!! idea 28 8 18, define constant masks, splice og matrix to check conditional
# should be less messy and ideally faster


@jit
# ensure before I masks are checked the sr and sc are changed in these functions below
def p3x3(sr, sc, fill, clean, matrix):
    #print("length of g_matrix in p3x3",len(matrix))
    #print(g_matrix[sr][sc+1])
    if(matrix[sr][sc+1] == BLACK and matrix[sr+1][sc] == BLACK
            and matrix[sr+2][sc+1] == BLACK and matrix[sr+1][sc+2] == BLACK
            and matrix[sr+1][sc+1] == WHITE):
        check_corners((sr, sc), (sr, sc+2), (sr+2, sc), (sr+2, sc+2),clean,matrix)
    elif(compareMask(I1MASK, sr-2, sc-2,matrix)):
        print("compare mask found, nothing to be done")
    else:
        fill.append((matrix[sr+1], matrix[sc+1],matrix))


@jit
def p4x3(sr, sc, fill, clean, matrix):
    if(matrix[sr][sc+1] == BLACK and matrix[sr+1][sc] == BLACK
            and matrix[sr+2][sc] == BLACK and matrix[sr+3][sc+1] == BLACK
            and matrix[sr+1][sc+2] == BLACK and matrix[sr+2][sc+2] == BLACK
            and matrix[sr+1][sc+1] == WHITE and matrix[sr+2][sc+1] == WHITE):
        check_corners((sr, sc), (sr+3, sc), (sr+2, sc), (sr+2, sc+3),clean,matrix)
    elif(compareMask(I2MASK, sr-2, sc-2)):
        print("compare mask found, nothing to be done")
    else:
        fill.append((matrix[sr+1], matrix[sc+1]))
        fill.append((matrix[sr+2], matrix[sc+1]))


@jit
def p3x4(sr, sc, fill, clean, matrix):
    if(matrix[sr+1][sc] == BLACK and matrix[sr][sc+1] == BLACK
            and matrix[sr][sc+2] == BLACK and matrix[sr+2][sc+1] == BLACK
            and matrix[sr+2][sc+2] == BLACK and matrix[sr+1][sr+2] == BLACK
            and matrix[sr+1][sc+1] == WHITE and matrix[sr+1][sc+2] == WHITE):
        check_corners((sr, sc), (sr, sc+3), (sr+2, sc), (sr+2, sc+3),clean,matrix)
    elif(compareMask(I3MASK, sr-2, sc-2)):
        print("compare mask found, nothing to be done")
    else:
        fill.append((matrix[sr+1], matrix[sc+1]))
        fill.append((matrix[sr+1], matrix[sc+2]))


# receives tuples from pXxX, marks elements for deletion if needed
@jit
def check_corners(tl, tr, br, bl,clean,matrix):
    print("checking corners..")
    if(matrix[tl[0]][tl[1]] == WHITE):
        clean.append((tl[0], tl[1]+1))
        clean.append((tl[0]+1, tl[1]))
    if(matrix[tr[0]][tr[1]] == WHITE):
        clean.append((tr[0], tr[1]-1))
        clean.append((tr[0]+1, tr[1]))
    if(matrix[br[0]][br[1]] == WHITE):
        clean.append((br[0]-1, br[1]))
        clean.append((br[0], br[1]-1))
    if(matrix[bl[0]][bl[1]] == WHITE):
        clean.append((bl[0]-1, bl[1]))
        clean.append((bl[0], bl[1]+1))


@jit
def clean_marked(clean,matrix):
    for i in clean:
        matrix[i[0]][i[1]] = WHITE


@jit
def fill_marked(fill,matrix):
    for i in fill:
        matrix[i[0]][i[1]] = BLACK

# iterates, drives processes steps 1-5
# main driver function
# 15:21 23-8, if issues remove - 3

#improvements in logic can be made below most likely
@jit
def clean_s5(matrix):
    #matrix = []
    fill = []
    clean = []
    print("len of gmatrix in local cleans5: ",len(matrix))
    for row in range(0, len(matrix)-3):
        for col in range(0, len(matrix[0])-3):
            print("row col of cur iteration:",row,col)
            if(row+4<len(matrix)):
                process(row,col,"4x3",fill,clean,matrix)
            if(col+4<len(matrix[0])):
                process(row,col,"3x4",fill,clean,matrix)
            process(row,col,"3x3",fill,clean,matrix)
            """
            garbo logic lole
            if(row+4 < len(matrix) and col+4 < len(matrix)):
                process(row, col, "3x3")
                process(row, col, "4x3")
                process(row, col, "3x4")
            else:
                if(row+4 < len(matrix)):
                    process(row, col, "4x3")
                if(col+4 < len(matrix)):
                    process(row, col, "3x4")
                # process 3x3 anyway
                process(row, col, "3x3")
            """
    clean_marked(clean,matrix)
    fill_marked(fill,matrix)
    print("success, no errors (but maybe undefined behavior)")
    #return matrix
# TODO: else if on step 2, step 6 onward