"use strict";
let puzzles = [
    [
        [2, 3, 4],
        [1, 1, 10]
    ],
    [
        [4, 23, 21],
        [22, 29, 19],
        [11, 4, 11],
        [8, 10, 5],
        [24, 18, 16],
        [11, 25, 22],
        [2, 13, 20],
        [24, 15, 14],
        [14, 22, 2],
        [30, 7, 3],
        [30, 22, 25],
        [29, 9, 9],
        [29, 29, 26],
        [14, 3, 16],
        [1, 10, 26],
        [29, 2, 30],
        [30, 10, 25],
        [10, 26, 20],
        [1, 2, 18],
        [25, 18, 5],
        [21, 3, 24],
        [2, 5, 7],
        [22, 11, 21],
        [11, 8, 8],
        [16, 18, 2],
        [13, 3, 8],
        [1, 16, 19],
        [19, 16, 12],
        [21, 15, 1],
        [29, 9, 4],
        [27, 10, 8],
        [2, 7, 27],
        [2, 20, 23],
        [24, 11, 5],
        [2, 8, 27],
        [10, 28, 10],
        [24, 11, 10],
        [19, 2, 12],
        [27, 5, 10],
        [1, 14, 25],
        [5, 14, 30],
        [15, 26, 12],
        [23, 20, 22],
        [5, 12, 1],
        [9, 26, 9],
        [23, 25, 5],
        [28, 16, 19],
        [17, 23, 17],
        [2, 27, 20],
        [18, 27, 13],
        [16, 7, 18],
        [22, 7, 29],
        [17, 28, 6],
        [9, 22, 17],
        [10, 5, 6],
        [14, 2, 12],
        [25, 5, 6],
        [26, 9, 10],
        [19, 21, 6],
        [19, 4, 27],
        [23, 16, 14],
        [21, 17, 29],
        [24, 18, 10],
        [7, 19, 6],
        [14, 15, 10],
        [9, 10, 19],
        [20, 18, 4],
        [11, 14, 8],
        [30, 15, 9],
        [25, 12, 24],
        [3, 12, 5],
        [12, 21, 28],
        [8, 23, 10],
        [18, 26, 8],
        [17, 1, 8],
        [2, 29, 15],
        [3, 13, 28],
        [23, 20, 11],
        [27, 25, 6],
        [19, 21, 3],
        [30, 22, 27],
        [28, 24, 4],
        [26, 18, 21],
        [11, 7, 16],
        [22, 27, 6],
        [27, 5, 26],
        [4, 10, 4],
        [4, 2, 27],
        [2, 3, 26],
        [26, 29, 19],
        [30, 26, 24],
        [8, 25, 12],
        [16, 17, 5],
        [13, 2, 3],
        [1, 30, 22],
        [20, 9, 1],
        [24, 26, 19],
        [26, 18, 1],
        [18, 29, 24],
        [1, 6, 9],
        [20, 27, 2],
        [3, 22, 21],
        [4, 16, 8],
        [29, 18, 16],
        [7, 16, 23],
        [13, 8, 14],
        [19, 25, 10],
        [23, 29, 6],
        [23, 21, 1],
        [22, 26, 10],
        [14, 4, 2],
        [18, 29, 17],
        [9, 4, 18],
        [7, 22, 9],
        [19, 5, 26],
        [27, 29, 19],
        [7, 13, 14],
        [19, 10, 1],
        [6, 22, 3],
        [12, 21, 5],
        [24, 20, 12],
        [28, 2, 11],
        [16, 18, 23],
        [2, 13, 25],
        [11, 7, 17],
        [27, 21, 4],
        [2, 10, 25],
        [22, 16, 17],
        [23, 22, 15],
        [17, 13, 13],
        [23, 24, 26],
        [27, 18, 24],
        [24, 7, 28],
        [30, 12, 15],
        [14, 28, 19],
        [2, 15, 29],
        [12, 13, 5],
        [17, 22, 21],
        [27, 10, 27],
        [17, 6, 25],
        [22, 2, 1],
        [1, 10, 9],
        [9, 7, 2],
        [30, 28, 3],
        [28, 11, 10],
        [8, 23, 15],
        [23, 4, 20],
        [12, 5, 4],
        [13, 17, 14],
        [28, 11, 2],
        [21, 11, 29],
        [10, 23, 22],
        [27, 23, 14],
        [7, 15, 23],
        [20, 2, 13],
        [8, 21, 4],
        [10, 20, 11],
        [23, 28, 11],
        [21, 22, 25],
        [23, 11, 17],
        [2, 29, 10],
        [28, 16, 5],
        [30, 26, 10],
        [17, 24, 16],
        [26, 27, 25],
        [14, 13, 25],
        [22, 27, 5],
        [24, 15, 12],
        [5, 21, 25],
        [4, 27, 1],
        [25, 4, 10],
        [15, 13, 1],
        [21, 23, 7],
        [8, 3, 4],
        [10, 5, 7],
        [9, 13, 30],
        [2, 2, 30],
        [26, 4, 29],
        [5, 14, 14],
        [2, 27, 9],
        [22, 16, 1],
        [4, 23, 5],
        [13, 7, 26],
        [2, 12, 10],
        [12, 7, 22],
        [26, 30, 26],
        [28, 16, 28],
        [15, 19, 11],
        [4, 18, 1],
        [20, 14, 24],
        [6, 10, 22],
        [9, 20, 3],
        [14, 9, 27],
        [26, 17, 9],
        [10, 30, 28],
        [6, 3, 29],
        [4, 16, 28],
        [8, 24, 11],
        [23, 10, 1],
        [11, 7, 7],
        [29, 6, 15],
        [13, 25, 12],
        [29, 14, 3],
        [26, 22, 21],
        [8, 3, 11],
        [27, 13, 25],
        [27, 6, 2],
        [8, 11, 7],
        [25, 12, 9],
        [24, 30, 12],
        [13, 1, 30],
        [25, 23, 16],
        [9, 13, 29],
        [29, 26, 16],
        [11, 15, 9],
        [11, 23, 6],
        [15, 27, 28],
        [27, 24, 21],
        [6, 24, 1],
        [25, 25, 5],
        [11, 1, 26],
        [21, 4, 24],
        [10, 5, 12],
        [4, 30, 13],
        [24, 22, 5],
        [26, 7, 21],
        [23, 3, 17],
        [22, 18, 2],
        [25, 1, 14],
        [23, 25, 30],
        [8, 7, 7],
        [30, 19, 8],
        [17, 6, 15],
        [2, 11, 20],
        [8, 3, 22],
        [23, 14, 26],
        [8, 22, 25],
        [27, 1, 2],
        [10, 26, 2],
        [28, 30, 7],
        [5, 30, 7],
        [27, 16, 30],
        [28, 29, 1],
        [8, 25, 18],
        [20, 12, 29],
        [9, 19, 9],
        [7, 25, 15],
        [25, 18, 18],
        [11, 8, 2],
        [4, 20, 6],
        [18, 5, 20],
        [2, 3, 29],
        [25, 26, 22],
        [18, 25, 26],
        [9, 12, 16],
        [18, 7, 27],
        [17, 20, 9],
        [6, 29, 26],
        [17, 7, 19],
        [21, 7, 5],
        [29, 15, 12],
        [22, 4, 1],
        [11, 12, 11],
        [26, 30, 4],
        [12, 24, 13],
        [13, 8, 3],
        [26, 25, 3],
        [21, 26, 10],
        [14, 9, 26],
        [20, 1, 7],
        [11, 12, 3],
        [12, 11, 4],
        [11, 15, 30],
        [17, 6, 25],
        [20, 22, 3],
        [1, 16, 17],
        [11, 5, 20],
        [12, 12, 7],
        [2, 14, 10],
        [14, 27, 3],
        [14, 16, 18],
        [21, 28, 24],
        [14, 20, 1],
        [29, 14, 1],
        [10, 10, 9],
        [25, 23, 4],
        [17, 15, 14],
        [9, 20, 26],
        [16, 2, 17],
        [13, 28, 25],
        [16, 1, 11],
        [19, 16, 8],
        [20, 21, 2],
        [27, 9, 22],
        [24, 18, 3],
        [23, 30, 6],
        [4, 18, 3],
        [30, 15, 8],
        [27, 20, 19],
        [28, 29, 26],
        [2, 21, 18],
        [1, 23, 30],
        [1, 9, 12],
        [4, 11, 30],
        [1, 28, 4],
        [17, 10, 10],
        [12, 14, 6],
        [8, 9, 24],
        [8, 3, 3],
        [29, 8, 20],
        [26, 29, 2],
        [29, 25, 25],
        [11, 17, 23],
        [6, 30, 21],
        [13, 18, 29],
        [2, 10, 8],
        [29, 29, 27],
        [27, 15, 15],
        [16, 17, 30],
        [3, 3, 22],
        [21, 12, 6],
        [22, 1, 5],
        [30, 8, 20],
        [6, 28, 13],
        [11, 2, 23],
        [14, 18, 27],
        [6, 26, 13],
        [10, 24, 24],
        [4, 24, 6],
        [20, 8, 3],
        [23, 11, 5],
        [29, 5, 24],
        [14, 15, 22],
        [21, 17, 13],
        [10, 10, 8],
        [1, 11, 23],
        [21, 19, 24],
        [19, 9, 13],
        [21, 26, 28],
        [25, 11, 28],
        [2, 17, 1],
        [18, 9, 8],
        [5, 21, 6],
        [12, 5, 2],
        [23, 8, 15],
        [30, 16, 24],
        [7, 9, 27],
        [16, 30, 7],
        [2, 21, 28],
        [5, 10, 6],
        [8, 7, 1],
        [28, 13, 5],
        [11, 5, 14],
        [26, 22, 29],
        [23, 15, 13],
        [14, 2, 16],
        [22, 21, 9],
        [4, 20, 3],
        [18, 17, 19],
        [12, 7, 9],
        [6, 12, 25],
        [3, 30, 27],
        [8, 19, 22],
        [1, 9, 27],
        [23, 20, 12],
        [14, 7, 29],
        [9, 12, 12],
        [30, 2, 6],
        [15, 7, 16],
        [19, 13, 18],
        [11, 8, 13],
        [16, 5, 3],
        [19, 26, 24],
        [26, 8, 21],
        [21, 20, 7],
        [15, 1, 25],
        [29, 15, 21],
        [22, 17, 7],
        [16, 17, 10],
        [6, 12, 24],
        [8, 13, 27],
        [30, 25, 14],
        [25, 7, 10],
        [15, 2, 2],
        [18, 15, 19],
        [18, 13, 24],
        [19, 30, 1],
        [17, 1, 3],
        [26, 21, 15],
        [10, 10, 18],
        [9, 16, 6],
        [29, 7, 30],
        [11, 10, 30],
        [6, 11, 2],
        [7, 29, 23],
        [13, 2, 30],
        [25, 27, 13],
        [5, 15, 21],
        [4, 8, 30],
        [15, 27, 11],
        [27, 1, 6],
        [2, 24, 11],
        [16, 20, 19],
        [25, 28, 20],
        [6, 8, 4],
        [27, 16, 11],
        [1, 5, 27],
        [12, 19, 26],
        [18, 24, 14],
        [4, 25, 17],
        [24, 24, 26],
        [28, 3, 18],
        [8, 20, 28],
        [22, 7, 21],
        [24, 5, 28],
        [23, 30, 29],
        [25, 16, 27],
        [28, 10, 30],
        [9, 2, 4],
        [30, 2, 23],
        [21, 9, 23],
        [27, 4, 26],
        [2, 23, 16],
        [24, 26, 30],
        [26, 1, 30],
        [10, 4, 28],
        [11, 29, 12],
        [28, 13, 30],
        [24, 10, 28],
        [8, 12, 12],
        [19, 27, 11],
        [11, 28, 7],
        [14, 6, 3],
        [6, 27, 5],
        [6, 17, 14],
        [24, 24, 17],
        [18, 23, 14],
        [17, 5, 7],
        [11, 4, 23],
        [5, 1, 17],
        [26, 15, 24],
        [3, 9, 24],
        [5, 3, 15],
        [5, 20, 19],
        [5, 21, 2],
        [13, 5, 30],
        [19, 6, 24],
        [19, 17, 6],
        [23, 7, 13],
        [28, 23, 13],
        [9, 1, 6],
        [15, 12, 16],
        [21, 19, 9],
        [25, 5, 5],
        [9, 7, 9],
        [6, 5, 8],
        [3, 11, 18],
        [23, 25, 11],
        [25, 4, 6],
        [4, 27, 1],
        [4, 3, 3],
        [30, 11, 5],
        [9, 17, 12],
        [15, 6, 24],
        [10, 22, 15],
        [29, 27, 9],
        [20, 21, 11],
        [18, 10, 5],
        [11, 2, 2],
        [9, 8, 8],
        [1, 26, 21],
        [11, 11, 16],
        [2, 18, 30],
        [29, 27, 24],
        [27, 8, 18],
        [19, 3, 17],
        [30, 21, 26],
        [25, 13, 25],
        [20, 22, 1],
        [10, 1, 12],
        [11, 17, 15],
        [29, 11, 30],
        [17, 30, 27],
        [21, 22, 17],
        [13, 6, 22],
        [22, 16, 12],
        [27, 18, 19],
        [4, 13, 6],
        [27, 29, 10],
        [3, 23, 10],
        [26, 16, 24],
        [18, 26, 20],
        [11, 28, 16],
        [21, 6, 15],
        [9, 26, 17],
        [8, 15, 8],
        [3, 7, 10],
        [2, 28, 8],
        [1, 2, 24],
        [7, 8, 9],
        [19, 4, 22],
        [11, 20, 9],
        [12, 22, 16],
        [26, 8, 19],
        [13, 28, 24],
        [4, 10, 16],
        [12, 8, 10],
        [14, 24, 24],
        [19, 19, 28],
        [29, 1, 15],
        [10, 5, 14],
        [20, 19, 23],
        [10, 7, 12],
        [1, 7, 13],
        [5, 12, 13],
        [25, 21, 8],
        [22, 28, 8],
        [7, 9, 4],
        [3, 20, 15],
        [15, 27, 19],
        [18, 24, 12],
        [16, 10, 16],
        [22, 19, 8],
        [15, 4, 3],
        [9, 30, 25],
        [1, 1, 6],
        [24, 4, 25],
        [13, 18, 29],
        [10, 2, 8],
        [21, 1, 17],
        [29, 14, 22],
        [17, 29, 11],
        [10, 27, 16],
        [25, 16, 15],
        [14, 2, 17],
        [12, 27, 3],
        [14, 17, 25],
        [24, 4, 1],
        [18, 28, 18],
        [9, 14, 26],
        [28, 24, 17],
        [1, 26, 12],
        [2, 18, 20],
        [12, 19, 22],
        [19, 25, 20],
        [5, 17, 27],
        [17, 29, 16],
        [29, 19, 11],
        [16, 2, 4],
        [23, 24, 1],
        [19, 18, 3],
        [28, 14, 6],
        [18, 5, 23],
        [9, 24, 12],
        [15, 4, 6],
        [15, 7, 24],
        [22, 15, 8],
        [22, 1, 22],
        [6, 4, 22],
        [26, 1, 30],
        [8, 21, 27],
        [7, 1, 11],
        [9, 8, 18],
        [20, 27, 12],
        [26, 23, 20],
        [26, 22, 30],
        [24, 3, 16],
        [8, 24, 28],
        [13, 28, 5],
        [4, 29, 23],
        [22, 5, 8],
        [20, 22, 3],
        [9, 9, 17],
        [28, 3, 30],
        [10, 13, 10],
        [10, 25, 13],
        [9, 20, 3],
        [1, 21, 25],
        [24, 21, 15],
        [21, 5, 14],
        [13, 8, 20],
        [29, 17, 3],
        [5, 17, 28],
        [16, 12, 7],
        [23, 1, 24],
        [4, 24, 29],
        [23, 25, 14],
        [8, 27, 2],
        [23, 11, 13],
        [13, 4, 5],
        [24, 1, 26],
        [21, 1, 23],
        [10, 12, 12],
        [21, 29, 25],
        [27, 25, 30],
        [24, 23, 4],
        [1, 30, 23],
        [29, 28, 14],
        [4, 11, 30],
        [9, 25, 10],
        [17, 11, 6],
        [14, 29, 30],
        [23, 5, 5],
        [25, 18, 21],
        [8, 7, 1],
        [27, 11, 3],
        [5, 10, 8],
        [11, 1, 11],
        [16, 17, 26],
        [15, 22, 19],
        [16, 9, 6],
        [18, 13, 27],
        [26, 4, 22],
        [1, 20, 21],
        [6, 14, 29],
        [11, 7, 6],
        [1, 23, 7],
        [12, 19, 13],
        [18, 21, 25],
        [15, 17, 20],
        [23, 8, 9],
        [15, 9, 26],
        [9, 12, 9],
        [12, 13, 14],
        [27, 26, 7],
        [11, 19, 22],
        [16, 12, 21],
        [10, 30, 28],
        [21, 2, 7],
        [12, 9, 18],
        [7, 17, 14],
        [13, 17, 17],
        [3, 21, 10],
        [30, 9, 15],
        [2, 8, 15],
        [15, 12, 10],
        [23, 26, 9],
        [29, 30, 10],
        [30, 22, 17],
        [17, 26, 30],
        [27, 26, 20],
        [17, 28, 17],
        [30, 12, 16],
        [7, 23, 15],
        [30, 15, 19],
        [13, 19, 10],
        [22, 10, 4],
        [17, 23, 10],
        [2, 28, 18],
        [27, 21, 28],
        [24, 26, 5],
        [6, 23, 25],
        [17, 4, 16],
        [14, 1, 13],
        [23, 21, 11],
        [14, 15, 30],
        [26, 13, 10],
        [30, 19, 25],
        [26, 6, 26],
        [9, 16, 29],
        [15, 2, 24],
        [13, 3, 20],
        [23, 12, 30],
        [22, 23, 23],
        [8, 21, 2],
        [18, 28, 5],
        [21, 27, 14],
        [29, 28, 23],
        [12, 30, 28],
        [17, 16, 3],
        [5, 19, 11],
        [28, 22, 22],
        [1, 4, 28],
        [10, 10, 14],
        [18, 15, 7],
        [18, 11, 1],
        [12, 7, 16],
        [10, 22, 24],
        [27, 25, 6],
        [19, 29, 25],
        [10, 1, 26],
        [26, 27, 30],
        [4, 23, 19],
        [24, 19, 4],
        [21, 11, 14],
        [4, 13, 27],
        [9, 1, 11],
        [16, 20, 8],
        [4, 3, 11],
        [1, 16, 12],
        [14, 6, 30],
        [8, 1, 10],
        [11, 18, 7],
        [29, 28, 30],
        [4, 21, 8],
        [3, 21, 4],
        [6, 1, 5],
        [26, 18, 3],
        [28, 27, 27],
        [17, 3, 12],
        [6, 1, 22],
        [23, 12, 28],
        [12, 13, 2],
        [11, 2, 13],
        [7, 1, 28],
        [27, 6, 25],
        [14, 14, 3],
        [14, 11, 20],
        [2, 27, 7],
        [22, 24, 23],
        [7, 15, 20],
        [30, 6, 17],
        [20, 23, 25],
        [18, 16, 27],
        [2, 9, 6],
        [9, 18, 19],
        [20, 11, 22],
        [11, 16, 19],
        [14, 29, 23],
        [14, 9, 20],
        [8, 10, 12],
        [18, 17, 6],
        [28, 7, 16],
        [12, 19, 28],
        [5, 3, 16],
        [1, 25, 10],
        [4, 14, 10],
        [9, 6, 3],
        [15, 27, 28],
        [13, 26, 14],
        [21, 8, 25],
        [29, 10, 20],
        [14, 26, 30],
        [25, 13, 28],
        [1, 15, 23],
        [6, 20, 21],
        [18, 2, 1],
        [22, 25, 16],
        [23, 25, 17],
        [2, 14, 21],
        [14, 25, 16],
        [12, 17, 6],
        [19, 29, 15],
        [25, 9, 6],
        [19, 17, 13],
        [24, 22, 5],
        [19, 4, 13],
        [10, 18, 6],
        [6, 25, 6],
        [23, 24, 20],
        [8, 22, 13],
        [25, 10, 29],
        [5, 12, 25],
        [20, 5, 11],
        [7, 16, 29],
        [29, 24, 22],
        [28, 20, 1],
        [10, 27, 10],
        [6, 9, 27],
        [26, 15, 30],
        [26, 3, 19],
        [20, 11, 3],
        [26, 1, 29],
        [6, 23, 4],
        [6, 13, 21],
        [9, 23, 25],
        [15, 1, 10],
        [29, 12, 13],
        [7, 8, 24],
        [29, 30, 27],
        [3, 29, 19],
        [14, 16, 17],
        [4, 8, 27],
        [26, 17, 8],
        [10, 27, 17],
        [11, 28, 17],
        [17, 16, 27],
        [1, 8, 22],
        [6, 30, 16],
        [7, 30, 22],
        [20, 12, 3],
        [18, 10, 2],
        [20, 21, 26],
        [11, 1, 17],
        [9, 15, 15],
        [19, 14, 30],
        [24, 22, 20],
        [11, 26, 23],
        [14, 3, 23],
        [1, 28, 29],
        [29, 20, 4],
        [1, 4, 20],
        [12, 26, 8],
        [14, 11, 14],
        [14, 19, 13],
        [15, 13, 24],
        [16, 7, 26],
        [11, 20, 11],
        [5, 24, 26],
        [24, 25, 7],
        [21, 3, 14],
        [24, 29, 20],
        [7, 12, 1],
        [16, 17, 4],
        [29, 16, 21],
        [28, 8, 17],
        [11, 30, 25],
        [1, 26, 23],
        [25, 19, 28],
        [30, 24, 5],
        [26, 29, 15],
        [4, 25, 23],
        [14, 25, 19],
        [29, 10, 7],
        [29, 29, 28],
        [19, 13, 24],
        [21, 28, 5],
        [8, 15, 24],
        [1, 10, 12],
        [2, 26, 6],
        [14, 14, 4],
        [10, 16, 27],
        [9, 17, 25],
        [25, 8, 7],
        [1, 9, 28],
        [10, 8, 17],
        [4, 12, 1],
        [17, 26, 29],
        [23, 12, 26],
        [2, 21, 22],
        [18, 23, 13],
        [1, 14, 5],
        [25, 27, 26],
        [4, 30, 30],
        [5, 13, 2],
        [17, 9, 6],
        [28, 18, 28],
        [7, 30, 2],
        [28, 22, 17],
        [14, 15, 14],
        [10, 14, 19],
        [6, 15, 22],
        [27, 4, 17],
        [28, 21, 6],
        [19, 29, 26],
        [6, 17, 17],
        [20, 13, 16],
        [25, 4, 1],
        [2, 9, 5],
        [30, 3, 1],
        [24, 21, 2],
        [14, 19, 12],
        [22, 5, 23],
        [14, 4, 21],
        [10, 2, 17],
        [3, 14, 10],
        [17, 5, 3],
        [22, 17, 13],
        [5, 19, 3],
        [29, 22, 6],
        [12, 28, 3],
        [9, 21, 25],
        [10, 2, 14],
        [13, 26, 7],
        [18, 23, 2],
        [9, 14, 17],
        [21, 3, 13],
        [13, 23, 9],
        [1, 20, 4],
        [11, 4, 1],
        [19, 5, 30],
        [9, 9, 29],
        [26, 29, 14],
        [1, 4, 10],
        [7, 27, 30],
        [8, 3, 23],
        [1, 27, 27],
        [7, 27, 27],
        [1, 26, 16],
        [29, 16, 14],
        [18, 6, 12],
        [24, 24, 24],
        [26, 2, 19],
        [15, 17, 4],
        [11, 7, 14],
        [14, 19, 10],
        [9, 10, 1],
        [14, 17, 9],
        [20, 19, 13],
        [25, 20, 8],
        [24, 20, 21],
        [26, 30, 2],
        [24, 2, 10],
        [28, 4, 13],
        [27, 17, 11],
        [15, 3, 8],
        [11, 29, 10],
        [26, 15, 16],
        [4, 28, 22],
        [7, 5, 22],
        [10, 28, 9],
        [6, 28, 13],
        [10, 5, 6],
        [20, 12, 6],
        [25, 30, 30],
        [17, 16, 14],
        [14, 20, 3],
        [16, 10, 8],
        [9, 28, 14],
        [16, 12, 12],
        [11, 13, 25],
        [21, 16, 28],
        [10, 3, 18],
        [5, 9, 20],
        [17, 23, 5],
        [3, 13, 16],
        [29, 30, 17],
        [2, 2, 8],
        [15, 8, 30],
        [20, 1, 16],
        [23, 10, 29],
        [4, 5, 4],
        [6, 18, 12],
        [26, 10, 22],
        [21, 10, 17],
        [26, 12, 29],
        [7, 20, 21],
        [18, 9, 15],
        [10, 23, 20],
        [20, 1, 27],
        [10, 10, 3],
        [25, 12, 23],
        [30, 11, 15],
        [16, 22, 3],
        [22, 10, 11],
        [15, 10, 20],
        [2, 20, 17],
        [20, 20, 1],
        [24, 16, 4],
        [23, 27, 7],
        [7, 27, 22],
        [24, 16, 8],
        [20, 11, 25],
        [30, 28, 11],
        [21, 6, 24],
        [15, 2, 9],
        [16, 30, 24],
        [21, 27, 9],
        [7, 19, 8],
        [24, 13, 28],
        [12, 26, 28],
        [16, 21, 11],
        [25, 5, 13],
        [23, 3, 17],
        [23, 1, 17],
        [4, 17, 18],
        [17, 13, 18],
        [25, 12, 19],
        [17, 4, 19],
        [4, 21, 26],
        [6, 28, 1],
        [23, 22, 15],
        [6, 23, 12],
        [21, 17, 9],
        [30, 4, 23],
        [2, 19, 21],
        [28, 24, 7],
        [19, 24, 14],
        [13, 20, 26],
        [19, 24, 29],
        [8, 26, 3],
        [16, 12, 14],
        [17, 4, 21],
        [8, 4, 20],
        [13, 27, 17],
        [9, 21, 1],
        [29, 25, 6],
        [7, 9, 26],
        [13, 25, 5],
        [6, 9, 21],
        [12, 10, 11],
        [30, 28, 21],
        [15, 6, 2],
        [8, 18, 19],
        [26, 20, 24],
        [26, 17, 14],
        [27, 8, 1],
        [19, 19, 18],
        [25, 24, 27],
        [14, 29, 15],
        [22, 26, 1],
        [14, 17, 9],
        [2, 6, 23],
        [29, 7, 5],
        [14, 16, 19],
        [14, 21, 18],
        [10, 15, 23],
        [21, 29, 14],
        [20, 29, 30],
        [23, 11, 5],
    ]
];
function paperRequired(l, w, h) {
    return 2 * l * w + 2 * l * h + 2 * w * h + Math.min(l * w, l * h, w * h);
}
function ribbonRequired(l, w, h) {
    return Math.min(2 * (l + w), 2 * (l + h), 2 * (w + h)) + l * w * h;
}
puzzles.forEach(boxes => {
    const totPaper = boxes.map(box => paperRequired(box[0], box[1], box[2])).reduce((s, p) => s + p, 0);
    console.log(totPaper);
    const totRibbon = boxes.map(box => ribbonRequired(box[0], box[1], box[2])).reduce((s, p) => s + p, 0);
    console.log(totRibbon);
});